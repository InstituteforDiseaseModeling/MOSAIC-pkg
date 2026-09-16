# =============================================================================
# harness.R -- timing, attribution, and ledger emission
#
# Design constraints, each of which exists because dropping it produced a wrong
# number in review:
#
#   * `min` is the headline statistic, not the mean. Timing contamination is
#     one-sided -- interference can only ADD time -- so under a
#     "true cost + non-negative noise" model the minimum is the maximum-
#     likelihood estimate of the noise-free cost. Measured: min-of-5 carries
#     2.98% relative sd against median-of-5's 6.22%.
#   * The full replicate vector is stored. Two order statistics and a count
#     cannot yield a confidence interval; the raw reps can.
#   * A row that cannot be attributed is not emitted. git sha resolves from the
#     SOURCE TREE being measured, never from getwd() -- the package's own
#     .mosaic_capture_environment() falls back to "." and will happily record
#     the sha of whatever repo the caller happened to cd into.
#   * A machine calibrator runs at session start and end. Measured drift on an
#     identical binary reached 7.8% over 20 minutes, which is larger than most
#     of the effects being tracked.
# =============================================================================

.bench_time <- function(expr, reps, warmup = 1L) {
     e <- substitute(expr); pf <- parent.frame()
     for (i in seq_len(warmup)) eval(e, pf)
     vapply(seq_len(reps), function(i) {
          t0 <- proc.time()[["elapsed"]]
          eval(e, pf)
          proc.time()[["elapsed"]] - t0
     }, numeric(1))
}

.bench_git <- function(src_dir) {
     if (is.null(src_dir) || !nzchar(src_dir) || !dir.exists(src_dir))
          return(list(sha = NA_character_, dirty = NA))
     sha <- tryCatch(system2("git", c("-C", src_dir, "rev-parse", "HEAD"),
                             stdout = TRUE, stderr = FALSE)[1],
                     error = function(e) NA_character_)
     st  <- tryCatch(system2("git", c("-C", src_dir, "status", "--porcelain"),
                             stdout = TRUE, stderr = FALSE),
                     error = function(e) NA_character_)
     list(sha = sha, dirty = if (all(is.na(st))) NA else length(st) > 0L)
}

.bench_meta <- function(arm, lib, src_dir, config_md5) {
     g  <- .bench_git(src_dir)
     si <- Sys.info()
     cpu <- tryCatch(sub("^.*: *", "",
             system2("sysctl", "machdep.cpu.brand_string", stdout = TRUE)[1]),
             error = function(e) NA_character_)
     if (is.na(cpu) || !nzchar(cpu))
          cpu <- tryCatch(sub("^.*: *", "",
                  grep("model name", readLines("/proc/cpuinfo"), value = TRUE)[1]),
                  error = function(e) NA_character_)
     list(
          arm             = arm,
          mosaic_version  = as.character(utils::packageVersion("MOSAIC")),
          engine          = .bench_engine_kind(),
          engine_version  = .bench_engine_version(),
          git_sha         = g$sha,
          git_dirty       = g$dirty,
          lib_path        = lib,
          config_md5      = config_md5,
          host            = si[["nodename"]],
          os_release      = paste(si[["sysname"]], si[["release"]]),
          cpu_model       = cpu,
          n_cores         = parallel::detectCores(),
          r_version       = paste0(R.version$major, ".", R.version$minor),
          r_enable_jit    = Sys.getenv("R_ENABLE_JIT", "default"),
          blas            = tryCatch(extSoftVersion()[["BLAS"]], error = function(e) NA_character_)
     )
}

# One row per (workload, metric). Long format: a new metric never changes the
# schema. `calibrator_s` is filled in by the runner once the session's
# calibrator has been measured, so every row carries the machine's
# contemporaneous speed and a row taken in September can be normalised against
# one taken in December.
.bench_rows <- function(meta, block, workload, reps_s, extra = list()) {
     base <- data.frame(
          run_id     = meta$run_id %||% NA_character_,
          ts_utc     = format(Sys.time(), tz = "UTC", "%Y-%m-%dT%H:%M:%SZ"),
          block      = block,
          workload   = workload,
          stringsAsFactors = FALSE
     )
     stats <- list(wall_min_s = min(reps_s), wall_median_s = stats::median(reps_s),
                   wall_mean_s = mean(reps_s), n_reps = length(reps_s),
                   reps_json = paste(sprintf("%.5f", reps_s), collapse = ";"))
     out <- do.call(rbind, lapply(names(stats), function(k) {
          cbind(base, data.frame(metric = k, value = as.character(stats[[k]]),
                                 stringsAsFactors = FALSE))
     }))
     if (length(extra)) {
          out <- rbind(out, do.call(rbind, lapply(names(extra), function(k) {
               cbind(base, data.frame(metric = k, value = as.character(extra[[k]]),
                                      stringsAsFactors = FALSE))
          })))
     }
     for (k in names(meta)) out[[k]] <- meta[[k]]
     out
}

`%||%` <- function(a, b) if (is.null(a)) b else a
