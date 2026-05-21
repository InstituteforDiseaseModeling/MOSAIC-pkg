# ============================================================================
# profile_gc_phase1.R
#
# Phase 1: Instrument MOSAIC GC behavior to confirm the diagnosis that 40% of
# CPU time is spent in garbage collection.
#
# This script:
#   1. Wraps every R gc() call in MOSAIC with timing + heap-delta logging
#      (uses trace() so we don't have to edit source files)
#   2. Runs a fixed-mode local-parallel calibration (NO Dask — easier to
#      profile, isolates R-side GC)
#   3. Writes a structured CSV/parquet of every gc() event for analysis
#   4. Designed to be wrapped by py-spy / samply for matched profiler output
#
# Backend choice — LOCAL parallel (not Dask):
#   The reported profile (33% R gc, 7% Python gc) was on a simplified scenario,
#   most likely run locally. Profiling Dask workers requires attaching py-spy
#   on Coiled VMs which is harder. We can extend to Dask in Phase 2 if needed.
#
# Outputs:
#   ~/output/gc_profile_<stamp>/gc_events.csv     (per-call gc timing)
#   ~/output/gc_profile_<stamp>/gc_summary.csv    (aggregated by site)
#   ~/output/gc_profile_<stamp>/run.log
#   ~/output/gc_profile_<stamp>/2_calibration/... (normal calibration outputs)
# ============================================================================

library(MOSAIC)

# ---------------------------------------------------------------------------
# Output directory
# ---------------------------------------------------------------------------
base_output_dir <- if (dir.exists("/workspace/output")) {
  "/workspace/output"
} else {
  file.path(getwd(), "output")
}

run_stamp  <- format(Sys.time(), "%Y%m%d_%H%M%S")
dir_output <- file.path(base_output_dir, paste0("gc_profile_", run_stamp))
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

GC_LOG_PATH <- file.path(dir_output, "gc_events.csv")
cat("GC events will be written to:", GC_LOG_PATH, "\n")
cat("Output directory:", dir_output, "\n")

# ---------------------------------------------------------------------------
# GC instrumentation: a wrapped gc() that logs (call_site, time_sec,
# mem_used_mb_before, mem_used_mb_after) to a CSV.
#
# Approach: replace base::gc with a wrapper that calls the real gc() but also
# records what happened. The wrapper inspects sys.call(-1) to attribute the
# call back to its R source location.
# ---------------------------------------------------------------------------
GC_EVENTS <- list()
GC_REAL   <- base::gc

GC_CSV_HEADER <- "ts,site,wall_sec,mb_used_before,mb_used_after,mb_freed"

# Initialize CSV with header (and re-write inside the wrapper if the file
# ever gets clobbered — defensive against anything that might truncate it).
writeLines(GC_CSV_HEADER, GC_LOG_PATH)

# Resolve a call-site label for the gc() invocation. R packages typically
# don't ship srcrefs, so a single file:line is rarely available. Instead we
# capture a short stack of function NAMES which identifies the call path
# inside MOSAIC (e.g. "run_MOSAIC > .mosaic_run_simulation_worker > gc").
# If any frame happens to carry a srcref, we append "@ file:line".
.gc_caller_label <- function() {
  tryCatch({
    calls <- sys.calls()
    if (length(calls) < 2L) return("<top-level>")

    # Drop our own (instrumented_gc) frame.
    parent_calls <- calls[seq_len(length(calls) - 1L)]

    # Function name for each frame (the first symbol in the call).
    fnames <- vapply(parent_calls, function(cl) {
      tryCatch({
        nm <- as.character(cl[[1L]])[1L]
        if (is.null(nm) || is.na(nm) || !nzchar(nm)) "<anon>" else nm
      }, error = function(e) "<?>")
    }, character(1L))

    # Keep the most recent 4 distinct names to keep labels short.
    keep <- utils::tail(fnames, 4L)
    label_path <- paste(keep, collapse = ">")

    # If any frame carries a srcref, append the most recent one.
    src_loc <- ""
    for (i in rev(seq_along(parent_calls))) {
      src <- attr(parent_calls[[i]], "srcref")
      if (!is.null(src)) {
        file <- tryCatch(attr(src, "srcfile")$filename, error = function(e) NULL)
        line <- tryCatch(as.integer(src)[1L], error = function(e) NA_integer_)
        if (!is.null(file) && nzchar(file) && !is.na(line)) {
          src_loc <- sprintf(" @ %s:%d", basename(file), line)
          break
        }
      }
    }
    paste0(label_path, src_loc)
  }, error = function(e) sprintf("<err:%s>", conditionMessage(e)))
}

instrumented_gc <- function(verbose = TRUE, reset = FALSE, full = TRUE) {
  caller_loc <- .gc_caller_label()

  mem_before <- sum(GC_REAL(verbose = FALSE, reset = FALSE)[, "(Mb)"])
  t0 <- proc.time()[["elapsed"]]
  res <- GC_REAL(verbose = verbose, reset = reset, full = full)
  wall <- proc.time()[["elapsed"]] - t0
  mem_after <- sum(res[, "(Mb)"])

  # Re-create header if the file is missing or has been truncated.
  if (!file.exists(GC_LOG_PATH) || file.size(GC_LOG_PATH) == 0L) {
    writeLines(GC_CSV_HEADER, GC_LOG_PATH)
  }

  # Quote site to be safe; it may contain commas, spaces, '>', '@'.
  site_csv <- sprintf("\"%s\"", gsub("\"", "'", caller_loc, fixed = TRUE))

  cat(
    sprintf("%s,%s,%.6f,%.2f,%.2f,%.2f\n",
            format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
            site_csv, wall, mem_before, mem_after,
            mem_before - mem_after),
    file = GC_LOG_PATH, append = TRUE
  )

  invisible(res)
}

# Inject the wrapper. Note: we override base::gc so EVERY gc() call gets
# logged — including indirect ones from packages we don't control. That's
# actually useful for the audit.
unlockBinding("gc", baseenv())
assign("gc", instrumented_gc, envir = baseenv())
lockBinding("gc", baseenv())

cat("Instrumented base::gc — every call will be logged.\n")

# Self-test: call gc() from a known line so we can confirm the caller-label
# machinery works before kicking off a 7-minute calibration.
cat("Self-test: triggering 2 gc() calls from this script for label check...\n")
gc()  # this line's number is logged as the site
gc()  # so is this one
.test_rows <- readLines(GC_LOG_PATH)
cat("CSV currently has ", length(.test_rows), " lines (1 header + N events).\n",
    "Last 2 lines:\n  ", paste(tail(.test_rows, 2), collapse = "\n  "), "\n", sep = "")

# ---------------------------------------------------------------------------
# MOSAIC setup (mirrors mosaic_dask_fixed_test.R)
# ---------------------------------------------------------------------------
set_root_directory("/workspace/MOSAIC")

config <- get_location_config(iso = "ETH")
priors <- get_location_priors(iso = "ETH")

# ---------------------------------------------------------------------------
# Profiling control: small enough to finish quickly (~5-15 min), large enough
# for GC to dominate. Local parallel — no Dask.
# ---------------------------------------------------------------------------
ctrl <- mosaic_control_defaults()

ctrl$calibration$n_simulations <- 100       # 100 sims × 3 iter is enough
ctrl$calibration$batch_size    <- 100       # to characterize gc() patterns
ctrl$calibration$n_iterations  <- 3         # sequentially (~5-8 min total)

ctrl$targets$ESS_param      <- 200
ctrl$targets$ESS_param_prop <- 0.9
ctrl$targets$ESS_best       <- 50
ctrl$targets$ess_method     <- "perplexity"

ctrl$sampling$sample_tau_i          <- FALSE
ctrl$sampling$sample_mobility_gamma <- FALSE
ctrl$sampling$sample_mobility_omega <- FALSE

ctrl$likelihood$weight_cases  <- 1
ctrl$likelihood$weight_deaths <- 0.05

ctrl$predictions$n_iter_best     <- 5      # Smaller post-cal too
ctrl$predictions$n_iter_ensemble <- 2

ctrl$npe$enable <- FALSE

# SEQUENTIAL execution — disable parallel so every gc() call happens in the
# master R process where our base::gc override is installed. PSOCK workers
# are fresh R sessions with untouched baseenv, so gc() calls inside them
# bypass the instrumentation. Sequential is ~4x slower for the same N sims,
# but the GC accounting is then complete.
#
# To profile the parallel hot path specifically, see Phase 2.
ctrl$parallel$enable  <- FALSE
ctrl$parallel$n_cores <- 1L

ctrl$paths$clean_output <- TRUE
ctrl$paths$plots        <- FALSE     # Skip plots — they have their own gc
ctrl$io                 <- mosaic_io_presets("fast")
ctrl$io$load_chunk_size <- 1000

# ---------------------------------------------------------------------------
# Capture overall wall time and peak RSS at the end
# ---------------------------------------------------------------------------
t_start <- proc.time()
cat(format(Sys.time()), " — Starting run_MOSAIC()\n", sep = "")

result <- run_MOSAIC(
  config     = config,
  priors     = priors,
  dir_output = dir_output,
  control    = ctrl,
  dask_spec  = NULL                     # local parallel, NOT Dask
)

t_end <- proc.time()
cat(format(Sys.time()), " — Finished. Elapsed (s):\n", sep = "")
print(t_end - t_start)

# ---------------------------------------------------------------------------
# Summarize GC events by call site (defensive: avoids split.data.frame which
# blows up if the `site` column is missing or empty; also tolerates single
# rows that aggregate() would collapse to a vector).
# ---------------------------------------------------------------------------
gc_df <- read.csv(GC_LOG_PATH, stringsAsFactors = FALSE)
cat("\n=== GC events: ", nrow(gc_df), " total calls ===\n", sep = "")

# Diagnostic: dump structure so we can see what was parsed if anything is off.
cat("Columns: ", paste(names(gc_df), collapse = ", "), "\n", sep = "")
cat("Head:\n"); print(utils::head(gc_df, 3))

required_cols <- c("site", "wall_sec", "mb_freed")
missing_cols  <- setdiff(required_cols, names(gc_df))
if (length(missing_cols) > 0L) {
  cat("WARNING: missing expected columns: ",
      paste(missing_cols, collapse = ", "),
      ". Skipping summary; raw events still in gc_events.csv.\n", sep = "")
  summary_df <- data.frame()
} else if (nrow(gc_df) == 0L) {
  cat("WARNING: no gc events were logged.\n")
  summary_df <- data.frame()
} else {
  # Coerce site to character & drop empty/NA, then loop by unique value.
  sites_all <- as.character(gc_df$site)
  sites_all[is.na(sites_all) | !nzchar(sites_all)] <- "<empty>"
  unique_sites <- unique(sites_all)

  summary_df <- do.call(rbind, lapply(unique_sites, function(s) {
    idx <- sites_all == s
    d   <- gc_df[idx, , drop = FALSE]
    data.frame(
      site          = s,
      n_calls       = nrow(d),
      wall_sum      = sum(d$wall_sec, na.rm = TRUE),
      wall_mean     = mean(d$wall_sec, na.rm = TRUE),
      wall_max      = max(d$wall_sec, na.rm = TRUE),
      mb_freed_sum  = sum(d$mb_freed, na.rm = TRUE),
      mb_freed_mean = mean(d$mb_freed, na.rm = TRUE),
      mb_freed_max  = max(d$mb_freed, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary_df) <- NULL
  summary_df <- summary_df[order(-summary_df$wall_sum), ]
}

summary_path <- file.path(dir_output, "gc_summary.csv")
write.csv(summary_df, summary_path, row.names = FALSE)

cat("Total wall time in gc():", sum(gc_df$wall_sec), "sec\n")
cat("Total run wall time:    ", (t_end - t_start)[["elapsed"]], "sec\n")
cat("GC as fraction of run:  ",
    100 * sum(gc_df$wall_sec) / (t_end - t_start)[["elapsed"]], "%\n")

cat("\n=== Top GC sites by total wall time ===\n")
print(head(summary_df, 10))

cat("\nGC summary written to:", summary_path, "\n")
cat("Per-call events at:    ", GC_LOG_PATH, "\n")
cat("Done.\n")
