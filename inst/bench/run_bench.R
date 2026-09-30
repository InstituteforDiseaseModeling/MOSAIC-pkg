# =============================================================================
# run_bench.R -- benchmark entry point.
#
#   Rscript inst/bench/run_bench.R --lib=<path> --arm=<name> --src=<repo> \
#           --out=<csv> [--block=N] [--only=id,id] [--parallel] [--calib]
#
# Runs STANDALONE against a target library rather than from inside the package
# it measures. That is deliberate: this suite has to produce comparable rows
# from codebases whose entry points differ (run_LASER vs run_simulation), so
# the newest copy of the script always drives and older libraries are the
# subject. Never source this from the package being benchmarked.
# =============================================================================

a <- commandArgs(TRUE)
getarg <- function(k, d = NULL) {
  m <- grep(paste0("^--", k, "="), a, value = TRUE)
  if (length(m)) sub(paste0("^--", k, "="), "", m[1]) else d
}
hasflag <- function(k) any(a == paste0("--", k))

lib   <- getarg("lib");  arm <- getarg("arm", "unknown")
src   <- getarg("src");  out <- getarg("out", "bench_results.csv")
block <- as.integer(getarg("block", "1"))
only  <- getarg("only"); only <- if (is.null(only)) NULL else strsplit(only, ",")[[1]]
if (is.null(lib)) stop("--lib=<library path> is required")

# Thread pinning. The engine workloads run in THIS process, which nothing in
# the package pins -- run_MOSAIC() pins its workers, but a bare run_simulation()
# inherits whatever the session had. Unpinned BLAS/arrow threads are a
# comparability nuisance across arms even where they are not a correctness one.
Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
           NUMEXPR_NUM_THREADS = "1", TBB_NUM_THREADS = "1", NUMBA_NUM_THREADS = "1",
           ARROW_NUM_THREADS = "1")

suppressMessages(library(MOSAIC, lib.loc = lib))
here <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))
source(file.path(here, "compat.R"));   source(file.path(here, "harness.R"))
source(file.path(here, "workloads.R")); source(file.path(here, "workloads_heavy.R"))

tryCatch(MOSAIC::set_root_directory("~/MOSAIC"), error = function(e) NULL)
has_root <- !is.null(getOption("root_directory")) &&
            !inherits(try(MOSAIC::get_paths(), silent = TRUE), "try-error")

cfg_path <- system.file("extdata", "config_default.json", package = "MOSAIC", lib.loc = lib)
cfg_full <- jsonlite::fromJSON(cfg_path, simplifyVector = TRUE)
T_full   <- as.integer(as.Date(cfg_full$date_stop) - as.Date(cfg_full$date_start)) + 1L

ctx <- list(
  lib = lib, fixture_dir = file.path(here, "fixtures"),
  config_full = cfg_full, config_path = cfg_path, T_full = T_full,
  reps = list(calibrator = 5L, anchor = 15L, path = 10L, small = 10L,
              component = 15L, worker = 10L, calib = as.integer(Sys.getenv('BENCH_CALIB_REPS','1'))),
  k_sweep = c(1L, 4L, 8L), calib_n = as.integer(Sys.getenv('BENCH_CALIB_N','200')), calib_cores = as.integer(Sys.getenv('BENCH_CALIB_CORES','8')),
  calibrator_passes = 15L
)
# A fixed prediction matrix for the likelihood workload, so its input does not
# depend on which engine produced it.
set.seed(99); ctx$pred <- list(
  rc = matrix(rpois(length(cfg_full$reported_cases), 5), nrow = nrow(cfg_full$reported_cases)),
  rd = matrix(rpois(length(cfg_full$reported_deaths), 1), nrow = nrow(cfg_full$reported_deaths)))

meta <- .bench_meta(arm, lib, src, unname(tools::md5sum(cfg_path)))
meta$run_id <- sprintf("%s-%s-b%02d", arm, format(Sys.time(), "%Y%m%d%H%M%S"), block)

wl <- BENCH_WORKLOADS
if (hasflag("parallel")) wl <- c(wl, BENCH_WORKLOADS_HEAVY[1])
if (hasflag("calib"))    wl <- c(wl, BENCH_WORKLOADS_HEAVY[2])
if (!is.null(only))      wl <- Filter(function(w) w$id %in% only, wl)

# Calibrator first: it is the session's machine-speed reference and every row
# carries it, so a row measured now stays comparable to one measured months
# from now on a differently-loaded host.
cal <- NA_real_
rows <- list()
for (w in wl) {
  if ("root" %in% w$needs && !has_root) {
    message(sprintf("  SKIP %-26s (no root_directory)", w$id)); next
  }
  r <- tryCatch(w$fn(ctx), error = function(e) {
    message(sprintf("  FAIL %-26s %s", w$id, substr(conditionMessage(e), 1, 120))); NULL })
  if (is.null(r)) next
  if (identical(w$id, "machine/calibrator")) cal <- min(r$reps_s)
  rows[[length(rows) + 1L]] <- .bench_rows(meta, block, w$id, r$reps_s, r$extra)
  message(sprintf("  %-26s min=%.4f s  (n=%d)", w$id, min(r$reps_s), length(r$reps_s)))
}
df <- do.call(rbind, rows)
df$calibrator_s <- cal
# Header on first write only. Keyed on SIZE, not existence: a driver that
# truncates the target with `: > out` creates a zero-byte file, so
# `!file.exists()` is already FALSE on the first append and the header is
# silently never written.
first <- !file.exists(out) || file.size(out) == 0
write.table(df, out, sep = ",", row.names = FALSE, qmethod = "double",
            col.names = first, append = !first)
message(sprintf("[%s] block %d -> %d rows", arm, block, nrow(df)))
