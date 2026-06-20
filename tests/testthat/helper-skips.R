# =============================================================================
# helper-skips.R -- single source of truth for the suite's skip helpers.
#
# These were previously duplicated inline across ~5 test files. Centralizing
# them (testthat auto-sources helper-*.R before tests) removes the drift risk
# and lets the Python-capability helpers read the ONE-TIME cached probe from
# setup-python.R (options(mosaic.test.*)) instead of re-probing per test.
# =============================================================================

# --- Python-capability skips (read cached probe from setup-python.R) ---------

PY_LIKELIHOOD_MODULE <- "laser.cholera.calc_model_likelihood"

# Skip unless the Python laser-cholera likelihood module is importable.
skip_if_no_python_likelihood <- function() {
  testthat::skip_if_not_installed("reticulate")
  testthat::skip_on_cran()
  if (!isTRUE(getOption("mosaic.test.py_available"))) {
    testthat::skip("Python not available via reticulate")
  }
  if (!isTRUE(getOption("mosaic.test.has_likelihood"))) {
    testthat::skip(sprintf("%s not installed (requires laser-cholera >= 0.13.1)",
                           PY_LIKELIHOOD_MODULE))
  }
  invisible(TRUE)
}

# Skip when the Python tensorflow module is unavailable (e.g. the worker image
# strips it). Keeps the suite portable; a no-op where TF is installed.
skip_without_tensorflow <- function() {
  testthat::skip_if_not_installed("reticulate")
  if (!isTRUE(getOption("mosaic.test.has_tensorflow"))) {
    testthat::skip("Python tensorflow module not available")
  }
}

# --- Data-availability skip (RETURNS a fixture list; do not change contract) -

# Loads config_default/priors_default and sets the MOSAIC root, returning a
# list(config=, priors=). Callers use it as `fx <- skip_if_no_data()`.
skip_if_no_data <- function() {
  testthat::skip_if_not_installed("MOSAIC")
  env <- new.env()
  ok <- tryCatch({
    utils::data("config_default", package = "MOSAIC", envir = env)
    utils::data("priors_default", package = "MOSAIC", envir = env)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok || !exists("config_default", envir = env)) {
    testthat::skip("config_default / priors_default not available")
  }
  root <- if (dir.exists("/workspace/MOSAIC")) "/workspace/MOSAIC" else "~/MOSAIC"
  if (!dir.exists(root)) {
    testthat::skip(paste("MOSAIC root not found at", root))
  }
  MOSAIC::set_root_directory(root)
  list(config = env$config_default, priors = env$priors_default)
}

# --- Core-count skip (parallel tests that spawn PSOCK/mclapply clusters) ------

# The OOM projection multiplies by n_workers = min(parallel_seeds, n_seeds,
# cores - 2); on a tiny CI box (<= 3 cores) the clamp drops concurrency to 1 and
# the guard is (correctly) silent, so the small-RAM warn assertions only hold
# with enough cores to actually spawn multiple workers.
skip_if_few_cores <- function(min_workers = 4L) {
  nc <- parallel::detectCores()
  if (is.na(nc)) nc <- 2L
  testthat::skip_if(nc - 2L < min_workers,
                    sprintf("needs >= %d spawnable cores (have %d)",
                            min_workers + 2L, nc))
}

# --- Prior-availability skip (rho_deaths default may predate data-raw rebuild) -

skip_if_no_rho_deaths_prior <- function() {
  testthat::skip_if(is.null(getOption("root_directory")),
                    "MOSAIC root directory not set")
  pri <- tryCatch(MOSAIC::priors_default, error = function(e) NULL)
  if (is.null(pri) || is.null(pri$parameters_global$rho_deaths)) {
    testthat::skip("priors_default$parameters_global$rho_deaths not yet populated (data-raw rebuild pending)")
  }
}

# --- Nested-parallel guard ---------------------------------------------------

# Some tests exercise inner parallelism (parallel::mclapply forks, or a PSOCK
# cluster) inside the function under test. When the WHOLE suite is run under
# Config/testthat/parallel, each test file is itself executed in a testthat
# worker subprocess; an inner fork/cluster spawned there collides with
# testthat's own result IPC and crashes the worker ("unknown type ..." /
# "no restore method available"). These tests skip when the outer suite is
# parallel -- their inner-parallel behavior is still covered by the serial
# fast-tier run and the nightly slow tier (which can run serially). This is the
# CLAUDE.md BLAS/Numba-style nesting landmine: do not nest parallel-over-parallel.
.testthat_running_parallel <- function() {
  # testthat parallel workers are callr subprocesses; callr sets
  # CALLR_IS_RUNNING in every child. It is empty in a serial devtools::test()
  # and in a plain `R CMD check` test process, so this is a reliable "am I a
  # parallel worker?" signal. (testthat::is_parallel() is unreliable inside the
  # worker, so we key off the subprocess marker instead.)
  nzchar(Sys.getenv("CALLR_IS_RUNNING"))
}

skip_if_testthat_parallel <- function() {
  if (isTRUE(.testthat_running_parallel())) {
    testthat::skip("inner-parallel test: skipped under Config/testthat/parallel (runs serially)")
  }
}

# --- Slow-tier gate (Lever 5) ------------------------------------------------

# Skip genuinely-slow, non-engine tests unless MOSAIC_RUN_SLOW_TESTS is set.
# Mirrors the existing MOSAIC_RUN_INTEGRATION / MOSAIC_RUN_KERAS_TESTS gating:
# the default PR / R CMD check run is the fast tier; the scheduled job sets the
# env var to exercise the slow tier.
skip_if_slow <- function() {
  if (!nzchar(Sys.getenv("MOSAIC_RUN_SLOW_TESTS"))) {
    testthat::skip("slow test (set MOSAIC_RUN_SLOW_TESTS=1 to run)")
  }
}
