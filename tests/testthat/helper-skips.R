# =============================================================================
# helper-skips.R -- single source of truth for the suite's skip helpers.
#
# These were previously duplicated inline across ~5 test files. Centralizing
# them (testthat auto-sources helper-*.R before tests) removes the drift risk
# and lets the Python-capability helpers read the ONE-TIME cached probe from
# setup-python.R (options(mosaic.test.*)) instead of re-probing per test.
# =============================================================================

# --- Python-capability skips -------------------------------------------------
#
# There is exactly one of these left. skip_if_no_python_likelihood() and the
# PY_LIKELIHOOD_MODULE constant were removed in v0.67.0: they gated the
# R-vs-Python calc_model_likelihood parity tests, which went with the Python
# engine, leaving the helper with zero callers. The eager probe in
# setup-python.R that fed it went at the same time.
#
# TensorFlow is the only Python capability the suite still cares about, because
# it is the only one the package still uses (the suitability model).

# Skip when the Python tensorflow module is unavailable (most machines that are
# not a suitability box). Keeps the suite portable; a no-op where TF is installed.
#
# LAZY PROBE: importing tensorflow costs ~10s, so it is deliberately NOT probed
# at startup (no fast-tier test reads the flag). The probe is performed here on
# first call and cached in options(mosaic.test.has_tensorflow) so subsequent
# calls in the same process are free. Only tests that actually need TF pay the
# cost, and only when they run.
skip_without_tensorflow <- function() {
  testthat::skip_if_not_installed("reticulate")
  has_tf <- getOption("mosaic.test.has_tensorflow")  # NULL until first probe
  if (is.null(has_tf)) {
    has_tf <- isTRUE(tryCatch(
      reticulate::py_available(initialize = TRUE) &&
        reticulate::py_module_available("tensorflow"),
      error = function(e) FALSE))
    options(mosaic.test.has_tensorflow = has_tf)
  }
  if (!isTRUE(has_tf)) {
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

# --- Oracle replay-fixture skips ---------------------------------------------

# The Tier B replay fixtures are frozen recordings of the Python laser-cholera
# oracle (see fixtures/ORACLE.md). They live here rather than in the test file
# that consumes most of them because test-sim_params.R needs them too, and a
# helper defined at the top of one test file is not reliably in scope in
# another. test-sim_engine_replay.R asserts the full inventory in one place, so
# a missing fixture fails loudly there rather than only thinning coverage here.

fixture_path <- function(name) {
  testthat::test_path("fixtures", paste0(name, ".rds"))
}

skip_if_no_fixture <- function(name) {
  if (!file.exists(fixture_path(name))) {
    testthat::skip(sprintf("replay fixture '%s' not committed", name))
  }
}
