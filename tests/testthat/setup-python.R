# =============================================================================
# setup-python.R -- one-time, per-process test environment setup.
#
# testthat auto-sources every setup-*.R ONCE per worker process (serial run:
# once; parallel run: once per PSOCK worker) BEFORE any test file. We use it to:
#
#   (a) Pin the canonical thread-env set to "1" BEFORE any Python interpreter,
#       BLAS, Arrow, or Numba pool can start. This is the CLAUDE.md
#       BLAS/Numba-deadlock landmine: parallel test workers that each spawn
#       LASER/numpy/arrow must not oversubscribe CPU. .mosaic_set_blas_threads()
#       sets OMP/MKL/OPENBLAS/NUMEXPR/TBB/NUMBA + ARROW_NUM_THREADS = "1" --
#       the same values test-dask-local-separation.R / test-dask-psock-
#       orchestrator.R assert, so this is consistent with their expectations.
#
#   (b) Probe the Python interpreter and optional modules ONCE, caching the
#       results in options(). Without this every Python-dependent test pays the
#       reticulate interpreter-init + py_module_available() cost. helper-skips.R
#       reads these cached options() instead of re-probing.
# =============================================================================

# (a) Thread pinning -- must run before anything spins up a thread pool.
# .mosaic_set_blas_threads() delegates to .mosaic_set_all_thread_env(1L), which
# sets all six CLAUDE.md thread vars plus ARROW_NUM_THREADS, all to "1".
suppressWarnings(try(MOSAIC:::.mosaic_set_blas_threads(1L), silent = TRUE))

# (b) One-time Python capability probe, cached in options().
local({
  py_available   <- FALSE
  has_likelihood <- FALSE
  has_tensorflow <- FALSE
  has_engine     <- FALSE

  if (requireNamespace("reticulate", quietly = TRUE)) {
    py_available <- isTRUE(tryCatch(
      reticulate::py_available(initialize = TRUE),
      error = function(e) FALSE))

    if (py_available) {
      mod_ok <- function(m) isTRUE(tryCatch(
        reticulate::py_module_available(m), error = function(e) FALSE))
      has_likelihood <- mod_ok("laser.cholera.calc_model_likelihood")
      has_tensorflow <- mod_ok("tensorflow")
      has_engine     <- mod_ok("laser.cholera.metapop.model")
    }
  }

  options(
    mosaic.test.py_available   = py_available,
    mosaic.test.has_likelihood = has_likelihood,
    mosaic.test.has_tensorflow = has_tensorflow,
    mosaic.test.has_engine     = has_engine
  )
})
