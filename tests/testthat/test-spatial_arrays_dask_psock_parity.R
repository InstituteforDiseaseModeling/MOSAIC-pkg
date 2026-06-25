# Phase-C gate (ii): Dask <-> PSOCK serialization round-trip parity for the
# engine spatial-structure arrays (spatial_hazard J x T, coupling J x J, pi_ij
# J x J). The local PSOCK path keeps the engine arrays as R matrices directly;
# the Dask path ships them as numpy.tolist() nested lists and reconstructs them
# with matrix(unlist(x), nrow = length(x), byrow = TRUE) (the .relist() logic in
# .mosaic_postca_dask_ensemble). Both must yield the IDENTICAL R matrix from one
# engine run at a fixed seed.
#
# A real Dask cluster is not run here (none available locally); this test
# exercises the serialization contract in-process. The full Dask path still
# needs VM/cluster validation.

skip_if_no_engine <- function() {
  testthat::skip_if_not_installed("reticulate")
  testthat::skip_on_cran()
  if (!isTRUE(getOption("mosaic.test.py_available")))
    testthat::skip("Python not available via reticulate")
  if (!isTRUE(getOption("mosaic.test.has_engine")))
    testthat::skip("laser.cholera.metapop.model not installed")
}

test_that("spatial arrays survive the Dask tolist()->R reconstruction identically", {
  skip_if_no_engine()

  do.call(Sys.setenv, stats::setNames(
    as.list(rep("1", 6)),
    c("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS",
      "NUMEXPR_NUM_THREADS", "TBB_NUM_THREADS", "NUMBA_NUM_THREADS")))

  cfg <- jsonlite::fromJSON(
    system.file("extdata", "config_default.json", package = "MOSAIC"),
    simplifyVector = TRUE)
  cfg$seed <- 123L

  lc <- tryCatch(reticulate::import("laser.cholera.metapop.model"),
                 error = function(e) NULL)
  skip_if(is.null(lc), "engine import failed")

  model <- tryCatch(
    lc$run_model(paramfile = MOSAIC:::.mosaic_prepare_config_for_python(cfg),
                 quiet = TRUE),
    error = function(e) NULL)
  skip_if(is.null(model), "engine run failed")

  # ---- Local PSOCK path: engine arrays as R matrices (ground truth) ---------
  local_H   <- as.matrix(model$results$spatial_hazard)
  local_C   <- as.matrix(model$results$coupling)
  local_pij <- as.matrix(model$results$pi_ij)

  skip_if(any(dim(local_H) == 0L), "engine produced empty spatial_hazard")

  J <- length(cfg$location_name)
  expect_equal(nrow(local_H), J)            # J x T hazard
  expect_equal(dim(local_C), c(J, J))       # J x J coupling
  expect_equal(dim(local_pij), c(J, J))     # J x J pi_ij

  # ---- Dask path: numpy.tolist() then R reconstruction ----------------------
  np <- reticulate::import("numpy", convert = FALSE)
  .roundtrip <- function(py_arr) {
    lst <- reticulate::py_to_r(np$array(py_arr)$tolist())  # nested R list
    matrix(unlist(lst), nrow = length(lst), byrow = TRUE)   # .relist() logic
  }
  dask_H   <- .roundtrip(model$results$spatial_hazard)
  dask_C   <- .roundtrip(model$results$coupling)
  dask_pij <- .roundtrip(model$results$pi_ij)

  # NaN cells in coupling must survive identically (DM#4), so compare with
  # NaN treated as equal.
  expect_equal(dask_H,   local_H,   tolerance = 1e-12)
  expect_equal(dask_C,   local_C,   tolerance = 1e-12)
  expect_equal(dask_pij, local_pij, tolerance = 1e-12)
})
