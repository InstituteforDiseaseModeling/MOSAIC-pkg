# =============================================================================
# test-review-runmosaic-ensemble-cluster.R
#
# With a caller-supplied cluster the post-calibration ensembles start a second
# cluster while the caller's is alive. The size is clamped to the free R
# connections and the run log says both clusters are resident.
# =============================================================================

test_that("without a supplied cluster the plan follows control$parallel", {
  ctl <- list(parallel = list(enable = TRUE, n_cores = 4L))
  p <- MOSAIC:::.mosaic_ensemble_parallel_plan(NULL, ctl)
  expect_identical(p$parallel, TRUE)
  expect_identical(p$n_cores, 4L)
  expect_null(p$note)
})

test_that("a supplied cluster is clamped to the free connections and noted", {
  fake_cl <- structure(vector("list", 100L), class = c("SOCKcluster", "cluster"))
  ctl <- list(parallel = list(enable = FALSE, n_cores = 1L))
  local_mocked_bindings(freeConnections = function() 25L, .package = "parallelly")
  p <- MOSAIC:::.mosaic_ensemble_parallel_plan(fake_cl, ctl)
  expect_identical(p$parallel, TRUE)
  expect_identical(p$n_cores, 23L)
  expect_match(p$note, "stays alive")
  expect_match(p$note, "reduced from 100")
})
