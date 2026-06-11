# Regression tests for the renamed-control-parameter migration in
# .mosaic_validate_and_merge_control() (deprecation shim fixed in v0.37.1).
#
# Bug: the shim copied a legacy name to its canonical name only when the
# canonical key was NULL, but mosaic_control_defaults() always populates the
# canonical key. After the deep-merge the canonical key was non-NULL, so the
# legacy value was silently dropped (run reverted to defaults) and no
# deprecation warning fired. The fix keys detection off the user's original
# control and treats the canonical key as "user-set" only when it differs from
# the pristine default.

norm <- MOSAIC:::.mosaic_validate_and_merge_control

test_that("legacy calibration names set on top of defaults are honoured (+ warn)", {
  ctl <- mosaic_control_defaults()
  ctl$calibration$batch_size      <- 250
  ctl$calibration$min_batches     <- 3
  ctl$calibration$max_batches     <- 10
  ctl$calibration$target_r2       <- 0.95
  ctl$calibration$max_simulations <- 1e6

  res <- suppressWarnings(norm(ctl))
  expect_equal(res$calibration$batch_size_adaptive,   250)
  expect_equal(res$calibration$min_batches_adaptive,  3)
  expect_equal(res$calibration$max_batches_adaptive,  10)
  expect_equal(res$calibration$target_r2_adaptive,    0.95)
  expect_equal(res$calibration$max_simulations_total, 1e6)

  # legacy keys are removed (use exact [[ ]] to avoid $ partial matching)
  expect_null(res$calibration[["batch_size"]])
  expect_null(res$calibration[["max_simulations"]])
  expect_false("target_r2" %in% names(res$calibration))
})

test_that("legacy ESS name (lowercase) is migrated to ESS_method", {
  ctl <- mosaic_control_defaults()
  ctl$targets$ess_method <- "kish"
  res <- suppressWarnings(norm(ctl))
  expect_equal(res$targets$ESS_method, "kish")
  expect_null(res$targets[["ess_method"]])
})

test_that("a deprecation warning is emitted for each legacy name", {
  ctl <- mosaic_control_defaults()
  ctl$calibration$max_simulations <- 1e6
  expect_warning(norm(ctl), "max_simulations is deprecated")
})

test_that("when both legacy and canonical are user-set, canonical wins (+ warn)", {
  # raw partial control with both names genuinely supplied
  ctl <- list(calibration = list(max_simulations = 1e6, max_simulations_total = 7777))
  expect_warning(res <- norm(ctl), "ignored")
  expect_equal(res$calibration$max_simulations_total, 7777)
})

test_that("canonical names alone produce no deprecation warnings", {
  ctl <- mosaic_control_defaults()
  ctl$calibration$max_batches_adaptive <- 11
  ctl$targets$ESS_method <- "kish"
  expect_no_warning(res <- norm(ctl))
  expect_equal(res$calibration$max_batches_adaptive, 11)
  expect_equal(res$targets$ESS_method, "kish")
})

test_that("plain defaults pass through without warnings", {
  expect_no_warning(norm(mosaic_control_defaults()))
})
