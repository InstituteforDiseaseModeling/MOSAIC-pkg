# =============================================================================
# test-review-runmosaic-run-status.R
#
# Fixed-mode runs never evaluate the calibration ESS criterion, yet always
# reported converged=false and status=completed_unconverged. They now report
# converged = NA and status completed_fixed; the post-hoc tier result is
# recorded in summary.json as posthoc_criteria_met.
# =============================================================================

test_that("fixed mode reports NA convergence and its own status", {
  fx <- list(mode = "fixed", converged = FALSE)
  expect_identical(MOSAIC:::.mosaic_run_converged(fx), NA)
  expect_identical(MOSAIC:::.mosaic_run_status(fx, TRUE), "completed_fixed")
  expect_identical(MOSAIC:::.mosaic_run_status(fx, FALSE), "completed_fixed_partial")
})

test_that("auto mode status tree is unchanged", {
  expect_identical(MOSAIC:::.mosaic_run_converged(list(mode = "auto", converged = FALSE)), FALSE)
  expect_identical(MOSAIC:::.mosaic_run_status(list(mode = "auto", converged = FALSE), TRUE),
                   "completed_unconverged")
  expect_identical(MOSAIC:::.mosaic_run_status(list(mode = "auto", converged = TRUE), FALSE),
                   "success_partial")
  expect_identical(MOSAIC:::.mosaic_run_status(list(mode = "auto", converged = TRUE), TRUE),
                   "success")
})

test_that("summary.json writes converged = null in fixed mode and the post-hoc result", {
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(withr::local_tempdir(), clean_output = FALSE)
  state <- list(mode = "fixed", converged = FALSE, batch_number = 1L,
                total_sims_run = 10L, total_sims_successful = 10L)
  cfg <- list(location_name = "MOZ", date_start = "2023-01-01", date_stop = "2023-12-31")
  s <- MOSAIC:::.mosaic_write_summary_json(dirs, state, Sys.time(), cfg,
                                           posthoc_criteria_met = TRUE,
                                           io = mosaic_control_defaults()$io)
  expect_identical(s$converged, NA)
  expect_identical(s$posthoc_criteria_met, TRUE)
  js <- jsonlite::read_json(file.path(dirs$results, "summary.json"))
  expect_true("converged" %in% names(js))
  expect_null(js$converged)
  expect_true(isTRUE(js$posthoc_criteria_met))
})
