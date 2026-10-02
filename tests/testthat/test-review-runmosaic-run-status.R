# =============================================================================
# test-review-runmosaic-run-status.R
#
# Fixed-mode runs never evaluate the calibration ESS criterion, yet reported
# status=completed_unconverged, indistinguishable from a failed auto run. They
# now report status completed_fixed and convergence_evaluated = FALSE, while
# `converged` stays a strict logical (FALSE) so the documented return contract
# of run_MOSAIC() is unchanged. The post-hoc tier result is recorded in
# summary.json as posthoc_criteria_met.
# =============================================================================

test_that("fixed mode reports an unevaluated criterion and its own status", {
  fx <- list(mode = "fixed", converged = FALSE)
  expect_identical(MOSAIC:::.mosaic_run_converged(fx), FALSE)
  expect_identical(MOSAIC:::.mosaic_run_converged(list(mode = "fixed", converged = TRUE)), FALSE)
  expect_identical(MOSAIC:::.mosaic_convergence_evaluated(fx), FALSE)
  expect_identical(MOSAIC:::.mosaic_run_status(fx, TRUE), "completed_fixed")
  expect_identical(MOSAIC:::.mosaic_run_status(fx, FALSE), "completed_fixed_partial")
})

test_that("auto mode status tree is unchanged", {
  expect_identical(MOSAIC:::.mosaic_run_converged(list(mode = "auto", converged = FALSE)), FALSE)
  expect_identical(MOSAIC:::.mosaic_run_converged(list(mode = "auto", converged = TRUE)), TRUE)
  expect_identical(MOSAIC:::.mosaic_convergence_evaluated(list(mode = "auto")), TRUE)
  expect_identical(MOSAIC:::.mosaic_run_status(list(mode = "auto", converged = FALSE), TRUE),
                   "completed_unconverged")
  expect_identical(MOSAIC:::.mosaic_run_status(list(mode = "auto", converged = TRUE), FALSE),
                   "success_partial")
  expect_identical(MOSAIC:::.mosaic_run_status(list(mode = "auto", converged = TRUE), TRUE),
                   "success")
})

test_that("summary.json keeps converged logical in fixed mode and adds the new fields", {
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(withr::local_tempdir(), clean_output = FALSE)
  state <- list(mode = "fixed", converged = FALSE, batch_number = 1L,
                total_sims_run = 10L, total_sims_successful = 10L)
  cfg <- list(location_name = "MOZ", date_start = "2023-01-01", date_stop = "2023-12-31")
  s <- MOSAIC:::.mosaic_write_summary_json(dirs, state, Sys.time(), cfg,
                                           posthoc_criteria_met = TRUE,
                                           io = mosaic_control_defaults()$io)
  expect_identical(s$converged, FALSE)
  expect_identical(s$convergence_evaluated, FALSE)
  expect_identical(s$posthoc_criteria_met, TRUE)
  js <- jsonlite::read_json(file.path(dirs$results, "summary.json"))
  expect_identical(js$converged, FALSE)
  expect_identical(js$convergence_evaluated, FALSE)
  expect_true(isTRUE(js$posthoc_criteria_met))
})

test_that("summary.json reports the dispersion block per channel, panel-trend count included", {
  # Release red team TA-05: the v0.101.0 gate reads nb_dispersion from summary.json.
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(withr::local_tempdir(), clean_output = FALSE)
  state <- list(mode = "fixed", converged = FALSE, batch_number = 1L,
                total_sims_run = 10L, total_sims_successful = 10L)
  cfg <- list(location_name = c("AAA", "BBB", "CCC"), date_start = "2023-01-01",
              date_stop = "2023-12-31")
  tab <- data.frame(channel  = rep(c("cases", "deaths"), each = 3L),
                    location = rep(c("AAA", "BBB", "CCC"), 2L),
                    k        = c(0.5, 1.5, Inf, 2, 0.1, 4),
                    status   = c("ok", "no_estimate_collapsed", "poisson",
                                 "ok", "clamped_lower_bound", "ok"),
                    panel_trend = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
                    stringsAsFactors = FALSE)
  write <- function(t) MOSAIC:::.mosaic_write_summary_json(dirs, state, Sys.time(), cfg,
                                                           nb_dispersion = t,
                                                           io = mosaic_control_defaults()$io)
  nb <- write(tab)$nb_dispersion
  expect_identical(nb$cases, list(median_k = 1, n_estimated = 2L, n_poisson = 1L, n_panel_trend = 1L))
  expect_identical(nb$deaths, list(median_k = 2, n_estimated = 3L, n_poisson = 0L, n_panel_trend = 0L))
  expect_identical(nb$bound_binds, 1L)
  expect_identical(nb$status_counts, list(clamped_lower_bound = 1L, no_estimate_collapsed = 1L,
                                          ok = 3L, poisson = 1L))
  js <- jsonlite::read_json(file.path(dirs$results, "summary.json"))
  expect_identical(js$nb_dispersion$cases$n_panel_trend, 1L)
  expect_identical(js$nb_dispersion$cases$n_poisson, 1L)
  expect_identical(js$nb_dispersion$status_counts$ok, 3L)
  # A table written before panel_trend existed counts no panel-trend locations.
  legacy <- tab; legacy$panel_trend <- NULL
  nb0 <- write(legacy)$nb_dispersion
  expect_identical(nb0$cases, list(median_k = 1, n_estimated = 2L, n_poisson = 1L, n_panel_trend = 0L))
  expect_identical(nb0$deaths$n_panel_trend, 0L)
})
