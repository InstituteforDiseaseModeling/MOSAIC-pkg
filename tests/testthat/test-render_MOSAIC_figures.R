# =============================================================================
# render_MOSAIC_figures() round-trip + invariant P5 (pure read-render).
#
# Build a minimal FINISHED dir_output on disk (no calibration), run the renderer,
# assert figures appear, and assert NO simulation is triggered: calc_model_ensemble,
# run_LASER, and sample_parameters are mocked to error -- if the renderer ever
# falls back to rebuilding an ensemble it would call calc_model_ensemble (which
# re-simulates locally per calc_model_ensemble.R:551/601), failing this test.
# =============================================================================

# Build a self-describing finished run directory with just enough artifacts for
# the "predictions" + "convergence" + "ppc" groups.
build_min_dir_output <- function(root) {
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(root, clean_output = FALSE)

  # --- ensemble fixture + persisted (stamped) .rds --------------------------
  n_locs <- 1L; n_times <- 8L
  cm <- matrix((10:17), n_locs, n_times)
  dm <- matrix(c(0, 0, 1, 2, 3, 2, 1, 0), n_locs, n_times)
  mk_ci <- function(m) list(list(lower = m * 0.8, upper = m * 1.2))
  ens <- structure(list(
    cases_mean = cm, cases_median = cm, deaths_mean = dm, deaths_median = dm,
    ci_bounds = list(cases = mk_ci(cm), deaths = mk_ci(dm)),
    obs_cases = cm + 2, obs_deaths = dm + 1,
    parameter_weights = 1, seeds = 1L,
    n_param_sets = 1L, n_simulations_per_config = 3L, n_successful = 3L,
    location_names = "TST", n_locations = n_locs, n_time_points = n_times,
    date_start = "2024-01-01", date_stop = "2024-02-19",
    envelope_quantiles = c(0.025, 0.975)
  ), class = "mosaic_ensemble")
  ens <- MOSAIC:::.mosaic_stamp_artifact(ens)
  saveRDS(ens, file.path(dirs$calibration, "ensemble_candidate.rds"))
  saveRDS(ens, file.path(dirs$calibration, "medoid_ensemble.rds"))

  # --- prediction CSVs (data layer already written by run_MOSAIC) -----------
  tbl <- MOSAIC:::.mosaic_assemble_prediction_table(ens, central_method = "median")
  MOSAIC:::.mosaic_write_prediction_csvs(tbl, data_dir = dirs$res_predictions,
                                         file_prefix = "ensemble", verbose = FALSE)
  MOSAIC:::.mosaic_write_prediction_csvs(tbl, data_dir = dirs$res_predictions,
                                         file_prefix = "medoid", verbose = FALSE)

  # --- control.json so central_method resolves -------------------------------
  jsonlite::write_json(list(predictions = list(central_method = "median")),
                       file.path(dirs$inputs, "control.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  dirs
}

test_that("render_MOSAIC_figures renders predictions without triggering simulation", {
  skip_if_not_installed("ggplot2")
  root <- withr::local_tempdir()
  dirs <- build_min_dir_output(root)

  # P5 guard: any re-simulation entry point is fatal.
  testthat::local_mocked_bindings(
    calc_model_ensemble = function(...) stop("P5 VIOLATION: calc_model_ensemble called"),
    run_LASER           = function(...) stop("P5 VIOLATION: run_LASER called"),
    sample_parameters   = function(...) stop("P5 VIOLATION: sample_parameters called")
  )

  expect_no_error(
    attempted <- render_MOSAIC_figures(root, which = "predictions", verbose = FALSE)
  )
  expect_true(attempted[["predictions"]])

  # Ensemble + medoid figures appear (per-location PDFs).
  pdfs <- list.files(dirs$res_fig_pred, pattern = "\\.pdf$")
  expect_true(any(grepl("predictions_ensemble_TST", pdfs)))
  expect_true(any(grepl("predictions_medoid_TST", pdfs)))
})

test_that("render_MOSAIC_figures(plots = FALSE) renders nothing", {
  root <- withr::local_tempdir()
  dirs <- build_min_dir_output(root)
  res <- render_MOSAIC_figures(root, plots = FALSE, verbose = FALSE)
  expect_length(res, 0L)
  expect_length(list.files(dirs$res_fig_pred, pattern = "\\.pdf$"), 0L)
})

test_that("render_MOSAIC_figures warns + skips on a missing ensemble artifact", {
  skip_if_not_installed("ggplot2")
  root <- withr::local_tempdir()
  dirs <- MOSAIC:::.mosaic_ensure_dir_tree(root, clean_output = FALSE)
  # No ensemble .rds, no medoid .rds, no CSVs.
  testthat::local_mocked_bindings(
    calc_model_ensemble = function(...) stop("P5 VIOLATION"),
    run_LASER           = function(...) stop("P5 VIOLATION"),
    sample_parameters   = function(...) stop("P5 VIOLATION")
  )
  expect_warning(
    render_MOSAIC_figures(root, which = "predictions", verbose = FALSE),
    "no ensemble .rds found"
  )
})

test_that("render_MOSAIC_figures rejects unknown figure groups", {
  root <- withr::local_tempdir()
  MOSAIC:::.mosaic_ensure_dir_tree(root, clean_output = FALSE)
  expect_error(
    render_MOSAIC_figures(root, which = "not_a_group", verbose = FALSE),
    "Unknown figure group"
  )
})

test_that("render_MOSAIC_figures warns + skips a schema-incompatible artifact", {
  skip_if_not_installed("ggplot2")
  root <- withr::local_tempdir()
  dirs <- build_min_dir_output(root)
  # Stamp the ensemble with a FUTURE schema version -> renderer must skip.
  ens <- readRDS(file.path(dirs$calibration, "ensemble_candidate.rds"))
  attr(ens, "mosaic_schema_version") <- MOSAIC:::.MOSAIC_ARTIFACT_SCHEMA_VERSION + 100L
  saveRDS(ens, file.path(dirs$calibration, "ensemble_candidate.rds"))
  # Remove medoid so only the (incompatible) ensemble path is exercised.
  file.remove(file.path(dirs$calibration, "medoid_ensemble.rds"))

  testthat::local_mocked_bindings(
    calc_model_ensemble = function(...) stop("P5 VIOLATION"),
    run_LASER           = function(...) stop("P5 VIOLATION"),
    sample_parameters   = function(...) stop("P5 VIOLATION")
  )
  expect_warning(
    render_MOSAIC_figures(root, which = "predictions", verbose = FALSE),
    "schema"
  )
})
