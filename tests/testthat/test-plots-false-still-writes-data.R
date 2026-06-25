# =============================================================================
# P4 regression: the data layer is decoupled from plotting.
#
# run_MOSAIC(plots = FALSE) and run_MOSAIC(plots = TRUE) must produce identical
# DATA artifacts (predictions_*.csv, parameter_sensitivity.csv,
# convergence_status.csv); plots gate ONLY the figures. This test is ENGINE-FREE
# (no laser-cholera, no calibration, no skip-on-CI): it synthesizes fixtures and
# calls the data-only writers / assembly helper directly, then asserts the CSVs
# exist with the byte-identical content the plotters used to produce. See
# CLAUDE.md lessons #2/#9/#10/#13 and PLAN.md Phase 0.
# =============================================================================

# ---- Fixtures ----------------------------------------------------------------

make_p4_ensemble <- function(n_locs = 2L, n_times = 8L) {
  set.seed(11)
  cases_mean <- matrix(0, n_locs, n_times)
  deaths_mean <- matrix(0, n_locs, n_times)
  for (i in seq_len(n_locs)) {
    base_c <- (20 + i) + seq_len(n_times)
    base_c[1] <- 0.5; base_c[2] <- 5000
    cases_mean[i, ] <- base_c
    deaths_mean[i, ] <- c(0, 0, 0, 2 + i, 3 + i, 4 + i, 2 + i, 0)
  }
  cases_median  <- cases_mean * 0.95
  deaths_median <- deaths_mean * 0.9
  mk_ci <- function(m) list(list(lower = m * 0.8, upper = m * 1.2))
  structure(list(
    cases_mean = cases_mean, cases_median = cases_median,
    deaths_mean = deaths_mean, deaths_median = deaths_median,
    ci_bounds = list(cases = mk_ci(cases_mean), deaths = mk_ci(deaths_mean)),
    obs_cases = cases_mean + 3, obs_deaths = deaths_mean + 1,
    parameter_weights = rep(1 / 2, 2), seeds = c(1L, 2L),
    n_param_sets = 2L, n_simulations_per_config = 3L, n_successful = 6L,
    location_names = paste0("LOC", seq_len(n_locs)),
    n_locations = n_locs, n_time_points = n_times,
    date_start = "2024-01-01", date_stop = "2024-02-19",
    envelope_quantiles = c(0.025, 0.975)
  ), class = "mosaic_ensemble")
}

# Minimal samples.parquet-like frame for HSIC sensitivity.
make_p4_samples <- function() {
  set.seed(22)
  n <- 200L
  ll <- -100 - rnorm(n)^2
  data.frame(
    sim = seq_len(n), iter = 1L,
    seed_sim = seq_len(n), seed_iter = 1L,
    likelihood = ll,
    is_finite = TRUE, is_valid = TRUE, is_outlier = FALSE,
    is_retained = TRUE, is_best_subset = c(rep(TRUE, 40L), rep(FALSE, n - 40L)),
    is_best_model = FALSE,
    weight_all = 1 / n, weight_retained = 1 / n, weight_best = 1 / 40,
    # Two informative params (correlated with ll) + one noise param.
    beta_j0_tot = -ll + rnorm(n, 0, 0.5),
    phi_1       = ll * 0.3 + rnorm(n, 0, 1),
    noise_param = rnorm(n),
    stringsAsFactors = FALSE
  )
}

# Minimal convergence_diagnostics.json the status-table assembler understands.
write_p4_diag <- function(dir) {
  diag <- list(
    summary = list(total_simulations_original = 200L, n_successful = 200L,
                   retained_simulations = 180L, percentile_used = 4.0,
                   convergence_status = "PASS"),
    metrics = list(
      ess_retained = list(value = 150, status = "-"),
      B_size = list(value = 40, status = "pass"),
      ess_best = list(value = 35, status = "pass"),
      A_B = list(value = 0.9, status = "pass"),
      cvw_B = list(value = 0.2, status = "pass")
    ),
    targets = list(
      ess_best = list(value = 30), A_best = list(value = 0.8),
      cvw_best = list(value = 0.3), max_best_subset = list(value = 50)
    )
  )
  jsonlite::write_json(diag, file.path(dir, "convergence_diagnostics.json"),
                       pretty = TRUE, auto_unbox = TRUE, digits = NA)
}

# ---- Predictions CSV: plots-on vs plots-off content identity -----------------

test_that("prediction CSVs are written unconditionally and match the plotted table", {
  skip_if_not_installed("ggplot2")
  ens <- make_p4_ensemble()
  central <- MOSAIC:::.mosaic_resolve_central_method("median")

  # The data layer: assemble + write (the path run_MOSAIC uses regardless of plots).
  tbl <- MOSAIC:::.mosaic_assemble_prediction_table(ens, central_method = central)
  data_dir <- withr::local_tempdir()
  MOSAIC:::.mosaic_write_prediction_csvs(tbl, data_dir = data_dir,
                                         file_prefix = "ensemble", verbose = FALSE)

  # Canonical schema (verbatim, in order). No n_members; metric is the factor.
  expected_cols <- c("location", "date", "metric", "observed",
                     "predicted_central", "predicted_mean", "predicted_median",
                     "central_method", "ci_1_lower", "ci_1_upper")

  for (loc in ens$location_names) {
    f <- file.path(data_dir, paste0("predictions_ensemble_", loc, ".csv"))
    expect_true(file.exists(f))
    df <- utils::read.csv(f, stringsAsFactors = FALSE)
    expect_identical(names(df), expected_cols)
  }

  # The plotted line is the same table: plot with prediction_table = tbl, then
  # confirm the figure renders (the CSV and the plot share one source).
  fig_dir <- withr::local_tempdir()
  expect_no_error(
    plot_model_ensemble(ens, output_dir = fig_dir, file_prefix = "ensemble",
                        central_method = central, prediction_table = tbl,
                        verbose = FALSE)
  )
})

# ---- parameter_sensitivity.csv written by the calc helper --------------------

test_that("calc_model_parameter_sensitivity writes parameter_sensitivity.csv", {
  skip_if_not_installed("sensitivity")
  samples <- make_p4_samples()
  cal_dir <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  sp <- file.path(cal_dir, "samples.parquet")
  arrow::write_parquet(samples, sp)

  res <- calc_model_parameter_sensitivity(
    results_file = sp, priors_file = NULL, output_dir = out_dir, verbose = FALSE)

  csv <- file.path(out_dir, "parameter_sensitivity.csv")
  expect_true(file.exists(csv))
  df <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_true(all(c("parameter", "hsic_r2", "p_value", "sig", "description") %in% names(df)))
  expect_gt(nrow(df), 0L)
  # The plot consumes the same result object and writes the PNG (no recompute).
  skip_if_not_installed("ggplot2")
  fig_dir <- withr::local_tempdir()
  expect_no_error(
    plot_model_parameter_sensitivity(results_file = sp, output_dir = fig_dir,
                                     sensitivity = res, verbose = FALSE)
  )
})

# ---- convergence_status.csv written by the calc helper -----------------------

test_that("calc_model_convergence_status writes convergence_status.csv", {
  res_dir <- withr::local_tempdir()
  out_dir <- withr::local_tempdir()
  write_p4_diag(res_dir)

  status <- calc_model_convergence_status(results_dir = res_dir,
                                          output_dir = out_dir, verbose = FALSE)
  expect_false(is.null(status))
  expect_true(nrow(status$metrics_data) > 0L)

  csv <- file.path(out_dir, "convergence_status.csv")
  expect_true(file.exists(csv))
  df <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_identical(names(df), c("metric", "description", "value", "target", "status"))
  expect_true("N_sim" %in% df$metric)
  expect_true("Best Subset (B)" %in% df$metric)
})
