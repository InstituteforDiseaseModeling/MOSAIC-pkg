# Regression tests for the DISPLAY-ONLY boundary-artifact mask in
# plot_model_ensemble(). The mask blanks (a) the final Deaths timestep
# (laser-cholera off-by-one: reported_deaths never written to the final slot)
# and (b) the leading Suspected-Cases warm-up steps (IC transient). It must be
# purely cosmetic: it acts on the exported CSV / rendered lines only and must
# NOT change any R2/bias/likelihood the caller computes from the raw ensemble
# object. See R/plot_model_ensemble.R and CLAUDE.md lessons #11/#12.

# ---------------------------------------------------------------------------
# Fixture: a small, fully-specified mosaic_ensemble object (no LASER needed)
# ---------------------------------------------------------------------------

make_test_ensemble <- function(n_locs = 2L, n_times = 8L) {
  set.seed(42)

  # Deterministic central trajectories with the two boundary artifacts baked in
  # so we can assert they are blanked. All-finite interior so we can assert
  # interior is untouched.
  cases_mean <- matrix(0, n_locs, n_times)
  deaths_mean <- matrix(0, n_locs, n_times)
  for (i in seq_len(n_locs)) {
    base_c <- (10 + i) + seq_len(n_times)            # smooth, all positive
    base_c[1] <- 0.01                                 # warm-up near-zero at t=1
    base_c[2] <- 9999                                 # warm-up transient peak t=2
    cases_mean[i, ] <- base_c

    base_d <- c(0, 0, 0, 0, 0, 3 + i, 4 + i, 0)       # leading reporting-lag
    # zeros are REAL (delta_reporting_deaths); final 0 is the engine artifact.
    deaths_mean[i, ] <- base_d
  }
  cases_median  <- cases_mean
  deaths_median <- deaths_mean

  obs_cases  <- cases_mean + 5            # arbitrary observed (interior finite)
  obs_deaths <- deaths_mean + 1

  mk_ci <- function(m) list(list(
    lower = m * 0.8,
    upper = m * 1.2
  ))

  structure(
    list(
      cases_mean    = cases_mean,
      cases_median  = cases_median,
      deaths_mean   = deaths_mean,
      deaths_median = deaths_median,
      ci_bounds     = list(cases = mk_ci(cases_mean), deaths = mk_ci(deaths_mean)),
      obs_cases     = obs_cases,
      obs_deaths    = obs_deaths,
      cases_array   = NULL,
      deaths_array  = NULL,
      parameter_weights = c(0.5, 0.5),
      seeds         = c(1L, 2L),
      n_param_sets  = 2L,
      n_simulations_per_config = 3L,
      n_successful  = 6L,
      location_names = paste0("LOC", seq_len(n_locs)),
      n_locations    = n_locs,
      n_time_points  = n_times,
      date_start     = "2024-01-01",
      date_stop      = "2024-02-19",
      envelope_quantiles = c(0.025, 0.975)
    ),
    class = "mosaic_ensemble"
  )
}

# Re-derive the exact metrics run_MOSAIC computes from the RAW ensemble object
# (calc_model_ensemble -> ensemble$*_mean matrices -> calc_model_R2 /
# calc_bias_ratio over the flattened location x time grid). These are the
# values written to summary.json.
raw_metrics <- function(ens, channel = c("cases", "deaths")) {
  channel <- match.arg(channel)
  if (channel == "cases") {
    obs  <- as.numeric(ens$obs_cases)
    pred <- as.numeric(ens$cases_mean)
  } else {
    obs  <- as.numeric(ens$obs_deaths)
    pred <- as.numeric(ens$deaths_mean)
  }
  list(
    r2   = calc_model_R2(obs, pred),
    bias = calc_bias_ratio(obs, pred)
  )
}

read_loc_csv <- function(dir, loc, prefix = "ensemble") {
  f <- file.path(dir, paste0("predictions_", prefix, "_", loc, ".csv"))
  expect_true(file.exists(f), info = paste("expected CSV:", f))
  utils::read.csv(f, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# (ii) Metrics from the raw object are unchanged by the mask
# ---------------------------------------------------------------------------

test_that("R2/bias from the raw ensemble are byte-identical with mask on vs off", {
  skip_if_not_installed("ggplot2")
  ens <- make_test_ensemble()

  # Metrics computed from the raw object BEFORE any plotting.
  m_cases_before  <- raw_metrics(ens, "cases")
  m_deaths_before <- raw_metrics(ens, "deaths")

  out_dir <- file.path(tempdir(), "mask_test_metrics")

  # Mask ON (default)
  invisible(plot_model_ensemble(ens, output_dir = out_dir,
                                save_predictions = FALSE,
                                mask_final_deaths_step = TRUE,
                                n_cases_warmup_mask = 2L,
                                verbose = FALSE))

  # Mask OFF
  invisible(plot_model_ensemble(ens, output_dir = out_dir,
                                save_predictions = FALSE,
                                mask_final_deaths_step = FALSE,
                                n_cases_warmup_mask = 0L,
                                verbose = FALSE))

  # The ensemble object itself must be untouched (matrices identical to fixture).
  ens_ref <- make_test_ensemble()
  expect_identical(ens$cases_mean,   ens_ref$cases_mean)
  expect_identical(ens$deaths_mean,  ens_ref$deaths_mean)
  expect_identical(ens$cases_median, ens_ref$cases_median)
  expect_identical(ens$deaths_median, ens_ref$deaths_median)

  # Metrics recomputed from the raw object AFTER plotting are identical.
  m_cases_after  <- raw_metrics(ens, "cases")
  m_deaths_after <- raw_metrics(ens, "deaths")

  expect_identical(m_cases_before$r2,    m_cases_after$r2)
  expect_identical(m_cases_before$bias,  m_cases_after$bias)
  expect_identical(m_deaths_before$r2,   m_deaths_after$r2)
  expect_identical(m_deaths_before$bias, m_deaths_after$bias)
})

# ---------------------------------------------------------------------------
# (i) Boundary cells are NA in the exported predictions
# (iii)/(iv) Deaths reporting-lag zeros + interior values, and cases interior,
#            are untouched
# ---------------------------------------------------------------------------

test_that("mask blanks deaths-final + cases-warmup in CSV and leaves the rest intact", {
  skip_if_not_installed("ggplot2")
  ens <- make_test_ensemble()
  n_t <- ens$n_time_points
  warmup <- 2L

  out_dir  <- file.path(tempdir(), "mask_test_csv_on")
  data_dir <- file.path(out_dir, "predictions")

  invisible(plot_model_ensemble(ens, output_dir = out_dir, data_dir = data_dir,
                                save_predictions = TRUE,
                                mask_final_deaths_step = TRUE,
                                n_cases_warmup_mask = warmup,
                                verbose = FALSE))

  pred_cols <- c("predicted_central", "predicted_mean", "predicted_median",
                 "ci_1_lower", "ci_1_upper")

  for (loc in ens$location_names) {
    df <- read_loc_csv(data_dir, loc)
    cases  <- df[df$metric == "Suspected Cases", ]
    deaths <- df[df$metric == "Deaths", ]

    # (i) cases warm-up steps blanked
    for (cc in pred_cols) {
      expect_true(all(is.na(cases[[cc]][seq_len(warmup)])),
                  info = paste("cases warm-up not NA:", loc, cc))
      # (iv) cases interior untouched (finite)
      expect_true(all(is.finite(cases[[cc]][(warmup + 1L):n_t])),
                  info = paste("cases interior altered:", loc, cc))
    }

    # (i) deaths final step blanked
    for (cc in pred_cols) {
      expect_true(is.na(deaths[[cc]][n_t]),
                  info = paste("deaths final not NA:", loc, cc))
    }

    # (iii) deaths leading reporting-lag zeros are REAL and untouched.
    # Fixture deaths = c(0,0,0,0,0, d6, d7, 0); steps 1-5 are legit zeros.
    expect_equal(deaths$predicted_central[1:5], rep(0, 5),
                 info = paste("deaths leading reporting-lag zeros masked:", loc))
    # (iii) deaths interior non-final values untouched (steps 6,7 finite & >0)
    expect_true(all(is.finite(deaths$predicted_central[6:7])),
                info = paste("deaths interior altered:", loc))
    expect_true(all(deaths$predicted_central[6:7] > 0))
  }
})

test_that("mask is a no-op when both knobs are disabled", {
  skip_if_not_installed("ggplot2")
  ens <- make_test_ensemble()
  n_t <- ens$n_time_points

  out_dir  <- file.path(tempdir(), "mask_test_csv_off")
  data_dir <- file.path(out_dir, "predictions")

  invisible(plot_model_ensemble(ens, output_dir = out_dir, data_dir = data_dir,
                                save_predictions = TRUE,
                                mask_final_deaths_step = FALSE,
                                n_cases_warmup_mask = 0L,
                                verbose = FALSE))

  for (loc in ens$location_names) {
    df <- read_loc_csv(data_dir, loc)
    cases  <- df[df$metric == "Suspected Cases", ]
    deaths <- df[df$metric == "Deaths", ]
    # Nothing blanked: every predicted_central cell is finite (the fixture has
    # no NA inputs).
    expect_true(all(is.finite(cases$predicted_central)))
    expect_true(all(is.finite(deaths$predicted_central)))
    # The engine artifact (deaths final 0) is present, unmasked.
    expect_equal(deaths$predicted_central[n_t], 0)
  }
})

test_that("n_cases_warmup_mask validates its argument", {
  skip_if_not_installed("ggplot2")
  ens <- make_test_ensemble()
  out_dir <- file.path(tempdir(), "mask_test_validate")
  expect_error(
    plot_model_ensemble(ens, output_dir = out_dir, verbose = FALSE,
                        n_cases_warmup_mask = -1L),
    "non-negative integer"
  )
})
