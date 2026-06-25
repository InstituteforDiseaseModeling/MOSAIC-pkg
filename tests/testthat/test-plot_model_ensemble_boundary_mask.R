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

# The masking regression now exercises the pure assembly helper
# .mosaic_assemble_prediction_table() directly (the masking lives there; the
# CSV is just .mosaic_write_prediction_csvs() of its output). This avoids the
# deprecated plot_model_ensemble(save_predictions=) and tests the data layer
# the way run_MOSAIC() now uses it.
assemble <- function(ens, ...) {
  MOSAIC:::.mosaic_assemble_prediction_table(ens, ...)
}

# ---------------------------------------------------------------------------
# (ii) Metrics from the raw object are unchanged by the mask
# ---------------------------------------------------------------------------

test_that("R2/bias from the raw ensemble are byte-identical with mask on vs off", {
  ens <- make_test_ensemble()

  m_cases_before  <- raw_metrics(ens, "cases")
  m_deaths_before <- raw_metrics(ens, "deaths")

  # Mask ON (default) and OFF -- neither touches the ensemble object.
  invisible(assemble(ens, mask_final_deaths_step = TRUE,  n_cases_warmup_mask = 2L))
  invisible(assemble(ens, mask_final_deaths_step = FALSE, n_cases_warmup_mask = 0L))

  ens_ref <- make_test_ensemble()
  expect_identical(ens$cases_mean,   ens_ref$cases_mean)
  expect_identical(ens$deaths_mean,  ens_ref$deaths_mean)
  expect_identical(ens$cases_median, ens_ref$cases_median)
  expect_identical(ens$deaths_median, ens_ref$deaths_median)

  m_cases_after  <- raw_metrics(ens, "cases")
  m_deaths_after <- raw_metrics(ens, "deaths")

  expect_identical(m_cases_before$r2,    m_cases_after$r2)
  expect_identical(m_cases_before$bias,  m_cases_after$bias)
  expect_identical(m_deaths_before$r2,   m_deaths_after$r2)
  expect_identical(m_deaths_before$bias, m_deaths_after$bias)
})

# ---------------------------------------------------------------------------
# (i) Boundary cells are NA in the assembled table
# (iii)/(iv) Deaths reporting-lag zeros + interior values, and cases interior,
#            are untouched
# ---------------------------------------------------------------------------

test_that("mask blanks deaths-final + cases-warmup and leaves the rest intact", {
  ens <- make_test_ensemble()
  n_t <- ens$n_time_points
  warmup <- 2L

  tbl <- assemble(ens, mask_final_deaths_step = TRUE, n_cases_warmup_mask = warmup)

  pred_cols <- c("predicted_central", "predicted_mean", "predicted_median",
                 "ci_1_lower", "ci_1_upper")

  for (loc in ens$location_names) {
    df <- tbl[tbl$location == loc, ]
    cases  <- df[df$metric == "Suspected Cases", ]
    deaths <- df[df$metric == "Deaths", ]

    for (cc in pred_cols) {
      expect_true(all(is.na(cases[[cc]][seq_len(warmup)])),
                  info = paste("cases warm-up not NA:", loc, cc))
      expect_true(all(is.finite(cases[[cc]][(warmup + 1L):n_t])),
                  info = paste("cases interior altered:", loc, cc))
    }

    for (cc in pred_cols) {
      expect_true(is.na(deaths[[cc]][n_t]),
                  info = paste("deaths final not NA:", loc, cc))
    }

    expect_equal(deaths$predicted_central[1:5], rep(0, 5),
                 info = paste("deaths leading reporting-lag zeros masked:", loc))
    expect_true(all(is.finite(deaths$predicted_central[6:7])),
                info = paste("deaths interior altered:", loc))
    expect_true(all(deaths$predicted_central[6:7] > 0))
  }
})

test_that("mask is a no-op when both knobs are disabled", {
  ens <- make_test_ensemble()
  n_t <- ens$n_time_points

  tbl <- assemble(ens, mask_final_deaths_step = FALSE, n_cases_warmup_mask = 0L)

  for (loc in ens$location_names) {
    df <- tbl[tbl$location == loc, ]
    cases  <- df[df$metric == "Suspected Cases", ]
    deaths <- df[df$metric == "Deaths", ]
    expect_true(all(is.finite(cases$predicted_central)))
    expect_true(all(is.finite(deaths$predicted_central)))
    expect_equal(deaths$predicted_central[n_t], 0)
  }
})

test_that("n_cases_warmup_mask validates its argument", {
  ens <- make_test_ensemble()
  expect_error(
    assemble(ens, n_cases_warmup_mask = -1L),
    "non-negative integer"
  )
})

test_that("CSV written by .mosaic_write_prediction_csvs matches the assembled table", {
  ens <- make_test_ensemble()
  td  <- withr::local_tempdir()
  tbl <- assemble(ens, mask_final_deaths_step = TRUE, n_cases_warmup_mask = 2L)
  written <- MOSAIC:::.mosaic_write_prediction_csvs(tbl, data_dir = td,
                                                    file_prefix = "ensemble",
                                                    verbose = FALSE)
  expect_length(written, length(ens$location_names))
  for (loc in ens$location_names) {
    f <- file.path(td, paste0("predictions_ensemble_", loc, ".csv"))
    expect_true(file.exists(f))
    df <- utils::read.csv(f, stringsAsFactors = FALSE)
    ref <- tbl[as.character(tbl$location) == loc, ]
    # Schema (column order) is the canonical CSV header.
    expect_identical(names(df), names(ref))
    expect_equal(df$predicted_central, ref$predicted_central)
  }
})

test_that("deprecated save_predictions arg warns and writes no CSV", {
  skip_if_not_installed("ggplot2")
  ens <- make_test_ensemble()
  td  <- withr::local_tempdir()
  expect_warning(
    plot_model_ensemble(ens, output_dir = td, save_predictions = TRUE,
                        verbose = FALSE),
    "deprecated"
  )
  # No CSV written by the plotter anymore.
  expect_length(list.files(td, pattern = "\\.csv$"), 0L)
})
