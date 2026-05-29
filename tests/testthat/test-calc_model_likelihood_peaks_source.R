# Tests that calc_model_likelihood() prefers config$epidemic_peaks when
# present and falls back to MOSAIC::epidemic_peaks when absent. Closes the
# R/Python sourcing asymmetry called out in the v0.30.27 review: the Python
# port reads peaks from config["epidemic_peaks"]; R should do the same.

# Build a minimal-but-valid set of inputs for calc_model_likelihood() with
# weight_peak_timing > 0 so the peak path is exercised.
make_inputs <- function() {
  n_loc <- 1L
  n_t   <- 60L
  obs_cases  <- matrix(rpois(n_loc * n_t, lambda = 5), n_loc, n_t)
  obs_deaths <- matrix(rpois(n_loc * n_t, lambda = 1), n_loc, n_t)
  # Estimated series has a clear synthetic peak at day 30.
  est_cases  <- matrix(0.5, n_loc, n_t)
  est_cases[1, 25:35] <- c(3, 5, 8, 12, 18, 25, 18, 12, 8, 5, 3)
  est_deaths <- matrix(0.1, n_loc, n_t)
  list(
    obs_cases  = obs_cases,
    est_cases  = est_cases,
    obs_deaths = obs_deaths,
    est_deaths = est_deaths,
    n_t        = n_t
  )
}

test_that("calc_model_likelihood prefers config$epidemic_peaks when supplied", {
  set.seed(1)
  d <- make_inputs()

  # Two configs, identical except for epidemic_peaks:
  # cfg_match : peak exactly at day 30 (matches the synthetic estimated peak)
  # cfg_off   : peak 25 days away (mismatch → much lower peak-timing LL)
  base <- list(
    location_name = "MOZ",
    date_start    = "2024-01-01",
    date_stop     = "2024-01-01" |> as.Date() |> {\(d0) d0 + (d$n_t - 1L)}() |> as.character()
  )

  cfg_match <- base
  cfg_match$epidemic_peaks <- data.frame(
    iso_code  = "MOZ",
    peak_date = as.character(as.Date("2024-01-01") + 29L),  # day 30
    stringsAsFactors = FALSE
  )

  cfg_off <- base
  cfg_off$epidemic_peaks <- data.frame(
    iso_code  = "MOZ",
    peak_date = as.character(as.Date("2024-01-01") + 4L),   # day 5 — far from sim peak
    stringsAsFactors = FALSE
  )

  # weight_deaths = 0 so only the cases peak-timing term differs between
  # cfg_match and cfg_off (est_deaths is flat → boundary argmax artefacts
  # would otherwise dominate).
  ll_match <- MOSAIC::calc_model_likelihood(
    obs_cases = d$obs_cases, est_cases = d$est_cases,
    obs_deaths = d$obs_deaths, est_deaths = d$est_deaths,
    config = cfg_match,
    weight_deaths = 0,
    weight_peak_timing = 1.0
  )
  ll_off <- MOSAIC::calc_model_likelihood(
    obs_cases = d$obs_cases, est_cases = d$est_cases,
    obs_deaths = d$obs_deaths, est_deaths = d$est_deaths,
    config = cfg_off,
    weight_deaths = 0,
    weight_peak_timing = 1.0
  )

  # If config$epidemic_peaks were ignored (still using MOSAIC::epidemic_peaks
  # under the hood), ll_match and ll_off would be identical — both runs would
  # source the same peaks regardless of cfg input.
  expect_true(is.finite(ll_match))
  expect_true(is.finite(ll_off))
  expect_gt(ll_match, ll_off)
})

test_that("calc_model_likelihood falls back to MOSAIC::epidemic_peaks when config has none", {
  set.seed(2)
  d <- make_inputs()

  # No epidemic_peaks key in config; function should still run via the
  # MOSAIC::epidemic_peaks fallback path. MOZ is in the package data.
  cfg <- list(
    location_name = "MOZ",
    date_start    = "2024-01-01",
    date_stop     = as.character(as.Date("2024-01-01") + (d$n_t - 1L))
  )
  expect_true(is.null(cfg$epidemic_peaks))

  ll <- MOSAIC::calc_model_likelihood(
    obs_cases = d$obs_cases, est_cases = d$est_cases,
    obs_deaths = d$obs_deaths, est_deaths = d$est_deaths,
    config = cfg,
    weight_peak_timing = 1.0
  )
  expect_true(is.finite(ll))
})

test_that("legacy peak helpers accept an explicit epidemic_peaks argument", {
  # Construct a synthetic estimated series with a known peak at index 15.
  est_vec <- c(rep(0.5, 14), 10, rep(0.5, 14))
  obs_vec <- c(rep(1, 14), 12, rep(1, 14))

  custom_peaks <- data.frame(
    iso_code  = "XYZ",
    peak_date = "2024-01-15",
    stringsAsFactors = FALSE
  )

  ll_t <- MOSAIC::calc_multi_peak_timing_ll(
    obs_vec = obs_vec, est_vec = est_vec, iso_code = "XYZ",
    date_start = "2024-01-01", date_stop = "2024-01-29",
    epidemic_peaks = custom_peaks
  )
  ll_m <- MOSAIC::calc_multi_peak_magnitude_ll(
    obs_vec = obs_vec, est_vec = est_vec, iso_code = "XYZ",
    date_start = "2024-01-01", date_stop = "2024-01-29",
    epidemic_peaks = custom_peaks
  )

  # XYZ is not in MOSAIC::epidemic_peaks — non-zero results prove the helper
  # consulted the supplied data.frame, not the package fallback.
  expect_true(is.finite(ll_t))
  expect_true(is.finite(ll_m))
  expect_true(ll_t != 0 || ll_m != 0)
})
