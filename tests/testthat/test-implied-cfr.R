# =============================================================================
# test-implied-cfr.R
#
# Unit tests for the derived implied-CFR helpers added in v0.32.4:
#   .mosaic_add_implied_cfr_columns()  — per-sample algebraic CFR
#   .mosaic_calc_cfr_period_implied()  — period-weighted CFR from ensemble
#
# These guard the v0.13+ schema identity
#   CFR_observed = mu_jt * rho_deaths * chi / rho
# and its per-episode counterpart
#   CFR_clinical = 1 - exp(-mu_jt / gamma_1)
# against silent regressions in numerical guards, domain clamping, and
# array-orientation contracts.
# =============================================================================

library(testthat)

test_that("implied CFR matches steady-state identity at known values", {
  # 5 samples, single location ETH
  results <- data.frame(
    rho                       = c(0.276, 0.276, 0.276, 0.276, 0.276),
    rho_deaths                = c(0.420, 0.420, 0.420, 0.420, 0.420),
    chi_endemic               = c(0.520, 0.520, 0.520, 0.520, 0.520),
    chi_epidemic              = c(0.758, 0.758, 0.758, 0.758, 0.758),
    gamma_1                   = c(0.200, 0.200, 0.200, 0.200, 0.200),
    mu_j_baseline_ETH         = c(0.010, 0.012, 0.014, 0.016, 0.020),
    mu_j_epidemic_factor_ETH  = c(0.000, 0.500, 1.000, 1.500, 2.000)
  )
  out <- MOSAIC:::.mosaic_add_implied_cfr_columns(results, iso_codes = "ETH", verbose = FALSE)

  expect_true(all(c("cfr_baseline_ETH", "cfr_epidemic_ETH",
                    "cfr_clinical_baseline_ETH", "cfr_clinical_epidemic_ETH") %in% names(out)))

  # Hand-verify first sample: 0.010 * 0.420 * 0.520 / 0.276 = 0.00791...
  expect_equal(out$cfr_baseline_ETH[1], 0.010 * 0.420 * 0.520 / 0.276, tolerance = 1e-10)

  # cfr_epidemic with eps=0 should differ from baseline only by chi_epi/chi_end
  ratio <- out$cfr_epidemic_ETH[1] / out$cfr_baseline_ETH[1]
  expect_equal(ratio, 0.758 / 0.520, tolerance = 1e-10)

  # Clinical CFR: 1 - exp(-mu/gamma_1) = 1 - exp(-0.010/0.2) = 1 - exp(-0.05)
  expect_equal(out$cfr_clinical_baseline_ETH[1], 1 - exp(-0.010 / 0.200), tolerance = 1e-10)
})

test_that("implied CFR is clamped to [0, 1] and non-finite becomes NA", {
  # Row 1: rho exactly zero -> division gives Inf -> NA-masked
  # Row 2: rho near zero -> finite but huge -> clamped to 1.0
  # Row 3: huge epidemic_factor -> > 1 mathematically -> clamped to 1.0
  results <- data.frame(
    rho                       = c(0.0,     1e-50,  0.276),
    rho_deaths                = c(0.42,    0.42,   0.42),
    chi_endemic               = c(0.52,    0.52,   0.52),
    chi_epidemic              = c(0.76,    0.76,   0.76),
    mu_j_baseline_ETH         = c(0.02,    0.02,   0.05),
    mu_j_epidemic_factor_ETH  = c(0.5,     0.5,    50.0)
  )
  out <- MOSAIC:::.mosaic_add_implied_cfr_columns(results, iso_codes = "ETH", verbose = FALSE)

  # Row 1: rho=0 -> Inf -> NA
  expect_true(is.na(out$cfr_baseline_ETH[1]))

  # Row 2: rho near 0 -> finite-but-huge -> clamped to 1.0
  expect_equal(out$cfr_baseline_ETH[2], 1.0)

  # Row 3: epidemic_factor=50 pushes mathematical value > 1 -> clamped
  expect_lte(out$cfr_epidemic_ETH[3], 1.0)
  expect_gte(out$cfr_epidemic_ETH[3], 0.0)
})

test_that("missing global columns trigger graceful skip", {
  # No rho_deaths column -> should return input unchanged
  results <- data.frame(
    rho                       = 0.276,
    chi_endemic               = 0.52,
    chi_epidemic              = 0.76,
    mu_j_baseline_ETH         = 0.012,
    mu_j_epidemic_factor_ETH  = 0.5
  )
  out <- MOSAIC:::.mosaic_add_implied_cfr_columns(results, iso_codes = "ETH", verbose = FALSE)
  expect_false("cfr_baseline_ETH" %in% names(out))
})

test_that("empty iso_codes returns input unchanged", {
  results <- data.frame(
    rho = 0.276, rho_deaths = 0.42, chi_endemic = 0.52, chi_epidemic = 0.76,
    mu_j_baseline_ETH = 0.012, mu_j_epidemic_factor_ETH = 0.5
  )
  out <- MOSAIC:::.mosaic_add_implied_cfr_columns(results, iso_codes = character(0), verbose = FALSE)
  expect_identical(names(out), names(results))
})

test_that("missing gamma_1 omits clinical CFR but keeps surveillance CFRs", {
  results <- data.frame(
    rho = 0.276, rho_deaths = 0.42, chi_endemic = 0.52, chi_epidemic = 0.76,
    mu_j_baseline_ETH = 0.012, mu_j_epidemic_factor_ETH = 0.5
  )
  out <- MOSAIC:::.mosaic_add_implied_cfr_columns(results, iso_codes = "ETH", verbose = FALSE)
  expect_true("cfr_baseline_ETH" %in% names(out))
  expect_true("cfr_epidemic_ETH" %in% names(out))
  expect_false("cfr_clinical_baseline_ETH" %in% names(out))
})

test_that("period CFR identity holds on synthetic ensemble", {
  # 1 location, 10 time steps, 4 param sets, 5 stoch reps
  # Construct so that every (p, s) member has constant per-tick rates:
  #   cases_per_tick = p * 100, deaths_per_tick = p * 100 * 0.02
  # so period CFR per member = 0.02 (independent of p, s).
  n_loc <- 1L; n_t <- 10L; n_p <- 4L; n_s <- 5L
  cases  <- array(0, dim = c(n_loc, n_t, n_p, n_s))
  deaths <- array(0, dim = c(n_loc, n_t, n_p, n_s))
  for (p in seq_len(n_p)) {
       cases[1L, , p, ]  <- p * 100
       deaths[1L, , p, ] <- p * 100 * 0.02
  }
  obs_cases  <- matrix(1000, nrow = 1, ncol = n_t)
  obs_deaths <- matrix(  20, nrow = 1, ncol = n_t)   # observed CFR = 2%

  res <- MOSAIC:::.mosaic_calc_cfr_period_implied(
       cases_array = cases, deaths_array = deaths,
       obs_cases = obs_cases, obs_deaths = obs_deaths,
       location_names = "ETH",
       envelope_quantiles = c(0.025, 0.5, 0.975)
  )

  expect_equal(res$ETH$predicted_median, 0.02, tolerance = 1e-10)
  expect_equal(res$ETH$observed, 0.02, tolerance = 1e-10)
  expect_equal(res$ETH$n_members, n_p * n_s)
  expect_equal(res$ETH$n_param_sets, n_p)
  expect_equal(res$ETH$n_stoch_per, n_s)
})

test_that("period CFR errors on obs_cases nrow mismatch (transposed orientation)", {
  cases  <- array(100, dim = c(2, 10, 4, 5))
  deaths <- array(  2, dim = c(2, 10, 4, 5))
  # WRONG: passing time x loc instead of loc x time
  obs_cases_wrong  <- matrix(1000, nrow = 10, ncol = 2)
  obs_deaths_wrong <- matrix(  20, nrow = 10, ncol = 2)

  expect_error(
       MOSAIC:::.mosaic_calc_cfr_period_implied(
            cases_array = cases, deaths_array = deaths,
            obs_cases = obs_cases_wrong, obs_deaths = obs_deaths_wrong,
            location_names = c("ETH", "MOZ")
       ),
       regexp = "obs_cases rows must match location_names"
  )
})

test_that("period CFR handles 3D array via auto-expand to 4D", {
  # 1 location, 5 time steps, 3 members
  cases  <- array(c(rep(50, 5), rep(100, 5), rep(150, 5)), dim = c(1, 5, 3))
  deaths <- array(c(rep( 1, 5), rep(  2, 5), rep(  3, 5)), dim = c(1, 5, 3))
  obs_cases  <- matrix(500, nrow = 1, ncol = 5)
  obs_deaths <- matrix( 10, nrow = 1, ncol = 5)
  res <- MOSAIC:::.mosaic_calc_cfr_period_implied(
       cases_array = cases, deaths_array = deaths,
       obs_cases = obs_cases, obs_deaths = obs_deaths,
       location_names = "ETH"
  )
  # Each member has CFR = 1/50, 2/100, 3/150 = 0.02 uniformly
  expect_equal(res$ETH$predicted_median, 0.02, tolerance = 1e-10)
})
