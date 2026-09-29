# =============================================================================
# test-cfr-period-implied.R
#
# .mosaic_calc_cfr_period_implied(): the period-weighted reported CFR each
# ensemble member actually produced (sum deaths / sum cases), and its
# distribution over members. Moved from test-implied-cfr.R when the algebraic
# implied-CFR columns (.mosaic_add_implied_cfr_columns) were retired in v0.96.0:
# the reported CFR is now the model input mu_jt, and its calibrated value is
# calc_model_ensemble()$cfr_posterior.
# =============================================================================

library(testthat)

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

test_that("period CFR uses only the scored observed window and the member weights", {
  # 1 location, 12 steps: steps 1-2 are burn-in, steps 11-12 are the unobserved
  # forecast tail. Member 1 has CFR 0.01 in the window, member 2 0.05; both put
  # a huge death count in the tail, which must not count.
  cases  <- array(100, dim = c(1, 12, 2, 1))
  deaths <- array(0,   dim = c(1, 12, 2, 1))
  deaths[1, 3:10, 1, 1] <- 1; deaths[1, 3:10, 2, 1] <- 5
  deaths[1, 11:12, , 1] <- 1000; deaths[1, 1:2, , 1] <- 1000
  obs_c <- matrix(c(rep(100, 10), NA, NA), 1)
  obs_d <- matrix(c(rep(2, 10), NA, NA), 1)
  res <- MOSAIC:::.mosaic_calc_cfr_period_implied(cases, deaths, obs_c, obs_d, "AAA",
                                                  member_weights = c(0.9, 0.1), score_idx = 3L)
  expect_equal(res$AAA$observed, 0.02)
  expect_equal(res$AAA$observed_total_cases, 800)
  expect_equal(res$AAA$predicted_mean, 0.9 * 0.01 + 0.1 * 0.05)
  # Same weighted-quantile rule as the ensemble predictions.
  expect_equal(res$AAA$predicted_median, MOSAIC::weighted_quantiles(c(0.01, 0.05), c(0.9, 0.1), 0.5))
  expect_equal(res$AAA$predicted_total_deaths, MOSAIC::weighted_quantiles(c(8, 40), c(0.9, 0.1), 0.5))
  expect_lt(res$AAA$predicted_total_deaths, 40)   # the burn-in and tail deaths are excluded
  expect_error(MOSAIC:::.mosaic_calc_cfr_period_implied(cases, deaths, obs_c, obs_d, "AAA",
                                                        member_weights = 1), "one value per parameter set")
})
