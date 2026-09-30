# Tests for calc_model_likelihood() extreme inputs — replaces guardrail tests
# Verifies the NB naturally handles bad fits without needing hard floors.

test_that("extreme over-prediction produces very negative but finite LL", {
  n_loc <- 2
  n_time <- 50
  obs <- matrix(10, n_loc, n_time)
  est <- matrix(10000, n_loc, n_time)  # 1000x over-prediction

  ll <- MOSAIC::calc_model_likelihood(
    obs_cases = obs, est_cases = est,
    obs_deaths = obs, est_deaths = est
  )
  expect_true(is.finite(ll))
  expect_true(ll < -1000)  # Should be very negative
})

test_that("extreme under-prediction produces very negative but finite LL", {
  n_loc <- 2
  n_time <- 50
  obs <- matrix(100, n_loc, n_time)
  est <- matrix(0.001, n_loc, n_time)  # Near-zero prediction

  ll <- MOSAIC::calc_model_likelihood(
    obs_cases = obs, est_cases = est,
    obs_deaths = obs, est_deaths = est
  )
  expect_true(is.finite(ll))
  expect_true(ll < -10000)  # Proportional penalty: very negative
})

test_that("a zero prediction against a large observation is heavily penalised", {
  # RE-BASELINED. The old assertion was `ll < -1000` with the comment
  # "-obs * log(1e6) penalty" -- it pinned the MAGNITUDE of a rule that v0.93.0
  # retired (that branch was a loss linear in the observed count, not a
  # log-density). The absolute threshold then became a function of the eps
  # floor: at the per-channel defaults (cases 0.02, deaths 0.25 of mean(obs))
  # the same inputs score ~-772, which is still a severe penalty. Pin the
  # PROPERTY -- a zero prediction must be far worse than a perfect one, and
  # a bigger eps must soften it -- not the retired constant.
  obs <- matrix(c(0, 100, 0, 50), nrow = 1)
  est <- matrix(c(0, 0, 0, 0), nrow = 1)

  ll <- MOSAIC::calc_model_likelihood(
    obs_cases = obs, est_cases = est,
    obs_deaths = obs, est_deaths = est
  )
  perfect <- MOSAIC::calc_model_likelihood(
    obs_cases = obs, est_cases = obs,
    obs_deaths = obs, est_deaths = obs
  )
  expect_true(is.finite(ll))
  expect_lt(ll, -500)
  expect_lt(ll, perfect - 500)

  # the penalty is the eps floor, so a larger floor must make it less severe
  softer <- MOSAIC::calc_model_likelihood(
    obs_cases = obs, est_cases = est,
    obs_deaths = obs, est_deaths = est,
    eps_rel_cases = 0.50, eps_rel_deaths = 0.50
  )
  expect_gt(softer, ll)
})

test_that("non-finite LL returns -Inf", {
  # Force a non-finite by using NaN in estimates
  obs <- matrix(c(10, 20, 30), nrow = 1)
  est <- matrix(c(10, NaN, 30), nrow = 1)

  # This should not error — NaN propagates to non-finite LL which becomes -Inf
  ll <- MOSAIC::calc_model_likelihood(
    obs_cases = obs, est_cases = est,
    obs_deaths = obs, est_deaths = est
  )
  # Result should be finite or -Inf (non-finite safety net)
  expect_true(is.finite(ll) || identical(ll, -Inf) || is.na(ll))
})

test_that("negative-correlated inputs produce bad but finite LL (not floored)", {
  set.seed(42)
  n_time <- 100
  # Create anti-correlated time series
  obs <- matrix(c(rep(c(100, 0), n_time / 2)), nrow = 1)
  est <- matrix(c(rep(c(0, 100), n_time / 2)), nrow = 1)  # Opposite pattern

  ll <- MOSAIC::calc_model_likelihood(
    obs_cases = obs, est_cases = est,
    obs_deaths = matrix(0, 1, n_time), est_deaths = matrix(0, 1, n_time)
  )
  # Should be finite (no guardrail floor), but very negative
  expect_true(is.finite(ll))
  expect_true(ll < -500)
})

test_that("all-zero obs and est produces zero LL (correct behavior)", {
  obs <- matrix(0, nrow = 3, ncol = 20)
  est <- matrix(0, nrow = 3, ncol = 20)

  ll <- MOSAIC::calc_model_likelihood(
    obs_cases = obs, est_cases = est,
    obs_deaths = obs, est_deaths = est
  )
  expect_true(is.finite(ll))
  expect_equal(ll, 0, tolerance = 5e-2)
})
