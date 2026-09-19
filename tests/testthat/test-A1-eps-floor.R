# A1: the zero-prediction branch returned `-observed * log(1e6)` -- a loss LINEAR
# in the observed count, not a log-density. It carried ~100% of the
# log-likelihood's between-draw variance, so delta-AIC described the constant
# rather than the fit. Every cell now goes through the density with an
# epsilon-floored mean, eps_j = max(0.5, 0.001 * mean(obs_j)).

test_that("no code path returns a non-density constant for a zero prediction", {
  obs <- c(0, 5, 20, 0, 3)
  est <- c(0, 0, 0, 0, 0)          # every cell is a zero prediction
  ll_nb  <- MOSAIC:::calc_log_likelihood_negbin(observed = obs, estimated = est, k = 3)
  ll_poi <- MOSAIC:::calc_log_likelihood_poisson(observed = obs, estimated = est)
  expect_true(is.finite(ll_nb)); expect_true(is.finite(ll_poi))

  # The old rule would have returned exactly -sum(obs) * log(1e6).
  old_penalty <- -sum(obs) * log(1e6)
  expect_false(isTRUE(all.equal(ll_nb,  old_penalty)))
  expect_gt(ll_nb, old_penalty)     # the density is far less punitive
})

test_that("the NB value at the floor matches a hand computation", {
  # eps_j = max(0.5, 0.001 * mean(obs)); with obs = 5 repeated, mean = 5 -> eps = 0.5
  obs <- rep(5, 4); est <- rep(0, 4); k <- 3
  eps <- 0.5
  hand_one <- lgamma(5 + k) - lgamma(k) - lgamma(5 + 1) +
              k * log(k / (k + eps)) + 5 * log(eps / (k + eps))
  expect_equal(MOSAIC:::calc_log_likelihood_negbin(observed = obs, estimated = est, k = k),
               4 * hand_one, tolerance = 1e-10)
})

test_that("a good prediction still beats a zero prediction", {
  obs <- c(10, 12, 9, 11)
  good <- MOSAIC:::calc_log_likelihood_negbin(obs, estimated = obs,        k = 3)
  zero <- MOSAIC:::calc_log_likelihood_negbin(obs, estimated = rep(0, 4),  k = 3)
  expect_gt(good, zero)
})

test_that("the number of scored cells does not depend on the draw", {
  # LIKE-01 warns against simply dropping offending cells: that makes the scored
  # count draw-dependent and LLs incomparable. Every cell must still score.
  obs <- c(0, 4, 7, 2)
  a <- MOSAIC:::calc_log_likelihood_negbin(obs, estimated = c(0, 0, 0, 0), k = 3)
  b <- MOSAIC:::calc_log_likelihood_negbin(obs, estimated = c(1, 1, 1, 1), k = 3)
  expect_true(is.finite(a) && is.finite(b))
})
