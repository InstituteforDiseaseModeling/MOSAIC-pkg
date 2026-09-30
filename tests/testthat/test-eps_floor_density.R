# Tests for the channel-relative epsilon floor (arm A1b).
#
# The retired code special-cased a zero prediction against a positive
# observation with `ll <- -observed[i] * log(1e6)` -- a loss LINEAR in the
# observed count, not a log-density. It carried essentially all of the
# log-likelihood's between-draw variance, so the ensemble's delta-AIC described
# that constant rather than model fit. Every cell now goes through the density
# with the mean floored at eps = max(1e-4, eps_rel * mean(observed)).
#
# This file pins the LEAF default, eps_rel = 0.02, which is unchanged. The
# per-channel sizing (cases 0.02, deaths 0.25) lives one level up in
# calc_model_likelihood() and is tested in test-eps_rel_channel.R.

.eps_of <- function(obs) max(1e-4, 0.02 * mean(obs[is.finite(obs)], na.rm = TRUE))

test_that("a zero prediction is scored by the density, not the retired linear penalty", {
     obs <- c(0, 5, 10, 20)
     est <- c(1, 0, 10, 20)            # cell 2 predicts zero against obs = 5
     ll <- MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3, verbose = FALSE)
     expect_true(is.finite(ll))

     # the retired rule would have contributed exactly -5 * log(1e6) for cell 2
     expect_false(isTRUE(all.equal(ll, -5 * log(1e6), tolerance = 1e-6)))

     # it now equals the NB density evaluated at the floored mean
     eps <- .eps_of(obs)
     hand <- sum(stats::dnbinom(obs, mu = pmax(est, eps), size = 3, log = TRUE))
     expect_equal(ll, hand, tolerance = 1e-8)
})

test_that("the floor is channel-relative, so it scales with the series level", {
     # cases-like series: mean ~30 -> eps ~0.6; deaths-like: mean ~0.4 -> eps ~0.008
     cases  <- c(10, 20, 30, 40, 50)
     deaths <- c(0, 0, 1, 0, 1)
     expect_gt(.eps_of(cases), .eps_of(deaths))
     expect_equal(.eps_of(cases), 0.02 * mean(cases), tolerance = 1e-12)

     # a fixed absolute floor of 0.5 would exceed the typical deaths rate; the
     # relative floor must stay well below it
     expect_lt(.eps_of(deaths), mean(deaths))
})

test_that("the floor never goes below the 1e-4 hard minimum", {
     allzero <- rep(0, 10)
     expect_equal(.eps_of(allzero), 1e-4, tolerance = 1e-12)
     ll <- MOSAIC::calc_log_likelihood_negbin(allzero, rep(0, 10), k = 3, verbose = FALSE)
     expect_true(is.finite(ll))
})

test_that("the Poisson path carries the same floor", {
     obs <- c(0, 5, 10, 20)
     est <- c(1, 0, 10, 20)
     ll <- MOSAIC::calc_log_likelihood_poisson(obs, est, verbose = FALSE)
     eps <- .eps_of(obs)
     hand <- sum(stats::dpois(obs, pmax(est, eps), log = TRUE))
     expect_equal(ll, hand, tolerance = 1e-8)
     expect_true(is.finite(ll))
})

test_that("zero-vs-zero cells are scored, not silently given ll = 0", {
     # the retired branch returned exactly 0 for obs == 0 & est == 0, which is
     # not the log-density of that cell
     obs <- c(0, 0, 4, 6)
     est <- c(0, 0, 4, 6)
     eps <- .eps_of(obs)
     ll <- MOSAIC::calc_log_likelihood_negbin(obs, est, k = 3, verbose = FALSE)
     hand <- sum(stats::dnbinom(obs, mu = pmax(est, eps), size = 3, log = TRUE))
     expect_equal(ll, hand, tolerance = 1e-8)
})
