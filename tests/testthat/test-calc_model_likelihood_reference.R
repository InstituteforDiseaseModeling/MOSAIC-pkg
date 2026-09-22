# Reference numerical tests for calc_model_likelihood
# These pin exact output values for known inputs to verify cross-implementation equivalence.
# If these tests break, the likelihood function's numerical behavior has changed.

# Shared test data: 2 locations x 10 timesteps
#
# RE-BASELINED at v0.92.0. These pin the likelihood ASSEMBLY, not the dispersion
# rule. Dispersion used to be estimated inside the likelihood by a marginal
# method-of-moments form and floored at 3; it is now estimated per location by
# est_nb_dispersion() and passed in. The expected values below are therefore
# pinned at an EXPLICIT k and were verified against an independent hand
# computation, sum(dnbinom(obs, mu = est, size = k, log = TRUE)) over both
# channels -- they are derived, not merely recorded.
.REF_K <- 3

ref_obs_c <- matrix(c(10,20,30,40,50,60,70,80,90,100,
                        5,10,15,20,25,30,35,40,45,50), nrow = 2, byrow = TRUE)
ref_est_c <- matrix(c(12,18,35,38,55,58,72,78,88,105,
                        6, 9,14,22,23,32,33,42,43, 52), nrow = 2, byrow = TRUE)
ref_obs_d <- round(ref_obs_c * 0.05)
ref_est_d <- round(ref_est_c * 0.05)

test_that("reference: core NB only produces known value", {
  ll <- MOSAIC::calc_model_likelihood(ref_obs_c, ref_est_c, ref_obs_d, ref_est_d,
                                      nb_k_cases = .REF_K, nb_k_deaths = .REF_K)
  expect_equal(ll, -107.24009351, tolerance = 1e-4)
  # independent hand computation of the same quantity
  hand <- sum(stats::dnbinom(ref_obs_c, mu = pmax(ref_est_c, 1e-10), size = .REF_K, log = TRUE)) +
          sum(stats::dnbinom(ref_obs_d, mu = pmax(ref_est_d, 1e-10), size = .REF_K, log = TRUE))
  expect_equal(ll, hand, tolerance = 1e-6)
})

test_that("reference: core NB + cumulative produces known value", {
  ll <- MOSAIC::calc_model_likelihood(ref_obs_c, ref_est_c, ref_obs_d, ref_est_d,
                                      nb_k_cases = .REF_K, nb_k_deaths = .REF_K,
                                      weight_cumulative_total = 0.25)
  expect_equal(ll, -108.77099366, tolerance = 1e-4)
})

test_that("reference: core NB + WIS produces known value", {
  ll <- MOSAIC::calc_model_likelihood(ref_obs_c, ref_est_c, ref_obs_d, ref_est_d,
                                      nb_k_cases = .REF_K, nb_k_deaths = .REF_K,
                                      weight_wis = 0.10)
  expect_equal(ll, -109.38549351, tolerance = 1e-4)
})

test_that("reference: perfect match (obs == est) produces known value", {
  ll <- MOSAIC::calc_model_likelihood(ref_obs_c, ref_obs_c, ref_obs_d, ref_obs_d,
                                      nb_k_cases = .REF_K, nb_k_deaths = .REF_K)
  expect_equal(ll, -105.97890716, tolerance = 1e-4)
  hand <- sum(stats::dnbinom(ref_obs_c, mu = pmax(ref_obs_c, 1e-10), size = .REF_K, log = TRUE)) +
          sum(stats::dnbinom(ref_obs_d, mu = pmax(ref_obs_d, 1e-10), size = .REF_K, log = TRUE))
  expect_equal(ll, hand, tolerance = 1e-6)
})

test_that("reference: extreme 1000x over-prediction produces known value", {
  ll <- MOSAIC::calc_model_likelihood(
    matrix(10, 1, 10), matrix(10000, 1, 10),
    matrix(1, 1, 10), matrix(1000, 1, 10))
  expect_equal(ll, -109160.93253574, tolerance = 1)
})

test_that("reference: NB element-level LL for known inputs", {
  ll <- MOSAIC::calc_log_likelihood_negbin(
    observed  = c(10, 20, 30, 40, 50),
    estimated = c(12, 18, 35, 38, 55),
    k = 3, weights = NULL, verbose = FALSE)
  expect_equal(ll, -18.70319184, tolerance = 1e-4)
})

test_that("reference: Poisson LL for perfect match", {
  ll <- MOSAIC::calc_log_likelihood_poisson(
    observed  = c(10, 20, 30),
    estimated = c(10, 20, 30),
    weights = NULL, verbose = FALSE)
  expect_equal(ll, -7.12184753, tolerance = 1e-4)
})

test_that("reference: 1x3 matrix (minimum viable input) produces known value", {
  # k = 90 / Inf is exactly what the retired marginal estimator produced for
  # this fixture (its floor of 3 never bound here), so the historical frozen
  # value is reproduced bit-for-bit once the dispersion is pinned.
  ll <- MOSAIC::calc_model_likelihood(
    matrix(c(50, 60, 70), 1, 3), matrix(c(55, 58, 72), 1, 3),
    matrix(c(5, 6, 7), 1, 3),   matrix(c(4, 6, 8), 1, 3),
    nb_k_cases = 90, nb_k_deaths = Inf)
  expect_equal(ll, -15.49095, tolerance = 1e-3)
})

test_that("reference: 1x1 matrix returns 0 (below min obs threshold)", {
  ll <- MOSAIC::calc_model_likelihood(
    matrix(50, 1, 1), matrix(55, 1, 1),
    matrix(5, 1, 1),  matrix(4, 1, 1))
  expect_equal(ll, 0, tolerance = 1e-8)
})

test_that("reference: perfect match always better than imperfect", {
  ll_perfect <- MOSAIC::calc_model_likelihood(ref_obs_c, ref_obs_c, ref_obs_d, ref_obs_d,
                                      nb_k_cases = .REF_K, nb_k_deaths = .REF_K)
  ll_close   <- MOSAIC::calc_model_likelihood(ref_obs_c, ref_est_c, ref_obs_d, ref_est_d,
                                      nb_k_cases = .REF_K, nb_k_deaths = .REF_K)
  expect_true(ll_perfect > ll_close)
})
