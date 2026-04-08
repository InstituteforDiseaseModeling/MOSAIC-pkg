
# 1. Length mismatch error
testthat::test_that("errors when observed and estimated lengths differ", {
     expect_error(
          MOSAIC::calc_log_likelihood_negbin(
               observed = c(1, 2),
               estimated = c(1),
               k = 1,
               weights = NULL,
               verbose = FALSE
          ), "Lengths of observed and estimated must match"
     )
})

# 2. NA removal: all NA -> returns NA with message
testthat::test_that("returns NA for all NA input after NA removal", {
     expect_message(
          ll <- MOSAIC::calc_log_likelihood_negbin(
               observed = c(NA, NA),
               estimated = c(NA, NA),
               k = 1,
               weights = NULL,
               verbose = TRUE
          ), "No usable data"
     )
     expect_true(is.na(ll))
})

# 3. Negative weights error
testthat::test_that("errors on negative weights", {
     expect_error(
          MOSAIC::calc_log_likelihood_negbin(
               observed = c(1, 2),
               estimated = c(1, 2),
               k = 1,
               weights = c(1, -1),
               verbose = FALSE
          ), "All weights must be >= 0"
     )
})

# 4. Zero-sum weights error
testthat::test_that("errors on zero-sum weights", {
     expect_error(
          MOSAIC::calc_log_likelihood_negbin(
               observed = c(1, 2),
               estimated = c(1, 2),
               k = 1,
               weights = c(0, 0),
               verbose = FALSE
          ), "All weights are zero"
     )
})

# 5. Non-integer observed values are rounded (cross-language float safety)
testthat::test_that("non-integer observed values are rounded silently", {
     # c(1.5, 2) rounds to c(2, 2) — no error
     ll <- MOSAIC::calc_log_likelihood_negbin(
          observed = c(1.5, 2),
          estimated = c(1, 2),
          k = 1,
          weights = NULL,
          verbose = FALSE
     )
     expect_true(is.finite(ll))

     # Float near-integers from parquet transport should not error
     ll2 <- MOSAIC::calc_log_likelihood_negbin(
          observed = c(2.0000000000000004, 3.0),
          estimated = c(2, 3),
          k = 3,
          weights = NULL,
          verbose = FALSE
     )
     expect_true(is.finite(ll2))

     # Negative values still error (round doesn't help)
     expect_error(
          MOSAIC::calc_log_likelihood_negbin(
               observed = c(-1, 2),
               estimated = c(1, 2),
               k = 1,
               weights = NULL,
               verbose = FALSE
          ), "observed must contain non-negative integer counts"
     )
})

# 6. Cushion for estimated <= 0 ensures finite result
testthat::test_that("cushions zero or negative estimates", {
     obs <- c(1, 2)
     est <- c(0, -5)
     ll <- MOSAIC::calc_log_likelihood_negbin(obs, est, k = 1, verbose = FALSE)
     expect_true(is.finite(ll))
})

# 7. Automatic k estimation: var <= mean -> Poisson fallback
testthat::test_that("defaults to Poisson (k=Inf) when variance <= mean", {
     obs <- rep(1, 5)  # var = 0, mean = 1
     est <- obs
     expect_message(
          ll_nb <- MOSAIC::calc_log_likelihood_negbin(obs, est, k = NULL, verbose = TRUE),
          "using Poisson"
     )
     ll_pois <- MOSAIC::calc_log_likelihood_poisson(obs, est, verbose = FALSE)
     expect_equal(ll_nb, ll_pois, tolerance = 1e-8)
})

# 8. Provided k is floored to k_min when too small
testthat::test_that("uses k_min floor when provided k is too small", {
     obs <- c(0, 1, 2)
     est <- c(1, 1, 1)
     # k=1 < k_min=3 (default), so function floors to k_min and messages about it
     expect_message(
          ll <- MOSAIC::calc_log_likelihood_negbin(obs, est, k = 1, verbose = TRUE),
          "k_min"
     )
     expect_true(is.finite(ll))
})

# 9. Correct NB log-likelihood calculation
testthat::test_that("matches manual Negative Binomial log-likelihood formula", {
     observed  <- c(0, 1, 2)
     estimated <- c(1, 2, 3)
     k_param   <- 2
     # manual NB ll_vec
     ll_vec <- lgamma(observed + k_param) - lgamma(k_param) - lgamma(observed + 1) +
          k_param * log(k_param / (k_param + estimated)) +
          observed * log(estimated / (k_param + estimated))
     ll_manual <- sum(ll_vec)

     ll_func <- MOSAIC::calc_log_likelihood_negbin(
          observed = observed,
          estimated = estimated,
          k = k_param,
          k_min = 0,  # Disable flooring so k=2 is used as-is
          weights = NULL,
          verbose = FALSE
     )
     expect_equal(ll_func, ll_manual, tolerance = 1e-8)
})

# 10. Weighted NB log-likelihood matches manual weights
testthat::test_that("matches manual calculation with weights", {
     observed  <- c(0, 1, 2)
     estimated <- c(1, 2, 3)
     weights   <- c(1, 2, 0.5)
     k_param   <- 2
     ll_vec    <- lgamma(observed + k_param) - lgamma(k_param) - lgamma(observed + 1) +
          k_param * log(k_param / (k_param + estimated)) +
          observed * log(estimated / (k_param + estimated))
     ll_manual <- sum(weights * ll_vec)

     ll_func <- MOSAIC::calc_log_likelihood_negbin(
          observed = observed,
          estimated = estimated,
          k = k_param,
          k_min = 0,  # Disable flooring so k=2 is used as-is
          weights = weights,
          verbose = FALSE
     )
     expect_equal(ll_func, ll_manual, tolerance = 1e-8)
})
