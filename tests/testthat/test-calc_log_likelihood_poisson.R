
# 1. NA handling: all observed NA -> returns NA with message
testthat::test_that("returns NA for all NA input", {
     expect_message(
          ll <- MOSAIC::calc_log_likelihood_poisson(
               observed = c(NA, NA),
               estimated = c(1, 2),
               weights = NULL,
               verbose = TRUE
          ))
     expect_true(is.na(ll))
})

# 2. Cushion for estimated <= 0 does not produce -Inf
testthat::test_that("cushions zero or negative estimates", {
     obs <- c(1, 2)
     est <- c(0, -5)
     ll <- MOSAIC::calc_log_likelihood_poisson(obs, est, verbose = FALSE)
     expect_true(is.finite(ll))
})

# 3. Length mismatch errors
testthat::test_that("errors on length mismatch", {
     expect_error(
          MOSAIC::calc_log_likelihood_poisson(
               observed = c(1, 2, 3, 4),
               estimated = c(1, 2),
               weights = NULL,
               verbose = FALSE
          ))
})

# 4. Negative weights error
testthat::test_that("errors on negative weights", {
     expect_error(
          MOSAIC::calc_log_likelihood_poisson(
               observed = c(1, 2),
               estimated = c(1, 2),
               weights = c(1, -1),
               verbose = FALSE
          ), "All weights must be >= 0"
     )
})

# 5. Zero-sum weights error
testthat::test_that("errors on zero-sum weights", {
     expect_error(
          MOSAIC::calc_log_likelihood_poisson(
               observed = c(1, 2),
               estimated = c(1, 2),
               weights = c(0, 0),
               verbose = FALSE
          ), "All weights are zero"
     )
})

# 6. Non-integer observed error
testthat::test_that("errors on non-integer observed values", {
     expect_error(
          MOSAIC::calc_log_likelihood_poisson(
               observed = c(1.5, 2),
               estimated = c(1, 2),
               weights = NULL,
               verbose = FALSE
          ), "observed must contain non-negative integer counts for Poisson"
     )
})

# 7. Overdispersion warning when var/mean > 1.5
testthat::test_that("warns on overdispersion", {
     observed <- c(0, 0, 5)
     estimated <- c(1, 1, 5)
     expect_warning(
          MOSAIC::calc_log_likelihood_poisson(observed, estimated, verbose = TRUE),
          "suggests overdispersion"
     )
})

# 8. Correct log-likelihood without weights
testthat::test_that("matches manual calculation without weights", {
     observed  <- c(0, 1, 2)
     estimated <- c(1, 2, 3)
     ll_manual <- sum(observed * log(estimated) - estimated - lgamma(observed + 1))
     ll_func   <- MOSAIC::calc_log_likelihood_poisson(
          observed = observed,
          estimated = estimated,
          weights = NULL,
          verbose = FALSE
     )
     expect_equal(ll_func, ll_manual, tolerance = 1e-8)
})

# 9. Correct log-likelihood with weights
testthat::test_that("matches manual calculation with weights", {
     observed  <- c(0, 1, 2)
     estimated <- c(1, 2, 3)
     weights   <- c(1, 2, 0.5)
     ll_manual <- sum(weights * (observed * log(estimated) - estimated - lgamma(observed + 1)))
     ll_func   <- MOSAIC::calc_log_likelihood_poisson(
          observed = observed,
          estimated = estimated,
          weights = weights,
          verbose = FALSE
     )
     expect_equal(ll_func, ll_manual, tolerance = 1e-8)
})
