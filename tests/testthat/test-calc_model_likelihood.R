
# Helper: simple zero data yields zero log-likelihood for Poisson
obs_zero <- matrix(0, nrow = 2, ncol = 3)
est_zero <- matrix(0, nrow = 2, ncol = 3)

# 1. Basic functionality: zero observed and estimated => LL = 0
testthat::test_that("zero data returns zero log-likelihood", {
     ll <- MOSAIC::calc_model_likelihood(
          obs_cases     = obs_zero,
          est_cases     = est_zero,
          obs_deaths    = obs_zero,
          est_deaths    = est_zero
     )
     expect_equal(ll, 0)
})

# 2. Weight scaling: non-default weight_cases and weight_deaths still yields zero
testthat::test_that("weights do not affect zero-data result", {
     ll <- MOSAIC::calc_model_likelihood(
          obs_cases     = obs_zero,
          est_cases     = est_zero,
          obs_deaths    = obs_zero,
          est_deaths    = est_zero,
          weight_cases  = 2,
          weight_deaths = 3
     )
     expect_equal(ll, 0)
})

# 3. Dimension errors: non-matrix inputs
testthat::test_that("errors on non-matrix inputs", {
     expect_error(
          MOSAIC::calc_model_likelihood(
               obs_cases  = as.list(obs_zero),
               est_cases  = est_zero,
               obs_deaths = obs_zero,
               est_deaths = est_zero
          ),
          "inputs must be matrices"
     )
})

# 4. Dimension mismatch error
testthat::test_that("errors when matrices have different dimensions", {
     est_bad <- matrix(0, nrow = 1, ncol = 3)
     expect_error(
          MOSAIC::calc_model_likelihood(
               obs_cases  = obs_zero,
               est_cases  = est_bad,
               obs_deaths = obs_zero,
               est_deaths = obs_zero
          ),
          "All matrices must have the same dimensions"
     )
})

# 5. Weight vector length check
testthat::test_that("errors when weight vectors have incorrect lengths", {
     expect_error(
          MOSAIC::calc_model_likelihood(
               obs_cases         = obs_zero,
               est_cases         = est_zero,
               obs_deaths        = obs_zero,
               est_deaths        = est_zero,
               weights_location  = c(1),
               weights_time      = c(1)
          ),
          "weights_location must match the number of locations"
     )
})

# 6. All NA skip returns NA
testthat::test_that("all NA data returns NA", {

     obs_na <- matrix(NA_real_, nrow = 2, ncol = 3)
     est_na <- matrix(NA_real_, nrow = 2, ncol = 3)

     ll <- MOSAIC::calc_model_likelihood(
          obs_cases  = obs_na,
          est_cases  = est_na,
          obs_deaths = obs_na,
          est_deaths = est_na,
          verbose     = TRUE
     )
     expect_true(is.na(ll))
})

# 6.5. Real values for estimated and all NA observed
testthat::test_that("all-NA observed with real estimates returns NA", {

     obs_na   <- matrix(NA_real_, nrow = 1, ncol = 2)
     est_real <- matrix(c(1.2, 3.4),   nrow = 1, ncol = 2)

     ll <- MOSAIC::calc_model_likelihood(
          obs_cases  = obs_na,
          est_cases  = est_real,
          obs_deaths = obs_na,
          est_deaths = est_real,
          verbose    = TRUE
     )

     expect_true(is.na(ll))
})

# 7. Real likelihood calculation
testthat::test_that("correct log-likelihood for simple non-zero data", {

     obs <- matrix(c(1, 1), nrow = 1, ncol = 2)
     est <- matrix(c(1, 1), nrow = 1, ncol = 2)

     ll <- MOSAIC::calc_model_likelihood(
          obs_cases  = obs,
          est_cases  = est,
          obs_deaths = obs,
          est_deaths = est
     )

     # ll_cases = sum(dpois(1,1,log=TRUE)*2) = -2
     # ll_max_cases = dpois(1,1,log=TRUE) = -1
     # total = 2*(ll_cases + ll_max_cases) = 2*(-2 + -1) = -6

     expect_equal(ll, -6)
})


