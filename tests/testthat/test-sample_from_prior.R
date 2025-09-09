# Test file for sample_from_prior unified function

library(testthat)

# Source the function
source("../../R/sample_from_prior.R")

# Helper to check if rgompertz exists (from fit_gompertz_from_ci.R)
if (file.exists("../../R/fit_gompertz_from_ci.R")) {
  source("../../R/fit_gompertz_from_ci.R")
}

test_that("sample_from_prior handles beta distribution", {
  prior <- list(
    distribution = "beta",
    parameters = list(shape1 = 2, shape2 = 5)
  )
  
  # Single sample
  result <- sample_from_prior(n = 1, prior_name = "test_beta", prior_obj = prior)
  expect_length(result, 1)
  expect_true(result >= 0 && result <= 1)
  
  # Multiple samples
  results <- sample_from_prior(n = 100, prior_name = "test_beta", prior_obj = prior)
  expect_length(results, 100)
  expect_true(all(results >= 0 & results <= 1))
  
  # Check approximate mean
  expected_mean <- 2 / (2 + 5)  # shape1 / (shape1 + shape2)
  expect_true(abs(mean(results) - expected_mean) < 0.1)
})

test_that("sample_from_prior handles gamma distribution", {
  prior <- list(
    distribution = "gamma",
    parameters = list(shape = 2, rate = 1)
  )
  
  result <- sample_from_prior(n = 1, prior_name = "test_gamma", prior_obj = prior)
  expect_length(result, 1)
  expect_true(result >= 0)
  
  results <- sample_from_prior(n = 100, prior_name = "test_gamma", prior_obj = prior)
  expect_length(results, 100)
  expect_true(all(results >= 0))
  
  # Check approximate mean (shape/rate)
  expected_mean <- 2 / 1
  expect_true(abs(mean(results) - expected_mean) < 0.5)
})

test_that("sample_from_prior handles lognormal with meanlog/sdlog", {
  prior <- list(
    distribution = "lognormal",
    parameters = list(meanlog = 0, sdlog = 1)
  )
  
  result <- sample_from_prior(n = 1, prior_name = "test_lognorm", prior_obj = prior)
  expect_length(result, 1)
  expect_true(result > 0)
  
  results <- sample_from_prior(n = 100, prior_name = "test_lognorm", prior_obj = prior)
  expect_length(results, 100)
  expect_true(all(results > 0))
})

test_that("sample_from_prior handles lognormal with mean/sd", {
  prior <- list(
    distribution = "lognormal",
    parameters = list(mean = 10, sd = 2)
  )
  
  result <- sample_from_prior(n = 1, prior_name = "test_lognorm_meansd", prior_obj = prior)
  expect_length(result, 1)
  expect_true(result > 0)
  
  results <- sample_from_prior(n = 100, prior_name = "test_lognorm_meansd", prior_obj = prior)
  expect_length(results, 100)
  expect_true(all(results > 0))
  
  # Check approximate mean
  expect_true(abs(mean(results) - 10) < 2)
})

test_that("sample_from_prior handles normal distribution", {
  prior <- list(
    distribution = "normal",
    parameters = list(mean = 5, sd = 1)
  )
  
  result <- sample_from_prior(n = 1, prior_name = "test_normal", prior_obj = prior)
  expect_length(result, 1)
  expect_type(result, "double")
  
  results <- sample_from_prior(n = 100, prior_name = "test_normal", prior_obj = prior)
  expect_length(results, 100)
  
  # Check approximate mean and sd
  expect_true(abs(mean(results) - 5) < 0.3)
  expect_true(abs(sd(results) - 1) < 0.3)
})

test_that("sample_from_prior handles uniform distribution", {
  prior <- list(
    distribution = "uniform",
    parameters = list(min = 2, max = 8)
  )
  
  result <- sample_from_prior(n = 1, prior_name = "test_uniform", prior_obj = prior)
  expect_length(result, 1)
  expect_true(result >= 2 && result <= 8)
  
  results <- sample_from_prior(n = 100, prior_name = "test_uniform", prior_obj = prior)
  expect_length(results, 100)
  expect_true(all(results >= 2 & results <= 8))
  
  # Check approximate mean (min + max) / 2
  expected_mean <- (2 + 8) / 2
  expect_true(abs(mean(results) - expected_mean) < 0.5)
})

test_that("sample_from_prior handles NULL priors", {
  # With na_on_error = TRUE (default)
  result <- sample_from_prior(n = 1, prior_name = "test_null", prior_obj = NULL)
  expect_length(result, 1)
  expect_true(is.na(result))
  
  results <- sample_from_prior(n = 5, prior_name = "test_null", prior_obj = NULL)
  expect_length(results, 5)
  expect_true(all(is.na(results)))
  
  # With na_on_error = FALSE
  expect_error(
    sample_from_prior(n = 1, prior_name = "test_null", prior_obj = NULL, na_on_error = FALSE),
    "Prior is NULL"
  )
})

test_that("sample_from_prior validates parameters", {
  # Invalid n
  prior <- list(distribution = "beta", parameters = list(shape1 = 2, shape2 = 5))
  expect_error(
    sample_from_prior(n = 0, prior_name = "test", prior_obj = prior),
    "positive integer"
  )
  expect_error(
    sample_from_prior(n = -1, prior_name = "test", prior_obj = prior),
    "positive integer"
  )
  expect_error(
    sample_from_prior(n = 2.5, prior_name = "test", prior_obj = prior),
    "positive integer"
  )
  
  # Invalid prior structure
  expect_error(
    sample_from_prior(n = 1, prior_name = "test", prior_obj = "not a list", na_on_error = FALSE),
    "must be a list"
  )
  
  # Missing distribution field
  bad_prior <- list(parameters = list(shape1 = 2, shape2 = 5))
  result <- sample_from_prior(n = 1, prior_name = "test", prior_obj = bad_prior)
  expect_true(is.na(result))
  
  # Missing parameters field
  bad_prior <- list(distribution = "beta")
  result <- sample_from_prior(n = 1, prior_name = "test", prior_obj = bad_prior)
  expect_true(is.na(result))
})

test_that("sample_from_prior handles invalid distribution parameters", {
  # Beta with invalid shape parameters
  prior <- list(
    distribution = "beta",
    parameters = list(shape1 = -1, shape2 = 5)
  )
  result <- sample_from_prior(n = 1, prior_name = "test_bad_beta", prior_obj = prior)
  expect_true(is.na(result))
  
  # Gamma with invalid parameters
  prior <- list(
    distribution = "gamma",
    parameters = list(shape = 0, rate = 1)
  )
  result <- sample_from_prior(n = 1, prior_name = "test_bad_gamma", prior_obj = prior)
  expect_true(is.na(result))
  
  # Uniform with min >= max
  prior <- list(
    distribution = "uniform",
    parameters = list(min = 5, max = 5)
  )
  result <- sample_from_prior(n = 1, prior_name = "test_bad_uniform", prior_obj = prior)
  expect_true(is.na(result))
})

test_that("sample_from_prior handles unknown distribution types", {
  prior <- list(
    distribution = "unknown_dist",
    parameters = list(param1 = 1, param2 = 2)
  )
  
  # With na_on_error = TRUE
  result <- sample_from_prior(n = 1, prior_name = "test_unknown", prior_obj = prior)
  expect_true(is.na(result))
  
  # With na_on_error = FALSE
  expect_error(
    sample_from_prior(n = 1, prior_name = "test_unknown", prior_obj = prior, na_on_error = FALSE),
    "Unknown distribution type"
  )
})

test_that("verbose option works correctly", {
  # Capture messages with verbose = TRUE
  expect_message(
    sample_from_prior(n = 1, prior_name = "test_verbose", prior_obj = NULL, verbose = TRUE),
    "Prior is NULL"
  )
  
  # No messages with verbose = FALSE (default)
  expect_silent(
    sample_from_prior(n = 1, prior_name = "test_quiet", prior_obj = NULL, verbose = FALSE)
  )
})

test_that("sample_from_prior handles gompertz distribution if available", {
  # Only test if rgompertz function exists
  if (exists("rgompertz", mode = "function")) {
    prior <- list(
      distribution = "gompertz",
      parameters = list(b = 0.5, eta = 2)
    )
    
    result <- sample_from_prior(n = 1, prior_name = "test_gompertz", prior_obj = prior)
    expect_length(result, 1)
    expect_true(result >= 0)
    
    results <- sample_from_prior(n = 100, prior_name = "test_gompertz", prior_obj = prior)
    expect_length(results, 100)
    expect_true(all(results >= 0))
  } else {
    skip("rgompertz function not available")
  }
})