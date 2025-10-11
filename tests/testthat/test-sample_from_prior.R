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
  result <- sample_from_prior(n = 1, prior = prior)
  expect_length(result, 1)
  expect_true(result >= 0 && result <= 1)

  # Multiple samples
  results <- sample_from_prior(n = 100, prior = prior)
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
  
  result <- sample_from_prior(n = 1, prior = prior)
  expect_length(result, 1)
  expect_true(result >= 0)

  results <- sample_from_prior(n = 100, prior = prior)
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
  
  result <- sample_from_prior(n = 1, prior = prior)
  expect_length(result, 1)
  expect_true(result > 0)

  results <- sample_from_prior(n = 100, prior = prior)
  expect_length(results, 100)
  expect_true(all(results > 0))
})

test_that("sample_from_prior handles lognormal with mean/sd", {
  prior <- list(
    distribution = "lognormal",
    parameters = list(mean = 10, sd = 2)
  )
  
  result <- sample_from_prior(n = 1, prior = prior)
  expect_length(result, 1)
  expect_true(result > 0)

  results <- sample_from_prior(n = 100, prior = prior)
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
  
  result <- sample_from_prior(n = 1, prior = prior)
  expect_length(result, 1)
  expect_type(result, "double")

  results <- sample_from_prior(n = 100, prior = prior)
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
  
  result <- sample_from_prior(n = 1, prior = prior)
  expect_length(result, 1)
  expect_true(result >= 2 && result <= 8)

  results <- sample_from_prior(n = 100, prior = prior)
  expect_length(results, 100)
  expect_true(all(results >= 2 & results <= 8))
  
  # Check approximate mean (min + max) / 2
  expected_mean <- (2 + 8) / 2
  expect_true(abs(mean(results) - expected_mean) < 0.5)
})

test_that("sample_from_prior handles NULL priors", {
  # Test with NULL prior
  result <- sample_from_prior(n = 1, prior = NULL)
  expect_length(result, 1)
  expect_true(is.na(result))

  results <- sample_from_prior(n = 5, prior = NULL)
  expect_length(results, 5)
  expect_true(all(is.na(results)))
})

test_that("sample_from_prior validates parameters", {
  # Invalid n
  prior <- list(distribution = "beta", parameters = list(shape1 = 2, shape2 = 5))
  expect_error(
    sample_from_prior(n = 0, prior = prior),
    "positive integer"
  )
  expect_error(
    sample_from_prior(n = -1, prior = prior),
    "positive integer"
  )
  expect_error(
    sample_from_prior(n = 2.5, prior = prior),
    "positive integer"
  )

  # Invalid prior structure - should return NA, not error
  result <- sample_from_prior(n = 1, prior = "not a list")
  expect_true(is.na(result))

  # Missing distribution field
  bad_prior <- list(parameters = list(shape1 = 2, shape2 = 5))
  result <- sample_from_prior(n = 1, prior = bad_prior)
  expect_true(is.na(result))

  # Missing parameters field
  bad_prior <- list(distribution = "beta")
  result <- sample_from_prior(n = 1, prior = bad_prior)
  expect_true(is.na(result))
})

test_that("sample_from_prior handles invalid distribution parameters", {
  # Beta with invalid shape parameters
  prior <- list(
    distribution = "beta",
    parameters = list(shape1 = -1, shape2 = 5)
  )
  result <- sample_from_prior(n = 1, prior = prior)
  expect_true(is.na(result))

  # Gamma with invalid parameters
  prior <- list(
    distribution = "gamma",
    parameters = list(shape = 0, rate = 1)
  )
  result <- sample_from_prior(n = 1, prior = prior)
  expect_true(is.na(result))

  # Uniform with min >= max
  prior <- list(
    distribution = "uniform",
    parameters = list(min = 5, max = 5)
  )
  result <- sample_from_prior(n = 1, prior = prior)
  expect_true(is.na(result))
})

test_that("sample_from_prior handles unknown distribution types", {
  prior <- list(
    distribution = "unknown_dist",
    parameters = list(param1 = 1, param2 = 2)
  )

  # Should return NA for unknown distribution
  result <- sample_from_prior(n = 1, prior = prior)
  expect_true(is.na(result))
})

test_that("verbose option works correctly", {
  # Capture messages with verbose = TRUE
  expect_message(
    sample_from_prior(n = 1, prior = NULL, verbose = TRUE),
    "Prior is NULL"
  )

  # No messages with verbose = FALSE (default)
  expect_silent(
    sample_from_prior(n = 1, prior = NULL, verbose = FALSE)
  )
})

test_that("sample_from_prior handles truncated normal distribution", {
  # Test basic functionality
  prior <- list(
    distribution = "truncnorm",
    parameters = list(mean = 0, sd = 1, a = -2, b = 2)
  )

  # Skip test if truncnorm package not available
  if (!requireNamespace("truncnorm", quietly = TRUE)) {
    skip("truncnorm package not available")
  }

  # Single sample
  result <- sample_from_prior(n = 1, prior = prior)
  expect_length(result, 1)
  expect_true(result >= -2 && result <= 2)
  expect_false(is.na(result))

  # Multiple samples
  results <- sample_from_prior(n = 100, prior = prior)
  expect_length(results, 100)
  expect_true(all(results >= -2 & results <= 2))
  expect_false(any(is.na(results)))

  # Check approximate mean (should be close to 0 for symmetric bounds)
  expect_true(abs(mean(results)) < 0.3)

  # Test with asymmetric bounds
  prior_asym <- list(
    distribution = "truncnorm",
    parameters = list(mean = 5, sd = 2, a = 0, b = 10)
  )

  results_asym <- sample_from_prior(n = 100, prior = prior_asym)
  expect_length(results_asym, 100)
  expect_true(all(results_asym >= 0 & results_asym <= 10))
  expect_true(mean(results_asym) > 3 && mean(results_asym) < 7)  # Should be between bounds, biased toward mean
})

test_that("sample_from_prior validates truncated normal parameters", {
  # Skip test if truncnorm package not available
  if (!requireNamespace("truncnorm", quietly = TRUE)) {
    skip("truncnorm package not available")
  }

  # Missing required parameters
  prior_missing <- list(
    distribution = "truncnorm",
    parameters = list(mean = 0, sd = 1)  # missing a and b
  )
  result <- sample_from_prior(n = 1, prior = prior_missing)
  expect_true(is.na(result))

  # Invalid bounds (a >= b)
  prior_invalid_bounds <- list(
    distribution = "truncnorm",
    parameters = list(mean = 0, sd = 1, a = 2, b = 2)  # a = b
  )
  result <- sample_from_prior(n = 1, prior = prior_invalid_bounds)
  expect_true(is.na(result))

  prior_invalid_bounds2 <- list(
    distribution = "truncnorm",
    parameters = list(mean = 0, sd = 1, a = 3, b = 1)  # a > b
  )
  result <- sample_from_prior(n = 1, prior = prior_invalid_bounds2)
  expect_true(is.na(result))

  # Negative standard deviation
  prior_negative_sd <- list(
    distribution = "truncnorm",
    parameters = list(mean = 0, sd = -1, a = -2, b = 2)
  )
  result <- sample_from_prior(n = 1, prior = prior_negative_sd)
  expect_true(is.na(result))

  # NA values in parameters
  prior_na <- list(
    distribution = "truncnorm",
    parameters = list(mean = NA, sd = 1, a = -2, b = 2)
  )
  result <- sample_from_prior(n = 1, prior = prior_na, verbose = TRUE)
  expect_true(is.na(result))
})

test_that("sample_from_prior handles truncnorm package not available", {
  # Temporarily hide the truncnorm package if it exists
  if (requireNamespace("truncnorm", quietly = TRUE)) {
    # Mock the requireNamespace function to return FALSE
    mockery_available <- requireNamespace("mockery", quietly = TRUE)

    if (mockery_available) {
      # Use mockery if available
      mockery::stub(sample_from_prior, 'requireNamespace', FALSE)

      prior <- list(
        distribution = "truncnorm",
        parameters = list(mean = 0, sd = 1, a = -2, b = 2)
      )

      result <- sample_from_prior(n = 1, prior = prior)
      expect_true(is.na(result))
    } else {
      # Skip if mockery not available
      skip("mockery package not available for mocking truncnorm dependency")
    }
  } else {
    # Test the actual error when truncnorm is not available
    prior <- list(
      distribution = "truncnorm",
      parameters = list(mean = 0, sd = 1, a = -2, b = 2)
    )

    result <- sample_from_prior(n = 1, prior = prior)
    expect_true(is.na(result))
  }
})

test_that("sample_from_prior handles extreme truncated normal bounds", {
  # Skip test if truncnorm package not available
  if (!requireNamespace("truncnorm", quietly = TRUE)) {
    skip("truncnorm package not available")
  }

  # Very tight bounds around the mean
  prior_tight <- list(
    distribution = "truncnorm",
    parameters = list(mean = 5, sd = 2, a = 4.9, b = 5.1)
  )

  results <- sample_from_prior(n = 50, prior = prior_tight)
  expect_length(results, 50)
  expect_true(all(results >= 4.9 & results <= 5.1))
  expect_true(abs(mean(results) - 5) < 0.05)  # Should be very close to 5

  # Bounds far from the mean
  prior_far <- list(
    distribution = "truncnorm",
    parameters = list(mean = 0, sd = 1, a = 10, b = 15)
  )

  results_far <- sample_from_prior(n = 50, prior = prior_far)
  expect_length(results_far, 50)
  expect_true(all(results_far >= 10 & results_far <= 15))
  # Mean should be much closer to lower bound since original mean is far below
  expect_true(mean(results_far) > 10)
})

test_that("sample_from_prior handles gompertz distribution if available", {
  # Only test if rgompertz function exists
  if (exists("rgompertz", mode = "function")) {
    prior <- list(
      distribution = "gompertz",
      parameters = list(b = 0.5, eta = 2)
    )

    result <- sample_from_prior(n = 1, prior = prior)
    expect_length(result, 1)
    expect_true(result >= 0)

    results <- sample_from_prior(n = 100, prior = prior)
    expect_length(results, 100)
    expect_true(all(results >= 0))
  } else {
    skip("rgompertz function not available")
  }
})