# test-calc_kl_divergence.R

test_that("calc_kl_divergence handles basic distributions correctly", {
  set.seed(123)

  # Test 1: Identical distributions should have KL divergence close to 0
  samples <- rnorm(1000)
  kl_div <- calc_kl_divergence(samples, NULL, samples, NULL)
  expect_true(kl_div >= 0)
  expect_true(kl_div < 0.01)  # Should be very close to 0

  # Test 2: Different distributions should have positive KL divergence
  samples1 <- rnorm(1000, mean = 0, sd = 1)
  samples2 <- rnorm(1000, mean = 2, sd = 1)
  kl_div <- calc_kl_divergence(samples1, NULL, samples2, NULL)
  expect_true(kl_div > 0)

  # Test 3: KL divergence is not symmetric
  kl_div_forward <- calc_kl_divergence(samples1, NULL, samples2, NULL)
  kl_div_backward <- calc_kl_divergence(samples2, NULL, samples1, NULL)
  expect_false(abs(kl_div_forward - kl_div_backward) < 1e-10)
})

test_that("calc_kl_divergence handles weighted samples", {
  set.seed(456)

  samples1 <- rnorm(500)
  weights1 <- runif(500, 0.5, 1.5)
  samples2 <- rnorm(500, mean = 1)
  weights2 <- runif(500, 0.5, 1.5)

  # Should work with weights
  kl_div <- calc_kl_divergence(samples1, weights1, samples2, weights2)
  expect_true(is.numeric(kl_div))
  expect_true(kl_div >= 0)

  # Different from unweighted version
  kl_div_unweighted <- calc_kl_divergence(samples1, NULL, samples2, NULL)
  expect_false(abs(kl_div - kl_div_unweighted) < 1e-10)
})

test_that("calc_kl_divergence validates input parameters", {
  set.seed(789)
  samples1 <- rnorm(100)
  samples2 <- rnorm(100)

  # Test non-numeric samples
  expect_error(calc_kl_divergence(c("a", "b"), NULL, samples2, NULL),
               "samples1 and samples2 must be numeric vectors")
  expect_error(calc_kl_divergence(samples1, NULL, c("a", "b"), NULL),
               "samples1 and samples2 must be numeric vectors")

  # Test empty samples
  expect_error(calc_kl_divergence(numeric(0), NULL, samples2, NULL),
               "samples1 and samples2 must not be empty")
  expect_error(calc_kl_divergence(samples1, NULL, numeric(0), NULL),
               "samples1 and samples2 must not be empty")

  # Test samples with insufficient data points
  expect_error(calc_kl_divergence(c(1), NULL, samples2, NULL),
               "samples1 and samples2 must have at least 2 points for density estimation")
  expect_error(calc_kl_divergence(samples1, NULL, c(1), NULL),
               "samples1 and samples2 must have at least 2 points for density estimation")

  # Test non-finite values
  expect_error(calc_kl_divergence(c(1, 2, NA), NULL, samples2, NULL),
               "samples1 and samples2 must contain only finite values")
  expect_error(calc_kl_divergence(samples1, NULL, c(1, 2, Inf), NULL),
               "samples1 and samples2 must contain only finite values")

  # Test weight validation
  expect_error(calc_kl_divergence(samples1, rep(1, 50), samples2, NULL),
               "weights1 must have the same length as samples1")
  expect_error(calc_kl_divergence(samples1, NULL, samples2, rep(1, 50)),
               "weights2 must have the same length as samples2")
  expect_error(calc_kl_divergence(samples1, c(-1, rep(1, 99)), samples2, NULL),
               "weights1 must be non-negative")
  expect_error(calc_kl_divergence(samples1, rep(0, 100), samples2, NULL),
               "weights1 must have non-zero sum")

  # Test n_points validation
  expect_error(calc_kl_divergence(samples1, NULL, samples2, NULL, n_points = -1),
               "n_points must be a positive integer")
  expect_error(calc_kl_divergence(samples1, NULL, samples2, NULL, n_points = 0),
               "n_points must be a positive integer")
  expect_error(calc_kl_divergence(samples1, NULL, samples2, NULL, n_points = 1),
               "n_points must be at least 2")
  expect_error(calc_kl_divergence(samples1, NULL, samples2, NULL, n_points = "abc"),
               "n_points must be a positive integer")

  # Test eps validation
  expect_error(calc_kl_divergence(samples1, NULL, samples2, NULL, eps = -1),
               "eps must be a positive numeric value")
  expect_error(calc_kl_divergence(samples1, NULL, samples2, NULL, eps = 0),
               "eps must be a positive numeric value")
  expect_error(calc_kl_divergence(samples1, NULL, samples2, NULL, eps = "abc"),
               "eps must be a positive numeric value")
})

test_that("calc_kl_divergence handles edge cases", {
  # Test with identical values
  expect_warning(
    kl_div <- calc_kl_divergence(rep(5, 100), NULL, rep(5, 100), NULL),
    "All samples have the same value"
  )
  expect_equal(kl_div, 0)

  # Test with very small sample sizes
  kl_div <- calc_kl_divergence(c(1, 2), NULL, c(1.5, 2.5), NULL)
  expect_true(is.numeric(kl_div))
  expect_true(kl_div >= 0)
})

test_that("calc_kl_divergence works with different n_points", {
  set.seed(321)
  samples1 <- rnorm(500)
  samples2 <- rnorm(500, mean = 0.5)

  # Test with different n_points values
  kl_div_100 <- calc_kl_divergence(samples1, NULL, samples2, NULL, n_points = 100)
  kl_div_1000 <- calc_kl_divergence(samples1, NULL, samples2, NULL, n_points = 1000)
  kl_div_5000 <- calc_kl_divergence(samples1, NULL, samples2, NULL, n_points = 5000)

  # All should be positive
  expect_true(all(c(kl_div_100, kl_div_1000, kl_div_5000) >= 0))

  # They should be reasonably close but not identical
  expect_true(abs(kl_div_100 - kl_div_1000) < 0.5)
  expect_true(abs(kl_div_1000 - kl_div_5000) < 0.1)
})

test_that("calc_kl_divergence works with different eps values", {
  set.seed(654)
  samples1 <- rnorm(500)
  samples2 <- rnorm(500, mean = 0.5)

  # Test with different eps values
  kl_div_small <- calc_kl_divergence(samples1, NULL, samples2, NULL, eps = 1e-15)
  kl_div_default <- calc_kl_divergence(samples1, NULL, samples2, NULL, eps = 1e-10)
  kl_div_large <- calc_kl_divergence(samples1, NULL, samples2, NULL, eps = 1e-5)

  # All should be positive
  expect_true(all(c(kl_div_small, kl_div_default, kl_div_large) >= 0))

  # They should be reasonably close
  expect_true(abs(kl_div_small - kl_div_default) < 0.01)
  expect_true(abs(kl_div_default - kl_div_large) < 0.01)
})

test_that("deprecated calculate_kl_divergence function still works", {
  set.seed(987)
  samples1 <- rnorm(100)
  samples2 <- rnorm(100, mean = 1)

  # Should produce deprecation warning
  expect_warning(
    kl_div_old <- calculate_kl_divergence(samples1, NULL, samples2, NULL),
    "deprecated"
  )

  # Should produce same result as new function
  kl_div_new <- calc_kl_divergence(samples1, NULL, samples2, NULL)
  expect_equal(kl_div_old, kl_div_new)
})

test_that("calc_kl_divergence produces reasonable values for known distributions", {
  set.seed(1234)
  n <- 10000  # Large sample for better approximation

  # Test 1: Two normal distributions with same variance
  # Theoretical KL divergence: 0.5 * ((mu1 - mu2)^2 / sigma^2)
  mu1 <- 0
  mu2 <- 1
  sigma <- 1
  samples1 <- rnorm(n, mean = mu1, sd = sigma)
  samples2 <- rnorm(n, mean = mu2, sd = sigma)

  theoretical_kl <- 0.5 * ((mu1 - mu2)^2 / sigma^2)
  empirical_kl <- calc_kl_divergence(samples1, NULL, samples2, NULL, n_points = 2000)

  # Should be within reasonable tolerance (accounting for sampling and estimation error)
  expect_true(abs(empirical_kl - theoretical_kl) < 0.1)

  # Test 2: Identical distributions should have KL close to 0
  samples_identical <- rnorm(n)
  kl_identical <- calc_kl_divergence(samples_identical, NULL, samples_identical, NULL)
  expect_true(kl_identical < 0.001)
})

test_that("calc_kl_divergence handles weight normalization correctly", {
  set.seed(5678)
  samples1 <- rnorm(100)
  samples2 <- rnorm(100, mean = 0.5)

  # Test with unnormalized weights
  weights1 <- runif(100, 1, 10)  # Not normalized
  weights2 <- runif(100, 2, 20)  # Not normalized

  # Function should normalize internally
  kl_div <- calc_kl_divergence(samples1, weights1, samples2, weights2)
  expect_true(is.numeric(kl_div))
  expect_true(kl_div >= 0)

  # Should get same result with pre-normalized weights
  weights1_norm <- weights1 / sum(weights1)
  weights2_norm <- weights2 / sum(weights2)
  kl_div_norm <- calc_kl_divergence(samples1, weights1_norm, samples2, weights2_norm)

  expect_equal(kl_div, kl_div_norm, tolerance = 1e-10)
})

test_that("calc_kl_divergence handles extreme distributions", {
  set.seed(9999)

  # Test with very different scales
  samples1 <- rnorm(500, mean = 0, sd = 0.01)  # Very narrow
  samples2 <- rnorm(500, mean = 0, sd = 10)    # Very wide

  kl_div <- calc_kl_divergence(samples1, NULL, samples2, NULL)
  expect_true(is.numeric(kl_div))
  expect_true(kl_div > 0)  # Should be large due to very different spreads

  # Test with separated distributions
  samples1 <- rnorm(500, mean = -10, sd = 1)
  samples2 <- rnorm(500, mean = 10, sd = 1)

  kl_div <- calc_kl_divergence(samples1, NULL, samples2, NULL)
  expect_true(is.numeric(kl_div))
  expect_true(kl_div > 1)  # Should be large due to separation
})