# Unit tests for calc_log_mean_exp function

test_that("calc_log_mean_exp handles basic cases correctly", {
  
  # Simple case - should be close to log(mean(exp(x)))
  x <- c(-1, -2, -3)
  result <- calc_log_mean_exp(x)
  expected <- log(mean(exp(x)))
  expect_equal(result, expected, tolerance = 1e-10)
  
  # Single value
  expect_equal(calc_log_mean_exp(-5), -5)
  
  # Identical values
  expect_equal(calc_log_mean_exp(c(-10, -10, -10)), -10)
  
})

test_that("calc_log_mean_exp is numerically stable", {
  
  # Large negative values that would cause underflow in naive approach
  x <- c(-1000, -1001, -999)
  result <- calc_log_mean_exp(x)
  expect_true(is.finite(result))
  expect_true(result > -1001)
  expect_true(result < -999)
  
  # Mix of large negative values
  x <- c(-50000, -50001, -49999, -50000.5)
  result <- calc_log_mean_exp(x)
  expect_true(is.finite(result))
  
  # Values that would cause overflow if we tried exp() directly
  x <- c(-100, -200, -50)
  result <- calc_log_mean_exp(x)
  expect_true(is.finite(result))
  
})

test_that("calc_log_mean_exp handles edge cases", {
  
  # Empty vector
  expect_true(is.na(calc_log_mean_exp(c())))
  
  # All NA values
  expect_true(is.na(calc_log_mean_exp(c(NA, NA))))
  
  # All infinite values
  expect_true(is.na(calc_log_mean_exp(c(-Inf, -Inf))))
  expect_true(is.na(calc_log_mean_exp(c(Inf, Inf))))
  
  # Mix with non-finite values - should use only finite values
  x <- c(-10, NA, -12, -Inf, -11)
  result <- calc_log_mean_exp(x)
  expected <- calc_log_mean_exp(c(-10, -12, -11))
  expect_equal(result, expected)
  
  # Single finite value among non-finite
  x <- c(NA, -5, Inf, -Inf)
  expect_equal(calc_log_mean_exp(x), -5)
  
})

test_that("calc_log_mean_exp mathematical properties", {
  
  # Property: LME should be <= max(x) for negative values
  x <- c(-10, -15, -5, -20)
  result <- calc_log_mean_exp(x)
  expect_true(result <= max(x))
  
  # Property: For any values, LME should be <= log(sum(exp(x))) but >= max(x) - log(n)
  x <- c(1, 2, 3, 4)
  result <- calc_log_mean_exp(x)
  # LME = log(mean(exp(x))) which should be less than max(x) when values differ significantly
  expect_true(result <= max(x))
  expect_true(result >= max(x) - log(length(x)))  # Lower bound
  
  # Property: Adding constant to all values should add same constant to result
  x <- c(-10, -12, -8)
  constant <- 100
  result1 <- calc_log_mean_exp(x)
  result2 <- calc_log_mean_exp(x + constant)
  expect_equal(result2, result1 + constant, tolerance = 1e-10)
  
})

test_that("calc_log_mean_exp matches manual calculation for small values", {
  
  # For small absolute values, can verify against direct calculation
  x <- c(-0.1, -0.2, -0.3)
  result <- calc_log_mean_exp(x)
  expected <- log(mean(exp(x)))
  expect_equal(result, expected, tolerance = 1e-12)
  
  # Test with more values
  x <- c(-1, -1.5, -0.5, -2, -1.2)
  result <- calc_log_mean_exp(x)
  expected <- log(mean(exp(x)))
  expect_equal(result, expected, tolerance = 1e-12)
  
})

test_that("calc_log_mean_exp handles likelihood-like values", {
  
  # Typical log-likelihood values (large negative numbers)
  x <- c(-5000.123, -5001.456, -4999.789, -5000.001)
  result <- calc_log_mean_exp(x)
  expect_true(is.finite(result))
  expect_true(result > min(x))
  expect_true(result <= max(x))
  
  # Very similar likelihood values (test precision)
  x <- c(-1000.001, -1000.002, -1000.000)
  result <- calc_log_mean_exp(x)
  expect_true(abs(result - (-1000.001)) < 0.01)
  
})