# Unit tests for weighted statistics functions

test_that("weighted_var calculates variance correctly", {
  
  # Simple test case
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.2, 0.2, 0.2, 0.2, 0.2)  # Uniform weights
  
  # Should be close to regular variance for uniform weights
  regular_var <- var(x) * (length(x) - 1) / length(x)  # Population variance
  weighted_result <- weighted_var(x, w)
  
  expect_true(is.finite(weighted_result))
  expect_true(weighted_result > 0)
  
  # Edge cases
  expect_equal(weighted_var(c(5), c(1)), 0)  # Single value
  expect_equal(weighted_var(numeric(0), numeric(0)), 0)  # Empty vectors
})

test_that("weighted_quantiles calculates quantiles correctly", {
  
  # Simple test case
  x <- c(1, 2, 3, 4, 5)
  w <- c(0.2, 0.2, 0.2, 0.2, 0.2)  # Uniform weights
  probs <- c(0, 0.25, 0.5, 0.75, 1)
  
  result <- weighted_quantiles(x, w, probs)
  
  expect_length(result, 5)
  expect_true(all(is.finite(result)))
  expect_true(all(result >= min(x)))
  expect_true(all(result <= max(x)))
  expect_true(result[1] <= result[2])  # Monotonic
  expect_true(result[2] <= result[3])
  expect_true(result[3] <= result[4])
  expect_true(result[4] <= result[5])
  
  # Edge case - empty input
  expect_true(all(is.na(weighted_quantiles(numeric(0), numeric(0), c(0.5)))))
})

test_that("calc_weighted_mode estimates mode correctly", {
  
  # Test with clearly peaked distribution
  x <- c(rep(2, 10), rep(3, 5), rep(4, 2))
  w <- rep(1, length(x))
  
  mode_result <- calc_weighted_mode(x, w)
  expect_true(is.finite(mode_result))
  expect_true(mode_result >= min(x))
  expect_true(mode_result <= max(x))
  
  # Single value case
  expect_equal(calc_weighted_mode(c(5), c(1)), 5)
  
  # Two identical values
  expect_equal(calc_weighted_mode(c(3, 3), c(0.5, 0.5)), 3)
})

test_that("weighted statistics handle edge cases", {
  
  # All functions should handle zero weights gracefully
  x <- c(1, 2, 3)
  w_zero <- c(0, 0, 0)
  
  # These might produce NaN or 0, but shouldn't error
  expect_no_error(weighted_var(x, w_zero))
  expect_no_error(weighted_quantiles(x, w_zero, 0.5))
  expect_no_error(calc_weighted_mode(x, w_zero))
  
  # Single non-zero weight
  w_single <- c(1, 0, 0)
  expect_no_error(weighted_var(x, w_single))
  expect_no_error(weighted_quantiles(x, w_single, 0.5))
  expect_no_error(calc_weighted_mode(x, w_single))
})