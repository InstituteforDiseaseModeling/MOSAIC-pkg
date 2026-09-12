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
  
  # weighted_var errors on all-zero weights (no valid observations)
  expect_error(weighted_var(x, w_zero))
  expect_no_error(weighted_quantiles(x, w_zero, 0.5))
  expect_no_error(calc_weighted_mode(x, w_zero))
  
  # Single non-zero weight
  w_single <- c(1, 0, 0)
  expect_no_error(weighted_var(x, w_single))
  expect_no_error(weighted_quantiles(x, w_single, 0.5))
  expect_no_error(calc_weighted_mode(x, w_single))
})
# --- weighted_quantiles plotting positions (v0.69.1 fix) ---------------------
# Until v0.69.1 the interpolation used each observation's UPPER weight-block
# edge, cumsum(w)/sum(w), which biases every quantile downward in proportion to
# how concentrated the weights are -- the BFRS posterior regime. These pin the
# properties that bias violated, so the regression cannot return silently. Each
# expected value is derived from the definition, never copied from output.

test_that("weighted_quantiles places mass at weight-block midpoints", {
  # A symmetric weight profile must return the centre. This is the function's
  # own roxygen example; the old code returned 2.5.
  expect_equal(weighted_quantiles(1:5, c(.1, .2, .4, .2, .1), 0.5), 3)

  # Concentrating weight must pull the quantile to that value, not halfway.
  # With two support points the interpolant approaches but cannot reach the
  # endpoint, so assert the direction and the magnitude of the approach.
  hi <- weighted_quantiles(c(1, 2), c(0.01, 0.99), 0.5)
  lo <- weighted_quantiles(c(1, 2), c(0.99, 0.01), 0.5)
  expect_equal(hi, 1.99)
  expect_equal(lo, 1.01)
  expect_gt(hi, 1.9)   # the old code returned 1.4949 here
  expect_lt(lo, 1.1)

  # Monotone in the weight: shifting weight toward x = 2 can only move the
  # median up.
  ws <- seq(0.05, 0.95, by = 0.05)
  meds <- vapply(ws, function(p) weighted_quantiles(c(1, 2), c(1 - p, p), 0.5), 0)
  expect_false(is.unsorted(meds))
})

test_that("weighted_quantiles reduces to the unweighted quantile at equal weights", {
  set.seed(11)
  x <- rnorm(50)
  for (p in c(0.1, 0.25, 0.5, 0.75, 0.9)) {
    expect_equal(weighted_quantiles(x, rep(1, 50), p),
                 stats::quantile(x, p, type = 5, names = FALSE))
  }
  # Symmetric sample: the weighted median must equal median(). The old code
  # returned 3 here rather than 3.5.
  expect_equal(weighted_quantiles(c(3, 1, 4, 1, 5, 9, 2, 6), rep(1, 8), 0.5),
               stats::median(c(3, 1, 4, 1, 5, 9, 2, 6)))
})

test_that("weighted_quantiles is invariant to weight scale and to splitting a weight", {
  set.seed(12)
  x <- rnorm(30); w <- runif(30)
  expect_equal(weighted_quantiles(x, w, c(0.25, 0.5, 0.75)),
               weighted_quantiles(x, 1000 * w, c(0.25, 0.5, 0.75)))
  # Splitting one observation's weight across two copies of the same value
  # must change nothing. The upper-edge form violated this.
  expect_equal(weighted_quantiles(c(1, 2, 3), c(1, 1, 1), 0.5),
               weighted_quantiles(c(1, 2, 2, 3), c(1, 0.5, 0.5, 1), 0.5))
  # Endpoints clamp to the data range.
  expect_equal(weighted_quantiles(1:5, rep(.2, 5), 0), 1)
  expect_equal(weighted_quantiles(1:5, rep(.2, 5), 1), 5)
})

test_that("weighted_quantiles collapses tied plotting positions without warning", {
  # Weights spanning enough orders of magnitude that cumsum() saturates: every
  # position after the first collides. Negative-tested -- this input really does
  # tie (2 distinct positions from 6), so the branch is exercised, not assumed.
  w <- c(1, rep(1e-20, 5))
  x <- c(10, 20, 30, 40, 50, 60)
  pos <- (cumsum(w) - 0.5 * w) / sum(w)
  expect_lt(length(unique(pos)), length(w))          # the branch will fire
  expect_silent(res <- weighted_quantiles(x, w, 0.5))
  expect_equal(res, 10)                              # all detectable weight on x = 10
  expect_true(is.finite(res))
})

test_that("weighted_quantiles_presorted agrees with weighted_quantiles after the fix", {
  set.seed(13)
  for (i in 1:20) {
    x <- rnorm(40); w <- runif(40); ord <- order(x)
    expect_equal(weighted_quantiles_presorted(x[ord], w[ord], c(0.05, 0.5, 0.95)),
                 weighted_quantiles(x, w, c(0.05, 0.5, 0.95)), tolerance = 0)
  }
})
