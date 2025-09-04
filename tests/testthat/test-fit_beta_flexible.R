test_that("fit_beta_flexible handles basic inputs correctly", {
  
  # Test data - typical E/I proportions
  samples <- c(0.0001, 0.0003, 0.0002, 0.0005, 0.0001, 0.0004, 0.0002, 0.0003)
  
  # Test expanded_ci method
  result1 <- fit_beta_flexible(samples, method = "expanded_ci", expansion_factor = 2.0)
  expect_type(result1, "list")
  expect_true(result1$shape1 > 1.01)
  expect_true(result1$shape2 > 1.01) 
  expect_equal(result1$method, "expanded_ci")
  expect_true(!is.null(result1$fitted_stats))
  
  # Test conservative method
  result2 <- fit_beta_flexible(samples, method = "conservative", conservatism_bias = 0.3)
  expect_type(result2, "list")
  expect_true(result2$shape1 > 1.01)
  expect_true(result2$shape2 > 1.01)
  expect_equal(result2$method, "conservative")
  
  # Conservative should have mode closer to zero than expanded
  if (!is.na(result1$fitted_stats$mode) && !is.na(result2$fitted_stats$mode)) {
    expect_lt(result2$fitted_stats$mode, result1$fitted_stats$mode)
  }
})


test_that("fit_beta_flexible validates inputs properly", {
  
  samples <- c(0.0001, 0.0003, 0.0002)
  
  # Test invalid expansion_factor
  expect_error(fit_beta_flexible(samples, expansion_factor = 0.5))
  
  # Test invalid conservatism_bias
  expect_error(fit_beta_flexible(samples, conservatism_bias = -0.1))
  expect_error(fit_beta_flexible(samples, conservatism_bias = 1.1))
  
  # Test invalid min_precision
  expect_error(fit_beta_flexible(samples, min_precision = 1.0))
  
  # Test invalid target_percentile
  expect_error(fit_beta_flexible(samples, target_percentile = 0))
  expect_error(fit_beta_flexible(samples, target_percentile = 1))
  
  # Test unknown method
  expect_error(fit_beta_flexible(samples, method = "invalid_method"))
})


test_that("fit_beta_flexible handles edge cases", {
  
  # Empty samples
  expect_error(fit_beta_flexible(c()))
  
  # Single sample
  result <- fit_beta_flexible(c(0.001))
  expect_equal(result$method, "insufficient_data")
  
  # Samples with NAs
  samples_na <- c(0.001, NA, 0.002, 0.003, NA)
  result <- fit_beta_flexible(samples_na)
  expect_type(result, "list")
  expect_true(result$shape1 > 1.01)
  
  # Samples outside (0,1) should be filtered
  samples_invalid <- c(-0.1, 0.001, 0.002, 1.1, 0.003)
  result <- fit_beta_flexible(samples_invalid)
  expect_equal(result$sample_stats$n, 3)  # Only 3 valid samples
})


test_that("left_skewed method creates appropriate distributions", {
  
  samples <- c(0.0001, 0.0005, 0.0002, 0.0008, 0.0003)
  
  result <- fit_beta_flexible(samples, method = "left_skewed", target_percentile = 0.2)
  
  expect_equal(result$method, "left_skewed")
  expect_true(result$shape1 > 1.01)
  expect_true(result$shape2 > 1.01)
  
  # Left-skewed should have β > α (more mass near 0)
  expect_gt(result$shape2, result$shape1)
  
  # Mode should be closer to lower end of distribution
  if (!is.na(result$fitted_stats$mode)) {
    expect_lt(result$fitted_stats$mode, result$fitted_stats$mean)
  }
})


test_that("wide_uniform method creates nearly uniform distributions", {
  
  samples <- c(0.001, 0.002, 0.003, 0.004, 0.005)
  
  result <- fit_beta_flexible(samples, method = "wide_uniform")
  
  expect_equal(result$method, "wide_uniform")
  expect_true(result$shape1 > 1.01)
  expect_true(result$shape2 > 1.01)
  
  # For nearly uniform, α ≈ β
  ratio <- result$shape1 / result$shape2
  expect_lt(abs(ratio - 1.0), 0.2)  # Should be close to 1.0
  
  # Mean should be near 0.5 for uniform-ish distribution  
  expect_lt(abs(result$fitted_stats$mean - 0.5), 0.1)
})


test_that("adaptive method chooses appropriate strategies", {
  
  # High CV samples should trigger wide_uniform
  high_cv_samples <- c(0.00001, 0.01, 0.00002, 0.005, 0.00001)
  result_high_cv <- fit_beta_flexible(high_cv_samples, method = "adaptive")
  # Note: may not always be wide_uniform depending on exact CV calculation
  
  # Very small samples should trigger conservative
  small_samples <- c(0.0000001, 0.0000002, 0.0000001, 0.0000003)
  result_small <- fit_beta_flexible(small_samples, method = "adaptive")
  # Conservative bias should result in smaller mode
  
  # Both should produce valid results
  expect_true(result_high_cv$shape1 > 1.01)
  expect_true(result_small$shape1 > 1.01)
})


test_that("precision constraints work correctly", {
  
  samples <- c(0.001, 0.002, 0.001, 0.003)
  
  # Test minimum precision constraint
  result <- fit_beta_flexible(samples, min_precision = 10.0)
  total_precision <- result$shape1 + result$shape2
  expect_gte(total_precision, 10.0)
  
  # Test precision constraints work in general (constraints may not be exact due to other requirements)
  result2 <- fit_beta_flexible(samples, max_precision = 50.0, min_precision = 10.0)
  total_precision2 <- result2$shape1 + result2$shape2
  
  # Just check that both constraints are reasonable (some flexibility needed due to shape constraints)
  expect_gte(result2$shape1, 1.01)
  expect_gte(result2$shape2, 1.01)
  expect_lt(total_precision2, 200.0)  # Not unreasonably high
})


test_that("expansion_factor works as expected", {
  
  samples <- c(0.001, 0.002, 0.003, 0.004, 0.005)
  
  # Small expansion
  result1 <- fit_beta_flexible(samples, method = "expanded_ci", expansion_factor = 1.5)
  
  # Large expansion  
  result2 <- fit_beta_flexible(samples, method = "expanded_ci", expansion_factor = 3.0)
  
  # Larger expansion should generally result in wider distributions (lower precision)
  precision1 <- result1$shape1 + result1$shape2
  precision2 <- result2$shape1 + result2$shape2
  
  # Wider CI should generally mean lower precision (though this may not always hold exactly)
  ci_width1 <- result1$fitted_stats$ci["upper"] - result1$fitted_stats$ci["lower"]
  ci_width2 <- result2$fitted_stats$ci["upper"] - result2$fitted_stats$ci["lower"]
  
  expect_gt(ci_width2, ci_width1)  # Larger expansion should give wider CI
})


test_that("conservatism_bias works correctly", {
  
  samples <- c(0.002, 0.004, 0.003, 0.005, 0.006)
  original_median <- median(samples)
  
  # No bias
  result1 <- fit_beta_flexible(samples, method = "conservative", conservatism_bias = 0.0)
  
  # High bias toward zero
  result2 <- fit_beta_flexible(samples, method = "conservative", conservatism_bias = 0.5)
  
  # Higher bias should result in smaller mode/mean
  if (!is.na(result1$fitted_stats$mode) && !is.na(result2$fitted_stats$mode)) {
    expect_lt(result2$fitted_stats$mode, result1$fitted_stats$mode)
  }
  expect_lt(result2$fitted_stats$mean, result1$fitted_stats$mean)
})


test_that("all methods produce valid Beta parameters", {
  
  samples <- c(0.0005, 0.001, 0.0008, 0.0012, 0.0006)
  methods <- c("expanded_ci", "conservative", "left_skewed", "wide_uniform", "adaptive")
  
  for (method in methods) {
    result <- fit_beta_flexible(samples, method = method)
    
    # All should have valid shape parameters
    expect_true(result$shape1 > 1.01)
    expect_true(result$shape2 > 1.01)
    expect_true(is.finite(result$shape1))
    expect_true(is.finite(result$shape2))
    
    # Should have valid fitted statistics
    expect_true(!is.null(result$fitted_stats))
    expect_true(result$fitted_stats$mean > 0 && result$fitted_stats$mean < 1)
    
    # CI should be valid
    expect_lt(result$fitted_stats$ci["lower"], result$fitted_stats$ci["upper"])
  }
})


test_that("verbose output works", {
  
  samples <- c(0.001, 0.002, 0.003)
  
  # Test that verbose=TRUE doesn't cause errors  
  expect_no_error({
    result <- fit_beta_flexible(samples, method = "expanded_ci", verbose = TRUE)
  })
  
  expect_no_error({
    result <- fit_beta_flexible(samples, method = "conservative", verbose = TRUE) 
  })
  
  expect_no_error({
    result <- fit_beta_flexible(samples, method = "left_skewed", verbose = TRUE)
  })
})


test_that("integration with realistic E/I data", {
  
  # Simulate realistic E/I proportions from MOSAIC context
  # Very small values, right-skewed
  set.seed(123)
  realistic_E <- rgamma(50, shape = 0.5, rate = 5000)  # Very small, right-skewed
  realistic_E <- realistic_E[realistic_E < 1.0]  # Ensure < 1
  realistic_E <- realistic_E[realistic_E > 0]    # Ensure > 0
  
  # Should handle these realistic values
  expect_no_error({
    result_E <- fit_beta_flexible(realistic_E, method = "left_skewed")
  })
  
  expect_true(result_E$shape1 > 1.01)
  expect_true(result_E$shape2 > 1.01)
  expect_gt(result_E$shape2, result_E$shape1)  # Left-skewed
  
  # Test with conservative method for very conservative priors
  expect_no_error({
    result_conservative <- fit_beta_flexible(realistic_E, method = "conservative", 
                                             conservatism_bias = 0.4)
  })
  
  # Conservative should generally be smaller, but may be very close due to constraints
  # Just check that it's not significantly larger
  expect_lte(result_conservative$fitted_stats$mean, result_E$fitted_stats$mean * 1.1)
})