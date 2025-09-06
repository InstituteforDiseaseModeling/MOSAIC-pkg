# ============================================================================
# Comprehensive tests for likelihood guardrails functions
# Testing all individual guardrails and their integration
# ============================================================================

# Helper function to create test matrices
create_test_matrices <- function(n_loc = 2, n_time = 52, 
                                obs_cases = 10, est_cases = 10,
                                obs_deaths = 2, est_deaths = 2) {
    list(
        obs_cases = matrix(obs_cases, nrow = n_loc, ncol = n_time),
        est_cases = matrix(est_cases, nrow = n_loc, ncol = n_time),
        obs_deaths = matrix(obs_deaths, nrow = n_loc, ncol = n_time),
        est_deaths = matrix(est_deaths, nrow = n_loc, ncol = n_time)
    )
}

# ============================================================================
# INDIVIDUAL GUARDRAIL FUNCTION TESTS
# ============================================================================

testthat::test_that("check_magnitude_mismatches detects over-prediction", {
    
    # Create data with 15x over-prediction (like ETH example)
    matrices <- create_test_matrices(n_loc = 1, n_time = 10,
                                   obs_cases = 10, est_cases = 150,
                                   obs_deaths = 1, est_deaths = 1)
    
    violations <- MOSAIC:::check_magnitude_mismatches(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        over_ratio = 10, under_ratio = 0.1,
        min_cases = 100, substantial = 1000, zero_thresh = 100
    )
    
    expect_true(length(violations) > 0)
    expect_true(any(grepl("over-prediction", violations)))
    expect_true(any(grepl("15.0x", violations)))  # Should report 15x ratio
})

testthat::test_that("check_magnitude_mismatches detects severe under-prediction", {
    
    # Create data with severe under-prediction
    matrices <- create_test_matrices(n_loc = 1, n_time = 10,
                                   obs_cases = 2000, est_cases = 50,  # 0.025x ratio
                                   obs_deaths = 200, est_deaths = 5)
    
    violations <- MOSAIC:::check_magnitude_mismatches(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        over_ratio = 10, under_ratio = 0.1,
        min_cases = 100, substantial = 1000, zero_thresh = 100
    )
    
    expect_true(length(violations) >= 2)  # Should detect both cases and deaths
    expect_true(any(grepl("severe under-prediction", violations)))
})

testthat::test_that("check_magnitude_mismatches detects missing epidemics", {
    
    # Large observed epidemic but tiny predicted
    matrices <- create_test_matrices(n_loc = 1, n_time = 10,
                                   obs_cases = 5000, est_cases = 10,
                                   obs_deaths = 500, est_deaths = 1)
    
    violations <- MOSAIC:::check_magnitude_mismatches(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        over_ratio = 10, under_ratio = 0.1,
        min_cases = 100, substantial = 1000, zero_thresh = 100
    )
    
    # Should detect severe under-prediction or missing epidemic
    expect_true(any(grepl("Missing.*epidemic|severe under-prediction", violations)))
    expect_true(length(violations) > 0)
})

testthat::test_that("check_magnitude_mismatches detects zero predictions", {
    
    # Substantial observed but zero predicted
    matrices <- create_test_matrices(n_loc = 1, n_time = 10,
                                   obs_cases = 500, est_cases = 0,
                                   obs_deaths = 50, est_deaths = 0)
    
    violations <- MOSAIC:::check_magnitude_mismatches(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        over_ratio = 10, under_ratio = 0.1,
        min_cases = 100, substantial = 1000, zero_thresh = 100
    )
    
    expect_true(any(grepl("Zero.*prediction", violations)))
})

testthat::test_that("check_negative_correlation detects negative correlations", {
    
    # Create negatively correlated time series (like ETH example: -0.09)
    set.seed(123)
    n_time <- 52
    
    # Observed epidemic pattern
    obs_cases <- matrix(0, nrow = 1, ncol = n_time)
    obs_cases[1, 15:35] <- dnorm(15:35, mean = 25, sd = 5) * 1000
    
    # Estimated pattern - opposite timing (negatively correlated)
    est_cases <- matrix(0, nrow = 1, ncol = n_time)
    est_cases[1, 40:50] <- rev(obs_cases[1, 15:25])  # Opposite pattern
    
    obs_deaths <- matrix(1, nrow = 1, ncol = n_time)
    est_deaths <- matrix(1, nrow = 1, ncol = n_time)
    
    violations <- MOSAIC:::check_negative_correlation(
        obs_cases, est_cases, obs_deaths, est_deaths,
        threshold = -0.05
    )
    
    expect_true(length(violations) > 0)
    expect_true(any(grepl("Negative.*correlation", violations)))
})

testthat::test_that("check_nonsensical_values detects negative values", {
    
    # Create matrices with negative estimated values
    matrices <- create_test_matrices(n_loc = 2, n_time = 10,
                                   obs_cases = 10, est_cases = 10,
                                   obs_deaths = 1, est_deaths = 1)
    
    # Add some negative values
    matrices$est_cases[1, 1] <- -5
    matrices$est_deaths[2, 3] <- -2
    
    violations <- MOSAIC:::check_nonsensical_values(
        matrices$est_cases, matrices$est_deaths,
        max_cases = 1e6, max_deaths = 1e5
    )
    
    expect_true(length(violations) >= 2)
    expect_true(any(grepl("Negative estimated cases", violations)))
    expect_true(any(grepl("Negative estimated deaths", violations)))
})

testthat::test_that("check_nonsensical_values detects non-finite values", {
    
    matrices <- create_test_matrices(n_loc = 2, n_time = 10)
    
    # Add non-finite values
    matrices$est_cases[1, 1] <- Inf
    matrices$est_cases[1, 2] <- -Inf
    matrices$est_deaths[2, 1] <- NaN
    
    violations <- MOSAIC:::check_nonsensical_values(
        matrices$est_cases, matrices$est_deaths,
        max_cases = 1e6, max_deaths = 1e5
    )
    
    expect_true(any(grepl("Non-finite", violations)))
})

testthat::test_that("check_nonsensical_values detects extreme values", {
    
    matrices <- create_test_matrices(n_loc = 1, n_time = 10)
    
    # Add extreme values
    matrices$est_cases[1, 1] <- 2e6    # Above max threshold
    matrices$est_deaths[1, 1] <- 2e5   # Above max threshold
    
    violations <- MOSAIC:::check_nonsensical_values(
        matrices$est_cases, matrices$est_deaths,
        max_cases = 1e6, max_deaths = 1e5
    )
    
    expect_true(any(grepl("Extreme estimated cases", violations)))
    expect_true(any(grepl("Extreme estimated deaths", violations)))
})

# ============================================================================
# INTEGRATION TESTS FOR MAIN GUARDRAIL FUNCTION
# ============================================================================

testthat::test_that("check_likelihood_guardrails returns PROCEED for good data", {
    
    # Create reasonable data that should pass all checks
    matrices <- create_test_matrices(n_loc = 2, n_time = 52,
                                   obs_cases = 50, est_cases = 45,
                                   obs_deaths = 5, est_deaths = 6)
    
    result <- MOSAIC::check_likelihood_guardrails(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths
    )
    
    expect_equal(result$status, "PROCEED")
    expect_equal(length(result$violations), 0)
})

testthat::test_that("check_likelihood_guardrails returns FLOOR for ETH-like scenario", {
    
    # Create ETH-like scenario: massive over-prediction + negative correlation
    set.seed(123)
    n_time <- 52
    
    # Simple clear over-prediction scenario
    obs_cases <- matrix(100, nrow = 1, ncol = n_time)  # Total: 5200
    est_cases <- matrix(1500, nrow = 1, ncol = n_time)  # Total: 78000 (15x over)
    
    obs_deaths <- matrix(10, nrow = 1, ncol = n_time)  # Total: 520
    est_deaths <- matrix(800, nrow = 1, ncol = n_time)  # Total: 41600 (80x over)
    
    result <- MOSAIC::check_likelihood_guardrails(
        obs_cases, est_cases, obs_deaths, est_deaths,
        verbose = TRUE
    )
    
    expect_equal(result$status, "FLOOR")
    expect_true(length(result$violations) >= 1)  # Should have violations
    expect_true(any(grepl("over-prediction", result$violations)))
})

testthat::test_that("check_likelihood_guardrails handles mixed violations", {
    
    # Create scenario with multiple types of violations
    matrices <- create_test_matrices(n_loc = 2, n_time = 10)
    
    # Location 1: Over-prediction (need >50x to trigger with new thresholds)
    matrices$obs_cases[1, ] <- 100
    matrices$est_cases[1, ] <- 6000  # 60x over
    
    # Location 2: Negative values (nonsensical)
    matrices$est_cases[2, 1] <- -10
    matrices$est_deaths[2, 1] <- Inf
    
    result <- MOSAIC::check_likelihood_guardrails(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths
    )
    
    expect_equal(result$status, "FLOOR")
    expect_true(length(result$violations) >= 3)
    expect_true(any(grepl("over-prediction", result$violations)))
    expect_true(any(grepl("Negative", result$violations)))
    expect_true(any(grepl("Non-finite", result$violations)))
})

# ============================================================================
# CALC_MODEL_LIKELIHOOD INTEGRATION TESTS
# ============================================================================

testthat::test_that("calc_model_likelihood integration - good data proceeds normally", {
    
    # Create good data
    matrices <- create_test_matrices(n_loc = 2, n_time = 20,
                                   obs_cases = 10, est_cases = 12,
                                   obs_deaths = 1, est_deaths = 1)
    
    # Test with guardrails enabled
    ll_with_guardrails <- MOSAIC::calc_model_likelihood(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        enable_guardrails = TRUE,
        verbose = FALSE
    )
    
    # Test with guardrails disabled  
    ll_without_guardrails <- MOSAIC::calc_model_likelihood(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        enable_guardrails = FALSE,
        verbose = FALSE
    )
    
    # Should get same result (good data passes guardrails)
    expect_equal(ll_with_guardrails, ll_without_guardrails)
    expect_true(is.finite(ll_with_guardrails))
})

testthat::test_that("calc_model_likelihood returns floor likelihood for bad data", {
    
    # Create clearly bad data (massive over-prediction)
    matrices <- create_test_matrices(n_loc = 1, n_time = 20,
                                   obs_cases = 100, est_cases = 15000,  # 150x over cases, total 2000 vs 300000 
                                   obs_deaths = 30, est_deaths = 4000)  # 133x over deaths, total 600 vs 80000
    
    floor_val <- -999999999
    
    ll_result <- MOSAIC::calc_model_likelihood(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        enable_guardrails = TRUE,
        floor_likelihood = floor_val,
        guardrail_verbose = TRUE
    )
    
    expect_equal(ll_result, floor_val)
})

testthat::test_that("guardrail thresholds are customizable", {
    
    # Create data that violates default but not custom thresholds
    matrices <- create_test_matrices(n_loc = 1, n_time = 10,
                                   obs_cases = 100, est_cases = 800,  # 8x over (default: 10x)
                                   obs_deaths = 10, est_deaths = 10)
    
    # Should pass with default threshold (10x)
    result_default <- MOSAIC::check_likelihood_guardrails(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        over_prediction_ratio = 10
    )
    expect_equal(result_default$status, "PROCEED")
    
    # Should fail with stricter threshold (5x)
    result_strict <- MOSAIC::check_likelihood_guardrails(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        over_prediction_ratio = 5
    )
    expect_equal(result_strict$status, "FLOOR")
})

# ============================================================================
# EDGE CASE TESTS
# ============================================================================

testthat::test_that("guardrails handle boundary values correctly", {
    
    # Test exactly at thresholds
    matrices <- create_test_matrices(n_loc = 1, n_time = 10,
                                   obs_cases = 100, est_cases = 1000,  # Exactly 10x
                                   obs_deaths = 10, est_deaths = 10)
    
    # Should trigger (>= threshold) after fixing boundary condition
    result <- MOSAIC::check_likelihood_guardrails(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        over_prediction_ratio = 10
    )
    expect_equal(result$status, "FLOOR")
    
    # Just under threshold should pass
    matrices$est_cases[1, ] <- 999  # Just under 10x
    result2 <- MOSAIC::check_likelihood_guardrails(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths,
        over_prediction_ratio = 10
    )
    expect_equal(result2$status, "PROCEED")
})

testthat::test_that("guardrails handle all-zero data gracefully", {
    
    # All zeros should proceed (no meaningful epidemic to check)
    matrices <- create_test_matrices(n_loc = 2, n_time = 10,
                                   obs_cases = 0, est_cases = 0,
                                   obs_deaths = 0, est_deaths = 0)
    
    result <- MOSAIC::check_likelihood_guardrails(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths
    )
    
    expect_equal(result$status, "PROCEED")
})

testthat::test_that("guardrails handle NA/missing data appropriately", {
    
    matrices <- create_test_matrices(n_loc = 2, n_time = 10)
    
    # Add some missing values
    matrices$obs_cases[1, 1:3] <- NA
    matrices$est_deaths[2, 5:7] <- NA
    
    # Should not crash and should proceed if no clear violations
    result <- MOSAIC::check_likelihood_guardrails(
        matrices$obs_cases, matrices$est_cases,
        matrices$obs_deaths, matrices$est_deaths
    )
    
    expect_true(result$status %in% c("PROCEED", "FLOOR"))
    expect_true(is.list(result))
})

testthat::test_that("correlation calculation handles edge cases safely", {
    
    # Test zero variance (constant) vectors
    obs_const <- matrix(10, nrow = 1, ncol = 10)
    est_const <- matrix(10, nrow = 1, ncol = 10)
    
    cor_result <- MOSAIC:::calc_pearson_correlation_safe(obs_const[1,], est_const[1,])
    expect_true(is.na(cor_result))
    
    # Test insufficient data points
    cor_result2 <- MOSAIC:::calc_pearson_correlation_safe(c(1, 2), c(2, 3))
    expect_true(is.na(cor_result2))
    
    # Test with some NAs
    obs_na <- c(1, 2, 3, NA, 5, 6, 7, 8, 9, 10)
    est_na <- c(2, 3, 4, 5, NA, 7, 8, 9, 10, 11)
    cor_result3 <- MOSAIC:::calc_pearson_correlation_safe(obs_na, est_na)
    expect_true(is.finite(cor_result3) || is.na(cor_result3))
})

# ============================================================================
# PERFORMANCE TESTS
# ============================================================================

testthat::test_that("guardrails provide computational speedup for bad data", {
    
    # Create large matrices with bad data
    n_loc <- 10
    n_time <- 100
    matrices <- create_test_matrices(n_loc = n_loc, n_time = n_time,
                                   obs_cases = 100, est_cases = 5000,  # Clearly bad
                                   obs_deaths = 10, est_deaths = 1000)
    
    # Time with guardrails (should be fast)
    time_with_guardrails <- system.time({
        ll_guardrails <- MOSAIC::calc_model_likelihood(
            matrices$obs_cases, matrices$est_cases,
            matrices$obs_deaths, matrices$est_deaths,
            enable_guardrails = TRUE,
            floor_likelihood = -1e6
        )
    })
    
    # Should return floor value quickly
    expect_equal(ll_guardrails, -1e6)
    expect_true(time_with_guardrails["elapsed"] < 1.0)  # Should be very fast
})