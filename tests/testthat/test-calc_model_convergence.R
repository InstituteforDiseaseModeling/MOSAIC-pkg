test_that("calc_model_aic_delta works correctly", {
  # Simple test case
  loglik <- c(100, 98, 95, 102)
  delta <- calc_model_aic_delta(loglik)
  
  # Should be -2 * (loglik - max(loglik))
  expected <- -2 * (loglik - 102)
  expect_equal(delta, expected)
  
  # Best model should have delta = 0
  expect_equal(delta[4], 0)
  
  # Test with single value
  single_ll <- 50
  single_delta <- calc_model_aic_delta(single_ll)
  expect_equal(single_delta, 0)
  
  # Test error handling
  expect_error(calc_model_aic_delta("not_numeric"))
})

test_that("calc_model_akaike_weights works correctly", {
  delta <- c(0, 2, 4, 6, 8)
  weights <- calc_model_akaike_weights(delta, delta_max = 6)
  
  # Check structure
  expect_named(weights, c("w", "w_tilde", "retained", "B_idx", "delta_max"))
  expect_equal(length(weights$w), 5)
  expect_equal(length(weights$w_tilde), 5)
  expect_equal(length(weights$retained), 5)
  
  # Weights should be exp(-0.5 * delta) for delta <= 6
  expected_w <- exp(-0.5 * delta)
  expected_w[delta > 6] <- 0
  expect_equal(weights$w, expected_w)
  
  # Last weight should be 0 (delta = 8 > 6)
  expect_equal(weights$w[5], 0)
  
  # Normalized weights should sum to 1 (for retained)
  expect_equal(sum(weights$w_tilde), 1, tolerance = 1e-10)
  
  # Test retained logic
  expect_equal(weights$retained, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(weights$B_idx, 1:4)
  
  # Test all weights truncated
  all_high_delta <- rep(10, 3)
  suppressWarnings(weights_all_zero <- calc_model_akaike_weights(all_high_delta, delta_max = 6))
  expect_equal(sum(weights_all_zero$w), 0)
  expect_equal(length(weights_all_zero$B_idx), 0)
})

test_that("calc_model_ess works correctly", {
  # Equal weights case
  w_equal <- rep(1/4, 4)
  ess_equal <- calc_model_ess(w_equal)
  expect_equal(ess_equal, 4)
  
  # Single non-zero weight
  w_single <- c(1, 0, 0, 0)
  ess_single <- calc_model_ess(w_single)
  expect_equal(ess_single, 1)
  
  # Test with unnormalized weights
  w_unnorm <- c(2, 2, 2, 2)
  ess_unnorm <- calc_model_ess(w_unnorm)
  expect_equal(ess_unnorm, 4)
  
  # Test edge cases
  expect_true(is.na(calc_model_ess(c(0, 0, 0))))
  expect_true(is.na(calc_model_ess(c(-1, 1))))
  
  # Test error handling
  expect_error(calc_model_ess("not_numeric"))
})

test_that("calc_model_agreement_index works correctly", {
  # Equal weights case
  w_equal <- rep(1, 4)
  ag_equal <- calc_model_agreement_index(w_equal)
  expect_named(ag_equal, c("A", "H", "B_size"))
  expect_equal(ag_equal$A, 1) # Maximum agreement
  expect_equal(ag_equal$B_size, 4)
  expect_equal(ag_equal$H, log(4))
  
  # Single weight
  w_single <- c(1, 0, 0, 0)
  ag_single <- calc_model_agreement_index(w_single)
  expect_equal(ag_single$A, 0)
  expect_equal(ag_single$B_size, 1)
  expect_equal(ag_single$H, 0)
  
  # No positive weights
  w_none <- rep(0, 3)
  ag_none <- calc_model_agreement_index(w_none)
  expect_equal(ag_none$A, 0)
  expect_equal(ag_none$B_size, 0)
  
  # Test with unequal weights
  w_unequal <- c(0.8, 0.1, 0.1, 0)
  ag_unequal <- calc_model_agreement_index(w_unequal)
  expect_true(ag_unequal$A > 0 && ag_unequal$A < 1)
  expect_equal(ag_unequal$B_size, 3)
  
  # Test error handling
  expect_error(calc_model_agreement_index("not_numeric"))
})

test_that("calc_model_cvw works correctly", {
  # Equal weights
  w_equal <- rep(1, 4)
  cv_equal <- calc_model_cvw(w_equal)
  expect_equal(cv_equal, 0) # No variation
  
  # Single weight
  w_single <- c(1, 0, 0)
  cv_single <- calc_model_cvw(w_single)
  expect_true(is.na(cv_single))
  
  # No positive weights
  w_none <- rep(0, 3)
  cv_none <- calc_model_cvw(w_none)
  expect_true(is.na(cv_none))
  
  # Test with variation
  w_varied <- c(0.5, 0.3, 0.2, 0)
  cv_varied <- calc_model_cvw(w_varied)
  expect_true(cv_varied > 0)
  
  # Manual calculation check
  wB <- c(0.5, 0.3, 0.2) / sum(c(0.5, 0.3, 0.2))
  expected_cv <- sd(wB) / mean(wB)
  expect_equal(cv_varied, expected_cv)
  
  # Test error handling
  expect_error(calc_model_cvw("not_numeric"))
})

test_that("calc_model_max_weight works correctly", {
  # Equal weights
  w_equal <- rep(1, 4)
  max_w_equal <- calc_model_max_weight(w_equal)
  expect_equal(max_w_equal, 0.25) # Each weight = 1/4
  
  # Single weight
  w_single <- c(1, 0, 0)
  max_w_single <- calc_model_max_weight(w_single)
  expect_equal(max_w_single, 1.0)
  
  # No positive weights
  w_none <- rep(0, 3)
  max_w_none <- calc_model_max_weight(w_none)
  expect_true(is.na(max_w_none))
  
  # Test with variation
  w_varied <- c(0.6, 0.3, 0.1, 0)
  max_w_varied <- calc_model_max_weight(w_varied)
  # Normalized within retained set: 0.6/1.0 = 0.6
  expect_equal(max_w_varied, 0.6)
  
  # Test error handling
  expect_error(calc_model_max_weight("not_numeric"))
})

test_that("calc_model_convergence integration test", {
  set.seed(42)
  loglik <- 1500 + rnorm(100, sd = 3)
  
  result <- calc_model_convergence(loglik, 
                                   delta_max = 6, 
                                   ess_min = 50, 
                                   A_min = 0.75, 
                                   cvw_max = 1.0,
                                   B_min = 2,
                                   max_w_max = 0.5)
  
  # Check structure
  expect_named(result, c("delta", "weights", "metrics", "targets", "pass"))
  expect_named(result$metrics, c("ESS", "A", "CVw", "B_size", "max_w"))
  expect_named(result$targets, c("ESS_min", "A_min", "CVw_max", "B_min", "max_w_max"))
  expect_named(result$pass, c("ESS", "A", "CVw", "B_size", "max_w"))
  
  # Check lengths
  expect_equal(length(result$delta), 100)
  expect_equal(length(result$weights$w), 100)
  
  # Check that delta calculation is correct
  expected_delta <- calc_model_aic_delta(loglik)
  expect_equal(result$delta, expected_delta)
  
  # Check that individual metrics match
  expected_ess <- calc_model_ess(result$weights$w_tilde)
  expect_equal(as.numeric(result$metrics["ESS"]), expected_ess)
  
  expected_ag <- calc_model_agreement_index(result$weights$w)
  expect_equal(as.numeric(result$metrics["A"]), expected_ag$A)
  expect_equal(as.numeric(result$metrics["B_size"]), expected_ag$B_size)
  
  expected_cvw <- calc_model_cvw(result$weights$w)
  expect_equal(as.numeric(result$metrics["CVw"]), expected_cvw)
  
  expected_max_w <- calc_model_max_weight(result$weights$w)
  expect_equal(as.numeric(result$metrics["max_w"]), expected_max_w)
  
  # Check targets
  expect_equal(as.numeric(result$targets["ESS_min"]), 50)
  expect_equal(as.numeric(result$targets["A_min"]), 0.75)
  expect_equal(as.numeric(result$targets["CVw_max"]), 1.0)
  expect_equal(as.numeric(result$targets["B_min"]), 2)
  expect_equal(as.numeric(result$targets["max_w_max"]), 0.5)
  
  # Check pass logic
  expect_equal(as.logical(result$pass["ESS"]), as.numeric(result$metrics["ESS"]) >= 50)
  expect_equal(as.logical(result$pass["A"]), as.numeric(result$metrics["A"]) >= 0.75)
  expect_equal(as.logical(result$pass["CVw"]), as.numeric(result$metrics["CVw"]) <= 1.0)
  expect_equal(as.logical(result$pass["B_size"]), as.numeric(result$metrics["B_size"]) >= 2)
  expect_equal(as.logical(result$pass["max_w"]), as.numeric(result$metrics["max_w"]) <= 0.5)
  
  # Test error handling
  expect_error(calc_model_convergence("not_numeric"))
})

test_that("calc_model_convergence with edge cases", {
  # All identical log-likelihoods
  loglik_identical <- rep(100, 50)
  result_identical <- calc_model_convergence(loglik_identical)
  expect_equal(all(result_identical$delta == 0), TRUE)
  expect_equal(as.numeric(result_identical$metrics["ESS"]), 50)
  expect_equal(as.numeric(result_identical$metrics["A"]), 1)
  expect_equal(as.numeric(result_identical$metrics["CVw"]), 0)
  
  # Single log-likelihood
  loglik_single <- 100
  result_single <- calc_model_convergence(loglik_single)
  expect_equal(result_single$delta, 0)
  expect_equal(as.numeric(result_single$metrics["ESS"]), 1)
  expect_equal(as.numeric(result_single$metrics["A"]), 0)
  expect_true(is.na(result_single$metrics["CVw"]))
  
  # Very spread out log-likelihoods (most truncated)
  loglik_spread <- c(100, 90, 80, 70, 60)
  result_spread <- calc_model_convergence(loglik_spread, delta_max = 5)
  # Only first few should be retained
  expect_true(sum(result_spread$weights$retained) < 5)
})