# Unit tests for calc_model_posterior_distributions function

test_that("calc_model_posterior_distributions basic functionality", {
  
  # Create test data
  set.seed(123)
  results <- data.frame(
    sim = 1:100,
    iter = rep(1, 100),
    likelihood = rnorm(100, -1000, 50),
    phi_1 = runif(100, 0, 1),
    phi_2 = runif(100, 0, 1),
    beta_env = rgamma(100, 2, 1)
  )
  
  # Test with columns: 1=sim, 2=iter, 3=likelihood, 4-6=parameters
  posterior <- calc_model_posterior_distributions(
    results = results,
    col_ll = 3,
    col_params = 4:6,
    probs = c(0.025, 0.5, 0.975)
  )
  
  # Check return structure
  expect_type(posterior, "list")
  expect_named(posterior, c("summary_table", "posterior_weights"))
  
  # Check summary table
  expect_s3_class(posterior$summary_table, "data.frame")
  expect_equal(nrow(posterior$summary_table), 3)  # 3 parameters
  expect_true("parameter" %in% names(posterior$summary_table))
  expect_true("mean" %in% names(posterior$summary_table))
  expect_true("sd" %in% names(posterior$summary_table))
  expect_true("mode" %in% names(posterior$summary_table))
  expect_true("effective_n" %in% names(posterior$summary_table))
  expect_true("q2.5" %in% names(posterior$summary_table))
  expect_true("q50" %in% names(posterior$summary_table))
  expect_true("q97.5" %in% names(posterior$summary_table))
  
  # Check posterior weights
  expect_type(posterior$posterior_weights, "double")
  expect_length(posterior$posterior_weights, nrow(results))
  expect_true(all(posterior$posterior_weights >= 0))
  expect_equal(sum(posterior$posterior_weights), 1, tolerance = 1e-10)
  
  # Check parameter names
  expect_equal(posterior$summary_table$parameter, c("phi_1", "phi_2", "beta_env"))
  
  # Check that all statistics are finite for valid data
  expect_true(all(is.finite(posterior$summary_table$mean)))
  expect_true(all(is.finite(posterior$summary_table$sd)))
  expect_true(all(posterior$summary_table$effective_n > 0))
})

test_that("calc_model_posterior_distributions handles missing values", {
  
  # Create data with some NA values
  results <- data.frame(
    sim = 1:20,
    iter = rep(1, 20),
    likelihood = c(rep(-1000, 15), rep(NA, 5)),  # 5 NA likelihoods
    param1 = c(runif(10), rep(NA, 5), runif(5))  # 5 NA parameters
  )
  
  posterior <- calc_model_posterior_distributions(
    results = results,
    col_ll = 3,
    col_params = 4,
    probs = c(0.25, 0.75)
  )
  
  # Should handle NAs gracefully
  expect_s3_class(posterior$summary_table, "data.frame")
  expect_equal(nrow(posterior$summary_table), 1)
  
  # Check that weights exclude NA likelihoods
  expect_length(posterior$posterior_weights, 20)
  expect_equal(sum(posterior$posterior_weights[1:15]), 1, tolerance = 1e-10)
  expect_true(all(is.na(posterior$posterior_weights[16:20])))
})

test_that("calc_model_posterior_distributions input validation", {
  
  results <- data.frame(likelihood = -1000, param1 = 0.5)
  
  # Invalid column indices
  expect_error(
    calc_model_posterior_distributions(results, 5, 2),
    "col_ll.*<= ncol"
  )
  
  expect_error(
    calc_model_posterior_distributions(results, 1, 5),
    "col_params.*<= ncol"
  )
  
  # Invalid probability values
  expect_error(
    calc_model_posterior_distributions(results, 1, 2, probs = c(-0.1, 0.5)),
    "probs.*>= 0"
  )
  
  expect_error(
    calc_model_posterior_distributions(results, 1, 2, probs = c(0.5, 1.1)),
    "probs.*<= 1"
  )
  
  # Non-data.frame input
  expect_error(
    calc_model_posterior_distributions("not_a_df", 1, 2),
    "is.data.frame"
  )
})

test_that("calc_model_posterior_distributions with all invalid likelihoods", {
  
  results <- data.frame(
    likelihood = c(NA, -Inf, NaN),
    param1 = c(1, 2, 3)
  )
  
  expect_error(
    calc_model_posterior_distributions(results, 1, 2),
    "No valid likelihood values"
  )
})