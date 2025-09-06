# Unit tests for calc_model_collapse_iterations function

test_that("calc_model_collapse_iterations handles data.frame input", {
  
  # Create test data.frame
  df <- data.frame(
    sim = c(1, 1, 1, 2, 2, 3, 3, 3, 3),
    iter = c(1, 2, 3, 1, 2, 1, 2, 3, 4),
    likelihood = c(-100, -101, -99, -200, -199, -150, -151, -149, -148),
    alpha = c(0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3),
    stringsAsFactors = FALSE
  )
  
  # Suppress messages for testing
  # Columns: 1=sim, 2=iter, 3=likelihood, 4=alpha
  suppressMessages({
    result <- calc_model_collapse_iterations(df, 3, 1, 2)
  })
  
  # Check basic properties
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)  # 3 unique simulations
  expect_equal(ncol(result), 4)  # Same number of columns
  expect_equal(names(result), names(df))
  
  # Check that iter column contains total iterations per sim
  expect_equal(result$iter[result$sim == 1], 3)
  expect_equal(result$iter[result$sim == 2], 2) 
  expect_equal(result$iter[result$sim == 3], 4)
  
  # Check that other columns preserved from first iteration
  expect_equal(result$alpha[result$sim == 1], 0.1)
  expect_equal(result$alpha[result$sim == 2], 0.2)
  expect_equal(result$alpha[result$sim == 3], 0.3)
  
  # Check likelihood values are reasonable (between min and max of original)
  sim1_ll <- result$likelihood[result$sim == 1]
  expect_true(sim1_ll >= -101 & sim1_ll <= -99)
  
})

test_that("calc_model_collapse_iterations handles matrix input", {
  
  # Create test matrix
  mat <- matrix(c(
    1, 1, -100, 0.1,
    1, 2, -101, 0.1,
    1, 3, -99,  0.1,
    2, 1, -200, 0.2,
    2, 2, -199, 0.2
  ), ncol = 4, byrow = TRUE)
  colnames(mat) <- c("sim", "iter", "likelihood", "alpha")
  
  # Suppress messages for testing
  # Columns: 1=sim, 2=iter, 3=likelihood, 4=alpha
  suppressMessages({
    result <- calc_model_collapse_iterations(mat, 3, 1, 2)
  })
  
  # Check basic properties
  expect_true(is.matrix(result))
  expect_equal(nrow(result), 2)  # 2 unique simulations
  expect_equal(ncol(result), 4)  # Same number of columns
  expect_equal(colnames(result), colnames(mat))
  
  # Check iteration counts
  expect_equal(as.numeric(result[result[,"sim"] == 1, "iter"]), 3)
  expect_equal(as.numeric(result[result[,"sim"] == 2, "iter"]), 2)
  
})

test_that("calc_model_collapse_iterations handles single iteration per sim", {
  
  # Data with no repeated simulations
  df <- data.frame(
    sim = c(1, 2, 3),
    iter = c(1, 1, 1),
    likelihood = c(-100, -200, -150)
  )
  
  # Should return unchanged
  # Columns: 1=sim, 2=iter, 3=likelihood
  suppressMessages({
    result <- calc_model_collapse_iterations(df, 3, 1, 2)
  })
  
  expect_equal(result, df)
  
})

test_that("calc_model_collapse_iterations error handling", {
  
  df <- data.frame(sim = 1, iter = 1, likelihood = -100)
  
  # Invalid column indices
  expect_error(
    calc_model_collapse_iterations(df, 5, 1, 2),  # col_ll out of bounds
    "col_ll.*col_sim.*col_iter.*<= ncol"
  )
  
  expect_error(
    calc_model_collapse_iterations(df, 3, 5, 2),  # col_sim out of bounds
    "col_ll.*col_sim.*col_iter.*<= ncol"
  )
  
  expect_error(
    calc_model_collapse_iterations(df, 3, 1, 5),  # col_iter out of bounds
    "col_ll.*col_sim.*col_iter.*<= ncol"
  )
  
  # Non-numeric likelihood
  df_bad <- data.frame(sim = 1, iter = 1, likelihood = "bad")
  expect_error(
    calc_model_collapse_iterations(df_bad, 3, 1, 2),
    "`col_ll` column must be numeric"
  )
  
  # Invalid input type
  expect_error(
    calc_model_collapse_iterations("not_a_dataframe", 3, 1, 2),
    "is.data.frame\\(data\\) \\|\\| is.matrix\\(data\\) is not TRUE"
  )
  
})

test_that("calc_model_collapse_iterations preserves column order", {
  
  # Test with different column orders
  df <- data.frame(
    alpha = c(0.1, 0.1, 0.2, 0.2),
    sim = c(1, 1, 2, 2),
    beta = c(0.5, 0.5, 0.6, 0.6),
    iter = c(1, 2, 1, 2),
    likelihood = c(-100, -101, -200, -199),
    gamma = c(0.9, 0.9, 0.8, 0.8)
  )
  
  # Columns: 1=alpha, 2=sim, 3=beta, 4=iter, 5=likelihood, 6=gamma
  suppressMessages({
    result <- calc_model_collapse_iterations(df, 5, 2, 4)
  })
  
  expect_equal(names(result), names(df))
  
})

test_that("calc_model_collapse_iterations log-mean-exp calculation", {
  
  # Test with known values where we can verify the calculation
  df <- data.frame(
    sim = c(1, 1, 1),
    iter = c(1, 2, 3),
    likelihood = c(-1, -2, -3)  # Small values for exact comparison
  )
  
  # Columns: 1=sim, 2=iter, 3=likelihood
  suppressMessages({
    result <- calc_model_collapse_iterations(df, 3, 1, 2)
  })
  
  # Manual calculation
  expected_ll <- calc_log_mean_exp(c(-1, -2, -3))
  expect_equal(as.numeric(result$likelihood), expected_ll, tolerance = 1e-10)
  
})

test_that("calc_model_collapse_iterations handles missing likelihood values", {
  
  # Test with NA values in likelihood
  df <- data.frame(
    sim = c(1, 1, 1, 2, 2),
    iter = c(1, 2, 3, 1, 2),
    likelihood = c(-100, NA, -102, -200, -201)
  )
  
  # Columns: 1=sim, 2=iter, 3=likelihood
  suppressMessages({
    result <- calc_model_collapse_iterations(df, 3, 1, 2)
  })
  
  # Should handle NA values gracefully
  expect_equal(nrow(result), 2)
  sim1_ll <- result$likelihood[result$sim == 1]
  expect_true(is.finite(sim1_ll))  # Should use finite values only
  
})

test_that("calc_model_collapse_iterations verbose output", {
  
  df <- data.frame(
    sim = c(1, 1, 2, 2, 3, 3, 4, 4),
    iter = c(1, 2, 1, 2, 1, 2, 1, 2),
    likelihood = c(-100, -101, -200, -199, -150, -151, -300, -299)
  )
  
  # Test that verbose = TRUE produces messages without error
  # Columns: 1=sim, 2=iter, 3=likelihood
  expect_message(
    calc_model_collapse_iterations(df, 3, 1, 2, verbose = TRUE),
    "Sim.*iterations.*→"
  )
  
})

test_that("calc_model_collapse_iterations realistic calibration scenario", {
  
  # Simulate realistic calibration data
  set.seed(123)
  n_sim <- 50
  n_iter <- 3
  
  sim_data <- data.frame(
    sim = rep(1:n_sim, each = n_iter),
    iter = rep(1:n_iter, times = n_sim),
    likelihood = rep(runif(n_sim, -10000, -1000), each = n_iter) + 
                 rnorm(n_sim * n_iter, 0, 10),  # Add some iteration variation
    param1 = rep(runif(n_sim, 0, 1), each = n_iter),
    param2 = rep(runif(n_sim, 0, 1), each = n_iter)
  )
  
  # Columns: 1=sim, 2=iter, 3=likelihood, 4=param1, 5=param2
  suppressMessages({
    result <- calc_model_collapse_iterations(sim_data, 3, 1, 2)
  })
  
  # Verify results
  expect_equal(nrow(result), n_sim)
  expect_true(all(result$iter == n_iter))  # All should show 3 iterations
  expect_true(all(is.finite(result$likelihood)))
  
  # Check that collapsed likelihoods are reasonable
  # (should be close to but not exactly the same as original means per sim)
  original_means <- aggregate(likelihood ~ sim, sim_data, mean)
  merged <- merge(result, original_means, by = "sim", suffixes = c("_lme", "_mean"))
  
  # LME should be different from arithmetic mean (bias reduction)
  expect_true(any(abs(merged$likelihood_lme - merged$likelihood_mean) > 0.001))
  
})