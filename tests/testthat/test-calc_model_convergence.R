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

  # Check structure (includes temperature field)
  expect_named(weights, c("w", "w_tilde", "retained", "B_idx", "delta_max", "temperature"))
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

  # All-zero weights should error (no positive weights)
  expect_error(calc_model_ess(c(0, 0, 0)), "No positive weights")

  # Negative weights filtered out; remaining positive should work
  expect_error(calc_model_ess(c(-1, 0, 0)), "No positive weights")

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

  # Manual calculation using population SD (matching calc_model_cvw implementation)
  wB <- c(0.5, 0.3, 0.2) / sum(c(0.5, 0.3, 0.2))
  m <- mean(wB)
  pop_sd <- sqrt(mean((wB - m)^2))
  expected_cv <- pop_sd / m
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

test_that("calc_model_convergence works with current API", {
  set.seed(42)
  n_sims <- 100
  results_df <- data.frame(
    sim = seq_len(n_sims),
    likelihood = 1500 + rnorm(n_sims, sd = 3)
  )

  # calc_model_convergence now requires PATHS, results (data.frame), output_dir
  tmp_dir <- tempdir()
  output_dir <- file.path(tmp_dir, "convergence_test")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Mock PATHS (not used internally but required as first arg)
  mock_paths <- list(ROOT = tmp_dir)

  result <- calc_model_convergence(
    PATHS = mock_paths,
    results = results_df,
    output_dir = output_dir,
    delta_max = 6,
    ess_min = 50,
    A_min = 0.75,
    cvw_max = 1.0,
    B_min = 2,
    max_w_max = 0.5,
    verbose = FALSE
  )

  # Check return structure
  expect_true(is.list(result))
  expect_true("status" %in% names(result))
  expect_true("n_total" %in% names(result))
  expect_true("n_successful" %in% names(result))
  expect_true("n_retained" %in% names(result))
  expect_true("ESS" %in% names(result))
  expect_true("files_written" %in% names(result))

  # Check values
  expect_equal(result$n_total, n_sims)
  expect_equal(result$n_successful, n_sims)  # All likelihoods are finite
  expect_true(result$status %in% c("PASS", "WARN", "FAIL"))
  expect_true(is.finite(result$ESS))

  # Check output files were written
  expect_true(file.exists(file.path(output_dir, "convergence_results.parquet")))
  expect_true(file.exists(file.path(output_dir, "convergence_diagnostics.json")))

  # Cleanup
  unlink(output_dir, recursive = TRUE)
})

test_that("calc_model_convergence prefers seed_sim over sim for seed column", {
  tmp_dir <- tempdir()
  mock_paths <- list(ROOT = tmp_dir)
  output_dir <- file.path(tmp_dir, "conv_seed_test")

  # When seed_sim is present, output seed column should use it
  results_with_seed_sim <- data.frame(
    sim = 1:10,
    seed_sim = 101:110,
    likelihood = 1500 + rnorm(10, sd = 1)
  )
  result <- calc_model_convergence(
    PATHS = mock_paths,
    results = results_with_seed_sim,
    output_dir = output_dir,
    verbose = FALSE
  )
  conv_data <- arrow::read_parquet(file.path(output_dir, "convergence_results.parquet"))
  expect_equal(conv_data$seed, 101:110)
  expect_equal(conv_data$sim, 1:10)

  # When seed_sim is absent, fall back to sim
  output_dir_2 <- file.path(tmp_dir, "conv_seed_test_2")
  results_no_seed_sim <- data.frame(
    sim = 1:10,
    likelihood = 1500 + rnorm(10, sd = 1)
  )
  result2 <- calc_model_convergence(
    PATHS = mock_paths,
    results = results_no_seed_sim,
    output_dir = output_dir_2,
    verbose = FALSE
  )
  conv_data_2 <- arrow::read_parquet(file.path(output_dir_2, "convergence_results.parquet"))
  expect_equal(conv_data_2$seed, 1:10)

  unlink(c(output_dir, output_dir_2), recursive = TRUE)
})

test_that("calc_model_convergence errors on all non-finite likelihoods", {
  tmp_dir <- tempdir()
  mock_paths <- list(ROOT = tmp_dir)
  output_dir <- file.path(tmp_dir, "conv_nonfinite")

  results_bad <- data.frame(
    sim = 1:5,
    likelihood = rep(-Inf, 5)
  )
  expect_error(
    calc_model_convergence(
      PATHS = mock_paths,
      results = results_bad,
      output_dir = output_dir,
      verbose = FALSE
    ),
    "non-finite"
  )

  unlink(output_dir, recursive = TRUE)
})

test_that("calc_model_convergence status thresholds respect user parameters", {
  tmp_dir <- tempdir()
  mock_paths <- list(ROOT = tmp_dir)
  output_dir <- file.path(tmp_dir, "conv_thresh")

  # Create data that produces moderate ESS (~50-100 range)
  set.seed(99)
  results_df <- data.frame(
    sim = seq_len(200),
    likelihood = 1500 + rnorm(200, sd = 3)
  )

  # With very low ess_min, ESS should pass (not fail due to hard-coded 500)
  result <- calc_model_convergence(
    PATHS = mock_paths,
    results = results_df,
    output_dir = output_dir,
    ess_min = 1,
    A_min = 0.01,
    cvw_max = 100,
    B_min = 1,
    max_w_max = 0.99,
    verbose = FALSE
  )
  # With such permissive targets, everything should pass

  expect_equal(result$status, "PASS")

  unlink(output_dir, recursive = TRUE)
})

test_that("calc_model_convergence with edge cases", {
  tmp_dir <- tempdir()
  mock_paths <- list(ROOT = tmp_dir)

  # All identical log-likelihoods
  output_dir_1 <- file.path(tmp_dir, "conv_edge_1")
  results_identical <- data.frame(
    sim = 1:50,
    likelihood = rep(100, 50)
  )
  result_identical <- calc_model_convergence(
    PATHS = mock_paths,
    results = results_identical,
    output_dir = output_dir_1,
    verbose = FALSE
  )
  expect_true(is.finite(result_identical$ESS))
  expect_equal(result_identical$n_total, 50)

  # Single simulation
  output_dir_2 <- file.path(tmp_dir, "conv_edge_2")
  results_single <- data.frame(
    sim = 1,
    likelihood = 100
  )
  result_single <- calc_model_convergence(
    PATHS = mock_paths,
    results = results_single,
    output_dir = output_dir_2,
    verbose = FALSE
  )
  expect_equal(result_single$n_total, 1)
  expect_true(is.finite(result_single$ESS))

  # Very spread out log-likelihoods (most truncated)
  output_dir_3 <- file.path(tmp_dir, "conv_edge_3")
  results_spread <- data.frame(
    sim = 1:5,
    likelihood = c(100, 90, 80, 70, 60)
  )
  result_spread <- calc_model_convergence(
    PATHS = mock_paths,
    results = results_spread,
    output_dir = output_dir_3,
    delta_max = 5,
    verbose = FALSE
  )
  # Only first few should be retained
  expect_true(result_spread$n_retained < 5)

  # Cleanup
  unlink(c(output_dir_1, output_dir_2, output_dir_3), recursive = TRUE)
})
