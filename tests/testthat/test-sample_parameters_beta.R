library(testthat)
library(MOSAIC)

# Set up test environment
setup({
  set_root_directory("~/MOSAIC")
})

test_that("beta_j0_tot and p_beta are sampled correctly", {
  # Sample parameters with fixed seed for reproducibility
  config <- sample_parameters(seed = 123, verbose = FALSE)
  
  # Check that new parameters exist
  expect_true("beta_j0_tot" %in% names(config))
  expect_true("p_beta" %in% names(config))
  
  # Check that parameters have correct length
  n_locations <- length(config$location_name)
  expect_equal(length(config$beta_j0_tot), n_locations)
  expect_equal(length(config$p_beta), n_locations)
  
  # Check that beta_j0_tot is positive
  expect_true(all(config$beta_j0_tot > 0))
  
  # Check that p_beta is in [0,1]
  expect_true(all(config$p_beta >= 0 & config$p_beta <= 1))
})

test_that("beta_j0_hum and beta_j0_env are derived correctly", {
  # Sample parameters with fixed seed
  config <- sample_parameters(seed = 456, verbose = FALSE)
  
  # Check that derived parameters exist
  expect_true("beta_j0_hum" %in% names(config))
  expect_true("beta_j0_env" %in% names(config))
  
  # Check that derived values match expected calculation
  expected_hum <- config$p_beta * config$beta_j0_tot
  expected_env <- (1 - config$p_beta) * config$beta_j0_tot
  
  expect_equal(config$beta_j0_hum, expected_hum, tolerance = 1e-10)
  expect_equal(config$beta_j0_env, expected_env, tolerance = 1e-10)
  
  # Check that derived values are positive
  expect_true(all(config$beta_j0_hum > 0))
  expect_true(all(config$beta_j0_env > 0))
})

test_that("p_beta prior has expected properties", {
  # Sample many times to test distribution properties
  n_samples <- 1000
  p_beta_samples <- numeric(n_samples)
  
  for (i in 1:n_samples) {
    config <- sample_parameters(seed = i, verbose = FALSE)
    # Take mean across locations for each sample
    p_beta_samples[i] <- mean(config$p_beta)
  }
  
  # Expected mean should be around 2/3 (0.667) based on Beta(2.25, 1.125)
  # shape1 / (shape1 + shape2) = 2.25 / (2.25 + 1.125) = 0.667
  expect_equal(mean(p_beta_samples), 2/3, tolerance = 0.05)
  
  # Check that values are in valid range
  expect_true(all(p_beta_samples >= 0 & p_beta_samples <= 1))
})

test_that("beta_j0_tot follows lognormal distribution", {
  # Sample many times to test distribution properties
  n_samples <- 1000
  beta_tot_samples <- numeric(n_samples)
  
  for (i in 1:n_samples) {
    config <- sample_parameters(seed = i + 1000, verbose = FALSE)
    # Take geometric mean across locations for each sample
    beta_tot_samples[i] <- exp(mean(log(config$beta_j0_tot)))
  }
  
  # Expected median should be around 1.5e-4 based on Lognormal(log(1.5e-4), 0.9)
  # Median of lognormal = exp(meanlog) = exp(log(1.5e-4)) = 1.5e-4
  expect_equal(median(beta_tot_samples), 1.5e-4, tolerance = 1.5e-4 * 0.5)
  
  # Check that all values are positive
  expect_true(all(beta_tot_samples > 0))
})

test_that("derived beta values maintain expected ratio", {
  # Sample parameters
  config <- sample_parameters(seed = 789, verbose = FALSE)
  
  # Calculate ratio of human to environmental transmission
  ratio <- config$beta_j0_hum / config$beta_j0_env
  
  # The ratio should equal p_beta / (1 - p_beta)
  expected_ratio <- config$p_beta / (1 - config$p_beta)
  
  expect_equal(ratio, expected_ratio, tolerance = 1e-10)
  
  # On average, human transmission should be about 2x environmental
  # since p_beta has mean 2/3, so ratio should have mean around 2
  mean_ratio <- mean(ratio[is.finite(ratio)])
  expect_true(mean_ratio > 1)  # Human > Environmental on average
})

test_that("config structure maintains laser-cholera compatibility", {
  # Sample parameters
  config <- sample_parameters(seed = 321, verbose = FALSE)
  
  # Check that the config has the required structure for laser-cholera
  expect_true("beta_j0_hum" %in% names(config))
  expect_true("beta_j0_env" %in% names(config))
  
  # Check that these are numeric vectors of the correct length
  n_locations <- length(config$location_name)
  expect_equal(length(config$beta_j0_hum), n_locations)
  expect_equal(length(config$beta_j0_env), n_locations)
  expect_type(config$beta_j0_hum, "double")
  expect_type(config$beta_j0_env, "double")
  
  # Check that values are in reasonable range for transmission rates
  expect_true(all(config$beta_j0_hum > 1e-8 & config$beta_j0_hum < 1e-2))
  expect_true(all(config$beta_j0_env > 1e-8 & config$beta_j0_env < 1e-2))
})

test_that("validation catches invalid beta values", {
  # Create a config with invalid values
  config <- sample_parameters(seed = 654, verbose = FALSE)
  
  # Test validation with valid config
  expect_true(validate_sampled_config(config, verbose = FALSE))
  
  # Make beta_j0_tot negative
  config_bad <- config
  config_bad$beta_j0_tot[1] <- -1
  expect_false(validate_sampled_config(config_bad, verbose = FALSE))
  
  # Make p_beta out of range
  config_bad <- config
  config_bad$p_beta[1] <- 1.5
  expect_false(validate_sampled_config(config_bad, verbose = FALSE))
  
  # Make derived beta_j0_hum negative (shouldn't happen naturally)
  config_bad <- config
  config_bad$beta_j0_hum[1] <- -0.001
  expect_false(validate_sampled_config(config_bad, verbose = FALSE))
})

test_that("transmission-only sampling pattern works", {
  # Use the helper function to sample only transmission parameters
  args <- create_sampling_args("transmission_only", seed = 987)
  config <- do.call(sample_parameters, args)
  
  # Check that transmission parameters were sampled
  expect_true("beta_j0_tot" %in% names(config))
  expect_true("p_beta" %in% names(config))
  expect_true("beta_j0_hum" %in% names(config))
  expect_true("beta_j0_env" %in% names(config))
  
  # Verify the derivation still works
  expected_hum <- config$p_beta * config$beta_j0_tot
  expected_env <- (1 - config$p_beta) * config$beta_j0_tot
  
  expect_equal(config$beta_j0_hum, expected_hum, tolerance = 1e-10)
  expect_equal(config$beta_j0_env, expected_env, tolerance = 1e-10)
})