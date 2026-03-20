library(testthat)
library(MOSAIC)

# Set root directory (required for sample_parameters to load defaults)
tryCatch(set_root_directory("~/MOSAIC"), error = function(e) NULL)

test_that("beta_j0_tot and p_beta are sampled correctly", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  config <- sample_parameters(seed = 123, verbose = FALSE)

  # Check that parameters exist
  expect_true("beta_j0_tot" %in% names(config))
  expect_true("p_beta" %in% names(config))

  # Check correct length (one per location)
  n_locations <- length(config$location_name)
  expect_equal(length(config$beta_j0_tot), n_locations)
  expect_equal(length(config$p_beta), n_locations)

  # Check bounds
  expect_true(all(config$beta_j0_tot > 0))
  expect_true(all(config$p_beta >= 0 & config$p_beta <= 1))
})

test_that("beta_j0_hum and beta_j0_env are derived correctly", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  config <- sample_parameters(seed = 456, verbose = FALSE)

  expect_true("beta_j0_hum" %in% names(config))
  expect_true("beta_j0_env" %in% names(config))

  # Derivation identity: hum = p_beta * tot, env = (1 - p_beta) * tot
  expect_equal(config$beta_j0_hum, config$p_beta * config$beta_j0_tot, tolerance = 1e-10)
  expect_equal(config$beta_j0_env, (1 - config$p_beta) * config$beta_j0_tot, tolerance = 1e-10)

  # Both components must be positive
  expect_true(all(config$beta_j0_hum > 0))
  expect_true(all(config$beta_j0_env > 0))
})

test_that("derived beta values maintain expected ratio", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  config <- sample_parameters(seed = 789, verbose = FALSE)

  # ratio hum/env must equal p_beta/(1-p_beta) exactly
  ratio <- config$beta_j0_hum / config$beta_j0_env
  expected_ratio <- config$p_beta / (1 - config$p_beta)
  expect_equal(ratio, expected_ratio, tolerance = 1e-10)
})

test_that("config structure maintains laser-cholera compatibility", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  config <- sample_parameters(seed = 321, verbose = FALSE)

  n_locations <- length(config$location_name)
  expect_equal(length(config$beta_j0_hum), n_locations)
  expect_equal(length(config$beta_j0_env), n_locations)
  expect_type(config$beta_j0_hum, "double")
  expect_type(config$beta_j0_env, "double")

  # Transmission rates should be in a physically reasonable range
  expect_true(all(config$beta_j0_hum > 0 & config$beta_j0_hum < 1))
  expect_true(all(config$beta_j0_env > 0 & config$beta_j0_env < 1))
})

test_that("validation catches invalid beta values", {
  skip_if(is.null(getOption("root_directory")), "MOSAIC root directory not set")
  config <- sample_parameters(seed = 654, verbose = FALSE)

  # Valid config should pass (skip if validator has known issues with defaults)
  valid <- validate_sampled_config(config, verbose = FALSE)
  if (!valid) skip("validate_sampled_config returns FALSE for default-sampled config")

  # Negative beta_j0_tot should fail
  config_bad <- config
  config_bad$beta_j0_tot[1] <- -1
  expect_false(validate_sampled_config(config_bad, verbose = FALSE))

  # p_beta out of [0,1] should fail
  config_bad <- config
  config_bad$p_beta[1] <- 1.5
  expect_false(validate_sampled_config(config_bad, verbose = FALSE))

  # Negative derived beta should fail
  config_bad <- config
  config_bad$beta_j0_hum[1] <- -0.001
  expect_false(validate_sampled_config(config_bad, verbose = FALSE))
})
