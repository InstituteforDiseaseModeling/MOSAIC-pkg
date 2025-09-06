# Unit tests for est_initial_V1_V2 function
library(testthat)
library(MOSAIC)

# Source the function if not available
if (!exists("est_initial_V1_V2")) {
  source("../../R/est_initial_V1_V2.R")
}

# Create mock data and setup for testing
create_test_environment <- function(temp_dir) {
  # Create temporary directory structure
  model_input_dir <- file.path(temp_dir, "MODEL_INPUT")
  data_demo_dir <- file.path(temp_dir, "DATA_DEMOGRAPHICS")
  dir.create(model_input_dir, recursive = TRUE)
  dir.create(data_demo_dir, recursive = TRUE)
  
  # Create mock vaccination data
  vacc_data <- data.frame(
    iso_code = rep(c("TST", "TST2"), each = 3),
    date = rep(c("2022-01-01", "2022-06-01", "2022-12-01"), 2),
    doses_distributed = c(1000, 2000, 3000, 500, 1000, 1500),
    stringsAsFactors = FALSE
  )
  write.csv(vacc_data, 
            file.path(model_input_dir, "data_vaccinations_GTFCC_redistributed.csv"),
            row.names = FALSE)
  
  # Create mock population data
  pop_data <- data.frame(
    iso_code = rep(c("TST", "TST2"), each = 3),
    date = rep(c("2022-01-01", "2023-01-01", "2024-01-01"), 2),
    total_population = c(100000, 100500, 101000, 50000, 50250, 50500),
    stringsAsFactors = FALSE
  )
  write.csv(pop_data,
            file.path(data_demo_dir, "UN_world_population_prospects_daily.csv"),
            row.names = FALSE)
  
  # Create mock PATHS object
  PATHS <- list(
    MODEL_INPUT = model_input_dir,
    DATA_DEMOGRAPHICS = data_demo_dir
  )
  
  return(PATHS)
}

# Create mock priors object
create_test_priors <- function() {
  list(
    parameters_global = list(
      omega_1 = list(
        parameter_name = "omega_1",
        distribution = "gamma",
        parameters = list(shape = 2, rate = 3000)
      ),
      omega_2 = list(
        parameter_name = "omega_2",
        distribution = "gamma", 
        parameters = list(shape = 2, rate = 4000)
      ),
      phi_1 = list(
        parameter_name = "phi_1",
        distribution = "beta",
        parameters = list(shape1 = 10, shape2 = 5)
      ),
      phi_2 = list(
        parameter_name = "phi_2",
        distribution = "beta",
        parameters = list(shape1 = 12, shape2 = 3)
      )
    )
  )
}

test_that("est_initial_V1_V2 returns expected structure", {
  skip_if_not_installed("fitdistrplus")
  
  # Setup test environment
  temp_dir <- tempdir()
  PATHS <- create_test_environment(temp_dir)
  priors <- create_test_priors()
  
  config <- list(
    date_start = "2023-01-01",
    location_codes = c("TST")
  )
  
  # Run function with small sample size
  result <- est_initial_V1_V2(
    PATHS = PATHS,
    priors = priors,
    config = config,
    n_samples = 10,  # Small for testing
    verbose = FALSE
  )
  
  # Check structure - now matches priors_default
  expect_type(result, "list")
  expect_s3_class(result, "mosaic_initial_conditions")
  expect_named(result, c("metadata", "parameters_location"))
  
  # Check metadata structure
  expect_type(result$metadata, "list")
  expect_true("initial_conditions_V1_V2" %in% names(result$metadata))
  expect_equal(result$metadata$initial_conditions_V1_V2$t0, "2023-01-01")
  expect_equal(result$metadata$initial_conditions_V1_V2$n_samples, 10)
  expect_equal(result$metadata$initial_conditions_V1_V2$n_locations_processed, 1)
  
  # Check parameters_location structure - now parameter-first hierarchy
  expect_type(result$parameters_location, "list")
  expect_named(result$parameters_location, c("prop_V1_initial", "prop_V2_initial"))
  
  # Check prop_V1_initial structure
  v1_param <- result$parameters_location$prop_V1_initial
  expect_type(v1_param, "list")
  expect_equal(v1_param$parameter_name, "prop_V1_initial")
  expect_equal(v1_param$distribution, "beta")
  expect_true("location" %in% names(v1_param$parameters))
  
  # Check TST location within prop_V1_initial
  expect_true("TST" %in% names(v1_param$parameters$location))
  tst_v1 <- v1_param$parameters$location$TST
  expect_type(tst_v1, "list")
  expect_true("shape1" %in% names(tst_v1))
  expect_true("shape2" %in% names(tst_v1))
  expect_true("metadata" %in% names(tst_v1))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("est_initial_V1_V2 handles multiple locations", {
  skip_if_not_installed("fitdistrplus")
  
  # Setup test environment
  temp_dir <- tempdir()
  PATHS <- create_test_environment(temp_dir)
  priors <- create_test_priors()
  
  config <- list(
    date_start = "2023-01-01",
    location_codes = c("TST", "TST2")
  )
  
  # Run function
  result <- est_initial_V1_V2(
    PATHS = PATHS,
    priors = priors,
    config = config,
    n_samples = 10,
    verbose = FALSE
  )
  
  # Check both locations processed - new structure
  expect_equal(result$metadata$initial_conditions_V1_V2$n_locations_processed, 2)
  expect_true(all(c("TST", "TST2") %in% names(result$parameters_location$prop_V1_initial$parameters$location)))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("est_initial_V1_V2 handles missing data gracefully", {
  skip_if_not_installed("fitdistrplus")
  
  # Setup test environment
  temp_dir <- tempdir()
  PATHS <- create_test_environment(temp_dir)
  priors <- create_test_priors()
  
  config <- list(
    date_start = "2023-01-01",
    location_codes = c("TST", "MISSING")  # MISSING not in data
  )
  
  # Run function - should process TST but skip MISSING with warning
  expect_warning(
    result <- est_initial_V1_V2(
      PATHS = PATHS,
      priors = priors,
      config = config,
      n_samples = 10,
      verbose = FALSE
    ),
    "No vaccination data for MISSING"
  )
  
  # Should only have TST in results
  expect_equal(result$metadata$initial_conditions_V1_V2$n_locations_processed, 1)
  expect_true("TST" %in% names(result$parameters_location$prop_V1_initial$parameters$location))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("est_initial_V1_V2 validates inputs", {
  # Invalid PATHS
  expect_error(
    est_initial_V1_V2(
      PATHS = "not_a_list",
      priors = list(),
      config = list()
    ),
    "PATHS must be a list"
  )
  
  # Invalid priors
  expect_error(
    est_initial_V1_V2(
      PATHS = list(),
      priors = "not_a_list",
      config = list()
    ),
    "priors must be a priors object"
  )
  
  # Invalid config
  expect_error(
    est_initial_V1_V2(
      PATHS = list(),
      priors = list(),
      config = "not_a_list"
    ),
    "config must be a configuration object"
  )
  
  # Missing t0
  temp_dir <- tempdir()
  PATHS <- create_test_environment(temp_dir)
  priors <- create_test_priors()
  
  config_no_t0 <- list(
    location_codes = c("TST")
  )
  
  expect_error(
    est_initial_V1_V2(
      PATHS = PATHS,
      priors = priors,
      config = config_no_t0
    ),
    "t0 must be provided or available in config\\$date_start"
  )
  
  # Missing location codes
  config_no_locs <- list(
    date_start = "2023-01-01"
  )
  
  expect_error(
    est_initial_V1_V2(
      PATHS = PATHS,
      priors = priors,
      config = config_no_locs
    ),
    "No location codes found in config"
  )
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("est_initial_V1_V2 handles missing prior distributions", {
  temp_dir <- tempdir()
  PATHS <- create_test_environment(temp_dir)
  
  # Priors missing omega_1
  priors_incomplete <- list(
    parameters_global = list(
      omega_2 = list(distribution = "gamma", parameters = list(shape = 2, rate = 3000)),
      phi_1 = list(distribution = "beta", parameters = list(shape1 = 10, shape2 = 5)),
      phi_2 = list(distribution = "beta", parameters = list(shape1 = 12, shape2 = 3))
    )
  )
  
  config <- list(
    date_start = "2023-01-01",
    location_codes = c("TST")
  )
  
  expect_error(
    est_initial_V1_V2(
      PATHS = PATHS,
      priors = priors_incomplete,
      config = config
    ),
    "Required prior distributions .* not found"
  )
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("est_initial_V1_V2 parallel processing works on Unix", {
  skip_on_os("windows")  # Skip on Windows
  skip_if_not_installed("fitdistrplus")
  
  # Setup test environment
  temp_dir <- tempdir()
  PATHS <- create_test_environment(temp_dir)
  priors <- create_test_priors()
  
  config <- list(
    date_start = "2023-01-01",
    location_codes = c("TST", "TST2")
  )
  
  # Run with parallel processing
  result <- est_initial_V1_V2(
    PATHS = PATHS,
    priors = priors,
    config = config,
    n_samples = 10,
    verbose = FALSE,
    parallel = TRUE  # Enable parallel
  )
  
  # Should process both locations - use new metadata structure
  expect_equal(result$metadata$initial_conditions_V1_V2$n_locations_processed, 2)
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("est_initial_V1_V2 handles t0 parameter override", {
  skip_if_not_installed("fitdistrplus")
  
  # Setup test environment
  temp_dir <- tempdir()
  PATHS <- create_test_environment(temp_dir)
  priors <- create_test_priors()
  
  config <- list(
    date_start = "2023-01-01",
    location_codes = c("TST")
  )
  
  # Override t0
  result <- est_initial_V1_V2(
    PATHS = PATHS,
    priors = priors,
    config = config,
    n_samples = 10,
    t0 = "2023-06-01",  # Override config t0
    verbose = FALSE
  )
  
  # Should use overridden t0
  expect_equal(result$metadata$initial_conditions_V1_V2$t0, "2023-06-01")
  expect_equal(result$parameters_location$prop_V1_initial$parameters$location$TST$metadata$t0, "2023-06-01")
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("est_initial_V1_V2 Monte Carlo sampling produces reasonable variation", {
  skip_if_not_installed("fitdistrplus")
  
  # Setup test environment
  temp_dir <- tempdir()
  PATHS <- create_test_environment(temp_dir)
  priors <- create_test_priors()
  
  config <- list(
    date_start = "2023-01-01",
    location_codes = c("TST")
  )
  
  # Run with more samples to check variation
  result <- est_initial_V1_V2(
    PATHS = PATHS,
    priors = priors,
    config = config,
    n_samples = 100,  # More samples
    verbose = FALSE
  )
  
  tst_v1 <- result$parameters_location$prop_V1_initial$parameters$location$TST
  
  # Beta parameters should be fitted (or NA if no data)
  if (!is.na(tst_v1$shape1)) {
    expect_true(tst_v1$shape1 > 0)
    expect_true(tst_v1$shape2 > 0)
    # Should have metadata with variation info
    expect_true(tst_v1$metadata$ci_upper > tst_v1$metadata$ci_lower)
  }
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("print method works for mosaic_initial_conditions", {
  skip_if_not_installed("fitdistrplus")
  
  # Setup test environment
  temp_dir <- tempdir()
  PATHS <- create_test_environment(temp_dir)
  priors <- create_test_priors()
  
  config <- list(
    date_start = "2023-01-01",
    location_codes = c("TST")
  )
  
  result <- est_initial_V1_V2(
    PATHS = PATHS,
    priors = priors,
    config = config,
    n_samples = 10,
    verbose = FALSE
  )
  
  # Should print without error
  expect_output(print(result), "MOSAIC Initial V1/V2 Conditions")
  expect_output(print(result), "TST")
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})