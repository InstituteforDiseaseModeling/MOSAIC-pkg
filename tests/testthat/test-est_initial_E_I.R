# Test file for est_initial_E_I functions
# Tests both the main wrapper function and the location-specific function

library(testthat)

# Load the functions by sourcing the R file
source("../../R/est_initial_E_I.R")

# Mock data and helper functions for testing
create_mock_surveillance_data <- function(n_days = 60, base_cases = 5) {
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = n_days)
  cases <- rpois(n_days, lambda = base_cases)
  data.frame(
    date = dates,
    iso_code = rep("TCD", n_days),
    cases = cases,
    stringsAsFactors = FALSE
  )
}

create_mock_population_data <- function() {
  data.frame(
    date = as.Date("2024-01-15"),
    iso_code = "TCD", 
    total_population = 1000000,
    stringsAsFactors = FALSE
  )
}

create_mock_priors <- function() {
  list(
    parameters_global = list(
      sigma = list(distribution = "beta", parameters = list(shape1 = 2, shape2 = 8)),
      iota = list(distribution = "gamma", parameters = list(shape = 2, rate = 2.8)),
      gamma_1 = list(distribution = "uniform", parameters = list(min = 0.14, max = 0.33)),
      gamma_2 = list(distribution = "uniform", parameters = list(min = 0.5, max = 1.0))
    ),
    parameters_location = list(
      rho = list(
        parameters = list(
          location = list(
            TCD = list(distribution = "beta", parameters = list(shape1 = 1, shape2 = 9))
          )
        )
      ),
      chi = list(
        parameters = list(
          location = list(
            TCD = list(distribution = "beta", parameters = list(shape1 = 5, shape2 = 5))
          )
        )
      )
    )
  )
}

create_mock_paths <- function(temp_dir) {
  list(
    DATA_CHOLERA_DAILY = temp_dir,
    DATA_DEMOGRAPHICS = temp_dir
  )
}

create_mock_config <- function() {
  list(
    location_name = c("TCD"),
    date_start = "2024-03-01"
  )
}

# Test the location-specific function first
test_that("est_initial_E_I_location works with basic inputs", {
  # Create test data
  cases <- c(0, 1, 2, 5, 3, 1, 0, 0, 1, 2)
  dates <- seq(as.Date("2024-02-01"), by = "day", length.out = 10)
  population <- 100000
  t0 <- as.Date("2024-02-15")
  
  # Test parameters
  sigma <- 0.2
  rho <- 0.1
  chi <- 0.7
  tau_r <- 4
  iota <- 0.714
  gamma_1 <- 0.2
  gamma_2 <- 0.67
  
  result <- est_initial_E_I_location(
    cases = cases,
    dates = dates,
    population = population,
    t0 = t0,
    lookback_days = 14,
    sigma = sigma,
    rho = rho,
    chi = chi,
    tau_r = tau_r,
    iota = iota,
    gamma_1 = gamma_1,
    gamma_2 = gamma_2,
    verbose = FALSE
  )
  
  # Basic checks
  expect_type(result, "list")
  expect_named(result, c("E", "I"))
  expect_true(result$E >= 0)
  expect_true(result$I >= 0)
  expect_true(result$E < population)
  expect_true(result$I < population)
})

test_that("est_initial_E_I_location parameter validation works", {
  # Test data
  cases <- c(1, 2, 3)
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 3)
  population <- 100000
  t0 <- as.Date("2024-01-10")
  
  # Test invalid parameters
  expect_error(est_initial_E_I_location(
    cases = cases[1:2],  # Wrong length
    dates = dates,
    population = population,
    t0 = t0,
    sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
    iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
  ), "same length")
  
  expect_error(est_initial_E_I_location(
    cases = cases, dates = dates,
    population = -1000,  # Negative population
    t0 = t0,
    sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
    iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
  ), "population must be positive")
  
  expect_error(est_initial_E_I_location(
    cases = cases, dates = dates, population = population, t0 = t0,
    sigma = 1.5,  # Invalid sigma > 1
    rho = 0.1, chi = 0.7, tau_r = 4,
    iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
  ), "sigma must be in")
  
  expect_error(est_initial_E_I_location(
    cases = cases, dates = dates, population = population, t0 = t0,
    sigma = 0.2, rho = -0.1,  # Negative rho
    chi = 0.7, tau_r = 4,
    iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
  ), "rho must be in")
})

test_that("est_initial_E_I_location handles edge cases", {
  population <- 100000
  t0 <- as.Date("2024-01-10")
  
  # Test with zero cases
  cases_zero <- rep(0, 10)
  dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 10)
  
  result_zero <- est_initial_E_I_location(
    cases = cases_zero, dates = dates, population = population, t0 = t0,
    sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
    iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
  )
  
  expect_equal(result_zero$E, 0)
  expect_equal(result_zero$I, 0)
  
  # Test with very high cases (should warn but not fail)
  cases_high <- rep(1000, 10)
  expect_warning(
    result_high <- est_initial_E_I_location(
      cases = cases_high, dates = dates, population = population, t0 = t0,
      sigma = 0.01, rho = 0.01, chi = 1.0, tau_r = 4,  # Parameters that create high multiplier
      iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
    ),
    "Implausibly high infection multiplier"
  )
  
  expect_true(result_high$E >= 0)
  expect_true(result_high$I >= 0)
})

# Test the main wrapper function
test_that("est_initial_E_I wrapper function works with mock data", {
  skip_if_not_installed("withr")
  
  withr::with_tempdir({
    temp_dir <- getwd()
    
    # Create mock data files
    surveillance_data <- create_mock_surveillance_data(n_days = 90, base_cases = 3)
    population_data <- create_mock_population_data()
    
    write.csv(surveillance_data, 
              file.path(temp_dir, "cholera_surveillance_daily_combined.csv"),
              row.names = FALSE)
    write.csv(population_data,
              file.path(temp_dir, "UN_world_population_prospects_daily.csv"), 
              row.names = FALSE)
    
    # Create mock inputs
    PATHS <- create_mock_paths(temp_dir)
    priors <- create_mock_priors()
    config <- create_mock_config()
    
    # Test basic functionality
    result <- est_initial_E_I(
      PATHS = PATHS,
      priors = priors, 
      config = config,
      n_samples = 10,  # Small for speed
      t0 = as.Date("2024-03-01"),
      lookback_days = 30,
      verbose = FALSE,
      parallel = FALSE
    )
    
    # Check result structure
    expect_type(result, "list")
    expect_named(result, c("metadata", "parameters_location"))
    
    # Check metadata
    expect_type(result$metadata, "list")
    expect_true("description" %in% names(result$metadata))
    expect_true("n_samples" %in% names(result$metadata))
    expect_equal(result$metadata$n_samples, 10)
    
    # Check parameters structure
    expect_named(result$parameters_location, c("prop_E_initial", "prop_I_initial"))
    
    # Check location-specific results
    tcd_E <- result$parameters_location$prop_E_initial$parameters$location$TCD
    tcd_I <- result$parameters_location$prop_I_initial$parameters$location$TCD
    
    expect_type(tcd_E, "list")
    expect_type(tcd_I, "list")
    expect_true("shape1" %in% names(tcd_E))
    expect_true("shape2" %in% names(tcd_E))
    expect_true("metadata" %in% names(tcd_E))
    
    # Check values are reasonable
    expect_true(tcd_E$shape1 > 0)
    expect_true(tcd_E$shape2 > 0)
    # Check that mean can be calculated from Beta parameters
    E_mean <- tcd_E$shape1 / (tcd_E$shape1 + tcd_E$shape2)
    I_mean <- tcd_I$shape1 / (tcd_I$shape1 + tcd_I$shape2)
    expect_true(E_mean >= 0 && E_mean <= 1)
    expect_true(I_mean >= 0 && I_mean <= 1)
  })
})

test_that("est_initial_E_I validation works", {
  # Test parameter validation
  expect_error(est_initial_E_I(
    PATHS = "not_a_list",
    priors = list(),
    config = list(),
    n_samples = 100
  ), "PATHS must be a list")
  
  expect_error(est_initial_E_I(
    PATHS = list(),
    priors = "not_a_list", 
    config = list(),
    n_samples = 100
  ), "priors must be a list")
  
  expect_error(est_initial_E_I(
    PATHS = list(),
    priors = list(),
    config = list(),
    n_samples = 0  # Invalid sample size
  ), "n_samples must be positive")
  
  expect_error(est_initial_E_I(
    PATHS = list(),
    priors = list(),
    config = list(),
    n_samples = 100,
    lookback_days = -10  # Negative lookback
  ), "lookback_days must be positive")
})

test_that("est_initial_E_I handles missing data gracefully", {
  skip_if_not_installed("withr")
  
  withr::with_tempdir({
    temp_dir <- getwd()
    
    # Create mock data with missing location
    surveillance_data <- create_mock_surveillance_data()
    surveillance_data$iso_code <- "AGO"  # Different location
    population_data <- create_mock_population_data() 
    population_data$iso_code <- "AGO"
    
    write.csv(surveillance_data,
              file.path(temp_dir, "cholera_surveillance_daily_combined.csv"),
              row.names = FALSE)
    write.csv(population_data,
              file.path(temp_dir, "UN_world_population_prospects_daily.csv"),
              row.names = FALSE)
    
    PATHS <- create_mock_paths(temp_dir)
    priors <- create_mock_priors()
    config <- create_mock_config()  # Still expects TCD
    
    result <- est_initial_E_I(
      PATHS = PATHS,
      priors = priors,
      config = config,
      n_samples = 10,
      verbose = FALSE,
      parallel = FALSE
    )
    
    # Should still return structure but with default near-zero values
    tcd_E <- result$parameters_location$prop_E_initial$parameters$location$TCD
    expect_equal(tcd_E$shape1, 1.0)  # Reverted default
    expect_equal(tcd_E$shape2, 9999)
    expect_false(tcd_E$metadata$data_available)
  })
})

test_that("Parallel processing works", {
  skip_if_not_installed("withr")
  skip_on_os("windows")  # mclapply doesn't work on Windows
  
  withr::with_tempdir({
    temp_dir <- getwd()
    
    surveillance_data <- create_mock_surveillance_data(n_days = 90, base_cases = 5)
    population_data <- create_mock_population_data()
    
    write.csv(surveillance_data,
              file.path(temp_dir, "cholera_surveillance_daily_combined.csv"),
              row.names = FALSE)
    write.csv(population_data,
              file.path(temp_dir, "UN_world_population_prospects_daily.csv"),
              row.names = FALSE)
    
    PATHS <- create_mock_paths(temp_dir)
    priors <- create_mock_priors()
    config <- create_mock_config()
    
    # Test parallel processing with sufficient samples
    result_parallel <- est_initial_E_I(
      PATHS = PATHS,
      priors = priors,
      config = config,
      n_samples = 100,  # Minimum for parallel
      verbose = FALSE,
      parallel = TRUE
    )
    
    # Should still produce valid results
    expect_type(result_parallel, "list")
    tcd_E <- result_parallel$parameters_location$prop_E_initial$parameters$location$TCD
    expect_true(tcd_E$shape1 > 0)
    expect_true(tcd_E$shape2 > 0)
  })
})

# Test helper functions
test_that("Beta fitting helper handles edge cases", {
  # This requires accessing the internal function, so we'll test through the main function
  # with crafted data that should trigger different beta fitting scenarios
  
  withr::with_tempdir({
    temp_dir <- getwd()
    
    # Create data that should trigger constant value scenario
    surveillance_data <- data.frame(
      date = seq(as.Date("2024-01-01"), by = "day", length.out = 60),
      iso_code = rep("TCD", 60),
      cases = c(rep(1, 30), rep(0, 30)),  # Constant cases for some period
      stringsAsFactors = FALSE
    )
    population_data <- create_mock_population_data()
    
    write.csv(surveillance_data,
              file.path(temp_dir, "cholera_surveillance_daily_combined.csv"),
              row.names = FALSE)
    write.csv(population_data,
              file.path(temp_dir, "UN_world_population_prospects_daily.csv"),
              row.names = FALSE)
    
    PATHS <- create_mock_paths(temp_dir)
    priors <- create_mock_priors()
    config <- create_mock_config()
    
    # Should handle this without error
    result <- est_initial_E_I(
      PATHS = PATHS,
      priors = priors,
      config = config,
      n_samples = 10,
      verbose = FALSE
    )
    
    expect_type(result, "list")
    tcd_E <- result$parameters_location$prop_E_initial$parameters$location$TCD
    expect_true(tcd_E$shape1 > 0)
    expect_true(tcd_E$shape2 > 0)
  })
})