# Simple Unit Tests for est_initial_E_I Functions
# Focused tests for core functionality without complex mock data

library(testthat)

# Load the functions
source("../../R/est_initial_E_I.R")

# Test the location-specific function with minimal setup
test_that("est_initial_E_I_location basic functionality", {
  # Simple test data
  cases <- c(0, 1, 2, 3, 1, 0)
  dates <- as.Date("2024-01-01") + 0:5
  population <- 100000
  t0 <- as.Date("2024-01-10")
  
  # Standard parameters
  result <- est_initial_E_I_location(
    cases = cases,
    dates = dates,
    population = population,
    t0 = t0,
    lookback_days = 10,
    sigma = 0.2,
    rho = 0.1, 
    chi = 0.7,
    tau_r = 4,
    iota = 0.714,
    gamma_1 = 0.2,
    gamma_2 = 0.67,
    verbose = FALSE
  )
  
  # Basic checks
  expect_type(result, "list")
  expect_named(result, c("E", "I"))
  expect_true(is.numeric(result$E))
  expect_true(is.numeric(result$I))
  expect_true(result$E >= 0)
  expect_true(result$I >= 0)
  expect_true(result$E < population)
  expect_true(result$I < population)
})

test_that("est_initial_E_I_location parameter validation", {
  cases <- c(1, 2, 3)
  dates <- as.Date("2024-01-01") + 0:2
  population <- 100000
  t0 <- as.Date("2024-01-10")
  
  # Test parameter bounds
  expect_error(
    est_initial_E_I_location(
      cases = cases, dates = dates, population = population, t0 = t0,
      sigma = 1.5,  # Invalid sigma > 1
      rho = 0.1, chi = 0.7, tau_r = 4,
      iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
    ),
    "sigma must be in"
  )
  
  expect_error(
    est_initial_E_I_location(
      cases = cases, dates = dates, population = population, t0 = t0,
      sigma = 0.2, rho = 0, chi = 0.7, tau_r = 4,  # Invalid rho = 0
      iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
    ),
    "rho must be in"
  )
  
  expect_error(
    est_initial_E_I_location(
      cases = cases, dates = dates, population = -1000, t0 = t0,  # Negative population
      sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
      iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
    ),
    "population must be positive"
  )
})

test_that("est_initial_E_I_location handles zero cases", {
  cases <- c(0, 0, 0, 0, 0)
  dates <- as.Date("2024-01-01") + 0:4
  population <- 100000
  t0 <- as.Date("2024-01-10")
  
  result <- est_initial_E_I_location(
    cases = cases, dates = dates, population = population, t0 = t0,
    sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
    iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
  )
  
  # Should return zeros
  expect_equal(result$E, 0)
  expect_equal(result$I, 0)
})

test_that("est_initial_E_I_location mathematical consistency", {
  # Test that E is typically smaller than I in endemic settings
  cases <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)  # Steady cases
  dates <- as.Date("2024-01-01") + 0:9
  population <- 100000
  t0 <- as.Date("2024-01-15")
  
  result <- est_initial_E_I_location(
    cases = cases, dates = dates, population = population, t0 = t0,
    sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
    iota = 0.714,  # Fast incubation
    gamma_1 = 0.2,  # Slow recovery
    gamma_2 = 0.67
  )
  
  # In steady state with fast incubation, I should be > E
  expect_true(result$I >= result$E)
  expect_true(result$E >= 0)
  expect_true(result$I >= 0)
})

test_that("est_initial_E_I main function parameter validation", {
  # Test parameter validation for main function
  expect_error(
    est_initial_E_I(
      PATHS = "not_a_list",
      priors = list(),
      config = list(),
      n_samples = 100
    ),
    "PATHS must be a list"
  )
  
  expect_error(
    est_initial_E_I(
      PATHS = list(),
      priors = "not_a_list",
      config = list(),
      n_samples = 100
    ),
    "priors must be a list"
  )
  
  expect_error(
    est_initial_E_I(
      PATHS = list(),
      priors = list(),
      config = list(),
      n_samples = 0  # Invalid sample size
    ),
    "n_samples must be positive"
  )
  
  expect_error(
    est_initial_E_I(
      PATHS = list(),
      priors = list(),
      config = list(),
      n_samples = 100,
      lookback_days = -10  # Negative lookback
    ),
    "lookback_days must be positive"
  )
})

test_that("Beta fitting helper handles constant values", {
  # Test that the internal beta fitting can handle constant proportions
  # We test this by creating a scenario that should produce constant results
  
  cases <- rep(1, 10)  # Constant cases
  dates <- as.Date("2024-01-01") + 0:9
  population <- 1000000  # Large population
  t0 <- as.Date("2024-01-15")
  
  # Use identical parameters that should give very similar results
  results_E <- numeric(5)
  results_I <- numeric(5)
  
  for (i in 1:5) {
    result <- est_initial_E_I_location(
      cases = cases, dates = dates, population = population, t0 = t0,
      sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,  # Fixed parameters
      iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
    )
    results_E[i] <- result$E
    results_I[i] <- result$I
  }
  
  # Results should be identical with fixed parameters
  expect_equal(length(unique(results_E)), 1)
  expect_equal(length(unique(results_I)), 1)
  expect_true(all(results_E >= 0))
  expect_true(all(results_I >= 0))
})

test_that("Numerical stability with extreme parameters", {
  cases <- c(1, 2, 1)
  dates <- as.Date("2024-01-01") + 0:2
  population <- 100000
  t0 <- as.Date("2024-01-10")
  
  # Test with very fast rates (potential numerical issues)
  result_fast <- est_initial_E_I_location(
    cases = cases, dates = dates, population = population, t0 = t0,
    sigma = 0.9, rho = 0.9, chi = 0.9, tau_r = 0.1,
    iota = 10,  # Very fast incubation
    gamma_1 = 10,  # Very fast recovery
    gamma_2 = 10
  )
  
  # Should still produce valid results
  expect_true(is.finite(result_fast$E))
  expect_true(is.finite(result_fast$I))
  expect_true(result_fast$E >= 0)
  expect_true(result_fast$I >= 0)
  
  # Test with very slow rates
  result_slow <- est_initial_E_I_location(
    cases = cases, dates = dates, population = population, t0 = t0,
    sigma = 0.1, rho = 0.1, chi = 0.1, tau_r = 10,
    iota = 0.01,  # Very slow incubation
    gamma_1 = 0.01,  # Very slow recovery
    gamma_2 = 0.01
  )
  
  expect_true(is.finite(result_slow$E))
  expect_true(is.finite(result_slow$I))
  expect_true(result_slow$E >= 0)
  expect_true(result_slow$I >= 0)
})

test_that("Function handles mismatched cases and dates lengths", {
  cases <- c(1, 2, 3)
  dates <- as.Date("2024-01-01") + 0:4  # Different length
  population <- 100000
  t0 <- as.Date("2024-01-10")
  
  expect_error(
    est_initial_E_I_location(
      cases = cases, dates = dates, population = population, t0 = t0,
      sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
      iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
    ),
    "same length"
  )
})