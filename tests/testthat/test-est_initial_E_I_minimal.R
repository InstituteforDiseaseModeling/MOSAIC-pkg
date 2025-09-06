# Minimal Unit Tests for est_initial_E_I Functions
# Just the core mathematical functionality

library(testthat)
source("../../R/est_initial_E_I.R")

test_that("Basic E/I estimation works", {
  # Minimal test case
  result <- est_initial_E_I_location(
    cases = c(1, 2, 1),
    dates = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    population = 10000,
    t0 = as.Date("2024-01-10"),
    sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
    iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
  )
  
  expect_true(result$E >= 0)
  expect_true(result$I >= 0)
  expect_true(result$E < 10000)
  expect_true(result$I < 10000)
})

test_that("Zero cases returns zero E/I", {
  result <- est_initial_E_I_location(
    cases = c(0, 0, 0),
    dates = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    population = 10000,
    t0 = as.Date("2024-01-10"),
    sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
    iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
  )
  
  expect_equal(result$E, 0)
  expect_equal(result$I, 0)
})

test_that("Parameter validation catches invalid inputs", {
  expect_error(
    est_initial_E_I_location(
      cases = c(1, 2), dates = as.Date(c("2024-01-01")),  # Mismatched lengths
      population = 10000, t0 = as.Date("2024-01-10"),
      sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
      iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
    )
  )
  
  expect_error(
    est_initial_E_I_location(
      cases = c(1, 2), dates = as.Date(c("2024-01-01", "2024-01-02")),
      population = -1000,  # Negative population
      t0 = as.Date("2024-01-10"),
      sigma = 0.2, rho = 0.1, chi = 0.7, tau_r = 4,
      iota = 0.714, gamma_1 = 0.2, gamma_2 = 0.67
    )
  )
})

test_that("Main function validates parameters", {
  expect_error(est_initial_E_I(PATHS = "not_list", priors = list(), config = list(), n_samples = 10))
  expect_error(est_initial_E_I(PATHS = list(), priors = list(), config = list(), n_samples = 0))
  expect_error(est_initial_E_I(PATHS = list(), priors = list(), config = list(), n_samples = 10, lookback_days = -1))
})