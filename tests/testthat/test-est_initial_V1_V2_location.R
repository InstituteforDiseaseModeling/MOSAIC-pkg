# Unit tests for est_initial_V1_V2_location function
library(testthat)
library(MOSAIC)

test_that("est_initial_V1_V2_location returns expected structure", {
  # Simple test case
  dates <- as.Date(c("2022-01-01", "2022-06-01"))
  doses <- c(1000, 2000)
  t0 <- as.Date("2023-01-01")
  N <- 100000
  
  result <- est_initial_V1_V2_location(
    dates = dates,
    doses = doses,
    N = N,
    t0 = t0
  )
  
  # Check result structure
  expect_type(result, "double")
  expect_named(result, c("V1", "V2"))
  expect_true(result["V1"] >= 0)
  expect_true(result["V2"] >= 0)
  expect_true(result["V1"] + result["V2"] <= N)
})

test_that("est_initial_V1_V2_location handles no waning correctly", {
  dates <- as.Date("2022-01-01")
  doses <- 1000
  t0 <- as.Date("2023-01-01")
  N <- 100000
  
  # With essentially no waning and perfect effectiveness
  result <- est_initial_V1_V2_location(
    dates = dates,
    doses = doses,
    N = N,
    t0 = t0,
    omega1 = 1e-10,
    omega2 = 1e-10,
    phi1 = 1.0,
    phi2 = 1.0,
    t_lag = 0
  )
  
  # Should retain all doses
  expect_equal(as.numeric(result["V1"]), 1000, tolerance = 0.1)
  expect_equal(as.numeric(result["V2"]), 0)
})

test_that("est_initial_V1_V2_location respects vaccination ceiling", {
  dates <- rep(as.Date("2022-01-01"), 10)
  doses <- rep(10000, 10)  # 100,000 total
  t0 <- as.Date("2023-01-01")
  N <- 100000
  
  result <- est_initial_V1_V2_location(
    dates = dates,
    doses = doses,
    N = N,
    t0 = t0,
    vacc_ceiling_frac = 0.6,  # 60% ceiling = 60,000
    omega1 = 1e-10,
    omega2 = 1e-10,
    phi1 = 1.0,
    phi2 = 1.0,
    t_lag = 0
  )
  
  # Total protected should not exceed ceiling
  expect_true(result["V1"] + result["V2"] <= 60000)
})

test_that("est_initial_V1_V2_location applies protection lag correctly", {
  t0 <- as.Date("2023-01-01")
  N <- 100000
  
  # Dose given 10 days before t0 (less than 14 day lag)
  result_recent <- est_initial_V1_V2_location(
    dates = as.Date("2022-12-22"),
    doses = 1000,
    N = N,
    t0 = t0,
    omega1 = 1e-10,
    phi1 = 1.0,
    t_lag = 14
  )
  
  # Should have no protection yet
  expect_equal(as.numeric(result_recent["V1"]), 0)
  
  # Dose given 20 days before t0 (more than 14 day lag)
  result_older <- est_initial_V1_V2_location(
    dates = as.Date("2022-12-12"),
    doses = 1000,
    N = N,
    t0 = t0,
    omega1 = 1e-10,
    phi1 = 1.0,
    t_lag = 14
  )
  
  # Should have protection
  expect_equal(as.numeric(result_older["V1"]), 1000, tolerance = 0.1)
})

test_that("est_initial_V1_V2_location applies vaccine effectiveness", {
  dates <- as.Date("2022-01-01")
  doses <- 1000
  t0 <- as.Date("2023-01-01")
  N <- 100000
  
  result <- est_initial_V1_V2_location(
    dates = dates,
    doses = doses,
    N = N,
    t0 = t0,
    omega1 = 1e-10,
    phi1 = 0.5,  # 50% effectiveness
    t_lag = 0
  )
  
  # Should reduce by effectiveness factor
  expect_equal(as.numeric(result["V1"]), 500, tolerance = 0.1)
})

test_that("est_initial_V1_V2_location handles FIFO and LIFO allocation", {
  dates <- as.Date(c("2020-01-01", "2021-01-01", "2022-01-01"))
  doses <- c(30000, 30000, 40000)  # Total 100,000
  t0 <- as.Date("2023-01-01")
  N <- 100000
  
  # FIFO allocation
  result_fifo <- est_initial_V1_V2_location(
    dates = dates,
    doses = doses,
    N = N,
    t0 = t0,
    vacc_ceiling_frac = 0.6,  # Triggers V2 allocation
    omega1 = 1e-10,
    omega2 = 1e-10,
    phi1 = 1.0,
    phi2 = 1.0,
    t_lag = 0,
    min_interdose_days = 40,
    allocation_logic = "FIFO"
  )
  
  # LIFO allocation
  result_lifo <- est_initial_V1_V2_location(
    dates = dates,
    doses = doses,
    N = N,
    t0 = t0,
    vacc_ceiling_frac = 0.6,
    omega1 = 1e-10,
    omega2 = 1e-10,
    phi1 = 1.0,
    phi2 = 1.0,
    t_lag = 0,
    min_interdose_days = 40,
    allocation_logic = "LIFO"
  )
  
  # Both should respect ceiling
  expect_true(result_fifo["V1"] + result_fifo["V2"] <= 60000)
  expect_true(result_lifo["V1"] + result_lifo["V2"] <= 60000)
  
  # V2 should be allocated
  expect_true(result_fifo["V2"] > 0)
  expect_true(result_lifo["V2"] > 0)
})

test_that("est_initial_V1_V2_location handles time-varying population", {
  dates <- as.Date(c("2022-01-01", "2022-06-01", "2022-12-01"))
  doses <- c(1000, 2000, 3000)
  t0 <- as.Date("2023-01-01")
  N <- c(100000, 101000, 102000)  # Growing population
  
  result <- est_initial_V1_V2_location(
    dates = dates,
    doses = doses,
    N = N,
    t0 = t0,
    omega1 = 1e-10,
    phi1 = 1.0,
    t_lag = 0
  )
  
  # Should handle vector N without error
  expect_type(result, "double")
  expect_named(result, c("V1", "V2"))
  expect_equal(as.numeric(result["V1"]), 6000, tolerance = 0.1)
})

test_that("est_initial_V1_V2_location filters future doses", {
  dates <- as.Date(c("2022-01-01", "2023-06-01"))  # One before, one after t0
  doses <- c(1000, 2000)
  t0 <- as.Date("2023-01-01")
  N <- 100000
  
  result <- est_initial_V1_V2_location(
    dates = dates,
    doses = doses,
    N = N,
    t0 = t0,
    omega1 = 1e-10,
    phi1 = 1.0,
    t_lag = 0
  )
  
  # Should only count dose before t0
  expect_equal(as.numeric(result["V1"]), 1000, tolerance = 0.1)
})

test_that("est_initial_V1_V2_location applies waning correctly", {
  dates <- as.Date("2022-01-01")
  doses <- 1000
  t0 <- as.Date("2023-01-01")  # 365 days later
  N <- 100000
  
  # With waning rate that gives ~50% remaining after 1 year
  omega1 <- log(2) / 365  # Half-life of 1 year
  
  result <- est_initial_V1_V2_location(
    dates = dates,
    doses = doses,
    N = N,
    t0 = t0,
    omega1 = omega1,
    phi1 = 1.0,
    t_lag = 0
  )
  
  # Should be approximately 500 (50% of 1000)
  expect_equal(as.numeric(result["V1"]), 500, tolerance = 50)
})

test_that("est_initial_V1_V2_location validates inputs", {
  dates <- as.Date("2022-01-01")
  doses <- 1000
  t0 <- as.Date("2023-01-01")
  N <- 100000
  
  # Invalid allocation logic
  expect_error(
    est_initial_V1_V2_location(
      dates = dates,
      doses = doses,
      N = N,
      t0 = t0,
      allocation_logic = "INVALID"
    ),
    "allocation_logic must be either 'FIFO' or 'LIFO'"
  )
  
  # Mismatched lengths
  expect_error(
    est_initial_V1_V2_location(
      dates = dates,
      doses = c(1000, 2000),  # Wrong length
      N = N,
      t0 = t0
    ),
    "dates and doses must have the same length"
  )
  
  # Invalid parameters
  expect_error(
    est_initial_V1_V2_location(
      dates = dates,
      doses = doses,
      N = N,
      t0 = t0,
      omega1 = -1
    ),
    "omega1 must be a single positive finite numeric value"
  )
  
  expect_error(
    est_initial_V1_V2_location(
      dates = dates,
      doses = doses,
      N = N,
      t0 = t0,
      phi1 = 1.5
    ),
    "phi1 must be a single numeric value between 0 and 1"
  )
})