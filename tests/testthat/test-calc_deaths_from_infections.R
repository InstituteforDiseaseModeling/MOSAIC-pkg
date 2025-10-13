test_that("calc_deaths_from_infections basic functionality", {

  # Basic test with no epidemic
  infections <- rep(10, 8)
  deaths <- calc_deaths_from_infections(
    infections = infections,
    N = 100000,
    mu_baseline = 0.01,
    mu_slope = 0,
    mu_epidemic_factor = 0.5,
    epidemic_threshold = 50/100000
  )

  expect_equal(deaths, rep(0.1, 8))  # 10 * 0.01 = 0.1
  expect_length(deaths, 8)
})


test_that("calc_deaths_from_infections epidemic threshold works", {

  # Create infections that cross threshold
  infections <- c(10, 20, 60, 100, 80, 40, 20, 10)  # 60 and above trigger epidemic
  N <- 100000
  threshold <- 50/100000  # Threshold is 50 per 100k

  deaths <- calc_deaths_from_infections(
    infections = infections,
    N = N,
    mu_baseline = 0.01,
    mu_slope = 0,
    mu_epidemic_factor = 0.5,  # 50% increase
    epidemic_threshold = threshold
  )

  # Check non-epidemic periods (0.01 IFR)
  expect_equal(deaths[1], 10 * 0.01)
  expect_equal(deaths[2], 20 * 0.01)
  expect_equal(deaths[7], 20 * 0.01)
  expect_equal(deaths[8], 10 * 0.01)

  # Check epidemic periods (0.015 IFR = 0.01 * 1.5)
  expect_equal(deaths[3], 60 * 0.015)
  expect_equal(deaths[4], 100 * 0.015)
  expect_equal(deaths[5], 80 * 0.015)
})


test_that("calc_deaths_from_infections temporal slope works", {

  infections <- rep(100, 5)

  deaths <- calc_deaths_from_infections(
    infections = infections,
    N = 100000,
    mu_baseline = 0.01,
    mu_slope = 0.2,  # 20% increase from start to end
    mu_epidemic_factor = 0,
    epidemic_threshold = 1  # Never reached
  )

  # IFR should increase linearly from 0.01 to 0.012
  expected_ifr <- 0.01 * (1 + 0.2 * (0:4)/4)
  expected_deaths <- infections * expected_ifr

  expect_equal(deaths, expected_deaths)
})


test_that("calc_deaths_from_infections handles time-varying population", {

  infections <- rep(50, 4)
  N_varying <- c(100000, 90000, 80000, 70000)  # Decreasing population

  deaths <- calc_deaths_from_infections(
    infections = infections,
    N = N_varying,
    mu_baseline = 0.01,
    mu_slope = 0,
    mu_epidemic_factor = 1.0,  # 100% increase
    epidemic_threshold = 60/100000  # Higher threshold
  )

  # Calculate expected epidemic flags
  incidence_rates <- infections / N_varying
  # c(50/100000, 50/90000, 50/80000, 50/70000)
  # = c(0.0005, 0.000556, 0.000625, 0.000714)
  # Only last two exceed 60/100000 = 0.0006

  expected_deaths <- c(
    50 * 0.01,  # Below threshold
    50 * 0.01,  # Below threshold
    50 * 0.02,  # Above threshold (0.01 * 2)
    50 * 0.02   # Above threshold
  )

  expect_equal(deaths, expected_deaths, tolerance = 1e-10)
})


test_that("calc_deaths_from_infections handles NA values", {

  infections <- c(10, NA, 30, 40, NA)

  deaths <- calc_deaths_from_infections(
    infections = infections,
    N = 100000,
    mu_baseline = 0.01,
    mu_slope = 0,
    mu_epidemic_factor = 0.5,
    epidemic_threshold = 50/100000
  )

  expect_true(is.na(deaths[2]))
  expect_true(is.na(deaths[5]))
  expect_equal(deaths[1], 0.1)
  expect_equal(deaths[3], 0.3)
  expect_equal(deaths[4], 0.4)
})


test_that("calc_deaths_from_infections input validation", {

  # Non-numeric infections
  expect_error(
    calc_deaths_from_infections(
      infections = "not numeric",
      N = 100000,
      mu_baseline = 0.01,
      mu_slope = 0,
      mu_epidemic_factor = 0.5,
      epidemic_threshold = 0.001
    ),
    "`infections` must be a numeric vector"
  )

  # Invalid N length
  expect_error(
    calc_deaths_from_infections(
      infections = c(10, 20, 30),
      N = c(100000, 90000),  # Wrong length
      mu_baseline = 0.01,
      mu_slope = 0,
      mu_epidemic_factor = 0.5,
      epidemic_threshold = 0.001
    ),
    "`N` must be a scalar or have same length as `infections`"
  )

  # Negative N
  expect_error(
    calc_deaths_from_infections(
      infections = c(10, 20),
      N = -100000,
      mu_baseline = 0.01,
      mu_slope = 0,
      mu_epidemic_factor = 0.5,
      epidemic_threshold = 0.001
    ),
    "`N` must be positive"
  )

  # Invalid mu_baseline
  expect_error(
    calc_deaths_from_infections(
      infections = c(10, 20),
      N = 100000,
      mu_baseline = 1.5,  # > 1
      mu_slope = 0,
      mu_epidemic_factor = 0.5,
      epidemic_threshold = 0.001
    ),
    "`mu_baseline` must be between 0 and 1"
  )

  # Negative mu_epidemic_factor
  expect_error(
    calc_deaths_from_infections(
      infections = c(10, 20),
      N = 100000,
      mu_baseline = 0.01,
      mu_slope = 0,
      mu_epidemic_factor = -0.5,
      epidemic_threshold = 0.001
    ),
    "`mu_epidemic_factor` must be non-negative"
  )
})


test_that("calc_deaths_from_infections edge cases", {

  # Empty input
  expect_equal(
    calc_deaths_from_infections(
      infections = numeric(0),
      N = 100000,
      mu_baseline = 0.01,
      mu_slope = 0,
      mu_epidemic_factor = 0.5,
      epidemic_threshold = 0.001
    ),
    numeric(0)
  )

  # Zero infections
  expect_equal(
    calc_deaths_from_infections(
      infections = rep(0, 5),
      N = 100000,
      mu_baseline = 0.01,
      mu_slope = 0,
      mu_epidemic_factor = 0.5,
      epidemic_threshold = 0.001
    ),
    rep(0, 5)
  )

  # Very high epidemic factor
  deaths <- calc_deaths_from_infections(
    infections = 100,
    N = 100000,
    mu_baseline = 0.5,
    mu_slope = 0,
    mu_epidemic_factor = 10,  # Would give IFR > 1
    epidemic_threshold = 50/100000
  )

  # Should be capped at IFR = 1
  expect_equal(deaths, 100)  # All infected die
})


test_that("calc_deaths_from_infections combined effects", {

  # Test slope + epidemic effects together
  infections <- c(20, 40, 80, 100, 60, 30)
  N <- 100000

  deaths <- calc_deaths_from_infections(
    infections = infections,
    N = N,
    mu_baseline = 0.01,
    mu_slope = 0.5,  # 50% increase over time
    mu_epidemic_factor = 1.0,  # 100% increase during epidemic
    epidemic_threshold = 50/100000
  )

  # Time factors: 0, 0.1, 0.2, 0.3, 0.4, 0.5
  # Epidemic flags: 0, 0, 1, 1, 1, 0
  # IFR = 0.01 * (1 + slope*time) * (1 + epidemic*flag)

  expected_ifr <- c(
    0.01 * 1.0 * 1.0,   # t=0, no epidemic
    0.01 * 1.1 * 1.0,   # t=0.2, no epidemic
    0.01 * 1.2 * 2.0,   # t=0.4, epidemic
    0.01 * 1.3 * 2.0,   # t=0.6, epidemic
    0.01 * 1.4 * 2.0,   # t=0.8, epidemic
    0.01 * 1.5 * 1.0    # t=1.0, no epidemic
  )

  expected_deaths <- infections * expected_ifr

  expect_equal(deaths, expected_deaths, tolerance = 1e-10)
})


test_that("calc_deaths_from_infections delta_t delay works", {

  # Test with 3-day delay
  infections <- c(10, 20, 50, 100, 80, 40, 20, 10)
  N <- 100000

  deaths_delayed <- calc_deaths_from_infections(
    infections = infections,
    N = N,
    mu_baseline = 0.01,
    mu_slope = 0,
    mu_epidemic_factor = 0.5,
    epidemic_threshold = 50/100000,
    delta_t = 3
  )

  # First 3 days should be NA (no infection data before start)
  expect_true(is.na(deaths_delayed[1]))
  expect_true(is.na(deaths_delayed[2]))
  expect_true(is.na(deaths_delayed[3]))

  # Deaths at day 4 from infections at day 1 (below threshold)
  expect_equal(deaths_delayed[4], 10 * 0.01)

  # Deaths at day 5 from infections at day 2 (below threshold)
  expect_equal(deaths_delayed[5], 20 * 0.01)

  # Deaths at day 6 from infections at day 3 (threshold = 50/100000)
  expect_equal(deaths_delayed[6], 50 * 0.01)

  # Deaths at day 7 from infections at day 4 (above threshold)
  expect_equal(deaths_delayed[7], 100 * 0.015)

  # Deaths at day 8 from infections at day 5 (above threshold)
  expect_equal(deaths_delayed[8], 80 * 0.015)
})


test_that("calc_deaths_from_infections delta_t with slope", {

  infections <- rep(100, 5)

  deaths_delayed <- calc_deaths_from_infections(
    infections = infections,
    N = 100000,
    mu_baseline = 0.01,
    mu_slope = 0.2,  # 20% increase over time
    mu_epidemic_factor = 0,
    epidemic_threshold = 1,  # Never reached
    delta_t = 2
  )

  # First 2 days should be NA
  expect_true(is.na(deaths_delayed[1]))
  expect_true(is.na(deaths_delayed[2]))

  # IFR should increase based on death day, not infection day
  # Time factor at death days 3,4,5: 2/4, 3/4, 4/4
  expected_ifr <- 0.01 * c(1 + 0.2 * 2/4, 1 + 0.2 * 3/4, 1 + 0.2 * 4/4)
  expected_deaths <- c(NA, NA, infections[1:3] * expected_ifr)

  expect_equal(deaths_delayed, expected_deaths)
})


test_that("calc_deaths_from_infections delta_t edge cases", {

  infections <- c(10, 20, 30, 40, 50)

  # Delta_t equals time series length
  deaths_long_delay <- calc_deaths_from_infections(
    infections = infections,
    N = 100000,
    mu_baseline = 0.01,
    mu_slope = 0,
    mu_epidemic_factor = 0.5,
    epidemic_threshold = 50/100000,
    delta_t = 5
  )
  expect_equal(deaths_long_delay, rep(NA_real_, 5))

  # Delta_t greater than time series length
  deaths_excess_delay <- calc_deaths_from_infections(
    infections = infections,
    N = 100000,
    mu_baseline = 0.01,
    mu_slope = 0,
    mu_epidemic_factor = 0.5,
    epidemic_threshold = 50/100000,
    delta_t = 10
  )
  expect_equal(deaths_excess_delay, rep(NA_real_, 5))

  # Delta_t validation
  expect_error(
    calc_deaths_from_infections(
      infections = infections,
      N = 100000,
      mu_baseline = 0.01,
      mu_slope = 0,
      mu_epidemic_factor = 0.5,
      epidemic_threshold = 50/100000,
      delta_t = -1
    ),
    "`delta_t` must be a non-negative integer"
  )

  expect_error(
    calc_deaths_from_infections(
      infections = infections,
      N = 100000,
      mu_baseline = 0.01,
      mu_slope = 0,
      mu_epidemic_factor = 0.5,
      epidemic_threshold = 50/100000,
      delta_t = 2.5
    ),
    "`delta_t` must be a non-negative integer"
  )
})