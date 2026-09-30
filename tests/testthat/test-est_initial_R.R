library(MOSAIC)

test_that("disagg_annual_cases_to_daily preserves annual totals exactly", {
    # Test data
    annual_cases <- c(100, 250, 180)
    years <- c(2021, 2022, 2023)
    
    # Test with different Fourier coefficients
    test_cases <- list(
        list(a1 = 0.5, b1 = 0.3, a2 = 0.2, b2 = 0.1),  # Mixed seasonality
        list(a1 = 1, b1 = 0, a2 = 0, b2 = 0),          # Pure cosine
        list(a1 = 0, b1 = 1, a2 = 0, b2 = 0),          # Pure sine
        list(a1 = 0, b1 = 0, a2 = 0, b2 = 0)           # Uniform (all zero)
    )
    
    for (params in test_cases) {
        result <- disagg_annual_cases_to_daily(
            annual_cases = annual_cases,
            years = years,
            a1 = params$a1,
            b1 = params$b1,
            a2 = params$a2,
            b2 = params$b2
        )
        
        # Check structure
        expect_true(is.data.frame(result))
        expect_true(all(c("date", "year", "day_of_year", "cases", "annual_total") %in% names(result)))
        
        # Check total preservation for each year
        for (i in seq_along(years)) {
            year_data <- result[result$year == years[i], ]
            total <- sum(year_data$cases)
            expected <- annual_cases[i]
            
            # Test exact preservation (within numerical precision)
            expect_equal(total, expected, tolerance = 1e-10,
                        info = paste("Year", years[i], "with params", 
                                   paste(names(params), params, collapse = ", ")))
        }
        
        # Check all cases are non-negative
        expect_true(all(result$cases >= 0))
        
        # Check dates are properly formatted
        expect_true(all(class(result$date) == "Date"))
    }
})

test_that("disagg_annual_cases_to_daily handles leap years correctly", {
    # Test with leap year (2020) and non-leap year (2021)
    annual_cases <- c(366, 365)  # Use day counts as cases for easy verification
    years <- c(2020, 2021)
    
    result <- disagg_annual_cases_to_daily(
        annual_cases = annual_cases,
        years = years,
        a1 = 0.5,
        b1 = 0.5,
        a2 = 0,
        b2 = 0
    )
    
    # Check correct number of days
    expect_equal(nrow(result[result$year == 2020, ]), 366)
    expect_equal(nrow(result[result$year == 2021, ]), 365)
    
    # Check total preservation
    expect_equal(sum(result[result$year == 2020, "cases"]), 366, tolerance = 1e-10)
    expect_equal(sum(result[result$year == 2021, "cases"]), 365, tolerance = 1e-10)
})

test_that("disagg_annual_cases_to_daily handles edge cases", {
    # Test with single year
    result <- disagg_annual_cases_to_daily(
        annual_cases = 1000,
        years = 2023,
        a1 = 0.5, b1 = 0.5, a2 = 0.2, b2 = 0.2
    )
    expect_equal(sum(result$cases), 1000, tolerance = 1e-10)
    
    # Test with zero cases
    result <- disagg_annual_cases_to_daily(
        annual_cases = c(100, 0, 200),
        years = c(2021, 2022, 2023),
        a1 = 0.5, b1 = 0.5, a2 = 0, b2 = 0
    )
    # Should skip year with zero cases
    expect_false(2022 %in% result$year)
    expect_equal(sum(result[result$year == 2021, "cases"]), 100, tolerance = 1e-10)
    expect_equal(sum(result[result$year == 2023, "cases"]), 200, tolerance = 1e-10)
    
    # Test with NA cases
    result <- disagg_annual_cases_to_daily(
        annual_cases = c(100, NA, 200),
        years = c(2021, 2022, 2023),
        a1 = 0.5, b1 = 0.5, a2 = 0, b2 = 0
    )
    # Should skip year with NA cases
    expect_false(2022 %in% result$year)
    
    # Test error on mismatched lengths
    expect_error(
        disagg_annual_cases_to_daily(
            annual_cases = c(100, 200),
            years = c(2021, 2022, 2023),
            a1 = 0, b1 = 0, a2 = 0, b2 = 0
        ),
        "same length"
    )
})

test_that("est_initial_R_location calculates R compartment correctly", {
    # Create test data with known properties
    # Simple case: 1000 infections 100 days ago
    cases <- 100  # Reported cases
    dates <- Sys.Date() - 100
    population <- 100000
    t0 <- Sys.Date()
    
    # Parameters that lead to 1000 infections from 100 cases
    # I = (C × χ) / (ρ × σ) = (100 × 0.5) / (0.1 × 0.05) = 10000
    sigma <- 0.05
    rho <- 0.1
    chi <- 0.5
    
    # Disease progression parameters
    iota <- 0.714      # ~1.4 day incubation
    gamma_1 <- 0.2     # ~5 day symptomatic recovery
    gamma_2 <- 0.67    # ~1.5 day asymptomatic recovery
    
    # Waning immunity
    epsilon <- 0.0004  # ~4 year half-life
    
    result <- est_initial_R_location(
        cases = cases,
        dates = dates,
        population = population,
        t0 = t0,
        epsilon = epsilon,
        sigma = sigma,
        rho = rho,
        chi = chi,
        iota = iota,
        gamma_1 = gamma_1,
        gamma_2 = gamma_2
    )
    
    # Check result is numeric and reasonable
    expect_true(is.numeric(result))
    expect_true(result >= 0)
    expect_true(result <= population)
    
    # Calculate expected value
    infections <- (cases * chi) / (rho * sigma)  # 10000
    # Total duration to recovery
    gamma_eff_inv <- sigma * (1/gamma_1) + (1 - sigma) * (1/gamma_2)
    total_duration <- 1/iota + gamma_eff_inv
    # Time since recovery
    time_since_recovery <- 100 - total_duration
    # Expected R with waning
    expected_R <- infections * exp(-epsilon * time_since_recovery)
    
    # Should be close to expected (within 1% due to rounding)
    expect_equal(result, expected_R, tolerance = 0.01)
})

test_that("est_initial_R_location handles multiple time points", {
    # Multiple infection events
    cases <- c(100, 200, 150)
    dates <- Sys.Date() - c(365, 180, 30)  # 1 year, 6 months, 1 month ago
    population <- 100000
    t0 <- Sys.Date()
    
    result <- est_initial_R_location(
        cases = cases,
        dates = dates,
        population = population,
        t0 = t0,
        epsilon = 0.0004,
        sigma = 0.01,
        rho = 0.1,
        chi = 0.5,
        iota = 0.714,
        gamma_1 = 0.2,
        gamma_2 = 0.67
    )
    
    expect_true(is.numeric(result))
    expect_true(result >= 0)
    expect_true(result <= population)
})

test_that("est_initial_R_location excludes infections still in progress", {
    # Recent infection that hasn't completed recovery
    cases <- 100
    dates <- Sys.Date() - 2  # Just 2 days ago
    population <- 100000
    t0 <- Sys.Date()
    
    result <- est_initial_R_location(
        cases = cases,
        dates = dates,
        population = population,
        t0 = t0,
        epsilon = 0.0004,
        sigma = 0.01,
        rho = 0.1,
        chi = 0.5,
        iota = 0.714,      # ~1.4 day incubation
        gamma_1 = 0.2,     # ~5 day recovery
        gamma_2 = 0.67
    )
    
    # Should be 0 because infection hasn't completed recovery yet
    expect_equal(result, 0)
})

test_that("fit_beta_safe handles edge cases", {
    # Test with uniform data
    x_uniform <- rep(0.5, 100)
    result <- fit_beta_safe(x_uniform)
    expect_null(result)
    
    # Test with valid data
    set.seed(123)
    x_valid <- rbeta(100, 2, 5)
    result <- fit_beta_safe(x_valid)
    expect_true(!is.null(result))
    expect_true("shape1" %in% names(result))
    expect_true("shape2" %in% names(result))
    expect_true(result$shape1 > 0)
    expect_true(result$shape2 > 0)
    
    # Test with extreme values
    x_extreme <- c(0, 0.5, 1)
    result <- fit_beta_safe(x_extreme)
    # Should handle by adjusting to (0,1)
    if (!is.null(result)) {
        expect_true(result$shape1 > 0)
        expect_true(result$shape2 > 0)
    }
    
    # Test with too few values
    result <- fit_beta_safe(0.5)
    expect_null(result)
    
    # Test with NA values
    x_with_na <- c(0.3, NA, 0.5, NA, 0.7)
    result <- fit_beta_safe(x_with_na)
    if (!is.null(result)) {
        expect_true(result$shape1 > 0)
        expect_true(result$shape2 > 0)
    }
})
