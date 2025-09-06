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

test_that("est_initial_R output structure matches priors_default", {
    skip("Requires full data setup - integration test")
    
    # This would be an integration test requiring actual data files
    # Create minimal test configuration
    PATHS <- list(
        DATA_PROCESSED = tempdir()
    )
    
    # Create minimal test data files
    who_data <- data.frame(
        iso_code = c("ETH", "ETH", "KEN", "KEN"),
        year = c(2022, 2023, 2022, 2023),
        cases_total = c(100, 150, 200, 250),
        stringsAsFactors = FALSE
    )
    
    pop_data <- data.frame(
        iso_code = c("ETH", "ETH", "KEN", "KEN"),
        date = c("2023-01-01", "2024-01-01", "2023-01-01", "2024-01-01"),
        total_population = c(120000000, 125000000, 54000000, 55000000),
        stringsAsFactors = FALSE
    )
    
    # Save test data
    dir.create(file.path(PATHS$DATA_PROCESSED, "WHO", "annual"), recursive = TRUE)
    dir.create(file.path(PATHS$DATA_PROCESSED, "population"), recursive = TRUE)
    
    write.csv(who_data, 
             file.path(PATHS$DATA_PROCESSED, "WHO/annual/who_afro_annual_1949_2024.csv"),
             row.names = FALSE)
    write.csv(pop_data,
             file.path(PATHS$DATA_PROCESSED, "population/population_data.csv"),
             row.names = FALSE)
    
    # Create minimal priors
    priors <- list(
        parameters_global = list(
            epsilon = list(distribution = "uniform", parameters = list(min = 0.0003, max = 0.0005)),
            sigma = list(distribution = "uniform", parameters = list(min = 0.005, max = 0.015)),
            iota = list(distribution = "uniform", parameters = list(min = 0.5, max = 1)),
            gamma_1 = list(distribution = "uniform", parameters = list(min = 0.14, max = 0.33)),
            gamma_2 = list(distribution = "uniform", parameters = list(min = 0.5, max = 1))
        ),
        parameters_location = list(
            rho = list(
                parameters = list(
                    location = list(
                        ETH = list(distribution = "uniform", parameters = list(min = 0.05, max = 0.15)),
                        KEN = list(distribution = "uniform", parameters = list(min = 0.05, max = 0.15))
                    )
                )
            ),
            chi = list(
                parameters = list(
                    location = list(
                        ETH = list(distribution = "uniform", parameters = list(min = 0.4, max = 0.6)),
                        KEN = list(distribution = "uniform", parameters = list(min = 0.4, max = 0.6))
                    )
                )
            )
        )
    )
    
    # Create minimal config
    config <- list(
        location_codes = c("ETH", "KEN")
    )
    
    # Run function
    result <- est_initial_R(
        PATHS = PATHS,
        priors = priors,
        config = config,
        n_samples = 100,
        t0 = as.Date("2024-01-01"),
        disaggregate = FALSE,
        verbose = FALSE
    )
    
    # Check structure
    expect_true("metadata" %in% names(result))
    expect_true("parameters_location" %in% names(result))
    expect_true("prop_R_initial" %in% names(result$parameters_location))
    expect_true("parameters" %in% names(result$parameters_location$prop_R_initial))
    expect_true("location" %in% names(result$parameters_location$prop_R_initial$parameters))
    
    # Check metadata
    expect_true("initial_conditions_R" %in% names(result$metadata))
    expect_equal(result$metadata$initial_conditions_R$method, "est_initial_R")
    expect_equal(result$metadata$initial_conditions_R$n_samples, 100)
    
    # Check location-specific results
    for (loc in c("ETH", "KEN")) {
        loc_result <- result$parameters_location$prop_R_initial$parameters$location[[loc]]
        if (!is.na(loc_result$shape1)) {
            expect_true("shape1" %in% names(loc_result))
            expect_true("shape2" %in% names(loc_result))
            expect_true("metadata" %in% names(loc_result))
            expect_true(loc_result$shape1 > 0)
            expect_true(loc_result$shape2 > 0)
        }
    }
    
    # Clean up
    unlink(file.path(PATHS$DATA_PROCESSED), recursive = TRUE)
})

test_that("est_initial_R handles disaggregate parameter correctly", {
    skip("Requires full data setup - integration test")
    
    # Similar setup as above but test both disaggregate = TRUE and FALSE
    # Would verify that:
    # 1. disaggregate = TRUE uses Fourier coefficients
    # 2. disaggregate = FALSE uses mid-year point estimate
    # 3. Results differ but are both reasonable
})

test_that("Surveillance cascade math is correct", {
    # Test the fundamental equation: I = (C × χ) / (ρ × σ)
    
    # Test case 1: Endemic setting
    C_endemic <- 1000  # Reported cases
    chi_endemic <- 0.5  # 50% diagnostic positivity
    rho_endemic <- 0.1  # 10% reporting rate
    sigma_endemic <- 0.01  # 1% symptomatic
    
    I_endemic <- (C_endemic * chi_endemic) / (rho_endemic * sigma_endemic)
    expect_equal(I_endemic, 500000)  # 500x multiplier
    
    # Test case 2: Epidemic setting
    C_epidemic <- 1000
    chi_epidemic <- 0.75  # 75% diagnostic positivity
    rho_epidemic <- 0.1   # 10% reporting rate
    sigma_epidemic <- 0.3  # 30% symptomatic
    
    I_epidemic <- (C_epidemic * chi_epidemic) / (rho_epidemic * sigma_epidemic)
    expect_equal(I_epidemic, 25000)  # 25x multiplier
    
    # Verify the multiplier difference
    multiplier_ratio <- I_endemic / I_epidemic
    expect_equal(multiplier_ratio, 20)  # 20-fold difference
})

test_that("Waning immunity follows exponential decay", {
    # Test exponential decay calculation
    epsilon <- 0.0004  # ~4 year half-life
    
    # Calculate half-life
    half_life_days <- log(2) / epsilon
    expect_equal(half_life_days, 1732.868, tolerance = 0.01)  # ~4.7 years
    
    # Test decay over time
    initial_R <- 10000
    
    # After 1 year
    R_1year <- initial_R * exp(-epsilon * 365)
    expect_equal(R_1year / initial_R, exp(-epsilon * 365))
    
    # After half-life
    R_halflife <- initial_R * exp(-epsilon * half_life_days)
    expect_equal(R_halflife / initial_R, 0.5, tolerance = 1e-10)
    
    # After 10 years
    R_10years <- initial_R * exp(-epsilon * 3650)
    expect_true(R_10years < initial_R * 0.25)  # Should be much less than 25%
})

test_that("Variance inflation preserves mean while increasing variance", {
    # Test the simplified variance inflation formula
    
    # Test multiple cases
    test_cases <- list(
        list(alpha = 10, beta = 30, inflation = 2),
        list(alpha = 50, beta = 4950, inflation = 3),
        list(alpha = 100, beta = 900, inflation = 4),
        list(alpha = 2, beta = 8, inflation = 2),
        list(alpha = 100, beta = 900, inflation = 100)  # High inflation test
    )
    
    for (case in test_cases) {
        alpha_orig <- case$alpha
        beta_orig <- case$beta
        variance_inflation <- case$inflation
        
        mean_orig <- alpha_orig / (alpha_orig + beta_orig)
        var_orig <- (alpha_orig * beta_orig) / ((alpha_orig + beta_orig)^2 * (alpha_orig + beta_orig + 1))
        
        # Apply simplified formula: sum_new = sum_original / k
        sum_original <- alpha_orig + beta_orig
        sum_new <- sum_original / variance_inflation
        
        # Minimal constraint
        sum_new <- max(sum_new, 0.1)
        
        alpha_new <- mean_orig * sum_new
        beta_new <- (1 - mean_orig) * sum_new
        
        # Check mean is preserved
        mean_new <- alpha_new / (alpha_new + beta_new)
        expect_equal(mean_new, mean_orig, tolerance = 1e-10,
                    info = sprintf("Mean preservation for Beta(%g,%g) with inflation %g", 
                                  alpha_orig, beta_orig, variance_inflation))
        
        # Check variance is increased appropriately
        var_new <- (alpha_new * beta_new) / ((alpha_new + beta_new)^2 * (alpha_new + beta_new + 1))
        variance_ratio <- var_new / var_orig
        
        # For high precision priors, ratio should be close to target
        # For low precision priors, it may hit the constraint
        if (sum_new > 0.1) {
            # Not constrained - should be close to target
            # Allow some tolerance as the relationship is approximate
            expect_true(variance_ratio > variance_inflation * 0.8,
                       info = sprintf("Variance increase for Beta(%g,%g) with inflation %g: got %.1fx", 
                                     alpha_orig, beta_orig, variance_inflation, variance_ratio))
        } else {
            # Hit constraint - just check it increased
            expect_true(variance_ratio > 1,
                       info = sprintf("Variance increased despite constraint for Beta(%g,%g)", 
                                     alpha_orig, beta_orig))
        }
    }
    
    # Test with different parameters
    test_cases <- list(
        list(alpha = 2, beta = 8),
        list(alpha = 50, beta = 50),
        list(alpha = 1, beta = 9)
    )
    
    for (params in test_cases) {
        alpha_orig <- params$alpha
        beta_orig <- params$beta
        mean_orig <- alpha_orig / (alpha_orig + beta_orig)
        
        # Apply inflation
        variance_inflation <- 2
        sum_original <- alpha_orig + beta_orig
        sum_new <- sum_original / sqrt(variance_inflation)
        
        alpha_new <- mean_orig * sum_new
        beta_new <- (1 - mean_orig) * sum_new
        
        # Ensure minimum values
        alpha_new <- max(alpha_new, 0.5)
        beta_new <- max(beta_new, 0.5)
        
        # Check mean preservation (allowing small deviation due to minimum value constraint)
        mean_new <- alpha_new / (alpha_new + beta_new)
        expect_equal(mean_new, mean_orig, tolerance = 0.05)
    }
})