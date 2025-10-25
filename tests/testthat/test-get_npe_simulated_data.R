# Tests for get_npe_simulated_data function

test_that("get_npe_simulated_data extracts single location data", {
    # Mock LASER result for single location
    laser_result <- list(
        results = list(
            expected_cases = c(8, 8, 8, 8, 7, 6, 5)
        ),
        params = list(
            location_name = "ETH"
        )
    )

    result <- get_npe_simulated_data(laser_result)

    expect_equal(nrow(result), 7)
    expect_equal(ncol(result), 3)
    expect_equal(unique(result$j), "ETH")
    expect_equal(result$t, 1:7)
    expect_equal(result$cases, c(8, 8, 8, 8, 7, 6, 5))
})

test_that("get_npe_simulated_data extracts multi-location data", {
    # Mock LASER result for multiple locations
    laser_result <- list(
        results = list(
            expected_cases = matrix(
                c(10, 15, 22,  # Location 1
                  5,  8, 12),  # Location 2
                nrow = 2, byrow = TRUE
            )
        )
    )

    laser_result$params <- list(location_name = c("ETH", "KEN"))

    result <- get_npe_simulated_data(laser_result)

    expect_equal(nrow(result), 6)  # 2 locations * 3 time points
    expect_equal(ncol(result), 3)
    expect_equal(sort(unique(result$j)), c("ETH", "KEN"))
    expect_equal(result$cases[result$j == "ETH"], c(10, 15, 22))
    expect_equal(result$cases[result$j == "KEN"], c(5, 8, 12))
})

test_that("get_npe_simulated_data includes deaths when requested", {
    laser_result <- list(
        results = list(
            expected_cases = matrix(c(10, 15, 5, 8), nrow = 2),
            disease_deaths = matrix(c(1, 2, 0, 1), nrow = 2)
        )
    )

    config <- list(
        location_name = c("ETH", "KEN")
    )

    result <- get_npe_simulated_data(laser_result, config, include_deaths = TRUE)

    expect_equal(ncol(result), 4)
    expect_true("deaths" %in% names(result))
    expect_equal(result$deaths[result$j == "ETH"], c(1, 2))
    expect_equal(result$deaths[result$j == "KEN"], c(0, 1))
})

test_that("get_npe_simulated_data extracts location codes from laser_result params", {
    laser_result <- list(
        results = list(
            expected_cases = matrix(1:6, nrow = 2)
        ),
        params = list(
            location_name = c("ETH", "KEN")
        )
    )

    result <- get_npe_simulated_data(laser_result, verbose = FALSE)

    expect_equal(sort(unique(result$j)), c("ETH", "KEN"))
})

test_that("get_npe_simulated_data uses numeric indices when no location info available", {
    laser_result <- list(
        results = list(
            expected_cases = matrix(1:6, nrow = 2)
        )
    )

    result <- get_npe_simulated_data(laser_result, verbose = FALSE)

    expect_equal(sort(unique(result$j)), c("1", "2"))
})


test_that("get_npe_simulated_data handles NA/Inf values", {
    laser_result <- list(
        results = list(
            expected_cases = c(10, NA, 22, NaN, Inf)
        )
    )

    laser_result$params <- list(location_name = "ETH")

    result <- get_npe_simulated_data(laser_result)

    expect_equal(result$cases, c(10, 0, 22, 0, 0))
    expect_false(any(is.na(result$cases)))
    expect_false(any(!is.finite(result$cases)))
})

test_that("get_npe_simulated_data validates output structure", {
    laser_result <- list(
        results = list(
            expected_cases = c(10, 15, 22)
        )
    )

    result <- get_npe_simulated_data(laser_result, validate = TRUE)

    expect_true(is.data.frame(result))
    expect_true(is.character(result$j))
    expect_true(is.numeric(result$t))
    expect_true(is.numeric(result$cases))
})

test_that("get_npe_simulated_data errors on NULL laser_result", {
    expect_error(
        get_npe_simulated_data(NULL),
        "cannot be NULL"
    )
})

test_that("get_npe_simulated_data errors on missing results field", {
    laser_result <- list(
        params = list()
    )

    expect_error(
        get_npe_simulated_data(laser_result),
        "must contain 'results'"
    )
})

test_that("get_npe_simulated_data errors on missing expected_cases", {
    laser_result <- list(
        results = list(
            disease_deaths = c(1, 2, 3)
        )
    )

    expect_error(
        get_npe_simulated_data(laser_result),
        "expected_cases"
    )
})

test_that("get_npe_simulated_data output matches get_npe_observed_data format", {
    # Create matching config
    config <- list(
        reported_cases = matrix(c(10, 15, 5, 8), nrow = 2),
        location_name = c("ETH", "KEN")
    )

    # Create matching laser result
    laser_result <- list(
        results = list(
            expected_cases = matrix(c(10, 15, 5, 8), nrow = 2)
        )
    )

    # Add params to laser_result to match config
    laser_result$params <- list(location_name = c("ETH", "KEN"))

    # Get both outputs
    observed_data <- get_npe_observed_data(config, verbose = FALSE)
    simulated_data <- get_npe_simulated_data(laser_result, verbose = FALSE)

    # Check structure is identical
    expect_equal(names(observed_data), names(simulated_data))
    expect_equal(class(observed_data), class(simulated_data))
    expect_equal(dim(observed_data), dim(simulated_data))
    expect_equal(observed_data$j, simulated_data$j)
    expect_equal(observed_data$t, simulated_data$t)
})