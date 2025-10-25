# Tests for get_npe_observed_data function

test_that("get_npe_observed_data handles single location vector data", {
    config <- list(
        reported_cases = c(10, 15, 22, 18, 12),
        location_name = "ETH"
    )

    result <- get_npe_observed_data(config)

    expect_equal(nrow(result), 5)
    expect_equal(ncol(result), 3)
    expect_equal(unique(result$j), "ETH")
    expect_equal(result$t, 1:5)
    expect_equal(result$cases, c(10, 15, 22, 18, 12))
})

test_that("get_npe_observed_data handles multi-location matrix data", {
    config <- list(
        reported_cases = matrix(
            c(10, 15, 22,  # Location 1
              5,  8, 12),  # Location 2
            nrow = 2, byrow = TRUE
        ),
        location_name = c("ETH", "KEN")
    )

    result <- get_npe_observed_data(config)

    expect_equal(nrow(result), 6)  # 2 locations * 3 time points
    expect_equal(ncol(result), 3)
    expect_equal(sort(unique(result$j)), c("ETH", "KEN"))
    expect_equal(result$cases[result$j == "ETH"], c(10, 15, 22))
    expect_equal(result$cases[result$j == "KEN"], c(5, 8, 12))
})

test_that("get_npe_observed_data includes deaths when requested", {
    config <- list(
        reported_cases = matrix(c(10, 15, 5, 8), nrow = 2),
        reported_deaths = matrix(c(1, 2, 0, 1), nrow = 2),
        location_name = c("ETH", "KEN")
    )

    result <- get_npe_observed_data(config, include_deaths = TRUE)

    expect_equal(ncol(result), 4)
    expect_true("deaths" %in% names(result))
    expect_equal(result$deaths[result$j == "ETH"], c(1, 2))
    expect_equal(result$deaths[result$j == "KEN"], c(0, 1))
})

test_that("get_npe_observed_data uses numeric indices when no location names provided", {
    config <- list(
        reported_cases = matrix(1:6, nrow = 2)
    )

    result <- get_npe_observed_data(config, verbose = FALSE)

    expect_equal(sort(unique(result$j)), c("1", "2"))
})

test_that("get_npe_observed_data handles custom time indices", {
    config <- list(
        reported_cases = c(10, 15, 22, 18),
        location_name = "ETH"
    )

    result <- get_npe_observed_data(config, time_index = 10:13)

    expect_equal(result$t, 10:13)
})

test_that("get_npe_observed_data handles NA values", {
    config <- list(
        reported_cases = c(10, NA, 22, NaN, Inf),
        location_name = "ETH"
    )

    result <- get_npe_observed_data(config)

    expect_equal(result$cases, c(10, 0, 22, 0, 0))
    expect_false(any(is.na(result$cases)))
    expect_false(any(!is.finite(result$cases)))
})

test_that("get_npe_observed_data validates output structure", {
    config <- list(
        reported_cases = c(10, 15, 22)
    )

    result <- get_npe_observed_data(config, validate = TRUE)

    expect_true(is.data.frame(result))
    expect_true(is.character(result$j))
    expect_true(is.numeric(result$t))
    expect_true(is.numeric(result$cases))
})

test_that("get_npe_observed_data errors on missing reported_cases", {
    config <- list(
        location_name = "ETH"
    )

    expect_error(
        get_npe_observed_data(config),
        "must contain 'reported_cases'"
    )
})

test_that("get_npe_observed_data warns on mismatched location codes", {
    config <- list(
        reported_cases = matrix(1:6, nrow = 2),
        location_name = c("ETH")  # Only 1 name for 2 locations
    )

    expect_warning(
        result <- get_npe_observed_data(config),
        "doesn't match number of locations"
    )

    # Should fall back to numeric indices
    expect_equal(sort(unique(result$j)), c("1", "2"))
})

test_that("get_npe_observed_data extracts from different config fields", {
    # Test location_name
    config1 <- list(
        reported_cases = matrix(1:4, nrow = 2),
        location_name = c("ETH", "KEN")
    )
    result1 <- get_npe_observed_data(config1, verbose = FALSE)
    expect_equal(sort(unique(result1$j)), c("ETH", "KEN"))

    # Test iso_code
    config2 <- list(
        reported_cases = matrix(1:4, nrow = 2),
        iso_code = c("ETH", "KEN")
    )
    result2 <- get_npe_observed_data(config2, verbose = FALSE)
    expect_equal(sort(unique(result2$j)), c("ETH", "KEN"))

    # Test location_code
    config3 <- list(
        reported_cases = matrix(1:4, nrow = 2),
        location_code = c("LOC1", "LOC2")
    )
    result3 <- get_npe_observed_data(config3, verbose = FALSE)
    expect_equal(sort(unique(result3$j)), c("LOC1", "LOC2"))
})

test_that("get_npe_observed_data warns about deprecated location_codes parameter", {
    config <- list(
        reported_cases = c(10, 15, 22),
        location_name = "ETH"
    )

    expect_warning(
        result <- get_npe_observed_data(config, location_codes = c("KEN")),
        "deprecated"
    )

    # Should use config location, not the provided one
    expect_equal(unique(result$j), "ETH")
})

test_that("get_npe_observed_data handles negative values with warning", {
    config <- list(
        reported_cases = c(10, -5, 22),
        location_name = "ETH"
    )

    expect_warning(
        result <- get_npe_observed_data(config, validate = TRUE),
        "Negative case counts"
    )

    expect_equal(result$cases[2], 0)
})