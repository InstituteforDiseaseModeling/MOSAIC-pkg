# Tests for get_npe_observed_data function
# Current API: get_npe_observed_data(config, aggregate_locations = FALSE, verbose = TRUE)

test_that("get_npe_observed_data handles single location vector data", {
    config <- list(
        reported_cases = c(10, 15, 22, 18, 12),
        location_name = "ETH"
    )

    result <- get_npe_observed_data(config, verbose = FALSE)

    expect_true(is.data.frame(result))
    expect_true("cases" %in% names(result))
    expect_true(nrow(result) > 0)
})

test_that("get_npe_observed_data handles multi-location matrix data", {
    config <- list(
        reported_cases = matrix(
            c(10, 15, 22,  # Location 1
              5,  8, 12),  # Location 2
            nrow = 2, byrow = TRUE
        ),
        reported_deaths = matrix(
            c(1, 2, 3,
              0, 1, 1),
            nrow = 2, byrow = TRUE
        ),
        location_name = c("ETH", "KEN")
    )

    result <- get_npe_observed_data(config, verbose = FALSE)

    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("j" %in% names(result))
    expect_true("cases" %in% names(result))
})

test_that("get_npe_observed_data uses numeric indices when no location names provided", {
    config <- list(
        reported_cases = matrix(1:6, nrow = 2),
        reported_deaths = matrix(1:6, nrow = 2)
    )

    result <- get_npe_observed_data(config, verbose = FALSE)

    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    # Location indices should be present
    expect_true("j" %in% names(result))
})

test_that("get_npe_observed_data errors on missing reported_cases", {
    config <- list(
        location_name = "ETH"
    )

    expect_error(
        get_npe_observed_data(config, verbose = FALSE)
    )
})

test_that("get_npe_observed_data handles aggregate_locations parameter", {
    config <- list(
        reported_cases = matrix(
            c(10, 15, 22,
              5,  8, 12),
            nrow = 2, byrow = TRUE
        ),
        reported_deaths = matrix(
            c(1, 2, 3,
              0, 1, 1),
            nrow = 2, byrow = TRUE
        ),
        location_name = c("ETH", "KEN")
    )

    # Default (no aggregation)
    result_default <- get_npe_observed_data(config, aggregate_locations = FALSE, verbose = FALSE)
    expect_true(is.data.frame(result_default))

    # With aggregation
    result_agg <- get_npe_observed_data(config, aggregate_locations = TRUE, verbose = FALSE)
    expect_true(is.data.frame(result_agg))
})
