test_that("get_param_names works with config objects", {
    # Create a simple test config
    test_config <- list(
        seed = 123,
        location_name = c("ETH", "KEN"),
        N_j_initial = c(1000000, 500000),
        S_j_initial = c(0.8, 0.75),
        E_j_initial = c(0.001, 0.002),
        I_j_initial = c(0.001, 0.001),
        R_j_initial = c(0.198, 0.247),
        V1_j_initial = c(0.01, 0.005),
        V2_j_initial = c(0.005, 0.002),
        phi_1 = 0.5,
        phi_2 = 0.3,
        omega_1 = 0.1,
        omega_2 = 0.05
    )
    
    result <- get_param_names(test_config)
    
    # Check structure
    expect_true(is.list(result))
    expect_true(all(c("all", "global", "location") %in% names(result)))
    
    # Check global parameters
    expect_true(is.character(result$global))
    expect_true("seed" %in% result$global)
    expect_true("phi_1" %in% result$global)
    expect_true("phi_2" %in% result$global)
    
    # Check location parameters
    expect_true(is.list(result$location))
    expect_true("ETH" %in% names(result$location))
    expect_true("KEN" %in% names(result$location))
    expect_true("S_j_initial" %in% result$location$ETH)
    expect_true("S_j_initial" %in% result$location$KEN)
    
    # Check all parameters
    expect_true(length(result$all) > 0)
    expect_true(all(result$global %in% result$all))
    expect_true(all(unlist(result$location) %in% result$all))
})

test_that("get_param_names works with results matrix", {
    # Create a test results vector (like output from convert_config_to_matrix)
    test_results <- c(
        seed = 123,
        phi_1 = 0.5,
        phi_2 = 0.3,
        omega_1 = 0.1,
        omega_2 = 0.05,
        S_j_initial_ETH = 0.8,
        S_j_initial_KEN = 0.75,
        E_j_initial_ETH = 0.001,
        E_j_initial_KEN = 0.002,
        I_j_initial_ETH = 0.001,
        I_j_initial_KEN = 0.001
    )
    
    result <- get_param_names(test_results)
    
    # Check structure
    expect_true(is.list(result))
    expect_true(all(c("all", "global", "location") %in% names(result)))
    
    # Check global parameters
    expect_true("seed" %in% result$global)
    expect_true("phi_1" %in% result$global)
    expect_true("phi_2" %in% result$global)
    
    # Check location parameters
    expect_true("ETH" %in% names(result$location))
    expect_true("KEN" %in% names(result$location))
    expect_true("S_j_initial_ETH" %in% result$location$ETH)
    expect_true("S_j_initial_KEN" %in% result$location$KEN)
    
    # Check that location-specific params are not in global
    expect_false("S_j_initial_ETH" %in% result$global)
    expect_false("E_j_initial_KEN" %in% result$global)
})

test_that("get_param_names works with matrix input", {
    # Create test matrix
    test_matrix <- matrix(
        c(123, 0.5, 0.3, 0.8, 0.75),
        nrow = 1,
        dimnames = list(NULL, c("seed", "phi_1", "phi_2", "S_j_initial_ETH", "S_j_initial_KEN"))
    )
    
    result <- get_param_names(test_matrix)
    
    # Check structure
    expect_true(is.list(result))
    expect_true(all(c("all", "global", "location") %in% names(result)))
    
    # Check that it correctly identifies global vs location params
    expect_true("seed" %in% result$global)
    expect_true("phi_1" %in% result$global)
    expect_true("S_j_initial_ETH" %in% result$location$ETH)
    expect_true("S_j_initial_KEN" %in% result$location$KEN)
})

test_that("get_param_names handles single location configs", {
    # Create single location config
    test_config <- list(
        seed = 456,
        location_name = "ETH",
        N_j_initial = 1000000,
        S_j_initial = 0.8,
        E_j_initial = 0.001,
        phi_1 = 0.5,
        phi_2 = 0.3
    )
    
    result <- get_param_names(test_config)
    
    # With single location, location-specific params may be treated as global
    # depending on how they're stored (scalar vs vector)
    expect_true(is.list(result))
    expect_true(all(c("all", "global", "location") %in% names(result)))
    expect_true("seed" %in% result$global)
    expect_true("phi_1" %in% result$global)
})

test_that("get_param_names handles empty location list", {
    # Create config without location names
    test_config <- list(
        seed = 789,
        phi_1 = 0.5,
        phi_2 = 0.3,
        omega_1 = 0.1
    )
    
    result <- get_param_names(test_config)
    
    # All should be global parameters
    expect_true(is.list(result))
    expect_true(length(result$location) == 0)
    expect_true(all(c("seed", "phi_1", "phi_2", "omega_1") %in% result$global))
})

test_that("get_param_names validates input", {
    expect_error(get_param_names(NULL), "object argument is required")
    expect_error(get_param_names("not valid"), "Results object must have named")
    expect_error(get_param_names(), "object argument is required")
    
    # Test unnamed vector
    unnamed_vec <- c(1, 2, 3)
    expect_error(get_param_names(unnamed_vec), "Results object must have named")
})

test_that("get_param_names returns sorted results", {
    test_config <- list(
        zeta_2 = 0.1,
        alpha_1 = 0.2,
        phi_1 = 0.5,
        seed = 123,
        location_name = c("ZWE", "AGO"),
        S_j_initial = c(0.8, 0.75),
        E_j_initial = c(0.001, 0.002)
    )
    
    result <- get_param_names(test_config)
    
    # Check that results are sorted
    expect_equal(result$global, sort(result$global))
    expect_equal(result$all, sort(result$all))
    
    # Check location lists are sorted
    if (length(result$location) > 0) {
        for (iso in names(result$location)) {
            expect_equal(result$location[[iso]], sort(result$location[[iso]]))
        }
    }
})

test_that("get_param_names consistency between config and results matrix", {
    skip_if_not_installed("MOSAIC")
    
    # Create a test config
    test_config <- list(
        seed = 999,
        location_name = c("ETH", "KEN"),
        N_j_initial = c(1000000, 500000),
        S_j_initial = c(0.8, 0.75),
        E_j_initial = c(0.001, 0.002),
        I_j_initial = c(0.001, 0.001),
        R_j_initial = c(0.198, 0.247),
        V1_j_initial = c(0.01, 0.005),
        V2_j_initial = c(0.005, 0.002),
        phi_1 = 0.5,
        phi_2 = 0.3,
        omega_1 = 0.1,
        omega_2 = 0.05
    )
    
    # Get param names from config
    config_result <- get_param_names(test_config)
    
    # Convert to matrix and get param names
    results_vec <- convert_config_to_matrix(test_config)
    matrix_result <- get_param_names(results_vec)
    
    # Compare global parameters (should be identical)
    expect_setequal(config_result$global, matrix_result$global)
    
    # Both should have the same locations
    expect_setequal(names(config_result$location), names(matrix_result$location))
    
    # Total parameter count should be similar
    # (may differ due to expansion of location-specific params)
    expect_true(length(config_result$all) > 0)
    expect_true(length(matrix_result$all) > 0)
})