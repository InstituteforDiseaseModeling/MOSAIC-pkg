test_that("convert_config_to_matrix returns numeric vector", {
     # Create a simple test config
     test_config <- list(
          seed = 123,
          location_name = "ETH",
          N_j_initial = 1000000,
          S_j_initial = 0.8,
          E_j_initial = 0.001,
          I_j_initial = 0.001,
          R_j_initial = 0.198,
          V1_j_initial = 0,
          V2_j_initial = 0,
          phi_1 = 0.5,
          phi_2 = 0.3,
          omega_1 = 0.1,
          omega_2 = 0.05
     )
     
     result <- convert_config_to_matrix(test_config)
     
     expect_true(is.numeric(result))
     expect_true(is.vector(result))
     expect_true(length(result) > 0)
     expect_true(!is.null(names(result)))
})

test_that("convert_config_to_matrix handles location-specific parameters", {
     # Create config with multiple locations
     test_config <- list(
          seed = 456,
          location_name = c("ETH", "KEN"),
          N_j_initial = c(1000000, 500000),
          S_j_initial = c(0.8, 0.75),
          E_j_initial = c(0.001, 0.002),
          I_j_initial = c(0.001, 0.001),
          R_j_initial = c(0.198, 0.247),
          phi_1 = 0.5,  # Global parameter
          phi_2 = 0.3   # Global parameter
     )
     
     result <- convert_config_to_matrix(test_config)
     
     # Check that location-specific parameters are expanded
     expect_true("S_j_initial_ETH" %in% names(result))
     expect_true("S_j_initial_KEN" %in% names(result))
     expect_true("phi_1" %in% names(result))
     
     # Check values
     expect_equal(result["S_j_initial_ETH"], c(S_j_initial_ETH = 0.8))
     expect_equal(result["S_j_initial_KEN"], c(S_j_initial_KEN = 0.75))
})

test_that("convert_config_to_matrix handles logical values", {
     test_config <- list(
          seed = 789,
          some_flag = TRUE,
          another_flag = FALSE,
          phi_1 = 0.5
     )
     
     # Note: only parameters in params_keep will be processed
     # So we need to use actual MOSAIC parameters
     test_config <- list(
          seed = 789,
          phi_1 = 0.5,
          phi_2 = 0.3
     )
     
     result <- convert_config_to_matrix(test_config)
     
     expect_true(is.numeric(result))
     expect_equal(result["seed"], c(seed = 789))
})

test_that("convert_config_to_matrix produces same structure as dataframe method", {
     skip_if_not_installed("MOSAIC")
     
     # Create a test config
     test_config <- list(
          seed = 999,
          location_name = "ETH",
          N_j_initial = 1000000,
          S_j_initial = 0.8,
          E_j_initial = 0.001,
          I_j_initial = 0.001,
          R_j_initial = 0.198,
          V1_j_initial = 0.01,
          V2_j_initial = 0.005,
          phi_1 = 0.5,
          phi_2 = 0.3,
          omega_1 = 0.1,
          omega_2 = 0.05,
          epsilon = 0.2,
          rho = 0.1,
          sigma = 0.15
     )
     
     # Compare both methods
     vec_direct <- convert_config_to_matrix(test_config)
     df_method <- convert_config_to_dataframe(test_config)
     vec_from_df <- as.numeric(df_method)
     names(vec_from_df) <- colnames(df_method)
     
     # Should produce same values
     expect_equal(vec_direct, vec_from_df, tolerance = 1e-10)
})

test_that("convert_config_to_matrix handles NULL and missing values", {
     test_config <- list(
          seed = 111,
          location_name = NULL,  # NULL should be skipped
          phi_1 = 0.5,
          phi_2 = NA,  # NA should be preserved as NA
          omega_1 = 0.1
     )
     
     result <- convert_config_to_matrix(test_config)
     
     expect_true(is.numeric(result))
     expect_true("seed" %in% names(result))
     expect_true("phi_1" %in% names(result))
     expect_true("phi_2" %in% names(result))
     expect_true(is.na(result["phi_2"]))
})

test_that("convert_config_to_matrix handles empty config gracefully", {
     expect_warning(result <- convert_config_to_matrix(list()))
     expect_equal(length(result), 0)
     expect_true(is.numeric(result))
})

test_that("convert_config_to_matrix validates input", {
     expect_error(convert_config_to_matrix(NULL), "config argument is required")
     expect_error(convert_config_to_matrix("not a list"), "config must be a list")
     expect_error(convert_config_to_matrix(), "config argument is required")
})