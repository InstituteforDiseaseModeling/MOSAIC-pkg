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

test_that("convert_config_to_matrix expands a length-nL alpha_1 to alpha_1_<ISO>", {
     # alpha_1 is now PER-LOCATION (v15.16/v4.7). A length-nL alpha_1 must expand
     # to per-ISO columns; a scalar alpha_1 (national/legacy) stays a single
     # global-style column. alpha_2 stays scalar.
     vec_cfg <- list(
          seed = 1,
          location_name = c("ETH", "KEN"),
          N_j_initial = c(1e6, 5e5),
          alpha_1 = c(0.27, 0.31),
          alpha_2 = 0.5
     )
     res_vec <- convert_config_to_matrix(vec_cfg)
     expect_true("alpha_1_ETH" %in% names(res_vec))
     expect_true("alpha_1_KEN" %in% names(res_vec))
     expect_false("alpha_1" %in% names(res_vec))
     expect_equal(unname(res_vec["alpha_1_ETH"]), 0.27)
     expect_equal(unname(res_vec["alpha_1_KEN"]), 0.31)
     # alpha_2 remains a single scalar column
     expect_true("alpha_2" %in% names(res_vec))

     # Scalar alpha_1 -> single column (back-compat)
     sc_cfg <- vec_cfg; sc_cfg$alpha_1 <- 0.27
     res_sc <- convert_config_to_matrix(sc_cfg)
     expect_true("alpha_1" %in% names(res_sc))
     expect_false("alpha_1_ETH" %in% names(res_sc))
})

test_that("convert_matrix_to_config round-trips per-ISO alpha_1 (seed carries length-nL)", {
     # The round-trip writes alpha_1_<ISO> back into config$alpha_1[idx] ONLY if
     # the seed config's alpha_1 already has length >= idx (a scalar seed silently
     # drops idx 2..nL). config_default v4.7 ships a length-nL alpha_1, so the
     # round-trip is robust. Mimic that here with a length-nL seed.
     seed_cfg <- list(
          seed = 1,
          location_name = c("ETH", "KEN"),
          N_j_initial = c(1e6, 5e5),
          alpha_1 = c(0.27, 0.27),   # length-nL seed (D1)
          alpha_2 = 0.5
     )
     m <- convert_config_to_matrix(seed_cfg)
     # Calibration would update the per-ISO entries:
     m["alpha_1_ETH"] <- 0.22
     m["alpha_1_KEN"] <- 0.38
     out <- convert_matrix_to_config(m, config_base = seed_cfg)
     expect_equal(length(out$alpha_1), 2L)
     expect_equal(out$alpha_1[1], 0.22)
     expect_equal(out$alpha_1[2], 0.38)
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