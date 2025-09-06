library(MOSAIC)

# Source the function directly for testing to get the latest version
source("../../R/convert_config_to_dataframe.R")

test_that("convert_config_to_dataframe works with default config", {
     # Test with default config
     df <- convert_config_to_dataframe(config_default)
     
     # Check it returns a dataframe
     expect_s3_class(df, "data.frame")
     
     # Check it's a single row
     expect_equal(nrow(df), 1)
     
     # Check it has many columns (one for each parameter/location combination)
     expect_gt(ncol(df), 100)
     
     # Check scalar parameters are preserved
     expect_true("seed" %in% names(df))
     expect_true("phi_1" %in% names(df))
     
     # Check location-specific parameters are expanded
     location_names <- config_default$location_name
     expect_true(paste0("beta_j0_env_", location_names[1]) %in% names(df))
     expect_true(paste0("S_j_initial_", location_names[1]) %in% names(df))
})

test_that("convert_config_to_dataframe handles NULL config", {
     expect_error(convert_config_to_dataframe(NULL), "config argument is required")
})

test_that("convert_config_to_dataframe handles non-list input", {
     expect_error(convert_config_to_dataframe("not a list"), "config must be a list")
})

test_that("convert_config_to_dataframe works with sampled config", {
     skip_if_not(exists("sample_parameters"))
     
     # Create sampled config with minimal sampling
     set_root_directory(system.file(package = "MOSAIC"))
     suppressMessages({
          PATHS <- get_paths()
          config_sampled <- sample_parameters(
               PATHS, 
               seed = 123,
               sample_beta_j0_env = TRUE
          )
     })
     
     # Convert to dataframe
     df <- convert_config_to_dataframe(config_sampled)
     
     # Check basic properties
     expect_s3_class(df, "data.frame")
     expect_equal(nrow(df), 1)
     
     # Check sampled parameter is present with location suffixes
     location_names <- config_sampled$location_name
     if (!is.null(location_names) && length(location_names) > 0) {
          expect_true(paste0("beta_j0_env_", location_names[1]) %in% names(df))
     }
})

test_that("convert_config_to_dataframe works with single location config", {
     skip_if_not(exists("get_location_config"))
     
     # Get single location config
     suppressMessages({
          config_eth <- get_location_config(iso = "ETH")
     })
     
     # Convert to dataframe
     df <- convert_config_to_dataframe(config_eth)
     
     # Check basic properties
     expect_s3_class(df, "data.frame")
     expect_equal(nrow(df), 1)
     
     # For single location, parameters should not have location suffixes
     expect_true("beta_j0_env" %in% names(df))
     expect_true("seed" %in% names(df))
})

test_that("convert_config_to_dataframe handles empty config", {
     empty_config <- list()
     df <- convert_config_to_dataframe(empty_config)
     
     # Should return empty dataframe with warning
     expect_s3_class(df, "data.frame")
     expect_equal(nrow(df), 0)
     expect_equal(ncol(df), 0)
})

test_that("convert_config_to_dataframe filters parameters correctly", {
     # Create config with extra parameters not in params_keep
     test_config <- list(
          seed = 123,
          phi_1 = 0.5,
          extra_param = "should be ignored",
          location_name = c("A", "B"),
          beta_j0_env = c(0.1, 0.2)
     )
     
     df <- convert_config_to_dataframe(test_config)
     
     # Check kept parameters are present
     expect_true("seed" %in% names(df))
     expect_true("phi_1" %in% names(df))
     expect_true("beta_j0_env_A" %in% names(df))
     expect_true("beta_j0_env_B" %in% names(df))
     
     # Check extra parameter is not present
     expect_false("extra_param" %in% names(df))
})