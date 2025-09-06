library(MOSAIC)

# Source the function directly for testing to get the latest version
source("../../R/get_location_config.R")

test_that("get_location_config works with default config and single location", {
     # Extract config for single location
     eth_config <- get_location_config(iso = "ETH")
     
     # Check it returns a list
     expect_type(eth_config, "list")
     
     # Check location_name is correctly subset
     expect_equal(eth_config$location_name, "ETH")
     
     # Check that location-specific parameters are subset
     expect_equal(length(eth_config$location_name), 1)
     
     # Check that scalar parameters are preserved
     if (!is.null(config_default$seed)) {
          expect_equal(eth_config$seed, config_default$seed)
     }
     
     # Check that initial compartment values are subset
     if (!is.null(config_default$N_j_initial)) {
          eth_idx <- which(config_default$location_name == "ETH")
          expect_equal(eth_config$N_j_initial, config_default$N_j_initial[eth_idx])
     }
})

test_that("get_location_config works with multiple locations", {
     # Extract config for multiple locations
     multi_config <- get_location_config(iso = c("ETH", "KEN", "UGA"))
     
     # Check structure
     expect_type(multi_config, "list")
     
     # Check location_name contains all requested locations
     expect_setequal(multi_config$location_name, c("ETH", "KEN", "UGA"))
     
     # Check length of location-specific parameters
     expect_equal(length(multi_config$location_name), 3)
     
     # Check initial compartments have correct length
     if (!is.null(multi_config$N_j_initial)) {
          expect_equal(length(multi_config$N_j_initial), 3)
     }
})

test_that("get_location_config handles missing iso argument", {
     expect_error(get_location_config(), "iso argument is required")
     expect_error(get_location_config(iso = NULL), "iso argument is required")
})

test_that("get_location_config handles invalid iso types", {
     expect_error(get_location_config(iso = 123), "iso must be a character vector")
     expect_error(get_location_config(iso = list("ETH")), "iso must be a character vector")
})

test_that("get_location_config handles empty iso vector", {
     expect_error(get_location_config(iso = character(0)), "iso must contain at least one location")
})

test_that("get_location_config handles non-existent locations", {
     expect_error(
          get_location_config(iso = "XXX"),
          "location\\(s\\) not found in config"
     )
     
     # Test with mix of valid and invalid
     expect_error(
          get_location_config(iso = c("ETH", "XXX")),
          "location\\(s\\) not found in config.*XXX"
     )
})

test_that("get_location_config handles case sensitivity", {
     # Should work with lowercase (converted to uppercase internally)
     eth_lower <- get_location_config(iso = "eth")
     eth_upper <- get_location_config(iso = "ETH")
     
     # Should produce identical results
     expect_equal(eth_lower$location_name, "ETH")
     expect_equal(eth_upper$location_name, "ETH")
})

test_that("get_location_config works with custom config object", {
     # Use default config as custom config
     custom_config <- config_default
     
     # Extract with custom config
     eth_config <- get_location_config(config = custom_config, iso = "ETH")
     
     # Check it works correctly
     expect_type(eth_config, "list")
     expect_equal(eth_config$location_name, "ETH")
})

test_that("get_location_config handles invalid config structure", {
     # Test with non-list config
     expect_error(
          get_location_config(config = "not a list", iso = "ETH"),
          "config must be a list"
     )
     
     # Test with missing location_name
     bad_config <- list(seed = 123, phi_1 = 0.5)
     expect_error(
          get_location_config(config = bad_config, iso = "ETH"),
          "config must contain 'location_name'"
     )
})

test_that("get_location_config preserves parameter structure", {
     eth_config <- get_location_config(iso = "ETH")
     
     # Check that global parameters are preserved
     global_params <- c("seed", "phi_1", "phi_2", "omega_1", "omega_2", 
                       "gamma_1", "gamma_2", "epsilon", "rho", "sigma",
                       "alpha_1", "alpha_2", "zeta_1", "zeta_2", "kappa",
                       "mobility_omega", "mobility_gamma", "iota",
                       "decay_days_short", "decay_days_long", 
                       "decay_shape_1", "decay_shape_2")
     
     for (param in global_params) {
          if (!is.null(config_default[[param]])) {
               expect_equal(eth_config[[param]], config_default[[param]],
                          info = paste("Global parameter", param, "should be preserved"))
          }
     }
})

test_that("get_location_config correctly subsets location parameters", {
     # Get indices for ETH and KEN
     eth_ken_config <- get_location_config(iso = c("ETH", "KEN"))
     
     # Find original indices
     orig_idx <- which(config_default$location_name %in% c("ETH", "KEN"))
     
     # Check location-specific parameters
     location_params <- c("N_j_initial", "S_j_initial", "E_j_initial", 
                         "I_j_initial", "R_j_initial", "V1_j_initial", "V2_j_initial",
                         "longitude", "latitude", "tau_i", "beta_j0_hum", "beta_j0_env",
                         "a_1_j", "a_2_j", "b_1_j", "b_2_j")
     
     for (param in location_params) {
          if (!is.null(config_default[[param]])) {
               expect_equal(eth_ken_config[[param]], config_default[[param]][orig_idx],
                          info = paste("Location parameter", param, "should be correctly subset"))
          }
     }
})

test_that("get_location_config correctly subsets time-varying parameters", {
     eth_config <- get_location_config(iso = "ETH")
     
     # Find ETH index
     eth_idx <- which(config_default$location_name == "ETH")
     
     # Check time-varying parameters (matrices)
     time_params <- c("b_jt", "d_jt", "nu_1_jt", "nu_2_jt", "mu_jt", "psi_jt",
                     "reported_cases", "reported_deaths")
     
     for (param in time_params) {
          if (!is.null(config_default[[param]]) && is.matrix(config_default[[param]])) {
               # Should be subset to single row (now a vector)
               if (nrow(config_default[[param]]) > 0) {
                    expect_equal(eth_config[[param]], config_default[[param]][eth_idx,],
                              info = paste("Time-varying parameter", param, "should be correctly subset"))
               }
          }
     }
})

test_that("get_location_config returns locations in original config order", {
     # Request locations in specific order different from original
     ordered_config <- get_location_config(iso = c("ZWE", "AGO", "KEN"))
     
     # Check that locations are returned in the order they appear in the original config
     # (not necessarily the order requested)
     expect_setequal(ordered_config$location_name, c("ZWE", "AGO", "KEN"))
     
     # Find the original order of these locations
     orig_indices <- which(config_default$location_name %in% c("ZWE", "AGO", "KEN"))
     expected_order <- config_default$location_name[orig_indices]
     
     # Verify they match the original order
     expect_equal(ordered_config$location_name, expected_order)
})

test_that("get_location_config works with sampled parameters", {
     skip_if_not(exists("sample_parameters"))
     
     # Create sampled config
     set_root_directory(system.file(package = "MOSAIC"))
     suppressMessages({
          PATHS <- get_paths()
          config_sampled <- sample_parameters(PATHS, seed = 123)
     })
     
     # Extract location from sampled config
     eth_sampled <- get_location_config(config = config_sampled, iso = "ETH")
     
     # Check it works correctly
     expect_type(eth_sampled, "list")
     expect_equal(eth_sampled$location_name, "ETH")
     
     # Check sampled values are preserved
     eth_idx <- which(config_sampled$location_name == "ETH")
     expect_equal(eth_sampled$beta_j0_env, config_sampled$beta_j0_env[eth_idx])
})