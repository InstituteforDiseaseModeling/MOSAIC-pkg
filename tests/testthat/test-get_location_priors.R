library(MOSAIC)

# Source the function directly for testing to get the latest version
source("../../R/get_location_priors.R")

test_that("get_location_priors works with default priors and single location", {
     # Extract priors for single location
     eth_priors <- get_location_priors(iso = "ETH")
     
     # Check it returns a list with correct structure
     expect_type(eth_priors, "list")
     expect_true("metadata" %in% names(eth_priors))
     expect_true("parameters_global" %in% names(eth_priors))
     expect_true("parameters_location" %in% names(eth_priors))
     
     # Check global parameters are preserved
     expect_equal(eth_priors$parameters_global, priors_default$parameters_global)
     
     # Check location-specific parameters contain only ETH
     if (length(eth_priors$parameters_location) > 0) {
          first_param <- eth_priors$parameters_location[[1]]
          if (!is.null(first_param$parameters$location)) {
               expect_equal(names(first_param$parameters$location), "ETH")
          }
     }
})

test_that("get_location_priors works with multiple locations", {
     # Extract priors for multiple locations
     multi_priors <- get_location_priors(iso = c("ETH", "KEN", "UGA"))
     
     # Check structure
     expect_type(multi_priors, "list")
     
     # Check location-specific parameters contain all requested locations
     if (length(multi_priors$parameters_location) > 0) {
          first_param <- multi_priors$parameters_location[[1]]
          if (!is.null(first_param$parameters$location)) {
               locations <- names(first_param$parameters$location)
               expect_setequal(locations, c("ETH", "KEN", "UGA"))
          }
     }
})

test_that("get_location_priors handles missing iso argument", {
     expect_error(get_location_priors(), "iso argument is required")
     expect_error(get_location_priors(iso = NULL), "iso argument is required")
})

test_that("get_location_priors handles invalid iso types", {
     expect_error(get_location_priors(iso = 123), "iso must be a character vector")
     expect_error(get_location_priors(iso = list("ETH")), "iso must be a character vector")
})

test_that("get_location_priors handles empty iso vector", {
     expect_error(get_location_priors(iso = character(0)), "iso must contain at least one location")
})

test_that("get_location_priors handles non-existent locations", {
     expect_error(
          get_location_priors(iso = "XXX"),
          "location\\(s\\) not found in priors"
     )
})

test_that("get_location_priors handles case insensitivity", {
     # Should work with lowercase
     eth_lower <- get_location_priors(iso = "eth")
     eth_upper <- get_location_priors(iso = "ETH")
     
     # Should produce identical results
     expect_equal(eth_lower, eth_upper)
})

test_that("get_location_priors removes duplicate locations", {
     # Should handle duplicates gracefully
     priors_dup <- get_location_priors(iso = c("ETH", "ETH", "eth"))
     
     # Check only one ETH in result
     if (length(priors_dup$parameters_location) > 0) {
          first_param <- priors_dup$parameters_location[[1]]
          if (!is.null(first_param$parameters$location)) {
               expect_equal(length(first_param$parameters$location), 1)
               expect_equal(names(first_param$parameters$location), "ETH")
          }
     }
})

test_that("get_location_priors works with custom priors object", {
     # Use default priors as custom priors
     custom_priors <- priors_default
     
     # Extract with custom priors
     eth_priors <- get_location_priors(priors = custom_priors, iso = "ETH")
     
     # Check it works correctly
     expect_type(eth_priors, "list")
     expect_true("parameters_location" %in% names(eth_priors))
})

test_that("get_location_priors handles invalid priors structure", {
     # Test with non-list priors
     expect_error(
          get_location_priors(priors = "not a list", iso = "ETH"),
          "priors must be a list"
     )
     
     # Test with missing parameters_location
     bad_priors <- list(metadata = list(), parameters_global = list())
     expect_error(
          get_location_priors(priors = bad_priors, iso = "ETH"),
          "missing 'parameters_location'"
     )
})

test_that("get_location_priors preserves metadata", {
     eth_priors <- get_location_priors(iso = "ETH")
     
     # Check metadata is preserved
     if (!is.null(priors_default$metadata)) {
          expect_equal(eth_priors$metadata, priors_default$metadata)
     }
})

test_that("get_location_priors preserves all parameter attributes", {
     eth_priors <- get_location_priors(iso = "ETH")
     
     # Check that parameter structure is preserved
     if (length(eth_priors$parameters_location) > 0) {
          for (param_name in names(eth_priors$parameters_location)) {
               param <- eth_priors$parameters_location[[param_name]]
               
               # Check standard attributes exist
               expect_true("parameter_name" %in% names(param))
               expect_true("description" %in% names(param))
               expect_true("distribution" %in% names(param))
               expect_true("parameters" %in% names(param))
          }
     }
})

test_that("get_location_priors class attribute is preserved if exists", {
     eth_priors <- get_location_priors(iso = "ETH")
     
     # Check if class is preserved (only if original has a class)
     if (inherits(priors_default, "mosaic_priors")) {
          expect_s3_class(eth_priors, "mosaic_priors")
     } else {
          # If no special class, should still be a list
          expect_type(eth_priors, "list")
     }
})