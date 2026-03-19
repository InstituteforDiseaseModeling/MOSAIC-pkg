test_that("forecast data management functions work", {
  
  # Test freshness checking
  expect_no_error(freshness <- check_forecast_freshness())
  expect_true(is.list(freshness))
  expect_true(all(c("overall_status", "enso", "dmi") %in% names(freshness)))
  
  # Test JSON loading functions
  expect_no_error(enso_json <- get_ENSO_forecast_from_json())
  expect_no_error(dmi_json <- get_DMI_forecast_from_json())
  
  # Check structure of JSON-loaded data
  expect_true(is.data.frame(enso_json))
  expect_true(is.data.frame(dmi_json))
  expect_true(all(c("year", "month", "month_name", "variable", "value") %in% names(enso_json)))
  expect_true(all(c("year", "month", "month_name", "variable", "value") %in% names(dmi_json)))
  
  # Check variable names
  expect_true(all(enso_json$variable %in% c("ENSO3", "ENSO34", "ENSO4")))
  expect_true(all(dmi_json$variable == "DMI"))
})

test_that("enhanced forecast functions work with JSON fallback", {
  
  # Test ENSO forecast with JSON enabled (default)
  expect_no_error(enso_data <- get_ENSO_forecast())
  expect_true(is.data.frame(enso_data))
  expect_true(nrow(enso_data) > 0)
  
  # Test DMI forecast with JSON enabled (default) 
  expect_no_error(dmi_data <- get_DMI_forecast())
  expect_true(is.data.frame(dmi_data))
  expect_true(nrow(dmi_data) > 0)
  
  # The use_json parameter was removed; functions now always use JSON fallback.
  # Verify the returned data has expected structure.
  expect_true(all(c("year", "month", "value") %in% names(enso_data)))
  expect_true(all(c("year", "month", "value") %in% names(dmi_data)))
})

test_that("JSON configuration files exist and are valid", {
  
  # Check files exist
  enso_file <- system.file("extdata", "enso_forecast_current.json", package = "MOSAIC")
  dmi_file <- system.file("extdata", "dmi_forecast_current.json", package = "MOSAIC")
  
  expect_true(file.exists(enso_file))
  expect_true(file.exists(dmi_file))
  
  # Check JSON is valid
  expect_no_error(enso_config <- jsonlite::fromJSON(enso_file))
  expect_no_error(dmi_config <- jsonlite::fromJSON(dmi_file))
  
  # Check required fields exist
  expect_true(all(c("last_updated", "expires_after", "forecasts") %in% names(enso_config)))
  expect_true(all(c("last_updated", "expires_after", "forecasts") %in% names(dmi_config)))
  
  # Check forecast structure
  expect_true("ENSO34" %in% names(enso_config$forecasts))
  expect_true("ENSO3" %in% names(enso_config$forecasts))
  expect_true("ENSO4" %in% names(enso_config$forecasts))
  expect_true("DMI" %in% names(dmi_config$forecasts))
})

test_that("freshness status calculation works correctly", {
  
  # This tests the internal freshness logic
  current_date <- Sys.Date()
  
  # Test expired data
  status_expired <- get_freshness_status(-10, 30)
  expect_equal(status_expired$level, "EXPIRED")
  expect_true(status_expired$needs_update)
  
  # Test expiring soon
  status_expiring <- get_freshness_status(15, 30)
  expect_equal(status_expiring$level, "EXPIRING_SOON")
  expect_true(status_expiring$needs_update)
  
  # Test fresh data
  status_fresh <- get_freshness_status(60, 30)
  expect_equal(status_fresh$level, "FRESH")
  expect_false(status_fresh$needs_update)
})

test_that("data consistency between forecast functions", {

  # Get data from both forecast functions
  suppressMessages({
    enso_data <- get_ENSO_forecast()
    dmi_data <- get_DMI_forecast()
  })

  # Both should have consistent column structure
  expect_true(all(c("year", "month", "value") %in% names(enso_data)))
  expect_true(all(c("year", "month", "value") %in% names(dmi_data)))
  expect_true(is.numeric(enso_data$year))
  expect_true(is.numeric(enso_data$value))
  expect_true(is.numeric(dmi_data$year))
  expect_true(is.numeric(dmi_data$value))
})