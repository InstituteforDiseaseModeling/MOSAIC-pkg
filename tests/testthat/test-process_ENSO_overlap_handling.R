test_that("process_ENSO_data handles overlapping data correctly", {
  
  # Skip if dependencies not available
  skip_if_not_installed("dplyr")
  skip_if_not_installed("zoo")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("ISOweek")
  
  # Create mock historical data
  mock_historical_enso <- data.frame(
    year = c(2025, 2025, 2025),
    month = c(8, 9, 10),
    month_name = c("Aug", "Sep", "Oct"), 
    variable = rep("ENSO34", 3),
    value = c(-0.3, -0.5, -0.7),
    stringsAsFactors = FALSE
  )
  
  mock_historical_dmi <- data.frame(
    year = c(2025, 2025, 2025),
    month = c(8, 9, 10),
    month_name = c("Aug", "Sep", "Oct"),
    variable = rep("DMI", 3), 
    value = c(-0.2, -0.4, -0.6),
    stringsAsFactors = FALSE
  )
  
  # Create mock forecast data that overlaps (Sep-Oct)
  mock_forecast_enso <- data.frame(
    year = c(2025, 2025, 2025),
    month = c(9, 10, 11),
    month_name = c("Sep", "Oct", "Nov"),
    variable = rep("ENSO34", 3),
    value = c(-0.9, -1.0, -0.8),  # Different values from historical
    stringsAsFactors = FALSE
  )
  
  mock_forecast_dmi <- data.frame(
    year = c(2025, 2025, 2025), 
    month = c(9, 10, 11),
    month_name = c("Sep", "Oct", "Nov"),
    variable = rep("DMI", 3),
    value = c(-0.8, -0.9, -0.5),  # Different values from historical
    stringsAsFactors = FALSE
  )
  
  # Mock the get_* functions to return our test data
  mockery::stub(process_ENSO_data, 'get_DMI_historical', mock_historical_dmi)
  mockery::stub(process_ENSO_data, 'get_DMI_forecast', mock_forecast_dmi)  
  mockery::stub(process_ENSO_data, 'get_ENSO_historical', mock_historical_enso)
  mockery::stub(process_ENSO_data, 'get_ENSO_forecast', mock_forecast_enso)
  
  # Test overlap handling - should prefer historical data
  expect_message(
    result <- process_ENSO_data(2025, "monthly", "linear"),
    "Found overlapping data between historical and forecast"
  )
  
  # Check that result contains expected data
  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
  
  # Check that Sep 2025 ENSO34 uses historical value (-0.5), not forecast (-0.9)
  sep_enso <- result[result$year == 2025 & result$month == 9 & result$variable == "ENSO34", "value"]
  expect_equal(sep_enso, -0.5)  # Historical value
  
  # Check that Oct 2025 ENSO34 uses historical value (-0.7), not forecast (-1.0) 
  oct_enso <- result[result$year == 2025 & result$month == 10 & result$variable == "ENSO34", "value"]
  expect_equal(oct_enso, -0.7)  # Historical value
  
  # Check that Nov 2025 ENSO34 uses forecast value (-0.8) since no historical data
  nov_enso <- result[result$year == 2025 & result$month == 11 & result$variable == "ENSO34", "value"]
  expect_equal(nov_enso, -0.8)  # Forecast value
  
  # Similar checks for DMI
  sep_dmi <- result[result$year == 2025 & result$month == 9 & result$variable == "DMI", "value"]
  expect_equal(sep_dmi, -0.4)  # Historical value
  
  oct_dmi <- result[result$year == 2025 & result$month == 10 & result$variable == "DMI", "value"]
  expect_equal(oct_dmi, -0.6)  # Historical value
})

test_that("process_ENSO_data works without overlaps", {
  
  # Skip if dependencies not available
  skip_if_not_installed("dplyr")
  skip_if_not_installed("zoo") 
  skip_if_not_installed("lubridate")
  skip_if_not_installed("ISOweek")
  
  # Create mock data with no overlaps
  mock_historical_enso <- data.frame(
    year = c(2025, 2025),
    month = c(7, 8),
    month_name = c("Jul", "Aug"),
    variable = rep("ENSO34", 2),
    value = c(-0.3, -0.4),
    stringsAsFactors = FALSE
  )
  
  mock_historical_dmi <- data.frame(
    year = c(2025, 2025),
    month = c(7, 8), 
    month_name = c("Jul", "Aug"),
    variable = rep("DMI", 2),
    value = c(-0.2, -0.3),
    stringsAsFactors = FALSE
  )
  
  # Forecast starts later with no overlap
  mock_forecast_enso <- data.frame(
    year = c(2025, 2025),
    month = c(10, 11),
    month_name = c("Oct", "Nov"),
    variable = rep("ENSO34", 2),
    value = c(-0.8, -0.9),
    stringsAsFactors = FALSE
  )
  
  mock_forecast_dmi <- data.frame(
    year = c(2025, 2025),
    month = c(10, 11),
    month_name = c("Oct", "Nov"), 
    variable = rep("DMI", 2),
    value = c(-0.7, -0.8),
    stringsAsFactors = FALSE
  )
  
  # Mock the functions
  mockery::stub(process_ENSO_data, 'get_DMI_historical', mock_historical_dmi)
  mockery::stub(process_ENSO_data, 'get_DMI_forecast', mock_forecast_dmi)
  mockery::stub(process_ENSO_data, 'get_ENSO_historical', mock_historical_enso)
  mockery::stub(process_ENSO_data, 'get_ENSO_forecast', mock_forecast_enso)
  
  # Should run without overlap messages
  expect_no_message(
    result <- process_ENSO_data(2025, "monthly", "linear"),
    "Found overlapping data"
  )
  
  # Should interpolate the gap (Sep 2025)
  expect_true(is.data.frame(result))
  expect_true(any(result$year == 2025 & result$month == 9))  # September should be interpolated
})

test_that("overlap detection and reporting works correctly", {
  
  # This test focuses on the overlap detection logic itself
  # Create test data with known overlaps
  test_df <- data.frame(
    date = as.Date(c("2025-09-01", "2025-09-01", "2025-10-01", "2025-10-01")),
    variable = c("ENSO34", "ENSO34", "ENSO34", "ENSO34"),
    value = c(-0.5, -0.9, -0.7, -1.0),
    data_source = c("historical", "forecast", "historical", "forecast"),
    year = c(2025, 2025, 2025, 2025),
    month = c(9, 9, 10, 10),
    stringsAsFactors = FALSE
  )
  
  # Sort as the function does
  test_df <- test_df[base::order(test_df$date, test_df$variable, test_df$data_source), ]
  
  # Find duplicates as the function does
  duplicated_entries <- test_df[base::duplicated(test_df[, c("date", "variable")]) | 
                               base::duplicated(test_df[, c("date", "variable")], fromLast = TRUE), ]
  
  expect_true(nrow(duplicated_entries) == 4)  # All 4 rows are part of duplicates
  
  # Test deduplication - should keep first occurrence (historical)
  deduplicated <- test_df[!base::duplicated(test_df[, c("date", "variable")]), ]
  expect_true(nrow(deduplicated) == 2)  # Should have 2 unique date-variable combinations
  expect_true(all(deduplicated$data_source == "historical"))  # Should keep historical
  expect_equal(deduplicated$value, c(-0.5, -0.7))  # Historical values
})