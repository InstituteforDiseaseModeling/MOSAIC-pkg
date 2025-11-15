# Get Historical Climate Data

This function retrieves historical weather data for a specified location
and one or more climate variables over a date range. It uses the
Open-Meteo API.

## Usage

``` r
get_climate_historical(
  lat,
  lon,
  start_date,
  end_date,
  climate_variables,
  api_key = NULL
)
```

## Arguments

- lat:

  A numeric value representing the latitude of the location.

- lon:

  A numeric value representing the longitude of the location.

- start_date:

  A character string representing the start date in "YYYY-MM-DD" format.

- end_date:

  A character string representing the end date in "YYYY-MM-DD" format.

- climate_variables:

  A character vector of climate variables to retrieve (e.g.,
  c("precipitation_sum", "temperature_2m_max")).

- api_key:

  A character string representing the API key for the Open-Meteo API. If
  not provided, a default key is used.

## Value

A data frame with columns:

- date:

  The date of the observation (class `Date`).

- climate_variable:

  The name of the climate variable.

- climate_value:

  The value of the climate variable for each date.

## Details

The function queries the Open-Meteo API for historical weather data for
a given latitude, longitude, and climate variables over a specified date
range. If the API request is successful, the function returns a data
frame containing the date, climate variable, and its value. If the
request fails, the function returns `NULL` and issues a warning with the
HTTP status code.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage to get historical data for multiple climate variables
lat <- 40.7128
lon <- -74.0060
start_date <- "2020-01-01"
end_date <- "2020-12-31"
climate_variables <- c("temperature_2m_max", "precipitation_sum")
api_key <- "your_api_key_here"  # Replace with your actual API key
climate_data <- get_climate_historical(lat, lon, start_date, end_date, climate_variables, api_key)
print(climate_data)
} # }
```
