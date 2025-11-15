# Get Climate Forecast Data (Daily)

This function retrieves daily climate forecast data for a specified
location and one or more climate variables over a range of forecast days
using the Open-Meteo API.

## Usage

``` r
get_climate_forecast(lat, lon, n_days, climate_variables, api_key = NULL)
```

## Arguments

- lat:

  A numeric value representing the latitude of the location.

- lon:

  A numeric value representing the longitude of the location.

- n_days:

  An integer representing the number of forecast days to retrieve
  (maximum value is 15).

- climate_variables:

  A character vector of climate variables to retrieve (e.g.,
  c("temperature_2m_max", "precipitation_sum")).

- api_key:

  A character string representing the API key for the Open-Meteo API.

## Value

A data frame with columns:

- date:

  The date of the forecast (class `Date`).

- climate_variable:

  The name of the climate variable.

- climate_value:

  The value of the climate variable for each date.

## Details

The function queries the Open-Meteo API for daily weather forecast data
for a specified latitude, longitude, and range of forecast days (up to
15 days). If the API request is successful, the function returns a data
frame containing the date, climate variable, and its value. If the
request fails, the function returns `NULL` and issues a warning with the
HTTP status code.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example usage to get forecast data for multiple climate variables
lat <- 52.52
lon <- 13.41
n_days <- 15
climate_variables <- c("temperature_2m_max", "precipitation_sum")
api_key <- "your_api_key_here"
forecast_data <- get_climate_forecast(lat, lon, n_days, climate_variables, api_key)
print(forecast_data)
} # }
```
