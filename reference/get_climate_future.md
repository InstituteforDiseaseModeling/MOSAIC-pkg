# Download Future Climate Data for Multiple Locations (One Model at a Time)

This function retrieves daily future climate data for multiple specified
locations and climate variables over a specified date range using a
specified climate model.

## Usage

``` r
get_climate_future(
  lat,
  lon,
  date_start,
  date_stop,
  climate_variables,
  climate_model,
  api_key = NULL
)
```

## Arguments

- lat:

  A numeric vector representing the latitudes of the locations.

- lon:

  A numeric vector representing the longitudes of the locations.

- date_start:

  A character string representing the start date for the data in
  "YYYY-MM-DD" format.

- date_stop:

  A character string representing the end date for the data in
  "YYYY-MM-DD" format.

- climate_variables:

  A character vector of climate variables to retrieve. Valid options
  include:

  - **temperature_2m_mean**: Mean 2m air temperature.

  - **temperature_2m_max**: Maximum 2m air temperature.

  - **temperature_2m_min**: Minimum 2m air temperature.

  - **wind_speed_10m_mean**: Mean 10m wind speed.

  - **wind_speed_10m_max**: Maximum 10m wind speed.

  - **cloud_cover_mean**: Mean cloud cover.

  - **shortwave_radiation_sum**: Sum of shortwave radiation.

  - **relative_humidity_2m_mean**: Mean 2m relative humidity.

  - **relative_humidity_2m_max**: Maximum 2m relative humidity.

  - **relative_humidity_2m_min**: Minimum 2m relative humidity.

  - **dew_point_2m_mean**: Mean 2m dew point temperature.

  - **dew_point_2m_min**: Minimum 2m dew point temperature.

  - **dew_point_2m_max**: Maximum 2m dew point temperature.

  - **precipitation_sum**: Total precipitation.

  - **rain_sum**: Total rainfall.

  - **snowfall_sum**: Total snowfall.

  - **pressure_msl_mean**: Mean sea level pressure.

  - **soil_moisture_0_to_10cm_mean**: Mean soil moisture (0-10 cm
    depth).

  - **et0_fao_evapotranspiration_sum**: Sum of evapotranspiration (FAO
    standard).

- climate_model:

  A single character string representing the climate model to use.
  Available models include:

  - **CMCC_CM2_VHR4**

  - **FGOALS_f3_H**

  - **HiRAM_SIT_HR**

  - **MRI_AGCM3_2_S**

  - **EC_Earth3P_HR**

  - **MPI_ESM1_2_XR**

  - **NICAM16_8S**

- api_key:

  A character string representing the API key for the climate data API.
  If not provided, the function assumes the API key is not required.

## Value

A data frame with columns:

- **date**: The date of the climate data.

- **latitude**: The latitude of the location.

- **longitude**: The longitude of the location.

- **climate_model**: The climate model used for the data.

- **variable_name**: The climate variable retrieved (e.g.,
  temperature_2m_mean, precipitation_sum).

- **value**: The value of the climate variable for each date.

## Details

The function retrieves daily future climate data for multiple specified
locations using the Open-Meteo Climate API. It downloads the specified
climate variables for each latitude and longitude provided, using a
single climate model. The data is retrieved for the date range specified
by `date_start` and `date_stop`. A progress bar is displayed to indicate
the download progress.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define latitudes and longitudes for the locations
lat <- c(40.7128, 34.0522)
lon <- c(-74.0060, -118.2437)

# Define the climate variables and model
climate_vars <- c("temperature_2m_mean", "precipitation_sum")
climate_model <- "MRI_AGCM3_2_S"

# Set the date range and API key
date_start <- "2023-03-01"
date_stop <- "2030-12-31"
api_key <- "your_api_key_here"

# Download the climate data
climate_data <- get_climate_future(lat, lon, date_start, date_stop,
                                   climate_vars, climate_model, api_key)

# Display the climate data
head(climate_data)
} # }
```
