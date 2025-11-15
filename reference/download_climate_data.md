# Download and Save Climate Data for Multiple Countries (Parquet Format, Multiple Models and Variables)

This function downloads daily climate data for a list of specified
countries, saving the data as Parquet files. The data includes both
historical and future climate variables at grid points within each
country for a specified set of climate models and variables.

## Usage

``` r
download_climate_data(
  PATHS,
  iso_codes,
  n_points,
  date_start,
  date_stop,
  climate_models,
  climate_variables,
  api_key
)
```

## Arguments

- PATHS:

  A list containing paths where raw and processed data are stored. PATHS
  is typically the output of the
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  function and should include:

  - **DATA_SHAPEFILES**: Path to the directory containing country
    shapefiles.

  - **DATA_CLIMATE**: Path to the directory where processed climate data
    will be saved.

- iso_codes:

  A character vector of ISO3 country codes for which climate data should
  be downloaded.

- n_points:

  An integer specifying the number of grid points to generate within
  each country for which climate data will be downloaded.

- date_start:

  A character string representing the start date for the climate data
  (in "YYYY-MM-DD" format).

- date_stop:

  A character string representing the end date for the climate data (in
  "YYYY-MM-DD" format).

- climate_models:

  A character vector of climate models to use. Available models include:

  - **CMCC_CM2_VHR4**

  - **FGOALS_f3_H**

  - **HiRAM_SIT_HR**

  - **MRI_AGCM3_2_S**

  - **EC_Earth3P_HR**

  - **MPI_ESM1_2_XR**

  - **NICAM16_8S**

- climate_variables:

  A character vector of climate variables to retrieve. See details for
  available variables.

- api_key:

  A character string representing the API key required to access the
  climate data API.

## Value

The function does not return a value. It downloads the climate data for
each country, climate model, and climate variable, saving the results as
Parquet files in the specified directory.

## Details

This function uses country shapefiles to generate a grid of points
within each country, at which climate data is downloaded. The function
retrieves climate data for the specified date range (`date_start` to
`date_stop`) and the specified climate models and variables. The data is
saved for each country, climate model, and climate variable in a Parquet
file named
`climate_data_{climate_model}_{climate_variable}_{date_start}_{date_stop}_{ISO3}.parquet`.

The available climate variables include:

- **temperature_2m_mean**, **temperature_2m_max**,
  **temperature_2m_min**

- **wind_speed_10m_mean**, **wind_speed_10m_max**

- **cloud_cover_mean**

- **shortwave_radiation_sum**

- **relative_humidity_2m_mean**, **relative_humidity_2m_max**,
  **relative_humidity_2m_min**

- **dew_point_2m_mean**, **dew_point_2m_min**, **dew_point_2m_max**

- **precipitation_sum**, **rain_sum**, **snowfall_sum**

- **pressure_msl_mean**

- **soil_moisture_0_to_10cm_mean**

- **et0_fao_evapotranspiration_sum**

## Examples

``` r
if (FALSE) { # \dontrun{
# Define paths for raw and processed data using get_paths()
PATHS <- get_paths()

# ISO3 country codes for African countries
iso_codes <- c("ZAF", "KEN", "NGA")

# API key for climate data API
api_key <- "your-api-key-here"

# Download climate data for multiple models and variables and save it for the specified countries
download_climate_data(PATHS, iso_codes, n_points = 5,
                      date_start = "1970-01-01", date_stop = "2030-12-31",
                      climate_models = c("MRI_AGCM3_2_S", "EC_Earth3P_HR"),
                      climate_variables = c("temperature_2m_mean", "precipitation_sum"), api_key)
} # }
```
