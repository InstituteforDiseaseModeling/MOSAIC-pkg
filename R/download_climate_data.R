#' Download and Save Climate Data for Multiple Countries (Parquet Format, Multiple Models and Variables)
#'
#' This function downloads daily climate data for a list of specified countries, saving the data as Parquet files. The data includes both historical and future climate variables at grid points within each country for a specified set of climate models and variables.
#'
#' @param PATHS A list containing paths where raw and processed data are stored.
#' PATHS is typically the output of the `get_paths()` function and should include:
#' \itemize{
#'   \item \strong{DATA_SHAPEFILES}: Path to the directory containing country shapefiles.
#'   \item \strong{DATA_CLIMATE}: Path to the directory where processed climate data will be saved.
#' }
#' @param iso_codes A character vector of ISO3 country codes for which climate data should be downloaded.
#' @param api_key A character string representing the API key required to access the climate data API.
#' @param n_points An integer specifying the number of grid points to generate within each country for which climate data will be downloaded.
#' @param date_start A character string representing the start date for the climate data (in "YYYY-MM-DD" format).
#' @param date_stop A character string representing the end date for the climate data (in "YYYY-MM-DD" format).
#' @param climate_models A character vector of climate models to use. Available models include:
#' \itemize{
#'   \item \strong{CMCC_CM2_VHR4}
#'   \item \strong{FGOALS_f3_H}
#'   \item \strong{HiRAM_SIT_HR}
#'   \item \strong{MRI_AGCM3_2_S}
#'   \item \strong{EC_Earth3P_HR}
#'   \item \strong{MPI_ESM1_2_XR}
#'   \item \strong{NICAM16_8S}
#' }
#' @param climate_variables A character vector of climate variables to retrieve. See details for available variables.
#'
#' @return The function does not return a value. It downloads the climate data for each country, climate model, and climate variable, saving the results as Parquet files in the specified directory.
#'
#' @details This function uses country shapefiles to generate a grid of points within each country, at which climate data is downloaded. The function retrieves climate data for the specified date range (`date_start` to `date_stop`) and the specified climate models and variables. The data is saved for each country, climate model, and climate variable in a Parquet file named `climate_data_{climate_model}_{climate_variable}_{date_start}_{date_stop}_{ISO3}.parquet`.
#'
#' The available climate variables include:
#' \itemize{
#'   \item \strong{temperature_2m_mean}, \strong{temperature_2m_max}, \strong{temperature_2m_min}
#'   \item \strong{wind_speed_10m_mean}, \strong{wind_speed_10m_max}
#'   \item \strong{cloud_cover_mean}
#'   \item \strong{shortwave_radiation_sum}
#'   \item \strong{relative_humidity_2m_mean}, \strong{relative_humidity_2m_max}, \strong{relative_humidity_2m_min}
#'   \item \strong{dew_point_2m_mean}, \strong{dew_point_2m_min}, \strong{dew_point_2m_max}
#'   \item \strong{precipitation_sum}, \strong{rain_sum}, \strong{snowfall_sum}
#'   \item \strong{pressure_msl_mean}
#'   \item \strong{soil_moisture_0_to_10cm_mean}
#'   \item \strong{et0_fao_evapotranspiration_sum}
#' }
#'
#' @importFrom dplyr select mutate
#' @importFrom lubridate year month week yday
#' @importFrom sf st_read st_coordinates
#' @importFrom glue glue
#' @importFrom arrow write_parquet
#' @examples
#' \dontrun{
#' # Define paths for raw and processed data using get_paths()
#' PATHS <- get_paths()
#'
#' # ISO3 country codes for African countries
#' iso_codes <- c("ZAF", "KEN", "NGA")
#'
#' # API key for climate data API
#' api_key <- "your-api-key-here"
#'
#' # Download climate data for multiple models and variables and save it for the specified countries
#' download_climate_data(PATHS, iso_codes, n_points = 5,
#'                       date_start = "1970-01-01", date_stop = "2030-12-31",
#'                       climate_models = c("MRI_AGCM3_2_S", "EC_Earth3P_HR"),
#'                       climate_variables = c("temperature_2m_mean", "precipitation_sum"), api_key)
#'}
#'
#' @export

download_climate_data <- function(PATHS,
                                  iso_codes,
                                  n_points,
                                  date_start,
                                  date_stop,
                                  climate_models,
                                  climate_variables,
                                  api_key) {

     climate_variables <- c(
          "temperature_2m_mean", "temperature_2m_max", "temperature_2m_min",
          "wind_speed_10m_mean", "wind_speed_10m_max", "cloud_cover_mean",
          "shortwave_radiation_sum", "relative_humidity_2m_mean",
          "relative_humidity_2m_max", "relative_humidity_2m_min",
          "dew_point_2m_mean", "dew_point_2m_min", "dew_point_2m_max",
          "precipitation_sum", "rain_sum",
          "pressure_msl_mean", "soil_moisture_0_to_10cm_mean",
          "et0_fao_evapotranspiration_sum"
     )

     # Ensure output directory exists, if not, create it
     if (!dir.exists(PATHS$DATA_CLIMATE)) {
          dir.create(PATHS$DATA_CLIMATE, recursive = TRUE)
     }

     # Loop through each climate model
     for (climate_model in climate_models) {

          # Loop through each ISO3 country code
          for (country_iso_code in iso_codes) {

               # Loop through each climate variable
               for (climate_variable in climate_variables) {

                    message(glue::glue("Downloading daily {climate_variable} data for {country_iso_code} using {climate_model} at {n_points} points"))

                    # Convert ISO3 code to country name and read country shapefile
                    country_name <- MOSAIC::convert_iso_to_country(country_iso_code)
                    country_shp <- sf::st_read(dsn = file.path(PATHS$DATA_SHAPEFILES, paste0(country_iso_code, "_ADM0.shp")), quiet = TRUE)

                    # Generate grid points within the country
                    grid_points <- MOSAIC::generate_country_grid_n(country_shp, n_points = n_points)
                    coords <- sf::st_coordinates(grid_points)
                    coords <- as.data.frame(coords)
                    rm(grid_points)
                    rm(country_shp)

                    # Download climate data for the generated grid points for the current climate variable
                    climate_data <- MOSAIC::get_climate_future(lat = coords$Y,
                                                               lon = coords$X,
                                                               date_start = date_start,
                                                               date_stop = date_stop,
                                                               climate_variables = climate_variable,  # Single variable per loop
                                                               climate_model = climate_model,
                                                               api_key = api_key)

                    # Add additional metadata columns
                    climate_data <- data.frame(
                         country_name = country_name,
                         iso_code = country_iso_code,
                         year = lubridate::year(climate_data$date),
                         month = lubridate::month(climate_data$date),
                         week = lubridate::week(climate_data$date),
                         doy = lubridate::yday(climate_data$date),
                         climate_data
                    )

                    # Save climate data as Parquet, including the climate model and variable in the file name
                    arrow::write_parquet(climate_data,
                                         sink = file.path(PATHS$DATA_RAW, paste0("climate/climate_data_", climate_model, "_", climate_variable, "_", date_start, "_", date_stop, "_", country_iso_code, ".parquet")))
               }
          }
     }

     message(glue::glue("Raw climate data saved for all countries, variables, and models here: {PATHS$DATA_RAW}/climate"))
}
