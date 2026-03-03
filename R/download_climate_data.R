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
#' @return The function does not return a value. It downloads the climate data for each country and climate model, saving the results as Parquet files in the specified directory. One file is saved per climate variable per model per country, named `climate_data_{climate_model}_{climate_variable}_{date_start}_{date_stop}_{ISO3}.parquet`.
#'
#' @details This function uses country shapefiles to generate a grid of points within each country, at which climate data is downloaded. All climate variables are fetched in a single API call per country/model combination (batching all grid points up to 1,000 per request), then split and saved as individual Parquet files per variable. The file naming convention is: `climate_data_{climate_model}_{climate_variable}_{date_start}_{date_stop}_{ISO3}.parquet`.
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
          "precipitation_sum", "pressure_msl_mean", "soil_moisture_0_to_10cm_mean",
          "et0_fao_evapotranspiration_sum"
     )

     if (!dir.exists(PATHS$DATA_CLIMATE)) {
          dir.create(PATHS$DATA_CLIMATE, recursive = TRUE)
     }

     is_free <- identical(api_key, "free")

     if (is_free) {
          n_days <- as.integer(as.Date(date_stop) - as.Date(date_start)) + 1L
          estimated_calls <- length(climate_models) * length(iso_codes) * n_points *
                             length(climate_variables) * 263.5 * (n_days / 36500)
          if (estimated_calls > 0.8 * 300000L) {
               warning(glue::glue(
                    "Estimated API call-equivalent cost ({round(estimated_calls)}) exceeds ",
                    "80% of the free tier monthly limit (300000). ",
                    "The request may hit the free API limit."
               ))
          }
     }

     n_cores <- max(1L, parallel::detectCores() - 1L)
     # n_cores <- if (is_free) 1L else max(1L, parallel::detectCores() - 1L)

     for (climate_model in climate_models) {

          process_country <- function(country_iso_code) {
               message(glue::glue("Downloading climate data for {country_iso_code} using {climate_model} at {n_points} points"))

               country_name <- MOSAIC::convert_iso_to_country(country_iso_code)
               country_shp  <- sf::st_read(
                    dsn = file.path(PATHS$DATA_SHAPEFILES, paste0(country_iso_code, "_ADM0.shp")),
                    quiet = TRUE)
               grid_points <- MOSAIC::generate_country_grid_n(country_shp, n_points = n_points)
               coords      <- as.data.frame(sf::st_coordinates(grid_points))
               rm(grid_points, country_shp)

               climate_data <- MOSAIC::get_climate_future(
                    lat = coords$Y, lon = coords$X,
                    date_start = date_start, date_stop = date_stop,
                    climate_variables = climate_variables,
                    climate_model = climate_model, api_key = api_key
               )
               if (is.null(climate_data) || nrow(climate_data) == 0) {
                    warning(glue::glue("No data returned for {country_iso_code} / {climate_model} — skipping"))
                    return(NULL)
               }
               climate_data <- data.frame(
                    country_name = country_name,
                    iso_code     = country_iso_code,
                    year  = lubridate::year(climate_data$date),
                    month = lubridate::month(climate_data$date),
                    week  = lubridate::week(climate_data$date),
                    doy   = lubridate::yday(climate_data$date),
                    climate_data
               )
               for (climate_variable in climate_variables) {
                    variable_data <- climate_data[climate_data$variable_name == climate_variable, ]
                    if (nrow(variable_data) == 0) next
                    arrow::write_parquet(
                         variable_data,
                         sink = file.path(PATHS$DATA_RAW, paste0(
                              "climate/climate_data_", climate_model, "_", climate_variable, "_",
                              date_start, "_", date_stop, "_", country_iso_code, ".parquet")))
               }
               if (is_free) Sys.sleep(0.11)
          }

          if (!is_free && .Platform$OS.type != "windows") {
               parallel::mclapply(iso_codes, process_country, mc.cores = n_cores)
          } else {
               lapply(iso_codes, process_country)
          }
     }

     message(glue::glue("Raw climate data saved for all countries, variables, and models here: {PATHS$DATA_RAW}/climate"))
}
