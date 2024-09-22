#' Download and Save Climate Data for Multiple Countries
#'
#' This function downloads daily climate data for a list of specified countries, saving the data as CSV files. The data includes both historical and future climate variables at grid points within each country.
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
#'
#' @return The function does not return a value. It downloads the climate data for each country and saves the results as CSV files in the specified directory.
#'
#' @details This function uses country shapefiles to generate a grid of points within each country, at which climate data is downloaded. The function retrieves climate data for the specified date range (`date_start` to `date_stop`). The data is saved for each country in a CSV file named `climate_data_<date_start>_<date_stop>_<ISO3>.csv`.
#'
#' The climate data variables include temperature, wind speed, cloud cover, precipitation, and more. The function retrieves data from multiple climate models, including MRI and EC Earth models.
#'
#' @importFrom dplyr select mutate
#' @importFrom lubridate year month week yday
#' @importFrom sf st_read st_coordinates
#' @importFrom glue glue
#' @importFrom utils write.csv
#' @importFrom MOSAIC convert_iso_to_country generate_country_grid_n get_climate_future
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
#' # Download climate data and save it for the specified countries
#' download_climate_data(PATHS, iso_codes, api_key, n_points = 5,
#'                       date_start = "1970-01-01", date_stop = "2030-12-31")
#'}
#'
#' @export
download_climate_data <- function(PATHS, iso_codes, api_key, n_points = 3, date_start = "1970-01-01", date_stop = "2030-12-31") {

     # Ensure output directory exists, if not, create it
     if (!dir.exists(PATHS$DATA_CLIMATE)) {
          dir.create(PATHS$DATA_CLIMATE, recursive = TRUE)
     }

     # List of climate variables for both historical and future data
     climate_variables_historical_and_future <- c(
          "temperature_2m_mean", "temperature_2m_max", "temperature_2m_min",
          "wind_speed_10m_mean", "wind_speed_10m_max", "cloud_cover_mean",
          "shortwave_radiation_sum", "relative_humidity_2m_mean",
          "relative_humidity_2m_max", "relative_humidity_2m_min",
          "dew_point_2m_mean", "dew_point_2m_min", "dew_point_2m_max",
          "precipitation_sum", "rain_sum", "snowfall_sum",
          "pressure_msl_mean", "soil_moisture_0_to_10cm_mean",
          "et0_fao_evapotranspiration_sum"
     )

     # Loop through each ISO3 country code
     for (country_iso_code in iso_codes) {

          message(glue::glue("Downloading daily climate data for {country_iso_code} at {n_points} points"))

          # Convert ISO3 code to country name and read country shapefile
          country_name <- MOSAIC::convert_iso_to_country(country_iso_code)
          country_shp <- sf::st_read(dsn = file.path(PATHS$DATA_SHAPEFILES, paste0(country_iso_code, "_ADM0.shp")), quiet = TRUE)

          # Generate grid points within the country
          grid_points <- MOSAIC::generate_country_grid_n(country_shp, n_points = n_points)
          coords <- sf::st_coordinates(grid_points)
          coords <- as.data.frame(coords)

          # Download climate data for the generated grid points
          climate_data <- MOSAIC::get_climate_future(lat = coords$Y,
                                                     lon = coords$X,
                                                     start_date = date_start,
                                                     end_date = date_stop,
                                                     climate_variables = climate_variables_historical_and_future,
                                                     climate_models = c("MRI_AGCM3_2_S", "EC_Earth3P_HR"),
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

          # Save climate data as CSV
          write.csv(climate_data,
                    file = file.path(PATHS$DATA_CLIMATE, paste0("climate_data_", date_start, "_", date_stop, "_", country_iso_code, ".csv")),
                    row.names = FALSE)
     }

     message(glue::glue("Climate data saved for all countries here: {PATHS$DATA_CLIMATE}"))
}
