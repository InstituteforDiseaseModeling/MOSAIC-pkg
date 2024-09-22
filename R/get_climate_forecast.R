#' Get Climate Forecast Data (Daily)
#'
#' This function retrieves daily climate forecast data for a specified location and one or more climate variables over a range of forecast days using the Open-Meteo API.
#'
#' @param lat A numeric value representing the latitude of the location.
#' @param lon A numeric value representing the longitude of the location.
#' @param n_days An integer representing the number of forecast days to retrieve (maximum value is 15).
#' @param climate_variables A character vector of climate variables to retrieve (e.g., c("temperature_2m_max", "precipitation_sum")).
#' @param api_key A character string representing the API key for the Open-Meteo API.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{date}{The date of the forecast (class \code{Date}).}
#'   \item{climate_variable}{The name of the climate variable.}
#'   \item{climate_value}{The value of the climate variable for each date.}
#' }
#'
#' @details The function queries the Open-Meteo API for daily weather forecast data for a specified latitude, longitude, and range of forecast days (up to 15 days). If the API request is successful, the function returns a data frame containing the date, climate variable, and its value. If the request fails, the function returns `NULL` and issues a warning with the HTTP status code.
#'
#' @examples
#' \dontrun{
#' # Example usage to get forecast data for multiple climate variables
#' lat <- 52.52
#' lon <- 13.41
#' n_days <- 15
#' climate_variables <- c("temperature_2m_max", "precipitation_sum")
#' api_key <- "your_api_key_here"
#' forecast_data <- get_climate_forecast(lat, lon, n_days, climate_variables, api_key)
#' print(forecast_data)
#'}
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @export


get_climate_forecast <- function(lat, lon, n_days, climate_variables, api_key = NULL) {

     # Set maximum value for n_days
     if (n_days > 15) {
          stop("Error: The maximum number of forecast days is 15.")
     }

     available_climate_variables <- c(
          "weather_code",
          "temperature_2m_max",
          "temperature_2m_min",
          "apparent_temperature_max",
          "apparent_temperature_min",
          "sunrise",
          "sunset",
          "daylight_duration",
          "sunshine_duration",
          "uv_index_max",
          "uv_index_clear_sky_max",
          "precipitation_sum",
          "rain_sum",
          "showers_sum",
          "snowfall_sum",
          "precipitation_hours",
          "precipitation_probability_max",
          "wind_speed_10m_max",
          "wind_gusts_10m_max",
          "wind_direction_10m_dominant",
          "shortwave_radiation_sum",
          "et0_fao_evapotranspiration"
     )

     # Validate that all provided climate variables are available
     if (!all(climate_variables %in% available_climate_variables)) {
          stop(paste("Error: Some climate variables are not available. Please choose from:", paste(available_climate_variables, collapse = ", ")))
     }

     # Join the climate variables into a comma-separated string for the API request
     variables_param <- paste(climate_variables, collapse = ",")

     # Construct the API request URL dynamically using the climate variables and forecast_days
     url <- paste0(
          "https://customer-api.open-meteo.com/v1/forecast?",
          "latitude=", lat,
          "&longitude=", lon,
          "&forecast_days=", n_days,
          "&daily=", variables_param,
          "&apikey=", api_key
     )

     # Create a temporary file to save the data
     temp_file <- tempfile(fileext = ".json")

     # Download the file using download.file with mode = "wb"
     download.file(url, temp_file, mode = "wb", quiet = TRUE)

     # Load the downloaded JSON file into a data frame
     data <- fromJSON(temp_file)

     # Extract the time (dates) and dynamically extract each variable's climate data
     dates <- data$daily$time

     # Ensure that 'dates' are converted to Date properly
     if (is.character(dates)) {
          dates <- as.Date(substr(dates, 1, 10))  # Convert to "YYYY-MM-DD" format
     }

     results_list <- list()

     # Loop through the climate variables to extract their respective data
     for (variable in climate_variables) {
          if (!is.null(data$daily[[variable]]) && length(dates) == length(data$daily[[variable]])) {
               # Create a data frame for each variable and bind them together
               results_list[[variable]] <- data.frame(
                    date = dates,
                    climate_variable = variable,
                    climate_value = data$daily[[variable]]
               )
          } else {
               warning(paste("No data found or mismatched lengths for:", variable))
          }
     }

     # Combine all variable results into one data frame
     results_df <- do.call(rbind, results_list)
     rownames(results_df) <- NULL  # Set rownames to NULL

     return(results_df)
}
