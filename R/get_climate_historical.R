#' Get Historical Climate Data
#'
#' This function retrieves historical weather data for a specified location and one or more climate variables over a date range. It uses the Open-Meteo API.
#'
#' @param lat A numeric value representing the latitude of the location.
#' @param lon A numeric value representing the longitude of the location.
#' @param start_date A character string representing the start date in "YYYY-MM-DD" format.
#' @param end_date A character string representing the end date in "YYYY-MM-DD" format.
#' @param climate_variables A character vector of climate variables to retrieve (e.g., c("precipitation_sum", "temperature_2m_max")).
#' @param api_key A character string representing the API key for the Open-Meteo API. If not provided, a default key is used.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{date}{The date of the observation (class `Date`).}
#'   \item{climate_variable}{The name of the climate variable.}
#'   \item{climate_value}{The value of the climate variable for each date.}
#' }
#'
#' @details The function queries the Open-Meteo API for historical weather data for a given latitude, longitude, and climate variables over a specified date range. If the API request is successful, the function returns a data frame containing the date, climate variable, and its value. If the request fails, the function returns `NULL` and issues a warning with the HTTP status code.
#'
#' @examples
#' \dontrun{
#' # Example usage to get historical data for multiple climate variables
#' lat <- 40.7128
#' lon <- -74.0060
#' start_date <- "2020-01-01"
#' end_date <- "2020-12-31"
#' climate_variables <- c("temperature_2m_max", "precipitation_sum")
#' api_key <- "your_api_key_here"  # Replace with your actual API key
#' climate_data <- get_climate_historical(lat, lon, start_date, end_date, climate_variables, api_key)
#' print(climate_data)
#' }
#' @importFrom httr GET content status_code
#' @importFrom jsonlite fromJSON
#' @export

get_climate_historical <- function(lat, lon, start_date, end_date, climate_variables, api_key = NULL) {

     available_climate_variables <- c(
          "temperature_2m_mean",
          "temperature_2m_max",
          "temperature_2m_min",
          "wind_speed_10m_mean",
          "wind_speed_10m_max",
          "cloud_cover_mean",
          "shortwave_radiation_sum",
          "relative_humidity_2m_mean",
          "relative_humidity_2m_max",
          "relative_humidity_2m_min",
          "dew_point_2m_mean",
          "dew_point_2m_min",
          "dew_point_2m_max",
          "precipitation_sum",
          "rain_sum",
          "snowfall_sum",
          "pressure_msl_mean",
          "soil_moisture_0_to_10cm_mean",
          "et0_fao_evapotranspiration_sum"
     )

     # Validate that all provided climate variables are available
     if (!all(climate_variables %in% available_climate_variables)) {
          stop(paste("Error: Some climate variables are not available. Please choose from:", paste(available_climate_variables, collapse = ", ")))
     }

     # Join the climate variables into a comma-separated string for the API request
     variables_param <- paste(climate_variables, collapse = ",")

     # Construct the API request URL dynamically using the climate variables
     url <- paste0(
          "https://customer-archive-api.open-meteo.com/v1/archive?",
          "latitude=", lat,
          "&longitude=", lon,
          "&start_date=", start_date,
          "&end_date=", end_date,
          "&daily=", variables_param,
          "&apikey=", api_key
     )

     # Send the GET request to the API
     response <- httr::GET(url)

     # Check if the request was successful
     if (httr::status_code(response) == 200) {

          # Parse the JSON response
          data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
          dates <- data$daily$time

          # Initialize an empty list to store the results
          results_list <- list()

          # Loop through the climate variables and extract their respective data
          for (variable in climate_variables) {
               if (!is.null(data$daily[[variable]])) {
                    # Create a data frame for each climate variable and append it to the results list
                    results_list[[variable]] <- data.frame(
                         date = as.Date(dates),
                         climate_variable = variable,
                         value = data$daily[[variable]]
                    )
               }
          }

          # Combine all variable results into one data frame
          results_df <- do.call(rbind, results_list)
          rownames(results_df) <- NULL  # Set rownames to NULL
          return(results_df)

     } else {

          # Handle errors and return NULL
          warning("Failed to retrieve data: ", httr::status_code(response))
          return(NULL)

     }
}
