#' Download Future Climate Data for Multiple Locations
#'
#' This function retrieves daily future climate data for multiple specified locations and climate variables over a specified date range using specified climate models.
#'
#' @param lat A numeric vector representing the latitudes of the locations.
#' @param lon A numeric vector representing the longitudes of the locations.
#' @param start_date A character string representing the start date for the data in "YYYY-MM-DD" format.
#' @param end_date A character string representing the end date for the data in "YYYY-MM-DD" format.
#' @param climate_variables A character vector of climate variables to retrieve. Valid options include:
#' \itemize{
#'   \item \strong{temperature_2m_mean}: Mean 2m air temperature.
#'   \item \strong{temperature_2m_max}: Maximum 2m air temperature.
#'   \item \strong{temperature_2m_min}: Minimum 2m air temperature.
#'   \item \strong{wind_speed_10m_mean}: Mean 10m wind speed.
#'   \item \strong{wind_speed_10m_max}: Maximum 10m wind speed.
#'   \item \strong{cloud_cover_mean}: Mean cloud cover.
#'   \item \strong{shortwave_radiation_sum}: Sum of shortwave radiation.
#'   \item \strong{relative_humidity_2m_mean}: Mean 2m relative humidity.
#'   \item \strong{relative_humidity_2m_max}: Maximum 2m relative humidity.
#'   \item \strong{relative_humidity_2m_min}: Minimum 2m relative humidity.
#'   \item \strong{dew_point_2m_mean}: Mean 2m dew point temperature.
#'   \item \strong{dew_point_2m_min}: Minimum 2m dew point temperature.
#'   \item \strong{dew_point_2m_max}: Maximum 2m dew point temperature.
#'   \item \strong{precipitation_sum}: Total precipitation.
#'   \item \strong{rain_sum}: Total rainfall.
#'   \item \strong{snowfall_sum}: Total snowfall.
#'   \item \strong{pressure_msl_mean}: Mean sea level pressure.
#'   \item \strong{soil_moisture_0_to_10cm_mean}: Mean soil moisture (0-10 cm depth).
#'   \item \strong{et0_fao_evapotranspiration_sum}: Sum of evapotranspiration (FAO standard).
#' }
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
#' @param api_key A character string representing the API key for the climate data API. If not provided, the function assumes the API key is not required.
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item \strong{date}: The date of the climate data.
#'   \item \strong{latitude}: The latitude of the location.
#'   \item \strong{longitude}: The longitude of the location.
#'   \item \strong{climate_model}: The climate model used for the data.
#'   \item \strong{variable_name}: The climate variable retrieved (e.g., temperature_2m_mean, precipitation_sum).
#'   \item \strong{value}: The value of the climate variable for each date.
#' }
#'
#' @details
#' The function retrieves daily future climate data for multiple specified locations using the Open-Meteo Climate API. It downloads the specified climate variables for each latitude and longitude provided, using the selected climate models. The data is retrieved for the date range specified by \code{start_date} and \code{end_date}. A progress bar is displayed to indicate the download progress.
#'
#' @examples
#' \dontrun{
#' # Define latitudes and longitudes for the locations
#' lat <- c(40.7128, 34.0522)
#' lon <- c(-74.0060, -118.2437)
#'
#' # Define the climate variables and models
#' climate_vars <- c("temperature_2m_mean", "precipitation_sum")
#' climate_models <- c("MRI_AGCM3_2_S", "EC_Earth3P_HR")
#'
#' # Set the date range and API key
#' start_date <- "2023-01-01"
#' end_date <- "2030-12-31"
#' api_key <- "your_api_key_here"
#'
#' # Download the climate data
#' climate_data <- get_climate_future(lat, lon, start_date, end_date,
#'                                    climate_vars, climate_models, api_key)
#'
#' # Display the climate data
#' head(climate_data)
#' }
#'
#' @export
get_climate_future <- function(lat, lon, start_date, end_date, climate_variables, climate_models, api_key = NULL) {

     available_climate_variables <- c(
          "temperature_2m_mean", "temperature_2m_max", "temperature_2m_min",
          "wind_speed_10m_mean", "wind_speed_10m_max", "cloud_cover_mean",
          "shortwave_radiation_sum", "relative_humidity_2m_mean",
          "relative_humidity_2m_max", "relative_humidity_2m_min",
          "dew_point_2m_mean", "dew_point_2m_min", "dew_point_2m_max",
          "precipitation_sum", "rain_sum", "snowfall_sum",
          "pressure_msl_mean", "soil_moisture_0_to_10cm_mean",
          "et0_fao_evapotranspiration_sum"
     )

     if (!all(climate_variables %in% available_climate_variables)) {
          stop(paste("Error: Some climate variables are not available. Please choose from:", paste(available_climate_variables, collapse = ", ")))
     }

     available_models <- c(
          "CMCC_CM2_VHR4", "FGOALS_f3_H", "HiRAM_SIT_HR",
          "MRI_AGCM3_2_S", "EC_Earth3P_HR", "MPI_ESM1_2_XR",
          "NICAM16_8S"
     )

     if (!all(climate_models %in% available_models)) {
          stop(paste("Error: Some of the provided models are not available. Please choose from:", paste(available_models, collapse = ", ")))
     }

     models_param <- paste(climate_models, collapse = ",")
     variables_param <- paste(climate_variables, collapse = ",")

     results_list <- list()

     pb <- txtProgressBar(min = 0, max = length(lat), style = 3)

     for (i in seq_along(lat)) {

          url <- paste0(
               "https://customer-climate-api.open-meteo.com/v1/climate?",
               "latitude=", lat[i],
               "&longitude=", lon[i],
               "&start_date=", start_date,
               "&end_date=", end_date,
               "&models=", models_param,
               "&daily=", variables_param,
               "&apikey=", api_key
          )

          response <- httr::GET(url)

          if (httr::status_code(response) == 200) {

               data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
               dates <- data$daily$time

               if (is.character(dates)) {
                    dates <- as.Date(substr(dates, 1, 10))
               }

               for (model in climate_models) {

                    for (variable in climate_variables) {

                         variable_name <- paste0(variable, "_", model)

                         if (!is.null(data$daily[[variable_name]])) {
                              results_list[[paste0(i, "_", model, "_", variable)]] <- data.frame(
                                   date = dates,
                                   latitude = lat[i],
                                   longitude = lon[i],
                                   climate_model = model,
                                   variable_name = variable,
                                   value = data$daily[[variable_name]]
                              )
                         }
                    }
               }

          } else {
               warning(paste("Failed to retrieve data for lat:", lat[i], "lon:", lon[i]))
          }

          # Update progress bar
          setTxtProgressBar(pb, i)
     }

     close(pb)

     results_df <- do.call(rbind, results_list)
     rownames(results_df) <- NULL

     return(results_df)
}
