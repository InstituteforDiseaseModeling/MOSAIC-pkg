#' Download Future Climate Data for Multiple Locations (One Model at a Time)
#'
#' This function retrieves daily future climate data for multiple specified locations and climate variables over a specified date range using a specified climate model.
#'
#' @param lat A numeric vector representing the latitudes of the locations.
#' @param lon A numeric vector representing the longitudes of the locations.
#' @param date_start A character string representing the start date for the data in "YYYY-MM-DD" format.
#' @param date_stop A character string representing the end date for the data in "YYYY-MM-DD" format.
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
#' @param climate_model A single character string representing the climate model to use. Available models include:
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
#' The function retrieves daily future climate data for multiple specified locations using the Open-Meteo
#' Climate API. Locations are batched into groups of up to 1,000 per request, and all climate variables
#' are fetched in a single API call per batch. Failed requests are retried with exponential backoff
#' (up to 5 attempts). The API requires a paid key (customer-climate-api.open-meteo.com).
#'
#' @examples
#' \dontrun{
#' # Define latitudes and longitudes for the locations
#' lat <- c(40.7128, 34.0522)
#' lon <- c(-74.0060, -118.2437)
#'
#' # Define the climate variables and model
#' climate_vars <- c("temperature_2m_mean", "precipitation_sum")
#' climate_model <- "MRI_AGCM3_2_S"
#'
#' # Set the date range and API key
#' date_start <- "2023-03-01"
#' date_stop <- "2030-12-31"
#' api_key <- "your_api_key_here"
#'
#' # Download the climate data
#' climate_data <- get_climate_future(lat, lon, date_start, date_stop,
#'                                    climate_vars, climate_model, api_key)
#'
#' # Display the climate data
#' head(climate_data)
#' }
#'
#' @export

get_climate_future <- function(lat,
                               lon,
                               date_start,
                               date_stop,
                               climate_variables,
                               climate_model,
                               api_key = NULL) {

     if (length(climate_model) > 1) stop("One climate model at a time")

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

     if (!(climate_model %in% available_models)) {
          stop(paste("Error: The provided climate model is not available. Please choose from:", paste(available_models, collapse = ", ")))
     }

     is_free  <- identical(api_key, "free")
     base_url <- if (is_free) "https://climate-api.open-meteo.com/v1/climate"
                 else          "https://customer-climate-api.open-meteo.com/v1/climate"

     variables_param <- paste(climate_variables, collapse = ",")
     chunk_size <- 1000L
     n_chunks <- ceiling(length(lat) / chunk_size)
     results_list <- list()

     for (chunk in seq_len(n_chunks)) {

          idx <- seq((chunk - 1L) * chunk_size + 1L, min(chunk * chunk_size, length(lat)))
          lat_chunk <- lat[idx]
          lon_chunk <- lon[idx]

          message(glue::glue("Fetching chunk {chunk}/{n_chunks} ({length(idx)} locations)"))

          url <- paste0(
               base_url, "?",
               "latitude=", paste(lat_chunk, collapse = ","),
               "&longitude=", paste(lon_chunk, collapse = ","),
               "&start_date=", date_start,
               "&end_date=", date_stop,
               "&models=", climate_model,
               "&daily=", variables_param,
               if (!is_free) paste0("&apikey=", api_key) else ""
          )

          response <- fetch_with_retry(url)

          if (httr::status_code(response) == 200) {

               raw <- jsonlite::fromJSON(
                    httr::content(response, "text", encoding = "UTF-8"),
                    simplifyVector = FALSE
               )

               # Single location -> object with $daily; multiple -> list of objects
               locations <- if (!is.null(raw$daily)) list(raw) else raw

               for (j in seq_along(locations)) {
                    loc   <- locations[[j]]
                    dates <- as.Date(substr(unlist(loc$daily$time), 1, 10))
                    lat_j <- lat_chunk[j]
                    lon_j <- lon_chunk[j]

                    for (variable in climate_variables) {
                         vals <- loc$daily[[variable]]
                         if (!is.null(vals)) {
                              results_list[[length(results_list) + 1L]] <- data.frame(
                                   date = dates,
                                   latitude = lat_j,
                                   longitude = lon_j,
                                   climate_model = climate_model,
                                   variable_name = variable,
                                   value = vapply(vals, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
                              )
                         }
                    }
               }

          } else {
               warning(paste("Failed to retrieve data for chunk", chunk,
                             "— HTTP", httr::status_code(response)))
          }

          if (is_free && chunk < n_chunks) Sys.sleep(0.11)
     }

     results_df <- do.call(rbind, results_list)
     rownames(results_df) <- NULL
     return(results_df)
}


fetch_with_retry <- function(url, max_retries = 5, base_wait = 60) {
     for (attempt in seq_len(max_retries)) {
          resp <- httr::GET(url, httr::timeout(120))
          code <- httr::status_code(resp)
          if (code == 200) return(resp)
          if (code == 429 || code >= 500) {
               wait <- base_wait * 2^(attempt - 1)
               message(glue::glue("HTTP {code} — retrying in {wait}s (attempt {attempt}/{max_retries})"))
               Sys.sleep(wait)
          } else {
               break
          }
     }
     return(resp)
}
