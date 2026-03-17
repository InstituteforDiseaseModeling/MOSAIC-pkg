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
#' @param climate_variables A character vector of climate variables to retrieve, or \code{NULL} (default) to use all available variables. See details for available variables.
#' @param api_key A character string representing the API key required to access the climate data API. Use \code{"free"} for the free tier (rate-limited).
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
#'   \item \strong{precipitation_sum}
#'   \item \strong{pressure_msl_mean}
#'   \item \strong{soil_moisture_0_to_10cm_mean}
#'   \item \strong{et0_fao_evapotranspiration_sum}
#' }
#'
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
                                  climate_variables = NULL,
                                  api_key) {

     if (is.null(climate_variables)) climate_variables <- .climate_variables_all()

     # Ensure output directory exists, if not, create it
     .ensure_climate_dir(PATHS)
     is_free <- identical(api_key, "free")

     .check_free_tier_quota(api_key, climate_models, climate_variables,
                            n_units = length(iso_codes), n_pts_per_unit = n_points,
                            date_start = date_start, date_stop = date_stop)

     units <- lapply(setNames(iso_codes, iso_codes), function(iso) {
          shp <- sf::st_read(
               dsn = file.path(PATHS$DATA_SHAPEFILES, paste0(iso, "_ADM0.shp")),
               quiet = TRUE)
          coords <- as.data.frame(sf::st_coordinates(MOSAIC::generate_country_grid_n(shp, n_points)))
          list(id = iso, lat = coords$Y, lon = coords$X)
     })

     .process_and_save_units(units, mode = "country", PATHS = PATHS,
                             date_start = date_start, date_stop = date_stop,
                             climate_models = climate_models,
                             climate_variables = climate_variables,
                             api_key = api_key, is_free = is_free)
}

#' Download and Save Climate Data for Custom Shapes (Parquet Format)
#'
#' Downloads daily climate data for regions defined by sf polygon shapes.
#' Generates a grid of points within each shape, fetches climate data,
#' and saves per-variable Parquet files with a \code{region_id} column.
#'
#' @inheritParams download_climate_data
#' @param shapes A named list of sf polygon objects. Names become unit IDs.
#'
#' @return Invisible NULL. Saves Parquet files to \code{PATHS$DATA_RAW/climate/}.
#' @export
download_climate_data_shapes <- function(PATHS, shapes, n_points,
                                         date_start, date_stop,
                                         climate_models, climate_variables = NULL, api_key) {

     if (is.null(climate_variables)) climate_variables <- .climate_variables_all()
     .ensure_climate_dir(PATHS)
     is_free <- identical(api_key, "free")

     .check_free_tier_quota(api_key, climate_models, climate_variables,
                            n_units = length(shapes), n_pts_per_unit = n_points,
                            date_start = date_start, date_stop = date_stop)

     units <- lapply(names(shapes), function(nm) {
          coords <- as.data.frame(sf::st_coordinates(MOSAIC::generate_country_grid_n(shapes[[nm]], n_points)))
          list(id = nm, lat = coords$Y, lon = coords$X)
     })

     .process_and_save_units(units, mode = "region", PATHS = PATHS,
                             date_start = date_start, date_stop = date_stop,
                             climate_models = climate_models,
                             climate_variables = climate_variables,
                             api_key = api_key, is_free = is_free)
}

#' Download and Save Climate Data for Explicit Points (Parquet Format)
#'
#' Downloads daily climate data for explicit lat/lon points. Points with the
#' same \code{id} are grouped together. If no \code{id} column exists, all
#' points form a single unit with id \code{"custom"}.
#'
#' @inheritParams download_climate_data
#' @param points A data.frame with columns \code{lat}, \code{lon}, and optional \code{id}.
#'
#' @return Invisible NULL. Saves Parquet files to \code{PATHS$DATA_RAW/climate/}.
#' @export
download_climate_data_points <- function(PATHS, points,
                                         date_start, date_stop,
                                         climate_models, climate_variables = NULL, api_key) {

     if (is.null(climate_variables)) climate_variables <- .climate_variables_all()
     .ensure_climate_dir(PATHS)
     is_free <- identical(api_key, "free")

     pts <- if (!"id" %in% names(points)) transform(points, id = "custom") else points
     n_pts_per_unit <- if ("id" %in% names(points)) {
          mean(as.integer(table(points$id)))
     } else {
          nrow(points)
     }
     n_units <- length(unique(pts$id))

     .check_free_tier_quota(api_key, climate_models, climate_variables,
                            n_units = n_units, n_pts_per_unit = n_pts_per_unit,
                            date_start = date_start, date_stop = date_stop)

     units <- lapply(split(pts, pts$id), function(g) list(id = g$id[1], lat = g$lat, lon = g$lon))

     .process_and_save_units(units, mode = "region", PATHS = PATHS,
                             date_start = date_start, date_stop = date_stop,
                             climate_models = climate_models,
                             climate_variables = climate_variables,
                             api_key = api_key, is_free = is_free)
}


# --- Private helpers (dot-prefixed, not exported by exportPattern) ---

.climate_variables_all <- function() {
     c("temperature_2m_mean", "temperature_2m_max", "temperature_2m_min",
       "wind_speed_10m_mean", "wind_speed_10m_max", "cloud_cover_mean",
       "shortwave_radiation_sum", "relative_humidity_2m_mean",
       "relative_humidity_2m_max", "relative_humidity_2m_min",
       "dew_point_2m_mean", "dew_point_2m_min", "dew_point_2m_max",
       "precipitation_sum", "pressure_msl_mean", "soil_moisture_0_to_10cm_mean",
       "et0_fao_evapotranspiration_sum")
}

.ensure_climate_dir <- function(PATHS) {
     # Ensure output directory exists, if not, create it
     if (!dir.exists(PATHS$DATA_CLIMATE)) {
          dir.create(PATHS$DATA_CLIMATE, recursive = TRUE)
     }
}

.check_free_tier_quota <- function(api_key, climate_models, climate_variables,
                                   n_units, n_pts_per_unit, date_start, date_stop) {
     if (!identical(api_key, "free")) return(invisible(NULL))

     n_days <- as.integer(as.Date(date_stop) - as.Date(date_start)) + 1L
     estimated_calls <- length(climate_models) * n_units * n_pts_per_unit *
          length(climate_variables) * 263.5 * (n_days / 36500)

     if (estimated_calls > 0.8 * 300000L) {
          warning(glue::glue(
               "Estimated API call-equivalent cost ({round(estimated_calls)}) exceeds ",
               "80% of the free tier monthly limit (300000). ",
               "The request may hit the free API limit."
          ))
     }
}

.process_and_save_units <- function(units, mode, PATHS, date_start, date_stop,
                                    climate_models, climate_variables,
                                    api_key, is_free) {

     n_cores <- max(1L, parallel::detectCores() - 1L)

     # Loop through each climate model
     for (climate_model in climate_models) {

          process_unit <- function(unit) {
               message(glue::glue("Downloading climate data for {unit$id} using {climate_model} at {length(unit$lat)} points"))

               # Download climate data for the generated grid points
               climate_data <- MOSAIC::get_climate_future(
                    lat = unit$lat, lon = unit$lon,
                    date_start = date_start, date_stop = date_stop,
                    climate_variables = climate_variables,
                    climate_model = climate_model, api_key = api_key
               )
               if (is.null(climate_data) || nrow(climate_data) == 0) {
                    warning(glue::glue("No data returned for {unit$id} / {climate_model} — skipping"))
                    return(NULL)
               }

               meta <- if (mode == "country") {
                    data.frame(
                         country_name = MOSAIC::convert_iso_to_country(unit$id),
                         iso_code     = unit$id,
                         stringsAsFactors = FALSE
                    )
               } else {
                    data.frame(region_id = unit$id, stringsAsFactors = FALSE)
               }

               # Add additional metadata columns
               climate_data <- data.frame(
                    meta,
                    year  = lubridate::year(climate_data$date),
                    month = lubridate::month(climate_data$date),
                    week  = lubridate::week(climate_data$date),
                    doy   = lubridate::yday(climate_data$date),
                    climate_data
               )

               # Save climate data as Parquet, including the climate model and variable in the file name
               for (climate_variable in climate_variables) {
                    variable_data <- climate_data[climate_data$variable_name == climate_variable, ]
                    if (nrow(variable_data) == 0) next
                    arrow::write_parquet(
                         variable_data,
                         sink = file.path(PATHS$DATA_RAW, paste0(
                              "climate/climate_data_", climate_model, "_", climate_variable, "_",
                              date_start, "_", date_stop, "_", unit$id, ".parquet")))
               }
               if (is_free) Sys.sleep(0.11)
          }

          if (!is_free && .Platform$OS.type != "windows") {
               parallel::mclapply(units, process_unit, mc.cores = n_cores)
          } else {
               lapply(units, process_unit)
          }
     }

     message(glue::glue("Raw climate data saved for all units, variables, and models here: {PATHS$DATA_RAW}/climate"))
}
