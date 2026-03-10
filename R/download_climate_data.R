#' Download and Save Climate Data (Parquet Format, Multiple Models and Variables)
#'
#' Downloads daily climate data for a set of locations defined by country ISO3 codes,
#' custom sf polygon shapes, or explicit lat/lon points. Data is saved as Parquet files,
#' one per variable per model per unit (country/shape/points group).
#'
#' @param PATHS A list containing paths where raw and processed data are stored.
#' PATHS is typically the output of the `get_paths()` function and should include:
#' \itemize{
#'   \item \strong{DATA_SHAPEFILES}: Path to the directory containing country shapefiles (required for \code{iso_codes} mode).
#'   \item \strong{DATA_CLIMATE}: Path to the directory where processed climate data will be saved.
#' }
#' @param iso_codes A character vector of ISO3 country codes. Exactly one of \code{iso_codes}, \code{shapes}, or \code{points} must be supplied.
#' @param shapes A named list of sf polygon objects. Names become unit IDs in output filenames and a \code{region_id} column.
#' @param points A data.frame with columns \code{lat} and \code{lon}, and an optional \code{id} column. If \code{id} is absent, all points form a single unit with id \code{"custom"}.
#' @param api_key A character string representing the API key required to access the climate data API.
#' @param n_points An integer specifying the number of grid points to generate within each country/shape. Not used in \code{points} mode.
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
#' @return The function does not return a value. It downloads the climate data for each unit and climate model,
#' saving the results as Parquet files in the specified directory. One file is saved per climate variable per
#' model per unit, named \code{climate_data_{climate_model}_{climate_variable}_{date_start}_{date_stop}_{unit_id}.parquet}.
#'
#' @details
#' Exactly one of \code{iso_codes}, \code{shapes}, or \code{points} must be supplied.
#'
#' \strong{Country mode} (\code{iso_codes}): Reads country shapefiles from \code{PATHS$DATA_SHAPEFILES}, generates a
#' grid of \code{n_points} points, and adds \code{country_name} and \code{iso_code} columns to output.
#'
#' \strong{Shapes mode} (\code{shapes}): Accepts a named list of sf polygons, generates a grid of \code{n_points}
#' points per polygon, and adds a \code{region_id} column to output.
#'
#' \strong{Points mode} (\code{points}): Accepts a data.frame with \code{lat}, \code{lon}, and optional \code{id}
#' columns. Points sharing the same \code{id} are downloaded together. Adds a \code{region_id} column to output.
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
#' PATHS <- get_paths()
#'
#' # Country mode
#' download_climate_data(PATHS, iso_codes = c("ZAF", "KEN"), n_points = 5,
#'                       date_start = "1970-01-01", date_stop = "2030-12-31",
#'                       climate_models = "MRI_AGCM3_2_S",
#'                       climate_variables = c("temperature_2m_mean", "precipitation_sum"),
#'                       api_key = "your-api-key")
#'
#' # Shapes mode
#' download_climate_data(PATHS, shapes = list(myregion = my_sf_polygon), n_points = 5,
#'                       date_start = "2020-01-01", date_stop = "2020-12-31",
#'                       climate_models = "MRI_AGCM3_2_S",
#'                       climate_variables = "temperature_2m_mean",
#'                       api_key = "your-api-key")
#'
#' # Points mode
#' pts <- data.frame(lat = c(1.0, 1.5), lon = c(36.0, 36.5), id = c("A", "A"))
#' download_climate_data(PATHS, points = pts,
#'                       date_start = "2020-01-01", date_stop = "2020-12-31",
#'                       climate_models = "MRI_AGCM3_2_S",
#'                       climate_variables = "temperature_2m_mean",
#'                       api_key = "your-api-key")
#'}
#'
#' @export

download_climate_data <- function(PATHS,
                                  iso_codes = NULL,
                                  n_points = NULL,
                                  date_start,
                                  date_stop,
                                  climate_models,
                                  climate_variables,
                                  api_key,
                                  shapes = NULL,
                                  points = NULL) {

     n_supplied <- sum(!is.null(iso_codes), !is.null(shapes), !is.null(points))
     if (n_supplied != 1L) stop("Exactly one of 'iso_codes', 'shapes', or 'points' must be supplied.")

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

     mode <- if (!is.null(iso_codes)) "country" else if (!is.null(shapes)) "shapes" else "points"

     if (is_free) {
          n_days <- as.integer(as.Date(date_stop) - as.Date(date_start)) + 1L
          n_units_est <- switch(mode,
               country = length(iso_codes),
               shapes  = length(shapes),
               points  = if ("id" %in% names(points)) length(unique(points$id)) else 1L
          )
          n_pts_est <- if (mode == "points") {
               if ("id" %in% names(points)) {
                    mean(as.integer(table(points$id)))
               } else {
                    nrow(points)
               }
          } else {
               n_points
          }
          estimated_calls <- length(climate_models) * n_units_est * n_pts_est *
                             length(climate_variables) * 263.5 * (n_days / 36500)
          if (estimated_calls > 0.8 * 300000L) {
               warning(glue::glue(
                    "Estimated API call-equivalent cost ({round(estimated_calls)}) exceeds ",
                    "80% of the free tier monthly limit (300000). ",
                    "The request may hit the free API limit."
               ))
          }
     }

     units <- if (mode == "country") {
          lapply(setNames(iso_codes, iso_codes), function(iso) {
               shp    <- sf::st_read(
                    dsn = file.path(PATHS$DATA_SHAPEFILES, paste0(iso, "_ADM0.shp")),
                    quiet = TRUE)
               coords <- as.data.frame(sf::st_coordinates(MOSAIC::generate_country_grid_n(shp, n_points)))
               list(id = iso, lat = coords$Y, lon = coords$X)
          })
     } else if (mode == "shapes") {
          lapply(names(shapes), function(nm) {
               coords <- as.data.frame(sf::st_coordinates(MOSAIC::generate_country_grid_n(shapes[[nm]], n_points)))
               list(id = nm, lat = coords$Y, lon = coords$X)
          })
     } else {
          pts <- if (!"id" %in% names(points)) transform(points, id = "custom") else points
          lapply(split(pts, pts$id), function(g) list(id = g$id[1], lat = g$lat, lon = g$lon))
     }

     n_cores <- max(1L, parallel::detectCores() - 1L)

     for (climate_model in climate_models) {

          process_unit <- function(unit) {
               message(glue::glue("Downloading climate data for {unit$id} using {climate_model} at {length(unit$lat)} points"))

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

               climate_data <- data.frame(
                    meta,
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
