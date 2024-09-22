#' Process and Save OAG Flight Data for Africa
#'
#' This function processes raw OAG flight data for African countries, including data cleaning,
#' location and date conversion, and aggregation. The processed data is saved as CSV files for
#' all African countries and a subset of countries used in the MOSAIC modeling framework.
#'
#' @param PATHS A list containing the paths where raw and processed data are stored.
#' PATHS is typically the output of the `get_paths()` function and should include the following components:
#' \itemize{
#'   \item \strong{DATA_RAW}: Path to the directory containing the raw data files.
#'   \item \strong{DATA_PROCESSED}: Path to the directory where processed data should be saved.
#' }
#'
#' @return The function does not return a value. It processes the OAG flight data and saves the
#' aggregated results to the specified processed data paths.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Loads raw OAG flight data from a CSV file.
#'   \item Cleans and formats the data, including converting country codes and date information.
#'   \item Aggregates the data by year and calculates the mean weekly passenger counts.
#'   \item Saves the aggregated data for African countries and the MOSAIC framework countries as separate CSV files.
#' }
#' The output files are saved in the processed data directory defined in `PATHS$DATA_PROCESSED`.
#'
#' @importFrom lubridate ceiling_date days
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#'
#' @examples
#' \dontrun{
#' # Define paths for raw and processed data using get_paths()
#' PATHS <- get_paths()
#'
#' # Process the OAG data and save the results
#' process_OAG_data(PATHS)
#'}
#' @export

process_OAG_data <- function(PATHS) {

     # Ensure output directory exists, if not, create it
     if (!dir.exists(PATHS$DATA_CLIMATE)) {
          dir.create(PATHS$DATA_OAG, recursive = TRUE)
     }



     # Load necessary packages and functions
     message("Loading raw OAG flight data")
     d <- read.csv(file.path(PATHS$DATA_RAW, "OAG/oag_africa_JobId3062750.CSV"), stringsAsFactors = FALSE)

     # Remove columns with all NA values
     d <- d[, colSums(is.na(d)) != nrow(d)]

     message("Fixing location and date information")

     # Rename columns for easier access
     colnames(d)[colnames(d) == "Origin.country"] <- "origin_iso2"
     colnames(d)[colnames(d) == "Destination.country"] <- "destination_iso2"
     colnames(d)[colnames(d) == "Total.Est..Pax.3"] <- "count"
     colnames(d)[colnames(d) == "Timeseries.1"] <- "yearmon"

     d[is.na(d$origin_iso2), "origin_iso2"] <- "NA"
     d[is.na(d$destination_iso2), "destination_iso2"] <- "NA"

     # Convert ISO codes and country names
     d$origin_iso3 <- MOSAIC::convert_iso_codes(d$origin_iso2)
     d$origin_name <- MOSAIC::convert_iso_to_country(d$origin_iso2)
     d$destination_iso3 <- MOSAIC::convert_iso_codes(d$destination_iso2)
     d$destination_name <- MOSAIC::convert_iso_to_country(d$destination_iso2)

     # Convert year/month information and add date columns
     d$date_start <- as.Date(paste0(d$yearmon, "01"), format = "%Y%m%d")
     d$date_stop <- lubridate::ceiling_date(d$date_start, "month") - lubridate::days(1)
     d$year <- as.numeric(format(d$date_start, "%Y"))
     d$month <- as.numeric(format(d$date_start, "%m"))
     d$month_name <- format(d$date_start, "%B")

     # Keep relevant columns
     d <- d[, c("origin_iso2", "origin_iso3", "origin_name",
                "destination_iso2", "destination_iso3", "destination_name",
                "date_start", "date_stop", "year", "month", "month_name",
                "count")]

     message("Aggregating by year")

     # Aggregate data by year and calculate mean weekly passengers
     d <- aggregate(count ~ origin_iso2 + origin_iso3 + origin_name +
                         destination_iso2 + destination_iso3 + destination_name + year,
                    data = d, function(x) sum(x, na.rm=TRUE) / 52)

     message("Saving to file")

     # Select African and MOSAIC countries
     sel_africa <- d$origin_iso3 %in% MOSAIC::iso_codes_africa & d$destination_iso3 %in% MOSAIC::iso_codes_africa
     sel_mosaic <- d$origin_iso3 %in% MOSAIC::iso_codes_mosaic & d$destination_iso3 %in% MOSAIC::iso_codes_mosaic

     d_africa <- d[sel_africa,]
     d_mosaic <- d[sel_mosaic,]

     # Check if all ISO codes for Africa and MOSAIC are present
     if (!(all(MOSAIC::iso_codes_africa %in% d_africa$origin_iso3))) {
          sel <- !(MOSAIC::iso_codes_africa %in% unique(d_africa$origin_iso3))
          missing <- unique(d_africa$origin_iso3)[sel]
          stop(glue("Some iso codes in MOSAIC::iso_codes_africa are missing: {missing}"))
     }

     if (!(all(MOSAIC::iso_codes_mosaic %in% d_mosaic$origin_iso3))) {
          sel <- !(MOSAIC::iso_codes_mosaic %in% unique(d_mosaic$origin_iso3))
          missing <- unique(d_mosaic$origin_iso3)[sel]
          stop(glue("Some iso codes in MOSAIC::iso_codes_mosaic are missing: {missing}"))
     }

     # Define paths for saving processed data
     path_africa <- file.path(PATHS$DATA_OAG, "oag_africa_2017_mean_weekly.csv")
     path_mosaic <- file.path(PATHS$DATA_OAG, "oag_mosaic_2017_mean_weekly.csv")

     # Save processed data as CSV
     write.csv(d_africa, file=path_africa, row.names = FALSE)
     write.csv(d_mosaic, file=path_mosaic, row.names = FALSE)

     message("OAG data saved here:")
     message(path_africa)
     message(path_mosaic)
}
