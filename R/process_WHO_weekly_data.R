#' Process Weekly Cholera Data: Convert Country Names to ISO3 and Filter by WHO AFRO Region
#'
#' This function processes weekly cholera data by converting country names to ISO3 codes,
#' filtering the data for countries in the WHO AFRO region, and organizing the data by
#' year and week. It also generates start and stop dates for each ISO week.
#'
#' @param PATHS A list containing paths to the raw and processed data directories. Typically generated by the `get_paths()` function and should include:
#' \itemize{
#'   \item \strong{DATA_SCRAPE_WHO_WEEKLY}: Path to the raw cholera data file (e.g., `cholera_country_weekly.csv`).
#'   \item \strong{DATA_WHO_WEEKLY}: Path to save the processed cholera data (e.g., `cholera_country_weekly_processed.csv`).
#' }
#'
#' @return Invisibly returns \code{NULL}. Side effects:
#' \itemize{
#'   \item Reads raw data with \code{read.csv()}.
#'   \item Writes processed data to \code{PATHS$DATA_WHO_WEEKLY/cholera_country_weekly_processed.csv} via \code{write.csv()}.
#' }
#'
#' @details
#' This function performs the following tasks:
#' \itemize{
#'   \item Reads raw cholera data from a CSV file.
#'   \item Converts country names to ISO3 codes using the \code{MOSAIC::convert_country_to_iso} and \code{MOSAIC::convert_iso_to_country} functions.
#'   \item Filters the dataset to include only countries in the WHO AFRO region (based on \code{MOSAIC::iso_codes_who_afro}).
#'   \item Orders the data by year, week, and country.
#'   \item Renames columns for consistency: \code{cases_by_week} to \code{cases} and \code{deaths_by_week} to \code{deaths}.
#'   \item Adds \code{date_start} and \code{date_stop} columns based on ISO week, using the \code{ISOweek::ISOweek2date} function.
#'   \item Computes the \code{month} from \code{date_start}.
#'   \item Filters out countries with fewer than ten observations.
#'   \item Checks that no ISO week index 53 remains.
#' }
#'
#' @importFrom utils read.csv write.csv
#' @importFrom ISOweek ISOweek2date
#' @importFrom lubridate month
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' process_WHO_weekly_data(PATHS)
#' }
#' @export

process_WHO_weekly_data <- function(PATHS) {

     if (!dir.exists(PATHS$DATA_WHO_WEEKLY)) {
          dir.create(PATHS$DATA_WHO_WEEKLY, recursive = TRUE)
     }

     raw_data_path <- file.path(PATHS$DATA_SCRAPE_WHO_WEEKLY, "cholera_country_weekly.csv")
     message("Loading raw data from: ", raw_data_path)

     d <- utils::read.csv(raw_data_path, stringsAsFactors = FALSE)

     # Convert country names to ISO3 codes and back
     d$iso_code <- MOSAIC::convert_country_to_iso(d$country)
     d$country  <- MOSAIC::convert_iso_to_country(d$iso_code)

     # Filter the data to include only AFRO countries
     d <- d[d$iso_code %in% MOSAIC::iso_codes_who_afro, ]

     # Order the data by year, week, and country
     d <- d[order(d$year, d$week, d$country), ]

     # Rename columns for consistency
     colnames(d)[colnames(d) == "cases_by_week"]  <- "cases"
     colnames(d)[colnames(d) == "deaths_by_week"] <- "deaths"

     # Compute start and stop dates for each ISO week
     d$iso_week   <- paste0(d$year, "-W", sprintf("%02d", d$week))
     d$date_start <- ISOweek::ISOweek2date(paste0(d$iso_week, "-1"))
     d$date_stop  <- ISOweek::ISOweek2date(paste0(d$iso_week, "-7"))
     d$iso_week   <- NULL

     # Month of week-start
     d$month <- lubridate::month(as.Date(d$date_start))

     # Filter out countries with fewer than ten observations
     tmp  <- table(d$iso_code)
     keep <- names(tmp[tmp > 10])
     d    <- d[d$iso_code %in% keep, ]

     # Print the first few rows for verification
     print(head(d))
     message("Latest observation: ", max(d$date_stop, na.rm = TRUE))

     # Check for ISO-week index 53
     if (53 %in% d$week) {
          stop("week index is out of bounds")
     }

     # Save the processed data to the processed data directory
     processed_data_path <- file.path(PATHS$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv")
     utils::write.csv(d, file = processed_data_path, row.names = FALSE)

     message("Processed weekly cholera data saved to: ", processed_data_path)
}
