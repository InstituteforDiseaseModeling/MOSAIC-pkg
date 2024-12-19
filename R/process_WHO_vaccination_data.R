#' Process Vaccination Data for MOSAIC Model
#'
#' This function processes raw vaccination data from WHO and associated population data to create a clean, structured dataset for the MOSAIC model. It handles data cleaning, infers missing campaign dates, validates the dataset, and saves the output for use in modeling cholera vaccination efforts.
#'
#' @param PATHS A list containing file paths for input and output data. The list should include:
#' \itemize{
#'   \item \strong{DATA_DEMOGRAPHICS}: Path to the directory where demographic data is stored.
#'   \item \strong{DATA_SCRAPE_WHO_VACCINATION}: Path to the directory containing raw WHO vaccination data.
#'   \item \strong{MODEL_INPUT}: Path to the directory where processed vaccination data will be saved.
#' }
#'
#' @return The function saves the processed vaccination data to a CSV file in the directory specified by `PATHS$MODEL_INPUT`. It also returns the processed data as a data frame for further use in R.
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item **Load and Filter Demographic Data**:
#'     - Reads demographic data for African countries from a CSV file.
#'     - Filters the data for the year 2023 and retains population sizes.
#'   \item **Load and Clean Vaccination Data**:
#'     - Reads raw WHO vaccination data.
#'     - Converts country names to ISO codes for consistency.
#'     - Filters the data to include only countries in the MOSAIC database.
#'   \item **Infer Missing Campaign Dates**:
#'     - Computes the delay between decision and campaign dates.
#'     - Infers missing campaign dates based on the mean delay where possible.
#'     - Fixes missing decision dates for grouped request numbers.
#'   \item **Validate and Filter Data**:
#'     - Ensures all rows have valid campaign dates.
#'     - Removes rows where `doses_shipped` is zero or missing.
#'   \item **Save Processed Data**:
#'     - Writes the cleaned and processed dataset to a CSV file for further modeling.
#' }
#'
#' @examples
#' # Example usage
#' PATHS <- list(
#'   DATA_DEMOGRAPHICS = "path/to/demographics",
#'   DATA_SCRAPE_WHO_VACCINATION = "path/to/who/vaccination",
#'   MODEL_INPUT = "path/to/model/input"
#' )
#'
#' processed_data <- process_WHO_vaccination_data(PATHS)
#'
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @importFrom base mean unique
#' @export

process_WHO_vaccination_data <- function(PATHS) {

     # Load population data and keep data for the year 2023
     message('Loading vaccination and population data')
     pop_data <- read.csv(file.path(PATHS$DATA_DEMOGRAPHICS, 'demographics_africa_2000_2023.csv'), stringsAsFactors = FALSE)
     pop_data <- pop_data[pop_data$year == 2023, c('iso_code', 'population')]
     message('NOTE: population sizes based on 2023')

     # Load vaccination data
     vaccination_data <- read.csv(file.path(PATHS$DATA_SCRAPE_WHO_VACCINATION, 'who_vaccination_data.csv'), stringsAsFactors = FALSE)

     vaccination_data$id <- 1:nrow(vaccination_data)
     vaccination_data$decision_date <- as.Date(vaccination_data$decision_date)
     vaccination_data$campaign_date <- as.Date(vaccination_data$campaign_date)
     vaccination_data <- vaccination_data[order(vaccination_data$request_number, decreasing = FALSE),]
     vaccination_data$iso_code <- MOSAIC::convert_country_to_iso(vaccination_data$country)
     vaccination_data$country <- MOSAIC::convert_iso_to_country(vaccination_data$iso_code)
     vaccination_data <- vaccination_data[vaccination_data$iso_code %in% MOSAIC::iso_codes_mosaic,]

     message("Inferring campaign dates where missing")
     vaccination_data$delay <- as.numeric(vaccination_data$campaign_date - vaccination_data$decision_date)
     vaccination_data$campaign_date_inferred <- vaccination_data$campaign_date

     # Fix missing decision dates by looping through request numbers
     sel <- which(is.na(vaccination_data$decision_date))
     if (length(sel) > 0) {

          message(glue::glue("Fixing {length(sel)} observations without a reported decision date"))

          # Loop through unique request numbers and fill NA decision dates with first data in round of requests
          for (request_num in unique(vaccination_data$request_number)) {

               subset_data <- vaccination_data[vaccination_data$request_number == request_num, ]
               unique_decision_dates <- unique(subset_data$decision_date[!is.na(subset_data$decision_date)])

               if (length(unique_decision_dates) == 1 & sum(is.na(subset_data$decision_date))) {
                    sel <- is.na(vaccination_data$decision_date) & vaccination_data$request_number == request_num
                    vaccination_data$decision_date[sel] <- unique_decision_dates
               }
          }

     }

     mean_delay <- as.integer(mean(vaccination_data$delay[vaccination_data$delay >= 0], na.rm = TRUE))
     message(glue::glue("Inferring missing campaign dates using the mean delay from decision date of {mean_delay} days"))
     sel <- vaccination_data$doses_shipped > 0 & is.na(vaccination_data$campaign_date)
     vaccination_data$campaign_date_inferred[sel] <- vaccination_data$decision_date[sel] + mean_delay

     # Keep only confirmed shipped doses
     vaccination_data <- vaccination_data[vaccination_data$doses_shipped != 0,]
     vaccination_data <- vaccination_data[!is.na(vaccination_data$doses_shipped),]

     if (any(is.na(vaccination_data$campaign_date_inferred))) {
          stop ('Could not resolve all campaign dates')
     } else {
          vaccination_data$campaign_date <- vaccination_data$campaign_date_inferred
          vaccination_data$campaign_date_inferred <- NULL
          vaccination_data$delay <- as.numeric(vaccination_data$campaign_date - vaccination_data$decision_date)
     }

     # Save redistributed vaccination data to CSV
     data_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_WHO.csv")
     write.csv(vaccination_data, data_path, row.names = FALSE)
     message(paste("Raw and redistributed vaccination data saved to:", data_path))


}
