#' Process WHO Vaccination Data
#'
#' This function processes WHO vaccination request data, redistributes doses based on a maximum daily rate, and saves the processed data
#' into a format suitable for use in the MOSAIC cholera model. It also fills in missing campaign dates, infers decision dates when needed,
#' calculates cumulative doses, and computes vaccination proportions.
#'
#' @param PATHS A list of file paths, which should include the following elements:
#' \describe{
#'   \item{DATA_SCRAPE_WHO_VACCINATION}{The path to the folder containing the scraped WHO vaccination data.}
#'   \item{DATA_DEMOGRAPHICS}{The path to the folder containing demographic data for population information.}
#'   \item{MODEL_INPUT}{The path to the folder where the processed data and parameter files will be saved.}
#' }
#' @param max_rate_per_day The maximum vaccination rate per day for dose redistribution. Default is 100,000 doses/day.
#'
#' @return This function returns no R object, but it saves two CSV files:
#' \itemize{
#'   \item A parameter data frame for the vaccination rate (nu) saved as \code{"param_nu_vaccination_rate.csv"}.
#'   \item The full redistributed vaccination data saved as \code{"data_vaccinations.csv"}.
#' }
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Loads WHO vaccination request data from 2016 to present and population data for 2023.
#'   \item Converts country names to ISO codes and removes rows for countries not included in MOSAIC's ISO code list.
#'   \item Infers missing campaign dates using the decision dates and calculates mean delays between decision and campaign dates.
#'   \item Redistributes shipped doses day by day, based on the maximum daily rate.
#'   \item Ensures that the full date range from the minimum distribution date to the current date is covered for all ISO codes, filling in any missing dates.
#'   \item Calculates cumulative doses, the proportion of the population vaccinated, and other related metrics.
#'   \item Merges population data and computes vaccination rates.
#'   \item Saves the processed vaccination data and parameter data frame for use in the MOSAIC cholera model.
#' }
#'
#' @examples
#' \dontrun{
#' PATHS <- list(
#'   DATA_SCRAPE_WHO_VACCINATION = "path/to/who_vaccination_data",
#'   DATA_DEMOGRAPHICS = "path/to/demographics",
#'   MODEL_INPUT = "path/to/save/processed/data"
#' )
#' process_WHO_vaccination_data(PATHS, max_rate_per_day = 100000)
#' }
#'
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
