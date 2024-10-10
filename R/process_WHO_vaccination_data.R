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
#'   \item Loads WHO vaccination request data and population data for 2023.
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

process_WHO_vaccination_data <- function(PATHS, max_rate_per_day) {

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

     message("Convert country names to ISO codes")
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

     message('Redistributing vaccine doses')

     # Create an empty data frame to store the redistributed data
     redistributed_data <- vaccination_data[0, ]  # Same structure, but initially empty

     # Iterate through each unique ISO code (country)
     for (iso_code in unique(vaccination_data$iso_code)) {

          # Subset data for the specific ISO code (country)
          country_data <- vaccination_data[vaccination_data$iso_code == iso_code, ]
          country_data <- country_data[order(country_data$decision_date),]  # Ensure ordered by decision_date for redistribution

          # Loop through each record in the country_data
          for (i in 1:nrow(country_data)) {

               row <- country_data[i, ]
               remaining_doses <- row$doses_shipped  # Track remaining doses
               current_date <- row$campaign_date  # Start redistributing from the campaign date

               # Redistribute doses day by day
               while (remaining_doses > 0) {

                    # Determine how much to administer on the current day
                    daily_doses <- min(remaining_doses, max_rate_per_day)

                    # Create a new row with the current day's dose
                    new_row <- row
                    new_row$doses_distributed <- daily_doses
                    new_row$distribution_date <- current_date  # Assign the current decision date for the day

                    redistributed_data <- rbind(redistributed_data, new_row)

                    # Update remaining doses and increment the date for the next iteration
                    remaining_doses <- remaining_doses - daily_doses
                    current_date <- current_date + 1  # Increment the date correctly for each iteration
               }
          }
     }


     redistributed_data <- merge(
          redistributed_data[,c('country', 'iso_code',
                                'distribution_date', 'delay',
                                'doses_distributed')],
          vaccination_data[,c('country', 'iso_code', 'decision_date', 'campaign_date',
                              'doses_requested', 'doses_approved', 'doses_shipped')],
          by.x = c('country', 'iso_code', 'distribution_date'),
          by.y = c('country', 'iso_code', 'campaign_date'),
          all = TRUE
     )


     ### Adding missing dates from global_min_date to Sys.Date()

     # Get global minimum date across all countries
     global_min_date <- min(c(vaccination_data$decision_date, redistributed_data$distribution_date))

     # Ensure all dates from global_min_date to Sys.Date() are included for all ISO codes
     for (iso_code in unique(redistributed_data$iso_code)) {

          # Subset the data for the specific ISO code
          country_data <- redistributed_data[redistributed_data$iso_code == iso_code, ]

          # Create a sequence of dates from global_min_date to Sys.Date()
          full_date_range <- seq(global_min_date, Sys.Date(), by = "day")

          # Ensure every date in the range is covered, even if no doses were distributed on some days
          country_data <- merge(
               data.frame(iso_code = iso_code, distribution_date = as.Date(full_date_range)),
               country_data,
               by = c("iso_code", "distribution_date"),
               all.x = TRUE
          )

          country_data$doses_shipped[is.na(country_data$doses_shipped)] <- 0
          country_data$doses_distributed[is.na(country_data$doses_distributed)] <- 0

          # Bind the updated country_data back to redistributed_data
          redistributed_data <- rbind(redistributed_data[redistributed_data$iso_code != iso_code, ], country_data)
     }

     # Order by iso_code and distribution_date (new date variable)
     redistributed_data <- redistributed_data %>%
          arrange(iso_code, distribution_date)

     # Calculate cumulative doses and proportion of the population vaccinated
     redistributed_data <- redistributed_data %>%
          group_by(iso_code) %>%
          mutate(doses_shipped_cumulative = cumsum(doses_shipped),
                 doses_distributed_cumulative = cumsum(doses_distributed))  # Cumulative doses

     # Merge population data into redistributed data by iso_code
     redistributed_data <- merge(redistributed_data, pop_data, by = "iso_code", all.x = TRUE)

     # Calculate the proportion of the population vaccinated for distributed doses
     redistributed_data$prop_vaccinated <- redistributed_data$doses_distributed_cumulative / redistributed_data$population


     redistributed_data <- redistributed_data[order(redistributed_data$iso_code, redistributed_data$distribution_date),]

     redistributed_data$country <- MOSAIC::convert_iso_to_country(redistributed_data$iso_code)

     redistributed_data$date <- redistributed_data$distribution_date

     cols <- c('country', 'iso_code', 'date',
               'doses_requested', 'doses_approved', 'doses_shipped', 'doses_distributed',
               'doses_shipped_cumulative', 'doses_distributed_cumulative', 'prop_vaccinated')

     redistributed_data <- redistributed_data[,cols]





     # Prepare parameter data frame for vaccination rate (nu)
     param_df <- MOSAIC::make_param_df(
          variable_name = 'nu',
          variable_description = 'vaccination rate absolute value',
          parameter_distribution = 'point',
          parameter_name = 'mean',
          j = redistributed_data$iso_code,
          t = redistributed_data$distribution_date,
          parameter_value = redistributed_data$doses_distributed
     )

     # Save parameter data frame to CSV
     param_path <- file.path(PATHS$MODEL_INPUT, "param_nu_vaccination_rate.csv")
     write.csv(param_df, param_path, row.names = FALSE)
     message(paste("Parameter data frame for vaccination rate (nu) saved to:", param_path))

     # Save redistributed vaccination data to CSV
     data_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations.csv")
     write.csv(redistributed_data, data_path, row.names = FALSE)
     message(paste("Raw and redistributed vaccination data saved to:", data_path))


}
