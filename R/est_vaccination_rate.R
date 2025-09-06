#' Estimate OCV Vaccination Rates
#'
#' This function processes vaccination data from WHO or GTFCC, redistributes doses based on a maximum daily rate, and calculates vaccination parameters for use in the MOSAIC cholera model. The processed data includes redistributed daily doses, cumulative doses, and the proportion of the population vaccinated. The results are saved as CSV files for downstream modeling.
#'
#' @param PATHS A list containing file paths, including:
#' \describe{
#'   \item{DATA_SCRAPE_WHO_VACCINATION}{The path to the folder containing WHO vaccination data.}
#'   \item{DATA_DEMOGRAPHICS}{The path to the folder containing demographic data.}
#'   \item{MODEL_INPUT}{The path to the folder where processed data will be saved.}
#' }
#' @param date_start The start date for the vaccination data range (in "YYYY-MM-DD" format). Defaults to the earliest date in the data.
#' @param date_stop The stop date for the vaccination data range (in "YYYY-MM-DD" format). Defaults to the latest date in the data.
#' @param max_rate_per_day The maximum vaccination rate per day used to redistribute doses. Default is 100,000 doses/day.
#' @param data_source The source of the vaccination data. Must be one of \code{"WHO"}, \code{"GTFCC"}, or \code{"BOTH"}. When \code{"BOTH"} is specified, the function uses combined data from both sources with GTFCC prioritized and unique WHO campaigns added.
#'
#' @return This function does not return an R object but saves the following files to the directory specified in \code{PATHS$MODEL_INPUT}:
#' \itemize{
#'   \item A redistributed vaccination data file named \code{"data_vaccinations_<suffix>_redistributed.csv"} where suffix is WHO, GTFCC, or GTFCC_WHO.
#'   \item A parameter data frame for the vaccination rate (nu) named \code{"param_nu_vaccination_rate_<suffix>.csv"}.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item **Load Vaccination Data**:
#'     - Reads processed vaccination data from WHO or GTFCC and filters for relevant columns (\code{iso_code}, \code{campaign_date}, \code{doses_shipped}).
#'   \item **Redistribute Doses**:
#'     - Redistributes shipped doses day by day based on a maximum daily rate (\code{max_rate_per_day}).
#'     - Ensures no duplication of \code{distribution_date} within \code{iso_code}.
#'   \item **Validate Redistribution**:
#'     - Checks that the redistributed doses sum to the total shipped doses.
#'   \item **Ensure Full Coverage**:
#'     - Ensures all ISO codes have data across the full date range (\code{date_start} to \code{date_stop}), filling missing dates with zero doses.
#'   \item **Calculate Population Metrics**:
#'     - Merges population data for 2023, calculates cumulative doses, and computes the proportion of the population vaccinated.
#'   \item **Save Outputs**:
#'     - Saves the redistributed vaccination data and the vaccination rate parameter data frame.
#' }
#'
#' @examples
#' \dontrun{
#' PATHS <- list(
#'   DATA_SCRAPE_WHO_VACCINATION = "path/to/who_vaccination_data",
#'   DATA_DEMOGRAPHICS = "path/to/demographics",
#'   MODEL_INPUT = "path/to/save/processed/data"
#' )
#' est_vaccination_rate(PATHS, max_rate_per_day = 100000, data_source = "WHO")
#' }
#'
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @importFrom base mean unique
#' @export


est_vaccination_rate <- function(PATHS,
                                 date_start=NULL,
                                 date_stop=NULL,
                                 max_rate_per_day,
                                 data_source) {


     if (data_source == "WHO") {

          message("Loading processed WHO ICG vaccination data")
          data_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_WHO.csv")
          vaccination_data <- read.csv(data_path, stringsAsFactors = FALSE)

     } else if (data_source == "GTFCC") {

          message("Loading processed GTFCC vaccination data")
          data_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_GTFCC.csv")
          vaccination_data <- read.csv(data_path, stringsAsFactors = FALSE)

     } else if (data_source == "BOTH") {

          message("Loading combined GTFCC+WHO vaccination data")
          # Check if combined file exists, if not create it
          combined_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_GTFCC_WHO.csv")
          if (!file.exists(combined_path)) {
               message("Combined data not found, creating it now...")
               combine_vaccination_data(PATHS)
          }
          data_path <- combined_path
          vaccination_data <- read.csv(data_path, stringsAsFactors = FALSE)

     } else {

          stop("data_source must be one of: 'WHO', 'GTFCC', or 'BOTH'")

     }

     vaccination_data <- vaccination_data[,c('iso_code', 'campaign_date', 'doses_shipped')]
     vaccination_data$campaign_date <- as.Date(vaccination_data$campaign_date)






     message(glue::glue('Redistributing vaccine doses using the maximum daily vaccination rate of {max_rate_per_day} per day'))

     # Create an empty data frame to store the redistributed data
     redistributed_data <- vaccination_data[0, ]  # Same structure, but initially empty

     # Iterate through each unique ISO code (country)
     for (iso_code in unique(vaccination_data$iso_code)) {

          # Subset data for the specific ISO code (country)
          country_data <- vaccination_data[vaccination_data$iso_code == iso_code, ]

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




     message("Checking for duplicated distribution dates")
     dups <- which(duplicated(redistributed_data[,c("iso_code", "distribution_date")]))
     if (length(dups) > 0) {

          message(glue::glue("Aggregating the {length(dups)} duplicated distribution dates found:"))
          tmp <- unique(redistributed_data[dups, c("iso_code", "distribution_date")])
          row.names(tmp) <- NULL
          message(paste0(capture.output(tmp), collapse = "\n"))

          redistributed_data <- aggregate(
               doses_distributed ~ iso_code + distribution_date,
               data = redistributed_data,
               FUN = function(x) {
                    if (is.numeric(x)) {
                         sum(x, na.rm = TRUE)  # Sum numeric columns, ignoring NA
                    } else {
                         unique(x)             # Retain unique non-numeric values
                    }
               }
          )

     }


     tot_doses_data <- sum(vaccination_data$doses_shipped, na.rm = TRUE)
     tot_doses_redist <- sum(redistributed_data$doses_distributed, na.rm = TRUE)
     if (tot_doses_redist != tot_doses_data) {
          stop(glue("doses_distributed ({tot_doses_redist}) do not sum to doses_shipped ({tot_doses_data})"))
     } else {
          message("Redistribution successful")
     }



     # Ensure all dates from date_start to Sys.Date() are included for all ISO codes
     iso_with_data <- unique(redistributed_data$iso_code)
     message("MOSAIC locations WITH vaccination data:")
     message(paste(iso_with_data, collapse = ", "))
     message("MOSAIC locations WITHOUT vaccination data:")
     message(paste(MOSAIC::iso_codes_mosaic[!MOSAIC::iso_codes_mosaic %in% iso_with_data], collapse = ", "))

     extras <- iso_with_data[!iso_with_data %in% MOSAIC::iso_codes_mosaic]
     if (length(extras) > 0) {
          message("Locations WITH vaccination data NOT in MOSAIC locations:")
          message(paste(extras, collapse = ", "))
     }

     # Set date range for vaccination parameter
     date_min <- as.Date(min(c(vaccination_data$campaign_date, redistributed_data$distribution_date), na.rm = TRUE))
     date_max <- as.Date(max(c(vaccination_data$campaign_date, redistributed_data$distribution_date), na.rm = TRUE))

     if (is.null(date_start)) date_start <- date_min
     if (is.null(date_stop)) date_stop <- date_max

     if (date_start > date_min) warning(glue::glue("date_start ({date_start}) is later than global minimum distribution date in vaccination data ({date_min})"))
     if (date_stop < date_max) warning(glue::glue("date_stop ({date_stop}) is earlier than global maximum distribution date in vaccination data ({date_max})"))

     full_date_range <- as.Date(seq(as.Date(date_start), as.Date(date_stop), by = "day"))

     message("Building redistributed vaccination data that is square across all locations and full date range")
     list_redistributed_data_square <- list()


     for (iso_code in MOSAIC::iso_codes_mosaic) {

          country_data_full <- data.frame(iso_code = iso_code,
                                          distribution_date = full_date_range,
                                          doses_distributed = 0)

          if (iso_code %in% redistributed_data$iso_code) {

               country_data_redistributed <- redistributed_data[redistributed_data$iso_code == iso_code, ]
               if (any(table(country_data_redistributed$distribution_date) > 1)) stop(glue::glue('{iso_code}: duplicated distribution dates found'))

               check <- all(country_data_redistributed$distribution_date %in% country_data_full$distribution_date)
               if (!check) warning(glue::glue('{iso_code}: redistributed data contains dates not found in square data'))

               for (i in 1:nrow(country_data_redistributed)) {

                    sel <- which(country_data_full$distribution_date == country_data_redistributed$distribution_date[i])
                    country_data_full[sel, 'doses_distributed'] <- country_data_redistributed[i, 'doses_distributed']
               }

               check <- sum(country_data_redistributed$doses_distributed) == sum(country_data_full$doses_distributed)
               if (!check) stop(glue::glue("{iso_code}: redistributed doses not equal after squaring dates"))
               message(iso_code)

          }

          country_data_full$doses_distributed_cumulative <- cumsum(country_data_full$doses_distributed)
          list_redistributed_data_square <- c(list_redistributed_data_square, list(country_data_full))

     }

     redistributed_data_square <- do.call(rbind, list_redistributed_data_square)
     rm(list_redistributed_data_square)

     redistributed_data_square <- redistributed_data_square %>% arrange(iso_code, distribution_date)




     # Merge population data into redistributed data by iso_code

     # Load population data and keep data for the year 2023
     message("Calculating proportion vaccinated")
     message('Loading vaccination and population data')
     pop_data <- read.csv(file.path(PATHS$DATA_DEMOGRAPHICS, 'demographics_africa_2000_2023.csv'), stringsAsFactors = FALSE)
     pop_data <- pop_data[pop_data$year == 2023, c('iso_code', 'population')]
     message('NOTE: population sizes based on 2023')

     sel <- redistributed_data_square$distribution_date >= min(redistributed_data$distribution_date) &
          redistributed_data_square$distribution_date <= max(redistributed_data$distribution_date)
     redistributed_data <- redistributed_data_square[sel,]

     redistributed_data <- merge(redistributed_data, pop_data, by = "iso_code", all.x = TRUE)

     # Calculate the proportion of the population vaccinated for distributed doses
     redistributed_data$prop_vaccinated <- redistributed_data$doses_distributed_cumulative / redistributed_data$population


     redistributed_data <- redistributed_data[order(redistributed_data$iso_code, redistributed_data$distribution_date),]

     redistributed_data$country <- MOSAIC::convert_iso_to_country(redistributed_data$iso_code)

     redistributed_data$date <- redistributed_data$distribution_date

     cols <- c('country', 'iso_code', 'date', 'doses_distributed', 'doses_distributed_cumulative', 'prop_vaccinated')
     redistributed_data <- redistributed_data[,cols]



     # Save redistributed vaccination data to CSV
     # Use appropriate suffix for combined data
     suffix <- ifelse(data_source == "BOTH", "GTFCC_WHO", data_source)
     data_path <- file.path(PATHS$MODEL_INPUT, glue::glue("data_vaccinations_{suffix}_redistributed.csv"))
     write.csv(redistributed_data, data_path, row.names = FALSE)
     message(paste("Redistributed vaccination data saved to:", data_path))


     # Prepare parameter data frame for vaccination rate (nu)
     param_df <- MOSAIC::make_param_df(
          variable_name = 'nu',
          variable_description = 'vaccination rate absolute value',
          parameter_distribution = 'point',
          parameter_name = 'mean',
          j = redistributed_data_square$iso_code,
          t = redistributed_data_square$distribution_date,
          parameter_value = redistributed_data_square$doses_distributed
     )

     # Save parameter data frame to CSV
     param_path <- file.path(PATHS$MODEL_INPUT, glue::glue("param_nu_vaccination_rate_{suffix}.csv"))
     write.csv(param_df, param_path, row.names = FALSE)
     message(paste("Parameter data frame for vaccination rate (nu) saved to:", param_path))



}
