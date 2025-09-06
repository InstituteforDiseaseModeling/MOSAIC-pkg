#' Process GTFCC Vaccination Data for MOSAIC Model
#'
#' This function processes raw GTFCC vaccination data that has been scraped from the GTFCC OCV dashboard
#' by the ees-cholera-mapping repository. It transforms the event-based data structure into a clean, 
#' structured dataset matching the WHO vaccination data format for use in the MOSAIC model.
#'
#' @param PATHS A list containing file paths for input and output data. The list should include:
#' \itemize{
#'   \item \strong{DATA_DEMOGRAPHICS}: Path to the directory where demographic data is stored.
#'   \item \strong{MODEL_INPUT}: Path to the directory where processed vaccination data will be saved.
#' }
#'
#' @return The function saves the processed vaccination data to a CSV file in the directory specified by `PATHS$MODEL_INPUT`. 
#' It also returns the processed data as a data frame for further use in R.
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item **Load and Transform GTFCC Data**:
#'     - Reads the scraped GTFCC data from ees-cholera-mapping repository.
#'     - Transforms event-based structure (Request, Decision, Delivery, Round events) to request-based structure.
#'   \item **Map to WHO Format**:
#'     - Aggregates events by request ID to extract doses requested, approved, and shipped.
#'     - Uses Delivery event dates as campaign dates.
#'     - Converts country names to ISO codes for consistency.
#'   \item **Load and Merge Demographic Data**:
#'     - Reads demographic data for African countries from a CSV file.
#'     - Filters the data for the year 2023 and retains population sizes.
#'   \item **Infer Missing Campaign Dates**:
#'     - Computes the delay between decision and campaign dates.
#'     - Infers missing campaign dates based on the mean delay where possible.
#'     - Fixes missing decision dates for grouped request numbers.
#'   \item **Validate and Filter Data**:
#'     - Ensures all rows have valid campaign dates.
#'     - Removes rows where `doses_shipped` is zero or missing.
#'     - Filters for MOSAIC countries only.
#'   \item **Save Processed Data**:
#'     - Writes the cleaned and processed dataset to a CSV file for further modeling.
#' }
#'
#' @examples
#' # Example usage
#' PATHS <- list(
#'   DATA_DEMOGRAPHICS = "path/to/demographics",
#'   MODEL_INPUT = "path/to/model/input"
#' )
#'
#' processed_data <- process_GTFCC_vaccination_data(PATHS)
#'
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @importFrom base mean unique
#' @export

process_GTFCC_vaccination_data <- function(PATHS) {
     
     # Load population data and keep data for the year 2023
     message('Loading GTFCC vaccination and population data')
     pop_data <- read.csv(file.path(PATHS$DATA_DEMOGRAPHICS, 'demographics_africa_2000_2023.csv'), stringsAsFactors = FALSE)
     pop_data <- pop_data[pop_data$year == 2023, c('iso_code', 'population')]
     message('NOTE: population sizes based on 2023')
     
     # Path to the scraped GTFCC data
     gtfcc_file <- file.path(PATHS$ROOT, "ees-cholera-mapping", "data", "cholera", 
                             "epicentre", "gtfcc", "cholera_vacc_requests.csv")
     
     if (!file.exists(gtfcc_file)) {
          stop(paste("GTFCC data file not found at:", gtfcc_file))
     }
     
     # Read the GTFCC data
     message("Processing GTFCC vaccination data from scraped source")
     gtfcc_raw <- read.csv(gtfcc_file, stringsAsFactors = FALSE)
     message(paste("Loaded", nrow(gtfcc_raw), "rows of GTFCC event data"))
     
     # Convert event_date to Date format
     gtfcc_raw$event_date <- as.Date(gtfcc_raw$event_date, format = "%Y-%m-%d")
     
     # Get unique request IDs
     unique_requests <- unique(gtfcc_raw$req_id)
     message(paste("Found", length(unique_requests), "unique vaccination requests"))
     
     # Initialize list to store processed requests
     processed_requests <- list()
     
     # Process each request
     for (req_id in unique_requests) {
          
          # Get all events for this request
          req_events <- gtfcc_raw[gtfcc_raw$req_id == req_id, ]
          
          # Extract country (should be same for all events in a request)
          country <- unique(req_events$country)[1]
          
          # Extract year from request ID (format: YYYY-IXX-DXX)
          year <- as.integer(substr(req_id, 1, 4))
          
          # Parse request number from req_id to create a unique numeric identifier
          # Convert format YYYY-IXX-DXX to YYYYXXX where XXX is the request number
          request_parts <- strsplit(req_id, "-")[[1]]
          if (length(request_parts) >= 2) {
               request_num <- gsub("I", "", request_parts[2])
               request_number <- as.integer(paste0(year, request_num))
          } else {
               request_number <- NA
          }
          
          # Get Request event data
          request_event <- req_events[req_events$event_type == "Request", ]
          doses_requested <- ifelse(nrow(request_event) > 0, 
                                   sum(request_event$doses, na.rm = TRUE), 
                                   NA)
          
          # Get Decision event data
          decision_event <- req_events[req_events$event_type == "Decision", ]
          if (nrow(decision_event) > 0) {
               decision_date <- decision_event$event_date[1]
               doses_approved <- sum(decision_event$doses, na.rm = TRUE)
               status <- "Approved"
          } else {
               decision_date <- NA
               doses_approved <- NA
               status <- "Pending"
          }
          
          # Get Delivery event data (use for campaign_date as specified)
          delivery_events <- req_events[req_events$event_type == "Delivery", ]
          if (nrow(delivery_events) > 0) {
               # Use the first delivery date as campaign date
               campaign_date <- min(delivery_events$event_date, na.rm = TRUE)
               doses_shipped <- sum(delivery_events$doses, na.rm = TRUE)
          } else {
               campaign_date <- NA
               doses_shipped <- 0
          }
          
          # Set context - GTFCC data doesn't have this field, so we'll use a default
          context <- "Outbreak response"
          
          # Create a row matching WHO format
          processed_row <- data.frame(
               year = year,
               country = country,
               request_number = request_number,
               status = status,
               context = context,
               decision_date = decision_date,
               doses_requested = doses_requested,
               doses_approved = doses_approved,
               doses_shipped = doses_shipped,
               campaign_date = campaign_date,
               stringsAsFactors = FALSE
          )
          
          # Add to list if we have valid data
          if (!is.na(doses_shipped) && doses_shipped > 0) {
               processed_requests[[length(processed_requests) + 1]] <- processed_row
          }
     }
     
     # Combine all processed requests into a single data frame
     vaccination_data <- do.call(rbind, processed_requests)
     row.names(vaccination_data) <- NULL
     
     # Add id column
     vaccination_data$id <- 1:nrow(vaccination_data)
     
     # Convert dates to Date format
     vaccination_data$decision_date <- as.Date(vaccination_data$decision_date)
     vaccination_data$campaign_date <- as.Date(vaccination_data$campaign_date)
     
     # Sort by request number
     vaccination_data <- vaccination_data[order(vaccination_data$request_number, decreasing = FALSE),]
     
     # Convert country names to ISO codes and standardize country names
     vaccination_data$iso_code <- MOSAIC::convert_country_to_iso(vaccination_data$country)
     vaccination_data$country <- MOSAIC::convert_iso_to_country(vaccination_data$iso_code)
     
     # Filter for MOSAIC countries only
     vaccination_data <- vaccination_data[vaccination_data$iso_code %in% MOSAIC::iso_codes_mosaic,]
     
     message("Inferring campaign dates where missing")
     vaccination_data$delay <- as.numeric(vaccination_data$campaign_date - vaccination_data$decision_date)
     vaccination_data$campaign_date_inferred <- vaccination_data$campaign_date
     
     # Fix missing decision dates by looping through request numbers
     sel <- which(is.na(vaccination_data$decision_date))
     if (length(sel) > 0) {
          
          message(glue::glue("Fixing {length(sel)} observations without a reported decision date"))
          
          # Loop through unique request numbers and fill NA decision dates with first date in round of requests
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
          warning('Some campaign dates could not be resolved and will be removed')
          vaccination_data <- vaccination_data[!is.na(vaccination_data$campaign_date_inferred),]
     }
     
     vaccination_data$campaign_date <- vaccination_data$campaign_date_inferred
     vaccination_data$campaign_date_inferred <- NULL
     vaccination_data$delay <- as.numeric(vaccination_data$campaign_date - vaccination_data$decision_date)
     
     # Print summary statistics
     message("\n=== GTFCC Vaccination Data Summary ===")
     message("Total number of observations: ", nrow(vaccination_data))
     message("Date range: ", min(vaccination_data$decision_date, na.rm = TRUE), 
             " to ", max(vaccination_data$decision_date, na.rm = TRUE))
     message("Total doses requested: ", format(sum(vaccination_data$doses_requested, na.rm = TRUE), 
                                               big.mark = ","))
     message("Total doses approved: ", format(sum(vaccination_data$doses_approved, na.rm = TRUE), 
                                              big.mark = ","))
     message("Total doses shipped: ", format(sum(vaccination_data$doses_shipped, na.rm = TRUE), 
                                             big.mark = ","))
     
     # Save processed vaccination data to CSV
     data_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_GTFCC.csv")
     write.csv(vaccination_data, data_path, row.names = FALSE)
     message(paste("Processed GTFCC vaccination data saved to:", data_path))
     
     return(vaccination_data)
}