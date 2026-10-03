#' Process GTFCC Vaccination Data for MOSAIC Model
#'
#' This function processes raw GTFCC vaccination data that has been scraped from the GTFCC OCV dashboard
#' by the ees-cholera-mapping repository. It transforms the event-based data structure into a clean, 
#' structured dataset matching the WHO vaccination data format for use in the MOSAIC model.
#'
#' @param PATHS A list containing file paths for input and output data. The list should include:
#' \itemize{
#'   \item \strong{MODEL_INPUT}: Path to the directory where processed vaccination data will be saved.
#' }
#'
#' @return The function saves the processed vaccination data to a CSV file in the directory specified by `PATHS$MODEL_INPUT`.
#' It also returns the processed data as a data frame for further use in R.
#'
#' @details
#' Each output row is one GTFCC request (\code{req_id}, e.g.
#' \code{"2019-I05-D01"}): \code{doses_shipped} is the sum of all its Delivery
#' events and \code{campaign_date} the first delivery date. Four columns carry
#' the request's delivery and round structure (MOSAIC v0.103.0); the round
#' columns are read from its Round events (\code{round_id}
#' \code{C<campaign>-R<round>}, \code{doses} = doses administered in that
#' round):
#' \describe{
#'   \item{\code{req_id}}{The GTFCC request identifier, for tracing a row back to the raw log.}
#'   \item{\code{delivery_schedule}}{The request's deliveries as
#'     \code{<date>:<doses>} pairs separated by \code{;}, in date order, with
#'     deliveries on the same date summed, e.g.
#'     \code{"2019-04-24:835200;2019-09-25:835200"}. Their doses sum to
#'     \code{doses_shipped}; \code{\link{est_vaccination_rate}} releases each
#'     delivery on its own date.}
#'   \item{\code{round_sequence}}{The request's doses as an ordered sequence of
#'     \code{<dose>:<weight>} blocks separated by \code{;}, e.g.
#'     \code{"1:357200;2:332900;1:562000;2:693900"}. \code{<dose>} is 1 for a
#'     first round (R01) and 2 for a second or later round (R02+), the blocks run
#'     in campaign order with R01 before R02 inside a campaign (campaign numbers
#'     follow administration order), consecutive rounds of the same dose are
#'     merged, and \code{<weight>} is the doses administered in the block (a
#'     campaign-round listed more than once counts once, with its reported
#'     doses). Weights are relative: \code{\link{est_vaccination_rate}} splits the
#'     shipped doses across the blocks in proportion to them, so a single block
#'     is written with weight 1. \code{NA} when the round is unknown.}
#'   \item{\code{round_basis}}{How the sequence was obtained:
#'     \code{"rounds"} (every round reports its doses, or all rounds are of one
#'     dose so the counts are not needed), \code{"rounds_imputed"} (the request
#'     has first and second rounds but some round dose counts are unreported;
#'     each unreported round carries the mean reported round of the request, or
#'     all rounds weigh equally when none is reported) or \code{"unknown"} (no
#'     Round event with a \code{C##-R##} identifier: the doses count as first
#'     doses downstream).}
#' }
#'
#' This function performs the following steps:
#' \enumerate{
#'   \item **Load and Transform GTFCC Data**:
#'     - Reads the scraped GTFCC data from ees-cholera-mapping repository.
#'     - Transforms event-based structure (Request, Decision, Delivery, Round events) to request-based structure.
#'   \item **Map to WHO Format**:
#'     - Aggregates events by request ID to extract doses requested, approved, and shipped.
#'     - Uses Delivery event dates as campaign dates.
#'     - Records each delivery's date and doses, and attributes the request's doses to first and second rounds from its Round events.
#'     - Converts country names to ISO codes for consistency.
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
#' \dontrun{
#' # Example usage
#' PATHS <- list(
#'   MODEL_INPUT = "path/to/model/input"
#' )
#'
#' processed_data <- process_GTFCC_vaccination_data(PATHS)
#' }
#'
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @importFrom base mean unique
#' @export

process_GTFCC_vaccination_data <- function(PATHS) {
     
     message('Loading GTFCC vaccination data')
     
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
               delivery_schedule <- .vacc_delivery_schedule(delivery_events$event_date,
                                                            delivery_events$doses)
          } else {
               campaign_date <- NA
               doses_shipped <- 0
               delivery_schedule <- NA_character_
          }
          
          # Set context - GTFCC data doesn't have this field, so we'll use a default
          context <- "Outbreak response"

          # First- and second-round doses from the request's Round events
          round_events <- req_events[req_events$event_type == "Round", ]
          rounds <- .vacc_round_sequence(round_events$round_id, round_events$doses)

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
               req_id = req_id,
               delivery_schedule = delivery_schedule,
               round_sequence = rounds$sequence,
               round_basis = rounds$basis,
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
     for (b in c("rounds", "rounds_imputed", "unknown")) {
          sel <- vaccination_data$round_basis == b
          message(sprintf("Round basis '%s': %d requests, %s doses shipped", b, sum(sel),
                          format(sum(vaccination_data$doses_shipped[sel]), big.mark = ",")))
     }

     # The request columns go last so the columns that predate them keep their positions
     request_cols <- c("req_id", "delivery_schedule", "round_sequence", "round_basis")
     vaccination_data <- vaccination_data[, c(setdiff(names(vaccination_data), request_cols), request_cols)]

     # Save processed vaccination data to CSV
     data_path <- file.path(PATHS$MODEL_INPUT, "data_vaccinations_GTFCC.csv")
     write.csv(vaccination_data, data_path, row.names = FALSE)
     message(paste("Processed GTFCC vaccination data saved to:", data_path))
     
     return(vaccination_data)
}
#' First- and second-round structure of one GTFCC request
#'
#' Builds the \code{round_sequence} and \code{round_basis} of a request from its
#' Round events (see \code{\link{process_GTFCC_vaccination_data}}). Rounds are
#' ordered by campaign and then round number -- campaign numbers follow
#' administration order and R01 precedes R02 within a campaign even where the
#' scraped dates disagree -- round 1 maps to dose 1 and rounds 2+ to dose 2,
#' consecutive rounds of the same dose are merged, and each block weighs its
#' administered doses. A campaign-round listed more than once counts once: the
#' sum of its reported doses, or unreported when none of its events reports any.
#' @param round_id Character \code{round_id} of the request's Round events.
#' @param doses Administered doses of the same events (\code{NA} = unreported).
#' @return A list with \code{sequence} (character, \code{NA} when unknown) and
#'   \code{basis} (\code{"rounds"}, \code{"rounds_imputed"} or \code{"unknown"}).
#' @noRd
.vacc_round_sequence <- function(round_id, doses) {
     unknown <- list(sequence = NA_character_, basis = "unknown")
     rid <- as.character(round_id)
     m <- regmatches(rid, regexec("^C([0-9]+)-R([0-9]+)$", rid))
     ok <- lengths(m) == 3L
     if (!any(ok)) return(unknown)
     campaign <- as.integer(vapply(m[ok], function(z) z[2], character(1)))
     round <- as.integer(vapply(m[ok], function(z) z[3], character(1)))
     d <- as.numeric(doses)[ok]

     # A campaign-round listed more than once counts once: its reported doses,
     # or unreported if none of its events reports any
     key <- paste(campaign, round)
     ukey <- unique(key)
     d <- vapply(ukey, function(k) {
          x <- d[key == k]
          if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
     }, numeric(1), USE.NAMES = FALSE)
     campaign <- campaign[match(ukey, key)]
     round <- round[match(ukey, key)]

     o <- order(campaign, round)
     dose <- ifelse(round[o] >= 2L, 2L, 1L)
     d <- d[o]

     # One dose throughout: the share is 100% whatever the dose counts say
     if (length(unique(dose)) == 1L) {
          return(list(sequence = paste0(dose[1], ":1"), basis = "rounds"))
     }
     basis <- "rounds"
     if (anyNA(d)) {
          basis <- "rounds_imputed"
          d[is.na(d)] <- if (any(!is.na(d))) mean(d, na.rm = TRUE) else 1
     }
     if (any(d < 0) || sum(d) <= 0) return(unknown)

     run <- cumsum(c(TRUE, diff(dose) != 0L))
     w <- round(as.numeric(tapply(d, run, sum)))
     if (sum(w) <= 0) w <- as.numeric(tapply(d, run, sum))
     block_dose <- as.integer(tapply(dose, run, function(z) z[1]))
     list(sequence = paste(paste0(block_dose, ":", format(w, scientific = FALSE, trim = TRUE)),
                           collapse = ";"),
          basis = basis)
}

#' Delivery schedule of one GTFCC request
#'
#' @param date Delivery event dates (Date).
#' @param doses Delivery event doses.
#' @return \code{"<date>:<doses>;..."} in date order with same-date deliveries
#'   summed, or \code{NA} when a delivery lacks a date or a dose count (the
#'   request is then released from its campaign date as one delivery).
#' @noRd
.vacc_delivery_schedule <- function(date, doses) {
     date <- as.Date(date)
     doses <- as.numeric(doses)
     if (!length(date) || anyNA(date) || anyNA(doses)) return(NA_character_)
     by_date <- tapply(doses, as.character(date), sum)
     paste(paste0(names(by_date), ":", format(as.numeric(by_date), scientific = FALSE, trim = TRUE)),
           collapse = ";")
}
