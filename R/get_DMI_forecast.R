#' Get DMI (Dipole Mode Index) Forecast Data
#'
#' This function retrieves DMI forecast data from JSON configuration files or
#' falls back to hardcoded values. DMI data comes from the Bureau of Meteorology's IOD forecast page.
#'
#' @param use_json Logical. Whether to try loading from JSON configuration first (default: TRUE)
#'
#' @return A data frame with columns for year, month (numeric), month_name, variable (DMI), and value. The DMI is a measure of the difference in sea surface temperature anomalies between the western and eastern Indian Ocean and is used to describe the Indian Ocean Dipole (IOD).
#'
#' @details This function first attempts to load forecast data from
#'   inst/extdata/dmi_forecast_current.json. If the JSON file doesn't exist or
#'   use_json=FALSE, it falls back to hardcoded values extracted from the Bureau
#'   of Meteorology's IOD forecast page. The data includes DMI values representing
#'   forecasts of sea surface temperature anomalies. Negative DMI values indicate
#'   cooler waters in the west, while positive DMI values indicate warmer waters
#'   in the west of the Indian Ocean.
#'
#' @examples
#' \dontrun{
#' # Get DMI forecast data (tries JSON first)
#' dmi_forecast <- get_DMI_forecast()
#'
#' # Force use of hardcoded values
#' dmi_forecast <- get_DMI_forecast(use_json = FALSE)
#'
#' # Display the DMI forecast data
#' print(dmi_forecast)
#'}
#' @export

get_DMI_forecast <- function(use_json = TRUE) {

    # Try JSON configuration first
    if (use_json) {
        tryCatch({
            return(get_DMI_forecast_from_json())
        }, error = function(e) {
            message("Could not load DMI forecast from JSON, using hardcoded values")
            message("Error: ", e$message)
        })
    }

     # Manually copy and paste IOD forecast from http://www.bom.gov.au/climate/ocean/outlooks/#region=IOD
     message("NOTE: IOD forecast manually extracted from the Bureau of Meteorology's ocean outlook page.")
     message("http://www.bom.gov.au/climate/ocean/outlooks/#region=IOD")

     text_data_1 <- "
IOD probabilities
Month	Oct 2024	Nov 2024	Dec 2024	Jan 2025	Feb 2025
IOD	−0.1	−0.3	−0.3	0	0
below −0.4	5.1%	33.3%	21.2%	3.0%	4.0%
neutral	91.9%	66.7%	75.8%	96.0%	88.9%
above 0.4	3.0%	0%	3.0%	1.0%	7.1%
     "

     text_data_2 <- "
Month	Jun 2025	Jul 2025	Aug 2025	Sep 2025	Oct 2025	Nov 2025
IOD	0 ℃	−0.3 ℃	−0.5 ℃	−0.5 ℃	−0.3 ℃	−0.2 ℃
below −0.4 ℃	0%	29.3%	51.5%	48.5%	40.4%	20.2%
neutral	98.0%	70.7%	46.5%	45.5%	49.5%	74.7%
above 0.4 ℃	2.0%	0%	2.0%	6.1%	10.1%	5.1%
     "

     text_data_3 <- "
Month	Sep 2025	Oct 2025	Nov 2025	Dec 2025	Jan 2026	Feb 2026
IOD	−0.9 ℃	−1.0 ℃	−0.6 ℃	−0.4 ℃	−0.2 ℃	0 ℃
below −0.4 ℃	100%	100%	78.8%	44.4%	10.1%	4.0%
neutral	0%	0%	21.2%	55.6%	87.9%	83.8%
above 0.4 ℃	0%	0%	0%	0%	2.0%	12.1%
     "

     message("Processing multiple DMI forecast updates - newer forecasts will override older ones for overlapping dates")
     
     # Helper function to process DMI text chunks
     process_dmi_text <- function(text_data) {
          # Split the text data into lines
          lines <- strsplit(text_data, "\n")[[1]]
          if (any(lines == "")) lines <- lines[-which(lines == "")]
          if (any(lines == "IOD probabilities")) lines <- lines[-which(lines == "IOD probabilities")]

          # Process the lines
          month_year <- unlist(strsplit(lines[grep("Month", lines)], "\\s+"))[-1]
          iod_values <- unlist(strsplit(lines[grep("IOD", lines)], "\\s+"))[-1]

          # Replace special minus sign (−) with ASCII minus sign (-) and remove degree Celsius symbol
          iod_values <- gsub("−", "-", iod_values)
          iod_values <- gsub("℃", "", iod_values)
          
          # Remove empty strings and standalone degree symbols
          iod_values <- iod_values[iod_values != "" & iod_values != " "]

          # Separate months and years correctly
          months <- month_year[seq(1, length(month_year), by = 2)]
          years <- month_year[seq(2, length(month_year), by = 2)]

          # Convert month names to numeric (1-12)
          numeric_months <- match(months, month.abb)

          # Convert IOD values to numeric, filtering out NAs
          iod_values <- as.numeric(iod_values)
          iod_values <- iod_values[!is.na(iod_values)]
          
          # Ensure we have matching lengths
          min_length <- min(length(numeric_months), length(years), length(months), length(iod_values))
          if (min_length == 0) {
               return(data.frame())
          }

          # Create data frame
          out <- data.frame(
               year = as.integer(years[1:min_length]),
               month = numeric_months[1:min_length],
               month_name = months[1:min_length],
               variable = "DMI",
               value = iod_values[1:min_length],
               stringsAsFactors = FALSE
          )

          return(out)
     }
     
     # Process all three data chunks
     dmi_df_1 <- process_dmi_text(text_data_1)
     dmi_df_2 <- process_dmi_text(text_data_2)
     dmi_df_3 <- process_dmi_text(text_data_3)
     
     # Combine with recency priority (newer updates override older ones)
     all_updates <- list(dmi_df_1, dmi_df_2, dmi_df_3)
     
     # Add update priorities
     for (i in seq_along(all_updates)) {
          if (nrow(all_updates[[i]]) > 0) {
               all_updates[[i]]$update_priority <- i
          }
     }
     
     # Combine all updates
     combined <- do.call(rbind, all_updates[sapply(all_updates, nrow) > 0])
     
     if (nrow(combined) == 0) {
          warning("No valid DMI data extracted from any update")
          return(data.frame(year = integer(), month = integer(), month_name = character(),
                           variable = character(), value = numeric()))
     }
     
     # Create date column for sorting
     combined$date <- as.Date(paste(combined$year, combined$month, "01", sep = "-"))
     
     # Sort by date and update priority (newer updates last)
     combined <- combined[order(combined$date, combined$update_priority), ]
     
     # Report overlaps
     duplicated_dates <- combined[duplicated(combined[, c("date", "variable")]) | 
                                duplicated(combined[, c("date", "variable")], fromLast = TRUE), ]
     
     if (nrow(duplicated_dates) > 0) {
          unique_dates <- unique(duplicated_dates$date)
          date_range <- range(unique_dates)
          message(sprintf("  DMI: %d overlapping months (%s to %s) - newer forecasts kept", 
                         length(unique_dates),
                         format(date_range[1], "%Y-%m"), 
                         format(date_range[2], "%Y-%m")))
     }
     
     # Remove duplicates, keeping last occurrence (newest update)
     deduplicated <- combined[!duplicated(combined[, c("date", "variable")], fromLast = TRUE), ]
     
     # Clean up columns
     out <- data.frame(
          year = as.integer(deduplicated$year),
          month = deduplicated$month,
          month_name = deduplicated$month_name,
          variable = deduplicated$variable,
          value = deduplicated$value,
          stringsAsFactors = FALSE
     )

     return(out)
}

