#' Get ENSO (Niño3, Niño3.4, and Niño4) Forecast Data
#'
#' This function retrieves manually extracted ENSO forecast data, including Niño3, Niño3.4, and Niño4 sea surface temperature (SST) anomalies, from the Bureau of Meteorology.
#'
#' @return A data frame with columns for year, month (numeric), month_name, variable (ENSO3, ENSO34, and ENSO4), and value.
#'
#' @details The ENSO forecast is manually extracted from the Bureau of Meteorology's ocean outlook page. The data includes SST anomalies for Niño3, Niño3.4, and Niño4 regions. Negative values indicate cooler sea surface temperatures, while positive values indicate warmer temperatures.
#'
#' @examples
#' \dontrun{
#' # Get the ENSO forecast data
#' enso_forecast <- get_ENSO_forecast()
#'
#' # Display the ENSO forecast data
#' print(enso_forecast)
#'}
#' @export

get_ENSO_forecast <- function() {

     # Manually copy and paste ENSO forecast from http://www.bom.gov.au/climate/ocean/outlooks/
     message("NOTE: ENSO forecast manually extracted from the Bureau of Meteorology's ocean outlook page.")
     message("http://www.bom.gov.au/climate/ocean/outlooks/#region=NINO4&region=NINO3&region=NINO34")


     # Update 1
     nino3_4_text_1 <- "
Month	Oct 2024	Nov 2024	Dec 2024	Jan 2025	Feb 2025
Niño3.4	−0.4℃	−0.3℃	−0.3℃	−0.1℃	0.1℃
below −0.8℃	1.0%	5.1%	2.0%	1.0%	1.0%
neutral	99.0%	94.9%	98.0%	99.0%	94.9%
above 0.8℃	0%	0%	0%	0%	4.0%
     "
     nino3_text_1 <- "
Month	Oct 2024	Nov 2024	Dec 2024	Jan 2025	Feb 2025
Niño3	−0.2℃	−0.2℃	−0.2℃	0℃	0.3℃
below −0.8℃	1.0%	2.0%	1.0%	1.0%	1.0%
neutral	99.0%	98.0%	99.0%	98.0%	82.8%
above 0.8℃	0%	0%	0%	1.0%	16.2%
     "

     # Niño4 probabilities
     nino4_text_1 <- "
Month	Oct 2024	Nov 2024	Dec 2024	Jan 2025	Feb 2025
Niño4	0.1℃	0.1℃	0.1℃	0.2℃	0.3℃
below −0.8℃	0%	0%	0%	0%	0%
neutral	100%	100%	100%	97.0%	97.0%
above 0.8℃	0%	0%	0%	3.0%	3.0%
     "


     # update 2

     nino3_4_text_2 <- "
     Month	Jun 2025	Jul 2025	Aug 2025	Sep 2025	Oct 2025	Nov 2025
Niño3.4	0.3 ℃	0.5 ℃	0.4 ℃	0.4 ℃	0.5 ℃	0.5 ℃
below −0.8 ℃	0%	0%	0%	0%	0%	0%
neutral	100%	98.0%	88.9%	76.8%	70.7%	70.7%
above 0.8 ℃	0%	2.0%	11.1%	23.2%	29.3%	29.3%
     "

     nino3_text_2 <- "
     Month	Jun 2025	Jul 2025	Aug 2025	Sep 2025	Oct 2025	Nov 2025
Niño3	0.3 ℃	0.5 ℃	0.5 ℃	0.5 ℃	0.5 ℃	0.6 ℃
below −0.8 ℃	0%	0%	0%	0%	1.0%	0%
neutral	100%	86.9%	77.8%	74.7%	67.7%	61.6%
above 0.8 ℃	0%	13.1%	22.2%	25.3%	31.3%	38.4%
     "

     nino4_text_2 <- "
     Month	Jun 2025	Jul 2025	Aug 2025	Sep 2025	Oct 2025	Nov 2025
Niño4	0.4 ℃	0.5 ℃	0.5 ℃	0.4 ℃	0.5 ℃	0.6 ℃
below −0.8 ℃	0%	0%	0%	0%	0%	0%
neutral	100%	100%	91.9%	88.9%	86.9%	76.8%
above 0.8 ℃	0%	0%	8.1%	11.1%	13.1%	23.2%
     "

# Update 3
     nino3_4_text_3 <- "
Month	Sep 2025	Oct 2025	Nov 2025	Dec 2025	Jan 2026	Feb 2026
R-Niño3.4	−0.9 ℃	−1.0 ℃	−0.9 ℃	−0.7 ℃	−0.5 ℃	−0.1 ℃
below −0.8 ℃	53.5%	73.7%	45.5%	30.3%	14.1%	6.1%
neutral	46.5%	26.3%	54.5%	69.7%	85.9%	92.9%
above 0.8 ℃	0%	0%	0%	0%	0%	1.0%
     "

     # Niño3 probabilities
     nino3_text_3 <- "
Month	Sep 2025	Oct 2025	Nov 2025	Dec 2025	Jan 2026	Feb 2026
R-Niño3	−0.7 ℃	−0.9 ℃	−0.9 ℃	−0.8 ℃	−0.4 ℃	0.1 ℃
below −0.8 ℃	21.2%	58.6%	56.6%	37.4%	22.2%	4.0%
neutral	78.8%	41.4%	43.4%	62.6%	77.8%	89.9%
above 0.8 ℃	0%	0%	0%	0%	0%	6.1%
     "

     # Niño4 probabilities
     nino4_text_3 <- "
Month	Sep 2025	Oct 2025	Nov 2025	Dec 2025	Jan 2026	Feb 2026
R-Niño4	−0.6 ℃	−0.7 ℃	−0.6 ℃	−0.3 ℃	0 ℃	0.1 ℃
below −0.8 ℃	0%	10.1%	9.1%	0%	0%	0%
neutral	100%	89.9%	90.9%	100%	99.0%	97.0%
above 0.8 ℃	0%	0%	0%	0%	1.0%	3.0%
     "

     # Helper function to process each ENSO text chunk
     process_enso_text <- function(text_data, variable_name) {

          # Split the text data into lines
          lines <- strsplit(text_data, "\n")[[1]]
          if (any(lines == "")) lines <- lines[-which(lines == "")]

          # Process the lines - data is tab-separated
          month_line <- lines[grep("Month", lines)]
          month_year <- unlist(strsplit(month_line, "\t"))[-1]  # Remove "Month" word, split on tabs
          
          enso_line <- lines[grep("Niño|R-Niño", lines)]
          enso_values <- unlist(strsplit(enso_line, "\t"))[-1]  # Extract ENSO values, split on tabs

          # Replace special minus sign (−) with ASCII minus sign (-) and remove degree Celsius symbol
          enso_values <- gsub("−", "-", enso_values)  # Fix negative signs
          enso_values <- gsub("℃", "", enso_values)  # Remove degree Celsius symbol

          # Remove empty strings and standalone degree symbols that may remain
          enso_values <- enso_values[enso_values != "" & enso_values != " "]

          # Parse month-year combinations like "Jun 2025", "Jul 2025"
          month_year_parsed <- strsplit(month_year, " ")
          months <- sapply(month_year_parsed, function(x) x[1])
          years <- sapply(month_year_parsed, function(x) x[2])

          # Convert month names to numeric (1-12)
          numeric_months <- match(months, month.abb)

          # Convert ENSO values to numeric, filtering out NAs
          enso_values_numeric <- as.numeric(enso_values)
          enso_values_clean <- enso_values_numeric[!is.na(enso_values_numeric)]

          # Ensure we have matching lengths
          min_length <- min(length(numeric_months), length(years), length(months), length(enso_values_clean))
          if (min_length == 0) {
               warning("No valid data extracted for variable: ", variable_name)
               return(data.frame())
          }

          # Create a data frame with year, month (numeric), month_name, variable, and value
          out <- data.frame(
               year = as.integer(years[1:min_length]),
               month = numeric_months[1:min_length],
               month_name = months[1:min_length],
               variable = variable_name,
               value = enso_values_clean[1:min_length],
               stringsAsFactors = FALSE
          )

          return(out)
     }

     # Process all three updates for each variable
     message("Processing multiple forecast updates - newer forecasts will override older ones for overlapping dates")
     
     # Process each update for ENSO34
     nino3_4_df_1 <- process_enso_text(nino3_4_text_1, "ENSO34")
     nino3_4_df_2 <- process_enso_text(nino3_4_text_2, "ENSO34") 
     nino3_4_df_3 <- process_enso_text(nino3_4_text_3, "ENSO34")
     
     # Process each update for ENSO3
     nino3_df_1 <- process_enso_text(nino3_text_1, "ENSO3")
     nino3_df_2 <- process_enso_text(nino3_text_2, "ENSO3")
     nino3_df_3 <- process_enso_text(nino3_text_3, "ENSO3")
     
     # Process each update for ENSO4  
     nino4_df_1 <- process_enso_text(nino4_text_1, "ENSO4")
     nino4_df_2 <- process_enso_text(nino4_text_2, "ENSO4")
     nino4_df_3 <- process_enso_text(nino4_text_3, "ENSO4")
     
     # Combine and prioritize newer forecasts over older ones
     combine_with_recency_priority <- function(df_list, variable_name) {
          # Add update numbers (higher = newer)
          for (i in seq_along(df_list)) {
               if (nrow(df_list[[i]]) > 0) {
                    df_list[[i]]$update_priority <- i
               }
          }
          
          # Combine all updates
          combined <- do.call(rbind, df_list[sapply(df_list, nrow) > 0])
          
          if (nrow(combined) == 0) {
               return(data.frame())
          }
          
          # Create date column for sorting
          combined$date <- as.Date(paste(combined$year, combined$month, "01", sep = "-"))
          
          # Sort by date and update priority (newer updates last)
          combined <- combined[order(combined$date, combined$update_priority), ]
          
          # Remove duplicates, keeping last occurrence (newest update)
          deduplicated <- combined[!duplicated(combined[, c("date", "variable")], fromLast = TRUE), ]
          
          # Report any overlaps that were resolved
          duplicated_dates <- combined[duplicated(combined[, c("date", "variable")]) | 
                                     duplicated(combined[, c("date", "variable")], fromLast = TRUE), ]
          
          if (nrow(duplicated_dates) > 0) {
               unique_dates <- unique(duplicated_dates$date)
               date_range <- range(unique_dates)
               message(sprintf("  %s: %d overlapping months (%s to %s) - newer forecasts kept", 
                              variable_name, 
                              length(unique_dates),
                              format(date_range[1], "%Y-%m"), 
                              format(date_range[2], "%Y-%m")))
          }
          
          # Clean up columns
          deduplicated$date <- NULL
          deduplicated$update_priority <- NULL
          
          return(deduplicated)
     }
     
     # Apply to each variable
     nino3_4_final <- combine_with_recency_priority(list(nino3_4_df_1, nino3_4_df_2, nino3_4_df_3), "ENSO34")
     nino3_final <- combine_with_recency_priority(list(nino3_df_1, nino3_df_2, nino3_df_3), "ENSO3")
     nino4_final <- combine_with_recency_priority(list(nino4_df_1, nino4_df_2, nino4_df_3), "ENSO4")
     
     # Combine all variables into final result
     combined_df <- rbind(nino3_4_final, nino3_final, nino4_final)
     combined_df$year <- as.integer(combined_df$year)

     return(combined_df)

}
