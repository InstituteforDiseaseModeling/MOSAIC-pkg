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

     # Niño3.4 probabilities
     nino3_4_text <- "
Month Oct 2024 Nov 2024 Dec 2024 Jan 2025 Feb 2025
Nino3.4 −0.4 −0.3 −0.3 −0.1 0.1
     "

     # Niño3 probabilities
     nino3_text <- "
Month Oct 2024 Nov 2024 Dec 2024 Jan 2025 Feb 2025
Nino3 −0.2 −0.2 −0.2 0 0.3
     "

     # Niño4 probabilities
     nino4_text <- "
Month Oct 2024 Nov 2024 Dec 2024 Jan 2025 Feb 2025
Nino4 0.1 0.1 0.1 0.2 0.3
     "

     # Helper function to process each ENSO text chunk
     process_enso_text <- function(text_data, variable_name) {

          # Split the text data into lines
          lines <- strsplit(text_data, "\n")[[1]]
          if (any(lines == "")) lines <- lines[-which(lines == "")]

          # Process the lines
          month_year <- unlist(strsplit(lines[grep("Month", lines)], "\\s+"))[-1]  # Extract month and year values combined
          enso_values <- unlist(strsplit(lines[grep("Niño", lines)], "\\s+"))[-1]  # Extract ENSO values

          # Replace special minus sign (−) with ASCII minus sign (-) and remove degree Celsius symbol
          enso_values <- gsub("−", "-", enso_values)  # Fix negative signs
          enso_values <- gsub("℃", "", enso_values)  # Remove degree Celsius symbol

          # Separate months and years correctly
          months <- month_year[seq(1, length(month_year), by = 2)]
          years <- month_year[seq(2, length(month_year), by = 2)]

          # Convert month names to numeric (1-12)
          numeric_months <- match(months, month.abb)

          # Convert ENSO values to numeric
          enso_values <- as.numeric(enso_values)

          # Create a data frame with year, month (numeric), month_name, variable, and value
          out <- data.frame(
               year = as.integer(years),
               month = numeric_months,
               month_name = months,
               variable = variable_name,
               value = enso_values,
               stringsAsFactors = FALSE
          )

          return(out)
     }

     # Process the text for Niño3.4, Niño3, and Niño4
     nino3_4_df <- process_enso_text(nino3_4_text, "ENSO34")
     nino3_df <- process_enso_text(nino3_text, "ENSO3")
     nino4_df <- process_enso_text(nino4_text, "ENSO4")

     # Combine all data frames into one
     combined_df <- as.data.frame(dplyr::bind_rows(nino3_4_df, nino3_df, nino4_df))
     combined_df$year <- as.integer(combined_df$year)

     return(combined_df)
}
