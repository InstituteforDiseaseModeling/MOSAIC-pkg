#' Get DMI (Dipole Mode Index) Forecast Data
#'
#' This function retrieves the manually extracted Dipole Mode Index (DMI) forecast data from the Bureau of Meteorology's IOD forecast page.
#'
#' @return A data frame with columns for year, month (numeric), month_name, variable (DMI), and value. The DMI is a measure of the difference in sea surface temperature anomalies between the western and eastern Indian Ocean and is used to describe the Indian Ocean Dipole (IOD).
#'
#' @details The IOD forecast is manually extracted from the Bureau of Meteorology's IOD forecast page: \url{http://www.bom.gov.au/climate/ocean/outlooks/#region=IOD}. The data includes DMI values over a series of months and years, representing forecasts of sea surface temperature anomalies. Negative DMI values indicate cooler waters in the west, while positive DMI values indicate warmer waters in the west of the Indian Ocean.
#'
#' @examples
#' \dontrun{
#' # Get the DMI forecast data
#' dmi_forecast <- get_DMI_forecast()
#'
#' # Display the DMI forecast data
#' print(dmi_forecast)
#'}
#' @export

get_DMI_forecast <- function() {

     # Manually copy and paste IOD forecast from http://www.bom.gov.au/climate/ocean/outlooks/#region=IOD
     message("NOTE: IOD forecast manually extracted from the Bureau of Meteorology's ocean outlook page.")
     message("http://www.bom.gov.au/climate/ocean/outlooks/#region=IOD")

     text_data <- "
IOD probabilities
Month	Oct 2024	Nov 2024	Dec 2024	Jan 2025	Feb 2025
IOD	−0.1	−0.3	−0.3	0	0
below −0.4	5.1%	33.3%	21.2%	3.0%	4.0%
neutral	91.9%	66.7%	75.8%	96.0%	88.9%
above 0.4	3.0%	0%	3.0%	1.0%	7.1%
     "

     # Split the text data into lines
     lines <- strsplit(text_data, "\n")[[1]]
     if (any(lines == "")) lines <- lines[-which(lines == "")]
     if (any(lines == "IOD probabilities")) lines <- lines[-which(lines == "IOD probabilities")]

     # Process the lines
     month_year <- unlist(strsplit(lines[grep("Month", lines)], "\\s+"))[-1]  # Extract month and year values combined
     iod_values <- unlist(strsplit(lines[grep("IOD", lines)], "\\s+"))[-1]  # Extract IOD values

     # Replace special minus sign (−) with ASCII minus sign (-) and remove degree Celsius symbol
     iod_values <- gsub("−", "-", iod_values)  # Fix negative signs
     iod_values <- gsub("℃", "", iod_values)  # Remove degree Celsius symbol

     # Separate months and years correctly
     months <- month_year[seq(1, length(month_year), by = 2)]
     years <- month_year[seq(2, length(month_year), by = 2)]

     # Convert month names to numeric (1-12)
     numeric_months <- match(months, month.abb)

     # Convert IOD values to numeric
     iod_values <- as.numeric(iod_values)

     # Create a data frame with the columns: year, month (numeric), month_name, variable, and value
     out <- data.frame(
          year = as.integer(years),
          month = numeric_months,
          month_name = months,
          variable = "DMI",  # Set variable as DMI for Dipole Mode Index
          value = iod_values,
          stringsAsFactors = FALSE
     )

     return(out)
}

