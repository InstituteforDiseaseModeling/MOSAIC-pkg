#' Download and Process Historical ENSO Data (Niño3, Niño3.4, Niño4)
#'
#' This function downloads the historical ENSO data for Niño3, Niño3.4, and Niño4 from NOAA and processes it into a single data frame with columns for year (as an integer), month, month_name, variable (ENSO3, ENSO34, ENSO4), and value.
#'
#' @return A data frame with columns for year, month, month_name, variable, and value.
#'
#' @details The historical ENSO data is downloaded from NOAA's historical ENSO data pages. The data includes sea surface temperature anomalies for Niño3, Niño3.4, and Niño4 regions.
#'
#' @importFrom utils read.table
#' @importFrom dplyr mutate filter select arrange bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom base lapply as.integer as.numeric match
#'
#' @examples
#' \dontrun{
#' # Get the historical ENSO data
#' enso_historical <- get_ENSO_historical()
#'
#' # Display the historical ENSO data
#' print(enso_historical)
#'}
#' @export
get_ENSO_historical <- function() {

     message("NOTE: Historical ENSO data downloaded from: https://psl.noaa.gov/gcos_wgsp/Timeseries/Data")

     # URLs for historical Niño3, Niño3.4, and Niño4 datasets from NOAA
     urls <- list(
          ENSO3 = "https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino3.long.anom.data",
          ENSO34 = "https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino34.long.anom.data",
          ENSO4 = "https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/nino4.long.anom.data"
     )

     # Helper function to download and process each dataset
     process_enso_historical <- function(url, variable_name) {
          # Read the raw data, skipping the header lines and excluding missing values (-99.99)
          raw_data <- utils::read.table(url, skip = 1, fill = TRUE, na.strings = "-99.99", header = FALSE)

          # The first column is the year, and the remaining columns represent the 12 months
          base::colnames(raw_data) <- c("year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

          # Convert year to integer and filter out invalid years (e.g., -99.99)
          raw_data <- dplyr::mutate(raw_data, year = base::as.integer(raw_data$year)) %>%
               dplyr::filter(!base::is.na(year))

          # Convert all monthly columns to numeric (to handle any mixed types)
          raw_data[ , -1] <- base::lapply(raw_data[ , -1], base::as.numeric)

          # Convert the data from wide to long format
          enso_long <- tidyr::pivot_longer(raw_data, cols = Jan:Dec, names_to = "month_name", values_to = "value")

          # Create a numeric month column (1-12) based on the month_name
          enso_long <- dplyr::mutate(enso_long, month = base::match(enso_long$month_name, base::month.abb))  # Convert month names to numbers (1-12)

          # Add the variable name (ENSO3, ENSO34, ENSO4)
          enso_long <- dplyr::mutate(enso_long, variable = variable_name) %>%
               dplyr::select(year, month, month_name, variable, value)  # Select and order columns

          # Ensure the data is ordered by year and month
          enso_long <- dplyr::arrange(enso_long, year, month)

          return(enso_long)
     }

     # Process and combine all datasets (ENSO3, ENSO34, ENSO4)
     enso3_df <- process_enso_historical(urls$ENSO3, "ENSO3")
     enso34_df <- process_enso_historical(urls$ENSO34, "ENSO34")
     enso4_df <- process_enso_historical(urls$ENSO4, "ENSO4")

     # Combine all data frames into one
     combined_df <- base::as.data.frame(dplyr::bind_rows(enso3_df, enso34_df, enso4_df))
     combined_df$year <- base::as.integer(combined_df$year)

     return(combined_df)
}
