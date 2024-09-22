#' Download and Process Historical DMI Data
#'
#' This function downloads the historical Dipole Mode Index (DMI) data from NOAA and processes it into a data frame with columns for year, month (1-12), month_name, variable (DMI), and value.
#'
#' @return A data frame with columns for year, month, month_name, variable (DMI), and value.
#'
#' @details The DMI data is downloaded from NOAA's historical DMI data page: \url{https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/dmi.had.long.data}. The data includes DMI values representing the difference in sea surface temperature anomalies between the western and eastern Indian Ocean.
#'
#' @examples
#' \dontrun{
#' # Get the historical DMI data
#' dmi_historical <- get_DMI_historical()
#'
#' # Display the historical DMI data
#' print(dmi_historical)
#'}
#' @export

get_DMI_historical <- function() {


     # NOAA DMI historical data URL
     url <- "https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/dmi.had.long.data"

     message("NOTE: Historical data downloaded from:")
     message("https://psl.noaa.gov/gcos_wgsp/Timeseries/Data/dmi.had.long.data")


     # Read the raw data, skipping the header lines and excluding missing values (-9999)
     raw_data <- read.table(url, skip = 1, fill = TRUE, na.strings = "-9999.000", header = FALSE)

     # The first column is the year, and the remaining columns represent the 12 months
     colnames(raw_data) <- c("year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

     # Remove rows where the year is -9999, as these indicate invalid or missing data
     raw_data <- raw_data[raw_data$year != -9999, ]

     # Convert year to integer
     raw_data$year <- as.integer(raw_data$year)

     # Convert all monthly columns to numeric (to handle any mixed types)
     raw_data[ , -1] <- lapply(raw_data[ , -1], as.numeric)

     # Convert the data from wide to long format
     dmi_long <- raw_data %>%
          pivot_longer(cols = Jan:Dec, names_to = "month_name", values_to = "value")

     # Create a numeric month column (1-12) based on the month_name
     dmi_long <- dmi_long %>%
          mutate(month = match(month_name, month.abb))  # Convert month names to numbers (1-12)

     # Add the variable name as "DMI" (Dipole Mode Index)
     dmi_long <- dmi_long %>%
          mutate(variable = "DMI") %>%
          select(year, month, month_name, variable, value)  # Select and order columns

     # Ensure the data is ordered by year and month
     dmi_long <- dmi_long %>%
          arrange(year, month)  # Arrange by year and month

     return(as.data.frame(dmi_long))
}
