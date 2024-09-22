#' Compile ENSO and DMI Data (Historical and Forecast)
#'
#' This function compiles historical and forecast data for DMI and ENSO (Niño3, Niño3.4, and Niño4) into a single data frame. The data is filtered to only include years from a specified `year_start` onwards. The function also allows for disaggregation of monthly data into daily or weekly values using either linear interpolation or spline interpolation.
#'
#' @param year_start An integer representing the start year for filtering the data. Only data from this year onward will be included in the compiled data. The value must be greater than or equal to 1870.
#' @param frequency A character string specifying the time resolution of the output data. Valid options are "daily", "weekly", or "monthly".
#' @param method A character string specifying the interpolation method to use. Valid options are "linear" (for linear interpolation using `zoo::na.approx()`) or "spline" (for spline interpolation using `zoo::na.spline()`).
#'
#' @return A data frame with combined historical and forecast data for DMI, ENSO3, ENSO34, and ENSO4. The data frame includes the following columns:
#' \itemize{
#'   \item \code{date}: The date of the data (daily, weekly, or monthly) in YYYY-MM-DD format.
#'   \item \code{variable}: The variable name, which can be "DMI", "ENSO3", "ENSO34", or "ENSO4".
#'   \item \code{value}: The value of the variable (sea surface temperature anomaly).
#'   \item \code{year}: The year corresponding to the date.
#'   \item \code{month}: The month corresponding to the date.
#'   \item \code{week}: The week of the year (for "weekly" and "daily" frequency).
#'   \item \code{doy}: The day of the year (only for "daily" frequency).
#' }
#'
#' @importFrom zoo na.approx na.spline
#' @importFrom stats ave
#' @importFrom base as.Date paste0
#' @importFrom lubridate year month week yday
#'
#' @examples
#' \dontrun{
#' # Compile the ENSO and DMI data from the year 2000 onwards
#' compiled_enso_data <- compile_ENSO_data(2010, "monthly")
#'
#' # Display the compiled data
#' head(compiled_enso_data)
#' }
#'
#' @export

compile_ENSO_data <- function(year_start=NULL, frequency = "monthly", method = "linear") {

     if (is.null(year_start)) year_start <- 1870

     # Check that year_start is greater than or equal to 1870
     if (year_start < 1870) {
          stop("The start year must be greater than or equal to 1870.")
     }

     # Check that frequency is valid
     if (!frequency %in% c("daily", "weekly", "monthly")) {
          stop("Invalid frequency. Choose 'daily', 'weekly', or 'monthly'.")
     }

     # Check that method is valid
     if (!method %in% c("linear", "spline")) {
          stop("Invalid method. Choose 'linear' or 'spline'.")
     }

     # Get historical and forecast DMI data
     dmi_historical <- get_DMI_historical()
     dmi_forecast <- get_DMI_forecast()

     # Get historical and forecast ENSO data
     enso_historical <- get_ENSO_historical()
     enso_forecast <- get_ENSO_forecast()

     # Combine the historical and forecast data for DMI and ENSO
     compiled_df <- base::rbind(
          dmi_historical,
          dmi_forecast,
          enso_historical,
          enso_forecast
     )

     # Filter data based on the start year
     compiled_df <- compiled_df[compiled_df$year >= year_start, ]

     # Create a monthly date column (first day of each month) with correct YYYY-MM-DD format
     compiled_df$date <- base::as.Date(base::paste0(compiled_df$year, "-", compiled_df$month, "-01"), format = "%Y-%m-%d")

     # Ensure dates are valid
     compiled_df <- compiled_df[!base::is.na(compiled_df$date), ]

     # Choose the interpolation method based on the 'method' argument
     interpolation_function <- if (method == "linear") zoo::na.approx else zoo::na.spline

     # Handle disaggregation based on frequency
     if (frequency == "daily") {
          # Generate daily data with chosen interpolation method between months
          start_date <- base::min(compiled_df$date)
          end_date <- base::max(compiled_df$date)
          daily_data <- base::data.frame(date = seq(start_date, end_date, by = "day"))
          daily_data <- merge(daily_data, compiled_df, by = "date", all.x = TRUE)
          daily_data$value <- stats::ave(daily_data$value, daily_data$variable, FUN = function(x) interpolation_function(x, na.rm = FALSE))
          daily_data <- daily_data[!base::is.na(daily_data$value), ]  # Remove rows with NA values

          # Add year, month, week, and day of year (doy) columns
          daily_data$year <- lubridate::year(daily_data$date)
          daily_data$month <- lubridate::month(daily_data$date)
          daily_data$week <- lubridate::week(daily_data$date)
          daily_data$doy <- lubridate::yday(daily_data$date)

          return(daily_data[base::order(daily_data$date), c("date", "year", "month", "week", "doy", "variable", "value")])

     } else if (frequency == "weekly") {
          # Generate weekly data with chosen interpolation method between months
          start_date <- base::min(compiled_df$date)
          end_date <- base::max(compiled_df$date)
          weekly_data <- base::data.frame(date = seq(start_date, end_date, by = "week"))
          weekly_data <- merge(weekly_data, compiled_df, by = "date", all.x = TRUE)
          weekly_data$value <- stats::ave(weekly_data$value, weekly_data$variable, FUN = function(x) interpolation_function(x, na.rm = FALSE))
          weekly_data <- weekly_data[!base::is.na(weekly_data$value), ]  # Remove rows with NA values

          # Add year, month, and week columns
          weekly_data$year <- lubridate::year(weekly_data$date)
          weekly_data$month <- lubridate::month(weekly_data$date)
          weekly_data$week <- lubridate::week(weekly_data$date)

          return(weekly_data[base::order(weekly_data$date), c("date", "year", "month", "week", "variable", "value")])
     }

     # If monthly frequency is selected, return the original data ordered by date
     compiled_df <- compiled_df[!base::is.na(compiled_df$value), ]  # Remove rows with NA values
     compiled_df <- compiled_df[base::order(compiled_df$date), ]

     return(compiled_df)
}
