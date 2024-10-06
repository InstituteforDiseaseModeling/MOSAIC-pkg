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
#'   \item \code{month_name}: The name of the month (e.g., "Jan").
#'   \item \code{week}: The week of the year (for "weekly" and "daily" frequency).
#'   \item \code{doy}: The day of the year (only for "daily" frequency).
#'   \item \code{date_start}: Start date for the week or month.
#'   \item \code{date_stop}: End date for the week or month.
#' }
#'
#' @importFrom zoo na.approx na.spline
#' @importFrom stats ave
#' @importFrom base as.Date paste0
#' @importFrom lubridate year month week yday floor_date
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom ISOweek ISOweek2date
#'
#' @examples
#' \dontrun{
#' # Compile the ENSO and DMI data from the year 2000 onwards
#' compiled_enso_data <- process_ENSO_data(2010, "monthly")
#'
#' # Display the compiled data
#' head(compiled_enso_data)
#' }
#'
#' @export
process_ENSO_data <- function(year_start = NULL, frequency = "monthly", method = "linear") {

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

     # Get the full sequence of dates from year_start to the present for each variable
     start_date <- as.Date(paste0(year_start, "-01-01"))
     end_date <- as.Date(max(compiled_df$date))

     all_dates <- seq.Date(start_date, end_date, by = "day")

     # Split the data by variable and fill in missing dates with NA for each variable
     compiled_df_list <- split(compiled_df, compiled_df$variable)
     compiled_df_list <- lapply(compiled_df_list, function(df) {
          all_dates_df <- data.frame(date = all_dates)
          merged_df <- merge(all_dates_df, df, by = "date", all.x = TRUE)
          merged_df$variable <- df$variable[1]  # Preserve variable name

          # Use ISOweek package to get ISO week numbers and years
          merged_df <- merged_df %>%
               dplyr::mutate(
                    ISOweek = ISOweek::ISOweek(date),
                    ISOyear = as.integer(substr(ISOweek, 1, 4)),
                    ISOweeknum = as.integer(substr(ISOweek, 7, 8)),
                    week = ISOweeknum,
                    year = ISOyear,
                    month = lubridate::month(date),
                    month_name = base::month.name[lubridate::month(date)],
                    doy = lubridate::yday(date)
               )

          return(merged_df)
     })

     # Recombine the list into a single data frame
     compiled_df <- do.call(rbind, compiled_df_list)

     # Choose the interpolation method based on the 'method' argument
     interpolation_function <- if (method == "linear") zoo::na.approx else zoo::na.spline

     # Apply interpolation to each variable separately
     compiled_df <- compiled_df %>%
          dplyr::group_by(variable) %>%
          dplyr::mutate(value = interpolation_function(value, na.rm = FALSE)) %>%
          dplyr::ungroup()

     # Handle disaggregation based on frequency
     if (frequency == "daily") {

          # No further processing required for daily data
          return(compiled_df[base::order(compiled_df$date), c("date", "year", "month", "month_name", "week", "doy", "variable", "value")])

     } else if (frequency == "weekly") {

          # Aggregate data by ISOyear and ISOweeknum
          weekly_data <- compiled_df %>%
               dplyr::group_by(variable, ISOyear, ISOweeknum) %>%
               dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
               dplyr::ungroup()

          # Add date_start and date_stop using ISOweek strings
          weekly_data <- weekly_data %>%
               dplyr::mutate(
                    ISOweek_str = paste0(ISOyear, "-W", sprintf("%02d", ISOweeknum)),
                    date_start = ISOweek::ISOweek2date(paste0(ISOweek_str, "-1")),
                    date_stop = ISOweek::ISOweek2date(paste0(ISOweek_str, "-7")),
                    year = ISOyear,
                    week = ISOweeknum
               ) %>%
               dplyr::select(variable, year, week, value, date_start, date_stop)

          # Reorder the data and return it
          weekly_data <- weekly_data[order(weekly_data$date_start), ]
          return(weekly_data)

     } else if (frequency == "monthly") {

          # Aggregate data by month and calculate the mean for each variable
          monthly_data <- compiled_df %>%
               dplyr::group_by(variable, year, month) %>%
               dplyr::summarise(value = mean(value, na.rm = TRUE)) %>%
               dplyr::ungroup()

          # Add columns for date_start and date_stop (first and last day of the month)
          monthly_data <- monthly_data %>%
               dplyr::mutate(
                    month_name = base::month.name[month],
                    date_start = lubridate::floor_date(as.Date(paste(year, month, "01", sep = "-")), "month"),
                    date_stop = lubridate::ceiling_date(as.Date(paste(year, month, "01", sep = "-")), "month") - 1
               )

          # Reorder the data and return it
          return(monthly_data[base::order(monthly_data$date_start), ])
     }
}
