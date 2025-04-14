#' Downscale Weekly Cholera Data to Daily Frequency
#'
#' This function reads a CSV file containing weekly processed cholera data from WHO and JHU sources and downscales
#' it to a daily time series using the \code{MOSAIC::downscale_weekly_values} method. For each country (identified by its ISO code), the function applies
#' the \code{downscale_weekly_values} function to both the cases and deaths columns, then merges
#' the results by date.
#'
#' @param PATHS A list of file paths. This list must include an element named \code{DATA_WHO_WEEKLY} and \code{DATA_WHO_DAILY}.
#'
#' @return Invisibly returns \code{NULL}.
#'
#' @details The input CSV file \code{cholera_country_weekly_processed.csv} is expected to contain at least
#'   the following columns:
#'   \itemize{
#'     \item \code{date_start} - The start date of the week (in "YYYY-MM-DD" format, must be a Monday).
#'     \item \code{iso_code} - The ISO code identifying the country.
#'     \item \code{cases} - The weekly total number of cholera cases.
#'     \item \code{deaths} - The weekly total number of cholera deaths.
#'   }
#'
#'   The function downsamples these weekly totals to daily counts using the helper function
#'   \code{downscale_weekly_values}, which distributes any remainders symmetrically across the days of each week
#'   when \code{integer = TRUE}. The \code{MOSAIC::convert_iso_to_country} function is used to convert the ISO code to a
#'   country name.
#'
#' @examples
#' \dontrun{
#'   # Define paths list with the directory containing the weekly data file:
#'   PATHS <- list(DATA_WHO_WEEKLY = "/path/to/your/data")
#'
#'   # Downscale the weekly cholera data to daily data:
#'   downscale_weekly_cholera_data(PATHS)
#' }
#'
#' @export
#'

downscale_weekly_cholera_data <- function(PATHS) {

     # Load weekly data
     df_weekly <- read.csv(file.path(PATHS$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv"))
     df_weekly$date_start <- as.Date(df_weekly$date_start)

     # Unique countries
     isos <- unique(df_weekly$iso_code)

     # Initialize list to hold results
     daily_list <- list()

     # Loop through each ISO
     for (iso in isos) {

          df_iso <- subset(df_weekly, iso_code == iso)

          # Downscale cases
          daily_cases <- MOSAIC::downscale_weekly_values(
               date_start = df_iso$date_start,
               value = df_iso$cases,
               integer = TRUE
          )
          names(daily_cases)[2] <- "cases"

          # Downscale deaths
          daily_deaths <- MOSAIC::downscale_weekly_values(
               date_start = df_iso$date_start,
               value = df_iso$deaths,
               integer = TRUE
          )
          names(daily_deaths)[2] <- "deaths"

          # Merge cases and deaths by date
          df_daily <- merge(daily_cases, daily_deaths, by = "date", all = TRUE)


          df_daily <- data.frame(
               country = MOSAIC::convert_iso_to_country(iso),
               iso_code = iso,
               month = format(df_daily$date, "%m"),
               week = format(df_daily$date, "%w"),
               date = df_daily$date,
               cases = df_daily$cases,
               deaths = df_daily$deaths
          )

          daily_list[[iso]] <- df_daily
     }

     df_daily_all <- do.call(rbind, daily_list)
     rownames(df_daily_all) <- NULL

     out_path <- file.path(PATHS$DATA_WHO_DAILY, "cholera_country_daily_processed.csv")
     write.csv(df_daily_all, out_path, row.names = FALSE)
     message("Processed daily cholera data saved to: ", out_path)

}
