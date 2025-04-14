#' Downscale weekly values to a daily time series
#'
#' This function takes a weekly time series (start dates and values) and returns a complete
#' daily time series with values distributed across the 7 days of each week. If `integer = TRUE`,
#' remainders are centered in the week. Missing weeks or days are filled with NA to ensure completeness.
#'
#' @param date_start A vector of Dates (class "Date") representing the start of each week.
#'        Must fall on Mondays.
#' @param value A numeric vector of weekly totals (e.g., cases or rainfall).
#' @param integer Logical; if TRUE, treat values as counts and distribute remainder symmetrically.
#'
#' @return A data frame with columns: `date`, `value`, one row per day from min to max date.
#' @export
#'
#' @examples
#'
#' downscale_weekly_values(as.Date(c("2023-01-02", "2023-01-16")), c(10, 3), integer = TRUE)
#'

downscale_weekly_values <- function(date_start, value, integer = TRUE) {

     if (!inherits(date_start, "Date")) stop("'date_start' must be of class 'Date' in YYYY-MM-DD format.")
     if (any(as.integer(format(date_start, "%u")) != 1)) stop("All 'date_start' entries must fall on Mondays.")
     stopifnot(length(date_start) == length(value))

     # Generate complete weekly sequence (by Monday)
     full_weeks <- seq(from = min(date_start), to = max(date_start), by = "1 week")
     value_full <- rep(NA_real_, length(full_weeks))
     value_full[match(date_start, full_weeks)] <- value
     date_start <- full_weeks
     value <- value_full

     daily_rows <- list()

     for (i in seq_along(date_start)) {

          start_date <- date_start[i]
          total <- value[i]

          if (is.na(total)) {

               daily_vals <- rep(NA_real_, 7)

          } else if (!integer) {

               daily_vals <- rep(total / 7, 7)

          } else {

               total <- as.integer(round(total))
               base <- total %/% 7
               rem <- total %% 7
               daily_vals <- rep(base, 7)

               if (rem > 0) {
                    center <- 4
                    spread_days <- seq(center - floor((rem - 1)/2), center + ceiling((rem - 1)/2))
                    daily_vals[spread_days] <- daily_vals[spread_days] + 1
               }

          }

          dates <- seq(start_date, by = "1 day", length.out = 7)
          daily_rows[[i]] <- data.frame(
               date = dates,
               value = daily_vals,
               stringsAsFactors = FALSE
          )
     }

     df_daily <- do.call(rbind, daily_rows)

     # Fill missing days to create a full daily series
     full_dates <- seq(min(df_daily$date), max(df_daily$date), by = "1 day")
     df_full <- merge(
          data.frame(date = full_dates, stringsAsFactors = FALSE),
          df_daily,
          by = "date",
          all.x = TRUE
     )

     # Sanity check: total match
     if (integer) {

          if (sum(as.integer(round(value)), na.rm = TRUE) != sum(df_full$value, na.rm = TRUE)) {
               stop("Total values do not match between weekly and daily data (integer = TRUE).")
          }

     } else {

          if (!all.equal(sum(value, na.rm = TRUE), sum(df_full$value, na.rm = TRUE), tolerance = 1e-6)) {
               stop("Total values do not match between weekly and daily data (integer = FALSE).")
          }

     }

     return(df_full)
}
