#' Process supplementary cholera outbreak data from raw SUPP directory
#'
#' This function reads the first CSV line-list file in the raw SUPP directory (e.g., Aceng et al. 2025 S1 Data),
#' cleans and standardizes the data, and returns a data.frame of weekly counts in the standard MOSAIC surveillance format.
#'
#' @param PATHS A list of file paths used by the MOSAIC pipeline. Must include:
#'   \describe{
#'     \item{DATA_RAW}{Root data directory (e.g., root/MOSAIC-data/raw).}
#'   }
#' @return A data.frame with columns:
#'   \describe{
#'     \item{country}{Country name (fixed to "Uganda").}
#'     \item{iso_code}{ISO3 country code (fixed to "UGA").}
#'     \item{year}{Calendar year of week.}
#'     \item{month}{Calendar month (1–12) of week start.}
#'     \item{week}{Epidemiological week number (1–52).}
#'     \item{date_start}{Start date of week (Monday).}
#'     \item{date_stop}{End date of week (Sunday).}
#'     \item{cases}{Number of onset dates in that week.}
#'     \item{deaths}{Number of outcomes coded as death in that week.}
#'     \item{cases_binary}{Indicator (0/1) if any cases >0.}
#'   }
#'
#' @export
#'

process_SUPP_weekly_data <- function(PATHS) {

     message("Processing supplemental data sources of weekly cholera surveillance data...")

     df_all <- data.frame()

     #--------------------------------------------------------------------------
     # Aceng et al 2025: Uganda outbreak 2021
     #--------------------------------------------------------------------------

     message("Aceng et al 2025: Uganda outbreak 2021")

     # Locate the first CSV file for Aceng 2025
     fp <- list.files(
          path = file.path(PATHS$DATA_RAW, "SUPP", "Aceng_2025"),
          pattern = "\\.csv?$",
          full.names = TRUE,
          recursive = TRUE
     )

     if (length(fp) < 1) {
          stop("No CSV files found in ", file.path(PATHS$DATA_RAW, "SUPP", "Aceng_2025"))
     }

     # Read raw line-list
     df <- utils::read.csv(fp[1], stringsAsFactors = FALSE)

     # Parse date of onset
     df$Date.of.onset <- as.Date(df$Date.of.onset, format = "%m/%d/%y")

     # Standardize outcome and flag deaths
     df$Outcome <- tolower(df$Outcome)
     death_flag  <- df$Outcome %in% c("death", "died", "dead")

     # Get weekly case and death sums (week starts on Monday)
     week_starts <- lubridate::floor_date(df$Date.of.onset, unit = "week", week_start = 1)
     all_weeks   <- seq(
          from = min(week_starts, na.rm = TRUE),
          to   = max(week_starts, na.rm = TRUE),
          by   = "week"
     )
     cases_vec   <- tapply(df$Date.of.onset, week_starts, length)
     deaths_vec  <- tapply(death_flag,          week_starts, sum, na.rm = TRUE)

     # Replace missing with zeros
     cases <- as.integer(cases_vec[as.character(all_weeks)])
     deaths <- as.integer(deaths_vec[as.character(all_weeks)])
     cases[is.na(cases)]   <- 0
     deaths[is.na(deaths)] <- 0

     # Map to WHO and JHU processed data format
     df_aceng_2025 <- data.frame(
          country      = "Uganda",
          iso_code     = "UGA",
          year         = lubridate::year(all_weeks),
          month        = lubridate::month(all_weeks),
          week         = lubridate::epiweek(all_weeks),
          date_start   = all_weeks,
          date_stop    = all_weeks + 6,
          cases        = cases,
          deaths       = deaths,
          note         = "Aceng et al 2025",
          stringsAsFactors = FALSE
     )


     #--------------------------------------------------------------------------
     # Other supplemental sources...
     #--------------------------------------------------------------------------


     df_all <- rbind(df_all, df_aceng_2025)

     processed_data_path <- file.path(
          PATHS$DATA_SUPP_WEEKLY,
          "cholera_country_weekly_processed.csv"
     )

     utils::write.csv(df_all, file = processed_data_path, row.names = FALSE)
     message("Processed supplemental sources (SUPP) weekly cholera data saved to: ", processed_data_path)

}
