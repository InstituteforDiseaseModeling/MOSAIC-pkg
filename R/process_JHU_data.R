#' Process Weekly Cholera Data from JHU: Convert to MOSAIC Format
#'
#' This function processes raw weekly cholera surveillance data from the JHU Public Surveillance Dataset,
#' aggregates to country level, parses ISO-weeks, computes week start/end dates, and prepares the data
#' for the MOSAIC framework.
#'
#' @param PATHS A list containing paths to the raw and processed data directories. Should include:
#' \itemize{
#'   \item \strong{DATA_RAW}: Path to the directory containing \code{Public_surveillance_dataset.rds}.
#'   \item \strong{DATA_JHU_WEEKLY}: Path to save the processed cholera data (e.g., \code{cholera_country_weekly_processed.csv}).
#' }
#'
#' @return
#' Invisibly returns \code{NULL}. Side effects:
#' \itemize{
#'   \item Reads raw data with \code{readRDS()}.
#'   \item Writes processed data to \code{PATHS$DATA_JHU_WEEKLY/cholera_country_weekly_processed.csv} via \code{write.csv()}.
#' }
#'
#' @details
#' Steps performed:
#' \itemize{
#'   \item Load \code{Public_surveillance_dataset.rds} and keep only \code{spatial_scale == "country"}.
#'   \item Extract \code{iso_code} from \code{location_name} (suffix after “::”) and convert to full country name.
#'   \item Split \code{epiweek} (e.g. "2017-15") into \code{year} and \code{week}.
#'   \item Create \code{date_start} (Monday) and \code{date_stop} (Sunday) for each ISO week via \code{ISOweek::ISOweek2date()}.
#'   \item Compute \code{month} from \code{date_start} using \code{lubridate::month()}.
#'   \item Define \code{cases} using suspected (\code{sCh}) if present, else confirmed (\code{cCh}), defaulting to 0.
#'   \item Define \code{deaths} (zero if \code{NA}).
#'   \item Save a data.frame with columns:
#'     \code{country, iso_code, year, month, week, date_start, date_stop, cases, deaths}.
#' }
#'
#' @importFrom ISOweek ISOweek2date
#' @importFrom lubridate month
#' @examples
#' \dontrun{
#' PATHS <- list(
#'   DATA_RAW        = "data/raw",
#'   DATA_JHU_WEEKLY = "data/processed/jhu_weekly"
#' )
#' process_JHU_weekly_data(PATHS)
#' }
#' @export

process_JHU_weekly_data <- function(PATHS) {

     if (!dir.exists(PATHS$DATA_JHU_WEEKLY))
          dir.create(PATHS$DATA_JHU_WEEKLY, recursive = TRUE)

     raw_data_path <- file.path(
          PATHS$DATA_RAW,
          "JHU/osfstorage-archive",
          "Public_surveillance_dataset.rds"
     )

     message("Loading raw data from: ", raw_data_path)
     data <- readRDS(raw_data_path)

     # keep only country-level observations
     data <- data[data$spatial_scale == "country", ]

     # extract ISO3 code and full country name
     data$iso_code <- sub(".*::", "", data$location_name)
     data$country  <- MOSAIC::convert_iso_to_country(data$iso_code)

     # parse epiweek into year & week
     parts      <- strsplit(data$epiweek, "-")
     data$year  <- as.integer(sapply(parts, "[", 1))
     data$week  <- as.integer(sapply(parts, "[", 2))

     # compute ISO-week start and stop dates
     data$iso_week   <- paste0(data$year, "-W", sprintf("%02d", data$week))
     data$date_start <- ISOweek::ISOweek2date(paste0(data$iso_week, "-1"))
     data$date_stop  <- ISOweek::ISOweek2date(paste0(data$iso_week, "-7"))
     data$iso_week   <- NULL

     # month of week-start
     data$month <- lubridate::month(data$date_start)

     # define cases and deaths
     data$cases  <- ifelse(!is.na(data$sCh), data$sCh,
                           ifelse(!is.na(data$cCh), data$cCh, 0))
     data$deaths <- ifelse(is.na(data$deaths), 0, data$deaths)

     # select and order columns
     d <- data[, c(
          "country", "iso_code", "year", "month", "week",
          "date_start", "date_stop", "cases", "deaths"
     )]

     print(head(d))
     message("Latest observation: ", max(d$date_stop, na.rm = TRUE))

     processed_data_path <- file.path(
          PATHS$DATA_JHU_WEEKLY,
          "cholera_country_weekly_processed.csv"
     )
     utils::write.csv(d, file = processed_data_path, row.names = FALSE)
     message("Processed JHU weekly cholera data saved to: ", processed_data_path)
}
