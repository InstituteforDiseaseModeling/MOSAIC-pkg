#' Process Combined Weekly and Daily Cholera Surveillance Data with Truly Square Data Structure
#'
#' This function reads the processed weekly cholera data from WHO, JHU, and supplemental (SUPP) sources,
#' labels each record by its source, combines them (removing duplicate country–week entries according to
#' the specified source preference), creates a truly square data structure with all country-week combinations
#' from min to max date across the entire dataset, and downscales the combined weekly totals to a daily time series 
#' using \code{MOSAIC::downscale_weekly_values} with integer allocation.
#'
#' @param PATHS A list of file paths. Must include:
#' \itemize{
#'   \item \strong{DATA_WHO_WEEKLY}: Directory containing \code{cholera_country_weekly_processed.csv} from WHO.
#'   \item \strong{DATA_JHU_WEEKLY}: Directory containing \code{cholera_country_weekly_processed.csv} from JHU.
#'   \item \strong{DATA_SUPP_WEEKLY}: Directory containing \code{cholera_country_weekly_processed.csv} from supplemental source (may include extra columns).
#'   \item \strong{DATA_CHOLERA_WEEKLY}: Directory where the combined weekly output will be saved.
#'   \item \strong{DATA_CHOLERA_DAILY}: Directory where the combined daily output will be saved.
#' }
#' @param keep_source Character; one of "WHO", "JHU", or "SUPP". When duplicate country–week entries
#'   occur across sources, the entry from \code{keep_source} will be used preferentially.
#'
#' @return Invisibly returns \code{NULL}. Side effects:
#' \itemize{
#'   \item Reads weekly CSVs from all three sources and adds a \code{source} column.
#'   \item Cleans rows with missing key grouping fields (iso_code, year, week).
#'   \item Harmonizes columns by keeping only those present in all three dataframes.
#'   \item Deduplicates by \code{iso_code}, \code{year}, \code{week}, choosing rows from \code{keep_source}.
#'   \item Creates truly square data structure with all country-week combinations from min to max date (missing data = NA).
#'   \item Saves the combined weekly data to
#'     \code{PATHS$DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_combined.csv}.
#'   \item Downscales weekly \code{cases} and \code{deaths} to daily counts,
#'     preserving square structure (keeping days with NA), and carries over the \code{source}.
#'   \item Saves the combined daily data to
#'     \code{PATHS$DATA_CHOLERA_DAILY/cholera_surveillance_daily_combined.csv}.
#' }
#'
#' @importFrom ISOweek ISOweek2date
#' @importFrom lubridate month wday floor_date
#' @export
#'

process_cholera_surveillance_data <- function(PATHS, keep_source = c("WHO", "JHU", "SUPP")) {

     keep_source <- match.arg(keep_source)

     # create output dirs
     dirs <- c(PATHS$DATA_CHOLERA_WEEKLY, PATHS$DATA_CHOLERA_DAILY)
     lapply(dirs, function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE))

     # helper: read and clean a source
     clean_in <- function(path, src) {
          df <- utils::read.csv(path, stringsAsFactors = FALSE)
          df <- df[rowSums(is.na(df)) < ncol(df), ]
          df <- subset(df, !is.na(iso_code) & !is.na(year) & !is.na(week))
          df$source <- src
          df
     }

     # file paths
     who_path <- file.path(PATHS$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv")
     jhu_path <- file.path(PATHS$DATA_JHU_WEEKLY, "cholera_country_weekly_processed.csv")
     sup_path <- file.path(PATHS$DATA_SUPP_WEEKLY, "cholera_country_weekly_processed.csv")

     # read each
     d_who <- clean_in(who_path, "WHO")
     d_jhu <- clean_in(jhu_path, "JHU")
     d_sup <- clean_in(sup_path, "SUPP")

     # harmonize columns: keep only those common to all sources
     common_cols <- Reduce(intersect, list(names(d_who), names(d_jhu), names(d_sup)))
     if (length(common_cols) == 0) stop("No common columns across all sources.")
     d_who <- d_who[common_cols]
     d_jhu <- d_jhu[common_cols]
     d_sup <- d_sup[common_cols]

     # combine and dedupe by iso_code/year/week
     all_df <- rbind(d_who, d_jhu, d_sup)
     n_before <- nrow(all_df)
     key_cols <- c("iso_code", "year", "week")
     
     # FIX: Remove rows with NA in key columns before deduplication to prevent all-NA rows
     all_df <- all_df[complete.cases(all_df[, key_cols]), ]
     message(sprintf("Removed %d rows with missing key fields (iso_code, year, week)", n_before - nrow(all_df)))
     
     # Improved deduplication using aggregate approach instead of problematic split/rbind
     all_df$key <- paste(all_df$iso_code, all_df$year, all_df$week, sep = "_")
     
     # For each unique key, select best record based on source preference
     dedup_list <- lapply(split(all_df, all_df$key), function(gr) {
          if (nrow(gr) > 1) {
               # First priority: preferred source with complete cases/deaths
               pref <- subset(gr, source == keep_source & !is.na(cases) & !is.na(deaths))
               if (nrow(pref) > 0) return(pref[1, ])
               
               # Second priority: any source with complete cases/deaths
               complete <- subset(gr, !is.na(cases) & !is.na(deaths))
               if (nrow(complete) > 0) return(complete[1, ])
               
               # Third priority: preferred source regardless of completeness
               pref_any <- subset(gr, source == keep_source)
               if (nrow(pref_any) > 0) return(pref_any[1, ])
               
               # Final fallback: first record
               return(gr[1, ])
          }
          return(gr[1, ])
     })
     
     # Combine deduplicated records
     dedup <- do.call(rbind, dedup_list)
     dedup$key <- NULL  # Remove temporary key column
     
     removed <- n_before - nrow(dedup)
     message(if (removed > 0) sprintf("Removed %d duplicate weekly entries, kept %s", removed, keep_source)
             else "No duplicate weekly entries found")
     
     # Create square data structure by filling missing country-week combinations
     message("Creating square data structure across entire dataset...")
     
     # Get all unique countries from the data
     all_countries <- unique(dedup$iso_code)
     
     # Create complete weekly sequence from min to max date across entire dataset
     min_date <- min(as.Date(dedup$date_start), na.rm = TRUE)
     max_date <- max(as.Date(dedup$date_start), na.rm = TRUE)
     
     # Generate all Monday dates (week starts) from min to max
     all_monday_dates <- seq(
          from = min_date,
          to = max_date,
          by = "week"
     )
     
     # Create complete time periods data frame
     complete_time_periods <- data.frame(
          date_start = all_monday_dates,
          date_stop = all_monday_dates + 6,  # Sunday = Monday + 6 days
          stringsAsFactors = FALSE
     )
     
     # Add year, week, and month information
     complete_time_periods$year <- as.integer(format(complete_time_periods$date_start, "%Y"))
     complete_time_periods$week <- as.integer(format(complete_time_periods$date_start, "%V"))
     complete_time_periods$month <- as.integer(format(complete_time_periods$date_start, "%m"))
     
     # Generate all possible country-time combinations (truly square)
     square_grid <- merge(
          data.frame(iso_code = all_countries, stringsAsFactors = FALSE),
          complete_time_periods,
          all = TRUE
     )
     
     # Add country names to the grid
     square_grid$country <- MOSAIC::convert_iso_to_country(square_grid$iso_code)
     
     # Merge with actual data to preserve reported values
     # Convert date columns to same format for proper merging
     dedup$date_start <- as.Date(dedup$date_start)
     dedup$date_stop <- as.Date(dedup$date_stop)
     
     wk <- merge(
          square_grid,
          dedup,
          by = c("iso_code", "country", "year", "week", "date_start", "date_stop", "month"),
          all.x = TRUE
     )
     
     # For missing combinations, set cases/deaths to NA and source to NA
     wk$cases[is.na(wk$cases)] <- NA
     wk$deaths[is.na(wk$deaths)] <- NA
     wk$source[is.na(wk$source)] <- NA
     
     # Sort by country and date for clean output
     wk <- wk[order(wk$iso_code, wk$year, wk$week), ]
     
     message(sprintf("Created truly square data structure: %d total observations (%d countries × %d weeks)", 
                     nrow(wk), 
                     length(all_countries), 
                     length(all_monday_dates)))
     message(sprintf("  - %d reported observations (%.1f%%)", 
                     sum(!is.na(wk$cases)), 
                     100 * sum(!is.na(wk$cases)) / nrow(wk)))
     message(sprintf("  - %d missing observations (%.1f%%)", 
                     sum(is.na(wk$cases)), 
                     100 * sum(is.na(wk$cases)) / nrow(wk)))

     # save combined weekly
     weekly_out <- file.path(PATHS$DATA_CHOLERA_WEEKLY,
                             "cholera_surveillance_weekly_combined.csv")
     utils::write.csv(wk, weekly_out, row.names = FALSE)
     message("Combined weekly data saved to: ", weekly_out)

     # downscale to daily
     daily_list <- lapply(split(wk, wk$iso_code), function(df_iso) {
          df_iso$date_start <- as.Date(df_iso$date_start)
          dc <- MOSAIC::downscale_weekly_values(df_iso$date_start, df_iso$cases, integer = TRUE)
          names(dc)[2] <- "cases"
          dd <- MOSAIC::downscale_weekly_values(df_iso$date_start, df_iso$deaths, integer = TRUE)
          names(dd)[2] <- "deaths"
          df_day <- merge(dc, dd, by = "date", all = TRUE)
          # Keep all days to maintain square structure (do not remove NA days)
          data.frame(
               country  = MOSAIC::convert_iso_to_country(df_iso$iso_code[1]),
               iso_code = df_iso$iso_code[1],
               month    = lubridate::month(df_day$date),
               week     = lubridate::wday(df_day$date),
               date     = df_day$date,
               cases    = as.integer(df_day$cases),
               deaths   = as.integer(df_day$deaths),
               source   = df_iso$source[1],
               stringsAsFactors = FALSE
          )
     })
     daily_all <- do.call(rbind, daily_list)
     
     # Log daily data structure
     daily_date_range <- range(as.Date(daily_all$date), na.rm = TRUE)
     daily_days <- as.numeric(diff(daily_date_range)) + 1
     daily_countries <- length(unique(daily_all$iso_code))
     daily_reported <- sum(!is.na(daily_all$cases))
     daily_missing <- sum(is.na(daily_all$cases))
     
     message(sprintf("Created truly square daily data structure: %d total observations (%d countries × %d days)", 
                     nrow(daily_all), daily_countries, daily_days))
     message(sprintf("  - %d reported daily observations (%.1f%%)", 
                     daily_reported, 100 * daily_reported / nrow(daily_all)))
     message(sprintf("  - %d missing daily observations (%.1f%%)", 
                     daily_missing, 100 * daily_missing / nrow(daily_all)))

     # save combined daily
     daily_out <- file.path(PATHS$DATA_CHOLERA_DAILY,
                            "cholera_surveillance_daily_combined.csv")
     utils::write.csv(daily_all, daily_out, row.names = FALSE)
     message("Combined daily data saved to: ", daily_out)

     invisible(NULL)
}
