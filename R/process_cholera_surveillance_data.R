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
#' @details Duplicate country–week entries across sources are resolved by a fixed
#'   priority \strong{WHO > JHU > AI > SUPP}, with a completeness tie-break: within
#'   a key, rows with complete cases AND deaths are preferred before the source
#'   priority is applied.
#' @param include_ai Logical (default \code{FALSE}). When \code{TRUE}, reads the
#'   AI-mined processed file (\code{DATA_AI_WEEKLY/cholera_country_weekly_processed.csv},
#'   produced by \code{\link{process_AI_cholera_data}}) as a fourth source, using
#'   its per-row \code{confidence_weight} and \code{disaggregation_method}. When
#'   \code{FALSE}, only WHO/JHU/SUPP are merged. If \code{include_ai = TRUE} but the
#'   AI file is missing/empty, a warning is emitted and the merge proceeds with the
#'   three direct sources.
#'
#'   The output always carries a \code{confidence_weight} and a
#'   \code{disaggregation_method} column (stable schema regardless of this flag):
#'   direct-source observations (WHO/JHU/SUPP) get \code{confidence_weight = 1.0}
#'   (fully trusted) and \code{disaggregation_method = NA}; AI rows keep their
#'   per-row values; and square-grid cells with no observation get
#'   \code{confidence_weight = NA}.
#'
#' @return Invisibly returns \code{NULL}. Side effects:
#' \itemize{
#'   \item Reads weekly CSVs from all three sources and adds a \code{source} column.
#'   \item Cleans rows with missing key grouping fields (iso_code, year, week).
#'   \item Harmonizes columns by taking the union across all sources, NA-filling any column missing from a given source (so source-specific columns are never silently dropped).
#'   \item Deduplicates by \code{iso_code}, \code{year}, \code{week} using the fixed priority WHO > JHU > AI > SUPP (completeness tie-break).
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

process_cholera_surveillance_data <- function(PATHS, include_ai = FALSE) {

     # create output dirs
     dirs <- c(PATHS$DATA_CHOLERA_WEEKLY, PATHS$DATA_CHOLERA_DAILY)
     lapply(dirs, function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE))

     # helper: read and clean a source (union-safe; returns NULL if absent/empty)
     read_source <- function(path, src) {
          if (!file.exists(path)) return(NULL)
          df <- utils::read.csv(path, stringsAsFactors = FALSE)
          df <- df[rowSums(is.na(df)) < ncol(df), ]
          df <- subset(df, !is.na(iso_code) & !is.na(year) & !is.na(week))
          if (nrow(df) == 0) return(NULL)
          df$source <- src
          df
     }

     # file paths
     who_path <- file.path(PATHS$DATA_WHO_WEEKLY, "cholera_country_weekly_processed.csv")
     jhu_path <- file.path(PATHS$DATA_JHU_WEEKLY, "cholera_country_weekly_processed.csv")
     sup_path <- file.path(PATHS$DATA_SUPP_WEEKLY, "cholera_country_weekly_processed.csv")
     ai_path  <- file.path(PATHS$DATA_AI_WEEKLY,  "cholera_country_weekly_processed.csv")

     # read each
     d_who <- read_source(who_path, "WHO")
     d_jhu <- read_source(jhu_path, "JHU")
     d_sup <- read_source(sup_path, "SUPP")
     d_ai  <- if (isTRUE(include_ai)) read_source(ai_path, "AI") else NULL

     if (isTRUE(include_ai) && is.null(d_ai)) {
          warning(sprintf(
               "include_ai=TRUE but no usable AI data at %s; proceeding with WHO/JHU/SUPP only.",
               ai_path), immediate. = TRUE)
     }

     # List order sets the rbind order; AI is placed before SUPP to match the
     # WHO > JHU > AI > SUPP priority. When include_ai=FALSE, d_ai is NULL and
     # this is byte-identical to the previous 3-source behavior.
     sources <- Filter(Negate(is.null), list(d_who, d_jhu, d_ai, d_sup))
     if (length(sources) == 0) stop("No surveillance sources available.")

     # UNION harmonization: take the union of all columns across sources, NA-fill
     # missing ones. Replaces the previous Reduce(intersect, ...) which silently
     # dropped any column not present in every source CSV.
     all_cols <- unique(unlist(lapply(sources, names)))
     sources <- lapply(sources, function(df) {
          for (cc in setdiff(all_cols, names(df))) df[[cc]] <- NA
          df[, all_cols]
     })

     # combine and dedupe by iso_code/year/week
     all_df <- do.call(rbind, sources)
     n_before <- nrow(all_df)
     key_cols <- c("iso_code", "year", "week")
     
     # FIX: Remove rows with NA in key columns before deduplication to prevent all-NA rows
     all_df <- all_df[complete.cases(all_df[, key_cols]), ]
     message(sprintf("Removed %d rows with missing key fields (iso_code, year, week)", n_before - nrow(all_df)))

     # Centralized confidence metadata (always present in the output schema):
     #   - direct-source observations (WHO/JHU/SUPP) are fully trusted -> 1.0
     #   - AI rows keep their per-row confidence_weight (from process_AI_cholera_data)
     #   - empty square-grid cells (added below) stay NA = "no observation"
     # disaggregation_method is NA for non-AI rows (not applicable). These columns
     # exist regardless of include_ai so the combined schema is stable.
     if (!"confidence_weight"     %in% names(all_df)) all_df$confidence_weight     <- NA_real_
     if (!"disaggregation_method" %in% names(all_df)) all_df$disaggregation_method <- NA_character_
     trusted <- all_df$source %in% c("WHO", "JHU", "SUPP")
     all_df$confidence_weight[trusted & is.na(all_df$confidence_weight)] <- 1.0

     all_df$key <- paste(all_df$iso_code, all_df$year, all_df$week, sep = "_")

     # Deduplicate by (iso_code, year, week) via a fixed source priority with a
     # completeness tie-break. Priority WHO > JHU > AI > SUPP. For each key we
     # prefer, in order: (1) rows with complete cases AND deaths, then
     # (2) higher-priority source. This reproduces the previous split()-based
     # logic for the 3-source case (WHO-complete > any-complete > WHO-any > first)
     # and extends it to AI as a fourth source. Vectorized: O(n log n).
     PRIORITY <- c(WHO = 1L, JHU = 2L, AI = 3L, SUPP = 4L)

     all_df$.priority <- PRIORITY[all_df$source]
     all_df$.complete <- !is.na(all_df$cases) & !is.na(all_df$deaths)
     all_df <- all_df[order(all_df$iso_code, all_df$year, all_df$week,
                            -all_df$.complete, all_df$.priority), ]
     dedup <- all_df[!duplicated(all_df$key), ]
     dedup$key <- NULL
     dedup$.priority <- NULL
     dedup$.complete <- NULL
     
     removed <- n_before - nrow(dedup)
     message(if (removed > 0) sprintf("Removed %d duplicate weekly entries (priority WHO>JHU>AI>SUPP)", removed)
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
