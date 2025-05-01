#' Process Combined Weekly and Daily Cholera Surveillance Data
#'
#' This function reads the processed weekly cholera data from WHO, JHU, and supplemental (SUPP) sources,
#' labels each record by its source, combines them (removing duplicate country–week entries according to
#' the specified source preference), saves the combined weekly dataset, and downscales the combined weekly
#' totals to a daily time series using \code{MOSAIC::downscale_weekly_values} with integer allocation.
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
#'   \item Cleans rows with all NA or missing key grouping fields.
#'   \item Harmonizes columns by keeping only those present in all three dataframes.
#'   \item Deduplicates by \code{iso_code}, \code{year}, \code{week}, choosing rows from \code{keep_source}.
#'   \item Saves the combined weekly data to
#'     \code{PATHS$DATA_CHOLERA_WEEKLY/cholera_surveillance_weekly_combined.csv}.
#'   \item Downscales weekly \code{cases} and \code{deaths} to daily counts,
#'     dropping days with both NA, and carries over the \code{source}.
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
     dedup <- do.call(rbind, lapply(split(all_df, all_df[, key_cols]), function(gr) {
          if (nrow(gr) > 1) {
               pref <- subset(gr, source == keep_source & !is.na(cases) & !is.na(deaths))
               if (nrow(pref)) return(pref[1, , drop = FALSE])
               gr$na_ct <- rowSums(is.na(gr[, c("cases","deaths")]))
               gr <- gr[order(gr$na_ct), ]
               gr$na_ct <- NULL
               return(gr[1, , drop = FALSE])
          }
          gr[1, , drop = FALSE]
     }))
     removed <- n_before - nrow(dedup)
     message(if (removed > 0) sprintf("Removed %d duplicate weekly entries, kept %s", removed, keep_source)
             else "No duplicate weekly entries found")
     wk <- dedup

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
          df_day <- subset(df_day, !(is.na(cases) & is.na(deaths)))
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

     # save combined daily
     daily_out <- file.path(PATHS$DATA_CHOLERA_DAILY,
                            "cholera_surveillance_daily_combined.csv")
     utils::write.csv(daily_all, daily_out, row.names = FALSE)
     message("Combined daily data saved to: ", daily_out)

     invisible(NULL)
}
