# Hand-curated surveillance corrections: one documented table, inst/extdata/
# surveillance_curation.csv, read by process_WHO_weekly_data() (who_window) and
# process_cholera_surveillance_data() (drop_imputed, flag_imputed).

.SURVEILLANCE_CURATION_ACTIONS <- c("who_window", "drop_imputed", "flag_imputed")


#' Read the surveillance curation table
#'
#' Each row is one correction the processing rules cannot derive from the data,
#' with the evidence and its source. Columns: \code{id} (unique), \code{iso_code},
#' \code{action}, \code{report_year} and \code{report_week} (the WHO epi year and
#' week of the report, \code{who_window} only), \code{date_start} and
#' \code{date_stop} (calendar dates as documented), \code{evidence},
#' \code{reference} and \code{added}. Actions:
#' \describe{
#'   \item{who_window}{the WHO report spreads from the WHO week containing
#'     \code{date_start}, with cases up to the week containing \code{date_stop}
#'     (blank: the report week) and 0 after it.}
#'   \item{drop_imputed}{imputed (tier 3) rows of weeks lying wholly within
#'     \code{[date_start, date_stop]} are emptied: a documented absence of cholera.}
#'   \item{flag_imputed}{those rows are kept and listed in the adjustments log.}
#' }
#'
#' @param action Optional action(s) to keep.
#' @param path The table (default: the package copy).
#' @return Data frame with Date columns \code{date_start}/\code{date_stop} and
#'   integer \code{report_year}/\code{report_week}.
#' @noRd
.surveillance_curation <- function(action = NULL,
                                   path = system.file("extdata", "surveillance_curation.csv",
                                                      package = "MOSAIC")) {
     if (!nzchar(path) || !file.exists(path))
          stop("Surveillance curation table not found: ", path, call. = FALSE)
     cur <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                            na.strings = "")
     need <- c("id", "iso_code", "action", "report_year", "report_week", "date_start",
               "date_stop", "evidence", "reference", "added")
     miss <- setdiff(need, names(cur))
     if (length(miss) > 0L)
          stop("Surveillance curation table lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
     bad <- function(cond, what) {
          if (any(cond)) stop(sprintf("Surveillance curation table: %s (%s)", what,
                                      paste(cur$id[cond], collapse = ", ")), call. = FALSE)
     }
     if (anyDuplicated(cur$id)) stop("Surveillance curation table has duplicated ids", call. = FALSE)
     bad(is.na(cur$id) | is.na(cur$iso_code) | is.na(cur$evidence) | is.na(cur$reference),
         "id, iso_code, evidence and reference are required")
     bad(!cur$action %in% .SURVEILLANCE_CURATION_ACTIONS, "unknown action")
     cur$date_start  <- as.Date(cur$date_start)
     cur$date_stop   <- as.Date(cur$date_stop)
     cur$report_year <- as.integer(cur$report_year)
     cur$report_week <- as.integer(cur$report_week)
     win <- cur$action == "who_window"
     bad(win & (is.na(cur$report_year) | is.na(cur$report_week) | is.na(cur$date_start)),
         "who_window needs report_year, report_week and date_start")
     bad(!win & (is.na(cur$date_start) | is.na(cur$date_stop)),
         "drop_imputed and flag_imputed need date_start and date_stop")
     bad(!is.na(cur$date_stop) & !is.na(cur$date_start) & cur$date_stop < cur$date_start,
         "date_stop precedes date_start")
     if (!is.null(action)) cur <- cur[cur$action %in% action, ]
     rownames(cur) <- NULL
     cur
}


#' Apply the drop_imputed and flag_imputed curation rows to selected weeks
#'
#' @param dedup Selected rows (one per country-week) with \code{.tier}.
#' @param cur Curation rows (any actions; only drop/flag are used).
#' @return list(data = dedup after the rows, log = list of adjustment-log frames).
#' @noRd
.apply_imputed_curation <- function(dedup, cur) {
     log <- list()
     cur <- cur[cur$action %in% c("drop_imputed", "flag_imputed"), ]
     empty_cols <- intersect(c("cases", "deaths", "source", "source_deaths", "note",
                               "confidence_weight", "disaggregation_method"), names(dedup))
     ws <- as.Date(dedup$date_start)
     imp <- dedup$.tier == 3L & !(is.na(dedup$cases) & is.na(dedup$deaths))
     for (i in seq_len(nrow(cur))) {
          rows <- which(imp & dedup$iso_code == cur$iso_code[i] &
                        ws >= cur$date_start[i] & ws + 6L <= cur$date_stop[i])
          if (length(rows) == 0L) {
               message(sprintf("Surveillance curation %s matches no imputed week of %s in %s..%s",
                               cur$id[i], cur$iso_code[i], format(cur$date_start[i]),
                               format(cur$date_stop[i])))
               next
          }
          detail <- sprintf("curated %s: %s", cur$id[i], cur$evidence[i])
          if (cur$action[i] == "drop_imputed") {
               log[[length(log) + 1L]] <- .surveillance_adjustment_log(
                    dedup[rows, ], "imputed_dropped_curated", detail = detail)
               for (cc in empty_cols) dedup[[cc]][rows] <- NA
          } else {
               log[[length(log) + 1L]] <- .surveillance_adjustment_log(
                    dedup[rows, ], "imputed_flagged_curated",
                    cases_after = dedup$cases[rows], deaths_after = dedup$deaths[rows],
                    detail = detail)
          }
     }
     list(data = dedup, log = log)
}
