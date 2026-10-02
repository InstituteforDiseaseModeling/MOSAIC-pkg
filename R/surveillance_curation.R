# Hand-curated surveillance corrections: one documented table, inst/extdata/
# surveillance_curation.csv, read by process_WHO_weekly_data() (who_window) and
# process_cholera_surveillance_data() (drop_imputed, flag_imputed), and the
# cumulative-count anchors that shape curated WHO windows,
# inst/extdata/surveillance_curation_shapes.csv.

.SURVEILLANCE_CURATION_ACTIONS <- c("who_window", "drop_imputed", "flag_imputed")
.SURVEILLANCE_CURATION_SHAPES  <- c("cumulative")


#' Read the surveillance curation table
#'
#' Each row is one correction the processing rules cannot derive from the data,
#' with the evidence and its source. Columns: \code{id} (unique), \code{iso_code},
#' \code{action}, \code{report_year} and \code{report_week} (the WHO epi year and
#' week of the report, \code{who_window} only), \code{date_start} and
#' \code{date_stop} (calendar dates as documented), \code{shape}
#' (\code{who_window} only), \code{evidence}, \code{reference} and \code{added}.
#' Actions:
#' \describe{
#'   \item{who_window}{the WHO report spreads from the WHO week containing
#'     \code{date_start}, with cases up to the week containing \code{date_stop}
#'     (blank: the report week) and 0 after it: evenly when \code{shape} is
#'     blank, and with \code{shape = "cumulative"} in proportion to a documented
#'     epidemic curve given as cumulative-count anchors in
#'     \code{surveillance_curation_shapes.csv} (see
#'     \code{.surveillance_curation_shapes()}).}
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
               "date_stop", "shape", "evidence", "reference", "added")
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
     bad(!is.na(cur$shape) & !cur$shape %in% .SURVEILLANCE_CURATION_SHAPES, "unknown shape")
     bad(!is.na(cur$shape) & !win, "a shape applies to who_window rows only")
     if (!is.null(action)) cur <- cur[cur$action %in% action, ]
     rownames(cur) <- NULL
     cur
}


#' Read the cumulative-count anchors that shape curated WHO windows
#'
#' One row per anchor: \code{id} (a \code{who_window} row of the curation table
#' with \code{shape = "cumulative"}), \code{date} (YYYY-MM-DD),
#' \code{cumulative_cases} and the optional \code{cumulative_deaths} (the
#' outbreak's cumulative counts through the end of \code{date}, in the source's
#' own units: the report totals rescale them) and \code{note}. A row may carry
#' either count or both. For each id the case anchors define a cumulative case
#' curve, linear between anchors, 0 at the first anchor and flat after the last;
#' the death anchors, when the id has any, define a deaths curve the same way
#' (otherwise, or when the column is absent, deaths follow the case curve). Each
#' WHO week of the window receives a curve's increment over its Monday-to-Sunday
#' span (see \code{.curated_shape_weights()}). Daily or weekly counts are written
#' as anchors at the end of each day or WHO week (a Sunday), so the increments
#' reproduce them exactly; coarser anchors (monthly totals, dated cumulative
#' reports) interpolate the cumulative curve linearly.
#'
#' @param curation \code{who_window} rows of \code{.surveillance_curation()}:
#'   every row with \code{shape = "cumulative"} needs anchors, and every anchor id
#'   must be such a row.
#' @param path The anchor table (default: the package copy).
#' @return Data frame (\code{id}, \code{date}, \code{cumulative_cases},
#'   \code{cumulative_deaths}) ordered by id and date; counts a row does not carry
#'   are NA.
#' @noRd
.surveillance_curation_shapes <- function(curation,
                                          path = system.file("extdata", "surveillance_curation_shapes.csv",
                                                             package = "MOSAIC")) {
     if (!nzchar(path) || !file.exists(path))
          stop("Surveillance curation shape table not found: ", path, call. = FALSE)
     sh <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", na.strings = "")
     miss <- setdiff(c("id", "date", "cumulative_cases", "note"), names(sh))
     if (length(miss) > 0L)
          stop("Surveillance curation shape table lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
     if (!"cumulative_deaths" %in% names(sh)) sh$cumulative_deaths <- NA_character_
     fail <- function(cond, what) {
          if (any(cond)) stop(sprintf("Surveillance curation shape table: %s (%s)", what,
                                      paste(unique(sh$id[cond]), collapse = ", ")), call. = FALSE)
     }
     num <- function(x) suppressWarnings(as.numeric(x))
     fail((!is.na(sh$cumulative_cases) & is.na(num(sh$cumulative_cases))) |
          (!is.na(sh$cumulative_deaths) & is.na(num(sh$cumulative_deaths))), "unreadable count")
     sh$date <- as.Date(sh$date, format = "%Y-%m-%d")
     sh$cumulative_cases  <- num(sh$cumulative_cases)
     sh$cumulative_deaths <- num(sh$cumulative_deaths)
     fail(is.na(sh$id) | is.na(sh$date), "id and a valid date (YYYY-MM-DD) are required")
     fail(is.na(sh$cumulative_cases) & is.na(sh$cumulative_deaths), "each anchor needs a case or death count")
     fail((!is.na(sh$cumulative_cases) & !is.finite(sh$cumulative_cases)) |
          (!is.na(sh$cumulative_deaths) & !is.finite(sh$cumulative_deaths)), "non-finite count")
     fail((!is.na(sh$cumulative_cases) & sh$cumulative_cases < 0) |
          (!is.na(sh$cumulative_deaths) & sh$cumulative_deaths < 0), "negative cumulative count")
     shaped <- curation$id[curation$action == "who_window" & curation$shape %in% "cumulative"]
     fail(!sh$id %in% shaped, "anchors for an id that is not a who_window row with shape 'cumulative'")
     lacking <- setdiff(shaped, sh$id)
     if (length(lacking) > 0L)
          stop("Surveillance curation shape table has no anchors for: ", paste(lacking, collapse = ", "), call. = FALSE)
     sh <- sh[order(sh$id, sh$date), c("id", "date", "cumulative_cases", "cumulative_deaths")]
     for (i in unique(sh$id)) {
          rows <- sh$id == i
          fail(rows & anyDuplicated(sh$date[rows]) > 0L, "each shape needs its anchors on distinct dates")
          for (col in c("cumulative_cases", "cumulative_deaths")) {
               v <- sh[[col]][rows & !is.na(sh[[col]])]
               if (col == "cumulative_deaths" && length(v) == 0L) next   # deaths follow the case curve
               lab <- if (col == "cumulative_cases") "case" else "death"
               fail(rows & length(v) < 2L, sprintf("a %s curve needs at least two anchors", lab))
               fail(rows & v[1L] != 0, sprintf("the cumulative %s count must start at 0", lab))
               fail(rows & any(diff(v) < 0), sprintf("the cumulative %s count must never decrease", lab))
               fail(rows & v[length(v)] <= 0, sprintf("the cumulative %s count must end above 0", lab))
          }
     }
     rownames(sh) <- NULL
     sh
}


#' Weights of the WHO weeks of a curated window under a cumulative curve
#'
#' The cumulative curve through the anchors of one series (linear between them,
#' 0 before the first, flat after the last) gives each WHO week -- Monday to
#' Sunday, stamped with its Monday -- its increment from the end of the previous
#' Sunday to the end of its own.
#'
#' @param anchors One id's anchors (\code{date} and the \code{value} column; rows
#'   where that column is NA are not anchors of this series).
#' @param week_start Date vector of the window's WHO week stamps (Mondays).
#' @param active Logical, the weeks that may carry counts (up to the week holding
#'   the curated \code{date_stop}).
#' @param id Curation id, for the error message.
#' @param value \code{"cumulative_cases"} (default) or \code{"cumulative_deaths"}.
#' @return Numeric weights, one per week, summing to the curve's total.
#' @noRd
.curated_shape_weights <- function(anchors, week_start, active, id, value = "cumulative_cases") {
     a <- anchors[!is.na(anchors[[value]]), ]
     y <- a[[value]][order(a$date)]
     x <- as.numeric(sort(a$date))
     cum <- function(t) stats::approx(x, y, xout = as.numeric(t), rule = 2, ties = "ordered")$y
     w <- cum(week_start + 6L) - cum(week_start - 1L)
     total <- y[length(y)]
     if (abs(sum(w[active]) - total) > 1e-8 * total || any(w[!active] > 0))
          stop(sprintf(paste0("Curated WHO window %s: its %s curve puts counts outside the window's weeks ",
                              "(%s to %s); the anchors must lie within them"),
                       id, if (value == "cumulative_deaths") "death" else "case",
                       format(min(week_start[active])), format(max(week_start[active]) + 6L)),
               call. = FALSE)
     w
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
