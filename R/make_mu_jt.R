#' Build the daily reported-CFR matrix mu_jt from annual estimates
#'
#' @description
#' Expands per-location, per-year reported case fatality ratios (from
#' \code{\link{est_CFR_hierarchical}}) into the [location x day] matrix the
#' engine reads as \code{config$mu_jt}. Values are interpolated linearly on the
#' logit scale between mid-year points, and held flat before the first and after
#' the last estimated year.
#'
#' @param cfr_estimates Data frame with columns \code{iso_code}, \code{year}
#'   and either \code{logit_mean} or \code{cfr_estimate} (for example
#'   \code{est_CFR_hierarchical()$predictions}, or the
#'   \code{cfr_hierarchical_estimates.csv} it writes).
#' @param location_name Character vector of ISO codes, in config row order.
#' @param date_start,date_stop First and last simulation dates (Date or character).
#' @param interpolation \code{"linear_logit"} (default) or \code{"step"} (the
#'   calendar-year value applies to every day of that year).
#' @param freeze_after Optional Date. Every day after it takes the value on that
#'   date (used by forecast cross-validation so no post-cutoff information enters).
#'
#' @return Numeric matrix with \code{length(location_name)} rows and one column
#'   per day from \code{date_start} to \code{date_stop}; every value in (0, 1).
#'
#' @details Days past the last estimated year carry that year's value forward,
#'   so a simulation window that runs beyond the WHO annual record is filled
#'   with the most recent estimate rather than an extrapolated trend.
#'
#' @seealso \code{\link{est_CFR_hierarchical}}
#' @examples
#' est <- data.frame(iso_code = rep(c("AAA", "BBB"), each = 3),
#'                   year = rep(2023:2025, 2),
#'                   cfr_estimate = c(0.02, 0.025, 0.03, 0.01, 0.01, 0.012))
#' mu <- make_mu_jt(est, c("AAA", "BBB"), "2023-01-01", "2025-12-31")
#' dim(mu)
#' @export
make_mu_jt <- function(cfr_estimates, location_name, date_start, date_stop,
                       interpolation = c("linear_logit", "step"),
                       freeze_after = NULL) {

     interpolation <- match.arg(interpolation)
     if (!is.data.frame(cfr_estimates) ||
         !all(c("iso_code", "year") %in% names(cfr_estimates)))
          stop("cfr_estimates must be a data frame with columns iso_code and year.")
     if (!is.null(cfr_estimates$logit_mean)) {
          lg <- as.numeric(cfr_estimates$logit_mean)
     } else if (!is.null(cfr_estimates$cfr_estimate)) {
          p <- as.numeric(cfr_estimates$cfr_estimate)
          if (any(!is.finite(p) | p <= 0 | p >= 1))
               stop("cfr_estimate values must lie in (0, 1).")
          lg <- stats::qlogis(p)
     } else {
          stop("cfr_estimates needs a logit_mean or a cfr_estimate column.")
     }
     if (any(!is.finite(lg))) stop("cfr_estimates contains non-finite values.")

     date_start <- as.Date(date_start); date_stop <- as.Date(date_stop)
     if (is.na(date_start) || is.na(date_stop) || date_stop < date_start)
          stop("date_start and date_stop must be valid dates with date_stop >= date_start.")
     dates <- seq.Date(date_start, date_stop, by = "day")

     location_name <- as.character(location_name)
     missing_loc <- setdiff(location_name, unique(as.character(cfr_estimates$iso_code)))
     if (length(missing_loc))
          stop("cfr_estimates has no rows for: ", paste(missing_loc, collapse = ", "))

     day_year <- as.integer(format(dates, "%Y"))
     # Mid-year anchor for each annual value (1 July), on a numeric day axis.
     day_num <- as.numeric(dates)

     out <- matrix(NA_real_, nrow = length(location_name), ncol = length(dates))
     for (i in seq_along(location_name)) {
          sel <- as.character(cfr_estimates$iso_code) == location_name[i]
          yrs <- as.integer(cfr_estimates$year[sel]); v <- lg[sel]
          if (anyDuplicated(yrs))
               stop("cfr_estimates has duplicate years for ", location_name[i], ".")
          o <- order(yrs); yrs <- yrs[o]; v <- v[o]
          if (interpolation == "step") {
               # A year without its own estimate takes the most recent earlier
               # one (the first estimate before the estimated range).
               out[i, ] <- stats::plogis(stats::approx(yrs, v, xout = day_year, method = "constant",
                                                       rule = 2, f = 0)$y)
          } else {
               mid <- as.numeric(as.Date(paste0(yrs, "-07-01")))
               if (length(mid) == 1L) {
                    out[i, ] <- stats::plogis(v)
               } else {
                    out[i, ] <- stats::plogis(stats::approx(mid, v, xout = day_num, rule = 2)$y)
               }
          }
     }

     if (!is.null(freeze_after)) out <- .mosaic_freeze_time_matrix(out, dates, freeze_after)
     out
}


#' Hold a [location x day] matrix constant after a date
#'
#' Every column dated after \code{cutoff} is replaced by the column at
#' \code{cutoff}. A cutoff before the first date freezes at the first column; a
#' cutoff on or after the last date leaves the matrix unchanged.
#'
#' @param mat Numeric matrix, one column per date.
#' @param dates Date vector, one entry per column of \code{mat}.
#' @param cutoff Date (or character coercible to Date).
#' @return \code{mat} with the post-cutoff columns replaced.
#' @keywords internal
.mosaic_freeze_time_matrix <- function(mat, dates, cutoff) {
     if (!is.matrix(mat)) mat <- matrix(mat, nrow = 1L)
     dates <- as.Date(dates); cutoff <- as.Date(cutoff)
     if (length(dates) != ncol(mat))
          stop("dates must have one entry per column of the matrix.")
     if (is.na(cutoff)) stop("cutoff must be a valid date.")
     after <- which(dates > cutoff)
     if (!length(after)) return(mat)
     anchor <- if (cutoff < dates[1]) 1L else max(which(dates <= cutoff))
     mat[, after] <- mat[, anchor]
     mat
}
