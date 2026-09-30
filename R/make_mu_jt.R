#' Build the daily reported-CFR matrix mu_jt from annual estimates
#'
#' @description
#' Expands per-location, per-year reported case fatality ratios (from
#' \code{\link{est_CFR_hierarchical}}) into the \[location x day\] matrix the
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
#'
#' @return Numeric matrix with \code{length(location_name)} rows and one column
#'   per day from \code{date_start} to \code{date_stop}; every value in (0, 1).
#'
#' @details Days past the last estimated year carry that year's value forward,
#'   so a simulation window that runs beyond the WHO annual record is filled
#'   with the most recent estimate rather than an extrapolated trend.
#'
#'   The values are the GAM's logit-scale centres, i.e. the median of each year's
#'   reported-CFR distribution, not its mean. That is the exact prior centre for
#'   the integrated deaths likelihood (whose year deviations have mean zero on the
#'   logit scale). A simulation that draws deaths directly at \code{mu_jt} -- a
#'   scenario run or a prior-predictive check -- runs 5-10% below WHO-annual
#'   deaths (2018-25); the logit-normal mean would over-predict, because annual
#'   CFR is lower in high-case years.
#'
#'   The default \code{"linear_logit"} centre and the integrated likelihood's
#'   year deviations use different time bases on purpose. The deviations are
#'   calendar-year levels blended over 30 days either side of 1 January
#'   (\code{.d7_basis()} in \code{calc_log_likelihood_deaths_integrated.R}),
#'   while the centre interpolates between 1 July anchors, so from January to
#'   June the centre sits partway toward the previous year's value. The
#'   resulting offset is at most the year-to-year change in the GAM centre
#'   (under 0.07 logit, about 3 percent of the CFR, for 2022-26 estimates) and is
#'   absorbed by the year deviation; years past the last estimate are held flat,
#'   so forecast windows are unaffected.
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
                       interpolation = c("linear_logit", "step")) {

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

     out
}


#' Build the priors \code{mu_jt} block from annual CFR estimates
#'
#' The prior the integrated deaths likelihood reads (\code{priors$mu_jt}): per
#' location and year the centre (\code{logit_mean}, the value \code{make_mu_jt()}
#' puts in the config) and the SE of the country-trend mean (\code{logit_se}),
#' plus the global widths. Used by \code{data-raw/make_priors_default.R} and by
#' \code{run_rolling_cv()} for its per-cutoff priors.
#'
#' @param cfr_estimates Data frame with \code{iso_code}, \code{year},
#'   \code{logit_mean} and \code{cfr_se} (\code{est_CFR_hierarchical()$predictions}).
#' @param location_name Character vector of ISO codes to include.
#' @param sd_year Positive scalar: the GAM country-year SD (sigma).
#' @param tau Scalar: the GAM between-country SD (reference only).
#' @param sd_product Positive scalar: residual error of the GAM centre against the
#'   observed reported CFR in a calibration window, on the logit scale.
#' @param year_min Integer: first year kept.
#' @return A list with \code{description}, \code{sd_year}, \code{sd_product},
#'   \code{tau} and \code{location} (one list of \code{year}, \code{logit_mean},
#'   \code{logit_se} per ISO code).
#' @keywords internal
.mosaic_mu_jt_prior <- function(cfr_estimates, location_name, sd_year, tau,
                                sd_product = 0.3, year_min = 2010L) {
     for (nm in c("iso_code", "year", "logit_mean", "cfr_se"))
          if (!nm %in% names(cfr_estimates)) stop("cfr_estimates lacks column ", nm)
     if (!is.numeric(sd_year) || length(sd_year) != 1L || !is.finite(sd_year) || sd_year <= 0)
          stop("sd_year must be a single positive number.")
     if (!is.numeric(sd_product) || length(sd_product) != 1L || !is.finite(sd_product) || sd_product <= 0)
          stop("sd_product must be a single positive number.")
     missing_loc <- setdiff(location_name, cfr_estimates$iso_code)
     if (length(missing_loc))
          stop("cfr_estimates has no rows for: ", paste(missing_loc, collapse = ", "))
     out <- list(
          description = paste0(
               "Reported case fatality ratio (reported deaths per reported suspected case) by location and year: ",
               "the prior for the reported CFR that run_MOSAIC() integrates out per simulated path. ",
               "Centres (logit_mean) are the est_CFR_hierarchical() WHO-annual GAM estimates that config$mu_jt ",
               "is built from; logit_se is the SE of the country-trend mean. The CFR is logit mu0_jt + a_j + delta_{j,y}, ",
               "with a_j ~ N(0, sd_product^2 + mean logit_se^2) and delta_{j,y} ~ N(0, sd_year^2). ",
               "sd_year is the GAM country-year SD; sd_product (0.3) is the residual error of the GAM centre against ",
               "the observed reported CFR in the calibration window (sd(log) 0.19-0.32 over the 15-17 countries with ",
               ">= 50 deaths, 2023-26; the WHO-annual and weekly surveillance products agree to sd(log) 0.03). ",
               "Not sampled."),
          sd_year    = unname(sd_year),
          sd_product = sd_product,
          tau        = unname(tau),
          location   = list())
     for (iso in location_name) {
          d <- cfr_estimates[cfr_estimates$iso_code == iso & cfr_estimates$year >= year_min, , drop = FALSE]
          d <- d[order(d$year), , drop = FALSE]
          if (!nrow(d) || any(!is.finite(d$logit_mean)) || any(!is.finite(d$cfr_se) | d$cfr_se <= 0))
               stop("Invalid mu_jt prior rows for ", iso)
          out$location[[iso]] <- list(year = as.integer(d$year),
                                      logit_mean = unname(d$logit_mean),
                                      logit_se = unname(d$cfr_se))
     }
     out
}
