#' Fit Diagnostics: Bias, Shape, and Variance for a Single Observed/Predicted Series
#'
#' @description
#' Computes a structured set of fit-quality diagnostics comparing an observed
#' daily series to a model-predicted daily series, decomposed along three
#' independent dimensions: \strong{bias} (total scale, by year, and by epidemic
#' regime), \strong{shape} (peak timing, peak magnitude, onset slope, seasonal
#' correlation, and a timing-normalised shape correlation), and \strong{variance}
#' (coefficient-of-variation ratio and residual autocorrelation). A PASS/WARN/FAIL
#' scorecard summarises each dimension.
#'
#' This is the metrics engine behind the active fit-diagnostic workflow (see the
#' \code{diagnose-fit} skill and [run_fit_sandbox()]). It is country-agnostic: it
#' takes plain numeric vectors, so it can be unit-tested without running a model.
#'
#' @details
#' Diagnose each dimension in order. Bias is addressed first because shape and
#' variance diagnostics are confounded when the total scale is wrong. Reuses
#' [calc_model_R2()] (corr and SSE) and [calc_bias_ratio()] for the level metrics.
#'
#' The endemic/epidemic bias split is a pragmatic \emph{observed-magnitude} proxy:
#' days whose observed value is at or above \code{epidemic_threshold} are treated
#' as epidemic, the rest endemic. When \code{epidemic_threshold} is \code{NULL} the
#' threshold defaults to the 75th percentile of positive observed values. This is a
#' diagnostic heuristic, not the model's internal regime classifier.
#'
#' @param observed Numeric vector; observed daily series (NA allowed for missing days).
#' @param predicted Numeric vector; model-predicted daily series, same length as \code{observed}.
#' @param dates Date vector (or coercible) aligned to \code{observed}/\code{predicted}.
#' @param epidemic_threshold Optional numeric; observed value at/above which a day is
#'   classed epidemic for the bias split. Default \code{NULL} (75th percentile of
#'   positive observed values).
#' @param smooth_window Integer; centred running-mean window (days) used for peak
#'   detection. Default \code{28}.
#' @param na_rm Logical; drop NA pairs in level metrics. Default \code{TRUE}.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{bias}{list: \code{total}, \code{by_year} (named vector), \code{endemic}, \code{epidemic}.}
#'   \item{r2}{list: \code{corr}, \code{sse}, \code{by_year} (named vector, corr method).}
#'   \item{shape}{list: \code{peak_timing_error_by_year} (days; positive = model peaks later),
#'     \code{peak_magnitude_ratio_by_year} (pred/obs), \code{onset_slope_ratio},
#'     \code{seasonal_corr}, \code{shape_corr}.}
#'   \item{variance}{list: \code{cv_ratio} (pred CV / obs CV), \code{residual_autocorr_lag7}.}
#'   \item{scorecard}{named character vector: \code{bias}, \code{peak_timing},
#'     \code{peak_shape}, \code{variance}, each "PASS"|"WARN"|"FAIL".}
#' }
#'
#' @seealso [run_fit_sandbox()], [calc_model_R2()], [calc_bias_ratio()]
#' @export
calc_fit_diagnostics <- function(observed,
                                 predicted,
                                 dates,
                                 epidemic_threshold = NULL,
                                 smooth_window = 28L,
                                 na_rm = TRUE) {

  obs  <- as.numeric(observed)
  pred <- as.numeric(predicted)
  dts  <- as.Date(dates)

  n <- min(length(obs), length(pred), length(dts))
  if (n < 2L) stop("calc_fit_diagnostics: need at least two aligned observations.")
  obs <- obs[seq_len(n)]; pred <- pred[seq_len(n)]; dts <- dts[seq_len(n)]

  yr <- as.integer(format(dts, "%Y"))
  years <- sort(unique(yr))

  # ---- Bias ----------------------------------------------------------------
  bias_total <- calc_bias_ratio(obs, pred, na_rm = na_rm)
  bias_by_year <- vapply(years, function(y) {
    idx <- yr == y
    calc_bias_ratio(obs[idx], pred[idx], na_rm = na_rm)
  }, numeric(1))
  names(bias_by_year) <- as.character(years)

  thr <- if (is.null(epidemic_threshold)) {
    pos <- obs[is.finite(obs) & obs > 0]
    if (length(pos) >= 1L) stats::quantile(pos, 0.75, names = FALSE) else NA_real_
  } else as.numeric(epidemic_threshold)

  bias_epidemic <- bias_endemic <- NA_real_
  if (is.finite(thr)) {
    epi <- is.finite(obs) & obs >= thr
    end <- is.finite(obs) & obs <  thr
    if (any(epi)) bias_epidemic <- calc_bias_ratio(obs[epi], pred[epi], na_rm = na_rm)
    if (any(end)) bias_endemic  <- calc_bias_ratio(obs[end], pred[end], na_rm = na_rm)
  }

  # ---- R2 ------------------------------------------------------------------
  r2_corr <- calc_model_R2(obs, pred, method = "corr", na_rm = na_rm)
  r2_sse  <- calc_model_R2(obs, pred, method = "sse",  na_rm = na_rm)
  r2_by_year <- vapply(years, function(y) {
    idx <- yr == y
    calc_model_R2(obs[idx], pred[idx], method = "corr", na_rm = na_rm)
  }, numeric(1))
  names(r2_by_year) <- as.character(years)

  # ---- Shape: peaks per year ----------------------------------------------
  peak_timing <- stats::setNames(rep(NA_real_, length(years)), as.character(years))
  peak_mag    <- stats::setNames(rep(NA_real_, length(years)), as.character(years))
  for (y in years) {
    idx <- which(yr == y)
    if (length(idx) < smooth_window) next
    o_pk <- .fit_annual_peak(obs[idx],  dts[idx], smooth_window)
    p_pk <- .fit_annual_peak(pred[idx], dts[idx], smooth_window)
    ky <- as.character(y)
    if (!is.null(o_pk) && !is.null(p_pk)) {
      # positive => model peaks LATER than observed
      peak_timing[ky] <- as.numeric(p_pk$day - o_pk$day)
      if (is.finite(o_pk$value) && o_pk$value > 0) {
        peak_mag[ky] <- p_pk$value / o_pk$value
      }
    }
  }

  # ---- Shape: onset slope ratio (largest observed-peak year) ---------------
  onset_slope_ratio <- NA_real_
  if (length(years)) {
    peak_year <- years[which.max(vapply(years, function(y) {
      v <- obs[yr == y]; if (all(is.na(v))) -Inf else max(v, na.rm = TRUE)
    }, numeric(1)))]
    idx <- which(yr == peak_year)
    os <- .fit_onset_slope(obs[idx],  dts[idx], smooth_window)
    ps <- .fit_onset_slope(pred[idx], dts[idx], smooth_window)
    if (is.finite(os) && os != 0 && is.finite(ps)) onset_slope_ratio <- ps / os
  }

  # ---- Shape: seasonal correlation (month-of-year means) -------------------
  mo <- as.integer(format(dts, "%m"))
  obs_m  <- tapply(obs,  mo, function(v) mean(v, na.rm = TRUE))
  pred_m <- tapply(pred, mo, function(v) mean(v, na.rm = TRUE))
  common <- intersect(names(obs_m), names(pred_m))
  seasonal_corr <- if (length(common) >= 3L) {
    tryCatch(suppressWarnings(stats::cor(obs_m[common], pred_m[common], use = "complete.obs")),
             error = function(e) NA_real_)
  } else NA_real_

  # ---- Shape: timing-normalised shape correlation --------------------------
  shape_corr <- .fit_shape_corr(obs, pred)

  # ---- Variance ------------------------------------------------------------
  cv_ratio <- .fit_cv_ratio(obs, pred)
  residual_autocorr_lag7 <- .fit_residual_autocorr(obs, pred, lag = 7L)

  # ---- Scorecard -----------------------------------------------------------
  # peak_shape grades on shape_corr (area-normalised, bias-free) so it does not
  # re-detect overall bias; peak_magnitude_ratio is reported raw above.
  # variance takes the worst of the CV-ratio grade and the residual-autocorr grade.
  scorecard <- c(
    bias        = .fit_grade(.fit_dev(bias_total),                 pass = 1.2,  warn = 2.0),
    peak_timing = .fit_grade(mean(abs(peak_timing), na.rm = TRUE), pass = 7,    warn = 21),
    peak_shape  = .fit_grade(if (is.finite(shape_corr)) max(0, 1 - shape_corr) else NA_real_,
                             pass = 0.15, warn = 0.4),
    variance    = .fit_worst(
      .fit_grade(.fit_dev(cv_ratio),               pass = 1.25, warn = 1.6),
      .fit_grade(abs(residual_autocorr_lag7),      pass = 0.2,  warn = 0.4)
    )
  )

  list(
    bias = list(total = bias_total, by_year = bias_by_year,
                endemic = bias_endemic, epidemic = bias_epidemic),
    r2 = list(corr = r2_corr, sse = r2_sse, by_year = r2_by_year),
    shape = list(peak_timing_error_by_year = peak_timing,
                 peak_magnitude_ratio_by_year = peak_mag,
                 onset_slope_ratio = onset_slope_ratio,
                 seasonal_corr = seasonal_corr,
                 shape_corr = shape_corr),
    variance = list(cv_ratio = cv_ratio,
                    residual_autocorr_lag7 = residual_autocorr_lag7),
    scorecard = scorecard
  )
}

# ---- Internal helpers ------------------------------------------------------

# Smoothed peak (day + value) within one year's segment. Returns list(day, value) or NULL.
.fit_annual_peak <- function(values, dates, smooth_window) {
  v <- as.numeric(values)
  if (sum(is.finite(v)) < 3L) return(NULL)
  k <- max(3L, as.integer(smooth_window))
  sm <- tryCatch(
    zoo::rollapply(zoo::zoo(v), width = k, FUN = function(x) mean(x, na.rm = TRUE),
                   partial = TRUE, align = "center"),
    error = function(e) v
  )
  sm <- as.numeric(sm)
  if (all(!is.finite(sm))) return(NULL)
  i <- which.max(sm)
  list(day = as.Date(dates[i]), value = sm[i])
}

# Onset slope: rise from the trough preceding the peak to the peak (units/day).
.fit_onset_slope <- function(values, dates, smooth_window) {
  pk <- .fit_annual_peak(values, dates, smooth_window)
  if (is.null(pk)) return(NA_real_)
  v <- as.numeric(values)
  ipk <- which(as.Date(dates) == pk$day)[1]
  if (is.na(ipk) || ipk <= 1L) return(NA_real_)
  pre <- v[seq_len(ipk)]
  itr <- which.min(replace(pre, !is.finite(pre), Inf))
  if (!is.finite(itr) || itr >= ipk) return(NA_real_)
  rise <- v[ipk] - v[itr]
  span <- as.numeric(as.Date(dates[ipk]) - as.Date(dates[itr]))
  if (!is.finite(rise) || !is.finite(span) || span <= 0) return(NA_real_)
  rise / span
}

# Correlation of area-normalised series (decouples scale, retains shape/timing).
.fit_shape_corr <- function(obs, pred) {
  o <- obs; p <- pred
  ok <- is.finite(o) & is.finite(p)
  if (sum(ok) < 3L) return(NA_real_)
  o <- o[ok]; p <- p[ok]
  so <- sum(o); sp <- sum(p)
  if (!is.finite(so) || !is.finite(sp) || so <= 0 || sp <= 0) return(NA_real_)
  suppressWarnings(stats::cor(o / so, p / sp))
}

# CV ratio = (sd(pred)/mean(pred)) / (sd(obs)/mean(obs)).
.fit_cv_ratio <- function(obs, pred) {
  cv <- function(x) {
    x <- x[is.finite(x)]
    m <- mean(x)
    if (length(x) < 2L || !is.finite(m) || m == 0) return(NA_real_)
    stats::sd(x) / m
  }
  cv_o <- cv(obs); cv_p <- cv(pred)
  if (!is.finite(cv_o) || cv_o == 0 || !is.finite(cv_p)) return(NA_real_)
  cv_p / cv_o
}

# Residual (obs - pred) autocorrelation at a given lag.
.fit_residual_autocorr <- function(obs, pred, lag = 7L) {
  r <- obs - pred
  r <- r[is.finite(r)]
  if (length(r) <= lag + 1L) return(NA_real_)
  ac <- tryCatch(stats::acf(r, lag.max = lag, plot = FALSE, demean = TRUE)$acf,
                 error = function(e) NULL)
  if (is.null(ac) || length(ac) < lag + 1L) return(NA_real_)
  as.numeric(ac[lag + 1L])
}

# Symmetric deviation of a ratio-like value from a reference: max(x/ref, ref/x).
# So 0.5 and 2.0 both map to 2.0. NA-safe (returns Inf so it grades FAIL/NA cleanly).
.fit_dev <- function(x, ref = 1) {
  if (!is.finite(x) || x <= 0) return(NA_real_)
  max(x / ref, ref / x)
}

# Grade a non-negative deviation against PASS/WARN cut points.
.fit_grade <- function(value, pass, warn) {
  if (!is.finite(value)) return("NA")
  if (value < pass) return("PASS")
  if (value < warn) return("WARN")
  "FAIL"
}

# Worst (most severe) of several PASS/WARN/FAIL/NA grades.
.fit_worst <- function(...) {
  g <- c(...)
  rank <- c(PASS = 1L, WARN = 2L, FAIL = 3L, "NA" = 0L)
  known <- g[g %in% names(rank)]
  if (!length(known)) return("NA")
  names(which.max(rank[known]))
}
