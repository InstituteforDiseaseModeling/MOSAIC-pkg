#' Per-country affine bias-calibration of suitability (psi) predictions
#'
#' Fits a per-country linear recalibration \eqn{obs \approx a_i \cdot pred + b_i}
#' on the \strong{training window only} and applies it to all predictions, clipped
#' to \eqn{[0, 1]}. Addresses the systematic negative bias (right shape, wrong
#' scale) of the suitability LSTM without altering its training.
#'
#' @details
#' For each \code{iso_code}, the affine coefficients are estimated by OLS on the
#' merged (prediction, observed) pairs with \code{date <= fit_date_stop} — i.e.
#' the training period only, so the calibration is leakage-clean with respect to
#' the out-of-sample window. Countries with fewer than \code{min_train} usable
#' training points (or zero predictor variance) fall back to the identity
#' transform with a warning.
#'
#' @param pred_df Data frame of predictions with at least \code{iso_code},
#'   \code{date}, and \code{pred_col}.
#' @param obs_df Data frame of observed targets with \code{iso_code},
#'   \code{date}, and \code{obs_col}.
#' @param fit_date_stop Date (or coercible); calibration is fit on
#'   \code{date <= fit_date_stop}.
#' @param pred_col Prediction column to calibrate (default \code{"pred_smooth"}).
#' @param obs_col Observed-target column (default \code{"transmission_intensity"}).
#' @param out_col Name of the calibrated output column (default
#'   \code{"pred_calibrated"}).
#' @param min_train Minimum training points per country to fit; otherwise identity
#'   (default 8).
#'
#' @return \code{pred_df} with an added \code{out_col} (calibrated, clipped to
#'   \eqn{[0,1]}).
#'
#' @seealso \code{\link{est_suitability}}
#' @importFrom stats lm predict reformulate sd
#' @export
calibrate_psi_predictions <- function(pred_df, obs_df, fit_date_stop,
                                      pred_col = "pred_smooth",
                                      obs_col  = "transmission_intensity",
                                      out_col  = "pred_calibrated",
                                      min_train = 8L) {

     stopifnot(all(c("iso_code", "date") %in% names(pred_df)),
               pred_col %in% names(pred_df),
               all(c("iso_code", "date") %in% names(obs_df)),
               obs_col %in% names(obs_df))

     pred_df$date  <- as.Date(pred_df$date)
     obs_df$date   <- as.Date(obs_df$date)
     fit_date_stop <- as.Date(fit_date_stop)

     m <- merge(pred_df[, c("iso_code", "date", pred_col)],
                obs_df[,  c("iso_code", "date", obs_col)],
                by = c("iso_code", "date"))
     m <- m[m$date <= fit_date_stop &
            is.finite(m[[pred_col]]) & is.finite(m[[obs_col]]), , drop = FALSE]

     pred_df[[out_col]] <- NA_real_
     n_identity <- 0L
     for (iso in unique(pred_df$iso_code)) {
          sel <- pred_df$iso_code == iso
          tr  <- m[m$iso_code == iso, , drop = FALSE]
          if (nrow(tr) >= min_train && stats::sd(tr[[pred_col]], na.rm = TRUE) > 0) {
               fit  <- stats::lm(stats::reformulate(pred_col, obs_col), data = tr)
               yhat <- stats::predict(fit, newdata = pred_df[sel, , drop = FALSE])
          } else {
               yhat <- pred_df[[pred_col]][sel]   # identity fallback
               n_identity <- n_identity + 1L
          }
          pred_df[[out_col]][sel] <- pmax(0, pmin(1, yhat))
     }
     if (n_identity > 0L) {
          warning(sprintf(
               "calibrate_psi_predictions: %d country/countries had < %d training points (or no predictor variance); used identity calibration for those.",
               n_identity, min_train), call. = FALSE)
     }
     pred_df
}
