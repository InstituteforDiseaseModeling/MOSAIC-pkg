#' Coefficient of Determination (R-squared) Between Observed and Estimated Series
#'
#' @description
#' Computes \eqn{R^2} between an observed series and model estimates.
#' Supports two definitions:
#' \itemize{
#'   \item \code{method = "corr"} (default): \eqn{R^2 = \mathrm{cor}(y,\hat y)^2} (Pearson), always in \eqn{[0,1]}.
#'   \item \code{method = "sse"}: \eqn{R^2 = 1 - \mathrm{SSE}/\mathrm{SST}} with optional weights.
#'         This can be \emph{negative} when the model underperforms the intercept-only baseline.
#' }
#'
#' @param observed Numeric vector or matrix.
#' @param estimated Numeric vector or matrix (same length after flattening).
#' @param method One of \code{"corr"} (default) or \code{"sse"}.
#' @param bounded Logical; if \code{TRUE}, clamp the result to \eqn{[0,1]}. Default \code{FALSE}.
#' @param na_rm Logical; drop NA pairs. Default \code{TRUE}.
#' @param finite_only Logical; drop non-finite values. Default \code{TRUE}.
#' @param weights Optional non-negative weights (used for \code{method="sse"} and weighted Pearson in \code{"corr"}).
#' @param verbose Logical; if \code{TRUE}, emit brief diagnostics.
#'
#' @return Scalar \eqn{R^2}. May be negative for \code{method="sse"} unless \code{bounded=TRUE}.
#'
#' @seealso [calc_model_mse()], [calc_model_cor()]
#' @export
calc_model_R2 <- function(observed,
                          estimated,
                          method = c("corr", "sse"),
                          bounded = FALSE,
                          na_rm = TRUE,
                          finite_only = TRUE,
                          weights = NULL,
                          verbose = FALSE) {

     method <- match.arg(method)

     y  <- as.numeric(observed)
     yh <- as.numeric(estimated)

     if (length(y) != length(yh)) {
          if (verbose) message("calc_model_R2: length mismatch after flattening.")
          return(NA_real_)
     }

     # Pairwise filtering
     if (!na_rm && (any(is.na(y)) || any(is.na(yh)))) {
          if (verbose) message("calc_model_R2: NA present and na_rm = FALSE -> NA.")
          return(NA_real_)
     }
     valid <- !(is.na(y) | is.na(yh))
     if (finite_only) valid <- valid & is.finite(y) & is.finite(yh)
     if (sum(valid) < 2L) {
          if (verbose) message("calc_model_R2: fewer than two valid pairs.")
          return(NA_real_)
     }
     y  <- y[valid]; yh <- yh[valid]

     # Handle weights
     if (!is.null(weights)) {
          w <- as.numeric(weights)[valid]
          if (length(w) == 1L) w <- rep(w, length(y))
          if (length(w) != length(y) || any(!is.finite(w)) || any(w < 0)) {
               if (verbose) message("calc_model_R2: invalid weights (length/finite/non-negative).")
               return(NA_real_)
          }
     } else {
          w <- rep(1, length(y))
     }
     sw <- sum(w)
     if (sw <= 0) {
          if (verbose) message("calc_model_R2: sum of weights is zero.")
          return(NA_real_)
     }

     r2 <- NA_real_

     if (method == "sse") {
          # Classic SSE/SST definition (can be negative)
          ybar_w <- sum(w * y) / sw
          sse <- sum(w * (y - yh)^2)
          sst <- sum(w * (y - ybar_w)^2)

          if (!is.finite(sst) || sst <= 0) {
               if (verbose) message("calc_model_R2: SST is non-positive; R^2 undefined.")
               return(NA_real_)
          }
          r2 <- 1 - (sse / sst)

          # Numerical guard against tiny roundoff > 1
          if (is.finite(r2)) r2 <- min(r2, 1)

     } else {
          # Correlation-squared definition (bounded)
          r <- calc_model_cor(y, yh, method = "pearson",
                              na_rm = TRUE, finite_only = TRUE,
                              weights = if (!is.null(weights)) w else NULL,
                              verbose = FALSE)
          if (!is.finite(r)) return(NA_real_)
          r2 <- r * r
     }

     if (bounded && is.finite(r2)) {
          r2 <- max(0, min(r2, 1))
     }

     r2
}


#' Bias Ratio Between Estimated and Observed Series
#'
#' @description
#' Computes \code{mean(estimated) / mean(observed)} as a measure of systematic
#' over- or under-prediction. A value of 1.0 indicates no bias. Pairs with
#' \code{calc_model_R2(method = "corr")} to give both pattern agreement and
#' magnitude calibration.
#'
#' @param observed Numeric vector or matrix.
#' @param estimated Numeric vector or matrix (same length after flattening).
#' @param na_rm Logical; drop NA pairs. Default \code{TRUE}.
#' @param finite_only Logical; drop non-finite values. Default \code{TRUE}.
#'
#' @return Scalar ratio. Values > 1 indicate over-prediction, < 1 under-prediction.
#'   Returns \code{NA_real_} if observed mean is zero or no valid pairs exist.
#'
#' @seealso [calc_model_R2()]
#' @export
calc_bias_ratio <- function(observed,
                            estimated,
                            na_rm = TRUE,
                            finite_only = TRUE) {

  y  <- as.numeric(observed)
  yh <- as.numeric(estimated)

  if (length(y) != length(yh)) return(NA_real_)

  valid <- !(is.na(y) | is.na(yh))
  if (finite_only) valid <- valid & is.finite(y) & is.finite(yh)
  if (sum(valid) < 1L) return(NA_real_)

  y  <- y[valid]
  yh <- yh[valid]

  mean_obs <- mean(y)
  if (!is.finite(mean_obs) || mean_obs == 0) return(NA_real_)

  mean(yh) / mean_obs
}
