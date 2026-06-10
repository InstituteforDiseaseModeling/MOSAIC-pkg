#' Weighted Statistical Functions for Posterior Analysis
#'
#' Helper functions for calculating weighted statistics used in posterior
#' parameter distribution analysis.

#' Weighted variance calculation
#'
#' Calculates the bias-corrected weighted variance for a vector of values.
#'
#' @param x Numeric vector of values
#' @param w Numeric vector of weights (same length as x)
#' @return Weighted variance (scalar)
#'
#' @details
#' Uses the bias correction: Var = sum(w * (x - mu)^2) / (sum(w) - sum(w^2)/sum(w))
#' where mu is the weighted mean. Returns 0 for single values or invalid denominators.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' w <- c(0.1, 0.2, 0.4, 0.2, 0.1)
#' weighted_var(x, w)
#'
#' @export
weighted_var <- function(x, w) {
  if (length(x) <= 1) return(0)
  
  mu <- weighted.mean(x, w)
  sum_w <- sum(w)
  sum_w2 <- sum(w^2)
  
  # Bias correction
  numerator <- sum(w * (x - mu)^2)
  denominator <- sum_w - sum_w2 / sum_w
  
  if (denominator <= 0) return(0)
  numerator / denominator
}

#' Weighted quantiles from already-sorted, pre-filtered inputs
#'
#' Internal core of \code{\link{weighted_quantiles}}: the cumulative-weight
#' interpolation, assuming inputs are already filtered (finite values, positive
#' weights) and sorted ascending by value with weights aligned. Exposed so hot
#' callers that sort once and reuse the order across many subsets (e.g.
#' \code{\link{optimize_ensemble_subset}}) can skip the per-call \code{order()}.
#'
#' @param x_sorted Numeric vector of values, sorted ascending, all finite.
#' @param w_sorted Numeric vector of weights aligned with \code{x_sorted}, all
#'   finite and positive.
#' @param probs Numeric vector of quantile probabilities (between 0 and 1).
#' @return Vector of weighted quantiles, one per element of \code{probs}.
#' @seealso \code{\link{weighted_quantiles}}
#' @export
weighted_quantiles_presorted <- function(x_sorted, w_sorted, probs) {
  n <- length(x_sorted)
  if (n == 0) return(rep(NA_real_, length(probs)))

  # Single / constant value
  if (n == 1) return(rep(x_sorted[1], length(probs)))
  if (length(unique(x_sorted)) == 1) return(rep(x_sorted[1], length(probs)))

  # Cumulative normalized weights (strictly increasing when all w > 0)
  cumsum_w_norm <- cumsum(w_sorted) / sum(w_sorted)

  # Need at least 2 distinct cumulative levels to interpolate
  if (length(unique(cumsum_w_norm)) < 2) {
    return(stats::quantile(x_sorted, probs = probs, na.rm = TRUE, names = FALSE))
  }

  tryCatch({
    stats::approx(cumsum_w_norm, x_sorted, xout = probs, rule = 2)$y
  }, error = function(e) {
    stats::quantile(x_sorted, probs = probs, na.rm = TRUE, names = FALSE)
  })
}

#' Weighted quantiles
#'
#' Calculates weighted quantiles for a vector of values using linear interpolation.
#'
#' @param x Numeric vector of values
#' @param w Numeric vector of weights (same length as x)
#' @param probs Numeric vector of quantile probabilities (between 0 and 1)
#' @return Vector of weighted quantiles
#'
#' @details
#' Drops non-finite values and non-positive weights, sorts the survivors by
#' value, then delegates to \code{\link{weighted_quantiles_presorted}} for the
#' cumulative-weight interpolation. (Splitting out the sorted core lets hot
#' callers sort once and reuse the order; the public behaviour is unchanged.)
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' w <- c(0.1, 0.2, 0.4, 0.2, 0.1)
#' weighted_quantiles(x, w, c(0.25, 0.5, 0.75))
#'
#' @export
weighted_quantiles <- function(x, w, probs) {
  if (length(x) == 0) return(rep(NA_real_, length(probs)))

  # Remove non-finite values and non-positive weights
  valid_idx <- is.finite(x) & is.finite(w) & w > 0
  if (sum(valid_idx) == 0) return(rep(NA_real_, length(probs)))

  x <- x[valid_idx]
  w <- w[valid_idx]

  # Sort by value and delegate to the presorted core
  ord <- order(x)
  weighted_quantiles_presorted(x[ord], w[ord], probs)
}

#' Weighted mode estimation using kernel density
#'
#' Estimates the mode of a weighted distribution using kernel density estimation.
#'
#' @param x Numeric vector of values
#' @param w Numeric vector of weights (same length as x)
#' @return Estimated mode (scalar)
#'
#' @details
#' Uses kernel density estimation with weights to find the mode (peak density).
#' Falls back to weighted median if density estimation fails.
#'
#' @examples
#' x <- c(1, 2, 2, 3, 3, 3, 4, 4, 5)
#' w <- rep(1, 9)
#' calc_weighted_mode(x, w)
#'
#' @export
calc_weighted_mode <- function(x, w) {
  if (length(unique(x)) <= 1) return(x[1])

  tryCatch({
    # Suppress expected warning about bandwidth not using weights (known R limitation)
    dens <- suppressWarnings(
      stats::density(x, weights = w / sum(w), adjust = 1.2, n = 512)
    )
    dens$x[which.max(dens$y)]
  }, error = function(e) {
    # Fallback to weighted median
    weighted_quantiles(x, w, 0.5)
  })
}