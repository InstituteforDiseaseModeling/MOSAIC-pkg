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
#' Sorts values and weights, calculates cumulative weight distribution,
#' and uses linear interpolation to estimate quantiles at specified probability levels.
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' w <- c(0.1, 0.2, 0.4, 0.2, 0.1)
#' weighted_quantiles(x, w, c(0.25, 0.5, 0.75))
#'
#' @export
weighted_quantiles <- function(x, w, probs) {
  if (length(x) == 0) return(rep(NA_real_, length(probs)))
  
  # Remove non-finite values
  valid_idx <- is.finite(x) & is.finite(w) & w > 0
  if (sum(valid_idx) == 0) return(rep(NA_real_, length(probs)))
  
  x <- x[valid_idx]
  w <- w[valid_idx]
  
  # Handle single value case
  if (length(x) == 1) return(rep(x[1], length(probs)))
  
  # Handle constant values case
  if (length(unique(x)) == 1) return(rep(x[1], length(probs)))
  
  # Sort values and weights
  ord <- order(x)
  x_sorted <- x[ord]
  w_sorted <- w[ord]
  
  # Calculate cumulative weights
  cumsum_w <- cumsum(w_sorted)
  cumsum_w_norm <- cumsum_w / sum(w_sorted)
  
  # Check if we have at least 2 distinct cumulative weight values
  if (length(unique(cumsum_w_norm)) < 2) {
    # Fallback: use simple quantiles without weights
    return(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE))
  }
  
  # Interpolate quantiles
  tryCatch({
    stats::approx(cumsum_w_norm, x_sorted, xout = probs, rule = 2)$y
  }, error = function(e) {
    # Final fallback
    stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  })
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