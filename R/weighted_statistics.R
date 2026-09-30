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
#' Internal core of \code{\link{weighted_quantiles}}: the midpoint-position
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

  # Plotting positions: the MIDPOINT of each observation's weight block,
  # (cumsum(w) - w/2) / sum(w), not its upper edge cumsum(w)/sum(w).
  #
  # Using the upper edge biases every quantile downward, because it credits each
  # observation with the whole of its own weight before interpolating to it. The
  # bias is invisible at equal weights spread thin and severe when weight
  # concentrates -- which is exactly the BFRS posterior regime this function is
  # used in. Two checks that pin it: with x = c(1, 2) and 99% of the weight on
  # x = 2 the upper-edge form returns 1.49 rather than ~2; and this function's
  # own documented example (x = 1:5, w = c(.1, .2, .4, .2, .1)) is symmetric
  # about 3 and returned 2.5. The midpoint form returns 1.99 and 3.
  #
  # For equal weights these positions are (i - 0.5)/n, so the unweighted case
  # reduces to the standard Hazen/type-5 quantile and agrees with median() on a
  # symmetric sample. Positions never reach 0 or 1, so rule = 2 clamps probs = 0
  # and probs = 1 to min(x) and max(x), which is the intended behaviour.
  sw <- sum(w_sorted)
  pos <- (cumsum(w_sorted) - 0.5 * w_sorted) / sw

  # Need at least 2 distinct positions to interpolate
  if (length(unique(pos)) < 2) {
    return(stats::quantile(x_sorted, probs = probs, na.rm = TRUE, names = FALSE))
  }

  # Weights spanning many orders of magnitude (the Gibbs weight floor is 1e-15)
  # make consecutive positions collide in double precision. approx()'s default
  # tie handling averages y over tied x, which is the right reduction here, but
  # it warns once per call -- 46,672 warnings in a single ensemble reduce during
  # A-5. Collapse the ties explicitly so the reduction is stated in this code
  # rather than left to a warning, and so callers' logs stay readable.
  if (anyDuplicated(pos)) {
    # pos is non-decreasing, so tied positions are consecutive and cumsum() of
    # the first-occurrence flag is a group index already in ascending order.
    # Collapse each group by its WEIGHTED mean, not approx()'s unweighted one:
    # a tied position means that observation's weight vanished against the
    # running sum, so giving it equal say in the collapsed value would restore
    # influence the tie says it does not have.
    keep <- !duplicated(pos)
    g  <- cumsum(keep)
    # Both callers guarantee w > 0 (weighted_quantiles() filters, and this
    # function documents it as a precondition), so every group sum is positive
    # and needs no zero-divisor guard -- one here could never fire.
    x_sorted <- as.vector(rowsum(x_sorted * w_sorted, g)) /
                as.vector(rowsum(w_sorted, g))
    pos <- pos[keep]
    if (length(pos) < 2) {
      return(stats::quantile(x_sorted, probs = probs, na.rm = TRUE, names = FALSE))
    }
  }

  tryCatch({
    stats::approx(pos, x_sorted, xout = probs, rule = 2, ties = "ordered")$y
  }, error = function(e) {
    stats::quantile(x_sorted, probs = probs, na.rm = TRUE, names = FALSE)
  })
}

#' Weighted quantiles
#'
#' Calculates weighted quantiles for a vector of values using linear
#' interpolation between midpoint plotting positions.
#'
#' @param x Numeric vector of values
#' @param w Numeric vector of weights (same length as x)
#' @param probs Numeric vector of quantile probabilities (between 0 and 1)
#' @return Vector of weighted quantiles
#'
#' @details
#' Drops non-finite values and non-positive weights, sorts the survivors by
#' value, then delegates to \code{\link{weighted_quantiles_presorted}} for the
#' interpolation. (Splitting out the sorted core lets hot callers sort once and
#' reuse the order.)
#'
#' Each observation is placed at the midpoint of its own weight block,
#' \eqn{(\sum_{k \le i} w_k - w_i/2) / \sum_k w_k}, and quantiles are linearly
#' interpolated between those positions. For equal weights the positions are
#' \eqn{(i - 0.5)/n}, so the unweighted case reduces to the standard Hazen
#' (type-5) quantile. Before v0.71.1 the upper edge \eqn{\sum_{k \le i} w_k}
#' was used instead, which biased every quantile downward in proportion to how
#' concentrated the weights were; see NEWS for the size of the effect on
#' \code{calc_model_ensemble()}.
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