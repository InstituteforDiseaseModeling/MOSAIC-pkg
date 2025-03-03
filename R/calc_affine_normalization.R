#' Affine Normalization of a Numeric Vector (Zero-Centered Min-Max Scaling)
#'
#' Transforms a numeric vector by centering it at zero and scaling it so that the
#' minimum value becomes \code{-1}. Specifically, the affine normalization is defined as:
#'
#' \deqn{scaled = \frac{x - \mu}{\mu - \min(x)}}
#'
#' where \eqn{\mu} is the mean of \code{x} computed with \code{na.rm = TRUE}.
#'
#' The transformation produces a vector with a mean that is approximately zero (ignoring
#' \code{NA} values), a minimum of \code{-1}, and a maximum above zero if the original
#' maximum exceeds the mean.
#'
#' @param x A numeric vector to be normalized. This vector may contain \code{NA} values.
#'
#' @return A numeric vector of the same length as \code{x} that has been affine normalized.
#'
#' @details The function computes the mean and minimum of \code{x} with \code{na.rm = TRUE}.
#'          If the mean equals the minimum (or is nearly equal within machine precision),
#'          the function issues a warning and returns a vector of \code{NA} values to
#'          avoid division by zero.
#'
#' @examples
#' # Example vector with NA values
#' x <- c(3, 5, 7, NA, 10, 12)
#'
#' # Apply affine normalization
#' normalized_x <- calc_affine_normalization(x)
#'
#' # The normalized vector should have a mean of approximately zero,
#' # a minimum of -1, and a maximum above zero (if the original maximum exceeds the mean).
#'
#' @export
#'

calc_affine_normalization <- function(x) {

     # If all values in x are NA, return x as is.
     if (all(is.na(x))) {
          return(x)
     }

     # Calculate mean and minimum ignoring NA values
     m <- mean(x, na.rm = TRUE)
     min_x <- min(x, na.rm = TRUE)

     # Check for division by zero when the mean equals the minimum
     if (abs(m - min_x) < .Machine$double.eps) {

          warning("Division by zero: mean equals minimum. Returning NA values.")
          return(rep(NA, length(x)))

     }

     # Perform the affine normalization transformation
     scaled <- (x - m) / (m - min_x)

     return(scaled)
}
