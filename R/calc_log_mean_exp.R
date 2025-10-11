#' Stable log-mean-exp computation
#'
#' Computes the log of the mean of exponentials in a numerically stable way.
#' This is equivalent to log(mean(exp(x))) but avoids numerical overflow/underflow
#' by subtracting the maximum value before exponentiation.
#'
#' @param x Numeric vector of log-values
#'
#' @return Numeric scalar; the log-mean-exp of the input vector
#'
#' @section Details:
#' The log-mean-exp for a vector \eqn{x} is computed as:
#' \deqn{ \mathrm{LME}(x) = \max(x) + \log\left(\frac{1}{|x|}\sum_i e^{x_i - \max(x)}\right) }
#' 
#' This is numerically stable because:
#' \itemize{
#'   \item The maximum value is subtracted before exponentiation, preventing overflow
#'   \item At least one exponentiated term equals 1, preventing underflow
#'   \item Equivalent to \code{log(mean(exp(x)))} but without numerical issues
#' }
#'
#' @examples
#' # Basic usage
#' x <- c(-100, -101, -99)
#' calc_log_mean_exp(x)  # ≈ -99.59
#' 
#' # Compare with naive approach (which would overflow for large negative values)
#' log(mean(exp(x)))     # Same result, but less stable
#' 
#' # With missing/infinite values
#' calc_log_mean_exp(c(-50, -Inf, -60, NA))  # Returns log-mean-exp of finite values
#' 
#' # Empty or all non-finite input
#' calc_log_mean_exp(c())           # Returns NA
#' calc_log_mean_exp(c(-Inf, NA))   # Returns NA
#'
#' @seealso calc_model_collapse_iterations which uses this for likelihood aggregation
#' @family utility-functions
#' @export
calc_log_mean_exp <- function(x) {
  # Remove non-finite values (NA, NaN, Inf, -Inf)
  x <- x[is.finite(x)]
  
  # Return NA if no finite values remain
  if (length(x) == 0L) return(NA_real_)
  
  # Stable log-mean-exp computation
  m <- max(x)
  m + log(mean(exp(x - m)))
}