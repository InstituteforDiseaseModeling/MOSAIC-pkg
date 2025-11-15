#' Fit Uniform Distribution from Mode and 95% Confidence Intervals
#'
#' This function calculates the min and max parameters of a uniform distribution
#' that best matches a given mode and 95% confidence intervals.
#'
#' @param mode_val Numeric. The mode of the distribution (any value within range for uniform).
#' @param ci_lower Numeric. The lower bound of the 95% confidence interval.
#' @param ci_upper Numeric. The upper bound of the 95% confidence interval.
#' @param method Character. Method to use: "exact" (default) or "extended".
#' @param extend_factor Numeric. Factor to extend bounds beyond CI (default: 1.026 for exact 95% CI).
#' @param verbose Logical. If TRUE, print diagnostic information.
#'
#' @return A list containing:
#' \itemize{
#'   \item min: The minimum parameter of the uniform distribution
#'   \item max: The maximum parameter of the uniform distribution
#'   \item mean: The mean of the fitted distribution (min + max)/2
#'   \item sd: The standard deviation of the fitted distribution
#'   \item fitted_ci: The 95% CI of the fitted distribution
#'   \item mode: The mode (undefined for uniform, returns NA)
#' }
#'
#' @details
#' For a uniform distribution U(min, max):
#' \itemize{
#'   \item Mean = (min + max) / 2
#'   \item Variance = (max - min)² / 12
#'   \item Mode is undefined (any value in \\code{[min, max]} is equally likely)
#'   \item 95% CI covers central 95% of the range
#' }
#'
#' The "exact" method sets bounds so that the 95% CI exactly matches the input CI.
#' For U(a, b), the 2.5th percentile is a + 0.025*(b-a) and the 97.5th percentile
#' is a + 0.975*(b-a).
#'
#' The "extended" method allows for bounds slightly beyond the CI for a more
#' conservative estimate.
#'
#' @examples
#' # Example 1: Exact fit to CI
#' result <- fit_uniform_from_ci(mode_val = 0.5,
#'                                ci_lower = 0.1,
#'                                ci_upper = 0.9)
#' print(result)
#'
#' # Example 2: Extended bounds for conservative estimate
#' result <- fit_uniform_from_ci(mode_val = 5,
#'                                ci_lower = 2,
#'                                ci_upper = 8,
#'                                method = "extended",
#'                                extend_factor = 1.05)
#'
#' @export
fit_uniform_from_ci <- function(mode_val, ci_lower, ci_upper,
                                method = "exact",
                                extend_factor = 1.026,
                                verbose = FALSE) {

  # Validate inputs
  if (!is.numeric(mode_val) || !is.numeric(ci_lower) || !is.numeric(ci_upper)) {
    stop("All inputs must be numeric")
  }

  if (ci_lower >= ci_upper) {
    stop("ci_lower must be less than ci_upper")
  }

  # For uniform, mode can be anywhere in the range
  if (mode_val < ci_lower || mode_val > ci_upper) {
    warning("Mode is outside CI range. For uniform distribution, mode is undefined.")
  }

  if (method == "exact") {
    # Method 1: Exact fit where 95% CI matches input CI
    # For uniform U(a,b):
    # 2.5th percentile = a + 0.025*(b-a)
    # 97.5th percentile = a + 0.975*(b-a)
    # So the 95% CI covers 0.95*(b-a) of the range

    # From the CI, we can derive:
    # ci_upper - ci_lower = 0.95*(max - min)
    # Therefore: max - min = (ci_upper - ci_lower) / 0.95

    range_width <- (ci_upper - ci_lower) / 0.95

    # The 2.5th percentile is at ci_lower, so:
    # ci_lower = min + 0.025 * range_width
    # min = ci_lower - 0.025 * range_width

    min_val <- ci_lower - 0.025 * range_width
    max_val <- ci_upper + 0.025 * range_width

  } else if (method == "extended") {
    # Method 2: Extended bounds for conservative coverage
    # Extend the bounds beyond the CI by the specified factor

    ci_width <- ci_upper - ci_lower
    extension <- (extend_factor - 1) * ci_width / 2

    min_val <- ci_lower - extension
    max_val <- ci_upper + extension

    if (verbose) {
      message(sprintf("Extended bounds by factor %.3f", extend_factor))
    }

  } else {
    stop("Method must be 'exact' or 'extended'")
  }

  # Calculate fitted statistics
  fitted_mean <- (min_val + max_val) / 2
  fitted_var <- (max_val - min_val)^2 / 12
  fitted_sd <- sqrt(fitted_var)

  # Calculate fitted CI (2.5th and 97.5th percentiles)
  fitted_ci <- c(
    qunif(0.025, min = min_val, max = max_val),
    qunif(0.975, min = min_val, max = max_val)
  )

  # Prepare output
  output <- list(
    min = min_val,
    max = max_val,
    mean = fitted_mean,
    sd = fitted_sd,
    fitted_ci = fitted_ci,
    mode = NA  # Mode is undefined for uniform distribution
  )

  if (verbose) {
    message("Fitted Uniform Distribution:")
    message(sprintf("  Min: %.6f", min_val))
    message(sprintf("  Max: %.6f", max_val))
    message(sprintf("  Mean: %.6f", fitted_mean))
    message(sprintf("  SD: %.6f", fitted_sd))
    message(sprintf("  Range width: %.6f", max_val - min_val))
    message(sprintf("  Fitted 95%% CI: [%.6f, %.6f]", fitted_ci[1], fitted_ci[2]))
    message(sprintf("  Target 95%% CI: [%.6f, %.6f]", ci_lower, ci_upper))

    # Check fit quality
    ci_error_lower <- abs(fitted_ci[1] - ci_lower)
    ci_error_upper <- abs(fitted_ci[2] - ci_upper)
    if (ci_error_lower > 1e-6 || ci_error_upper > 1e-6) {
      message("Warning: Fitted CI does not exactly match target CI")
      message(sprintf("  Lower CI error: %.6e", ci_error_lower))
      message(sprintf("  Upper CI error: %.6e", ci_error_upper))
    }
  }

  return(output)
}