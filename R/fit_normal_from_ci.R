#' Fit Normal Distribution from Mode and 95% Confidence Intervals
#'
#' This function calculates the mean and standard deviation parameters of a normal
#' distribution that best matches a given mode and 95% confidence intervals.
#'
#' @param mode_val Numeric. The mode of the distribution (equals mean for normal).
#' @param ci_lower Numeric. The lower bound of the 95% confidence interval.
#' @param ci_upper Numeric. The upper bound of the 95% confidence interval.
#' @param method Character. Method to use: "moment_matching" (default) or "optimization".
#' @param verbose Logical. If TRUE, print diagnostic information.
#'
#' @return A list containing:
#' \itemize{
#'   \item mean: The mean parameter (mu) of the normal distribution
#'   \item sd: The standard deviation parameter (sigma) of the normal distribution
#'   \item fitted_ci: The 95% CI of the fitted distribution
#'   \item mode: The mode of the fitted distribution (equals mean)
#' }
#'
#' @details
#' For a normal distribution:
#' \itemize{
#'   \item The mode equals the mean
#'   \item The 95% CI is approximately mean ± 1.96*sd
#'   \item The distribution is symmetric around the mean
#' }
#'
#' The moment matching method uses the fact that for a normal distribution,
#' the 95% CI width is approximately 3.92 standard deviations.
#'
#' @examples
#' # Example 1: Fit normal distribution
#' result <- fit_normal_from_ci(mode_val = 0,
#'                               ci_lower = -2,
#'                               ci_upper = 2)
#' print(result)
#'
#' # Example 2: Using optimization method
#' result <- fit_normal_from_ci(mode_val = 5,
#'                               ci_lower = 2,
#'                               ci_upper = 8,
#'                               method = "optimization")
#'
#' @export
fit_normal_from_ci <- function(mode_val, ci_lower, ci_upper,
                               method = "moment_matching",
                               verbose = FALSE) {

  # Validate inputs
  if (!is.numeric(mode_val) || !is.numeric(ci_lower) || !is.numeric(ci_upper)) {
    stop("All inputs must be numeric")
  }

  if (ci_lower >= ci_upper) {
    stop("ci_lower must be less than ci_upper")
  }

  if (mode_val < ci_lower || mode_val > ci_upper) {
    if (verbose) {
      warning("Mode is outside the confidence interval. For normal distribution, mode should be centered.")
    }
  }

  if (method == "moment_matching") {
    # Method 1: Moment matching approach
    # For normal distribution: mode = mean = median
    mean_val <- mode_val

    # For normal, 95% CI is approximately mean ± 1.96*sd
    # So CI width = 2 * 1.96 * sd = 3.92 * sd
    sd_val <- (ci_upper - ci_lower) / 3.92

    # Check if distribution is reasonably symmetric
    center_ci <- (ci_lower + ci_upper) / 2
    if (abs(mean_val - center_ci) > sd_val * 0.5) {
      if (verbose) {
        message("Warning: CI is not symmetric around mode. Adjusting mean.")
      }
      # Use center of CI as mean for better fit
      mean_val <- center_ci
    }

  } else if (method == "optimization") {
    # Method 2: Optimization to match mode and quantiles

    objective <- function(params) {
      mean_val <- params[1]
      sd_val <- params[2]

      if (sd_val <= 0) return(1e10)

      # Get fitted quantiles
      fitted_lower <- qnorm(0.025, mean = mean_val, sd = sd_val)
      fitted_upper <- qnorm(0.975, mean = mean_val, sd = sd_val)

      # Calculate error
      error_lower <- (fitted_lower - ci_lower)^2
      error_upper <- (fitted_upper - ci_upper)^2
      error_mode <- (mean_val - mode_val)^2  # Mode = mean for normal

      return(error_lower + error_upper + error_mode)
    }

    # Initial guess
    init_mean <- mode_val
    init_sd <- (ci_upper - ci_lower) / 3.92

    # Optimize
    result <- optim(c(init_mean, init_sd), objective, method = "L-BFGS-B",
                   lower = c(-Inf, 1e-6))

    mean_val <- result$par[1]
    sd_val <- result$par[2]

    if (verbose) {
      message(sprintf("Optimization converged: %s",
                     ifelse(result$convergence == 0, "Yes", "No")))
    }

  } else {
    stop("Method must be 'moment_matching' or 'optimization'")
  }

  # Calculate fitted CI
  fitted_ci <- c(
    qnorm(0.025, mean = mean_val, sd = sd_val),
    qnorm(0.975, mean = mean_val, sd = sd_val)
  )

  # Prepare output
  output <- list(
    mean = mean_val,
    sd = sd_val,
    fitted_ci = fitted_ci,
    mode = mean_val  # Mode equals mean for normal distribution
  )

  if (verbose) {
    message("Fitted Normal Distribution:")
    message(sprintf("  Mean (mu): %.6f", mean_val))
    message(sprintf("  SD (sigma): %.6f", sd_val))
    message(sprintf("  Fitted 95%% CI: [%.6f, %.6f]", fitted_ci[1], fitted_ci[2]))
    message(sprintf("  Target 95%% CI: [%.6f, %.6f]", ci_lower, ci_upper))
  }

  return(output)
}