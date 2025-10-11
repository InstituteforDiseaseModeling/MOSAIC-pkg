#' Fit Truncated Normal Distribution from Mode and 95% Confidence Intervals
#'
#' This function calculates the mean, sd, and truncation bounds of a truncated normal
#' distribution that best matches a given mode and 95% confidence intervals.
#'
#' @param mode_val Numeric. The mode of the distribution.
#' @param ci_lower Numeric. The lower bound of the 95% confidence interval.
#' @param ci_upper Numeric. The upper bound of the 95% confidence interval.
#' @param a Numeric. Lower truncation bound (default: -Inf).
#' @param b Numeric. Upper truncation bound (default: Inf).
#' @param method Character. Method to use: "moment_matching" (default) or "optimization".
#' @param verbose Logical. If TRUE, print diagnostic information.
#'
#' @return A list containing:
#' \itemize{
#'   \item mean: The mean parameter (mu) of the underlying normal distribution
#'   \item sd: The standard deviation parameter (sigma) of the underlying normal distribution
#'   \item a: The lower truncation bound
#'   \item b: The upper truncation bound
#'   \item fitted_mean: The mean of the truncated distribution
#'   \item fitted_ci: The 95% CI of the fitted truncated distribution
#'   \item mode: The mode of the fitted truncated distribution
#' }
#'
#' @details
#' For a truncated normal distribution:
#' \itemize{
#'   \item The mode may differ from the mean due to truncation
#'   \item If truncation is symmetric and far from the mean, mode ≈ mean
#'   \item Asymmetric truncation shifts the mode away from the mean
#' }
#'
#' If truncation bounds are not specified, they are estimated from the CI,
#' extending slightly beyond to allow for a proper truncated distribution.
#'
#' @examples
#' # Example 1: Fit truncated normal with specified bounds
#' result <- fit_truncnorm_from_ci(mode_val = 0.5,
#'                                  ci_lower = 0.2,
#'                                  ci_upper = 0.8,
#'                                  a = 0, b = 1)
#' print(result)
#'
#' # Example 2: Auto-detect truncation bounds
#' result <- fit_truncnorm_from_ci(mode_val = 5,
#'                                  ci_lower = 2,
#'                                  ci_upper = 8)
#'
#' @importFrom truncnorm qtruncnorm dtruncnorm
#' @export
fit_truncnorm_from_ci <- function(mode_val, ci_lower, ci_upper,
                                  a = NULL, b = NULL,
                                  method = "moment_matching",
                                  verbose = FALSE) {

  # Load required package
  if (!requireNamespace("truncnorm", quietly = TRUE)) {
    stop("Package 'truncnorm' is required. Install it with: install.packages('truncnorm')")
  }

  # Validate inputs
  if (!is.numeric(mode_val) || !is.numeric(ci_lower) || !is.numeric(ci_upper)) {
    stop("All inputs must be numeric")
  }

  if (ci_lower >= ci_upper) {
    stop("ci_lower must be less than ci_upper")
  }

  # Auto-detect truncation bounds if not provided
  if (is.null(a) && is.null(b)) {
    # Estimate bounds from CI, extending by 20% on each side
    ci_width <- ci_upper - ci_lower
    a <- ci_lower - 0.2 * ci_width
    b <- ci_upper + 0.2 * ci_width

    if (verbose) {
      message(sprintf("Auto-detected truncation bounds: [%.6f, %.6f]", a, b))
    }
  } else {
    # Use provided bounds or set to infinity if only one is provided
    if (is.null(a)) a <- -Inf
    if (is.null(b)) b <- Inf
  }

  # Ensure mode is within truncation bounds
  if (!is.infinite(a) && mode_val <= a) {
    stop("Mode must be greater than lower truncation bound")
  }
  if (!is.infinite(b) && mode_val >= b) {
    stop("Mode must be less than upper truncation bound")
  }

  if (method == "moment_matching") {
    # Method 1: Moment matching approach
    # Start with assumption that truncated mean ≈ mode for moderate truncation
    mean_est <- mode_val

    # Estimate sd from CI width, accounting for truncation
    # For truncated normal, effective CI is narrower than full normal
    if (!is.infinite(a) || !is.infinite(b)) {
      # With truncation, CI is compressed
      truncation_factor <- 1.2  # Adjustment factor for truncation
      sd_est <- (ci_upper - ci_lower) / (3.92 * truncation_factor)
    } else {
      # No truncation, standard normal
      sd_est <- (ci_upper - ci_lower) / 3.92
    }

    # Refine estimate if bounds are close
    if (!is.infinite(a) && !is.infinite(b)) {
      bound_width <- b - a
      if (bound_width < 4 * sd_est) {
        # Significant truncation, adjust parameters
        mean_est <- (a + b) / 2 + (mode_val - (a + b) / 2) * 0.8
        sd_est <- bound_width / 6  # Rough approximation
      }
    }

    mean_val <- mean_est
    sd_val <- sd_est

  } else if (method == "optimization") {
    # Method 2: Optimization to match mode and quantiles

    # Helper function to find mode of truncated normal
    find_truncnorm_mode <- function(mean, sd, a, b, n_points = 1000) {
      if (!is.finite(a)) a <- mean - 5 * sd
      if (!is.finite(b)) b <- mean + 5 * sd

      x_seq <- seq(max(a, mean - 4*sd), min(b, mean + 4*sd), length.out = n_points)
      densities <- truncnorm::dtruncnorm(x_seq, a = a, b = b, mean = mean, sd = sd)
      x_seq[which.max(densities)]
    }

    objective <- function(params) {
      mean <- params[1]
      sd <- params[2]

      if (sd <= 0) return(1e10)

      # Check if parameters make sense with bounds
      if (!is.infinite(a) && mean - 4*sd > a) return(1e10)
      if (!is.infinite(b) && mean + 4*sd < b) return(1e10)

      tryCatch({
        # Find mode of truncated distribution
        fitted_mode <- find_truncnorm_mode(mean, sd, a, b)

        # Get fitted quantiles
        fitted_lower <- truncnorm::qtruncnorm(0.025, a = a, b = b, mean = mean, sd = sd)
        fitted_upper <- truncnorm::qtruncnorm(0.975, a = a, b = b, mean = mean, sd = sd)

        # Calculate error
        error_mode <- ((fitted_mode - mode_val) / (b - a))^2 * 10
        error_lower <- ((fitted_lower - ci_lower) / (b - a))^2
        error_upper <- ((fitted_upper - ci_upper) / (b - a))^2

        return(error_mode + error_lower + error_upper)
      }, error = function(e) {
        return(1e10)
      })
    }

    # Initial guess
    init_mean <- mode_val
    init_sd <- (ci_upper - ci_lower) / 4

    # Set bounds for optimization
    opt_lower <- c(ifelse(is.infinite(a), -Inf, a), 1e-6)
    opt_upper <- c(ifelse(is.infinite(b), Inf, b), Inf)

    # Optimize
    result <- optim(c(init_mean, init_sd), objective, method = "L-BFGS-B",
                   lower = opt_lower, upper = opt_upper)

    mean_val <- result$par[1]
    sd_val <- result$par[2]

    if (verbose) {
      message(sprintf("Optimization converged: %s",
                     ifelse(result$convergence == 0, "Yes", "No")))
    }

  } else {
    stop("Method must be 'moment_matching' or 'optimization'")
  }

  # Calculate fitted values
  fitted_ci <- c(
    truncnorm::qtruncnorm(0.025, a = a, b = b, mean = mean_val, sd = sd_val),
    truncnorm::qtruncnorm(0.975, a = a, b = b, mean = mean_val, sd = sd_val)
  )

  # Calculate actual mean of truncated distribution
  # For truncated normal, the mean shifts from the underlying normal mean
  if (!requireNamespace("truncnorm", quietly = TRUE)) {
    fitted_mean <- mean_val  # Approximation
    fitted_mode <- mode_val  # Use input
  } else {
    # Generate samples to estimate mean
    samples <- truncnorm::rtruncnorm(10000, a = a, b = b, mean = mean_val, sd = sd_val)
    fitted_mean <- mean(samples)

    # Find mode numerically
    x_seq <- seq(max(a, mean_val - 3*sd_val), min(b, mean_val + 3*sd_val), length.out = 1000)
    densities <- truncnorm::dtruncnorm(x_seq, a = a, b = b, mean = mean_val, sd = sd_val)
    fitted_mode <- x_seq[which.max(densities)]
  }

  # Prepare output
  output <- list(
    mean = mean_val,
    sd = sd_val,
    a = a,
    b = b,
    fitted_mean = fitted_mean,
    fitted_ci = fitted_ci,
    mode = fitted_mode
  )

  if (verbose) {
    message("Fitted Truncated Normal Distribution:")
    message(sprintf("  Mean (mu): %.6f", mean_val))
    message(sprintf("  SD (sigma): %.6f", sd_val))
    message(sprintf("  Truncation: [%.6f, %.6f]", a, b))
    message(sprintf("  Fitted mode: %.6f", fitted_mode))
    message(sprintf("  Fitted mean: %.6f", fitted_mean))
    message(sprintf("  Fitted 95%% CI: [%.6f, %.6f]", fitted_ci[1], fitted_ci[2]))
    message(sprintf("  Target 95%% CI: [%.6f, %.6f]", ci_lower, ci_upper))
  }

  return(output)
}