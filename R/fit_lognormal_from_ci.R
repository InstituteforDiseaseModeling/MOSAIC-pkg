#' Fit Lognormal Distribution from Mode and 95% Confidence Intervals
#'
#' This function calculates the meanlog and sdlog parameters of a lognormal
#' distribution that best matches a given mode and 95% confidence intervals.
#'
#' @param mode_val Numeric. The mode of the distribution.
#' @param ci_lower Numeric. The lower bound of the 95% confidence interval.
#' @param ci_upper Numeric. The upper bound of the 95% confidence interval.
#' @param method Character. Method to use: "moment_matching" (default) or "optimization".
#' @param verbose Logical. If TRUE, print diagnostic information.
#'
#' @return A list containing:
#' \itemize{
#'   \item meanlog: The mean of the logarithm (mu) of the lognormal distribution
#'   \item sdlog: The standard deviation of the logarithm (sigma) of the lognormal distribution
#'   \item mean: The mean of the distribution (not meanlog)
#'   \item sd: The standard deviation of the distribution (not sdlog)
#'   \item fitted_ci: The 95% CI of the fitted distribution
#'   \item mode: The mode of the fitted distribution
#' }
#'
#' @details
#' For a lognormal distribution with parameters meanlog (μ) and sdlog (σ):
#' \itemize{
#'   \item Mode = exp(μ - σ²)
#'   \item Mean = exp(μ + σ²/2)
#'   \item Variance = (exp(σ²) - 1) * exp(2μ + σ²)
#' }
#'
#' The moment matching method estimates parameters from the mode and CI width
#' on the log scale, then adjusts for the lognormal relationship.
#'
#' @examples
#' # Example 1: Fit lognormal distribution
#' result <- fit_lognormal_from_ci(mode_val = 1,
#'                                  ci_lower = 0.5,
#'                                  ci_upper = 3)
#' print(result)
#'
#' # Example 2: Using optimization method
#' result <- fit_lognormal_from_ci(mode_val = 10,
#'                                  ci_lower = 2,
#'                                  ci_upper = 50,
#'                                  method = "optimization")
#'
#' @export
fit_lognormal_from_ci <- function(mode_val, ci_lower, ci_upper,
                                  method = "moment_matching",
                                  verbose = FALSE) {

  # Validate inputs
  if (!is.numeric(mode_val) || !is.numeric(ci_lower) || !is.numeric(ci_upper)) {
    stop("All inputs must be numeric")
  }

  if (mode_val <= 0 || ci_lower <= 0 || ci_upper <= 0) {
    stop("For lognormal distribution, all values must be positive")
  }

  if (ci_lower >= ci_upper) {
    stop("ci_lower must be less than ci_upper")
  }

  if (method == "moment_matching") {
    # Method 1: Moment matching approach
    # For lognormal: mode = exp(meanlog - sdlog^2)
    # Therefore: meanlog = log(mode) + sdlog^2

    # Work on log scale
    log_mode <- log(mode_val)
    log_ci_lower <- log(ci_lower)
    log_ci_upper <- log(ci_upper)

    # Estimate sdlog from CI width on log scale
    # On log scale, 95% CI is approximately meanlog ± 1.96*sdlog
    sdlog_est <- (log_ci_upper - log_ci_lower) / 3.92

    # Calculate meanlog from mode relationship
    # mode = exp(meanlog - sdlog^2)
    # log(mode) = meanlog - sdlog^2
    # meanlog = log(mode) + sdlog^2
    meanlog_est <- log_mode + sdlog_est^2

    # Refine estimate by checking fitted quantiles
    fitted_lower <- qlnorm(0.025, meanlog = meanlog_est, sdlog = sdlog_est)
    fitted_upper <- qlnorm(0.975, meanlog = meanlog_est, sdlog = sdlog_est)

    # Adjust if needed
    if (fitted_lower > ci_lower * 1.1 || fitted_upper < ci_upper * 0.9) {
      # CI too narrow, increase sdlog
      sdlog_est <- sdlog_est * 1.1
      meanlog_est <- log_mode + sdlog_est^2
    } else if (fitted_lower < ci_lower * 0.9 || fitted_upper > ci_upper * 1.1) {
      # CI too wide, decrease sdlog
      sdlog_est <- sdlog_est * 0.9
      meanlog_est <- log_mode + sdlog_est^2
    }

    meanlog_val <- meanlog_est
    sdlog_val <- sdlog_est

  } else if (method == "optimization") {
    # Method 2: Optimization to match mode and quantiles

    objective <- function(params) {
      meanlog <- params[1]
      sdlog <- params[2]

      if (sdlog <= 0) return(1e10)

      # Check if mode matches
      fitted_mode <- exp(meanlog - sdlog^2)

      # Get fitted quantiles
      fitted_lower <- qlnorm(0.025, meanlog = meanlog, sdlog = sdlog)
      fitted_upper <- qlnorm(0.975, meanlog = meanlog, sdlog = sdlog)

      # Calculate error (use relative error for lognormal)
      error_mode <- ((fitted_mode - mode_val) / mode_val)^2
      error_lower <- ((fitted_lower - ci_lower) / ci_lower)^2
      error_upper <- ((fitted_upper - ci_upper) / ci_upper)^2

      return(10 * error_mode + error_lower + error_upper)
    }

    # Initial guess
    init_sdlog <- (log(ci_upper) - log(ci_lower)) / 3.92
    init_meanlog <- log(mode_val) + init_sdlog^2

    # Optimize
    result <- optim(c(init_meanlog, init_sdlog), objective, method = "L-BFGS-B",
                   lower = c(-Inf, 1e-6))

    meanlog_val <- result$par[1]
    sdlog_val <- result$par[2]

    if (verbose) {
      message(sprintf("Optimization converged: %s",
                     ifelse(result$convergence == 0, "Yes", "No")))
    }

  } else {
    stop("Method must be 'moment_matching' or 'optimization'")
  }

  # Calculate fitted values
  fitted_mode <- exp(meanlog_val - sdlog_val^2)
  fitted_mean <- exp(meanlog_val + sdlog_val^2/2)
  fitted_var <- (exp(sdlog_val^2) - 1) * exp(2*meanlog_val + sdlog_val^2)
  fitted_sd <- sqrt(fitted_var)
  fitted_ci <- c(
    qlnorm(0.025, meanlog = meanlog_val, sdlog = sdlog_val),
    qlnorm(0.975, meanlog = meanlog_val, sdlog = sdlog_val)
  )

  # Prepare output
  output <- list(
    meanlog = meanlog_val,
    sdlog = sdlog_val,
    mean = fitted_mean,
    sd = fitted_sd,
    fitted_ci = fitted_ci,
    mode = fitted_mode
  )

  if (verbose) {
    message("Fitted Lognormal Distribution:")
    message(sprintf("  Meanlog (mu): %.6f", meanlog_val))
    message(sprintf("  SDlog (sigma): %.6f", sdlog_val))
    message(sprintf("  Mean: %.6f", fitted_mean))
    message(sprintf("  Mode: %.6f", fitted_mode))
    message(sprintf("  Fitted 95%% CI: [%.6f, %.6f]", fitted_ci[1], fitted_ci[2]))
    message(sprintf("  Target 95%% CI: [%.6f, %.6f]", ci_lower, ci_upper))
  }

  return(output)
}