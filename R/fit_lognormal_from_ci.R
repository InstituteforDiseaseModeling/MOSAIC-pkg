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
#' The moment matching method (default) matches the 95% CI exactly on the
#' log scale: a lognormal is fully determined by two quantiles, so
#' \code{meanlog} is the midpoint of \code{log(ci_lower)} and
#' \code{log(ci_upper)} and \code{sdlog} is their distance divided by
#' \code{2 * qnorm(0.975)}. \code{mode_val} is validated but does not move
#' the fit; the implied mode \code{exp(meanlog - sdlog^2)} is returned as
#' \code{mode}. Anchoring on a sample-based mode (for example a KDE mode of
#' posterior draws) and then adding \code{sdlog^2} shifted wide CIs upward by
#' orders of magnitude, which is why the CI is authoritative here.
#'
#' The optimization method fits \code{meanlog} and \code{sdlog} jointly to the
#' mode and both quantiles, with all errors measured on the log scale and the
#' mode weighted 10x; use it when \code{mode_val} is a trusted anchor.
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
    # Two quantiles determine a lognormal exactly (log X ~ Normal).
    z <- stats::qnorm(0.975)
    meanlog_val <- (log(ci_lower) + log(ci_upper)) / 2
    sdlog_val <- (log(ci_upper) - log(ci_lower)) / (2 * z)

  } else if (method == "optimization") {
    # Joint fit to mode and quantiles, all on the log scale (relative errors).
    log_targets <- log(c(ci_lower, ci_upper))
    log_mode <- log(mode_val)

    objective <- function(params) {
      meanlog <- params[1]
      sdlog <- exp(params[2])
      q <- stats::qnorm(c(0.025, 0.975), mean = meanlog, sd = sdlog)
      10 * ((meanlog - sdlog^2) - log_mode)^2 + sum((q - log_targets)^2)
    }

    init_sdlog <- (log(ci_upper) - log(ci_lower)) / (2 * stats::qnorm(0.975))
    init_meanlog <- (log(ci_lower) + log(ci_upper)) / 2
    result <- stats::optim(c(init_meanlog, log(init_sdlog)), objective,
                           method = "Nelder-Mead",
                           control = list(maxit = 2000, reltol = 1e-12))

    meanlog_val <- result$par[1]
    sdlog_val <- exp(result$par[2])

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