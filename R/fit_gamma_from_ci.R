#' Fit Gamma Distribution from Mode and 95% Confidence Intervals
#'
#' This function calculates the shape and rate parameters of a gamma distribution
#' that best matches a given mode and 95% confidence intervals.
#'
#' @param mode_val Numeric. The mode of the distribution.
#' @param ci_lower Numeric. The lower bound of the 95% confidence interval.
#' @param ci_upper Numeric. The upper bound of the 95% confidence interval.
#' @param method Character. Method to use: "moment_matching" (default) or "optimization".
#' @param verbose Logical. If TRUE, print diagnostic information.
#'
#' @return A list containing:
#' \itemize{
#'   \item shape: The shape parameter (alpha) of the gamma distribution
#'   \item rate: The rate parameter (beta) of the gamma distribution
#'   \item scale: The scale parameter (1/rate) of the gamma distribution
#'   \item fitted_mean: The mean of the fitted distribution
#'   \item fitted_ci: The 95% CI of the fitted distribution
#'   \item mode: The mode of the fitted distribution (if shape > 1)
#' }
#'
#' @examples
#' # Example 1: Fit gamma for omega_1
#' result <- fit_gamma_from_ci(mode_val = 0.000705, 
#'                              ci_lower = 0.000471, 
#'                              ci_upper = 0.001107)
#' print(result)
#'
#' # Example 2: Using optimization method
#' result <- fit_gamma_from_ci(mode_val = 0.000337, 
#'                              ci_lower = 0.0000984, 
#'                              ci_upper = 0.000992,
#'                              method = "optimization")
#'
#' @export
fit_gamma_from_ci <- function(mode_val, ci_lower, ci_upper, 
                              method = "moment_matching", 
                              verbose = FALSE) {
  
  # Validate inputs
  if (mode_val <= 0) {
    stop("Mode must be positive")
  }
  
  if (ci_lower <= 0) {
    stop("For gamma distribution, all values must be positive")
  }
  
  if (ci_lower >= ci_upper) {
    stop("ci_lower must be less than ci_upper")
  }
  
  if (method == "moment_matching") {
    # Method 1: Moment matching approach
    # For gamma: mode = (shape - 1) / rate when shape > 1
    # We need to estimate shape from the CI width
    
    # Estimate coefficient of variation from CI
    # Approximate mean from mode and CI
    approx_mean <- (ci_lower + ci_upper) / 2
    
    # If mode is closer to lower bound, distribution is right-skewed
    # Adjust mean estimate accordingly
    skewness_factor <- (mode_val - ci_lower) / (ci_upper - ci_lower)
    if (skewness_factor < 0.5) {
      # Right-skewed, mean > mode
      approx_mean <- mode_val * 1.2
    }
    
    # Estimate CV from CI width
    cv <- (ci_upper - ci_lower) / (4 * approx_mean)
    
    # Calculate shape from CV
    # CV = 1/sqrt(shape) for gamma
    shape <- 1 / cv^2
    
    # Ensure shape > 1 for mode to exist
    if (shape <= 1) {
      shape <- 1.5  # Minimum shape for mode to exist
    }
    
    # Calculate rate from mode
    # mode = (shape - 1) / rate
    rate <- (shape - 1) / mode_val
    
  } else if (method == "optimization") {
    # Method 2: Optimization to match mode and quantiles
    
    objective <- function(params) {
      if (params[1] <= 1 || params[2] <= 0) return(1e10)  # shape must be > 1 for mode
      
      shape <- params[1]
      rate <- params[2]
      
      # Check if mode matches
      fitted_mode <- (shape - 1) / rate
      
      # Get fitted quantiles
      fitted_lower <- qgamma(0.025, shape = shape, rate = rate)
      fitted_upper <- qgamma(0.975, shape = shape, rate = rate)
      
      # Calculate error (heavily weight mode matching)
      error <- 1000 * (fitted_mode - mode_val)^2 + 
               (fitted_lower - ci_lower)^2 + 
               (fitted_upper - ci_upper)^2
      
      return(error)
    }
    
    # Initial guess
    # Estimate mean from mode and CI
    approx_mean <- mode_val * 1.2  # Rough estimate: mean > mode for gamma
    cv_init <- (ci_upper - ci_lower) / (4 * approx_mean)
    shape_init <- max(2, 1 / cv_init^2)  # Ensure shape > 1
    rate_init <- (shape_init - 1) / mode_val
    
    # Optimize
    result <- optim(c(shape_init, rate_init), 
                   objective, 
                   method = "L-BFGS-B",
                   lower = c(1.01, 0.001),  # shape must be > 1
                   upper = c(1000, 1000000))
    
    shape <- result$par[1]
    rate <- result$par[2]
    
  } else {
    stop("Method must be 'moment_matching' or 'optimization'")
  }
  
  # Calculate fitted statistics
  fitted_mean <- shape / rate
  fitted_var <- shape / rate^2
  fitted_sd <- sqrt(fitted_var)
  fitted_lower <- qgamma(0.025, shape = shape, rate = rate)
  fitted_upper <- qgamma(0.975, shape = shape, rate = rate)
  
  # Mode (only exists if shape > 1)
  fitted_mode <- if (shape > 1) (shape - 1) / rate else NA
  
  # Prepare output
  output <- list(
    shape = shape,
    rate = rate,
    scale = 1/rate,
    fitted_mode = fitted_mode,
    fitted_mean = fitted_mean,
    fitted_sd = fitted_sd,
    fitted_ci = c(lower = fitted_lower, upper = fitted_upper),
    input_mode = mode_val,
    input_ci = c(lower = ci_lower, upper = ci_upper)
  )
  
  if (verbose) {
    cat("\n=== Gamma Distribution Fitting ===\n")
    cat(sprintf("Input: mode = %.6f, 95%% CI = [%.6f, %.6f]\n", 
                mode_val, ci_lower, ci_upper))
    cat(sprintf("Method: %s\n", method))
    cat("\nFitted parameters:\n")
    cat(sprintf("  Shape (alpha): %.4f\n", shape))
    cat(sprintf("  Rate (beta): %.4f\n", rate))
    cat(sprintf("  Scale (1/beta): %.6f\n", 1/rate))
    cat("\nFitted statistics:\n")
    cat(sprintf("  Mode: %.6f (target: %.6f, diff: %.2f%%)\n", 
                fitted_mode, mode_val, 100*(fitted_mode - mode_val)/mode_val))
    cat(sprintf("  Mean: %.6f\n", fitted_mean))
    cat(sprintf("  SD: %.6f\n", fitted_sd))
    cat(sprintf("  95%% CI: [%.6f, %.6f]\n", fitted_lower, fitted_upper))
    cat(sprintf("  Target CI: [%.6f, %.6f]\n", ci_lower, ci_upper))
    cat("\n")
  }
  
  return(output)
}