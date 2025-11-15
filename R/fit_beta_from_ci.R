#' Fit Beta Distribution from Mode and 95% Confidence Intervals
#'
#' This function calculates the shape parameters (alpha and beta) of a beta distribution
#' that best matches a given mode and 95% confidence intervals.
#'
#' @param mode_val Numeric. The mode of the distribution (must be in (0,1)).
#' @param ci_lower Numeric. The lower bound of the 95% confidence interval (must be in (0,1)).
#' @param ci_upper Numeric. The upper bound of the 95% confidence interval (must be in (0,1)).
#' @param method Character. Method to use: "moment_matching" (default) or "optimization".
#' @param verbose Logical. If TRUE, print diagnostic information.
#'
#' @return A list containing:
#' \itemize{
#'   \item shape1: The alpha shape parameter of the beta distribution
#'   \item shape2: The beta shape parameter of the beta distribution
#'   \item fitted_mode: The mode of the fitted distribution
#'   \item fitted_mean: The mean of the fitted distribution
#'   \item fitted_var: The variance of the fitted distribution
#'   \item fitted_ci: The 95% CI of the fitted distribution
#'   \item input_mode: The input mode value
#'   \item input_ci: The input confidence interval
#' }
#'
#' @examples
#' # Example 1: Fit beta for phi_1 (vaccine effectiveness)
#' result <- fit_beta_from_ci(mode_val = 0.788, 
#'                             ci_lower = 0.753, 
#'                             ci_upper = 0.822)
#' print(result)
#'
#' # Example 2: Using optimization method
#' result <- fit_beta_from_ci(mode_val = 0.65, 
#'                             ci_lower = 0.50, 
#'                             ci_upper = 0.78,
#'                             method = "optimization")
#'
#' @export
fit_beta_from_ci <- function(mode_val, ci_lower, ci_upper, 
                             method = "moment_matching", 
                             verbose = FALSE) {
  
  # Validate inputs
  if (mode_val <= 0 || mode_val >= 1) {
    stop("Mode must be in (0, 1) for beta distribution")
  }
  
  if (ci_lower <= 0 || ci_lower >= 1) {
    stop("ci_lower must be in (0, 1) for beta distribution")
  }
  
  if (ci_upper <= 0 || ci_upper >= 1) {
    stop("ci_upper must be in (0, 1) for beta distribution")
  }
  
  if (ci_lower >= ci_upper) {
    stop("ci_lower must be less than ci_upper")
  }
  
  if (mode_val <= ci_lower || mode_val >= ci_upper) {
    if (verbose) {
      warning("Mode is outside the confidence interval - this may lead to poor fits")
    }
  }
  
  if (method == "moment_matching") {
    # Method 1: Moment matching approach
    # For beta: mode = (alpha - 1) / (alpha + beta - 2) when alpha, beta > 1
    
    # Estimate mean and variance from CI
    # For beta on [0,1], the distribution is often roughly symmetric
    # or can be transformed to be more symmetric
    
    # Estimate mean (could be different from mode for skewed distributions)
    # Use weighted average based on mode position
    mode_position <- (mode_val - ci_lower) / (ci_upper - ci_lower)
    
    if (mode_position < 0.5) {
      # Left-skewed, mean > mode
      approx_mean <- mode_val + 0.1 * (ci_upper - mode_val)
    } else if (mode_position > 0.5) {
      # Right-skewed, mean < mode
      approx_mean <- mode_val - 0.1 * (mode_val - ci_lower)
    } else {
      # Symmetric
      approx_mean <- mode_val
    }
    
    # Ensure mean is within bounds
    approx_mean <- max(ci_lower + 0.01, min(ci_upper - 0.01, approx_mean))
    
    # Estimate variance from CI width
    # For beta, approximately 95% of mass is within mean ± 2*sd
    approx_sd <- (ci_upper - ci_lower) / 4
    approx_var <- approx_sd^2
    
    # Method of moments for beta distribution
    # mean = alpha / (alpha + beta)
    # var = alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1))
    
    # Solve for alpha and beta
    # Let S = alpha + beta
    # From mean: alpha = mean * S
    # From variance: var = mean * (1 - mean) / (S + 1)
    # Therefore: S = mean * (1 - mean) / var - 1
    
    S <- approx_mean * (1 - approx_mean) / approx_var - 1
    S <- max(2.1, S)  # Ensure S > 2 for mode to exist
    
    alpha <- approx_mean * S
    beta <- (1 - approx_mean) * S
    
    # Ensure both parameters > 1 for mode to exist
    alpha <- max(1.01, alpha)
    beta <- max(1.01, beta)
    
    # Adjust to match mode
    # mode = (alpha - 1) / (alpha + beta - 2)
    # Rearranging: alpha = mode * (alpha + beta - 2) + 1
    # Let ratio = alpha / beta, then we can solve
    
    if (alpha > 1 && beta > 1) {
      # Current mode
      current_mode <- (alpha - 1) / (alpha + beta - 2)
      
      # Scale factor to adjust mode
      scale_factor <- (mode_val * (alpha + beta - 2) + 1) / alpha
      alpha <- alpha * scale_factor
      # Keep sum approximately constant
      beta <- S - alpha
      
      # Ensure constraints
      alpha <- max(1.01, alpha)
      beta <- max(1.01, beta)
    }
    
  } else if (method == "optimization") {
    # Method 2: Optimization to match mode and quantiles
    
    objective <- function(params) {
      if (params[1] <= 1 || params[2] <= 1) return(1e10)  # Both must be > 1 for mode
      
      alpha <- params[1]
      beta <- params[2]
      
      # Check if mode matches
      fitted_mode <- (alpha - 1) / (alpha + beta - 2)
      
      # Get fitted quantiles
      fitted_lower <- qbeta(0.025, shape1 = alpha, shape2 = beta)
      fitted_upper <- qbeta(0.975, shape1 = alpha, shape2 = beta)
      
      # Calculate error (heavily weight mode matching)
      error <- 1000 * (fitted_mode - mode_val)^2 + 
               10 * (fitted_lower - ci_lower)^2 + 
               10 * (fitted_upper - ci_upper)^2
      
      return(error)
    }
    
    # Initial guess based on moment matching
    approx_mean <- (ci_lower + ci_upper) / 2
    approx_var <- ((ci_upper - ci_lower) / 4)^2
    
    S_init <- approx_mean * (1 - approx_mean) / approx_var - 1
    S_init <- max(3, S_init)
    
    alpha_init <- approx_mean * S_init
    beta_init <- (1 - approx_mean) * S_init
    
    # Ensure initial values satisfy constraints
    alpha_init <- max(1.1, alpha_init)
    beta_init <- max(1.1, beta_init)
    
    # Optimize
    result <- optim(c(alpha_init, beta_init), 
                   objective, 
                   method = "L-BFGS-B",
                   lower = c(1.01, 1.01),  # Both must be > 1
                   upper = c(1000, 1000))
    
    alpha <- result$par[1]
    beta <- result$par[2]
    
  } else {
    stop("Method must be 'moment_matching' or 'optimization'")
  }
  
  # Calculate fitted statistics
  fitted_mean <- alpha / (alpha + beta)
  fitted_var <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
  fitted_sd <- sqrt(fitted_var)
  fitted_lower <- qbeta(0.025, shape1 = alpha, shape2 = beta)
  fitted_upper <- qbeta(0.975, shape1 = alpha, shape2 = beta)
  
  # Mode (only exists if both shape parameters > 1)
  fitted_mode <- if (alpha > 1 && beta > 1) {
    (alpha - 1) / (alpha + beta - 2)
  } else {
    NA
  }
  
  # Prepare output
  output <- list(
    shape1 = alpha,
    shape2 = beta,
    fitted_mode = fitted_mode,
    fitted_mean = fitted_mean,
    fitted_var = fitted_var,
    fitted_sd = fitted_sd,
    fitted_ci = c(lower = fitted_lower, upper = fitted_upper),
    input_mode = mode_val,
    input_ci = c(lower = ci_lower, upper = ci_upper)
  )
  
  if (verbose) {
    cat("\n=== Beta Distribution Fitting ===\n")
    cat(sprintf("Input: mode = %.4f, 95%% CI = [%.4f, %.4f]\n", 
                mode_val, ci_lower, ci_upper))
    cat(sprintf("Method: %s\n", method))
    cat("\nFitted parameters:\n")
    cat(sprintf("  Alpha (shape1): %.4f\n", alpha))
    cat(sprintf("  Beta (shape2): %.4f\n", beta))
    cat("\nFitted statistics:\n")
    if (!is.na(fitted_mode)) {
      cat(sprintf("  Mode: %.4f (target: %.4f, diff: %.2f%%)\n", 
                  fitted_mode, mode_val, 100*(fitted_mode - mode_val)/mode_val))
    }
    cat(sprintf("  Mean: %.4f\n", fitted_mean))
    cat(sprintf("  SD: %.4f\n", fitted_sd))
    cat(sprintf("  95%% CI: [%.4f, %.4f]\n", fitted_lower, fitted_upper))
    cat(sprintf("  Target CI: [%.4f, %.4f]\n", ci_lower, ci_upper))
    cat("\n")
  }
  
  return(output)
}