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
    warning("Mode is outside the confidence interval - this may lead to poor fits")
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

#' Test the beta fitting function with vaccine effectiveness parameters
#'
#' @examples
#' \dontrun{
#' # Test with phi_1 values from the vaccine effectiveness analysis
#' phi_1_fit <- fit_beta_from_ci(
#'   mode_val = 0.788,
#'   ci_lower = 0.753, 
#'   ci_upper = 0.822,
#'   method = "optimization",
#'   verbose = TRUE
#' )
#' 
#' # Test with phi_2 values
#' phi_2_fit <- fit_beta_from_ci(
#'   mode_val = 0.769,
#'   ci_lower = 0.739,
#'   ci_upper = 0.798,
#'   method = "optimization",
#'   verbose = TRUE
#' )
#' 
#' # Plot the fitted distributions
#' library(ggplot2)
#' 
#' x_vals <- seq(0, 1, length.out = 1000)
#' df <- data.frame(
#'   x = rep(x_vals, 2),
#'   density = c(
#'     dbeta(x_vals, shape1 = phi_1_fit$shape1, shape2 = phi_1_fit$shape2),
#'     dbeta(x_vals, shape1 = phi_2_fit$shape1, shape2 = phi_2_fit$shape2)
#'   ),
#'   parameter = rep(c("phi_1", "phi_2"), each = length(x_vals))
#' )
#' 
#' ggplot(df, aes(x = x, y = density, color = parameter)) +
#'   geom_line(size = 1.5) +
#'   theme_minimal() +
#'   labs(title = "Fitted Beta Distributions for Initial Vaccine Effectiveness",
#'        x = "Initial effectiveness (phi)",
#'        y = "Probability density")
#' }
test_beta_fitting <- function() {
  cat("Testing beta distribution fitting for vaccine effectiveness:\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  # Test phi_1 (one-dose initial effectiveness)
  cat("\nPhi_1 (one-dose initial effectiveness):\n")
  phi_1_mm <- fit_beta_from_ci(mode_val = 0.788, 
                                ci_lower = 0.753, 
                                ci_upper = 0.822, 
                                method = "moment_matching", verbose = TRUE)
  phi_1_opt <- fit_beta_from_ci(mode_val = 0.788, 
                                 ci_lower = 0.753, 
                                 ci_upper = 0.822, 
                                 method = "optimization", verbose = TRUE)
  
  # Test phi_2 (two-dose initial effectiveness)
  cat("\nPhi_2 (two-dose initial effectiveness):\n")
  phi_2_mm <- fit_beta_from_ci(mode_val = 0.769, 
                                ci_lower = 0.739, 
                                ci_upper = 0.798, 
                                method = "moment_matching", verbose = TRUE)
  phi_2_opt <- fit_beta_from_ci(mode_val = 0.769, 
                                 ci_lower = 0.739, 
                                 ci_upper = 0.798, 
                                 method = "optimization", verbose = TRUE)
  
  # Compare methods
  cat("\n=== Method Comparison ===\n")
  cat("Phi_1:\n")
  cat(sprintf("  Moment matching: alpha = %.2f, beta = %.2f\n", 
              phi_1_mm$shape1, phi_1_mm$shape2))
  cat(sprintf("  Optimization:    alpha = %.2f, beta = %.2f\n", 
              phi_1_opt$shape1, phi_1_opt$shape2))
  
  cat("\nPhi_2:\n")
  cat(sprintf("  Moment matching: alpha = %.2f, beta = %.2f\n", 
              phi_2_mm$shape1, phi_2_mm$shape2))
  cat(sprintf("  Optimization:    alpha = %.2f, beta = %.2f\n", 
              phi_2_opt$shape1, phi_2_opt$shape2))
  
  invisible(list(phi_1_mm = phi_1_mm, phi_1_opt = phi_1_opt,
                 phi_2_mm = phi_2_mm, phi_2_opt = phi_2_opt))
}

#' Compare beta and gamma fitting approaches
#'
#' @examples
#' \dontrun{
#' # For parameters bounded in [0,1], compare beta distribution
#' phi_beta <- fit_beta_from_ci(mode_val = 0.788, 
#'                               ci_lower = 0.753, 
#'                               ci_upper = 0.822,
#'                               method = "optimization")
#' 
#' # For positive unbounded parameters, use gamma distribution
#' source("fit_gamma_from_ci.R")
#' omega_gamma <- fit_gamma_from_ci(mode_val = 0.000705, 
#'                                   ci_lower = 0.000471, 
#'                                   ci_upper = 0.001107,
#'                                   method = "optimization")
#' 
#' cat("Beta for bounded [0,1] parameters:\n")
#' cat("  Use for: effectiveness, proportions, probabilities\n")
#' cat("  Example: phi_1 ~ Beta(", phi_beta$shape1, ", ", phi_beta$shape2, ")\n\n")
#' 
#' cat("Gamma for positive unbounded parameters:\n")  
#' cat("  Use for: rates, times, positive continuous values\n")
#' cat("  Example: omega_1 ~ Gamma(", omega_gamma$shape, ", ", omega_gamma$rate, ")\n")
#' }
compare_distributions <- function() {
  cat("\n=== When to Use Beta vs Gamma Distributions ===\n\n")
  
  cat("Beta Distribution:\n")
  cat("  - Domain: [0, 1]\n")
  cat("  - Use for: proportions, probabilities, effectiveness\n")
  cat("  - Examples: phi_1, phi_2 (vaccine effectiveness)\n")
  cat("  - Mode exists when: alpha > 1 and beta > 1\n")
  cat("  - Mode formula: (alpha - 1) / (alpha + beta - 2)\n\n")
  
  cat("Gamma Distribution:\n")
  cat("  - Domain: (0, ∞)\n")
  cat("  - Use for: rates, times, positive continuous values\n")
  cat("  - Examples: omega_1, omega_2 (waning rates)\n")
  cat("  - Mode exists when: shape > 1\n")
  cat("  - Mode formula: (shape - 1) / rate\n\n")
  
  cat("Both functions:\n")
  cat("  - fit_beta_from_ci(): Fits beta with mode and CI constraints\n")
  cat("  - fit_gamma_from_ci(): Fits gamma with mode and CI constraints\n")
  cat("  - Methods: 'moment_matching' or 'optimization'\n")
  cat("  - Returns: shape parameters and fitted statistics\n")
}