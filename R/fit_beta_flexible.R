#' Enhanced Beta Distribution Fitting with Flexible Prior Creation
#'
#' This function provides multiple methods for creating Beta priors from sample data
#' with enhanced flexibility for controlling distribution width, shape, and bias.
#' Designed to overcome limitations of simple variance inflation and provide better
#' control for MOSAIC model scenario exploration.
#'
#' @param samples Numeric vector of sample proportions (must be in (0,1))
#' @param method Character. Method for creating prior:
#'   - "expanded_ci": Enhanced CI expansion with non-linear scaling
#'   - "conservative": Bias mode toward zero for conservative priors  
#'   - "left_skewed": Create left-skewed distributions favoring lower values
#'   - "wide_uniform": Create very wide, nearly uniform distributions
#'   - "adaptive": Choose method based on sample characteristics
#' @param expansion_factor Numeric. Factor for CI expansion (1.0 = no change, 2.0 = double width)
#' @param conservatism_bias Numeric. Shift mode toward zero (0 = no shift, 0.5 = halfway to zero)  
#' @param min_precision Numeric. Minimum sum of shape parameters (α + β). Default 4.0.
#' @param max_precision Numeric. Maximum sum of shape parameters. Default 100.0.
#' @param target_percentile Numeric. Percentile to use as mode (0.5 = median). Default 0.3 for left-skew.
#' @param verbose Logical. Print diagnostic information.
#'
#' @return A list containing:
#' \itemize{
#'   \item shape1: Alpha parameter of Beta distribution  
#'   \item shape2: Beta parameter of Beta distribution
#'   \item method: Method used for fitting
#'   \item fitted_stats: List with mean, mode, variance, CI
#'   \item sample_stats: List with original sample statistics
#'   \item parameters_used: List of parameters used in fitting
#' }
#'
#' @examples
#' \dontrun{
#' # Generate some sample E/I proportions
#' samples <- c(0.0001, 0.0003, 0.0002, 0.0005, 0.0001, 0.0004)
#' 
#' # Expanded CI method - much wider priors
#' result1 <- fit_beta_flexible(samples, method = "expanded_ci", 
#'                              expansion_factor = 3.0)
#' 
#' # Conservative method - biased toward zero  
#' result2 <- fit_beta_flexible(samples, method = "conservative",
#'                              conservatism_bias = 0.3)
#'                              
#' # Left-skewed for natural zero-bias
#' result3 <- fit_beta_flexible(samples, method = "left_skewed")
#' 
#' # Compare distributions
#' x <- seq(0.0001, 0.01, length.out = 1000)
#' plot(x, dbeta(x, result1$shape1, result1$shape2), type="l", col="red")
#' lines(x, dbeta(x, result2$shape1, result2$shape2), col="blue")  
#' lines(x, dbeta(x, result3$shape1, result3$shape2), col="green")
#' }
#'
#' @export
fit_beta_flexible <- function(samples, 
                              method = "expanded_ci",
                              expansion_factor = 2.0,
                              conservatism_bias = 0.2,
                              min_precision = 4.0,
                              max_precision = 100.0,
                              target_percentile = 0.3,
                              verbose = FALSE) {
  
  # Input validation
  if (length(samples) == 0) stop("samples cannot be empty")
  
  # Remove invalid samples and ensure they're in (0,1)
  valid_samples <- samples[!is.na(samples) & samples > 0 & samples < 1]
  
  if (length(valid_samples) < 2) {
    warning("Insufficient valid samples - using minimal precision fallback")
    return(list(shape1 = 1.02, shape2 = 99, method = "insufficient_data",
                fitted_stats = NULL, sample_stats = NULL, parameters_used = NULL))
  }
  
  if (expansion_factor < 1.0) stop("expansion_factor must be >= 1.0")
  if (conservatism_bias < 0 || conservatism_bias > 1) stop("conservatism_bias must be in [0,1]")
  if (min_precision < 2.1) stop("min_precision must be > 2.1 for valid Beta distribution")
  if (target_percentile <= 0 || target_percentile >= 1) stop("target_percentile must be in (0,1)")
  
  # Calculate sample statistics
  sample_mean <- mean(valid_samples)
  sample_median <- median(valid_samples) 
  sample_q025 <- quantile(valid_samples, 0.025)
  sample_q975 <- quantile(valid_samples, 0.975)
  sample_var <- var(valid_samples)
  sample_sd <- sd(valid_samples)
  
  sample_stats <- list(
    mean = sample_mean, median = sample_median,
    q025 = sample_q025, q975 = sample_q975,
    var = sample_var, sd = sample_sd, n = length(valid_samples)
  )
  
  if (verbose) {
    cat(sprintf("\\n=== Flexible Beta Fitting: %s ===\\n", method))
    cat(sprintf("Samples: n=%d, mean=%.6f, median=%.6f\\n", 
                length(valid_samples), sample_mean, sample_median))
    cat(sprintf("Original 95%% CI: [%.6f, %.6f]\\n", sample_q025, sample_q975))
  }
  
  # Method-specific fitting
  if (method == "expanded_ci") {
    result <- fit_beta_expanded_ci(valid_samples, sample_stats, expansion_factor, 
                                   min_precision, max_precision, verbose)
    
  } else if (method == "conservative") {  
    result <- fit_beta_conservative(valid_samples, sample_stats, conservatism_bias,
                                    min_precision, max_precision, verbose)
    
  } else if (method == "left_skewed") {
    result <- fit_beta_left_skewed(valid_samples, sample_stats, target_percentile,
                                   min_precision, max_precision, verbose)
    
  } else if (method == "wide_uniform") {
    result <- fit_beta_wide_uniform(valid_samples, sample_stats, 
                                    min_precision, max_precision, verbose)
    
  } else if (method == "adaptive") {
    result <- fit_beta_adaptive(valid_samples, sample_stats, expansion_factor,
                                min_precision, max_precision, verbose)
    
  } else {
    stop(sprintf("Unknown method: %s. Use 'expanded_ci', 'conservative', 'left_skewed', 'wide_uniform', or 'adaptive'", method))
  }
  
  # Add metadata to result
  result$sample_stats <- sample_stats
  result$parameters_used <- list(
    method = method, expansion_factor = expansion_factor,
    conservatism_bias = conservatism_bias, min_precision = min_precision,
    max_precision = max_precision, target_percentile = target_percentile
  )
  
  return(result)
}


#' Expanded CI Method - Enhanced confidence interval expansion
#'
#' Creates much wider confidence intervals using non-linear expansion
#' and improved boundary handling.
#'
#' @keywords internal
fit_beta_expanded_ci <- function(samples, sample_stats, expansion_factor,
                                 min_precision, max_precision, verbose) {
  
  # Non-linear CI expansion
  current_width <- sample_stats$q975 - sample_stats$q025
  new_width <- current_width * expansion_factor
  
  # Center expansion around median rather than mean for better boundary handling
  ci_center <- sample_stats$median
  
  # Calculate new bounds with better boundary protection
  ci_lower_new <- max(1e-10, ci_center - new_width/2)
  ci_upper_new <- min(0.999, ci_center + new_width/2)
  
  # If bounds hit limits, expand in available direction
  if (ci_lower_new <= 1e-10) {
    ci_upper_new <- min(0.999, sample_stats$q025 + new_width)
  }
  if (ci_upper_new >= 0.999) {
    ci_lower_new <- max(1e-10, sample_stats$q975 - new_width)
  }
  
  # Use median as mode for expanded distributions
  mode_val <- sample_stats$median
  
  if (verbose) {
    cat(sprintf("Expanded CI: [%.6f, %.6f] -> [%.6f, %.6f] (%.1fx wider)\\n",
                sample_stats$q025, sample_stats$q975, ci_lower_new, ci_upper_new, expansion_factor))
  }
  
  # Fit using enhanced fit_beta_from_ci  
  beta_fit <- fit_beta_from_ci(
    mode_val = mode_val,
    ci_lower = ci_lower_new,
    ci_upper = ci_upper_new,
    method = "optimization"
  )
  
  # Ensure both parameters are > 1.01 for well-defined mode
  beta_fit$shape1 <- max(1.02, beta_fit$shape1)
  beta_fit$shape2 <- max(1.02, beta_fit$shape2)
  
  # Ensure minimum precision
  total_precision <- beta_fit$shape1 + beta_fit$shape2
  if (total_precision < min_precision) {
    scale_factor <- min_precision / total_precision
    beta_fit$shape1 <- beta_fit$shape1 * scale_factor
    beta_fit$shape2 <- beta_fit$shape2 * scale_factor
  }
  
  # Cap maximum precision to prevent over-tight distributions
  total_precision <- beta_fit$shape1 + beta_fit$shape2
  if (total_precision > max_precision) {
    scale_factor <- max_precision / total_precision
    beta_fit$shape1 <- beta_fit$shape1 * scale_factor
    beta_fit$shape2 <- beta_fit$shape2 * scale_factor
  }
  
  # Final check to ensure both > 1.01
  beta_fit$shape1 <- max(1.02, beta_fit$shape1)
  beta_fit$shape2 <- max(1.02, beta_fit$shape2)
  
  return(create_beta_result(beta_fit$shape1, beta_fit$shape2, "expanded_ci", verbose))
}


#' Conservative Method - Bias mode toward zero  
#'
#' Creates priors biased toward lower values for conservative estimation.
#'
#' @keywords internal
fit_beta_conservative <- function(samples, sample_stats, conservatism_bias,
                                  min_precision, max_precision, verbose) {
  
  # Shift mode toward zero
  original_mode <- sample_stats$median
  conservative_mode <- original_mode * (1 - conservatism_bias)
  conservative_mode <- max(1e-10, conservative_mode)
  
  # Keep CI width similar but shift down
  ci_width <- sample_stats$q975 - sample_stats$q025
  ci_shift <- original_mode - conservative_mode
  
  ci_lower_new <- max(1e-10, sample_stats$q025 - ci_shift)
  ci_upper_new <- max(conservative_mode + 1e-6, sample_stats$q975 - ci_shift)
  
  if (verbose) {
    cat(sprintf("Conservative shift: mode %.6f -> %.6f (%.1f%% toward zero)\\n",
                original_mode, conservative_mode, 100*conservatism_bias))
    cat(sprintf("Conservative CI: [%.6f, %.6f]\\n", ci_lower_new, ci_upper_new))
  }
  
  # Fit with conservative mode
  beta_fit <- fit_beta_from_ci(
    mode_val = conservative_mode,
    ci_lower = ci_lower_new, 
    ci_upper = ci_upper_new,
    method = "optimization"
  )
  
  # Convert to proper format and apply precision constraints
  beta_result <- list(shape1 = beta_fit$shape1, shape2 = beta_fit$shape2)
  beta_result <- apply_precision_constraints(beta_result, min_precision, max_precision)
  
  return(create_beta_result(beta_result$shape1, beta_result$shape2, "conservative", verbose))
}


#' Left-skewed Method - Create naturally zero-biased distributions
#'
#' Creates left-skewed Beta distributions that naturally favor values near zero.
#'
#' @keywords internal
fit_beta_left_skewed <- function(samples, sample_stats, target_percentile, 
                                 min_precision, max_precision, verbose) {
  
  # Use a low percentile as the mode to create left-skewed distribution
  mode_val <- quantile(samples, target_percentile)
  mode_val <- max(1e-10, mode_val)
  
  # Expand CI asymmetrically - more expansion toward higher values
  ci_lower_new <- max(1e-10, sample_stats$q025 * 0.5)  # Compress lower tail
  ci_upper_new <- min(0.999, sample_stats$q975 * 2.0)  # Expand upper tail
  
  if (verbose) {
    cat(sprintf("Left-skewed: mode at %.0fth percentile = %.6f\\n", 
                100*target_percentile, mode_val))
    cat(sprintf("Asymmetric CI: [%.6f, %.6f]\\n", ci_lower_new, ci_upper_new))
  }
  
  # For left-skewed distributions, we want β > α (more mass near 0)
  # Start with method of moments then adjust for skewness
  mean_approx <- (ci_lower_new + ci_upper_new) / 2
  var_approx <- ((ci_upper_new - ci_lower_new) / 4)^2
  
  # Method of moments
  precision <- mean_approx * (1 - mean_approx) / var_approx - 1
  precision <- max(min_precision, precision)
  
  alpha <- mean_approx * precision
  beta <- (1 - mean_approx) * precision
  
  # Adjust for left skewness (increase β relative to α)
  skew_factor <- 1.5  # Increase right-side parameter
  beta <- beta * skew_factor
  
  # Ensure constraints  
  alpha <- max(1.02, alpha)
  beta <- max(1.02, beta) 
  
  beta_fit <- list(shape1 = alpha, shape2 = beta)
  beta_fit <- apply_precision_constraints(beta_fit, min_precision, max_precision)
  
  return(create_beta_result(beta_fit$shape1, beta_fit$shape2, "left_skewed", verbose))
}


#' Wide Uniform Method - Create very wide, nearly uniform distributions
#'
#' Creates Beta distributions that are nearly uniform over the valid range.
#'
#' @keywords internal  
fit_beta_wide_uniform <- function(samples, sample_stats, min_precision, max_precision, verbose) {
  
  # For nearly uniform distribution, use α ≈ β and both slightly > 1
  # Lower precision = more uniform
  precision <- min_precision * 0.8  # Even lower precision for wider spread
  
  # Use uniform parameters
  alpha <- precision / 2
  beta <- precision / 2
  
  # Ensure both > 1.01
  alpha <- max(1.02, alpha)
  beta <- max(1.02, beta)
  
  if (verbose) {
    cat(sprintf("Wide uniform: α = β = %.3f (nearly uniform)\\n", alpha))
    fitted_mean <- alpha / (alpha + beta)
    cat(sprintf("Results in mean = %.3f, wide support\\n", fitted_mean))
  }
  
  beta_fit <- list(shape1 = alpha, shape2 = beta)
  
  return(create_beta_result(beta_fit$shape1, beta_fit$shape2, "wide_uniform", verbose))
}


#' Adaptive Method - Choose best method based on sample characteristics
#'
#' Automatically selects the most appropriate method based on sample properties.
#'
#' @keywords internal
fit_beta_adaptive <- function(samples, sample_stats, expansion_factor, 
                              min_precision, max_precision, verbose) {
  
  # Decision logic based on sample characteristics
  cv <- sample_stats$sd / sample_stats$mean  # Coefficient of variation
  
  if (cv > 2.0) {
    # High variability - use wide uniform
    method_chosen <- "wide_uniform"
    if (verbose) cat("Adaptive choice: high CV -> wide_uniform\\n")
    return(fit_beta_wide_uniform(samples, sample_stats, min_precision, max_precision, verbose))
    
  } else if (sample_stats$mean < 0.001) {
    # Very small values - use conservative  
    method_chosen <- "conservative"
    if (verbose) cat("Adaptive choice: small mean -> conservative\\n")
    return(fit_beta_conservative(samples, sample_stats, 0.3, min_precision, max_precision, verbose))
    
  } else {
    # Default to expanded CI
    method_chosen <- "expanded_ci"
    if (verbose) cat("Adaptive choice: default -> expanded_ci\\n")
    return(fit_beta_expanded_ci(samples, sample_stats, expansion_factor, min_precision, max_precision, verbose))
  }
}


#' Apply precision constraints to Beta fit
#'
#' @keywords internal
apply_precision_constraints <- function(beta_fit, min_precision, max_precision) {
  # First ensure both parameters are > 1.01
  beta_fit$shape1 <- max(1.02, beta_fit$shape1)
  beta_fit$shape2 <- max(1.02, beta_fit$shape2)
  
  total_precision <- beta_fit$shape1 + beta_fit$shape2
  
  if (total_precision < min_precision) {
    scale_factor <- min_precision / total_precision
    beta_fit$shape1 <- beta_fit$shape1 * scale_factor
    beta_fit$shape2 <- beta_fit$shape2 * scale_factor
  }
  
  if (total_precision > max_precision) {
    scale_factor <- max_precision / total_precision  
    beta_fit$shape1 <- beta_fit$shape1 * scale_factor
    beta_fit$shape2 <- beta_fit$shape2 * scale_factor
  }
  
  # Final check to ensure constraints maintained
  beta_fit$shape1 <- max(1.02, beta_fit$shape1)
  beta_fit$shape2 <- max(1.02, beta_fit$shape2)
  
  return(beta_fit)
}


#' Create standardized result object
#'
#' @keywords internal
create_beta_result <- function(shape1, shape2, method, verbose) {
  
  # Calculate fitted statistics
  fitted_mean <- shape1 / (shape1 + shape2)
  fitted_var <- (shape1 * shape2) / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))
  fitted_mode <- if (shape1 > 1 && shape2 > 1) {
    (shape1 - 1) / (shape1 + shape2 - 2)
  } else {
    NA
  }
  
  fitted_lower <- qbeta(0.025, shape1, shape2) 
  fitted_upper <- qbeta(0.975, shape1, shape2)
  
  fitted_stats <- list(
    mean = fitted_mean, mode = fitted_mode, variance = fitted_var,
    sd = sqrt(fitted_var), ci = c(lower = fitted_lower, upper = fitted_upper)
  )
  
  if (verbose) {
    cat(sprintf("Final Beta(%.3f, %.3f): mean=%.6f, mode=%.6f\\n", 
                shape1, shape2, fitted_mean, fitted_mode))
    cat(sprintf("95%% CI: [%.6f, %.6f]\\n", fitted_lower, fitted_upper))
  }
  
  return(list(
    shape1 = shape1, shape2 = shape2, method = method,
    fitted_stats = fitted_stats
  ))
}