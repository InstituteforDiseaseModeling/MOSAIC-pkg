#' Check likelihood guardrails to identify unrealistic predictions
#'
#' This function performs pre-screening checks on observed vs estimated epidemic data
#' to identify clearly unrealistic predictions that should receive floor likelihood values.
#' It checks for magnitude mismatches, negative correlations, and nonsensical values.
#'
#' @param obs_cases Matrix of observed cases, \code{n_locations} x \code{n_time_steps}.
#' @param est_cases Matrix of estimated cases, same dimension as \code{obs_cases}.
#' @param obs_deaths Matrix of observed deaths, \code{n_locations} x \code{n_time_steps}.
#' @param est_deaths Matrix of estimated deaths, same dimension as \code{obs_deaths}.
#' @param floor_ll Numeric scalar, the floor likelihood value to return if guardrails triggered.
#'                 Default \code{-1e8}.
#' @param over_prediction_ratio Numeric scalar, threshold for over-prediction detection.
#'                             Triggers if estimated total >= ratio * observed total. Default \code{50}.
#' @param under_prediction_ratio Numeric scalar, threshold for severe under-prediction.
#'                              Triggers if estimated total < ratio * observed total. Default \code{0.02}.
#' @param min_cases_for_epidemic Numeric scalar, minimum observed cases to trigger over-prediction check.
#'                              Default \code{500}.
#' @param substantial_epidemic Numeric scalar, threshold for "missing epidemic" detection.
#'                            Triggers under-prediction check when observed > this value. Default \code{2000}.
#' @param zero_threshold Numeric scalar, minimum observed total to penalize zero predictions.
#'                      Default \code{500}.
#' @param negative_correlation_threshold Numeric scalar, correlation threshold below which to trigger guardrail.
#'                                      Default \code{-0.2}.
#' @param max_cases_per_timestep Numeric scalar, maximum reasonable cases per location per timestep.
#'                              Default \code{1e6}.
#' @param max_deaths_per_timestep Numeric scalar, maximum reasonable deaths per location per timestep.
#'                               Default \code{1e5}.
#' @param cumulative_over_ratio Numeric scalar, threshold for cumulative over-prediction detection.
#'                             Triggers if estimated cumulative >= ratio * observed cumulative. Default \code{500}.
#' @param cumulative_under_ratio Numeric scalar, threshold for severe cumulative under-prediction.
#'                              Triggers if estimated cumulative < ratio * observed cumulative. Default \code{0.001}.
#' @param min_cumulative_for_check Numeric scalar, minimum observed cumulative to trigger ratio checks.
#'                                Default \code{100}.
#' @param verbose Logical, if \code{TRUE} prints detailed violation messages. Default \code{FALSE}.
#'
#' @return A list with components:
#'   \itemize{
#'     \item \code{status}: Character, either \code{"PROCEED"} or \code{"FLOOR"}
#'     \item \code{violations}: Character vector describing any violations found
#'     \item \code{floor_ll}: The floor likelihood value (only if status is \code{"FLOOR"})
#'   }
#'
#' @details
#' The function performs three categories of checks:
#' \enumerate{
#'   \item \strong{Magnitude Mismatches}: Detects extreme over/under-prediction, missing epidemics, or zero predictions
#'   \item \strong{Negative Correlations}: Identifies cases where predictions move opposite to observations
#'   \item \strong{Nonsensical Values}: Catches negative, infinite, or extremely large predicted values
#' }
#' 
#' If any guardrail is triggered, the function returns \code{status = "FLOOR"} indicating that
#' \code{calc_model_likelihood} should return the floor likelihood value immediately rather than
#' performing expensive likelihood calculations on clearly unrealistic predictions.
#'
#' @export
#'
#' @examples
#' # Example with over-prediction that should trigger guardrail
#' obs_cases <- matrix(c(10, 20, 15, 5), nrow = 2, ncol = 2)
#' est_cases <- matrix(c(200, 300, 250, 100), nrow = 2, ncol = 2)  # 15x over-prediction
#' obs_deaths <- matrix(c(1, 2, 1, 0), nrow = 2, ncol = 2)
#' est_deaths <- matrix(c(1, 2, 1, 0), nrow = 2, ncol = 2)
#'
#' result <- check_likelihood_guardrails(obs_cases, est_cases, obs_deaths, est_deaths, verbose = TRUE)
#' print(result$status)  # Should be "FLOOR"
#'
check_likelihood_guardrails <- function(
    obs_cases,
    est_cases, 
    obs_deaths,
    est_deaths,
    floor_ll = -1e8,
    # Magnitude mismatch thresholds
    over_prediction_ratio = 50,
    under_prediction_ratio = 0.02,
    min_cases_for_epidemic = 500,
    substantial_epidemic = 2000,
    zero_threshold = 500,
    # Correlation threshold
    negative_correlation_threshold = -0.2,
    # Nonsensical value limits
    max_cases_per_timestep = 1e6,
    max_deaths_per_timestep = 1e5,
    # Cumulative guardrails
    cumulative_over_ratio = 500,
    cumulative_under_ratio = 0.001,
    min_cumulative_for_check = 100,
    verbose = FALSE
) {
    
    # Input validation
    if (!is.matrix(obs_cases) || !is.matrix(est_cases) || 
        !is.matrix(obs_deaths) || !is.matrix(est_deaths)) {
        stop("All inputs must be matrices")
    }
    
    if (!all(dim(obs_cases) == dim(est_cases)) ||
        !all(dim(obs_cases) == dim(obs_deaths)) ||
        !all(dim(obs_cases) == dim(est_deaths))) {
        stop("All matrices must have the same dimensions")
    }
    
    # Special case: if all data is NA, let original function handle it (should return NA)
    if (all(is.na(obs_cases)) && all(is.na(est_cases)) && 
        all(is.na(obs_deaths)) && all(is.na(est_deaths))) {
        if (verbose) {
            message("All data is NA - proceeding to original likelihood calculation")
        }
        return(list(status = "PROCEED", violations = character(0)))
    }
    
    violations <- character(0)
    
    # Check 1: Magnitude mismatches
    magnitude_violations <- check_magnitude_mismatches(
        obs_cases, est_cases, obs_deaths, est_deaths,
        over_prediction_ratio, under_prediction_ratio,
        min_cases_for_epidemic, substantial_epidemic, zero_threshold
    )
    violations <- c(violations, magnitude_violations)
    
    # Check 2: Negative correlations  
    correlation_violations <- check_negative_correlation(
        obs_cases, est_cases, obs_deaths, est_deaths,
        negative_correlation_threshold
    )
    violations <- c(violations, correlation_violations)
    
    # Check 3: Nonsensical values
    nonsensical_violations <- check_nonsensical_values(
        est_cases, est_deaths,
        max_cases_per_timestep, max_deaths_per_timestep
    )
    violations <- c(violations, nonsensical_violations)
    
    # Check 4: Cumulative mismatches
    cumulative_violations <- check_cumulative_mismatches(
        obs_cases, est_cases, obs_deaths, est_deaths,
        cumulative_over_ratio, cumulative_under_ratio, min_cumulative_for_check
    )
    violations <- c(violations, cumulative_violations)
    
    # Determine result
    if (length(violations) > 0) {
        if (verbose) {
            message("Likelihood guardrails triggered:")
            for (v in violations) {
                message(sprintf("  - %s", v))
            }
            message(sprintf("Returning floor likelihood: %.0f", floor_ll))
        }
        
        return(list(
            status = "FLOOR",
            violations = violations,
            floor_ll = floor_ll
        ))
    }
    
    return(list(
        status = "PROCEED",
        violations = character(0)
    ))
}


#' Check for magnitude mismatches between observed and estimated data
#'
#' @param obs_cases Matrix of observed cases
#' @param est_cases Matrix of estimated cases  
#' @param obs_deaths Matrix of observed deaths
#' @param est_deaths Matrix of estimated deaths
#' @param over_ratio Threshold for over-prediction detection
#' @param under_ratio Threshold for under-prediction detection
#' @param min_cases Minimum observed cases to trigger over-prediction check
#' @param substantial Threshold for missing epidemic detection
#' @param zero_thresh Minimum observed to penalize zero predictions
#'
#' @return Character vector of violations found
#' @keywords internal
check_magnitude_mismatches <- function(obs_cases, est_cases, obs_deaths, est_deaths,
                                     over_ratio, under_ratio, min_cases, substantial, zero_thresh) {
    
    violations <- character(0)
    n_locations <- nrow(obs_cases)
    
    for (i in seq_len(n_locations)) {
        
        # Calculate totals for this location
        obs_cases_total <- sum(obs_cases[i, ], na.rm = TRUE)
        est_cases_total <- sum(est_cases[i, ], na.rm = TRUE)
        obs_deaths_total <- sum(obs_deaths[i, ], na.rm = TRUE)  
        est_deaths_total <- sum(est_deaths[i, ], na.rm = TRUE)
        
        # Skip if no meaningful observed data
        if (!is.finite(obs_cases_total) && !is.finite(obs_deaths_total)) next
        if (obs_cases_total == 0 && obs_deaths_total == 0) next
        
        # Check cases
        if (is.finite(obs_cases_total) && is.finite(est_cases_total)) {
            
            # Over-prediction check
            if (obs_cases_total >= min_cases && est_cases_total >= over_ratio * obs_cases_total) {
                violations <- c(violations, 
                    sprintf("Cases over-prediction location %d: estimated %.0f vs observed %.0f (%.1fx)", 
                            i, est_cases_total, obs_cases_total, est_cases_total / obs_cases_total))
            }
            
            # Severe under-prediction check
            if (obs_cases_total >= substantial && est_cases_total < under_ratio * obs_cases_total) {
                violations <- c(violations,
                    sprintf("Cases severe under-prediction location %d: estimated %.0f vs observed %.0f (%.2fx)",
                            i, est_cases_total, obs_cases_total, est_cases_total / obs_cases_total))
            }
            
            # Missing epidemic check
            if (obs_cases_total >= substantial && est_cases_total < min_cases) {
                violations <- c(violations,
                    sprintf("Missing epidemic location %d: observed %.0f cases but estimated only %.0f",
                            i, obs_cases_total, est_cases_total))
            }
            
            # Zero prediction check
            if (obs_cases_total >= zero_thresh && est_cases_total == 0) {
                violations <- c(violations,
                    sprintf("Zero cases prediction location %d: observed %.0f cases but estimated 0",
                            i, obs_cases_total))
            }
        }
        
        # Check deaths (same logic)
        if (is.finite(obs_deaths_total) && is.finite(est_deaths_total)) {
            
            # Over-prediction check  
            if (obs_deaths_total >= min_cases && est_deaths_total >= over_ratio * obs_deaths_total) {
                violations <- c(violations,
                    sprintf("Deaths over-prediction location %d: estimated %.0f vs observed %.0f (%.1fx)",
                            i, est_deaths_total, obs_deaths_total, est_deaths_total / obs_deaths_total))
            }
            
            # Severe under-prediction check
            if (obs_deaths_total >= substantial && est_deaths_total < under_ratio * obs_deaths_total) {
                violations <- c(violations,
                    sprintf("Deaths severe under-prediction location %d: estimated %.0f vs observed %.0f (%.2fx)",
                            i, est_deaths_total, obs_deaths_total, est_deaths_total / obs_deaths_total))
            }
            
            # Missing epidemic check
            if (obs_deaths_total >= substantial && est_deaths_total < min_cases) {
                violations <- c(violations,
                    sprintf("Missing deaths epidemic location %d: observed %.0f deaths but estimated only %.0f",
                            i, obs_deaths_total, est_deaths_total))
            }
            
            # Zero prediction check
            if (obs_deaths_total >= zero_thresh && est_deaths_total == 0) {
                violations <- c(violations,
                    sprintf("Zero deaths prediction location %d: observed %.0f deaths but estimated 0",
                            i, obs_deaths_total))
            }
        }
    }
    
    return(violations)
}


#' Check for negative correlations between observed and estimated time series
#'
#' @param obs_cases Matrix of observed cases
#' @param est_cases Matrix of estimated cases
#' @param obs_deaths Matrix of observed deaths  
#' @param est_deaths Matrix of estimated deaths
#' @param threshold Correlation threshold below which to trigger guardrail
#'
#' @return Character vector of violations found
#' @keywords internal
check_negative_correlation <- function(obs_cases, est_cases, obs_deaths, est_deaths, threshold) {
    
    violations <- character(0)
    n_locations <- nrow(obs_cases)
    
    for (i in seq_len(n_locations)) {
        
        # Check cases correlation
        obs_cases_vec <- obs_cases[i, ]
        est_cases_vec <- est_cases[i, ]
        
        cor_cases <- calc_pearson_correlation_safe(obs_cases_vec, est_cases_vec)
        if (!is.na(cor_cases) && is.finite(cor_cases) && cor_cases < threshold) {
            violations <- c(violations,
                sprintf("Negative cases correlation location %d: r = %.3f", i, cor_cases))
        }
        
        # Check deaths correlation  
        obs_deaths_vec <- obs_deaths[i, ]
        est_deaths_vec <- est_deaths[i, ]
        
        cor_deaths <- calc_pearson_correlation_safe(obs_deaths_vec, est_deaths_vec)
        if (!is.na(cor_deaths) && is.finite(cor_deaths) && cor_deaths < threshold) {
            violations <- c(violations,
                sprintf("Negative deaths correlation location %d: r = %.3f", i, cor_deaths))
        }
    }
    
    return(violations)
}


#' Check for nonsensical values in estimated data
#'
#' @param est_cases Matrix of estimated cases
#' @param est_deaths Matrix of estimated deaths
#' @param max_cases Maximum reasonable cases per timestep
#' @param max_deaths Maximum reasonable deaths per timestep
#'
#' @return Character vector of violations found  
#' @keywords internal
check_nonsensical_values <- function(est_cases, est_deaths, max_cases, max_deaths) {
    
    violations <- character(0)
    
    # Check estimated cases
    if (any(est_cases < 0, na.rm = TRUE)) {
        n_negative <- sum(est_cases < 0, na.rm = TRUE)
        violations <- c(violations,
            sprintf("Negative estimated cases: %d values < 0", n_negative))
    }
    
    if (any(!is.finite(est_cases))) {
        n_nonfinite <- sum(!is.finite(est_cases))
        violations <- c(violations,
            sprintf("Non-finite estimated cases: %d non-finite values", n_nonfinite))
    }
    
    if (any(est_cases > max_cases, na.rm = TRUE)) {
        max_found <- max(est_cases, na.rm = TRUE)
        violations <- c(violations,
            sprintf("Extreme estimated cases: max value %.0f > threshold %.0f", max_found, max_cases))
    }
    
    # Check estimated deaths
    if (any(est_deaths < 0, na.rm = TRUE)) {
        n_negative <- sum(est_deaths < 0, na.rm = TRUE)
        violations <- c(violations,
            sprintf("Negative estimated deaths: %d values < 0", n_negative))
    }
    
    if (any(!is.finite(est_deaths))) {
        n_nonfinite <- sum(!is.finite(est_deaths))
        violations <- c(violations,
            sprintf("Non-finite estimated deaths: %d non-finite values", n_nonfinite))
    }
    
    if (any(est_deaths > max_deaths, na.rm = TRUE)) {
        max_found <- max(est_deaths, na.rm = TRUE)
        violations <- c(violations,
            sprintf("Extreme estimated deaths: max value %.0f > threshold %.0f", max_found, max_deaths))
    }
    
    return(violations)
}


#' Safely calculate Pearson correlation handling edge cases
#'
#' @param x Numeric vector
#' @param y Numeric vector  
#'
#' @return Numeric correlation coefficient or NA if cannot be calculated
#' @keywords internal
calc_pearson_correlation_safe <- function(x, y) {
    
    # Remove any paired NAs
    valid_idx <- is.finite(x) & is.finite(y)
    if (sum(valid_idx) < 3) return(NA_real_)  # Need at least 3 points
    
    x_clean <- x[valid_idx]
    y_clean <- y[valid_idx]
    
    # Check for zero variance (constant vectors)
    if (stats::var(x_clean) == 0 || stats::var(y_clean) == 0) return(NA_real_)
    
    # Calculate correlation safely
    tryCatch({
        stats::cor(x_clean, y_clean, method = "pearson")
    }, error = function(e) {
        NA_real_
    })
}


#' Check for cumulative case and death mismatches
#'
#' @param obs_cases Matrix of observed cases
#' @param est_cases Matrix of estimated cases
#' @param obs_deaths Matrix of observed deaths
#' @param est_deaths Matrix of estimated deaths
#' @param over_ratio Threshold for cumulative over-prediction detection
#' @param under_ratio Threshold for severe cumulative under-prediction
#' @param min_cumulative Minimum observed cumulative to trigger ratio checks
#'
#' @return Character vector of violations found
#' @keywords internal
check_cumulative_mismatches <- function(obs_cases, est_cases, obs_deaths, est_deaths,
                                       over_ratio, under_ratio, min_cumulative) {
    
    violations <- character(0)
    n_locations <- nrow(obs_cases)
    
    for (i in seq_len(n_locations)) {
        
        # Calculate cumulative totals for this location
        obs_cases_cum <- sum(obs_cases[i, ], na.rm = TRUE)
        est_cases_cum <- sum(est_cases[i, ], na.rm = TRUE)
        obs_deaths_cum <- sum(obs_deaths[i, ], na.rm = TRUE)
        est_deaths_cum <- sum(est_deaths[i, ], na.rm = TRUE)
        
        # Skip if no meaningful observed data
        if (!is.finite(obs_cases_cum) && !is.finite(obs_deaths_cum)) next
        if (obs_cases_cum == 0 && obs_deaths_cum == 0) next
        
        # Check cumulative cases
        if (is.finite(obs_cases_cum) && is.finite(est_cases_cum) && 
            obs_cases_cum >= min_cumulative) {
            
            # Extreme cumulative over-prediction
            if (est_cases_cum >= over_ratio * obs_cases_cum) {
                violations <- c(violations,
                    sprintf("Cumulative cases over-prediction location %d: estimated %.0f vs observed %.0f (%.0fx)",
                            i, est_cases_cum, obs_cases_cum, est_cases_cum / obs_cases_cum))
            }
            
            # Extreme cumulative under-prediction  
            if (est_cases_cum < under_ratio * obs_cases_cum) {
                violations <- c(violations,
                    sprintf("Cumulative cases severe under-prediction location %d: estimated %.0f vs observed %.0f (%.3fx)",
                            i, est_cases_cum, obs_cases_cum, est_cases_cum / obs_cases_cum))
            }
        }
        
        # Check cumulative deaths
        if (is.finite(obs_deaths_cum) && is.finite(est_deaths_cum) &&
            obs_deaths_cum >= min_cumulative) {
            
            # Extreme cumulative over-prediction
            if (est_deaths_cum >= over_ratio * obs_deaths_cum) {
                violations <- c(violations,
                    sprintf("Cumulative deaths over-prediction location %d: estimated %.0f vs observed %.0f (%.0fx)",
                            i, est_deaths_cum, obs_deaths_cum, est_deaths_cum / obs_deaths_cum))
            }
            
            # Extreme cumulative under-prediction
            if (est_deaths_cum < under_ratio * obs_deaths_cum) {
                violations <- c(violations,
                    sprintf("Cumulative deaths severe under-prediction location %d: estimated %.0f vs observed %.0f (%.3fx)",
                            i, est_deaths_cum, obs_deaths_cum, est_deaths_cum / obs_deaths_cum))
            }
        }
    }
    
    return(violations)
}