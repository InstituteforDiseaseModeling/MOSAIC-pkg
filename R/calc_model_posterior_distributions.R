#' Calculate Model Posterior Parameter Distributions
#'
#' Calculates posterior parameter distributions from calibration results using
#' importance sampling. Transforms log-likelihood values to importance weights
#' and computes Bayesian posterior summaries for specified parameters.
#'
#' @param results Data frame containing simulation results
#' @param col_ll Column index containing log-likelihood values
#' @param col_params Vector of parameter column indices to analyze
#' @param probs Quantile probabilities (default: c(0.025, 0.25, 0.5, 0.75, 0.975))
#'
#' @return List with two components:
#' \itemize{
#'   \item \code{summary_table}: Data frame with posterior statistics for each parameter
#'   \item \code{posterior_weights}: Vector of importance weights for reuse
#' }
#'
#' @details
#' This function implements unbiased importance sampling by using ALL samples
#' (no AIC cutoff) to avoid posterior distribution bias. The importance weights
#' are calculated using Akaike weights: w_i = exp(-0.5 * delta_i) where
#' delta_i = -2 * (loglik_i - max(loglik)).
#'
#' The summary table includes:
#' \itemize{
#'   \item \code{parameter}: Parameter name
#'   \item \code{mean}: Posterior mean
#'   \item \code{sd}: Posterior standard deviation
#'   \item \code{mode}: Posterior mode (via weighted kernel density)
#'   \item \code{effective_n}: Effective sample size
#'   \item Quantile columns for specified probability levels
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' results <- read.csv("calibration_results.csv")
#' posterior <- calc_model_posterior_distributions(
#'   results = results,
#'   col_ll = 3,           # likelihood column
#'   col_params = 4:8,     # parameter columns
#'   probs = c(0.025, 0.5, 0.975)
#' )
#'
#' # View summary
#' print(posterior$summary_table)
#'
#' # Access weights for plotting
#' weights <- posterior$posterior_weights
#' }
#'
#' @seealso [calc_model_akaike_weights()], [plot_model_posteriors()]
#' @family posterior-analysis
#' @export
calc_model_posterior_distributions <- function(results, 
                                              col_ll,
                                              col_params,
                                              probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  
  # Input validation
  stopifnot(is.data.frame(results))
  stopifnot(is.numeric(col_ll) && length(col_ll) == 1)
  stopifnot(col_ll > 0 && col_ll <= ncol(results))
  stopifnot(is.numeric(col_params) && all(col_params > 0 & col_params <= ncol(results)))
  stopifnot(is.numeric(probs) && all(probs >= 0 & probs <= 1))
  
  # Calculate importance weights from ALL samples (no AIC cutoff)
  loglik <- results[[col_ll]]
  
  # Filter only invalid likelihoods
  valid_idx <- !is.na(loglik) & is.finite(loglik)
  
  if (sum(valid_idx) == 0) {
    stop("No valid likelihood values found in column ", col_ll)
  }
  
  # Calculate AIC delta and untruncated Akaike weights
  delta <- calc_model_aic_delta(loglik)
  weights_result <- calc_model_akaike_weights(delta, delta_max = Inf)  # No cutoff
  posterior_weights <- weights_result$w_tilde
  
  # Filter to valid samples
  results_valid <- results[valid_idx, ]
  weights_valid <- posterior_weights[valid_idx]
  
  # Calculate posterior summaries for each parameter
  summary_rows <- lapply(seq_along(col_params), function(i) {
    param_idx <- col_params[i]
    param_name <- names(results)[param_idx]
    values <- results_valid[[param_idx]]
    
    # Handle parameters with missing values
    param_valid <- !is.na(values)
    if (sum(param_valid) == 0) {
      # Return row of NAs
      row <- data.frame(parameter = param_name, stringsAsFactors = FALSE)
      row$mean <- NA_real_
      row$sd <- NA_real_
      row$mode <- NA_real_
      row$effective_n <- 0
      
      # Add quantile columns
      for (j in seq_along(probs)) {
        row[[paste0("q", format(probs[j] * 100, digits = 2))]] <- NA_real_
      }
      return(row)
    }
    
    # Use only valid parameter values and corresponding weights
    param_values <- values[param_valid]
    param_weights <- weights_valid[param_valid]
    
    # Renormalize weights
    param_weights <- param_weights / sum(param_weights)
    
    # Calculate statistics
    posterior_mean <- weighted.mean(param_values, param_weights)
    posterior_sd <- sqrt(weighted_var(param_values, param_weights))
    posterior_mode <- calc_weighted_mode(param_values, param_weights)
    effective_n <- 1 / sum(param_weights^2)
    
    # Calculate quantiles
    quantiles <- weighted_quantiles(param_values, param_weights, probs)
    
    # Build result row
    row <- data.frame(
      parameter = param_name,
      mean = posterior_mean,
      sd = posterior_sd,
      mode = posterior_mode,
      effective_n = effective_n,
      stringsAsFactors = FALSE
    )
    
    # Add quantile columns
    for (j in seq_along(probs)) {
      col_name <- paste0("q", format(probs[j] * 100, digits = 2))
      row[[col_name]] <- quantiles[j]
    }
    
    return(row)
  })
  
  # Combine into summary table
  summary_table <- do.call(rbind, summary_rows)
  rownames(summary_table) <- NULL
  
  # Return both summary table and weights
  list(
    summary_table = summary_table,
    posterior_weights = posterior_weights
  )
}