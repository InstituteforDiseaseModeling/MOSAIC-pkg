#' Grid Search for Best Subset with Early Stopping
#'
#' Performs exhaustive grid search to find the smallest subset size that meets
#' convergence criteria (ESS, A, CVw) using Gibbs weighting. Stops at first convergence.
#'
#' @param results Data frame of calibration results with columns: sim, likelihood
#' @param target_ESS Numeric target for Effective Sample Size (ESS)
#' @param target_A Numeric target for Agreement Index (A)
#' @param target_CVw Numeric target for Coefficient of Variation of weights (CVw)
#' @param min_size Integer minimum subset size to search
#' @param max_size Integer maximum subset size to search
#' @param step_size Integer step size for search (default 1)
#' @param ess_method Character ESS calculation method: "kish" or "perplexity"
#' @param verbose Logical print progress messages
#'
#' @return List with elements:
#' \itemize{
#'   \item n: Optimal subset size (smallest n meeting criteria)
#'   \item subset: Data frame of selected simulations
#'   \item metrics: List with ESS, A, CVw values at optimal n
#'   \item converged: Logical indicating if criteria were met
#'   \item evaluations: Integer number of n values tested
#' }
#'
#' @details
#' The function searches from min_size to max_size by step_size, stopping at the
#' first size where all three criteria are met simultaneously:
#' - ESS >= target_ESS
#' - A >= target_A
#' - CVw <= target_CVw
#'
#' Metrics are calculated using Gibbs weighting with dynamic temperature:
#' 1. Calculate AIC = -2 * likelihood for subset
#' 2. Calculate Delta AIC (relative to best in subset)
#' 3. Calculate dynamic temperature:
#'    - effective_range = 4.0 (standard Akaike range)
#'    - actual_range = range(Delta AIC)
#'    - temperature = 0.5 * (effective_range / actual_range)
#' 4. Apply Gibbs weighting: weights = exp(-Delta AIC / temperature) / Z
#' 5. Calculate ESS, A, CVw from Gibbs-weighted samples
#'
#' The temperature adapts to the data's actual Delta AIC range, providing
#' consistent discrimination across different likelihood spreads.
#'
#' If no size meets criteria, returns results at max_size with converged=FALSE.
#'
#' @examples
#' \dontrun{
#' result <- grid_search_best_subset(
#'   results = calibration_results,
#'   target_ESS = 500,
#'   target_A = 0.95,
#'   target_CVw = 0.7,
#'   min_size = 30,
#'   max_size = 1000
#' )
#' }
#'
#' @export
grid_search_best_subset <- function(
    results,
    target_ESS,
    target_A,
    target_CVw,
    min_size,
    max_size,
    step_size = 1,
    ess_method = c("kish", "perplexity"),
    verbose = FALSE
) {

  # Validate inputs
  if (!is.data.frame(results)) {
    stop("results must be a data frame")
  }

  if (!all(c("sim", "likelihood") %in% names(results))) {
    stop("results must contain columns: sim, likelihood")
  }

  if (nrow(results) == 0) {
    stop("results is empty")
  }

  if (!is.numeric(target_ESS) || target_ESS <= 0) {
    stop("target_ESS must be positive numeric")
  }

  if (!is.numeric(target_A) || target_A <= 0 || target_A > 1) {
    stop("target_A must be in (0, 1]")
  }

  if (!is.numeric(target_CVw) || target_CVw <= 0) {
    stop("target_CVw must be positive numeric")
  }

  if (!is.numeric(min_size) || min_size < 1) {
    stop("min_size must be positive integer")
  }

  if (!is.numeric(max_size) || max_size < min_size) {
    stop("max_size must be >= min_size")
  }

  if (max_size > nrow(results)) {
    warning(sprintf("max_size (%d) exceeds available simulations (%d), using %d",
                    max_size, nrow(results), nrow(results)))
    max_size <- nrow(results)
  }

  ess_method <- match.arg(ess_method)

  # Rank results by likelihood (descending)
  results_ranked <- results[order(results$likelihood, decreasing = TRUE), ]

  # Grid search with early stopping
  n_values <- seq(min_size, max_size, by = step_size)
  evaluations <- 0

  for (n in n_values) {
    evaluations <- evaluations + 1

    # Select top n
    subset_n <- results_ranked[1:n, ]

    # Calculate Gibbs weights based on likelihood (matching identify_best_subset logic)
    # 1. Calculate AIC for subset
    aic_subset <- -2 * subset_n$likelihood

    # 2. Calculate delta AIC within subset (relative to best in subset)
    best_aic_in_subset <- min(aic_subset)
    delta_subset <- aic_subset - best_aic_in_subset

    # 3. Calculate dynamic Gibbs temperature
    # Use fixed effective AIC range of 4.0 (standard for Akaike weighting)
    effective_range <- 4.0
    actual_range <- diff(range(delta_subset))

    # Avoid division by zero if all models have same likelihood
    if (actual_range < .Machine$double.eps) {
      gibbs_temperature <- 1  # Default to standard if no variation
    } else {
      # Scale to get similar discrimination as standard Akaike
      gibbs_temperature <- 0.5 * (effective_range / actual_range)
    }

    # 4. Calculate Gibbs weights
    weights_n <- calc_model_weights_gibbs(
      x = delta_subset,
      temperature = gibbs_temperature,
      verbose = FALSE
    )

    # 5. Calculate metrics using Gibbs weights
    # ESS (using normalized weights)
    if (ess_method == "kish") {
      ESS <- 1 / sum(weights_n^2)
    } else {  # perplexity
      ESS <- exp(-sum(weights_n * log(weights_n + 1e-300)))
    }

    # Agreement Index (A) - using unnormalized weights
    w_unnorm <- weights_n * n
    ag <- calc_model_agreement_index(w_unnorm)
    A <- ag$A

    # Coefficient of Variation of weights (CVw) - using unnormalized weights
    CVw <- calc_model_cvw(w_unnorm)

    if (verbose) {
      cat(sprintf("  n=%5d: ESS=%7.1f (target=%.1f), A=%5.3f (target=%.3f), CVw=%5.3f (target=%.3f)\n",
                  n, ESS, target_ESS, A, target_A, CVw, target_CVw))
    }

    # Check convergence (all three criteria must be met)
    if (ESS >= target_ESS && A >= target_A && CVw <= target_CVw) {
      if (verbose) {
        cat(sprintf("  ✓ Converged at n=%d after %d evaluations\n", n, evaluations))
      }

      return(list(
        n = n,
        subset = subset_n,
        metrics = list(
          ESS = ESS,
          A = A,
          CVw = CVw
        ),
        converged = TRUE,
        evaluations = evaluations
      ))
    }
  }

  # No convergence - return results at max_size with Gibbs weights
  subset_max <- results_ranked[1:max_size, ]

  # Calculate Gibbs weights for fallback (with dynamic temperature)
  aic_max <- -2 * subset_max$likelihood
  best_aic_max <- min(aic_max)
  delta_max <- aic_max - best_aic_max

  # Dynamic temperature for fallback
  effective_range_max <- 4.0
  actual_range_max <- diff(range(delta_max))

  if (actual_range_max < .Machine$double.eps) {
    gibbs_temperature_max <- 1
  } else {
    gibbs_temperature_max <- 0.5 * (effective_range_max / actual_range_max)
  }

  weights_max <- calc_model_weights_gibbs(
    x = delta_max,
    temperature = gibbs_temperature_max,
    verbose = FALSE
  )

  # Calculate metrics
  if (ess_method == "kish") {
    ESS_max <- 1 / sum(weights_max^2)
  } else {
    ESS_max <- exp(-sum(weights_max * log(weights_max + 1e-300)))
  }

  w_max_unnorm <- weights_max * max_size
  ag_max <- calc_model_agreement_index(w_max_unnorm)
  A_max <- ag_max$A
  CVw_max <- calc_model_cvw(w_max_unnorm)

  if (verbose) {
    cat(sprintf("  ✗ No convergence after %d evaluations (max_size=%d reached)\n",
                evaluations, max_size))
    cat(sprintf("    Final: ESS=%.1f (target=%.1f), A=%.3f (target=%.3f), CVw=%.3f (target=%.3f)\n",
                ESS_max, target_ESS, A_max, target_A, CVw_max, target_CVw))
  }

  return(list(
    n = max_size,
    subset = subset_max,
    metrics = list(
      ESS = ESS_max,
      A = A_max,
      CVw = CVw_max
    ),
    converged = FALSE,
    evaluations = evaluations
  ))
}
