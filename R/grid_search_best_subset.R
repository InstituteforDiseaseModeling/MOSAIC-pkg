#' Grid Search for Best Subset with Early Stopping
#'
#' Performs exhaustive grid search to find the smallest subset size that meets
#' convergence criteria (ESS, A, CVw) using uniform weights. Stops at first convergence.
#'
#' @param results Data frame of calibration results with columns: sim_id, likelihood
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
#' Metrics are calculated using uniform weights (1/n for each sample) since Gibbs
#' weighting is applied after subset selection in the workflow.
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

  if (!all(c("sim_id", "likelihood") %in% names(results))) {
    stop("results must contain columns: sim_id, likelihood")
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

    # Calculate convergence metrics using uniform weights
    # At subset optimization stage, Gibbs weights haven't been calculated yet
    weights_n <- rep(1/n, n)  # Uniform weights

    # ESS
    if (ess_method == "kish") {
      ESS <- 1 / sum(weights_n^2)
    } else {  # perplexity
      ESS <- exp(-sum(weights_n * log(weights_n + 1e-300)))
    }

    # Agreement Index (A) - proportion of effective weight in top subset
    # A = ESS / n (ratio of effective to actual sample size)
    A <- ESS / n

    # Coefficient of Variation of weights (CVw)
    CVw <- sqrt(sum(weights_n^2) - (1/n)) / (1/n)

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

  # No convergence - return results at max_size
  subset_max <- results_ranked[1:max_size, ]
  weights_max <- rep(1/max_size, max_size)  # Uniform weights

  if (ess_method == "kish") {
    ESS_max <- 1 / sum(weights_max^2)
  } else {
    ESS_max <- exp(-sum(weights_max * log(weights_max + 1e-300)))
  }

  A_max <- ESS_max / max_size
  CVw_max <- sqrt(sum(weights_max^2) - (1/max_size)) / (1/max_size)

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
