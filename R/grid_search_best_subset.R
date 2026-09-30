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
#' @param weighting Character best-subset weighting scheme, matching \code{control$targets$best_subset_weighting}: "saturated" (default) or "tempered"
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
#' For each candidate size n the top-n draws by likelihood are weighted, and
#' ESS, A and CVw are calculated from those weights:
#' \itemize{
#'   \item \code{"saturated"} (default): \eqn{\Delta_i = -2(\ell_i - \max \ell)}{Delta_i = -2 (ll_i - max ll)},
#'     saturated at 4, and \eqn{w_i \propto \exp(-0.5 \min(\Delta_i, 4))}{w_i ~ exp(-0.5 min(Delta_i, 4))}
#'     (MOSAIC-docs calibration chapter, equation aic-weights). Weights lie in
#'     \eqn{[e^{-2}, 1]} before normalisation. This is the scheme of the
#'     \code{weight_best} posterior, and \code{run_MOSAIC()} always selects the
#'     subset with it. Once most of the subset is past \eqn{\Delta = 4} the
#'     weights are nearly flat, so in practice the ESS target alone sets n and
#'     the A and CVw targets rarely bind.
#'   \item \code{"tempered"}: the adaptive-eta Gibbs weights that
#'     \code{run_MOSAIC()} uses for its final ESS_B/A/CVw gate when
#'     \code{best_subset_weighting = "tempered"}. They place the worst draw of
#'     the subset at a weight floor of 1e-15, so on production likelihoods the
#'     ESS stays far below typical targets and the search usually ends at
#'     \code{max_size} with \code{converged = FALSE}.
#' }
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
    weighting = c("saturated", "tempered"),
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
  weighting  <- match.arg(weighting)

  # Rank by likelihood (descending). Compute the ORDER once and slice the
  # likelihood VECTOR in the loop (the metrics use only $likelihood); the full
  # (wide) subset data.frame is materialized once, at the chosen n. This avoids
  # re-sorting/copying the full frame on every call and per-n wide-row copies.
  # Bit-identical: ll_sorted[1:n] == results_ranked[1:n, ]$likelihood, and
  # results[ord[1:n], ] == results_ranked[1:n, ] (same rows, order, row.names).
  ord       <- order(results$likelihood, decreasing = TRUE)
  ll_sorted <- results$likelihood[ord]

  # Grid search with early stopping
  n_values <- seq(min_size, max_size, by = step_size)
  evaluations <- 0

  for (n in n_values) {
    evaluations <- evaluations + 1

    # Weights on the top-n likelihoods (vector slice; no wide-frame copy),
    # using the same scheme as the final gate in run_MOSAIC().
    weights_n <- .mosaic_best_subset_weights(ll_sorted[1:n], scheme = weighting)$weights

    # Calculate metrics from the best-subset weights
    ESS <- calc_model_ess(weights_n, method = ess_method)

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
        cat(sprintf("  \u2713 Converged at n=%d after %d evaluations\n", n, evaluations))
      }

      return(list(
        n = n,
        subset = results[ord[1:n], ],   # materialize the selected rows once
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

  # No convergence - return results at max_size with the same weighting
  weights_max <- .mosaic_best_subset_weights(ll_sorted[1:max_size], scheme = weighting)$weights

  # Calculate metrics
  ESS_max <- calc_model_ess(weights_max, method = ess_method)

  w_max_unnorm <- weights_max * max_size
  ag_max <- calc_model_agreement_index(w_max_unnorm)
  A_max <- ag_max$A
  CVw_max <- calc_model_cvw(w_max_unnorm)

  if (verbose) {
    cat(sprintf("  \u2717 No convergence after %d evaluations (max_size=%d reached)\n",
                evaluations, max_size))
    cat(sprintf("    Final: ESS=%.1f (target=%.1f), A=%.3f (target=%.3f), CVw=%.3f (target=%.3f)\n",
                ESS_max, target_ESS, A_max, target_A, CVw_max, target_CVw))
  }

  return(list(
    n = max_size,
    subset = results[ord[1:max_size], ],
    metrics = list(
      ESS = ESS_max,
      A = A_max,
      CVw = CVw_max
    ),
    converged = FALSE,
    evaluations = evaluations
  ))
}


#' Best-subset weights (shared by subset selection and subset optimization)
#'
#' Computes best-subset weights for \code{grid_search_best_subset()} and
#' \code{optimize_ensemble_subset()}. "saturated" reproduces the
#' \code{weight_best} posterior (and the final gate under the default
#' \code{best_subset_weighting}); "tempered" reproduces only the final gate
#' under \code{best_subset_weighting = "tempered"}, because \code{weight_best}
#' is always saturated.
#'
#' @param likelihood Numeric vector of finite log-likelihoods for the subset.
#' @param scheme "saturated" (\eqn{w \propto \exp(-0.5 \min(\Delta, 4))}) or
#'   "tempered" (adaptive-eta Gibbs weights).
#' @return List with \code{weights} (normalised, same order as
#'   \code{likelihood}) and \code{temperature} (the eta applied).
#' @keywords internal
#' @noRd
.mosaic_best_subset_weights <- function(likelihood, scheme = c("saturated", "tempered")) {
  scheme <- match.arg(scheme)
  if (identical(scheme, "tempered")) {
    res <- .mosaic_calc_adaptive_gibbs_weights(likelihood = likelihood, verbose = FALSE)
    return(list(weights = res$weights, temperature = res$temperature))
  }
  aic   <- -2 * likelihood
  delta <- aic - min(aic)
  list(weights = calc_model_weights_gibbs(x = pmin(delta, 4.0), eta = 0.5, verbose = FALSE),
       temperature = 0.5)
}
