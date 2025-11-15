#' Identify Best Subset of Simulations for NPE Prior Definition
#'
#' Uses binary search on percentile to find the smallest subset of top simulations
#' (by likelihood) that meets convergence criteria for ESS_B, A, CVw, and minimum size.
#' This subset will be used to define priors for Neural Posterior Estimation (NPE).
#'
#' @param results Data frame containing simulation results with columns:
#'   - sim: Simulation ID
#'   - likelihood: Log-likelihood values
#'   - Other columns containing parameter values
#' @param min_B Integer minimum number of simulations to include even if percentile is smaller (default: 100)
#' @param target_ESS_B Numeric target minimum ESS for the subset using Kish formula (default: 50)
#' @param target_A Numeric target minimum agreement index (default: 0.95)
#' @param target_CVw Numeric target maximum coefficient of variation (default: 0.5)
#' @param min_percentile Numeric minimum percentile to consider (default: 0.001)
#'   The search will not consider subsets smaller than this percentile.
#'   E.g., 0.001 means at least the top 0.001% will be considered.
#'   Note: The actual minimum subset size is max(min_percentile%, min_B simulations).
#' @param max_percentile Numeric maximum percentile to consider (default: 30)
#'   The search will not consider subsets larger than this percentile.
#'   E.g., 30 means at most the top 30% will be considered.
#' @param precision Numeric precision for binary search convergence as a percentage (default: 0.1)
#'   The search stops when the range is smaller than this value.
#' @param verbose Logical whether to print progress messages (default: FALSE)
#'
#' @return A list containing:
#'   - subset: Data frame of selected simulations
#'   - metrics: Final convergence metrics (ESS_B, A, CVw, B_size)
#'   - converged: Logical indicating if all criteria were met
#'   - n_selected: Number of simulations in final subset
#'   - percentile_used: Final percentile of simulations selected
#'
#' @examples
#' \dontrun{
#' # Identify best subset from calibration results
#' best_subset <- identify_best_subset(
#'   results = calibration_results,
#'   min_B = 100,
#'   target_ESS_B = 50,
#'   target_A = 0.95,
#'   target_CVw = 0.5,
#'   min_percentile = 0.001,  # Search down to top 0.001%
#'   max_percentile = 30,     # Don't consider more than top 30%
#'   verbose = TRUE
#' )
#'
#' # Use subset for NPE prior definition
#' npe_priors <- estimate_priors_from_subset(best_subset$subset)
#' }
#'
#' @export
identify_best_subset <- function(
    results,
    min_B = 100,
    target_ESS_B = 50,
    target_A = 0.95,
    target_CVw = 0.5,
    min_percentile = 0.001,
    max_percentile = 30,
    precision = 0.1,
    verbose = FALSE
) {

    # ==========================================================================
    # Input validation
    # ==========================================================================

    if (!is.data.frame(results)) {
        stop("results must be a data frame")
    }

    if (!"likelihood" %in% colnames(results)) {
        stop("results must contain a 'likelihood' column")
    }

    if (!"sim" %in% colnames(results)) {
        stop("results must contain a 'sim' column")
    }

    # Filter to valid likelihoods
    valid_rows <- is.finite(results$likelihood)
    if (sum(valid_rows) < min_B) {
        stop(sprintf("Insufficient valid simulations: %d (need at least %d)",
                     sum(valid_rows), min_B))
    }

    results_valid <- results[valid_rows, ]

    if (verbose) {
        log_msg("Identifying best subset from %d valid simulations", nrow(results_valid))
        log_msg("Criteria: ESS_B>=%.1f, A>=%.3f, CVw<=%.3f",
                target_ESS_B, target_A, target_CVw)
        log_msg("Search range: %.3f%% to %.1f%%",
                min_percentile, max_percentile)
    }

    # ==========================================================================
    # Rank simulations by likelihood
    # ==========================================================================

    results_ranked <- results_valid[order(results_valid$likelihood, decreasing = TRUE), ]
    n_total <- nrow(results_ranked)

    # ==========================================================================
    # Helper function to evaluate metrics for a given percentile
    # ==========================================================================

    evaluate_subset <- function(percentile) {
        n_subset <- max(min_B, ceiling(n_total * percentile / 100))
        n_subset <- min(n_subset, n_total)  # Can't exceed total

        current_subset <- results_ranked[1:n_subset, ]

        # Calculate AIC for subset
        aic_subset <- -2 * current_subset$likelihood

        # RECALCULATE delta AIC within subset (reset baseline to best in subset)
        best_aic_in_subset <- min(aic_subset)
        delta_subset <- aic_subset - best_aic_in_subset

        # Dynamic Gibbs temperature scaling based on actual delta AIC range
        # Use continuous function for effective range
        effective_range <- get_effective_aic_range(percentile)
        actual_range <- diff(range(delta_subset))

        # Avoid division by zero if all models have same likelihood
        if (actual_range < .Machine$double.eps) {
            gibbs_temperature <- 1  # Default to standard if no variation
        } else {
            # Scale to get similar discrimination as standard Akaike
            gibbs_temperature <- 0.5 * (effective_range / actual_range)
        }

        # Calculate weights with Gibbs tempering
        # Requires calc_model_weights_gibbs from MOSAIC package
        weights <- calc_model_weights_gibbs(
            x = delta_subset,
            temperature = gibbs_temperature,
            verbose = FALSE
        )
        w_tilde <- weights  # Already normalized
        w <- weights * length(weights)  # Unnormalized for consistency with other functions

        # Calculate all metrics using Kish formula for ESS
        ESS_B <- calc_model_ess(w_tilde, method = "kish")
        ag <- calc_model_agreement_index(w)
        A <- ag$A
        CVw <- calc_model_cvw(w)
        B_size <- n_subset

        metrics <- list(
            ESS_B = ESS_B,
            A = A,
            CVw = CVw,
            B_size = B_size,
            gibbs_temperature = gibbs_temperature
        )

        # Check if all criteria are met (min_B ensures minimum size but is not a criterion)
        criteria_met <- (
            ESS_B >= target_ESS_B &&
            A >= target_A &&
            CVw <= target_CVw
        )

        return(list(
            subset = current_subset,
            metrics = metrics,
            criteria_met = criteria_met,
            percentile = percentile
        ))
    }

    # ==========================================================================
    # Binary search for optimal percentile
    # ==========================================================================

    # Calculate search range
    # Minimum percentile is either user-specified (default 0.001%) or whatever gives min_B simulations
    # whichever is LARGER (to ensure we have at least min_B simulations)
    min_percentile_for_size <- ceiling(100 * min_B / n_total)
    search_min_percentile <- max(min_percentile, min_percentile_for_size)

    # Maximum percentile is user-specified (default 30%)
    # This prevents searching unreasonably large subsets
    search_max_percentile <- min(max_percentile, 100)

    # Track best valid result
    best_valid_result <- NULL

    if (verbose) {
        log_msg("Starting binary search for optimal subset (n_total=%d, min_B=%d)", n_total, min_B)
        log_msg("Search range: %.3f%% to %.1f%% (min_percentile=%.3f%%, enforcing min_B=%d)",
                search_min_percentile, search_max_percentile, min_percentile, min_B)
        log_msg("Using adaptive effective AIC range for Gibbs tempering")
    }

    # Binary search with maximum iterations to prevent infinite loops
    max_iterations <- ceiling(log2(100 / precision)) + 5  # Theoretical max + buffer
    iteration <- 0

    # First test the minimum possible percentile
    initial_test <- evaluate_subset(search_min_percentile)
    if (initial_test$criteria_met) {
        if (verbose) {
            log_msg("Minimum percentile %.3f%% already meets all criteria!", search_min_percentile)
            log_msg("  Returning minimum subset of %d simulations", initial_test$metrics$B_size)
        }
        return(list(
            subset = initial_test$subset,
            metrics = initial_test$metrics,
            converged = TRUE,
            n_selected = initial_test$metrics$B_size,
            percentile_used = search_min_percentile
        ))
    }

    if (verbose) {
        log_msg("Binary search precision: %.3f%% (max %d iterations)", precision, max_iterations)
        log_msg("Minimum percentile %.3f%% did NOT meet criteria, searching larger subsets", search_min_percentile)
    }

    while (search_min_percentile <= search_max_percentile && iteration < max_iterations) {
        iteration <- iteration + 1

        # Try middle percentile
        mid_percentile <- (search_min_percentile + search_max_percentile) / 2

        result <- evaluate_subset(mid_percentile)

        if (verbose && iteration <= 15) {
            criteria_status <- ifelse(result$criteria_met, "PASS", "FAIL")
            log_msg("  Iteration %d: Testing %.3f%% (n=%d) - %s [ESS_B=%.1f, A=%.3f, CVw=%.3f, T=%.4f]",
                    iteration, mid_percentile, result$metrics$B_size, criteria_status,
                    result$metrics$ESS_B, result$metrics$A, result$metrics$CVw,
                    result$metrics$gibbs_temperature)
        }

        if (result$criteria_met) {
            # All criteria met - store this result and try smaller subset
            if (is.null(best_valid_result) || result$metrics$B_size < best_valid_result$metrics$B_size) {
                best_valid_result <- result
                if (verbose) {
                    log_msg("    -> New best: %.3f%% (%d simulations)",
                            mid_percentile, result$metrics$B_size)
                }
            }
            # Continue searching for even smaller subset
            search_max_percentile <- mid_percentile - precision
        } else {
            # Criteria not met - need larger subset
            search_min_percentile <- mid_percentile + precision
        }

        # Stop if we've converged to within specified precision
        if (search_max_percentile < search_min_percentile) {
            if (verbose) {
                log_msg("  Converged: search complete (min > max)")
            }
            break
        }

        if (abs(search_max_percentile - search_min_percentile) < precision) {
            if (verbose) {
                log_msg("  Converged: search range %.3f%% (< precision %.3f%%)",
                        abs(search_max_percentile - search_min_percentile), precision)
            }
            break
        }
    }

    # ==========================================================================
    # Return best result or full dataset if no valid subset found
    # ==========================================================================

    if (!is.null(best_valid_result)) {
        if (verbose) {
            log_msg("Optimal subset found: %.1f%% (%d simulations)",
                    best_valid_result$percentile,
                    best_valid_result$metrics$B_size)
            log_msg("  Final metrics: ESS_B=%.1f, A=%.3f, CVw=%.3f",
                    best_valid_result$metrics$ESS_B,
                    best_valid_result$metrics$A,
                    best_valid_result$metrics$CVw)
            log_msg("  Gibbs temperature used: %.6f (effective range %.1f, actual range %.1f)",
                    best_valid_result$metrics$gibbs_temperature,
                    6.0,  # effective_range is hardcoded as 6
                    6.0 / (2 * best_valid_result$metrics$gibbs_temperature))  # Back-calculate actual range
        }

        return(list(
            subset = best_valid_result$subset,
            metrics = best_valid_result$metrics,
            converged = TRUE,
            n_selected = best_valid_result$metrics$B_size,
            percentile_used = best_valid_result$percentile
        ))
    }

    # No valid subset found within search range
    if (verbose) {
        log_msg("WARNING: No subset meeting all criteria found within top %.1f%%",
                max_percentile)
        log_msg("Trying top %.1f%% as fallback",
                max_percentile)
    }

    # Return the maximum allowed percentile as fallback
    final_result <- evaluate_subset(max_percentile)
    final_metrics <- final_result$metrics

    # Report details about full dataset metrics
    if (verbose) {
        log_msg("Full dataset metrics: ESS_B=%.1f (target>=%.1f), A=%.3f (target>=%.3f), CVw=%.3f (target<=%.3f)",
                final_metrics$ESS_B, target_ESS_B,
                final_metrics$A, target_A,
                final_metrics$CVw, target_CVw)
        log_msg("  Gibbs temperature: %.6f", final_metrics$gibbs_temperature)

        # Report which criteria were not met
        unmet <- c()
        if (final_metrics$ESS_B < target_ESS_B) unmet <- c(unmet, sprintf("ESS_B (%.1f < %.1f)", final_metrics$ESS_B, target_ESS_B))
        if (final_metrics$A < target_A) unmet <- c(unmet, sprintf("A (%.3f < %.3f)", final_metrics$A, target_A))
        if (final_metrics$CVw > target_CVw) unmet <- c(unmet, sprintf("CVw (%.3f > %.3f)", final_metrics$CVw, target_CVw))

        if (length(unmet) > 0) {
            log_msg("  Unmet criteria: %s", paste(unmet, collapse = ", "))
            log_msg("  Consider adjusting targets or running more simulations")
        }
    }

    return(list(
        subset = final_result$subset,
        metrics = final_metrics,
        converged = FALSE,
        n_selected = final_metrics$B_size,
        percentile_used = 100
    ))
}