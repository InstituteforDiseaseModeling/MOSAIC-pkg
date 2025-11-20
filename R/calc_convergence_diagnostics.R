#' Calculate comprehensive convergence diagnostics with status indicators
#'
#' Centralized function for computing all convergence diagnostics and status
#' indicators for BFRS calibration results. Takes metrics and targets as input,
#' calculates pass/warn/fail status for each metric using consistent thresholds,
#' and aggregates to an overall convergence status.
#'
#' @param n_total Integer total number of simulations attempted
#' @param n_successful Integer number of simulations that completed successfully
#'   (finite likelihood)
#' @param n_retained Integer number of simulations retained after outlier removal
#' @param n_best_subset Integer number of simulations in the optimized best subset
#' @param ess_best Numeric effective sample size within the best subset
#' @param A_best Numeric agreement index (entropy-based) for the best subset
#' @param cvw_best Numeric coefficient of variation of weights in the best subset
#' @param percentile_used Numeric percentile of likelihood distribution used for
#'   best subset selection (e.g., 5.0 for top 5%)
#' @param convergence_tier Character string indicating which tier criteria were
#'   used (e.g., "tier_3", "fallback")
#' @param param_ess_results Optional data frame with columns \code{parameter} and
#'   \code{ess_marginal} containing parameter-specific ESS values. If NULL,
#'   parameter ESS metrics are omitted.
#'
#' @param target_ess_best Numeric target ESS for the best subset (default 300)
#' @param target_A_best Numeric target agreement index (default 0.95)
#' @param target_cvw_best Numeric target coefficient of variation (default 0.5)
#' @param target_B_min Integer minimum acceptable size for best subset (default 100)
#' @param target_percentile_max Numeric maximum percentile for subset selection
#'   (default 5.0, meaning top 5%)
#' @param target_ess_param Numeric target ESS for individual parameters (default 300)
#' @param target_ess_param_prop Numeric target proportion of parameters that must
#'   meet ESS threshold (default 0.95, meaning 95%)
#'
#' @param ess_method Character string specifying ESS calculation method: "kish" (default)
#'   or "perplexity". This should match the method used for all ESS calculations in the
#'   workflow.
#' @param temperature Numeric temperature parameter used for weight scaling
#'   (default 1, for documentation purposes)
#' @param verbose Logical indicating whether to print diagnostic messages
#'   (default TRUE)
#'
#' @return A list with the following structure:
#' \describe{
#'   \item{settings}{List with optimization_tier, percentile_used, temperature, description}
#'   \item{targets}{List of target values with descriptions for each metric}
#'   \item{metrics}{List of metric values with descriptions and status indicators}
#'   \item{summary}{List with counts, tier info, and overall convergence status}
#' }
#'
#' The returned list is ready for JSON serialization and contains all information
#' needed for visualization by \code{\link{plot_model_convergence_status}}.
#'
#' @section Status Thresholds:
#' Status indicators are calculated using multiplicative thresholds relative to
#' targets:
#'
#' \strong{For "higher is better" metrics (ESS, Agreement):}
#' \itemize{
#'   \item \strong{PASS}: value >= target * pass_threshold
#'   \item \strong{WARN}: value >= target * warn_threshold but < pass_threshold
#'   \item \strong{FAIL}: value < target * warn_threshold
#' }
#'
#' \strong{For "lower is better" metrics (CVw):}
#' \itemize{
#'   \item \strong{PASS}: value <= target * pass_threshold
#'   \item \strong{WARN}: value <= target * warn_threshold but > pass_threshold
#'   \item \strong{FAIL}: value > target * warn_threshold
#' }
#'
#' \strong{Default thresholds by metric:}
#' \itemize{
#'   \item ESS_B: pass=0.8 (80%), warn=0.5 (50%)
#'   \item A_B: pass=0.9 (90%), warn=0.7 (70%)
#'   \item CVw_B: pass=1.2 (120%), warn=2.0 (200%)
#'   \item B_size: pass=1.0 (100%), warn=0.5 (50%)
#'   \item Percentile: pass=1.0 (100%), warn=1.5 (150%)
#'   \item Param ESS: pass=1.0 (100%), warn=0.8 (80%)
#' }
#'
#' @section Overall Status:
#' The overall convergence status is determined by aggregating individual metric
#' statuses:
#' \itemize{
#'   \item \strong{FAIL}: Any metric has status "fail"
#'   \item \strong{WARN}: No failures, but at least one metric has status "warn"
#'   \item \strong{PASS}: All metrics have status "pass"
#' }
#'
#' @examples
#' \dontrun{
#' # After running BFRS calibration and calculating metrics
#' diagnostics <- calc_convergence_diagnostics(
#'     n_total = 10000,
#'     n_successful = 9500,
#'     n_retained = 8500,
#'     n_best_subset = 500,
#'     ess_best = 280,
#'     A_best = 0.92,
#'     cvw_best = 0.55,
#'     percentile_used = 4.8,
#'     convergence_tier = "tier_3",
#'     param_ess_results = param_ess_df,
#'     target_ess_best = 300,
#'     target_A_best = 0.95,
#'     target_cvw_best = 0.5,
#'     target_B_min = 100,
#'     target_percentile_max = 5.0,
#'     ess_method = "kish"
#' )
#'
#' # Save to JSON
#' jsonlite::write_json(diagnostics, "convergence_diagnostics.json",
#'                      pretty = TRUE, auto_unbox = TRUE)
#'
#' # Check overall status
#' print(diagnostics$summary$convergence_status)
#' }
#'
#' @seealso
#'   \code{\link{plot_model_convergence_status}},
#'   \code{\link{calc_model_ess}},
#'   \code{\link{calc_model_agreement_index}}
#'
#' @family calibration-metrics
#' @export
calc_convergence_diagnostics <- function(
    # Metrics
    n_total,
    n_successful,
    n_retained,
    n_best_subset,
    ess_best,
    A_best,
    cvw_best,
    percentile_used,
    convergence_tier,
    param_ess_results = NULL,

    # Targets
    target_ess_best = 300,
    target_A_best = 0.95,
    target_cvw_best = 0.5,
    target_B_min = 100,
    target_percentile_max = 5.0,
    target_ess_param = 300,
    target_ess_param_prop = 0.95,

    # Settings
    ess_method = c("kish", "perplexity"),
    temperature = 1,
    verbose = TRUE
) {

    # ============================================================================
    # Input validation
    # ============================================================================

    stopifnot("n_total must be positive integer" =
              is.numeric(n_total) && n_total > 0)
    stopifnot("n_successful must be non-negative integer" =
              is.numeric(n_successful) && n_successful >= 0 && n_successful <= n_total)
    stopifnot("n_retained must be non-negative integer" =
              is.numeric(n_retained) && n_retained >= 0 && n_retained <= n_successful)
    stopifnot("n_best_subset must be non-negative integer" =
              is.numeric(n_best_subset) && n_best_subset >= 0 && n_best_subset <= n_retained)
    stopifnot("ess_best must be numeric" = is.numeric(ess_best))
    stopifnot("A_best must be numeric" = is.numeric(A_best))
    stopifnot("cvw_best must be numeric" = is.numeric(cvw_best))
    stopifnot("percentile_used must be numeric" = is.numeric(percentile_used))
    stopifnot("convergence_tier must be character" = is.character(convergence_tier))

    if (!is.null(param_ess_results)) {
        stopifnot("param_ess_results must be data frame" = is.data.frame(param_ess_results))
        stopifnot("param_ess_results must have 'parameter' column" =
                  "parameter" %in% names(param_ess_results))
        stopifnot("param_ess_results must have 'ess_marginal' column" =
                  "ess_marginal" %in% names(param_ess_results))
    }

    # Validate and normalize ess_method
    ess_method <- match.arg(ess_method)

    # ============================================================================
    # Calculate individual metric statuses
    # ============================================================================

    # B_size: Number of simulations in best subset
    status_B_size <- .calc_status(
        value = n_best_subset,
        target = target_B_min,
        direction = "higher",
        pass_threshold = 1.0,    # Must meet minimum
        warn_threshold = 0.5
    )

    # ESS_best: Effective sample size in best subset
    status_ess_best <- .calc_status(
        value = ess_best,
        target = target_ess_best,
        direction = "higher",
        pass_threshold = 0.8,
        warn_threshold = 0.5
    )

    # A_best: Agreement index
    status_A_best <- .calc_status(
        value = A_best,
        target = target_A_best,
        direction = "higher",
        pass_threshold = 0.9,
        warn_threshold = 0.7
    )

    # cvw_best: Coefficient of variation (lower is better)
    status_cvw_best <- .calc_status(
        value = cvw_best,
        target = target_cvw_best,
        direction = "lower",
        pass_threshold = 1.2,
        warn_threshold = 2.0
    )

    # Percentile: Should be small (concentrated selection)
    status_percentile <- .calc_percentile_status(
        percentile_val = percentile_used,
        target_max = target_percentile_max
    )

    # Parameter ESS: Proportion meeting target
    if (!is.null(param_ess_results)) {
        n_params <- nrow(param_ess_results)
        n_pass <- sum(param_ess_results$ess_marginal >= target_ess_param, na.rm = TRUE)
        pct_pass <- n_pass / n_params

        status_param_ess <- .calc_status(
            value = pct_pass,
            target = target_ess_param_prop,
            direction = "higher",
            pass_threshold = 1.0,    # Must meet proportion
            warn_threshold = 0.8
        )
    } else {
        n_params <- NA_integer_
        n_pass <- NA_integer_
        pct_pass <- NA_real_
        status_param_ess <- "info"
    }

    # ============================================================================
    # Calculate overall status
    # ============================================================================

    # Collect all statuses (exclude info-only metrics)
    all_statuses <- c(
        status_B_size,
        status_ess_best,
        status_A_best,
        status_cvw_best,
        status_percentile
    )

    # Include param_ess if available
    if (!is.na(status_param_ess) && status_param_ess != "info") {
        all_statuses <- c(all_statuses, status_param_ess)
    }

    # Aggregate: any fail → FAIL, any warn → WARN, else PASS
    overall_status <- if (any(all_statuses == "fail")) {
        "FAIL"
    } else if (any(all_statuses == "warn")) {
        "WARN"
    } else {
        "PASS"
    }

    # ============================================================================
    # Verbose output
    # ============================================================================

    if (verbose) {
        message("========================================")
        message("Convergence Diagnostics Summary")
        message("========================================")
        message("")
        message("Counts:")
        message("  Total simulations: ", format(n_total, big.mark = ","))
        message("  Successful: ", format(n_successful, big.mark = ","),
                " (", round(100 * n_successful / n_total, 1), "%)")
        message("  Retained: ", format(n_retained, big.mark = ","),
                " (", round(100 * n_retained / n_successful, 1), "%)")
        message("  Best subset: ", format(n_best_subset, big.mark = ","),
                " (", round(100 * n_best_subset / n_retained, 1), "%)")
        message("")
        message("Subset Selection:")
        message("  Convergence tier: ", convergence_tier)
        message("  Percentile used: ", round(percentile_used, 2), "%",
                " (target <= ", target_percentile_max, "%) - ", toupper(status_percentile))
        message("")
        message("Best Subset Metrics:")
        message("  Size (B): ", n_best_subset,
                " (target >= ", target_B_min, ") - ", toupper(status_B_size))
        message("  ESS_B: ", round(ess_best, 1),
                " (target >= ", target_ess_best, ") - ", toupper(status_ess_best))
        message("  A_B: ", round(A_best, 3),
                " (target >= ", target_A_best, ") - ", toupper(status_A_best))
        message("  CVw_B: ", round(cvw_best, 3),
                " (target <= ", target_cvw_best, ") - ", toupper(status_cvw_best))
        message("")
        if (!is.na(pct_pass)) {
            message("Parameter Coverage:")
            message("  Parameters meeting ESS target: ", n_pass, "/", n_params,
                    " (", round(pct_pass * 100, 1), "%)")
            message("  Target: >= ", round(target_ess_param_prop * 100, 1),
                    "% - ", toupper(status_param_ess))
            message("")
        }
        message("----------------------------------------")
        message("Overall Status: ", overall_status)
        message("----------------------------------------")
    }

    # ============================================================================
    # Build diagnostics structure
    # ============================================================================

    diagnostics <- list(
        settings = list(
            optimization_tier = convergence_tier,
            percentile_used = percentile_used,
            temperature = temperature,
            ess_method = ess_method,
            description = "BFRS convergence with post-hoc optimization"
        ),

        targets = list(
            B_min = list(
                value = target_B_min,
                description = "Minimum best subset size"
            ),
            ess_best = list(
                value = target_ess_best,
                description = "Target ESS for best subset"
            ),
            A_best = list(
                value = target_A_best,
                description = "Target agreement index"
            ),
            cvw_best = list(
                value = target_cvw_best,
                description = "Target coefficient of variation"
            ),
            percentile_max = list(
                value = target_percentile_max,
                description = "Maximum percentile for subset selection"
            ),
            ess_param = list(
                value = target_ess_param,
                description = "Target ESS for individual parameters"
            ),
            ess_param_prop = list(
                value = target_ess_param_prop,
                description = "Target proportion of parameters with adequate ESS"
            )
        ),

        metrics = list(
            # B_size - number in best subset
            B_size = list(
                value = n_best_subset,
                description = "Number of simulations in best subset",
                status = status_B_size
            ),

            # ess_best - ESS within best subset
            ess_best = list(
                value = ess_best,
                description = "Effective sample size in best subset",
                status = status_ess_best
            ),

            # A_B - agreement index
            A_B = list(
                value = A_best,
                description = "Agreement index for best subset",
                status = status_A_best
            ),

            # cvw_B - coefficient of variation
            cvw_B = list(
                value = cvw_best,
                description = "Coefficient of variation of weights in best subset",
                status = status_cvw_best
            )
        ),

        summary = list(
            # Counts
            total_simulations_original = n_total,
            n_successful = n_successful,
            retained_simulations = n_retained,

            # Subset info
            convergence_tier = convergence_tier,
            percentile_used = percentile_used,

            # Overall status
            convergence_status = overall_status
        )
    )

    # Add parameter ESS metric if available
    if (!is.null(param_ess_results)) {
        diagnostics$metrics$param_ess <- list(
            value = pct_pass,
            n_pass = n_pass,
            n_total = n_params,
            description = "Proportion of parameters with adequate ESS",
            status = status_param_ess
        )
    }

    return(diagnostics)
}


# ==============================================================================
# Helper functions
# ==============================================================================

#' Calculate status from value and target with multiplicative thresholds
#'
#' @param value Numeric value to assess
#' @param target Numeric target value
#' @param direction Character "higher" (higher is better) or "lower" (lower is better)
#' @param pass_threshold Numeric multiplicative threshold for pass status
#' @param warn_threshold Numeric multiplicative threshold for warn status
#'
#' @return Character status: "pass", "warn", or "fail"
#' @keywords internal
#' @noRd
.calc_status <- function(value, target, direction = "higher",
                        pass_threshold = 0.8, warn_threshold = 0.5) {

    # Handle non-finite values
    if (!is.finite(value)) {
        return("fail")
    }

    if (direction == "higher") {
        # Higher is better (ESS, Agreement)
        if (value >= target * pass_threshold) {
            return("pass")
        } else if (value >= target * warn_threshold) {
            return("warn")
        } else {
            return("fail")
        }
    } else if (direction == "lower") {
        # Lower is better (CVw)
        if (value <= target * pass_threshold) {
            return("pass")
        } else if (value <= target * warn_threshold) {
            return("warn")
        } else {
            return("fail")
        }
    } else {
        stop("direction must be 'higher' or 'lower'")
    }
}


#' Calculate percentile status
#'
#' @param percentile_val Numeric percentile value (e.g., 5.0 for top 5%)
#' @param target_max Numeric maximum acceptable percentile
#'
#' @return Character status: "pass", "warn", or "fail"
#' @keywords internal
#' @noRd
.calc_percentile_status <- function(percentile_val, target_max) {

    if (!is.finite(percentile_val)) {
        return("fail")
    }

    if (percentile_val <= target_max) {
        return("pass")
    } else if (percentile_val <= target_max * 1.5) {
        return("warn")
    } else {
        return("fail")
    }
}
