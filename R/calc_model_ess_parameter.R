#' Calculate Parameter-Specific ESS
#'
#' Computes the effective sample size (ESS) of each parameter's weighted
#' posterior mean: a function-specific, variance-ratio (delta-method) ESS of
#' the weights, bounded above by their Kish ESS. \code{run_MOSAIC()} reports it in
#' \code{parameter_ess.csv} and the auto-mode stopping rule reads it.
#'
#' @details
#' For parameter \eqn{\theta} with normalised weights \eqn{\tilde w_i},
#' posterior mean \eqn{\tilde\mu = \sum_i \tilde w_i \theta_i} and posterior
#' variance \eqn{\tilde\sigma^2 = \sum_i \tilde w_i (\theta_i - \tilde\mu)^2},
#' the delta-method variance of the self-normalised estimate \eqn{\tilde\mu} is
#' \eqn{\sum_i \tilde w_i^2 (\theta_i - \tilde\mu)^2} (Owen 2013, eq. 9.9). This
#' is a variance ratio, the definition behind Owen's \eqn{n_e} (eq. 9.13); it is
#' not the weight-based \eqn{n_e(f)} of Owen's eq. 9.17, which uses weights
#' proportional to \eqn{w_i |f_i|}.
#' The ESS is the number of independent posterior draws that would give the
#' same precision:
#' \deqn{\mathrm{ESS}_\theta = \frac{\sum_i \tilde w_i (\theta_i - \tilde\mu)^2}
#'       {\sum_i \tilde w_i^2 (\theta_i - \tilde\mu)^2}.}
#' It is computed directly from the weights, with no bandwidth or grid, and is
#' bounded above by the Kish ESS of the weights, \eqn{(\sum w)^2/\sum w^2}.
#' The bound is needed because both variances are estimated from the same
#' weights: when one weight dominates, the unbounded ratio degenerates upward.
#' For example, two draws weighted 0.99 and 0.01 give 50.5, and a near point
#' mass on a 50,000-draw run gives close to \eqn{n}. The bounded value is
#' exactly \eqn{n} for uniform weights and about 1 for a point mass, and it
#' falls as the weights concentrate. It is lower than the Kish ESS for a
#' parameter whose heavy weights sit in its tails. When the weighted spread is
#' zero (a point mass, or a parameter constant over the weighted draws), the
#' Kish ESS is returned.
#'
#' The weights are \code{weights} when supplied. \code{run_MOSAIC()} passes the
#' weights of the ensemble members the posterior is built from (zero outside
#' them), so \code{parameter_ess.csv} is a diagnostic of the written posterior
#' (since MOSAIC 1.4.0; it gates nothing). Otherwise they are the raw
#' importance weights \eqn{w_i \propto \exp(\ell_i - \max \ell)}, which on
#' production-scale likelihoods collapse to a point mass.
#'
#' Until MOSAIC 1.3.0 this function returned a grid estimate (a weighted KDE or
#' binned marginal rescaled by the number of draws). That estimate did not
#' measure the posterior: its bandwidth came from the unweighted draws, so a
#' point mass on a randomly chosen draw of a 50,000-draw production run scored a
#' median of 2,518 against 2,541 for the run's own weights. It was removed, with
#' its arguments \code{n_bins}, \code{n_grid}, \code{method} and
#' \code{marginal_method}; passing any of them now warns and is otherwise
#' ignored.
#'
#' @param results Data frame containing simulation results
#' @param param_names Character vector of parameter names to analyze (required)
#' @param likelihood_col Character name of the column containing log-likelihood values (default: "likelihood")
#' @param weights Optional non-negative weights, one per row of \code{results}.
#'   \code{NULL} (default) uses the raw importance weights
#'   \eqn{\exp(\ell - \max \ell)}.
#' @param verbose Logical whether to print progress messages (default: FALSE)
#' @param ... Defunct arguments of the pre-1.3.0 grid estimate (\code{n_bins},
#'   \code{n_grid}, \code{method}, \code{marginal_method}); a warning is given
#'   and they are ignored. Any other argument is an error.
#'
#' @return Data frame with columns:
#'   - parameter: Parameter name
#'   - type: Parameter type ('global' or 'location')
#'   - iso_code: ISO code for location parameters (NA for global)
#'   - ess_marginal: Function-specific ESS of the parameter's posterior mean
#'
#' @references
#' Owen AB (2013). \emph{Monte Carlo theory, methods and examples}, chapter 9
#' (importance sampling), eqs. 9.9 and 9.13. \url{https://artowen.su.domains/mc/}
#'
#' @examples
#' \dontrun{
#' # Function-specific ESS under the raw importance weights
#' ess_results <- calc_model_ess_parameter(
#'   results = simulation_results,
#'   param_names = c("tau_i", "beta_j0_tot", "gamma_2")
#' )
#'
#' # Under explicit posterior weights (e.g. the coarsened posterior)
#' ess_results <- calc_model_ess_parameter(
#'   results = simulation_results,
#'   param_names = c("tau_i", "beta_j0_tot", "gamma_2"),
#'   weights = simulation_results$weight_all
#' )
#' }
#'
#' @export
calc_model_ess_parameter <- function(
    results,
    param_names,
    likelihood_col = "likelihood",
    weights = NULL,
    verbose = FALSE,
    ...
) {

    dots <- list(...)
    if (length(dots)) {
        defunct <- c("n_bins", "n_grid", "method", "marginal_method")
        nm <- names(dots)
        if (is.null(nm) || any(!nzchar(nm)) || !all(nm %in% defunct)) {
            bad <- if (is.null(nm)) "(unnamed)" else paste(setdiff(nm, defunct), collapse = ", ")
            stop("calc_model_ess_parameter(): unused argument(s): ", bad, call. = FALSE)
        }
        warning(sprintf(paste0("calc_model_ess_parameter(): argument(s) %s are defunct since ",
                               "MOSAIC 1.3.0 and were ignored. The grid-based marginal ESS they ",
                               "configured was removed; the function-specific ESS needs no grid."),
                        paste(nm, collapse = ", ")), call. = FALSE)
    }

    # ==========================================================================
    # Input validation
    # ==========================================================================

    if (!is.data.frame(results)) {
        stop("results must be a data frame")
    }

    if (!is.character(likelihood_col) || length(likelihood_col) != 1) {
        stop("likelihood_col must be a single character string")
    }

    if (!likelihood_col %in% colnames(results)) {
        stop(sprintf("Column '%s' not found in results", likelihood_col))
    }

    if (missing(param_names) || is.null(param_names)) {
        stop("param_names is required. Please specify which parameters to analyze.")
    }

    if (!is.character(param_names)) {
        stop("param_names must be a character vector")
    }

    if (length(param_names) == 0) {
        stop("param_names cannot be empty")
    }

    if (!is.logical(verbose)) {
        stop("verbose must be logical (TRUE/FALSE)")
    }

    # Filter to valid rows
    valid_rows <- !is.na(results[[likelihood_col]]) & is.finite(results[[likelihood_col]])
    n_valid <- sum(valid_rows)

    if (n_valid < 50) {
        stop(sprintf("Insufficient valid samples: %d (need at least 50 for ESS calculation)",
                     n_valid))
    }

    if (!is.null(weights)) {
        weights <- as.numeric(weights)
        if (length(weights) != nrow(results))
            stop("weights must have one entry per row of results")
        if (any(weights < 0, na.rm = TRUE)) stop("weights must be non-negative")
        weights <- weights[valid_rows]
        weights[!is.finite(weights)] <- 0
        if (sum(weights) <= 0) stop("weights sum to zero over the valid rows")
    }

    results_valid <- results[valid_rows, , drop = FALSE]

    # ==========================================================================
    # Validate parameters exist
    # ==========================================================================

    missing_params <- setdiff(param_names, colnames(results_valid))
    if (length(missing_params) > 0) {
        stop(sprintf("Parameters not found in results: %s", paste(missing_params, collapse = ", ")))
    }

    non_numeric_params <- param_names[!vapply(results_valid[param_names], is.numeric, logical(1))]
    if (length(non_numeric_params) > 0) {
        stop(sprintf("Parameters must be numeric. Non-numeric parameters found: %s",
                     paste(non_numeric_params, collapse = ", ")))
    }

    # Only parameters estimated by the MOSAIC framework; compartment counts
    # (N_j_initial, S_j_initial, ...) are derived from the sampled proportions.
    est_params <- get("estimated_parameters", envir = asNamespace("MOSAIC"))
    base_of <- gsub("_[A-Z]{3}$", "", param_names)
    param_names_filtered <- param_names[base_of %in% est_params$parameter_name]
    param_names_filtered <- param_names_filtered[!grepl("^[NSEIRV][12]?_j_initial", param_names_filtered)]

    if (length(param_names_filtered) == 0) {
        stop("No valid estimated parameters found in param_names. Check that parameters match those in MOSAIC::estimated_parameters")
    }

    if (verbose && length(param_names_filtered) < length(param_names)) {
        excluded <- setdiff(param_names, param_names_filtered)
        log_msg("Excluded %d non-estimated parameters: %s",
                length(excluded),
                if (length(excluded) <= 5) paste(excluded, collapse = ", ")
                else paste(c(utils::head(excluded, 5), "..."), collapse = ", "))
    }
    param_names <- param_names_filtered

    # ==========================================================================
    # Weights: supplied, or the raw importance weights
    # ==========================================================================

    if (is.null(weights)) {
        ll <- results_valid[[likelihood_col]]
        weights <- exp(ll - max(ll))
    }

    # ==========================================================================
    # Per parameter
    # ==========================================================================

    loc_params <- est_params$parameter_name[est_params$scale == "location"]
    ess_list <- lapply(param_names, function(param_name) {
        has_iso <- grepl("_[A-Z]{3}$", param_name)
        base_param <- gsub("_[A-Z]{3}$", "", param_name)
        data.frame(
            parameter = param_name,
            type = if (has_iso && base_param %in% loc_params) "location" else "global",
            iso_code = if (has_iso) gsub(".*_([A-Z]{3})$", "\\1", param_name) else NA_character_,
            ess_marginal = .mosaic_function_specific_ess(results_valid[[param_name]], weights),
            stringsAsFactors = FALSE
        )
    })
    ess_results <- do.call(rbind, ess_list)

    if (verbose) {
        ok <- is.finite(ess_results$ess_marginal)
        log_msg("Parameters analyzed: %d | median ESS %.1f | min ESS %.1f",
                nrow(ess_results),
                if (any(ok)) stats::median(ess_results$ess_marginal[ok]) else NA_real_,
                if (any(ok)) min(ess_results$ess_marginal[ok]) else NA_real_)
    }

    ess_results
}

# Variance-ratio (delta-method) ESS of a weighted posterior mean: posterior
# variance over the delta-method variance of the self-normalised mean (Owen
# 2013, eq. 9.9; not the weight-based n_e(f) of eq. 9.17), bounded above by
# the Kish ESS of the weights. The bound matters: both
# variances are estimated from the same weights, and when one weight dominates
# the ratio degenerates upward (two draws weighted 0.99 / 0.01 give 50.5; a
# production near-point mass whose runner-up sits 34 log units behind gives
# ~n). Kish ESS when the weighted spread is zero; never below 1. NA when no
# draw has a finite value and positive weight.
.mosaic_function_specific_ess <- function(x, w) {
    ok <- is.finite(x) & is.finite(w) & w > 0
    if (!any(ok)) return(NA_real_)
    x <- x[ok]; w <- w[ok] / sum(w[ok])
    kish <- 1 / sum(w^2)
    m <- sum(w * x)
    d2 <- (x - m)^2
    num <- sum(w * d2); den <- sum(w^2 * d2)
    if (!(num > 0) || !(den > 0)) return(kish)
    max(1, min(kish, num / den))
}
