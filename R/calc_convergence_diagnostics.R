#' Calculate comprehensive convergence diagnostics with status indicators
#'
#' Assembles the convergence diagnostics of a BFRS calibration
#' (\code{2_calibration/diagnostics/convergence_diagnostics.json}). Since MOSAIC
#' 1.4.0 the verdict is the simulation-convergence status of
#' \code{\link{calc_convergence_status}} (green / yellow / red), carried by
#' \code{weight_health}; \code{summary$convergence_status} mirrors
#' \code{summary.json} for one release, and new consumers read
#' \code{3_results/calibration_diagnostics.json}. Under the legacy
#' \code{"saturated"} / \code{"tempered"} schemes the tier-searched best
#' subset is also reported, with its pass/warn/fail statuses.
#'
#' @param n_total Integer total number of simulations attempted
#' @param n_successful Integer number of simulations that completed successfully
#'   (finite likelihood)
#' @param n_retained Integer number of simulations retained after outlier removal
#' @param n_best_subset Integer number of simulations in the optimized best subset
#' @param ess_best Numeric effective sample size within the best subset
#' @param A_best Numeric agreement index (entropy-based) for the best subset
#' @param cvw_best Numeric coefficient of variation of weights in the best subset
#' @param percentile_used Numeric size of the best subset as a percentage of all
#'   \code{n_total} draws (e.g., 5.0 for the top 5%)
#' @param convergence_tier Character string indicating which tier criteria were
#'   used (e.g., "tier_3", "fallback")
#' @param param_ess_results Optional data frame with columns \code{parameter} and
#'   \code{ess} (or the deprecated \code{ess_marginal}) containing
#'   parameter-specific ESS values. If NULL, parameter ESS metrics are omitted.
#'   Under the 1.4.0 status (\code{weight_health} supplied) the per-parameter
#'   ESS is a diagnostic (status \code{"info"}); only a standalone legacy call
#'   gates it.
#'
#' @param target_ess_best Numeric target for both subset size and ESS (default 300).
#'   Both B_size and ESS_B must be >= target_ess_best.
#' @param target_A_best Numeric target agreement index (default 0.95)
#' @param target_cvw_best Numeric target coefficient of variation (default 0.5)
#' @param target_max_best_subset Numeric maximum best subset size in absolute count
#'   (default 1000). Replaced target_percentile_max for absolute count-based control.
#' @param target_ess_param Numeric target ESS for individual parameters (default
#'   300); used only by a standalone legacy call (no \code{weight_health}).
#' @param target_ess_param_prop Numeric target proportion of parameters that must
#'   meet ESS threshold (default 0.95); standalone legacy calls only.
#'
#' @param ess_method Character string specifying ESS calculation method: "kish" (default)
#'   or "perplexity". This should match the method used for all ESS calculations in the
#'   workflow.
#' @param temperature Numeric temperature parameter used for weight scaling
#'   (default 1, for documentation purposes)
#' @param is_diagnostics Optional list of \eqn{\eta = 1} diagnostics from
#'   \code{\link{calc_is_diagnostics}}, with elements \code{best} and
#'   \code{all} (and \code{optimized} when the subset optimiser ran). Their
#'   ESS and positive-ratio counts are reported under
#'   \code{$likelihood_concentration} with an explanatory note and are never
#'   gated: they describe the untempered likelihood, which collapses onto one
#'   draw for any informative stochastic likelihood whatever the fit.
#' @param weight_health Optional result of \code{\link{calc_weight_health}}
#'   for the members the posterior is built from (\code{run_MOSAIC()} passes
#'   it under every scheme). When it carries a convergence status, that status
#'   is the verdict: \code{summary$convergence_status} is \code{"green"},
#'   \code{"yellow"} or \code{"red"} (mirroring \code{summary.json}),
#'   \code{$status} holds the label, reasons, flags and the four checks, and
#'   \code{$metrics} the ESS_all, ensemble ESS, Monte Carlo error, k-hat and
#'   informativeness rows. Required under \code{weighting = "bestfit"} or
#'   \code{"coarsened"}.
#' @param weighting Posterior weighting scheme the inputs come from
#'   (\code{control$targets$posterior_weighting}): \code{"saturated"} (default,
#'   the pre-1.3.0 behaviour) or \code{"tempered"} also report the
#'   tier-searched best subset (B_size, its cap, ESS_B, A_B, CV_B) with the
#'   legacy PASS / WARN / FAIL verdict, which becomes \code{summary$tier_status}
#'   when \code{weight_health} supplies the 1.4.0 status (and is
#'   \code{summary$convergence_status} otherwise, for a standalone legacy
#'   call). \code{"bestfit"} and \code{"coarsened"} report no tier rows.
#' @param target_batch_ratio_max Deprecated since MOSAIC 1.4.0 and ignored: the
#'   seed-batch-ratio rule was retired in favour of the Monte Carlo error bands
#'   of \code{\link{calc_convergence_status}}.
#' @param verbose Logical indicating whether to print diagnostic messages
#'   (default TRUE)
#'
#' @return A list with the following structure:
#' \describe{
#'   \item{settings}{Weighting scheme, eta, coarsening alpha, ESS method,
#'     description (plus optimization_tier, percentile_used and temperature
#'     under the legacy schemes)}
#'   \item{targets}{Target values with descriptions}
#'   \item{metrics}{Metric values with descriptions and statuses: ess_all,
#'     ess_best, mce, khat and informativeness (1.4.0 status colours), the
#'     legacy tier rows under "saturated" / "tempered", and param_ess}
#'   \item{summary}{Counts and the overall convergence status (plus the
#'     legacy tier verdict \code{tier_status})}
#'   \item{status}{(1.4.0) status, label, reasons, flags and the four checks}
#'   \item{likelihood_concentration, weight_health}{When supplied}
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
#'   \item ESS_B (target \code{target_ess_best}): pass=1.0 (100%), warn=0.8 (80%)
#'   \item A_B: pass=0.9 (90%), warn=0.7 (70%)
#'   \item CVw_B: pass=1.2 (120%), warn=2.0 (200%)
#'   \item B_size (lower bound, target \code{target_ess_best}): pass=1.0 (100%), warn=0.5 (50%)
#'   \item B_size_upper (upper cap, target \code{target_max_best_subset}, lower is
#'     better): pass when \code{n_best_subset <= target}, warn when it exceeds the
#'     cap by at most 20%, fail beyond that. \code{run_MOSAIC()} enforces the cap
#'     when it selects the subset, so in a normal run this is an invariant check.
#'   \item Param ESS: pass=1.0 (100%), warn=0.8 (80%)
#' }
#'
#' The subset percentile (\code{percentile_used} against
#' \code{target_max_best_subset / n_total * 100}) carries the same information
#' as B_size_upper on the same denominator, so its status is reported in
#' \code{summary$percentile_status} but is not gated.
#'
#' The function defaults for the targets (300 / 0.95 / 0.5) are not the
#' \code{run_MOSAIC()} control defaults (\code{ESS_best = 100},
#' \code{A_best = 0.70}, \code{CVw_best = 1.0}); \code{run_MOSAIC()} always
#' passes its targets explicitly.
#'
#' @section Overall Status:
#' With \code{weight_health} carrying a status, \code{summary$convergence_status}
#' is that status (\code{"green"}, \code{"yellow"} or \code{"red"}: the worst
#' of the ensemble-ESS, Monte Carlo error, k-hat and informativeness checks).
#' A standalone legacy call (\code{"saturated"} / \code{"tempered"}, no
#' \code{weight_health}) aggregates the tier statuses and the per-parameter
#' ESS proportion instead:
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
#'     target_max_best_subset = 1000,
#'     ess_method = "kish"
#' )
#'
#' # Save to JSON. digits = NA preserves full numerical precision;
#' # the default digits = 4 silently rounds small diagnostic values
#' # (p-values, ESS ratios, etc.) to zero.
#' jsonlite::write_json(diagnostics, "convergence_diagnostics.json",
#'                      pretty = TRUE, auto_unbox = TRUE, digits = NA)
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
    target_max_best_subset = 1000,  # Changed from target_percentile_max
    target_ess_param = 300,
    target_ess_param_prop = 0.95,

    # Settings
    ess_method = c("kish", "perplexity"),
    temperature = 1,
    is_diagnostics = NULL,
    weight_health = NULL,
    weighting = c("saturated", "tempered", "coarsened", "bestfit"),
    target_batch_ratio_max = 0.2,
    verbose = TRUE
) {

    weighting <- match.arg(weighting)
    legacy_tier <- weighting %in% c("saturated", "tempered")
    conv <- weight_health$convergence
    has_status <- !is.null(conv) && !is.null(conv$status)
    if (!legacy_tier && !has_status)
        stop("calc_convergence_diagnostics(): weighting = \"", weighting, "\" needs weight_health ",
             "carrying the convergence status (calc_weight_health()).", call. = FALSE)

    # ============================================================================
    # Input validation
    # ============================================================================

    stopifnot("n_total must be positive integer" =
              is.numeric(n_total) && n_total > 0)
    stopifnot("n_successful must be non-negative integer" =
              is.numeric(n_successful) && n_successful >= 0 && n_successful <= n_total)
    stopifnot("n_retained must be non-negative integer" =
              is.numeric(n_retained) && n_retained >= 0 && n_retained <= n_successful)
    # Under "bestfit" / "coarsened" the members come from every valid draw, not
    # from the retained (fence-passing) ones, so they are bounded by n_successful.
    stopifnot("n_best_subset must be non-negative integer" =
              is.numeric(n_best_subset) && n_best_subset >= 0 &&
              n_best_subset <= (if (legacy_tier) n_retained else n_successful))
    stopifnot("ess_best must be numeric" = is.numeric(ess_best))
    stopifnot("A_best must be numeric" = is.numeric(A_best))
    stopifnot("cvw_best must be numeric" = is.numeric(cvw_best))
    stopifnot("percentile_used must be numeric" = is.numeric(percentile_used))
    stopifnot("convergence_tier must be character" = is.character(convergence_tier))

    if (!is.null(param_ess_results)) {
        stopifnot("param_ess_results must be data frame" = is.data.frame(param_ess_results))
        stopifnot("param_ess_results must have 'parameter' column" =
                  "parameter" %in% names(param_ess_results))
        stopifnot("param_ess_results must have an 'ess' (or 'ess_marginal') column" =
                  any(c("ess", "ess_marginal") %in% names(param_ess_results)))
        param_ess_results$ess <- param_ess_results$ess %||% param_ess_results$ess_marginal
    }

    # Validate and normalize ess_method
    ess_method <- match.arg(ess_method)

    # ============================================================================
    # Calculate individual metric statuses
    # ============================================================================

    # B_size: Number of simulations in best subset (must be >= ESS_best)
    status_B_size <- .calc_status(
        value = n_best_subset,
        target = target_ess_best,
        direction = "higher",
        pass_threshold = 1.0,    # Must meet minimum
        warn_threshold = 0.5
    )

    # B_size upper bound: Must not exceed max_best_subset
    status_B_size_upper <- .calc_status(
        value = n_best_subset,
        target = target_max_best_subset,  # Direct comparison (absolute count)
        direction = "lower",              # Must be BELOW target
        pass_threshold = 1.0,             # Must not exceed limit
        warn_threshold = 1.2              # Warn if the cap is exceeded by up to 20%
    )

    # ESS_best: Effective sample size in best subset
    status_ess_best <- .calc_status(
        value = ess_best,
        target = target_ess_best,
        direction = "higher",
        pass_threshold = 1.0,
        warn_threshold = 0.8
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

    # Percentile: the subset as a percentage of ALL draws. percentile_used is
    # n_best / n_total * 100 (run_MOSAIC()), so the cap is converted on the same
    # denominator. On that denominator the check is equivalent to B_size_upper,
    # so it is reported (summary$percentile_status) but not gated. When the
    # absolute cap exceeds n_total the ratio exceeds 100%; clamp it so the
    # displayed target stays sensible and flag that the cap is non-binding.
    target_percentile_max_raw <- (target_max_best_subset / n_total) * 100
    target_percentile_max_derived <- min(100, target_percentile_max_raw)
    if (legacy_tier && verbose && target_percentile_max_raw > 100) {
        message(sprintf(
            "  Note: target_max_best_subset (%d) exceeds n_total (%d); ",
            as.integer(target_max_best_subset), as.integer(n_total)),
            "clamped derived target percentile to 100% (cap is non-binding).")
    }
    status_percentile <- .calc_percentile_status(
        percentile_val = percentile_used,
        target_max = target_percentile_max_derived
    )

    # Parameter ESS: Proportion meeting target
    if (!is.null(param_ess_results)) {
        n_params <- nrow(param_ess_results)
        n_pass <- sum(param_ess_results$ess >= target_ess_param, na.rm = TRUE)
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
    # Overall status
    # ============================================================================

    # The legacy tier verdict (saturated / tempered): any fail -> FAIL, any
    # warn -> WARN, else PASS, over the tier metrics (the percentile status
    # duplicates B_size_upper and is reported only) and, for a standalone call,
    # the per-parameter ESS proportion.
    tier_status <- if (legacy_tier) {
        st <- c(status_B_size, status_B_size_upper, status_ess_best, status_A_best, status_cvw_best)
        if (!has_status && !is.na(status_param_ess) && status_param_ess != "info")
            st <- c(st, status_param_ess)
        if (any(st == "fail")) "FAIL" else if (any(st == "warn")) "WARN" else "PASS"
    } else NA_character_
    if (has_status) status_param_ess <- "info"

    # The 1.4.0 verdict: the convergence status of calc_convergence_status(),
    # carried by weight_health. A standalone legacy call keeps the tier verdict.
    overall_status <- if (has_status) as.character(conv$status) else tier_status

    # ============================================================================
    # Verbose output
    # ============================================================================

    if (verbose) {
        message("========================================")
        message("Convergence Diagnostics Summary")
        message("========================================")
        message("Simulations: ", format(n_total, big.mark = ","), " total, ",
                format(n_successful, big.mark = ","), " valid")
        message("Posterior weighting: ", weighting)
        if (has_status) {
            ck <- conv$checks
            for (k in seq_len(NROW(ck)))
                message(sprintf("  %-38s %-7s %s", ck$label[k], toupper(ck$colour[k]), ck$reason[k]))
            if (length(conv$flag_text))
                message("Flags: ", paste(unlist(conv$flag_text), collapse = "; "))
        }
        if (legacy_tier) {
            message("Legacy best subset (tier ", convergence_tier, "): B = ", n_best_subset,
                    ", ESS_B = ", round(ess_best, 1), ", A_B = ", round(A_best, 3),
                    ", CV_B = ", round(cvw_best, 3), " - tier verdict ", tier_status)
        }
        message("Overall status: ", overall_status)
        message("----------------------------------------")
    }

    # ============================================================================
    # Build diagnostics structure
    # ============================================================================

    num <- function(x) { x <- suppressWarnings(as.numeric(unlist(x))); if (length(x) == 1L) x else NA_real_ }
    th <- weight_health$targets$thresholds
    # The summary.json view of the status (k-hat string included), so the two
    # files describe the k-hat the same way (R10).
    wh_fields <- if (has_status) .mosaic_weight_health_fields(weight_health) else NULL
    diagnostics <- list(
        settings = list(
            weighting = weighting,
            eta = num(weight_health$coarsened$eta),
            coarsening_alpha = num(weight_health$coarsened$alpha),
            # The ensemble ESS of the 1.4.0 status (summary.json ess_best /
            # ess_all) is Kish; ESS_method governs only the legacy ESS_B / A /
            # CVw and the eta = 1 concentration numbers (likelihood_concentration$method).
            ess_method = if (has_status) "kish" else ess_method,
            description = switch(weighting,
                bestfit = "BFRS calibration: coarsened posterior, best-fit ensemble",
                coarsened = "BFRS calibration: coarsened posterior, resampled ensemble",
                "BFRS calibration: legacy tier-searched best subset")
        ),
        targets = list(
            ess_best = list(
                value = if (has_status) num(weight_health$targets$ess_best_target) else target_ess_best,
                description = if (legacy_tier) "Ensemble ESS target (legacy schemes: also the tier-search size and ESS_B target)"
                              else "Required ensemble ESS (GREEN at or above)"
            )
        ),
        metrics = list(),
        summary = list(
            total_simulations_original = n_total,
            n_successful = n_successful,
            n_ensemble_members = n_best_subset,
            convergence_status = overall_status,
            convergence_label = if (has_status) as.character(conv$label) else NA_character_
        )
    )

    if (has_status) {
        ck <- conv$checks
        diagnostics$targets$best_subset_ratio <- list(
            value = num(weight_health$targets$best_subset_ratio),
            description = "Share of ESS_all the best-fit ensemble must reach")
        diagnostics$targets$ess_all_target <- list(
            value = num(weight_health$targets$ess_all_target),
            description = "ESS_all target (ess_best / best_subset_ratio)")
        diagnostics$targets$ensemble_members <- list(
            min = num(weight_health$targets$min_members), max = num(weight_health$targets$max_members),
            description = "Bounds on the ensemble size")
        diagnostics$targets$status_thresholds <- th
        colour_of <- function(id) as.character(ck$colour[ck$check == id])
        reason_of <- function(id) as.character(ck$reason[ck$check == id])
        diagnostics$metrics <- list(
            ess_all = list(value = num(weight_health$coarsened$ess_kish),
                           target = num(weight_health$targets$ess_all_target),
                           description = "Kish ESS of the coarsened posterior over all valid draws",
                           status = "info"),
            ess_best = list(value = num(weight_health$bestfit$ess_best),
                            target = num(weight_health$targets$ess_best_target),
                            description = "Kish ESS of the ensemble members (the convergence status's ESS check)",
                            status = colour_of("ess"), reason = reason_of("ess")),
            mce = list(value = num(weight_health$mce$p90),
                       median = num(weight_health$mce$median), max = num(weight_health$mce$max),
                       target = num(th$mce_green),
                       description = "Monte Carlo error of the members' posterior means / posterior SD (90th percentile over parameters)",
                       status = colour_of("mce"), reason = reason_of("mce")),
            khat = list(value = num(weight_health$coarsened$khat), target = num(th$khat_green),
                        khat_status = wh_fields$khat_status,
                        description = "Pareto k-hat of the tempered weights (validity guard)",
                        status = colour_of("khat"), reason = reason_of("khat")),
            informativeness = list(value = num(weight_health$informativeness$ess_untempered_frac),
                                   target = num(th$flat_ess_frac),
                                   eta_star = num(weight_health$eta_star$eta),
                                   description = paste0("Kish ESS of the untempered (eta = 1) weights / valid ",
                                                        "draws; flat likelihood (yellow) at or above the target. ",
                                                        "eta* is a diagnostic only"),
                                   status = colour_of("informativeness"),
                                   reason = reason_of("informativeness"))
        )
        diagnostics$status <- list(
            convergence_status = as.character(conv$status),
            convergence_label = as.character(conv$label),
            reasons = I(as.character(unlist(conv$reasons))),
            flags = I(as.character(unlist(conv$flags))),
            flags_text = I(as.character(unlist(conv$flag_text))),
            checks = ck[, intersect(c("check", "label", "value", "threshold_green", "threshold_yellow",
                                      "colour", "reason"), names(ck)), drop = FALSE]
        )
    }

    if (legacy_tier) {
        diagnostics$settings$optimization_tier <- convergence_tier
        diagnostics$settings$percentile_used <- percentile_used
        diagnostics$settings$temperature <- temperature
        diagnostics$targets$A_best <- list(value = target_A_best, description = "Target agreement index (tier search)")
        diagnostics$targets$cvw_best <- list(value = target_cvw_best,
                                             description = "Target coefficient of variation (tier search)")
        diagnostics$targets$max_best_subset <- list(value = target_max_best_subset,
                                                    description = "Maximum best subset size (absolute count)")
        diagnostics$targets$percentile_max <- list(
            value = target_percentile_max_derived,
            description = "Maximum best subset size as a percentage of all draws (max_best_subset / n_total * 100)")
        diagnostics$targets$ess_param <- list(value = target_ess_param,
                                              description = "Target ESS for individual parameters")
        diagnostics$targets$ess_param_prop <- list(value = target_ess_param_prop,
                                                   description = "Target proportion of parameters with adequate ESS")
        legacy_rows <- list(
            B_size = list(value = n_best_subset,
                          description = "Number of simulations in best subset (lower bound)",
                          status = status_B_size),
            B_size_upper = list(value = n_best_subset, target = target_max_best_subset,
                                description = "Best subset size must not exceed max_best_subset",
                                status = status_B_size_upper),
            ess_B = list(value = ess_best, description = "Effective sample size in best subset (ESS_method)",
                         status = status_ess_best),
            A_B = list(value = A_best, description = "Agreement index for best subset",
                       status = status_A_best),
            cvw_B = list(value = cvw_best, description = "Coefficient of variation of weights in best subset",
                         status = status_cvw_best)
        )
        # A standalone legacy call keeps its single-table layout: ess_best is the
        # legacy ESS_B row.
        if (!has_status) {
            legacy_rows$ess_best <- legacy_rows$ess_B
            legacy_rows$ess_B <- NULL
        }
        diagnostics$metrics <- c(legacy_rows, diagnostics$metrics)
        diagnostics$summary$retained_simulations <- n_retained
        diagnostics$summary$convergence_tier <- convergence_tier
        diagnostics$summary$percentile_used <- percentile_used
        diagnostics$summary$percentile_status <- status_percentile
        diagnostics$summary$tier_status <- tier_status
    }

    # eta = 1 likelihood concentration: reported, never gated, no k-hat.
    if (!is.null(is_diagnostics)) {
        conc <- function(d) {
            if (is.null(d)) return(NULL)
            list(n = d$n, ess_is = d$ess_is, ess_is_prop = d$ess_is_prop,
                 n_positive_ratios = d$n_positive_ratios)
        }
        diagnostics$likelihood_concentration <- list(
            all_draws        = conc(is_diagnostics$all),
            best_subset      = conc(is_diagnostics$best),
            optimized_subset = conc(is_diagnostics$optimized),
            method           = ess_method,
            note             = .mosaic_likelihood_concentration_note()
        )
    }

    if (!is.null(weight_health)) {
        diagnostics$weight_health <- .mosaic_weight_health_for_json(weight_health)
        if (has_status && !is.null(diagnostics$weight_health$coarsened)) {
            # khat_status as in summary.json; the cause of an undefined k-hat
            # (calc_weight_health()'s status attribute) is kept as khat_cause.
            diagnostics$weight_health$coarsened$khat_cause <- diagnostics$weight_health$coarsened$khat_status
            diagnostics$weight_health$coarsened$khat_status <- wh_fields$khat_status
        }
    }

    # Per-parameter ESS: a diagnostic under the 1.4.0 status, gated only by a
    # standalone legacy call.
    if (!is.null(param_ess_results)) {
        ok <- is.finite(param_ess_results$ess)
        diagnostics$metrics$param_ess <- list(
            value = pct_pass,
            n_pass = n_pass,
            n_total = n_params,
            min = if (any(ok)) min(param_ess_results$ess[ok]) else NA_real_,
            median = if (any(ok)) stats::median(param_ess_results$ess[ok]) else NA_real_,
            description = "Per-parameter ESS of the members' weights (diagnostic)",
            status = status_param_ess
        )
        # Under the 1.4.0 status there is no per-parameter ESS target: the
        # share passing the legacy target_ess_param (always 0 against the
        # ensemble sizes) is not reported (R10).
        if (has_status) diagnostics$metrics$param_ess[c("value", "n_pass")] <- NULL
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
