#' Health of the posterior weights behind a BFRS calibration
#'
#' Computes every quantity behind the MOSAIC 1.4.0 simulation-convergence
#' status from the scored draws, and the status itself through
#' \code{\link{calc_convergence_status}} (the only place the rule lives). The
#' calculation changes no weight, subset or posterior.
#'
#' @description
#' Draws are sampled from the prior and scored by the log-likelihood
#' \eqn{\ell}, so the posterior weights of the draws are a coarsened
#' (generalised-Bayes) likelihood, \eqn{w_i \propto \exp\{\eta(\ell_i -
#' \max_j \ell_j)\}}, with \eqn{\eta = \alpha / n_{obs}}: \eqn{\alpha}
#' (\code{coarsening_alpha}) is the effective number of independent
#' observations the likelihood is trusted with and \eqn{n_{obs}} the number of
#' scored observation cells (Miller and Dunson 2019; Bissiri, Holmes and Walker
#' 2016). \eqn{\eta} is fixed before the weights are seen, so the ESS of these
#' weights is not circular.
#'
#' The result holds:
#' \describe{
#'   \item{\code{coarsened}}{The posterior over every valid draw: \code{eta},
#'     \code{ess_kish}
#'     (ESS_all), \code{ess_prop}, the largest weight share, \code{khat}
#'     (Pareto k-hat of the tempered weights,
#'     \code{calc_is_diagnostics(eta * log_lik)}) with \code{khat_status}, and
#'     \code{khat_trend} / \code{khat_trend_slope} (whether k-hat falls as
#'     draws accumulate, \code{.mosaic_khat_trend()}); \code{n_locations} and
#'     \code{alpha_per_location} (\eqn{\eta} is pooled over locations).}
#'   \item{\code{bestfit}}{The best-fit ensemble: draws in descending
#'     log-likelihood until their subset ESS reaches
#'     \code{best_subset_ratio * ESS_all}, bounded by
#'     \code{[min_members, max_members]}. Members (\code{idx}, \code{sim}),
#'     their renormalised weights, \code{ess_best}, \code{mass} (posterior
#'     weight held), \code{binding} and \code{capped}.}
#'   \item{\code{mce}}{Monte Carlo standard error of the members' posterior
#'     means per parameter (seed batches by \code{(sim - 1) mod n_batches};
#'     SD of the batch means / posterior SD / \eqn{\sqrt{B'}}, over the
#'     \eqn{B'} batches holding at least 1 percent of the members' weight),
#'     with median, p90 and max.}
#'   \item{\code{informativeness}}{The N-invariant informativeness measure:
#'     \code{ess_untempered} (Kish ESS of the untempered, \eqn{\eta = 1},
#'     weights), \code{ess_untempered_frac} (that ESS / valid draws),
#'     \code{n_tied_max} (valid draws tied at the best log-likelihood) and
#'     \code{flat_ess_frac} (the threshold). \code{flat} is
#'     \code{ess_untempered_frac >= flat_ess_frac}, \code{NA} when not
#'     assessable (fewer than \code{1 / flat_ess_frac} valid draws).}
#'   \item{\code{eta_star}}{A diagnostic only (since red-team round 1): the
#'     largest \eqn{\eta \le 1} at which the Kish ESS over all draws still
#'     reaches the ESS_all target (\code{ess_best_target / best_subset_ratio}),
#'     by bisection on \eqn{\log\eta}; \code{n_eff = n_obs * eta}. It grows
#'     with the draws, so it no longer decides flatness.}
#'   \item{\code{slope}}{The log-log ESS growth slope, supplied by the caller
#'     from a batch history (\code{NA} for a single snapshot).}
#'   \item{\code{convergence}}{The \code{\link{calc_convergence_status}}
#'     result (through \code{.mosaic_health_status()}); \code{status} and
#'     \code{reasons} repeat its colour and its reasons (joined by "; ").}
#'   \item{\code{context}}{\code{budget_exhausted}, \code{fixed_mode} and
#'     \code{n_attempted}, so the status can be re-derived for other members.}
#' }
#'
#' The \eqn{\eta = 1} likelihood-concentration numbers are not part of this
#' result (they are in \code{convergence_diagnostics.json} only, from
#' \code{\link{calc_is_diagnostics}}), nor is the all-draws seed-batch ratio.
#'
#' No Monte Carlo-noise ceiling is applied to \eqn{\eta^*}. It would be
#' \eqn{\eta \le 1.7/\sigma_{seed}}, where \eqn{\sigma_{seed}} is the
#' seed-to-seed SD of the log-likelihood at fixed parameters (Doucet et al.
#' 2015), which needs per-replicate log-likelihoods that \code{run_MOSAIC()}
#' collapses by log-mean-exp and does not store.
#'
#' @param log_lik Numeric vector of per-draw log-likelihoods. Non-finite
#'   entries get zero weight.
#' @param params Data frame or matrix of sampled parameter values, one row per
#'   draw. Non-numeric columns and columns constant over the valid draws are
#'   dropped.
#' @param n_obs Number of scored observation cells (see
#'   \code{calc_run_weight_health()}); sets \eqn{\eta = \alpha / n_{obs}}.
#' @param sim Simulation ids, one per draw (seed batches and selection ties).
#'   Default \code{seq_along(log_lik)}.
#' @param coarsening_alpha Positive number: the effective number of independent
#'   observations of the posterior. Default \code{10}.
#' @param best_subset_ratio Share of ESS_all the best-fit ensemble must reach,
#'   in (0, 1]. Default \code{0.2}.
#' @param ess_best_target Required ensemble ESS. Default \code{200}.
#' @param min_members,max_members Bounds on the ensemble size. Defaults
#'   \code{30} and \code{1000}.
#' @param n_batches Number of seed batches. Default \code{10}.
#' @param thresholds Named list overriding the status bands (see
#'   \code{\link{calc_convergence_status}}).
#' @param slope Log-log ESS growth slope over recent batches (from
#'   \code{run_state.json}); \code{NA} for a single snapshot.
#' @param budget_exhausted Logical; the run stopped at its simulation cap.
#' @param fixed_mode Logical; a fixed-budget run (the status is assessed, not
#'   used to stop).
#' @param n_attempted Simulations attempted, valid or not (converts projected
#'   valid draws to simulations). Default \code{NULL}: \code{length(log_lik)}.
#' @param ess_all_target ESS_all target of the reasons and projections.
#'   Default \code{NULL}: \code{ess_best_target / best_subset_ratio} (the
#'   best-fit rule). Under \code{"coarsened"} pass the scheme's own target
#'   (the resample size, so about \code{ess_best_target}).
#' @param max_simulations The run's \code{max_simulations_total} (adaptive), so
#'   the k-hat reason can say whether its projection is within the budget.
#'   Default \code{NULL} (unknown).
#' @param batch_size_min,batch_size_max \code{batch_size_adaptive} and
#'   \code{max_batch_predictive}: bound the confirming batch included in
#'   \code{sims_projected_for_green}. Defaults 500 and 10,000.
#' @param n_locations Number of locations in the run (recorded with
#'   \code{alpha_per_location}; \eqn{\eta} is pooled over locations).
#'   Default \code{NULL} (unknown).
#' @param weights,ess_target,batch_max,ess_method Deprecated since MOSAIC
#'   1.4.0 and ignored (with a warning): the posterior summaries use the
#'   best-fit members, the ESS target is \code{ess_best_target}, the Monte
#'   Carlo error bands replace the batch-ratio limit, and the \eqn{\eta = 1}
#'   block is gone.
#'
#' @return A list with elements \code{n_draws}, \code{n_valid}, \code{targets},
#'   \code{coarsened}, \code{bestfit}, \code{mce}, \code{informativeness},
#'   \code{eta_star}, \code{slope}, \code{flat}, \code{context},
#'   \code{convergence}, \code{status}
#'   (\code{"green"}, \code{"yellow"} or \code{"red"}) and \code{reasons} (one
#'   string).
#'
#' @references
#' Miller JW, Dunson DB (2019). Robust Bayesian inference via coarsening.
#' \emph{JASA} 114(527):1113-1125.
#'
#' Bissiri PG, Holmes CC, Walker SG (2016). A general framework for updating
#' belief distributions. \emph{JRSS B} 78(5):1103-1130.
#'
#' Doucet A, Pitt MK, Deligiannidis G, Kohn R (2015). Efficient implementation
#' of Markov chain Monte Carlo when using an unbiased likelihood estimator.
#' \emph{Biometrika} 102(2):295-313.
#'
#' @examples
#' set.seed(1)
#' n <- 20000
#' theta <- runif(n)
#' ll <- -2e5 * (theta - 0.4)^2            # a likelihood over 100,000 cells
#' wh <- calc_weight_health(ll, params = data.frame(theta = theta), n_obs = 1e5)
#' wh$bestfit$ess_best
#' wh$status
#' wh$convergence$checks
#'
#' @seealso \code{\link{calc_convergence_status}}, \code{\link{calc_run_weight_health}}
#' @family calibration-metrics
#' @export
calc_weight_health <- function(log_lik, weights = NULL, params, n_obs, sim = NULL,
                               coarsening_alpha = 10, best_subset_ratio = 0.2,
                               ess_best_target = 200, min_members = 30L,
                               max_members = 1000L, n_batches = 10L,
                               thresholds = list(), slope = NA_real_,
                               budget_exhausted = FALSE, fixed_mode = FALSE,
                               n_attempted = NULL, n_locations = NULL, ess_all_target = NULL,
                               max_simulations = NULL, batch_size_min = 500L, batch_size_max = 10000L,
                               ess_target = NULL, batch_max = NULL, ess_method = NULL) {

     dep <- c(weights = !is.null(weights), ess_target = !is.null(ess_target),
              batch_max = !is.null(batch_max), ess_method = !is.null(ess_method))
     if (any(dep))
          warning("calc_weight_health(): ", paste(names(dep)[dep], collapse = ", "),
                  " ignored since MOSAIC 1.4.0 (the posterior summaries use the best-fit members; ",
                  "the ESS target is ess_best_target; the Monte Carlo error bands replace batch_max).",
                  call. = FALSE)
     log_lik <- as.numeric(log_lik)
     N <- length(log_lik)
     if (N < 2L) stop("calc_weight_health(): need at least two draws.", call. = FALSE)
     params <- as.data.frame(params)
     if (nrow(params) != N)
          stop("calc_weight_health(): params must have one row per draw.", call. = FALSE)
     if (length(n_obs) != 1L || !is.numeric(n_obs) || !is.finite(n_obs) || n_obs <= 0)
          stop("calc_weight_health(): n_obs must be a single positive number; the posterior's ",
               "learning rate eta = coarsening_alpha / n_obs is otherwise undefined.", call. = FALSE)
     if (length(coarsening_alpha) != 1L || !is.numeric(coarsening_alpha) ||
         !is.finite(coarsening_alpha) || coarsening_alpha <= 0)
          stop("calc_weight_health(): coarsening_alpha must be a single positive finite number.",
               call. = FALSE)
     if (length(ess_best_target) != 1L || !is.numeric(ess_best_target) ||
         !is.finite(ess_best_target) || ess_best_target <= 0)
          stop("calc_weight_health(): ess_best_target must be a single positive finite number.", call. = FALSE)
     n_batches <- suppressWarnings(as.integer(n_batches))
     if (length(n_batches) != 1L || is.na(n_batches) || n_batches < 2L)
          stop("calc_weight_health(): n_batches must be an integer >= 2.", call. = FALSE)
     if (is.null(sim)) sim <- seq_len(N)
     if (length(sim) != N) stop("calc_weight_health(): sim must have one entry per draw.", call. = FALSE)
     th <- .mosaic_convergence_thresholds(thresholds)

     valid <- is.finite(log_lik)
     n_valid <- sum(valid)
     if (n_valid < 2L) stop("calc_weight_health(): need at least two finite log-likelihoods.", call. = FALSE)

     P <- .mosaic_health_param_matrix(params, valid)
     eta <- coarsening_alpha / n_obs
     ess_all_target <- suppressWarnings(as.numeric(ess_all_target %||% (ess_best_target / best_subset_ratio)))
     if (length(ess_all_target) != 1L || !is.finite(ess_all_target) || ess_all_target <= 0)
          stop("calc_weight_health(): ess_all_target must be a single positive number.", call. = FALSE)

     n_att <- suppressWarnings(as.numeric(n_attempted %||% N))
     if (length(n_att) != 1L || !is.finite(n_att) || n_att < n_valid) n_att <- N
     n_loc <- suppressWarnings(as.integer(n_locations %||% NA_integer_))
     if (length(n_loc) != 1L || (!is.na(n_loc) && n_loc < 1L)) n_loc <- NA_integer_

     # (a) the posterior over all draws
     w_all <- .mosaic_tempered_weights(log_lik, eta)
     ess_all <- calc_model_ess(w_all, method = "kish")
     khat <- .mosaic_khat_tempered(log_lik, eta)
     ktr <- .mosaic_khat_trend(log_lik, sim, eta)
     coarsened <- list(alpha = coarsening_alpha, n_obs = n_obs, eta = eta,
                       ess_kish = ess_all, ess_prop = ess_all / n_valid,
                       max_share = max(w_all), n_nonzero = sum(w_all > 0),
                       khat = as.numeric(khat), khat_status = attr(khat, "status"),
                       khat_trend = ktr$trend, khat_trend_slope = ktr$slope, khat_trend_se = ktr$se,
                       n_locations = n_loc, alpha_per_location = coarsening_alpha / n_loc)

     # (b) the best-fit ensemble and its Monte Carlo error
     sel <- .mosaic_bestfit_select(log_lik, eta, best_subset_ratio, min_members, max_members, sim = sim)
     bestfit <- list(n_members = sel$n_members, n_unique = sel$n_members, ess_best = sel$ess_best,
                     ess_target = sel$ess_target, mass = sel$mass, binding = sel$binding,
                     capped = sel$capped, capped_reason = sel$capped_reason, ll_cut = sel$ll_cut,
                     idx = sel$idx, sim = sim[sel$idx], weights = sel$weights)
     mce <- if (ncol(P)) .mosaic_member_mce(P, sel$idx, sel$weights, sim, n_batches)
            else list(per_parameter = numeric(0), median = NA_real_, p90 = NA_real_, max = NA_real_,
                      n_parameters = 0L, n_batches = n_batches, n_batches_used = NA_integer_)

     # (c) data informativeness: the Kish ESS of the untempered (eta = 1)
     # weights as a share of the valid draws. About 1/N when the data inform
     # the parameters, whatever N; a sizeable share means a flat likelihood.
     ll_ok <- log_lik[valid]
     ess1 <- calc_model_ess(.mosaic_tempered_weights(log_lik, 1), method = "kish")
     ll_max <- max(ll_ok)
     informativeness <- list(ess_untempered = ess1, ess_untempered_frac = ess1 / n_valid,
                             n_tied_max = sum(abs(ll_ok - ll_max) <= 1e-9 * max(1, abs(ll_max))),
                             flat_ess_frac = th$flat_ess_frac)

     # (d) the supported resolution eta* at the ESS_all target (a diagnostic)
     eta_star <- .mosaic_eta_star(log_lik, ess_all_target)
     eta_star$n_eff <- if (is.finite(eta_star$eta)) n_obs * eta_star$eta else NA_real_

     # (e) the status: one rule, in calc_convergence_status(). Draws counted
     # are the valid ones (summary.json sims_used).
     slope <- suppressWarnings(as.numeric(slope))
     if (length(slope) != 1L) slope <- NA_real_
     wh <- list(n_draws = N, n_valid = n_valid,
                targets = list(ess_best_target = ess_best_target, best_subset_ratio = best_subset_ratio,
                               ess_all_target = ess_all_target, min_members = as.integer(min_members),
                               max_members = as.integer(max_members), n_batches = n_batches,
                               thresholds = th),
                coarsened = coarsened, bestfit = bestfit, mce = mce, informativeness = informativeness,
                eta_star = eta_star, slope = slope, flat = NA,
                context = list(budget_exhausted = isTRUE(budget_exhausted), fixed_mode = isTRUE(fixed_mode),
                               n_attempted = n_att,
                               max_simulations = suppressWarnings(as.numeric(max_simulations %||% NA_real_))[1],
                               batch_size_min = as.numeric(batch_size_min), batch_size_max = as.numeric(batch_size_max)))
     conv <- .mosaic_health_status(wh)
     wh$flat <- conv$flat
     wh$convergence <- conv
     wh$status <- conv$status
     wh$reasons <- paste(conv$reasons, collapse = "; ")
     wh
}


#' Convergence status of a weight-health result, optionally for other members
#'
#' The one place \code{calc_weight_health()} results are turned into a
#' \code{\link{calc_convergence_status}} call: every all-draws quantity
#' (ESS_all, k-hat and its cause and trend, the informativeness share, eta*,
#' the growth slope, the success rate) comes from \code{wh}, and the member
#' quantities can be overridden for an ensemble other than the best-fit
#' selection (the "coarsened" resample, the optimised subset).
#' @param wh A \code{calc_weight_health()} result.
#' @param ess_best,mce_p90,size_bound Member quantities (default: those of
#'   \code{wh$bestfit} and \code{wh$mce}).
#' @return The \code{calc_convergence_status()} result.
#' @noRd
.mosaic_health_status <- function(wh, ess_best = wh$bestfit$ess_best, mce_p90 = wh$mce$p90,
                                  size_bound = wh$bestfit$binding) {
     num <- function(x) { x <- suppressWarnings(as.numeric(unlist(x))); if (length(x) == 1L) x else NA_real_ }
     chr <- function(x) { x <- as.character(unlist(x)); if (length(x) == 1L && !identical(x, "NA")) x else NA_character_ }
     flags <- as.character(unlist(wh$convergence$flags))
     ctx <- wh$context %||% list()
     inf <- wh$informativeness
     calc_convergence_status(
          ess_best = num(ess_best), ess_best_target = num(wh$targets$ess_best_target),
          mce_p90 = num(mce_p90), khat = num(wh$coarsened$khat),
          flat = if (is.null(inf)) as.logical(num(wh$flat)) else NA,
          ess_all = num(wh$coarsened$ess_kish), ess_all_target = num(wh$targets$ess_all_target),
          n_draws = num(wh$n_valid), slope = num(wh$slope),
          budget_exhausted = isTRUE(ctx$budget_exhausted %||% ("budget_cap_reached" %in% flags)),
          fixed_mode = isTRUE(ctx$fixed_mode %||% ("fixed_budget" %in% flags)),
          size_bound = chr(size_bound), eta_star = num(wh$eta_star$eta),
          thresholds = as.list(wh$targets$thresholds %||% list()),
          ess_untempered_frac = num(inf$ess_untempered_frac), n_tied_max = num(inf$n_tied_max),
          khat_cause = chr(wh$coarsened$khat_status), khat_trend = chr(wh$coarsened$khat_trend),
          khat_trend_slope = num(wh$coarsened$khat_trend_slope), n_attempted = num(ctx$n_attempted),
          max_simulations = num(ctx$max_simulations),
          batch_size_min = if (is.null(ctx$batch_size_min)) 500 else num(ctx$batch_size_min),
          batch_size_max = if (is.null(ctx$batch_size_max)) 10000 else num(ctx$batch_size_max))
}


#' Weight health and convergence status of a finished calibration, from its output directory
#'
#' Recomputes \code{\link{calc_weight_health}} (and so the 1.4.0
#' \code{\link{calc_convergence_status}}) for an existing \code{run_MOSAIC()}
#' output directory from its stored likelihoods, so runs produced before the
#' diagnostic existed (MOSAIC 1.1.x, 1.3.0) can be assessed without re-running
#' anything. It reads \code{2_calibration/samples.parquet},
#' \code{1_inputs/config.json} and \code{1_inputs/control.json} (and
#' \code{3_results/summary.json} when present, for the location label). It
#' never modifies those files.
#'
#' \code{n_obs} is the number of scored location-day cells with a finite cases
#' or deaths observation inside the scored window, after the burn-in and the
#' deaths-era start of \code{control$likelihood}.
#'
#' The best-fit settings come from the run's control when it is a 1.4.0
#' control (it carries \code{targets$status_thresholds}):
#' \code{targets$best_subset_ratio}, \code{targets$ESS_best},
#' \code{targets$min_ensemble_members}, \code{targets$max_ensemble_members}
#' and \code{targets$status_thresholds}. An older control is assessed on
#' the 1.4.0 defaults, because its \code{ESS_best} and member caps meant
#' something else. Arguments override both. A run is fixed-budget when its
#' control has a positive \code{calibration$n_simulations}; an adaptive run has
#' exhausted its budget when its draws reach
#' \code{calibration$max_simulations_total}. The growth slope is replayed over
#' the draws in simulation order (\code{.mosaic_ess_replay()}, as for a
#' fixed-budget run), so a stalled growth is flagged. Simulations attempted
#' are the rows of \code{samples.parquet}.
#'
#' @param dir_output A \code{run_MOSAIC()} output directory.
#' @param write Logical; write \code{3_results/weight_health.json}. Default
#'   \code{TRUE}.
#' @param coarsening_alpha Effective number of independent observations of the
#'   posterior. Default \code{NULL} means the run's
#'   \code{control$targets$coarsening_alpha}, or the package default for a run
#'   that predates it.
#' @param weight_col Deprecated since MOSAIC 1.4.0 and ignored (with a
#'   warning): the status is assessed on the best-fit members.
#' @param n_batches Number of seed batches. Default \code{10}.
#' @param best_subset_ratio,ess_best_target,min_members,max_members Optional
#'   overrides of the best-fit settings (see Details).
#' @param thresholds Optional named list overriding the status bands.
#'
#' @return Invisibly, a list with \code{location}, \code{dir_output},
#'   \code{timestamp}, \code{fields} (the flat summary.json schema 2 fields) and
#'   \code{weight_health} (the full \code{calc_weight_health()} result).
#'
#' @examples
#' \dontrun{
#' wh <- calc_run_weight_health("output/national/MOZ/v2026-10-08.01")
#' wh$fields$convergence_status
#' wh$fields$convergence_reasons
#' }
#'
#' @seealso \code{\link{calc_weight_health}}, \code{\link{calc_convergence_status}}
#' @family calibration-metrics
#' @export
calc_run_weight_health <- function(dir_output, write = TRUE, coarsening_alpha = NULL,
                                   weight_col = NULL, n_batches = 10L, best_subset_ratio = NULL,
                                   ess_best_target = NULL, min_members = NULL, max_members = NULL,
                                   thresholds = NULL) {
     if (!is.null(weight_col))
          warning("calc_run_weight_health(): weight_col is ignored since MOSAIC 1.4.0 (the status ",
                  "is assessed on the best-fit members).", call. = FALSE)
     f_samples <- file.path(dir_output, "2_calibration", "samples.parquet")
     f_config  <- file.path(dir_output, "1_inputs", "config.json")
     f_control <- file.path(dir_output, "1_inputs", "control.json")
     for (f in c(f_samples, f_config, f_control))
          if (!file.exists(f)) stop("calc_run_weight_health(): missing ", f, call. = FALSE)

     samples <- as.data.frame(arrow::read_parquet(f_samples))
     config  <- read_json_to_list(f_config)
     ctrl    <- read_json_to_list(f_control)
     ctrl_raw <- if (!is.null(ctrl$control)) ctrl$control else ctrl
     # A persisted control is the run's own: its retired keys (e.g.
     # ESS_marginal_method on a pre-1.3.0 run) are expected, not user errors.
     control <- suppressWarnings(.mosaic_validate_and_merge_control(ctrl_raw))
     alpha <- coarsening_alpha %||% control$targets$coarsening_alpha
     # Scheme and best-fit settings from the RAW control: merged defaults would
     # mislabel a run that predates the keys, and a pre-1.4.0 ESS_best is the
     # legacy tier target, not the ensemble ESS target. A 1.4.0 control is one
     # with targets$status_thresholds (1.3.0 already had posterior_weighting).
     tg <- ctrl_raw$targets
     weighting <- tg$posterior_weighting %||% tg$best_subset_weighting %||% "saturated"
     v14 <- !is.null(tg$status_thresholds)
     dflt <- .mosaic_convergence_defaults()
     pick <- function(arg, key, default) {
          if (!is.null(arg)) return(arg)
          if (v14 && !is.null(tg[[key]])) return(unlist(tg[[key]]))
          default
     }
     ratio <- pick(best_subset_ratio, "best_subset_ratio", dflt$best_subset_ratio)
     ess_best_t <- pick(ess_best_target, "ESS_best", dflt$ESS_best)
     min_m <- pick(min_members, "min_ensemble_members", dflt$min_ensemble_members)
     max_m <- pick(max_members, "max_ensemble_members", dflt$max_ensemble_members)
     th <- thresholds %||% (if (v14) tg$status_thresholds) %||% list()

     n_sims <- ctrl_raw$calibration$n_simulations
     fixed <- is.numeric(n_sims) && length(n_sims) == 1L && is.finite(n_sims) && n_sims > 0
     cap <- suppressWarnings(as.numeric(control$calibration$max_simulations_total))
     exhausted <- !fixed && length(cap) == 1L && is.finite(cap) && nrow(samples) >= cap

     n_obs <- .mosaic_n_scored_obs(config, .mosaic_resolve_score_window(config, control))
     pcols <- .mosaic_weight_health_param_cols(names(samples))
     eta_bf <- .mosaic_coarsened_eta(alpha, n_obs)
     rp <- .mosaic_ess_replay(samples$likelihood, samples$sim, eta_bf)
     slope <- .mosaic_gate_slope(rp$n, rp$ess_all)
     wh <- calc_weight_health(log_lik = samples$likelihood,
                              params = samples[, pcols, drop = FALSE], n_obs = n_obs,
                              sim = samples$sim, coarsening_alpha = alpha,
                              best_subset_ratio = ratio, ess_best_target = ess_best_t,
                              min_members = min_m, max_members = max_m,
                              n_batches = n_batches, thresholds = th, slope = slope,
                              budget_exhausted = exhausted, fixed_mode = fixed,
                              n_attempted = nrow(samples),
                              n_locations = length(config$location_name),
                              max_simulations = if (!fixed && length(cap) == 1L && is.finite(cap)) cap,
                              # the run's own scheme target and batch sizes, so the
                              # backfill projects as the run did (round 3)
                              ess_all_target = if (v14 && identical(weighting, "coarsened")) ess_best_t,
                              batch_size_min = control$calibration$batch_size_adaptive %||% 500L,
                              batch_size_max = control$calibration$max_batch_predictive %||% 10000L)
     # A 1.4.0 run's status describes the members of its posterior_weighting
     # (the coarsened resample with the run's seed, the legacy tier subset):
     # re-derive them, so the backfill equals the run's own summary.
     if (v14 && !identical(weighting, "bestfit")) {
          ctl_s <- control
          ctl_s$targets$posterior_weighting <- weighting
          ctl_s$targets$ESS_best <- ess_best_t
          ctl_s$targets$min_ensemble_members <- min_m; ctl_s$targets$max_ensemble_members <- max_m
          m <- tryCatch(.mosaic_scheme_members(samples, ctl_s, n_obs, seed = .mosaic_resample_seed(config)),
                        error = function(e) NULL)
          if (!is.null(m))
               wh <- .mosaic_members_health(wh, samples, pcols, m$idx, m$w, n_members = m$n_members,
                                            binding = m$binding)
     }
     wh$weighting <- weighting

     f_summary <- file.path(dir_output, "3_results", "summary.json")
     location <- if (file.exists(f_summary)) {
          tryCatch(jsonlite::read_json(f_summary)$location, error = function(e) NULL)
     } else NULL
     location <- location %||% paste(config$location_name, collapse = ", ")

     out <- list(location = location, dir_output = normalizePath(dir_output, mustWork = FALSE),
                 timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                 fields = .mosaic_weight_health_fields(wh),
                 weight_health = wh)
     if (isTRUE(write)) {
          dir.create(file.path(dir_output, "3_results"), recursive = TRUE, showWarnings = FALSE)
          js <- out; js$weight_health <- .mosaic_weight_health_for_json(wh)
          jsonlite::write_json(js, file.path(dir_output, "3_results", "weight_health.json"),
                               pretty = TRUE, auto_unbox = TRUE, digits = NA, na = "null")
     }
     invisible(out)
}


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Explanation stored with the eta = 1 numbers wherever they are written.
.mosaic_likelihood_concentration_note <- function() {
     paste("eta = 1 likelihood concentration: the importance weights the untempered likelihood",
           "would give the prior draws. With thousands of scored cells and a stochastic",
           "likelihood they are expected to collapse to an ESS of about 1 whatever the fit,",
           "so these numbers measure how sharply the raw likelihood ranks the draws. They are",
           "not a fit or convergence diagnostic, and no posterior is built from them.")
}

# Normalised tempered weights exp(eta * (ll - max ll)); zero for non-finite ll.
.mosaic_tempered_weights <- function(log_lik, eta) {
     w <- numeric(length(log_lik))
     ok <- is.finite(log_lik)
     if (!any(ok)) return(w)
     w[ok] <- exp(eta * (log_lik[ok] - max(log_lik[ok])))
     w / sum(w)
}

# Numeric parameter matrix over all draws, keeping columns that vary over the
# valid draws.
.mosaic_health_param_matrix <- function(params, valid) {
     keep <- vapply(params, function(x) {
          is.numeric(x) && {
               v <- x[valid & is.finite(x)]
               length(v) >= 2L && diff(range(v)) > 0
          }
     }, logical(1))
     as.matrix(params[, keep, drop = FALSE])
}

# Between-batch SD of within-batch weighted means over the posterior SD, per
# parameter column of P. A batch counts only if it holds at least
# min_batch_weight of the total weight: a batch holding a negligible share has
# a mean that is pure noise (one member at weight 1e-9 moved the SD from 0.08
# to 4.2; red-team round 1, T5/C13). Inf when fewer than two batches count.
.mosaic_batch_ratio <- function(w, P, batch, min_batch_weight = 0.01) {
     if (!ncol(P)) return(numeric(0))
     out <- stats::setNames(rep(NA_real_, ncol(P)), colnames(P))
     fin <- is.finite(P)
     Wm <- w * fin                                   # weight, zero where the value is missing
     Pz <- P; Pz[!fin] <- 0
     tot <- colSums(Wm)
     m <- colSums(Wm * Pz) / tot
     s <- sqrt(colSums(Wm * sweep(Pz, 2L, m)^2 * fin) / tot)
     den <- rowsum(Wm, batch, reorder = TRUE)
     mb <- rowsum(Wm * Pz, batch, reorder = TRUE) / den
     mb[!(den > 0)] <- NA_real_
     share <- as.numeric(rowsum(w, batch, reorder = TRUE)) / sum(w)
     used <- share >= min_batch_weight
     mb[!used, ] <- NA_real_
     for (k in seq_len(ncol(P))) {
          if (!(tot[k] > 0) || !is.finite(s[k]) || !(s[k] > 0)) next
          v <- mb[, k]; v <- v[is.finite(v)]
          out[k] <- if (length(v) < 2L) Inf else stats::sd(v) / s[k]
     }
     # Fewer than two batches counted overall: no between-batch information.
     if (sum(used) < 2L) out[] <- Inf
     out
}

# Largest eta in (0, 1] at which the Kish ESS of the tempered weights over all
# valid draws is >= ess_target, by bisection on log(eta). The ESS is treated as
# non-increasing in eta. A diagnostic only: eta* = 1 means the raw likelihood
# already meets the target, which depends on N (red-team round 1, T1); NA means
# no eta meets it with these draws.
.mosaic_eta_star <- function(log_lik, ess_target) {
     note <- paste("No Monte Carlo-noise ceiling (eta <= 1.7 / seed SD of the log-likelihood):",
                   "per-replicate log-likelihoods are collapsed by log-mean-exp and not stored.",
                   "eta* meets the ESS target by construction; it is a resolution, not a verdict.",
                   "It grows with the draws, so it does not decide flatness (ess_untempered_frac does).")
     ess_at <- function(eta) calc_model_ess(.mosaic_tempered_weights(log_lik, eta), method = "kish")
     out <- function(eta, ess, binding)
          list(eta = eta, ess_kish = ess, target = ess_target, binding = binding, note = note)
     fin <- log_lik[is.finite(log_lik)]
     spread <- if (length(fin)) diff(range(fin)) else 0
     if (!(spread > 0)) {
          e <- length(fin)
          return(if (e >= ess_target) out(1, e, "none (eta = 1)")
                 else out(NA_real_, NA_real_, "no eta meets the target with these draws"))
     }
     e1 <- ess_at(1)
     if (e1 >= ess_target) return(out(1, e1, "none (eta = 1)"))
     lo <- log(1e-6 / spread); hi <- 0
     if (ess_at(exp(lo)) < ess_target)
          return(out(NA_real_, NA_real_, "no eta meets the target with these draws"))
     for (i in seq_len(60L)) {
          mid <- (lo + hi) / 2
          if (ess_at(exp(mid)) >= ess_target) lo <- mid else hi <- mid
          if (hi - lo < 1e-4) break
     }
     out(exp(lo), ess_at(exp(lo)), "ess")
}

# Flat summary.json schema 2 fields (claude/plan_convergence/SCHEMA_v2.md
# section 1, with the round-1 additions sims_projected_for_green,
# projection_reliable, ess_untempered_frac, n_locations and
# alpha_per_location) that the weight health supplies, plus the two aliases it can fill
# (ess_coarsened = ess_all; weight_health_status = "ok" if green else "warn").
# Run-level fields (schema_version, mode, stopping_rule_met, param_ess_*, the
# other aliases) are the summary writer's. Vector fields are AsIs so a single
# reason stays a JSON array. Tolerant of a block read back from JSON, where NA
# can be the string "NA" and vectors are lists.
.mosaic_weight_health_fields <- function(wh) {
     one <- function(x) { x <- unlist(x); if (length(x) != 1L) NA else x }
     num <- function(x) suppressWarnings(as.numeric(one(x)))
     int <- function(x) suppressWarnings(as.integer(one(x)))
     chr <- function(x) { v <- as.character(one(x)); if (identical(v, "NA")) NA_character_ else v }
     lgl <- function(x) suppressWarnings(as.logical(one(x)))
     cv <- wh$convergence
     ck <- cv$checks
     colour_of <- function(id) {
          if (is.null(ck)) return(NA_character_)
          if (is.data.frame(ck)) return(chr(ck$colour[ck$check == id]))
          chr(Filter(function(r) identical(unlist(r$check), id), ck)[[1]]$colour)
     }
     reasons <- as.character(unlist(cv$reasons) %||% character(0))
     flags <- as.character(unlist(cv$flags) %||% character(0))
     flag_text <- as.character(unlist(cv$flag_text) %||% character(0))
     status <- chr(cv$status)
     ess_all <- num(wh$coarsened$ess_kish)
     target <- num(wh$targets$ess_all_target)
     khat <- num(wh$coarsened$khat)
     khat_col <- colour_of("khat")
     list(
          convergence_status         = status,
          convergence_label          = chr(cv$label),
          convergence_reasons        = I(reasons),
          convergence_reasons_text   = if (is.null(cv)) NA_character_ else paste(reasons, collapse = "; "),
          convergence_flags          = I(flags),
          convergence_flags_text     = if (is.null(cv)) NA_character_ else paste(flag_text, collapse = "; "),
          check_ess_status           = colour_of("ess"),
          check_mce_status           = colour_of("mce"),
          check_khat_status          = khat_col,
          check_informativeness_status = colour_of("informativeness"),
          posterior_weighting        = chr(wh$weighting),
          coarsening_alpha           = num(wh$coarsened$alpha),
          eta                        = num(wh$coarsened$eta),
          n_obs_scored               = int(wh$coarsened$n_obs),
          ess_all                    = ess_all,
          ess_all_target             = target,
          ess_best                   = num(wh$bestfit$ess_best),
          ess_best_target            = num(wh$targets$ess_best_target),
          best_subset_ratio          = num(wh$targets$best_subset_ratio),
          best_subset_mass           = num(wh$bestfit$mass),
          ensemble_members           = int(wh$bestfit$n_members),
          ensemble_members_unique    = int(wh$bestfit$n_unique),
          ensemble_binding           = chr(wh$bestfit$binding),
          mce_median                 = num(wh$mce$median),
          mce_p90                    = num(wh$mce$p90),
          mce_max                    = num(wh$mce$max),
          khat_tempered              = khat,
          khat_status                = if (is.null(cv)) NA_character_
                                       else if (!is.finite(khat)) {
                                            if (identical(khat_col, "green")) "undefined (weights nearly flat)"
                                            else sprintf("undefined (not assessed: %s)",
                                                         if (identical(as.numeric(unlist(wh$n_valid))[1] < 2, TRUE))
                                                              "fewer than two valid draws"
                                                         else .mosaic_khat_cause_text(chr(wh$coarsened$khat_status)))
                                       } else sprintf("%.2f (%s)", khat, switch(khat_col %||% "red",
                                            green = "good", yellow = "marginal", "unreliable")),
          eta_star                   = num(wh$eta_star$eta),
          eta_star_n_eff             = num(wh$eta_star$n_eff),
          flat_likelihood            = if (is.null(cv)) NA else lgl(cv$flat %||% wh$flat),
          ess_growth_slope           = num(wh$slope),
          sims_used                  = int(wh$n_valid),
          sims_projected_for_target  = if (is.finite(ess_all) && is.finite(target) && ess_all < target)
                                            num(cv$n_draws_projected) else NA_real_,
          budget_exhausted           = if (is.null(cv)) NA
                                       else "budget_cap_reached" %in% flags && !identical(status, "green"),
          sims_projected_for_green   = num(cv$sims_projected_for_green),
          projection_reliable        = if (is.null(cv)) NA
                                       else isTRUE(lgl(cv$projection_reliable)),
          ess_untempered_frac        = num(wh$informativeness$ess_untempered_frac),
          n_locations                = int(wh$coarsened$n_locations),
          alpha_per_location         = num(wh$coarsened$alpha_per_location),
          ess_coarsened              = ess_all,
          weight_health_status       = if (is.na(status)) NA_character_
                                       else if (status == "green") "ok" else "warn"
     )
}

# calc_weight_health() result in a JSON-safe shape: jsonlite drops the names of
# atomic vectors, so the named per-parameter Monte Carlo errors become a named
# list (a JSON object). The flags are already a named list.
.mosaic_weight_health_for_json <- function(wh) {
     if (is.null(wh)) return(NULL)
     if (!is.null(wh$mce$per_parameter)) wh$mce$per_parameter <- as.list(wh$mce$per_parameter)
     wh
}

# Sampled-parameter columns of a samples.parquet frame: estimated parameters,
# without the compartment counts that are derived from the sampled proportions.
.mosaic_weight_health_param_cols <- function(nms) {
     est <- get("estimated_parameters", envir = asNamespace("MOSAIC"))$parameter_name
     base <- gsub("_[A-Z]{3}$", "", nms)
     nms[base %in% est & !grepl("^[NSEIRV][12]?_j_initial", nms)]
}

# Scored observation cells: location-days inside the scored window with a
# finite cases observation (from score_window$idx_cases) or a finite deaths
# observation (from score_window$idx_deaths).
.mosaic_n_scored_obs <- function(config, score_window = NULL) {
     m1 <- function(x) if (is.null(x) || is.matrix(x)) x else matrix(as.numeric(x), nrow = 1L)
     oc <- m1(config$reported_cases); od <- m1(config$reported_deaths)
     if (is.null(oc) && is.null(od)) return(NA_real_)
     nT <- if (!is.null(oc)) ncol(oc) else ncol(od)
     ic <- if (!is.null(score_window$idx_cases)) score_window$idx_cases else 1L
     id <- if (!is.null(score_window$idx_deaths)) score_window$idx_deaths else 1L
     col <- matrix(seq_len(nT), nrow = if (!is.null(oc)) nrow(oc) else nrow(od), ncol = nT, byrow = TRUE)
     fc <- if (!is.null(oc)) is.finite(oc) & col >= ic else FALSE
     fd <- if (!is.null(od) && identical(dim(od), dim(col))) is.finite(od) & col >= id else FALSE
     n <- sum(fc | fd)
     if (n > 0) as.numeric(n) else NA_real_
}
