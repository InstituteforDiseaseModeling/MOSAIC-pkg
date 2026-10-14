#' Health of the importance weights behind a BFRS calibration
#'
#' Reports whether the prior draws of a calibration represent its posterior
#' well enough to trust posterior summaries. The calculation itself changes no
#' weight, subset or posterior. Under the default
#' \code{control$targets$best_subset_weighting = "coarsened"}
#' \code{run_MOSAIC()} resamples its ensemble members, from which the parameter
#' posterior summaries are built, from the coarsened posterior described here,
#' and \code{status} is its convergence verdict (\code{posthoc_criteria_met}).
#'
#' @description
#' Draws are sampled from the prior and scored by the log-likelihood
#' \eqn{\ell}, so the posterior weights of the draws are a tempered
#' (generalised-Bayes) likelihood, \eqn{w_i \propto \exp\{\eta(\ell_i -
#' \max_j \ell_j)\}}. The learning rate \eqn{\eta} is the number of independent
#' observations the likelihood is trusted with, per scored cell. \eqn{\eta = 1}
#' trusts every scored cell as independent evidence. That is far too much for
#' daily cells holding weekly reports from an imperfect, stochastic model:
#' every reasonable draw would then sit thousands of log units from the best
#' one, and the weights collapse onto a single draw whatever the fit.
#'
#' Four groups of numbers are returned.
#'
#' \describe{
#'   \item{\code{coarsened}}{The headline. This is the \emph{declared}
#'     posterior (since MOSAIC 1.3.0, by default, the posterior
#'     \code{run_MOSAIC()} resamples its ensemble members from), a coarsened posterior (Miller and Dunson 2019) with learning
#'     rate \eqn{\eta_{ref} = \alpha / n_{obs}}. Here \eqn{\alpha} (argument
#'     \code{coarsening_alpha}) is the effective number of independent
#'     observations the likelihood is trusted with, and \eqn{n_{obs}} is the
#'     number of scored observation cells. It gives the Kish ESS of those
#'     weights over every valid draw (\code{ess_kish}, \code{ess_prop}) and
#'     their seed-batch stability. \eqn{\eta_{ref}} is fixed before the
#'     weights are seen, so this ESS is not circular: it measures how many
#'     effective draws the run supplies for a posterior declared in advance.}
#'   \item{\code{eta_star}}{The "supported resolution". This is the largest
#'     \eqn{\eta} at which the draws still give a Kish ESS of at least
#'     \code{ess_target} and a batch ratio below \code{batch_max}, found by
#'     bisection on \eqn{\log\eta}. \code{n_eff = n_obs * eta} is the number
#'     of effective observations the draws can resolve. Its ESS meets the
#'     target by construction, so it is reported as a resolution and never
#'     used as a health verdict.}
#'   \item{\code{weights_used}}{The weights the posterior summaries are
#'     actually built from: in \code{run_MOSAIC()}, the ensemble members'
#'     weights (\code{weight_best}: count / K resample weights under
#'     \code{"coarsened"}, best-subset weights under the legacy schemes), or
#'     \code{weight_best_opt} once the subset optimiser has run. Kish and perplexity ESS,
#'     the largest weight share, the number of non-zero weights and the
#'     seed-batch stability. Under \code{"coarsened"} \code{run_MOSAIC()} adds
#'     \code{resample} (the number of draws \code{K} systematically resampled
#'     for the ensemble, the unique members and the seed).}
#'   \item{\code{likelihood_concentration}}{The \eqn{\eta = 1} numbers from
#'     \code{\link{calc_is_diagnostics}}: ESS and the number of non-underflowing
#'     ratios. They are kept for reference with an explanatory note and never
#'     enter \code{status}.}
#' }
#'
#' \strong{Seed-batch stability.} Draws are split into \code{n_batches}
#' disjoint batches by simulation id (\code{(sim - 1) mod n_batches}). Each
#' batch is an independent sub-run, because draw \code{sim} is sampled with
#' seed \code{sim}. For each parameter, the ratio is the standard deviation of
#' the within-batch weighted means across batches, divided by the posterior
#' standard deviation from all draws. Only batches carrying positive weight
#' count. When fewer than two batches carry weight the ratio is \code{Inf}.
#' Parameters with zero posterior spread are skipped. For a posterior with
#' Kish ESS \eqn{E} the ratio of one parameter is roughly \eqn{\sqrt{B/E}}; the
#' reported maximum over parameters is larger (about \eqn{1.5\sqrt{B/E}} for
#' 40 parameters, \eqn{1.8\sqrt{B/E}} for 1,500), so with the default 10
#' batches the \code{batch_max = 0.2} criterion needs \eqn{E} of roughly 550
#' to 800, more than the default \code{ESS_param}.
#'
#' \strong{Status.} \code{"ok"} when the declared (coarsened) posterior has
#' Kish ESS \eqn{\ge} \code{ess_target} \emph{and} batch ratio \eqn{<}
#' \code{batch_max}. Otherwise it is \code{"warn"}, with the reasons spelled
#' out. Neither the \eqn{\eta = 1} numbers nor \eqn{\eta^*} affect it.
#'
#' No Monte Carlo-noise ceiling is applied to \eqn{\eta^*}. The ceiling would
#' be \eqn{\eta \le 1.7/\sigma_{seed}}, where \eqn{\sigma_{seed}} is the
#' seed-to-seed SD of the log-likelihood at fixed parameters (Doucet et al.
#' 2015). It needs the per-replicate log-likelihoods, which
#' \code{run_MOSAIC()} collapses by log-mean-exp and does not store.
#'
#' The default \code{coarsening_alpha = 10} is \code{control$targets$coarsening_alpha}.
#' On the 2026-10 national runs the pre-1.3.0 saturated subset posteriors
#' corresponded to an \eqn{\alpha} of roughly 6 to 20.
#'
#' @param log_lik Numeric vector of per-draw log-likelihoods. Non-finite
#'   entries get zero weight.
#' @param weights Numeric vector of the weights actually used, one per draw
#'   (zero outside the subset; normalised internally).
#' @param params Data frame or matrix of sampled parameter values, one row per
#'   draw. Non-numeric columns and columns that are constant over the valid
#'   draws are dropped.
#' @param n_obs Number of scored observation cells (see
#'   \code{calc_run_weight_health()}). Sets \eqn{\eta_{ref} = \alpha / n_{obs}}
#'   and \code{eta_star$n_eff}.
#' @param sim Simulation ids, one per draw, used to form the seed batches.
#'   Default \code{seq_along(log_lik)}.
#' @param coarsening_alpha Positive number: the effective number of independent
#'   observations of the declared posterior. Default \code{10}, the
#'   \code{run_MOSAIC()} default.
#' @param ess_target Kish ESS target. Default \code{NULL} means
#'   \code{min(1000, 0.02 * n_valid)}; \code{run_MOSAIC()} passes
#'   \code{control$targets$ESS_param}.
#' @param batch_max Largest acceptable batch ratio. Default \code{0.2};
#'   \code{run_MOSAIC()} passes \code{control$targets$batch_ratio_max}.
#' @param n_batches Number of seed batches. Default \code{10}.
#' @param ess_method ESS formula for the \eqn{\eta = 1} block only,
#'   \code{"kish"} or \code{"perplexity"}.
#'
#' @return A list with elements \code{n_draws}, \code{n_valid}, \code{targets},
#'   \code{coarsened}, \code{eta_star}, \code{weights_used},
#'   \code{likelihood_concentration}, \code{status} (\code{"ok"} or
#'   \code{"warn"}) and \code{reasons} (one string).
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
#' n <- 5000
#' theta <- runif(n)
#' ll <- -2000 * (theta - 0.4)^2           # a likelihood over 1000 cells
#' w_used <- numeric(n); top <- order(-ll)[1:100]; w_used[top] <- 1 / 100
#' wh <- calc_weight_health(ll, w_used, data.frame(theta = theta), n_obs = 1000,
#'                          coarsening_alpha = 10, ess_target = 200)
#' wh$coarsened$ess_kish
#' wh$status
#'
#' @seealso \code{\link{calc_run_weight_health}}, \code{\link{calc_is_diagnostics}}
#' @family calibration-metrics
#' @export
calc_weight_health <- function(log_lik, weights, params, n_obs, sim = NULL,
                               coarsening_alpha = 10, ess_target = NULL,
                               batch_max = 0.2, n_batches = 10L,
                               ess_method = c("kish", "perplexity")) {

     ess_method <- match.arg(ess_method)
     log_lik <- as.numeric(log_lik)
     N <- length(log_lik)
     if (N < 2L) stop("calc_weight_health(): need at least two draws.", call. = FALSE)
     weights <- as.numeric(weights)
     if (length(weights) != N)
          stop("calc_weight_health(): weights must have one entry per draw.", call. = FALSE)
     if (any(weights < 0, na.rm = TRUE))
          stop("calc_weight_health(): weights must be non-negative.", call. = FALSE)
     params <- as.data.frame(params)
     if (nrow(params) != N)
          stop("calc_weight_health(): params must have one row per draw.", call. = FALSE)
     if (length(n_obs) != 1L || !(is.na(n_obs) || (is.numeric(n_obs) && n_obs > 0)))
          stop("calc_weight_health(): n_obs must be a single positive number (or NA).", call. = FALSE)
     if (length(coarsening_alpha) != 1L || !is.numeric(coarsening_alpha) ||
         !is.finite(coarsening_alpha) || coarsening_alpha <= 0)
          stop("calc_weight_health(): coarsening_alpha must be a single positive finite number.",
               call. = FALSE)
     n_batches <- as.integer(n_batches)
     if (length(n_batches) != 1L || is.na(n_batches) || n_batches < 2L)
          stop("calc_weight_health(): n_batches must be an integer >= 2.", call. = FALSE)
     if (is.null(sim)) sim <- seq_len(N)
     if (length(sim) != N) stop("calc_weight_health(): sim must have one entry per draw.", call. = FALSE)

     valid <- is.finite(log_lik)
     n_valid <- sum(valid)
     if (n_valid < 2L) stop("calc_weight_health(): need at least two finite log-likelihoods.", call. = FALSE)
     if (is.null(ess_target)) ess_target <- min(1000, 0.02 * n_valid)

     P <- .mosaic_health_param_matrix(params, valid)
     batch <- (as.integer(sim) - 1L) %% n_batches + 1L

     # (a) the weights actually used
     w_used <- weights
     w_used[!is.finite(w_used)] <- 0
     if (sum(w_used) <= 0) stop("calc_weight_health(): weights sum to zero.", call. = FALSE)
     w_used <- w_used / sum(w_used)
     used <- .mosaic_weights_block(w_used, P, batch)

     # (b) the declared, coarsened posterior: eta_ref = alpha / n_obs
     eta_ref <- if (is.finite(n_obs)) coarsening_alpha / n_obs else NA_real_
     coarsened <- if (is.finite(eta_ref)) {
          blk <- .mosaic_weights_block(.mosaic_tempered_weights(log_lik, eta_ref), P, batch)
          c(list(alpha = coarsening_alpha, n_obs = n_obs, eta_ref = eta_ref),
            blk, list(ess_prop = blk$ess_kish / n_valid))
     } else {
          list(alpha = coarsening_alpha, n_obs = NA_real_, eta_ref = NA_real_,
               ess_kish = NA_real_, ess_perplexity = NA_real_, max_share = NA_real_,
               n_nonzero = NA_integer_, batch_ratio_max = NA_real_,
               batch_ratio_median = NA_real_, ess_prop = NA_real_)
     }

     # (c) the supported resolution eta*
     eta_star <- .mosaic_eta_star(log_lik, P, batch, ess_target, batch_max)
     eta_star$n_eff <- if (is.finite(n_obs) && is.finite(eta_star$eta)) n_obs * eta_star$eta else NA_real_

     # (d) eta = 1 concentration, reference only
     isd <- calc_is_diagnostics(log_lik, method = ess_method)
     conc <- list(ess_is = isd$ess_is, ess_is_prop = isd$ess_is_prop,
                  n_positive_ratios = isd$n_positive_ratios, method = ess_method,
                  note = .mosaic_likelihood_concentration_note())

     # Status from the declared posterior only.
     reasons <- character(0)
     if (!is.finite(coarsened$ess_kish)) {
          reasons <- c(reasons, "n_obs unknown, so the declared posterior (eta_ref = alpha / n_obs) cannot be evaluated")
     } else {
          if (coarsened$ess_kish < ess_target)
               reasons <- c(reasons, sprintf(paste0(
                    "draws too few for the declared posterior: Kish ESS %.0f < target %.0f at ",
                    "eta_ref = %.3g (alpha = %g); run more simulations"),
                    coarsened$ess_kish, ess_target, eta_ref, coarsening_alpha))
          if (!(coarsened$batch_ratio_max < batch_max))
               reasons <- c(reasons, sprintf(paste0(
                    "declared-posterior summaries vary across seed batches (between-batch SD / ",
                    "posterior SD = %.2f, limit %.2f); run more simulations"),
                    coarsened$batch_ratio_max, batch_max))
     }
     status <- if (length(reasons)) "warn" else "ok"
     if (!length(reasons))
          reasons <- sprintf(paste0("declared posterior (alpha = %g) has Kish ESS %.0f >= %.0f ",
                                    "and batch ratio %.2f < %.2f"),
                             coarsening_alpha, coarsened$ess_kish, ess_target,
                             coarsened$batch_ratio_max, batch_max)

     list(n_draws = N, n_valid = n_valid,
          targets = list(ess_target = ess_target, batch_max = batch_max, n_batches = n_batches),
          coarsened = coarsened, eta_star = eta_star, weights_used = used,
          likelihood_concentration = conc,
          status = status, reasons = paste(reasons, collapse = "; "))
}


#' Weight health of a finished calibration, recomputed from its output directory
#'
#' Recomputes \code{\link{calc_weight_health}} for an existing
#' \code{run_MOSAIC()} output directory, so that results produced before the
#' diagnostic existed can be backfilled without re-running anything. It reads
#' \code{2_calibration/samples.parquet}, \code{1_inputs/config.json} and
#' \code{1_inputs/control.json} (and \code{3_results/summary.json} when present,
#' for the location label). It never modifies those files.
#'
#' \code{n_obs} is the number of scored location-day cells with a finite cases
#' or deaths observation inside the scored window, after the burn-in and the
#' deaths-era start of \code{control$likelihood}.
#'
#' @param dir_output A \code{run_MOSAIC()} output directory.
#' @param write Logical; write \code{3_results/weight_health.json}. Default
#'   \code{TRUE}.
#' @param coarsening_alpha Effective number of independent observations of the
#'   declared posterior. Default \code{NULL} means the run's
#'   \code{control$targets$coarsening_alpha}, or the package default for a run
#'   that predates it.
#' @param weight_col Which weights the posterior used: \code{"auto"} (default:
#'   \code{weight_best_opt} when the run has a non-empty optimised subset, else
#'   \code{weight_best}, the ensemble members, under every scheme),
#'   \code{"weight_all"}, \code{"weight_best"} or \code{"weight_best_opt"}.
#'   A run whose control.json has no \code{best_subset_weighting} predates the
#'   key and is read as \code{"saturated"}.
#' @param n_batches Number of seed batches. Default \code{10}.
#'
#' @return Invisibly, a list with \code{location}, \code{weights_used_column},
#'   \code{fields} (the flat \code{summary.json}-style fields) and
#'   \code{weight_health} (the full \code{calc_weight_health()} result).
#'
#' @examples
#' \dontrun{
#' wh <- calc_run_weight_health("output/national/MOZ/v2026-10-08.01")
#' wh$fields$ess_coarsened
#' }
#'
#' @seealso \code{\link{calc_weight_health}}
#' @family calibration-metrics
#' @export
calc_run_weight_health <- function(dir_output, write = TRUE, coarsening_alpha = NULL,
                                   weight_col = c("auto", "weight_all", "weight_best", "weight_best_opt"),
                                   n_batches = 10L) {
     weight_col <- match.arg(weight_col)
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
     # Scheme from the RAW control: the merged default ("coarsened") would
     # mislabel a run that predates the key.
     weighting <- ctrl_raw$targets$best_subset_weighting %||% "saturated"
     coarsened_run <- identical(weighting, "coarsened")

     if (weight_col == "auto") {
          weight_col <- if ("weight_best_opt" %in% names(samples) &&
                            sum(samples$weight_best_opt, na.rm = TRUE) > 0) "weight_best_opt"
                        else "weight_best"
     }
     if (!weight_col %in% names(samples))
          stop("calc_run_weight_health(): samples.parquet has no column ", weight_col, call. = FALSE)

     n_obs <- .mosaic_n_scored_obs(config, .mosaic_resolve_score_window(config, control))
     pcols <- .mosaic_weight_health_param_cols(names(samples))
     wh <- calc_weight_health(log_lik = samples$likelihood, weights = samples[[weight_col]],
                              params = samples[, pcols, drop = FALSE], n_obs = n_obs,
                              sim = samples$sim, coarsening_alpha = alpha,
                              ess_target = control$targets$ESS_param,
                              batch_max = control$targets$batch_ratio_max,
                              n_batches = n_batches,
                              ess_method = control$targets$ESS_method)
     wh$weights_used_column <- weight_col
     wh$weighting <- weighting
     if (coarsened_run && all(c("weight_all", "is_best_subset") %in% names(samples))) {
          # The resample's K is clamp(round(Kish ESS of weight_all)), exactly as
          # run_MOSAIC() drew it; the unique members are is_best_subset.
          w_all <- samples$weight_all
          w_all[!is.finite(w_all)] <- 0
          K <- if (sum(w_all) > 0) as.integer(min(max(round(calc_model_ess(w_all / sum(w_all), method = "kish")),
                                                      control$targets$min_best_subset),
                                                  .mosaic_ensemble_K_max(control$targets))) else NA_integer_
          wh$resample <- list(K = K, n_unique = sum(as.logical(samples$is_best_subset), na.rm = TRUE))
     }

     f_summary <- file.path(dir_output, "3_results", "summary.json")
     location <- if (file.exists(f_summary)) {
          tryCatch(jsonlite::read_json(f_summary)$location, error = function(e) NULL)
     } else NULL
     location <- location %||% paste(config$location_name, collapse = ", ")

     out <- list(location = location, dir_output = normalizePath(dir_output, mustWork = FALSE),
                 timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                 weights_used_column = weight_col,
                 fields = .mosaic_weight_health_fields(wh),
                 weight_health = wh)
     if (isTRUE(write)) {
          dir.create(file.path(dir_output, "3_results"), recursive = TRUE, showWarnings = FALSE)
          jsonlite::write_json(out, file.path(dir_output, "3_results", "weight_health.json"),
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
# parameter column of P. Inf when fewer than two batches carry weight.
.mosaic_batch_ratio <- function(w, P, batch) {
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
     for (k in seq_len(ncol(P))) {
          if (!(tot[k] > 0) || !is.finite(s[k]) || !(s[k] > 0)) next
          v <- mb[, k]; v <- v[is.finite(v)]
          out[k] <- if (length(v) < 2L) Inf else stats::sd(v) / s[k]
     }
     # Fewer than two weighted batches overall: no between-batch information.
     if (sum(tapply(w, batch, sum) > 0) < 2L) out[] <- Inf
     out
}

# ESS, concentration and seed-batch stability of one normalised weight vector.
.mosaic_weights_block <- function(w, P, batch) {
     br <- .mosaic_batch_ratio(w, P, batch)
     br_ok <- br[!is.na(br)]
     list(ess_kish = calc_model_ess(w, method = "kish"),
          ess_perplexity = calc_model_ess(w, method = "perplexity"),
          max_share = max(w), n_nonzero = sum(w > 0),
          batch_ratio_max = if (length(br_ok)) max(br_ok) else NA_real_,
          batch_ratio_median = if (length(br_ok)) stats::median(br_ok) else NA_real_)
}

# Largest eta in (0, 1] with Kish ESS >= ess_target and batch ratio < batch_max,
# by bisection on log(eta). Both criteria tighten as eta grows, so the
# predicate is treated as monotone.
.mosaic_eta_star <- function(log_lik, P, batch, ess_target, batch_max) {
     note <- paste("No Monte Carlo-noise ceiling (eta <= 1.7 / seed SD of the log-likelihood):",
                   "per-replicate log-likelihoods are collapsed by log-mean-exp and not stored.",
                   "eta* meets the ESS target by construction; it is a resolution, not a verdict.")
     eval_at <- function(eta) {
          w <- .mosaic_tempered_weights(log_lik, eta)
          br <- .mosaic_batch_ratio(w, P, batch); br <- br[!is.na(br)]
          e <- calc_model_ess(w, method = "kish")
          bmax <- if (length(br)) max(br) else 0
          list(ess = e, bmax = bmax, bmed = if (length(br)) stats::median(br) else NA_real_,
               ok_ess = e >= ess_target, ok_batch = bmax < batch_max)
     }
     ok <- function(r) r$ok_ess && r$ok_batch
     fin <- log_lik[is.finite(log_lik)]
     spread <- diff(range(fin))
     none <- list(eta = NA_real_, ess_kish = NA_real_, batch_ratio_max = NA_real_,
                  batch_ratio_median = NA_real_, binding = NA_character_, note = note)
     if (!(spread > 0)) {
          r <- eval_at(1)
          if (!ok(r)) return(c(none[1:4], list(binding = "no eta meets the targets with these draws", note = note)))
          return(list(eta = 1, ess_kish = r$ess, batch_ratio_max = r$bmax,
                      batch_ratio_median = r$bmed, binding = "none (eta = 1)", note = note))
     }
     r_hi <- eval_at(1)
     if (ok(r_hi)) return(list(eta = 1, ess_kish = r_hi$ess, batch_ratio_max = r_hi$bmax,
                              batch_ratio_median = r_hi$bmed, binding = "none (eta = 1)", note = note))
     lo <- log(1e-6 / spread); hi <- 0
     r_lo <- eval_at(exp(lo))
     if (!ok(r_lo)) return(c(none[1:4], list(binding = "no eta meets the targets with these draws", note = note)))
     for (i in seq_len(60L)) {
          mid <- (lo + hi) / 2
          if (ok(eval_at(exp(mid)))) lo <- mid else hi <- mid
          if (hi - lo < 1e-4) break
     }
     r <- eval_at(exp(lo)); r_up <- eval_at(exp(hi))
     binding <- if (!r_up$ok_ess && !r_up$ok_batch) "ess and batch" else if (!r_up$ok_ess) "ess" else "batch"
     list(eta = exp(lo), ess_kish = r$ess, batch_ratio_max = r$bmax, batch_ratio_median = r$bmed,
          binding = binding, note = note)
}

# Flat fields written to summary.json and weight_health.json.
.mosaic_weight_health_fields <- function(wh) {
     num <- function(x) if (is.null(x) || length(x) != 1L || !is.finite(x)) NA_real_ else as.numeric(x)
     list(
          ess_coarsened              = num(wh$coarsened$ess_kish),
          ess_coarsened_prop         = num(wh$coarsened$ess_prop),
          coarsening_alpha           = num(wh$coarsened$alpha),
          eta_ref                    = num(wh$coarsened$eta_ref),
          n_obs_scored               = num(wh$coarsened$n_obs),
          batch_ratio_coarsened_max  = num(wh$coarsened$batch_ratio_max),
          batch_ratio_coarsened_median = num(wh$coarsened$batch_ratio_median),
          eta_star                   = num(wh$eta_star$eta),
          eta_star_n_eff             = num(wh$eta_star$n_eff),
          eta_star_ess               = num(wh$eta_star$ess_kish),
          eta_star_batch_ratio_max   = num(wh$eta_star$batch_ratio_max),
          weights_used_ess_kish      = num(wh$weights_used$ess_kish),
          weights_used_ess_perplexity = num(wh$weights_used$ess_perplexity),
          weights_used_max_share     = num(wh$weights_used$max_share),
          weights_used_n             = if (length(wh$weights_used$n_nonzero) == 1L)
                                            as.integer(wh$weights_used$n_nonzero) else NA_integer_,
          weights_used_column        = if (length(wh$weights_used_column) == 1L)
                                            as.character(wh$weights_used_column) else NA_character_,
          weights_used_batch_ratio_max    = num(wh$weights_used$batch_ratio_max),
          weights_used_batch_ratio_median = num(wh$weights_used$batch_ratio_median),
          posterior_weighting        = if (length(wh$weighting) == 1L) as.character(wh$weighting) else NA_character_,
          resample_K                 = if (length(wh$resample$K) == 1L && !is.na(wh$resample$K))
                                            as.integer(wh$resample$K) else NA_integer_,
          resample_n_unique          = if (length(wh$resample$n_unique) == 1L && !is.na(wh$resample$n_unique))
                                            as.integer(wh$resample$n_unique) else NA_integer_,
          batch_ratio_target         = num(wh$targets$batch_max),
          weight_health_status       = if (length(wh$status) == 1L) as.character(wh$status) else NA_character_,
          weight_health_reasons      = if (length(wh$reasons) == 1L) as.character(wh$reasons) else NA_character_,
          likelihood_concentration_note = .mosaic_likelihood_concentration_note()
     )
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
