#' Simulation-convergence status of a calibration (green / yellow / red)
#'
#' Answers one question: are there enough simulations to represent the
#' model's best-fitting ensemble reliably? The status is the single rule behind
#' the adaptive stopping gate, \code{summary.json}, the calibration report and
#' the backfill (\code{\link{calc_run_weight_health}}). Whether the model
#' \emph{fits} the data (R², bias, coverage) is reported elsewhere and never
#' enters it.
#'
#' @description
#' Four checks, each coloured green, yellow or red; the overall status is the
#' \strong{worst} of them.
#'
#' \tabular{llll}{
#'   \strong{Check} \tab \strong{Green} \tab \strong{Yellow} \tab \strong{Red} \cr
#'   Ensemble ESS (\code{ess_best}) \tab \eqn{\ge} target \tab \eqn{\ge} 0.5 x target \tab below \cr
#'   Monte Carlo error (\code{mce_p90}) \tab \eqn{\le} 0.10 \tab \eqn{\le} 0.15 \tab above \cr
#'   Validity guard (\code{khat}) \tab < 0.5 \tab < 0.7 \tab \eqn{\ge} 0.7 \cr
#'   Data informativeness (\code{ess_untempered_frac}) \tab < 0.01 \tab \eqn{\ge} 0.01 (flat likelihood) \tab -- \cr
#' }
#'
#' \itemize{
#'   \item \strong{Ensemble ESS}: the Kish ESS of the best-fit ensemble. The
#'     Monte Carlo standard error of a posterior mean is about SD /
#'     \eqn{\sqrt{ESS}}, so 7 percent of the posterior SD at 200 and 10 percent
#'     at 100.
#'   \item \strong{Monte Carlo error}: the 90th percentile over parameters of
#'     the seed-batch standard error of the ensemble's posterior means, in
#'     posterior SDs (Flegal & Jones 2010). It is a standard error: two
#'     independent reruns differ by about \eqn{\sqrt{2}} times it (SD). The
#'     true value is nearly the same for every parameter (about
#'     \eqn{1/\sqrt{ESS_{best}}}), so the spread over parameters is mostly
#'     estimation noise and the 90th percentile is a conservative summary
#'     (about 1.3 times the typical value).
#'   \item \strong{Validity guard}: Pareto k-hat of the tempered importance
#'     weights (Vehtari et al. 2024). The tempered weights are bounded, so the
#'     true tail shape is \eqn{\le 0} and k-hat falls as draws accumulate; a
#'     high value means a few draws carry much of the weight at this number of
#'     draws. The reason states the k-hat trend (\code{khat_trend}): when
#'     falling, the simulations at which it would reach 0.5 at that rate and
#'     whether that is within \code{max_simulations}; "uncertain" when the
#'     trend is within twice its standard error of zero.
#'     An undefined k-hat is green only when the tail ratios are tied (weights
#'     nearly flat, \code{khat_cause} containing "tied"); any other cause is
#'     yellow, "not assessed".
#'   \item \strong{Data informativeness}: the Kish ESS of the untempered
#'     (\eqn{\eta = 1}) likelihood weights as a share of the valid draws. With
#'     informative data it collapses to about \eqn{1/N}; a share of at least
#'     \code{thresholds$flat_ess_frac} (default 0.01) means the likelihood is
#'     flat over a sizeable part of the prior, so the posterior is close to the
#'     prior. The share does not depend on the number of draws. The sampling
#'     can be adequate while the result is uninformative, so it is yellow,
#'     never red. A flat verdict also needs an untempered Kish ESS of at least
#'     20: the share is at least \eqn{1/N}, so on a small run a collapsed
#'     likelihood (ESS 1-3) would otherwise read as flat. It is not assessed
#'     (yellow) with fewer than two valid draws. Many draws tied at the best
#'     log-likelihood (a plateau, at least 10) add a note to the reason, and an
#'     undefined k-hat from too few distinct tail ratios then counts as tied.
#' }
#'
#' A run with fewer than two valid draws has failed: its first reason says so
#' (no posterior can be formed and more simulations of the same setup will not
#' help) and every check reads "not assessed".
#'
#' Missing values in the other checks are never green: an \code{NA}
#' \code{ess_best} is red (no ensemble), an \code{NA} \code{mce_p90} is yellow
#' ("not assessed"). An infinite \code{mce_p90} (fewer than two seed batches
#' hold members) is red.
#'
#' Every non-green check contributes one plain-language reason naming its
#' remedy. The ESS reason quotes the draws projected to reach the target,
#' \eqn{N \times ESS_{all}^{target} / ESS_{all}} (supply \code{n_draws},
#' \code{ess_all} and \code{ess_all_target}, or \code{n_draws_projected}); the
#' Monte Carlo error reason uses MCE \eqn{\propto 1/\sqrt{N}}. Both are
#' converted from valid draws to simulations at the run's success rate
#' (\code{n_draws / n_attempted}). A projection is reliable only when the ESS
#' growth slope is known and at least \code{thresholds$slope_stall}; every
#' reason that quotes one says so otherwise.
#'
#' Flags never change the colour: \code{budget_cap_reached},
#' \code{ess_growth_stalled} (\code{slope} below \code{thresholds$slope_stall}),
#' \code{ensemble_size_bound} (a member bound set the ensemble size) and
#' \code{fixed_budget} (the status is assessed, not used to stop).
#'
#' The function is pure (no I/O, no RNG) and takes scalars; apply it across
#' runs with \code{lapply()} / \code{Map()}.
#'
#' @param ess_best Kish ESS of the best-fit ensemble.
#' @param ess_best_target Required ensemble ESS (\code{control$targets$ESS_best}). Default 200.
#' @param mce_p90 90th percentile over parameters of the members' Monte Carlo error / posterior SD.
#' @param khat Pareto k-hat of the tempered weights (\code{NA} = undefined).
#' @param flat Optional logical flat-likelihood verdict, used only when \code{ess_untempered_frac} is \code{NA}.
#' @param ess_all Optional Kish ESS of the posterior over all draws.
#' @param ess_all_target Optional ESS_all target (\code{ess_best_target / best_subset_ratio}).
#' @param n_draws Optional valid draws (finite log-likelihood) so far.
#' @param n_draws_projected Optional total valid draws projected to reach the ESS target; computed from \code{n_draws}, \code{ess_all} and \code{ess_all_target} when \code{NA}.
#' @param slope Optional log-log ESS growth slope over the latter part of the run.
#' @param budget_exhausted Logical; the run stopped at \code{max_simulations_total}.
#' @param fixed_mode Logical; a fixed-budget run.
#' @param size_bound Which bound set the ensemble size: \code{"ratio"}, \code{"min_members"}, \code{"max_members"} (or \code{NA}).
#' @param eta_star Optional supported resolution \eqn{\eta^*} (a diagnostic; not used by the rule).
#' @param thresholds Named list overriding the band thresholds: \code{ess_yellow_frac} (0.5), \code{mce_green} (0.10), \code{mce_yellow} (0.15), \code{khat_green} (0.5), \code{khat_yellow} (0.7), \code{slope_stall} (0.8), \code{flat_ess_frac} (0.01).
#' @param ess_untempered_frac Optional Kish ESS of the untempered (\eqn{\eta = 1}) weights / valid draws.
#' @param n_tied_max Optional number of valid draws tied at the best log-likelihood.
#' @param khat_cause Optional reason k-hat is undefined (the \code{calc_is_diagnostics()} \code{khat_status}).
#' @param khat_trend Optional \code{"falling"}, \code{"not falling"} or \code{"uncertain"}: k-hat over growing numbers of draws (\code{.mosaic_khat_trend()}).
#' @param khat_trend_slope Optional change of k-hat per doubling of the draws, quoted when k-hat is falling.
#' @param n_attempted Optional simulations attempted (valid or not); converts projected valid draws to simulations.
#' @param max_simulations Optional simulation budget (\code{max_simulations_total}); the k-hat reason says whether its projection is within it.
#' @param batch_size_min,batch_size_max Bounds of the confirming batch in \code{sims_projected_for_green} (\code{batch_size_adaptive}, \code{max_batch_predictive}). Defaults 500 and 10,000.
#'
#' @return A list with
#' \describe{
#'   \item{\code{status}}{\code{"green"}, \code{"yellow"} or \code{"red"}.}
#'   \item{\code{label}}{The status text: "Converged: enough simulations",
#'     "Usable with caution" or "Not converged: more simulations needed".}
#'   \item{\code{checks}}{Data frame, one row per check (\code{check} is
#'     \code{"ess"}, \code{"mce"}, \code{"khat"} or
#'     \code{"informativeness"}): \code{label}, \code{value},
#'     \code{threshold_green}, \code{threshold_yellow} (\code{NA} for the
#'     informativeness check), \code{colour} and \code{reason}.}
#'   \item{\code{reasons}}{Character vector, one reason per non-green check.}
#'   \item{\code{flags}}{Character vector of flag identifiers, zero or more of
#'     \code{"budget_cap_reached"}, \code{"ess_growth_stalled"},
#'     \code{"ensemble_size_bound"} and \code{"fixed_budget"} (summary.json
#'     schema 2 \code{convergence_flags}).}
#'   \item{\code{flag_text}}{Character vector parallel to \code{flags}: the
#'     plain-language text of each flag (joined into
#'     \code{convergence_flags_text}).}
#'   \item{\code{n_draws_projected}, \code{n_draws_more}}{Projected total
#'     valid draws for the ESS target and the shortfall (\code{NA} when
#'     unknown).}
#'   \item{\code{sims_projected_for_green}}{Total simulations projected for
#'     GREEN (units: simulations): the larger of the ESS and Monte Carlo error
#'     projections that exceed the current valid draws, divided by the success
#'     rate, plus, in adaptive mode, one confirming batch (a quarter of that
#'     total within \code{[batch_size_min, batch_size_max]}: the second green
#'     check of the stopping rule; a fixed-budget run has none); \code{NA} when the three sampling checks are green or
#'     when no check that more draws can fix is behind.}
#'   \item{\code{projection_reliable}}{\code{FALSE} when the growth slope is
#'     unknown or below \code{thresholds$slope_stall}.}
#'   \item{\code{success_rate}}{\code{n_draws / n_attempted} (1 when unknown).}
#'   \item{\code{sampling_green}}{\code{TRUE} when the three sampling checks
#'     (ensemble ESS, Monte Carlo error, k-hat) are green, whatever the
#'     informativeness check: the adaptive stopping condition.}
#'   \item{\code{flat}}{The flat-likelihood verdict (\code{NA} when not
#'     assessed).}
#'   \item{\code{thresholds}}{The thresholds used.}
#' }
#'
#' @references
#' Vehtari A, Gelman A, Simpson D, Carpenter B, Bürkner P-C (2021).
#' Rank-normalization, folding, and localization: an improved R-hat for
#' assessing convergence of MCMC. \emph{Bayesian Analysis} 16(2):667-718.
#'
#' Vehtari A, Simpson D, Gelman A, Yao Y, Gabry J (2024). Pareto smoothed
#' importance sampling. \emph{JMLR} 25(72):1-58.
#'
#' Flegal JM, Jones GL (2010). Batch means and spectral variance estimators in
#' Markov chain Monte Carlo. \emph{Annals of Statistics} 38(2):1034-1070.
#'
#' @examples
#' st <- calc_convergence_status(ess_best = 140, ess_best_target = 200,
#'                               mce_p90 = 0.08, khat = 0.2,
#'                               ess_untempered_frac = 2e-5,
#'                               ess_all = 700, ess_all_target = 1000,
#'                               n_draws = 50000, slope = 0.98)
#' st$status
#' st$reasons
#' st$checks
#'
#' @seealso \code{\link{calc_weight_health}}, \code{\link{calc_run_weight_health}}
#' @family calibration-metrics
#' @export
calc_convergence_status <- function(ess_best, ess_best_target = 200, mce_p90, khat, flat = NA,
                                    ess_all = NA_real_, ess_all_target = NA_real_,
                                    n_draws = NA_real_, n_draws_projected = NA_real_,
                                    slope = NA_real_, budget_exhausted = FALSE,
                                    fixed_mode = FALSE, size_bound = NA_character_,
                                    eta_star = NA_real_, thresholds = list(),
                                    ess_untempered_frac = NA_real_, n_tied_max = NA_real_,
                                    khat_cause = NA_character_, khat_trend = NA_character_,
                                    khat_trend_slope = NA_real_, n_attempted = NA_real_,
                                    max_simulations = NA_real_, batch_size_min = 500,
                                    batch_size_max = 10000) {

     num1 <- function(x, nm, positive = FALSE) {
          if (is.null(x)) x <- NA_real_
          if (length(x) != 1L || !(is.numeric(x) || (is.logical(x) && is.na(x))))
               stop("calc_convergence_status(): ", nm, " must be a single number (or NA).", call. = FALSE)
          x <- as.numeric(x)
          if (positive && is.finite(x) && x <= 0)
               stop("calc_convergence_status(): ", nm, " must be positive.", call. = FALSE)
          x
     }
     lgl1 <- function(x, nm) {
          if (is.null(x)) x <- NA
          if (length(x) != 1L || !(is.logical(x) || (is.numeric(x) && x %in% c(0, 1))))
               stop("calc_convergence_status(): ", nm, " must be a single logical (or NA).", call. = FALSE)
          as.logical(x)
     }
     chr1 <- function(x, nm) {
          if (is.null(x) || (length(x) == 1L && is.na(x))) return(NA_character_)
          if (length(x) != 1L || !is.character(x))
               stop("calc_convergence_status(): ", nm, " must be a single string (or NA).", call. = FALSE)
          x
     }
     ess_best <- num1(ess_best, "ess_best")
     ess_best_target <- num1(ess_best_target, "ess_best_target", positive = TRUE)
     if (!is.finite(ess_best_target))
          stop("calc_convergence_status(): ess_best_target must be a positive finite number.", call. = FALSE)
     mce_p90 <- num1(mce_p90, "mce_p90")
     khat <- num1(khat, "khat")
     flat <- lgl1(flat, "flat")
     ess_all <- num1(ess_all, "ess_all")
     ess_all_target <- num1(ess_all_target, "ess_all_target")
     n_draws <- num1(n_draws, "n_draws")
     n_draws_projected <- num1(n_draws_projected, "n_draws_projected")
     slope <- num1(slope, "slope")
     eta_star <- num1(eta_star, "eta_star")
     ess_untempered_frac <- num1(ess_untempered_frac, "ess_untempered_frac")
     n_tied_max <- num1(n_tied_max, "n_tied_max")
     n_attempted <- num1(n_attempted, "n_attempted")
     khat_trend_slope <- num1(khat_trend_slope, "khat_trend_slope")
     max_simulations <- num1(max_simulations, "max_simulations")
     batch_size_min <- num1(batch_size_min, "batch_size_min")
     batch_size_max <- num1(batch_size_max, "batch_size_max")
     # A run with fewer than two valid draws has failed: no posterior exists, and
     # more simulations of the same setup are not the remedy (round 2, RN5).
     failed <- is.finite(n_draws) && n_draws < 2
     fail_txt <- "fewer than two draws have a finite log-likelihood"
     khat_cause <- chr1(khat_cause, "khat_cause")
     khat_trend <- chr1(khat_trend, "khat_trend")
     budget_exhausted <- isTRUE(lgl1(budget_exhausted, "budget_exhausted"))
     fixed_mode <- isTRUE(lgl1(fixed_mode, "fixed_mode"))
     if (is.null(size_bound) || (length(size_bound) == 1L && is.na(size_bound))) size_bound <- NA_character_
     if (length(size_bound) != 1L || !(is.character(size_bound) || is.logical(size_bound)))
          stop("calc_convergence_status(): size_bound must be a single string (or NA).", call. = FALSE)
     if (is.logical(size_bound)) size_bound <- if (isTRUE(size_bound)) "bound" else NA_character_
     th <- .mosaic_convergence_thresholds(thresholds)

     if (!is.finite(n_draws_projected))
          n_draws_projected <- .mosaic_projected_draws(n_draws, ess_all, ess_all_target)
     n_draws_more <- if (is.finite(n_draws_projected) && is.finite(n_draws))
          max(0, n_draws_projected - n_draws) else NA_real_
     # Valid draws -> simulations: the run's success rate (valid / attempted).
     rate <- if (is.finite(n_draws) && is.finite(n_attempted) && n_draws > 0 && n_attempted >= n_draws)
          n_draws / n_attempted else 1
     stalled <- is.finite(slope) && slope < th$slope_stall
     reliable <- .mosaic_projection_reliable(slope, th$slope_stall)
     proj_note <- if (reliable) "" else if (stalled)
          sprintf(" (unreliable: ESS growth slope %.2f < %s)", slope, th$slope_stall)
          else " (indicative only: the ESS growth slope is not yet known)"
     stall_txt <- .mosaic_stall_remedy_text()

     # ---- colours ---------------------------------------------------------------
     ess_yellow <- th$ess_yellow_frac * ess_best_target
     ess_col <- if (!is.finite(ess_best)) "red"
                else if (ess_best >= ess_best_target) "green"
                else if (ess_best >= ess_yellow) "yellow" else "red"
     mce_col <- if (is.na(mce_p90)) "yellow"
                else if (mce_p90 <= th$mce_green) "green"
                else if (mce_p90 <= th$mce_yellow) "yellow" else "red"
     # Undefined k-hat is green for tied tail ratios, and for "fewer than 5
     # distinct tail ratios" on a plateau of tied best draws (round 2, TN1).
     plateau_on <- is.finite(n_tied_max) && n_tied_max >= .mosaic_plateau_min_ties()
     khat_tied <- !is.finite(khat) && !is.na(khat_cause) &&
          (grepl("tied", khat_cause, fixed = TRUE) ||
           (plateau_on && grepl("fewer than 5 distinct", khat_cause, fixed = TRUE)))
     khat_col <- if (!is.finite(khat)) (if (khat_tied) "green" else "yellow")
                 else if (khat < th$khat_green) "green"
                 else if (khat < th$khat_yellow) "yellow" else "red"
     # Informativeness: the share at or above flat_ess_frac AND an untempered
     # Kish ESS of at least .mosaic_flat_min_ess() (20). The share is at least
     # 1/N, so on a small run a collapsed likelihood (ESS 1-3) reaches the share;
     # the absolute floor keeps it from reading as flat (round 2, PN6).
     ess1 <- if (is.finite(ess_untempered_frac) && is.finite(n_draws)) ess_untempered_frac * n_draws else NA_real_
     flat <- if (failed) NA
             else if (is.finite(ess_untempered_frac))
                  ess_untempered_frac >= th$flat_ess_frac && (!is.finite(ess1) || ess1 >= .mosaic_flat_min_ess() - 1e-9)
             else flat
     flat_col <- if (is.na(flat) || flat) "yellow" else "green"
     cols <- c(ess = ess_col, mce = mce_col, khat = khat_col, informativeness = flat_col)
     status <- c("green", "yellow", "red")[max(match(cols, c("green", "yellow", "red")))]

     # ---- projections -----------------------------------------------------------
     mce_proj <- if (is.finite(mce_p90) && is.finite(n_draws) && n_draws > 0)
          n_draws * (mce_p90 / th$mce_green)^2 else NA_real_
     comps <- c(if (ess_col != "green") n_draws_projected, if (mce_col != "green") mce_proj)
     comps <- comps[is.finite(comps) & is.finite(n_draws) & comps > n_draws]
     sampling_green <- all(cols[c("ess", "mce", "khat")] == "green")
     # Simulations for GREEN, plus one confirming batch (the second green check
     # of the stopping rule: a geometric step, a quarter of the projected total
     # within [batch_size_min, batch_size_max]). Null when the sampling checks
     # are green: an informativeness-only yellow needs no more simulations.
     # A fixed-budget run has no stopping rule, so no confirming batch (round 3).
     sims_green <- if (sampling_green || !length(comps)) NA_real_ else {
          tot <- max(comps) / rate
          if (fixed_mode) tot
          else tot + min(max(0.25 * tot, batch_size_min, na.rm = TRUE), batch_size_max, na.rm = TRUE)
     }
     more_sims <- function(valid_more) valid_more / rate

     # ---- ensemble ESS ----------------------------------------------------------
     ess_reason <- if (failed) {
          paste0("ensemble ESS not computed: ", fail_txt)
     } else if (!is.finite(ess_best)) {
          "ensemble ESS could not be computed (no valid posterior weights): check the scored observations and the log-likelihoods"
     } else if (ess_col == "green") {
          sprintf("ensemble ESS %s >= %s", .mosaic_fmt_count(ess_best), .mosaic_fmt_count(ess_best_target))
     } else {
          more <- if (is.finite(n_draws_more) && n_draws_more > 0) {
               sprintf("about %s more simulations %s%s", .mosaic_fmt_draws(more_sims(n_draws_more)),
                       if (ess_col == "yellow") "would reach the target" else "needed", proj_note)
          } else if (identical(size_bound, "max_members")) {
               "the ensemble is at the max_members bound: raise max_ensemble_members or run more simulations"
          } else "run more simulations"
          txt <- sprintf("ensemble ESS %s of %s: %s", .mosaic_fmt_count(ess_best),
                         .mosaic_fmt_count(ess_best_target), more)
          if (stalled) txt <- paste0(txt, "; ESS growth has stalled, so ", stall_txt)
          txt
     }

     # ---- Monte Carlo error -----------------------------------------------------
     mce_reason <- if (failed) {
          paste0("Monte Carlo error not assessed: ", fail_txt)
     } else if (is.na(mce_p90)) {
          "Monte Carlo error not assessed (no parameter varies among the ensemble members)"
     } else if (is.infinite(mce_p90)) {
          "Monte Carlo error cannot be estimated: fewer than two seed batches hold ensemble members; run more simulations"
     } else if (mce_col == "green") {
          sprintf("Monte Carlo standard error of the posterior means %s of the posterior SD (90th percentile over parameters) <= %s",
                  .mosaic_fmt_pct(mce_p90), .mosaic_fmt_pct(th$mce_green))
     } else {
          more <- if (is.finite(mce_proj)) mce_proj - n_draws else NA_real_
          sprintf(paste0("Monte Carlo standard error of the posterior means is %s of the posterior SD ",
                         "(90th percentile over parameters; target %s): %s"),
                  .mosaic_fmt_pct(mce_p90), .mosaic_fmt_pct(th$mce_green),
                  if (is.finite(more) && more > 0)
                       sprintf("about %s more simulations would bring it to the target%s",
                               .mosaic_fmt_draws(more_sims(more)), proj_note)
                  else "run more simulations")
     }

     # ---- validity guard (k-hat) ------------------------------------------------
     # The k-hat trend (round 2, TN2/RN4): "uncertain" when the slope is within
     # 2 SE of zero; when falling, the draws at which the trend would reach
     # khat_green, said to be within or beyond the budget, never promised; with
     # stalled ESS growth the inefficiency remedy is repeated, not contradicted.
     trend_txt <- function(red) {
          stall_add <- if (stalled) "; ESS growth has stalled, so more simulations alone remain inefficient" else ""
          if (identical(khat_trend, "falling") && is.finite(khat_trend_slope) && khat_trend_slope < 0) {
               rate_d <- -khat_trend_slope
               at <- if (is.finite(n_draws) && n_draws > 0)
                    n_draws * 2^((khat - th$khat_green) / rate_d) / rate else NA_real_
               cap_txt <- if (!is.finite(at)) ""
                    else if (is.finite(max_simulations))
                         if (at <= max_simulations) sprintf(", within the max_simulations_total budget of %s",
                                                            .mosaic_fmt_count(max_simulations))
                         else sprintf(", beyond the max_simulations_total budget of %s",
                                      .mosaic_fmt_count(max_simulations))
                    else ""
               paste0(sprintf("; k-hat is falling as draws accumulate (by about %.2f per doubling of the draws)",
                              rate_d),
                      if (is.finite(at)) sprintf(": at that rate it would reach %s at about %s simulations%s",
                                                 th$khat_green, .mosaic_fmt_draws(at), cap_txt) else "",
                      stall_add)
          } else if (identical(khat_trend, "not falling")) {
               if (red || stalled) paste0("; k-hat is not falling as draws accumulate: ", stall_txt)
               else "; k-hat is not falling as draws accumulate: run more simulations and check that it falls"
          } else if (identical(khat_trend, "uncertain")) {
               paste0("; the k-hat trend over the latter part of the run is uncertain (within twice its ",
                      "standard error of zero): run more simulations and check that it falls", stall_add)
          } else if (red) paste0("; the tempered weights are bounded, so k-hat usually falls as draws ",
                                 "accumulate: run more simulations and check that it falls", stall_add)
          else paste0("; run more simulations and check that k-hat falls", stall_add)
     }
     khat_reason <- if (failed) {
          paste0("Pareto k-hat of the tempered weights not assessed: ", fail_txt)
     } else if (!is.finite(khat)) {
          if (khat_tied) "undefined (weights nearly flat)"
          else sprintf("Pareto k-hat of the tempered weights not assessed (%s)",
                       .mosaic_khat_cause_text(khat_cause))
     } else if (khat_col == "green") {
          sprintf("Pareto k-hat of the tempered weights %.2f < %s", khat, th$khat_green)
     } else if (khat_col == "yellow") {
          paste0(sprintf(paste0("Pareto k-hat of the tempered weights is %.2f (green < %s, red >= %s): the ",
                                "largest weights form a heavy tail at this number of draws, so the ESS and the ",
                                "Monte Carlo error may be optimistic"), khat, th$khat_green, th$khat_yellow),
                 trend_txt(FALSE))
     } else {
          paste0(sprintf(paste0("Pareto k-hat of the tempered weights is %.2f (>= %s): a few draws dominate ",
                                "the weights, so the ESS and the Monte Carlo error are unreliable"),
                         khat, th$khat_yellow),
                 trend_txt(TRUE))
     }

     # ---- data informativeness --------------------------------------------------
     plateau <- if (plateau_on)
          sprintf("; %s draws tie at the best log-likelihood (a plateau: the likelihood cannot rank them)",
                  .mosaic_fmt_count(n_tied_max)) else ""
     flat_reason <- if (is.na(flat)) {
          if (failed) paste0("data informativeness not assessed: ", fail_txt)
          else "data informativeness not assessed"
     } else if (flat) {
          paste0(if (is.finite(ess_untempered_frac))
                      sprintf(paste0("the data carry little information: the posterior is close to the prior ",
                                     "(flat likelihood: the untempered likelihood spreads its weight over %s ",
                                     "of the draws, >= %s)"),
                              .mosaic_fmt_frac(ess_untempered_frac), .mosaic_fmt_frac(th$flat_ess_frac))
                 else "the data carry little information: the posterior is close to the prior (flat likelihood)",
                 plateau)
     } else {
          paste0(if (is.finite(ess_untempered_frac))
                      if (ess_untempered_frac >= th$flat_ess_frac)
                           sprintf(paste0("the data inform the posterior (the untempered likelihood ESS is %s, ",
                                          "below the %s needed to call it flat)"),
                                   .mosaic_fmt_draws(ess1), .mosaic_flat_min_ess())
                      else sprintf("the data inform the posterior (the untempered likelihood ESS is %s of the draws, < %s)",
                                   .mosaic_fmt_frac(ess_untempered_frac), .mosaic_fmt_frac(th$flat_ess_frac))
                 else "the data inform the posterior",
                 plateau)
     }

     checks <- data.frame(
          check = names(cols),
          label = c("Ensemble ESS", "Monte Carlo error (90th percentile)",
                    "Validity guard (Pareto k-hat)", "Data informativeness"),
          value = c(ess_best, mce_p90, khat, ess_untempered_frac),
          threshold_green = c(ess_best_target, th$mce_green, th$khat_green, th$flat_ess_frac),
          threshold_yellow = c(ess_yellow, th$mce_yellow, th$khat_yellow, NA_real_),
          colour = unname(cols),
          reason = c(ess_reason, mce_reason, khat_reason, flat_reason),
          stringsAsFactors = FALSE)

     flags <- .mosaic_convergence_flags(budget_exhausted = budget_exhausted, stalled = stalled,
                                        size_bound = size_bound, fixed_mode = fixed_mode,
                                        n_attempted = n_attempted, n_draws = n_draws, slope = slope,
                                        slope_stall = th$slope_stall)

     list(status = status,
          label = switch(status, green = "Converged: enough simulations",
                         yellow = "Usable with caution",
                         red = "Not converged: more simulations needed"),
          checks = checks,
          reasons = c(if (failed) sprintf(paste0("the run failed: only %s of %s simulations %s a finite ",
                                                 "likelihood, so no posterior or ensemble can be formed; more ",
                                                 "simulations of the same setup will not help: check the scored ",
                                                 "observations, the priors and the simulation logs"),
                                          .mosaic_fmt_count(n_draws),
                                          if (is.finite(n_attempted)) .mosaic_fmt_count(n_attempted) else "the",
                                          if (n_draws == 1) "has" else "have"),
                      checks$reason[checks$colour != "green"]),
          flags = names(flags) %||% character(0),
          flag_text = unname(as.character(unlist(flags))) %||% character(0),
          n_draws_projected = n_draws_projected,
          n_draws_more = n_draws_more,
          sims_projected_for_green = sims_green,
          projection_reliable = reliable,
          success_rate = rate,
          sampling_green = sampling_green,
          flat = flat,
          thresholds = th)
}


# The remedy quoted by the stalled-growth reason and flag.
.mosaic_stall_remedy_text <- function() {
     "more simulations alone are inefficient; use a sharper sampler or review the priors"
}

#' Convergence flags and their plain-language text
#'
#' The flags of \code{calc_convergence_status()}, in their fixed order, each
#' with its text. Shared by the status and by the 1.5.1 upgrade of an older
#' calibration_diagnostics.json, which regenerates each flag's text from the
#' numbers in the file (the file joined them into one string, and a flag's own
#' text can contain "; ").
#' @param budget_exhausted,stalled,fixed_mode Logical.
#' @param size_bound \code{"min_members"}, \code{"max_members"}, \code{"bound"}
#'   or \code{NA}.
#' @param n_attempted,n_draws Simulations attempted and valid draws (the
#'   budget flag quotes the first, else the second).
#' @param slope,slope_stall ESS growth slope and its stall threshold.
#' @return Named list, flag id -> text (empty when no flag is raised).
#' @noRd
.mosaic_convergence_flags <- function(budget_exhausted, stalled, size_bound, fixed_mode,
                                      n_attempted = NA_real_, n_draws = NA_real_, slope = NA_real_,
                                      slope_stall = 0.8) {
     flags <- stats::setNames(list(), character(0))
     if (isTRUE(budget_exhausted))
          flags[["budget_cap_reached"]] <- if (is.finite(n_attempted) || is.finite(n_draws))
               sprintf("budget cap reached: %s simulations (max_simulations_total)",
                       .mosaic_fmt_count(if (is.finite(n_attempted)) n_attempted else n_draws))
          else "budget cap reached (max_simulations_total)"
     if (isTRUE(stalled))
          flags[["ess_growth_stalled"]] <- sprintf(
               "stalled ESS growth: log-log slope %.2f < %s over the latter part of the run; %s",
               slope, slope_stall, .mosaic_stall_remedy_text())
     if (length(size_bound) == 1L && !is.na(size_bound) && size_bound %in% c("min_members", "max_members", "bound"))
          flags[["ensemble_size_bound"]] <- switch(size_bound,
               min_members = "ensemble at the min_members bound",
               max_members = "ensemble at the max_members bound (the ESS target is not reached within it)",
               bound = "ensemble at a member bound")
     if (isTRUE(fixed_mode))
          flags[["fixed_budget"]] <- "fixed-budget run: the status is assessed, not used to stop"
     flags
}

#' Is a draw projection reliable?
#'
#' The projections \eqn{N \times target / ESS_{all}} and
#' \eqn{N (MCE / MCE_{green})^2} assume the ESS grows in proportion to the
#' draws. They are reliable when the log-log growth slope is known and at
#' least \code{slope_stall}. k-hat does not enter: the tempered weights are
#' bounded, so the ESS keeps growing linearly even while k-hat is high
#' (red-team round 1, T3).
#' @param slope Log-log ESS growth slope (\code{NA} = unknown).
#' @param slope_stall Stall threshold (\code{status_thresholds$slope_stall}). Default 0.8.
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
.mosaic_projection_reliable <- function(slope, slope_stall = 0.8) {
     slope <- suppressWarnings(as.numeric(slope))
     length(slope) == 1L && is.finite(slope) && slope >= slope_stall
}

#' The adaptive stopping decision: sampling GREEN at two consecutive checks
#'
#' An adaptive run stops when, at its two latest convergence checks, the three
#' sampling checks (ensemble ESS, Monte Carlo error, k-hat) are all green and
#' the minimum run length is met at the latest. The informativeness check does
#' not block the stop, because more simulations cannot fix a flat likelihood;
#' it still makes the overall status yellow. One green check is not enough:
#' stopping at the first crossing selects a favourable fluctuation, so a fresh
#' rerun at that size is green only 73-80 percent of the time on the
#' borderline production models, 99.9 percent with two (red-team round 1, T6,
#' D5). Checks at the same draw count (a resume re-deriving an entry) count
#' once (the latest).
#' @param history Convergence history: a list of entries with \code{n} and
#'   \code{sampling_green} (run_state.json \code{convergence_history}). An
#'   entry without \code{sampling_green} (an older checkpoint) counts as
#'   sampling-green when its \code{status} is \code{"green"}.
#' @param min_ok Logical; the minimum run length (\code{min_batches_adaptive}
#'   and \code{min_draws}) is met at the latest check.
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
.mosaic_stop_rule <- function(history, min_ok) {
     if (!isTRUE(min_ok) || length(history) < 2L) return(FALSE)
     n <- vapply(history, function(e) suppressWarnings(as.numeric(unlist(e$n %||% NA))[1]), numeric(1))
     ok <- vapply(history, function(e) {
          sg <- suppressWarnings(as.logical(unlist(e$sampling_green))[1])
          if (length(sg) == 1L && !is.na(sg)) sg
          else identical(as.character(unlist(e$status %||% NA_character_))[1], "green")
     }, logical(1))
     ok <- ok[!duplicated(n, fromLast = TRUE) | is.na(n)]
     k <- length(ok)
     k >= 2L && ok[k] && ok[k - 1L]
}

#' Are the three sampling checks of a weight-health result green?
#'
#' The \code{sampling_green} field of a convergence-history entry: ensemble
#' ESS, Monte Carlo error and k-hat all green (informativeness ignored).
#' @param wh A \code{calc_weight_health()} result (or one read back from JSON).
#' @return \code{TRUE} or \code{FALSE} (\code{FALSE} when no status exists).
#' @noRd
.mosaic_sampling_green <- function(wh) {
     cv <- wh$convergence
     sg <- suppressWarnings(as.logical(unlist(cv$sampling_green))[1])
     if (length(sg) == 1L && !is.na(sg)) return(sg)
     ck <- cv$checks
     if (is.null(ck)) return(FALSE)
     col <- if (is.data.frame(ck)) stats::setNames(ck$colour, ck$check)
            else stats::setNames(vapply(ck, function(r) as.character(unlist(r$colour))[1], character(1)),
                                 vapply(ck, function(r) as.character(unlist(r$check))[1], character(1)))
     all(col[c("ess", "mce", "khat")] %in% "green")
}

# Draws tied at the best log-likelihood from which the informativeness reason
# notes a plateau (and an undefined k-hat on too few distinct tail ratios is
# read as tied).
.mosaic_plateau_min_ties <- function() 10L

# Smallest untempered Kish ESS for a flat verdict (round 2, PN6): below it the
# share ESS / N is noise from a handful of draws.
.mosaic_flat_min_ess <- function() 20

# Why k-hat is undefined, without a doubled "not assessed" (round 2, CN4).
.mosaic_khat_cause_text <- function(cause) {
     cause <- if (length(cause) == 1L && !is.na(cause)) trimws(sub("^not assessed:?\\s*", "", cause)) else ""
     if (nzchar(cause)) cause else "the tail could not be fitted"
}


# ---------------------------------------------------------------------------
# Defaults and formatting helpers
# ---------------------------------------------------------------------------

#' Defaults of the 1.4.0 best-fit ensemble and convergence status
#'
#' The single source of the SPEC defaults (SPEC_CONVERGENCE.md sections 3 and
#' 5). \code{calc_weight_health()}'s formals carry the same values (pinned by a
#' test), and control defaults should be read from here.
#'
#' \code{flat_ess_frac = 0.01} (red-team round 1, D1): on sim-order prefixes of
#' the 28 national production pools at 7k, 15k, 30k and 50k draws, the
#' untempered-ESS share is 0.052-0.054 for BFA (flat) and at most 2.7e-4 for
#' the other 27 (about 1/N), so 0.01 flags BFA at every size with a 5x margin
#' and none of the others with a 37x margin
#' (claude/redteam/theory/r1/12_flat_frac.R).
#' @return A list: \code{best_subset_ratio}, \code{ESS_best},
#'   \code{min_ensemble_members}, \code{max_ensemble_members} and
#'   \code{thresholds}.
#' @noRd
.mosaic_convergence_defaults <- function() {
     list(best_subset_ratio = 0.2, ESS_best = 200, min_ensemble_members = 30L,
          max_ensemble_members = 1000L,
          thresholds = list(ess_yellow_frac = 0.5, mce_green = 0.10, mce_yellow = 0.15,
                            khat_green = 0.5, khat_yellow = 0.7, slope_stall = 0.8,
                            flat_ess_frac = 0.01))
}

#' Band thresholds of the convergence status, defaults merged with overrides
#' @param thresholds Named list of overrides (names from the defaults).
#' @return The complete, validated threshold list.
#' @noRd
.mosaic_convergence_thresholds <- function(thresholds = list()) {
     th <- .mosaic_convergence_defaults()$thresholds
     if (is.null(thresholds)) thresholds <- list()
     if (!is.list(thresholds) || (length(thresholds) && is.null(names(thresholds))))
          stop("thresholds must be a named list.", call. = FALSE)
     bad <- setdiff(names(thresholds), names(th))
     if (length(bad))
          stop("unknown convergence threshold(s): ", paste(bad, collapse = ", "),
               " (valid: ", paste(names(th), collapse = ", "), ").", call. = FALSE)
     for (nm in names(thresholds)) {
          v <- thresholds[[nm]]
          if (length(v) != 1L || !is.numeric(v) || !is.finite(v) || v <= 0)
               stop("convergence threshold ", nm, " must be a single positive number.", call. = FALSE)
          th[[nm]] <- as.numeric(v)
     }
     if (th$ess_yellow_frac > 1) stop("convergence threshold ess_yellow_frac must be in (0, 1].", call. = FALSE)
     if (th$flat_ess_frac > 1) stop("convergence threshold flat_ess_frac must be in (0, 1].", call. = FALSE)
     if (th$mce_green > th$mce_yellow) stop("convergence thresholds need mce_green <= mce_yellow.", call. = FALSE)
     if (th$khat_green > th$khat_yellow) stop("convergence thresholds need khat_green <= khat_yellow.", call. = FALSE)
     th
}

# Whole-number display with thousands separators ("1,234").
.mosaic_fmt_count <- function(x) formatC(round(x), format = "d", big.mark = ",")

# Percentage with one decimal ("12.3%"); values of 1,000% or more (a
# near-degenerate estimate) print as "more than 1,000%".
.mosaic_fmt_pct <- function(x) {
     if (is.finite(x) && x >= 10) return("more than 1,000%")
     sprintf("%.1f%%", 100 * x)
}

# Small shares as a percentage with two significant digits ("5.3%", "0.002%").
.mosaic_fmt_frac <- function(x) paste0(format(signif(100 * x, 2), scientific = FALSE, drop0trailing = TRUE), "%")

# Draw or simulation counts at two significant digits (three from 100k):
# "850", "1,000", "21k", "117k", "1.2M". Never rounded up to the next unit.
.mosaic_fmt_draws <- function(x) {
     if (!is.finite(x)) return("NA")
     x <- max(0, x)
     if (x < 100) return(as.character(round(x)))
     # Round first, then choose the unit, so a value that rounds up to the next
     # unit prints in that unit (9,999 -> "10k", 999,999 -> "1.0M", not "1000k").
     if (x < 1e4) {
          r <- signif(x, 2)
          if (r < 1e4) return(formatC(r, format = "d", big.mark = ","))
          x <- r
     }
     if (x < 1e6) {
          r <- signif(x / 1e3, if (x < 1e5) 2 else 3)
          if (r < 1e3) return(paste0(format(r, scientific = FALSE), "k"))
          x <- r * 1e3
     }
     paste0(format(signif(x / 1e6, 2), nsmall = 1), "M")
}

#' Calibration metric glossary
#'
#' Reads \code{inst/extdata/metric_glossary.yml}, the single source of the
#' metric definitions, formulas, target templates and fixed wording shown by
#' the calibration report, its appendix, the docs and the dashboard (schema at
#' the top of the file). Every entry must carry the fields of
#' \code{.mosaic_glossary_fields()}; a formula must parse as plotmath and every
#' rich-text token it names must be defined.
#' @param path Glossary file. Default: the installed package copy.
#' @return A data frame, one row per metric, with the columns of
#'   \code{.mosaic_glossary_fields()} (\code{formula_plotmath} and
#'   \code{formula_katex} are \code{""} for a metric without a formula;
#'   \code{target_template} and \code{target_note} are \code{NA} when absent);
#'   attributes \code{"version"}, \code{"tokens"} (named list of
#'   \code{text}/\code{ascii}/\code{plotmath}/\code{katex}), \code{"report"}
#'   (the report's fixed wording) and \code{"retired"} (the retired-metrics
#'   note).
#' @noRd
.mosaic_glossary <- function(path = system.file("extdata", "metric_glossary.yml", package = "MOSAIC")) {
     if (!nzchar(path) || !file.exists(path))
          stop("metric glossary not found: ", path, call. = FALSE)
     g <- yaml::read_yaml(path)
     if (!is.list(g$metrics) || !length(g$metrics))
          stop("metric glossary has no 'metrics' entries.", call. = FALSE)
     req <- .mosaic_glossary_required()
     may_empty <- c("formula_plotmath", "formula_katex")
     optional <- c("target_template", "target_note")
     is_str <- function(v) length(v) == 1L && is.character(v)
     rows <- lapply(seq_along(g$metrics), function(i) {
          e <- g$metrics[[i]]
          miss <- req[!vapply(req, function(f) {
               v <- e[[f]]
               is_str(v) && (f %in% may_empty || nzchar(trimws(v)))
          }, logical(1))]
          if (length(miss))
               stop(sprintf("metric glossary entry %d (%s) lacks: %s", i, e$key %||% "?",
                            paste(miss, collapse = ", ")), call. = FALSE)
          for (f in optional) {
               v <- e[[f]]
               if (!is.null(v) && !is_str(v))
                    stop(sprintf("metric glossary entry %d (%s): %s must be a string or null", i, e$key, f),
                         call. = FALSE)
               e[[f]] <- if (is.null(v)) NA_character_ else v
          }
          if (nzchar(e$formula_plotmath))
               tryCatch(str2expression(e$formula_plotmath), error = function(err)
                    stop(sprintf("metric glossary entry %s: formula_plotmath does not parse: %s", e$key,
                                 conditionMessage(err)), call. = FALSE))
          as.data.frame(e[.mosaic_glossary_fields()], stringsAsFactors = FALSE)
     })
     out <- do.call(rbind, rows)
     if (anyDuplicated(out$key))
          stop("metric glossary has duplicate keys: ",
               paste(unique(out$key[duplicated(out$key)]), collapse = ", "), call. = FALSE)
     tokens <- g$tokens %||% list()
     for (nm in names(tokens)) {
          tk <- tokens[[nm]]
          if (!all(vapply(c("text", "ascii", "plotmath", "katex"), function(f) is_str(tk[[f]]), logical(1))))
               stop("metric glossary token '", nm, "' needs text, ascii, plotmath and katex.", call. = FALSE)
          tryCatch(str2lang(tk$plotmath), error = function(err)
               stop("metric glossary token '", nm, "': plotmath does not parse.", call. = FALSE))
     }
     used <- unlist(regmatches(c(out$short_label, out$what_it_tells_you, out$target_template),
                               gregexpr("\\{[A-Za-z0-9_]+\\}", c(out$short_label, out$what_it_tells_you,
                                                                  out$target_template))))
     unknown <- setdiff(gsub("[{}]", "", used), c(names(tokens), .mosaic_glossary_placeholders()))
     if (length(unknown))
          stop("metric glossary uses undefined token(s): ", paste(unique(unknown), collapse = ", "), call. = FALSE)
     attr(out, "version") <- g$version
     attr(out, "tokens") <- tokens
     attr(out, "report") <- g$report %||% list()
     attr(out, "retired") <- g$retired_metrics %||% list()
     out
}

# Fields every glossary entry carries (the first seven are the 1.4.0 schema).
.mosaic_glossary_fields <- function() {
     c("key", "label", "definition", "formula", "how_to_read", "threshold", "not_a_measure_of",
       "short_label", "what_it_tells_you", "formula_plotmath", "formula_katex", "target_template",
       "target_note", "units")
}

# The fields an entry must define (target_template and target_note may be absent).
.mosaic_glossary_required <- function() setdiff(.mosaic_glossary_fields(), c("target_template", "target_note"))

# Placeholders filled from calibration_diagnostics.json (metric_glossary.yml header).
.mosaic_glossary_placeholders <- function() c("op", "op_yellow", "green", "yellow", "target", "min", "max")
