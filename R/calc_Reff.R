# -----------------------------------------------------------------------------
# Cori effective reproductive number (R_eff) on simulated infection incidence
# -----------------------------------------------------------------------------
# Analytic reduction over the ensemble trajectories the run already captured
# (no extra simulation). The estimand is the Cori (2013) instantaneous INFECTION
# effective reproductive number: secondary infections per infection, computed
# from the realised S->E infection-incidence flow (the captured `incidence`
# channel = incidence_human + incidence_env), with both infectious classes
# counted at EQUAL weight. Infectiousness heterogeneity is carried by the
# moment-matched generation-interval kernel g, NOT by an incidence weight.
#
# Canonical theory: MOSAIC-docs/04-model-description.Rmd, eq:R / eq:I-star and
# the generation-interval moment eqs (gamma-eff / generation-time-moments /
# infectious-period-variance / gamma-shape-rate).
#
# CAVEAT (carried in roxygen + provenance): this R_eff is computed on SIMULATED
# incidence, so it is a DESCRIPTOR of the model trajectory (directly comparable
# to a surveillance-derived R_eff computed the same way), NOT an independent
# first-principles invasion threshold.
# -----------------------------------------------------------------------------

#' Cori renewal estimator on a single incidence series (pure core)
#'
#' The mathematical core of \code{\link{calc_Reff}}: applies the Cori et al.
#' (2013) instantaneous renewal estimator to one numeric incidence series and a
#' normalized generation-interval kernel. Pure (no I/O, no ensemble object) so it
#' is unit-testable in isolation.
#'
#' For each time step \eqn{t}, the estimate is
#' \deqn{R_{t} = \frac{I^{*}_{t}}{\sum_{\Delta t = 1}^{\min(t-1,\,K)} g(\Delta t)\,
#'   I^{*}_{t-\Delta t}}}
#' where \eqn{K = \texttt{length}(g)} and \eqn{g} is normalized so
#' \eqn{\sum g = 1} (doc eq:R). The denominator is the generation-weighted past
#' infectiousness. Leading steps whose denominator window is empty or non-positive
#' return \code{NA} (the standard Cori warm-up convention).
#'
#' @param incidence Numeric vector of infection incidence \eqn{I^{*}_{t}} (a
#'   flow, e.g. new S->E infections per day), in chronological order. Non-finite
#'   entries are treated as 0 in the denominator convolution but propagate to the
#'   numerator (the ratio is \code{NA} where the numerator is non-finite).
#' @param g Numeric vector. The normalized generation-interval kernel pmf
#'   (\code{g[k]} = probability the interval is \code{k} steps). Must be finite
#'   and non-negative with positive sum; it is renormalized defensively.
#' @param infectiousness_floor Numeric scalar \eqn{\ge 0}. Minimum
#'   generation-weighted past infectiousness (in effective past infections)
#'   required to report \eqn{R_{t}}. Steps whose denominator is below this floor
#'   return \code{NA} rather than an explosive ratio. This guards two failure
#'   modes seen on real series: (i) an initial-condition seed at \eqn{t=1}
#'   followed by a tiny window denominator (which otherwise yields
#'   \eqn{R \approx 10^{3}}), and (ii) deep inter-epidemic troughs where the
#'   denominator is \eqn{\approx 0} (which otherwise yields meaningless
#'   \eqn{R \approx 0.01}). Default \code{1} (require ~1 effective past
#'   infection); set \code{0} to recover the pure Cori convention (only the
#'   non-positive-denominator guard).
#'
#' @return A numeric vector the same length as \code{incidence} giving
#'   \eqn{R_{t}}, with \code{NA} at warm-up steps with an empty window and at any
#'   step whose generation-weighted denominator is below
#'   \code{infectiousness_floor}.
#'
#' @keywords internal
#' @noRd
.cori_reff <- function(incidence, g, infectiousness_floor = 1) {
  if (!is.numeric(incidence))
    stop(".cori_reff: incidence must be numeric")
  if (!is.numeric(g) || length(g) < 1L)
    stop(".cori_reff: g must be a non-empty numeric vector")
  if (any(!is.finite(g)) || any(g < 0))
    stop(".cori_reff: g must be finite and non-negative")
  if (!is.numeric(infectiousness_floor) || length(infectiousness_floor) != 1L ||
      !is.finite(infectiousness_floor) || infectiousness_floor < 0)
    stop(".cori_reff: infectiousness_floor must be a single finite scalar >= 0")
  gsum <- sum(g)
  if (!is.finite(gsum) || gsum <= 0)
    stop(".cori_reff: g must have positive total mass")
  g <- g / gsum

  Tn <- length(incidence)
  if (Tn == 0L) return(numeric(0))
  K  <- length(g)
  # Denominator convolution uses zeros for non-finite/absent history.
  inc_hist <- incidence
  inc_hist[!is.finite(inc_hist)] <- 0

  out <- rep(NA_real_, Tn)
  for (t in seq_len(Tn)) {
    max_lag <- min(t - 1L, K)
    if (max_lag < 1L) next                       # warm-up: empty window
    lags <- seq_len(max_lag)
    denom <- sum(g[lags] * inc_hist[t - lags])
    # Infectiousness-floor gate: below ~1 effective past infection the ratio is
    # numerically meaningless (IC seed spikes -> R~1e3; deep troughs -> R~0.01).
    if (!is.finite(denom) || denom <= 0 || denom < infectiousness_floor) next
    num <- incidence[t]
    if (!is.finite(num)) next
    out[t] <- num / denom
  }
  out
}

#' Cori effective reproductive number (R_eff) from ensemble trajectories
#'
#' Computes the per-location, time-varying Cori (2013) instantaneous
#' \strong{infection} effective reproductive number \eqn{R^{\mathrm{cori}}_{jt}}
#' by an analytic reduction over the ensemble trajectories the run already
#' captured (no extra simulation). The renewal numerator and denominator both use
#' the realised \strong{infection-incidence} flow (the captured \code{incidence}
#' channel = \code{incidence_human + incidence_env}, recorded at the S->E moment),
#' with the two infectious classes counted at \strong{equal weight}. Heterogeneity
#' in infectiousness over the course of infection is carried by the moment-matched
#' generation-interval kernel \eqn{g} (see \code{.mosaic_generation_time_pmf}),
#' not by any weight on the incidence input. There is therefore no \code{numerator}
#' or shedding-weight argument, and the function never reads \code{Isym}/\code{Iasym}
#' or \code{zeta_*}.
#'
#' \strong{Caveat (model descriptor, not an invasion threshold).} Because this is
#' computed on \emph{simulated} incidence, it describes the model trajectory and
#' is directly comparable to a surveillance-derived R_eff computed the same way;
#' it is \emph{not} an independent first-principles basic reproductive number.
#'
#' \strong{Estimand limitation (two-clock generation interval).} The kernel
#' \eqn{g} is the \strong{two-clock} (latent + infectious, human-route)
#' generation interval, \eqn{\mathcal{G} = 1/\iota + 1/\gamma_{\mathrm{eff}}}. The
#' documented \strong{three-clock} extension that adds an environmental-survival
#' delay \eqn{1/\delta_{jt}} (04-model-description.Rmd, "Generation-time
#' distribution (latent + infectious + environmental delay)") is deliberately
#' \emph{excluded} here. Consequently, in waterborne-dominated locations where the
#' environmental pathway carries much of the transmission, this estimator uses a
#' shorter mean generation time than the true infection-to-infection interval and
#' is therefore a \strong{lower-mean-\eqn{\mathcal{G}} approximation}: it will read
#' slightly closer to 1 (less extreme in either direction) than a three-clock
#' kernel would. The pooled \code{incidence} series itself does include both the
#' human and environmental S->E routes; only the kernel timing is two-clock.
#'
#' @param ensemble Either a \code{mosaic_trajectories} artifact (the object
#'   \code{calc_model_ensemble()} attaches as \code{$trajectories} and
#'   \code{.mosaic_persist_trajectory_artifact()} writes to
#'   \code{2_calibration/trajectories_ensemble.rds}) OR a \code{mosaic_ensemble}
#'   carrying that artifact in its \code{$trajectories} field. It must provide the
#'   weighted-median \code{incidence} channel (\code{summary$incidence$median}, an
#'   \eqn{nL \times T} matrix); the per-member \code{lines} are used for the
#'   posterior credible interval when they are daily-consecutive.
#' @param config The medoid \code{config} list. Reads only the kernel timing
#'   parameters \code{iota}, \code{gamma_1}, \code{gamma_2}, \code{sigma} (scalars).
#' @param max_days Integer. Generation-interval kernel truncation horizon (days).
#'   Default \code{56L}.
#' @param weights Optional numeric vector of per-member weights for the posterior
#'   reduction. \code{NULL} (default) uses the member weights carried in the
#'   trajectory \code{lines}.
#' @param probs Numeric vector of credible-interval quantile probabilities.
#'   Default \code{c(0.025, 0.25, 0.5, 0.75, 0.975)}.
#' @param infectiousness_floor Numeric scalar \eqn{\ge 0}. Minimum
#'   generation-weighted past infectiousness (effective past infections) required
#'   to report \eqn{R_{t}} at a step; below it the cell is \code{NA}. Guards
#'   initial-condition seed spikes (which otherwise yield \eqn{R \approx 10^{3}})
#'   and deep inter-epidemic troughs (\eqn{R \approx 0.01}). Default \code{1}.
#'   Applied identically to the central (medoid) series and to every per-member
#'   posterior series. Set \code{0} for the pure Cori convention.
#' @param verbose Logical; emit progress messages. Default \code{TRUE}.
#'
#' @return A tidy long \code{data.frame} (\code{reproductive_numbers} schema) with
#'   columns \code{location}, \code{date}, \code{t}, \code{estimand}
#'   (\code{"R_eff"}), \code{central} (the medoid point estimate = renewal on the
#'   weighted-median incidence), and one column per requested quantile
#'   (\code{q2.5}, \code{q25}, \code{q50}, \code{q75}, \code{q97.5}, ...) from the
#'   posterior reduction over members via \code{\link{weighted_quantiles}}. The
#'   wide \eqn{nL \times T} central matrix is attached as attribute
#'   \code{"central_matrix"}. Provenance attributes: \code{kernel}
#'   (\code{"moment_matched"}), \code{series} (\code{"infection_incidence"}),
#'   \code{kernel_params}, \code{ci_source}, and \code{caveat}.
#'
#' @details
#' \strong{Point estimate (central).} The headline per-location series is the
#' renewal estimator applied to \code{summary$incidence$median} (the full daily
#' weighted-median incidence), consistent with every other medoid-based diagnostic.
#'
#' \strong{Posterior credible interval.} Each retained member's daily R_eff series
#' is reconstructed from the per-member \code{incidence} \code{lines} and the
#' kernel, then reduced per \code{(location, t)} cell with
#' \code{\link{weighted_quantiles}} (weight applied \emph{once}; never
#' \code{sample(prob = w)}). This requires daily-consecutive lines; the persisted
#' artifact thins \code{lines} on a stride (default every 7 days), on which a
#' daily renewal convolution is undefined. When \code{lines} are \emph{not}
#' daily-consecutive the quantile columns are returned as \code{NA} with
#' \code{ci_source = "unavailable_strided_lines"} and a warning, rather than
#' fabricating a CI from sub-daily data. The kernel is held fixed at the medoid
#' config across members (the thinned lines do not carry per-member kernel draws;
#' documented approximation, plan section G.2).
#'
#' \strong{The posterior CI is currently unavailable on production-default
#' artifacts.} The trajectory-capture grid in \code{calc_model_ensemble()} writes
#' \code{lines} on a stride that does not yield a daily-consecutive run, so the
#' \code{unavailable_strided_lines} path is taken in production and only the
#' \code{central} (medoid) series is populated. This is \strong{not} fixable by
#' merely re-capturing with a finer stride from the caller: the builder grid
#' formula yields an empty-or-still-strided set at every stride setting, so the
#' daily-consecutive precondition is unreachable until the capture grid itself is
#' changed. Restoring a usable posterior CI requires a \strong{Phase-2 fix to the
#' trajectory-capture grid} in \code{calc_model_ensemble()}; do not expect a
#' \code{line_stride = 1} re-capture to enable it.
#'
#' @references Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new framework
#'   and software to estimate time-varying reproduction numbers during epidemics.
#'   American Journal of Epidemiology 178(9):1505-1512.
#'
#' @seealso \code{\link{weighted_quantiles}}
#' @export
calc_Reff <- function(ensemble,
                      config,
                      max_days = 56L,
                      weights  = NULL,
                      probs    = c(0.025, 0.25, 0.5, 0.75, 0.975),
                      infectiousness_floor = 1,
                      verbose  = TRUE) {

  caveat <- paste0("Cori R_eff computed on SIMULATED infection incidence: a ",
                   "descriptor of the model trajectory (comparable to a ",
                   "surveillance-derived R_eff computed identically), NOT an ",
                   "independent first-principles basic reproductive number.")

  # --- Resolve the trajectory artifact ---------------------------------------
  traj <- ensemble
  if (!inherits(traj, "mosaic_trajectories")) {
    if (is.list(ensemble) && inherits(ensemble$trajectories, "mosaic_trajectories")) {
      traj <- ensemble$trajectories
    } else {
      stop("calc_Reff: `ensemble` must be a 'mosaic_trajectories' artifact or a ",
           "list carrying one in $trajectories (got class ",
           paste(class(ensemble), collapse = "/"), ").")
    }
  }

  if (is.null(config) || !is.list(config))
    stop("calc_Reff: `config` must be the medoid config list with kernel params.")
  for (nm in c("iota", "gamma_1", "gamma_2", "sigma")) {
    if (is.null(config[[nm]]))
      stop("calc_Reff: config is missing kernel parameter '", nm, "'.")
  }
  if (!is.numeric(probs) || length(probs) == 0L || any(!is.finite(probs)) ||
      any(probs < 0) || any(probs > 1))
    stop("calc_Reff: `probs` must be finite numerics in [0, 1].")
  if (!is.numeric(infectiousness_floor) || length(infectiousness_floor) != 1L ||
      !is.finite(infectiousness_floor) || infectiousness_floor < 0)
    stop("calc_Reff: `infectiousness_floor` must be a single finite scalar >= 0.")

  # --- Kernel (shared across locations within a config) ----------------------
  g <- .mosaic_generation_time_pmf(
    iota    = as.numeric(config$iota)[1],
    gamma_1 = as.numeric(config$gamma_1)[1],
    gamma_2 = as.numeric(config$gamma_2)[1],
    sigma   = as.numeric(config$sigma)[1],
    max_days = max_days)

  # --- Point estimate (medoid): renewal on the weighted-median incidence -----
  inc_med <- traj$summary[["incidence"]]$median
  if (is.null(inc_med) || !is.matrix(inc_med))
    stop("calc_Reff: trajectory artifact has no `incidence` channel median ",
         "(summary$incidence$median). Was the run captured with ",
         "capture_trajectories = TRUE and the `incidence` channel present?")

  loc_names <- traj$location_names
  nL <- traj$n_locations
  Tn <- traj$n_time_points
  if (nrow(inc_med) != nL || ncol(inc_med) != Tn)
    stop("calc_Reff: incidence median dims [", nrow(inc_med), "x", ncol(inc_med),
         "] do not match n_locations/n_time_points [", nL, "x", Tn, "].")

  central_mat <- matrix(NA_real_, nrow = nL, ncol = Tn)
  for (i in seq_len(nL))
    central_mat[i, ] <- .cori_reff(as.numeric(inc_med[i, ]), g,
                                   infectiousness_floor = infectiousness_floor)

  # --- Date axis -------------------------------------------------------------
  d0    <- tryCatch(as.Date(traj$date_start), error = function(e) NA)
  dates <- if (!is.na(d0)) d0 + (seq_len(Tn) - 1L) else as.Date(NA) + seq_len(Tn)

  # --- Posterior CI via weighted_quantiles over per-member series ------------
  prob_cols <- .mosaic_reff_prob_colnames(probs)
  qmats <- array(NA_real_, dim = c(nL, Tn, length(probs)))
  ci_source <- "weighted_quantiles_per_member"

  lines <- traj$lines
  inc_lines <- if (is.data.frame(lines) && nrow(lines) > 0L)
    lines[lines$channel == "incidence", , drop = FALSE] else
      lines[0, , drop = FALSE]

  if (nrow(inc_lines) == 0L) {
    ci_source <- "unavailable_no_incidence_lines"
    if (verbose)
      message("calc_Reff: no per-member `incidence` lines in artifact; ",
              "credible-interval columns returned as NA.")
  } else {
    # The persisted lines are thinned on a time stride; a daily renewal
    # convolution is only valid on a daily-consecutive grid. Detect it from the
    # distinct t values actually present.
    t_present <- sort(unique(inc_lines$t))
    daily_consecutive <- length(t_present) >= 2L &&
      all(diff(t_present) == 1L)
    if (!daily_consecutive) {
      ci_source <- "unavailable_strided_lines"
      warning("calc_Reff: per-member trajectory `lines` are time-strided ",
              "(stride != 1 day), on which the daily Cori renewal is undefined; ",
              "returning point estimate with NA credible-interval columns. The ",
              "posterior CI is unavailable on production-default artifacts and ",
              "requires a Phase-2 fix to the trajectory-capture grid in ",
              "calc_model_ensemble() (a finer line_stride from the caller does ",
              "NOT enable it).",
              call. = FALSE)
    } else {
      qmats <- .mosaic_reff_member_quantiles(
        inc_lines = inc_lines, g = g, loc_names = loc_names,
        t_present = t_present, nL = nL, Tn = Tn, probs = probs,
        weights = weights, infectiousness_floor = infectiousness_floor)
    }
  }

  # --- Assemble tidy long data.frame -----------------------------------------
  parts <- vector("list", nL)
  for (i in seq_len(nL)) {
    df <- data.frame(
      location = loc_names[i],
      date     = dates,
      t        = seq_len(Tn),
      estimand = "R_eff",
      central  = central_mat[i, ],
      stringsAsFactors = FALSE)
    for (k in seq_along(probs))
      df[[prob_cols[k]]] <- qmats[i, , k]
    parts[[i]] <- df
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL

  attr(out, "central_matrix") <- central_mat
  attr(out, "location_names") <- loc_names
  attr(out, "dates")          <- dates
  attr(out, "kernel")         <- "moment_matched"
  attr(out, "series")         <- "infection_incidence"
  attr(out, "kernel_params")  <- c(
    iota    = as.numeric(config$iota)[1],
    gamma_1 = as.numeric(config$gamma_1)[1],
    gamma_2 = as.numeric(config$gamma_2)[1],
    sigma   = as.numeric(config$sigma)[1],
    shape   = attr(g, "shape"),
    rate    = attr(g, "rate"),
    mean    = attr(g, "mean"),
    var     = attr(g, "var"))
  attr(out, "probs")     <- probs
  attr(out, "ci_source") <- ci_source
  attr(out, "caveat")    <- caveat
  class(out) <- c("reproductive_numbers", "data.frame")

  if (verbose)
    message(sprintf("calc_Reff: R_eff computed for %d location(s) x %d step(s); ",
                    nL, Tn), "CI source: ", ci_source, ".")
  out
}

#' Quantile-column names for the reproductive_numbers schema
#' @keywords internal
#' @noRd
.mosaic_reff_prob_colnames <- function(probs) {
  pct <- probs * 100
  lab <- ifelse(pct == round(pct), sprintf("%d", round(pct)),
                sub("0+$", "", sprintf("%.4f", pct)))
  paste0("q", lab)
}

#' Re-simulate the saved posterior ensemble and build a per-member R_eff CI
#'
#' Faithful re-simulation path for the Cori R_eff posterior credible interval.
#' The persisted production artifacts do NOT carry daily-consecutive per-member
#' infection-incidence (\code{trajectories_ensemble.rds} thins \code{lines} on a
#' stride, and \code{ensemble_candidate.rds} holds DAILY arrays only for reported
#' cases/deaths, not the \code{incidence} S->E flow). To compute a member-level
#' R_eff CI we therefore RE-RUN the exact posterior members the calibration ran
#' and capture daily \code{incidence} per member.
#'
#' \strong{Faithfulness.} Each member's config is rebuilt with the same recipe
#' \code{calc_model_ensemble()} uses for its local worker: \code{sample_parameters(
#' PATHS, priors, config = base, seed = parameter_seeds[p], sample_args)} then
#' \code{.mosaic_clamp_transmission_params()}; the per-(param, stoch) LASER seed
#' is the same deterministic \code{param_idx * 1000L + stoch_idx} the worker sets;
#' the engine is invoked through \code{.mosaic_prepare_config_for_python()} +
#' \code{lc$run_model(quiet = TRUE)}. The captured \code{reported_cases} per
#' (param, stoch) are compared against the saved \code{cases_array} from the
#' ensemble object (the FAITHFULNESS GATE): if they do not match, the re-sim is
#' not reproducing the calibration and the function stops rather than shipping a
#' wrong CI.
#'
#' Each (param_idx, stoch_idx) pair is one ensemble member with weight
#' \code{parameter_weights[param_idx] / n_simulations_per_config} (matching the
#' \code{sim_weights} convention used throughout \code{calc_model_ensemble()}).
#' Each member's R_eff series uses its OWN moment-matched generation-interval
#' kernel built from that member's sampled \code{iota/gamma_1/gamma_2/sigma}.
#'
#' @param ensemble A \code{mosaic_ensemble} object (read from
#'   \code{2_calibration/ensemble_candidate.rds}) carrying \code{seeds}
#'   (= the per-member \code{parameter_seeds}), \code{parameter_weights},
#'   \code{cases_array}, \code{n_param_sets}, \code{n_simulations_per_config},
#'   \code{location_names}, \code{date_start}.
#' @param base_config The base \code{config} list (medoid \code{config.json}) the
#'   members were sampled from.
#' @param priors The priors object (\code{1_inputs/priors.json}).
#' @param sampling_args The \code{control$sampling} list used at calibration.
#' @param PATHS \code{get_paths()} result (required by \code{sample_parameters}).
#' @param max_days Generation-interval kernel truncation (days). Default 56.
#' @param probs Quantile probabilities. Default \code{c(0.025, 0.5, 0.975)}.
#' @param infectiousness_floor Passed to \code{.cori_reff}. Default 1.
#' @param gate_rel_tol Numeric. Statistical-equivalence faithfulness gate:
#'   maximum allowed relative total-case error between the re-simulated
#'   \code{reported_cases} and the saved \code{cases_array}, applied to BOTH the
#'   \code{gate_frac}-percentile per-member error AND the ensemble-weighted
#'   aggregate error. Default \code{0.05} (5\%). A bitwise (exact) gate is NOT
#'   used because the LASER engine is bitwise-deterministic only WITHIN a process;
#'   the same seed + config re-run in a fresh process yields a
#'   statistically-equivalent (not identical) stochastic realization (numba RNG
#'   cross-process non-determinism).
#' @param gate_frac Numeric in (0, 1]. The per-member relative-error percentile
#'   used by the gate. Default \code{0.95} -- i.e. up to 5\% of members may be
#'   near-critical/bistable outliers (a tiny RNG difference flips
#'   outbreak/no-outbreak) without failing the gate, while a systematic
#'   reconstruction failure (which moves the BULK of members) still fails.
#' @param gate_cor_min Numeric. Statistical-equivalence faithfulness gate:
#'   minimum allowed MEDIAN per-member Pearson correlation between the
#'   re-simulated and saved \code{reported_cases} time profiles. Default
#'   \code{0.95}.
#' @param verbose Logical; emit progress. Default TRUE.
#'
#' @return A list with \code{qmats} (\code{nL x T x length(probs)} array of
#'   weighted quantiles), \code{central_mat} (\code{nL x T} weighted-median of the
#'   per-member R_eff), \code{probs}, the gate diagnostics
#'   (\code{gate_rel_err_pct}, \code{gate_rel_err_max}, \code{gate_agg_rel_err},
#'   \code{gate_cor_median}, \code{gate_cor_min}, \code{gate_max_abs_diff},
#'   \code{gate_n_outliers}, \code{gate_frac}), \code{n_members}, and
#'   \code{kernel_params} (the medoid kernel, for provenance).
#'
#' @keywords internal
#' @noRd
.mosaic_reff_resim_ci <- function(ensemble, base_config, priors, sampling_args,
                                  PATHS, max_days = 56L,
                                  probs = c(0.025, 0.5, 0.975),
                                  infectiousness_floor = 1,
                                  gate_rel_tol = 0.05, gate_frac = 0.95,
                                  gate_cor_min = 0.95, verbose = TRUE) {
  if (!inherits(ensemble, "mosaic_ensemble"))
    stop(".mosaic_reff_resim_ci: `ensemble` must be a mosaic_ensemble object.")
  for (nm in c("seeds", "parameter_weights", "cases_array", "n_param_sets",
               "n_simulations_per_config", "location_names"))
    if (is.null(ensemble[[nm]]))
      stop(".mosaic_reff_resim_ci: ensemble is missing `", nm, "`.")

  # Pin BLAS/Numba threads to 1. This path drives LASER directly (outside
  # run_MOSAIC(), which is otherwise the only place threads are pinned), so
  # without this many concurrent re-sims (e.g. a multi-model batch on a
  # many-core host) would each spawn full thread pools and thrash the machine.
  # Must run before the laser/numba import below (numba reads its thread count
  # at import time).
  .mosaic_set_blas_threads(1L)
  Sys.setenv(OMP_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
             OPENBLAS_NUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1",
             TBB_NUM_THREADS = "1", NUMBA_NUM_THREADS = "1")

  parameter_seeds <- as.integer(ensemble$seeds)
  pw    <- as.numeric(ensemble$parameter_weights)
  nP    <- as.integer(ensemble$n_param_sets)
  nS    <- as.integer(ensemble$n_simulations_per_config)
  locs  <- as.character(ensemble$location_names)
  nL    <- length(locs)
  ca    <- ensemble$cases_array          # [nL, T, nP, nS]
  Tn    <- dim(ca)[2L]
  if (length(parameter_seeds) != nP)
    stop(".mosaic_reff_resim_ci: seeds length != n_param_sets.")

  # --- Reconstruct member configs (same recipe as the ensemble worker) -------
  if (verbose) message("  Re-sampling ", nP, " posterior member configs...")
  member_cfgs <- vector("list", nP)
  for (p in seq_len(nP)) {
    member_cfgs[[p]] <- .mosaic_clamp_transmission_params(
      sample_parameters(PATHS = PATHS, priors = priors, config = base_config,
                        seed = parameter_seeds[p], sample_args = sampling_args,
                        verbose = FALSE))
  }

  # --- Load the engine (same import as the worker) ---------------------------
  if (!exists("lc", where = .GlobalEnv, inherits = FALSE)) {
    lc <- reticulate::import("laser.cholera.metapop.model")
    .mosaic_strip_laser_file_handler()
  } else {
    lc <- get("lc", envir = .GlobalEnv)
  }

  # --- Re-simulate each member, capturing daily incidence + reported_cases ---
  # Member index m = (s - 1) * nP + p ; weight = pw[p] / nS.
  n_members <- nP * nS
  # Per-member R_eff series [n_members x Tn], member weight, plus the gate diff.
  reff_by_member <- matrix(NA_real_, nrow = n_members, ncol = Tn * nL)  # dummy; reshaped below
  # Easier: keep a per-location list of [n_members x Tn] R_eff matrices.
  reff_loc <- lapply(seq_len(nL), function(i) matrix(NA_real_, n_members, Tn))
  member_w <- numeric(n_members)
  # Statistical-equivalence gate (see rationale below). The LASER engine is
  # bitwise-deterministic WITHIN a process but NOT across cold processes (numba
  # RNG state differs), so a re-sim of an identical seed + config is
  # statistically equivalent, not bitwise identical. We therefore record, per
  # member, the relative total-case error and the cases correlation vs the saved
  # cases_array, and gate on ROBUST statistics of those (NOT the single worst
  # member): a tiny fraction of near-critical/bistable members can flip
  # outbreak/no-outbreak on an RNG difference and blow up one member's relative
  # error without the posterior being wrong. A systematic reconstruction failure
  # would instead push the BULK of members off, which the percentile/aggregate
  # checks below catch. (Cross-process non-determinism: cf. the est_suitability
  # finding -- accept on statistical equivalence, never bitwise parity.)
  re_vec  <- rep(NA_real_, n_members)   # per-member relative total-case error
  cc_vec  <- rep(NA_real_, n_members)   # per-member cases correlation
  ssum_v  <- rep(NA_real_, n_members)   # per-member saved total
  rsum_v  <- rep(NA_real_, n_members)   # per-member resim total
  max_abs <- 0
  if (verbose) message("  Re-simulating ", n_members, " members (", nP, " x ", nS, ")...")
  for (p in seq_len(nP)) {
    cfg <- member_cfgs[[p]]
    g   <- .mosaic_generation_time_pmf(
      iota = as.numeric(cfg$iota)[1], gamma_1 = as.numeric(cfg$gamma_1)[1],
      gamma_2 = as.numeric(cfg$gamma_2)[1], sigma = as.numeric(cfg$sigma)[1],
      max_days = max_days)
    for (s in seq_len(nS)) {
      m <- (s - 1L) * nP + p
      member_w[m] <- pw[p] / nS
      run_cfg <- cfg
      run_cfg$seed <- (p * 1000L) + s
      model <- lc$run_model(
        paramfile = .mosaic_prepare_config_for_python(run_cfg), quiet = TRUE)
      inc <- model$results$incidence       # [nL, T] (or [T] when nL == 1)
      rc  <- model$results$reported_cases
      inc_m <- .mosaic_reff_to_mat(inc, nL, Tn)
      rc_m  <- .mosaic_reff_to_mat(rc,  nL, Tn)
      # Statistical-equivalence diagnostics vs saved cases_array (this (p, s)).
      saved <- matrix(as.numeric(ca[, , p, s, drop = FALSE]), nrow = nL, ncol = Tn)
      rv <- as.numeric(rc_m); sv <- as.numeric(saved)
      ok <- is.finite(rv) & is.finite(sv)
      if (any(ok)) {
        ssum <- sum(sv[ok]); rsum <- sum(rv[ok])
        re_vec[m]  <- if (ssum > 0) abs(rsum - ssum) / ssum else abs(rsum - ssum)
        cc_vec[m]  <- suppressWarnings(stats::cor(rv[ok], sv[ok]))
        ssum_v[m]  <- ssum; rsum_v[m] <- rsum
        max_abs    <- max(max_abs, max(abs(rv[ok] - sv[ok])))
      }
      for (i in seq_len(nL))
        reff_loc[[i]][m, ] <- .cori_reff(inc_m[i, ], g,
                                         infectiousness_floor = infectiousness_floor)
      reticulate::import("gc")$collect()
    }
    if (verbose && (p %% 10L == 0L || p == nP))
      message(sprintf("    members done: %d/%d | rel_err med=%.4f p%.0f=%.4f | cor med=%.4f",
                      p, nP, stats::median(re_vec, na.rm = TRUE), gate_frac * 100,
                      stats::quantile(re_vec, gate_frac, na.rm = TRUE, names = FALSE),
                      stats::median(cc_vec, na.rm = TRUE)))
  }

  # FAITHFULNESS GATE (robust statistical equivalence). Pass requires:
  #   (1) the gate_frac-percentile per-member relative total-case error <= gate_rel_tol
  #       (tolerates up to (1 - gate_frac) bistable outlier members),
  #   (2) the ensemble-WEIGHTED aggregate total-case relative error <= gate_rel_tol
  #       (the posterior-relevant burden must match), and
  #   (3) the MEDIAN per-member cases correlation >= gate_cor_min.
  # A systematic reconstruction bug fails all three; a lone near-critical member
  # fails none. Worst-member stats are reported for provenance but not gated.
  n_compared <- sum(is.finite(re_vec))
  if (n_compared == 0L)
    stop(".mosaic_reff_resim_ci: FAITHFULNESS GATE FAILED -- no overlapping ",
         "reported_cases cells to compare against the saved cases_array.")
  rel_err_pct  <- stats::quantile(re_vec, gate_frac, na.rm = TRUE, names = FALSE)
  rel_err_max  <- max(re_vec, na.rm = TRUE)
  cor_median   <- stats::median(cc_vec, na.rm = TRUE)
  cor_min      <- min(cc_vec, na.rm = TRUE)
  # Per-member weight aligned to member index m = (s-1)*nP + p.
  agg_saved <- sum(ssum_v * member_w, na.rm = TRUE)
  agg_resim <- sum(rsum_v * member_w, na.rm = TRUE)
  agg_rel_err <- if (agg_saved > 0) abs(agg_resim - agg_saved) / agg_saved else
    abs(agg_resim - agg_saved)
  n_outliers <- sum(re_vec > gate_rel_tol, na.rm = TRUE)

  gate_pass <- is.finite(rel_err_pct) && rel_err_pct <= gate_rel_tol &&
    is.finite(agg_rel_err) && agg_rel_err <= gate_rel_tol &&
    is.finite(cor_median) && cor_median >= gate_cor_min
  if (verbose)
    message(sprintf(paste0("  Faithfulness gate: p%.0f rel_err=%.4f (tol %.3f), ",
                          "ensemble-agg rel_err=%.4f, median cor=%.4f (min %.3f), ",
                          "%d/%d member outliers (worst rel_err=%.3f)."),
                    gate_frac * 100, rel_err_pct, gate_rel_tol, agg_rel_err,
                    cor_median, gate_cor_min, n_outliers, n_compared, rel_err_max))
  if (!gate_pass)
    stop(sprintf(paste0(".mosaic_reff_resim_ci: FAITHFULNESS GATE FAILED. ",
                        "Re-simulated reported_cases are not statistically ",
                        "equivalent to the saved cases_array: p%.0f per-member ",
                        "relative total-case error = %.4f (tol %.4f), ",
                        "ensemble-aggregate relative error = %.4f (tol %.4f), ",
                        "median per-member correlation = %.4f (min %.4f); ",
                        "%d/%d members exceed the per-member tolerance (worst ",
                        "rel_err = %.3f). This indicates a systematic ",
                        "reconstruction failure (not a lone bistable member); ",
                        "refusing to ship an untrustworthy R_eff CI."),
                 gate_frac * 100, rel_err_pct, gate_rel_tol, agg_rel_err,
                 gate_rel_tol, cor_median, gate_cor_min, n_outliers, n_compared,
                 rel_err_max))

  # --- Weighted-quantile reduction per (location, t) -------------------------
  qmats   <- array(NA_real_, dim = c(nL, Tn, length(probs)))
  central <- matrix(NA_real_, nrow = nL, ncol = Tn)
  for (i in seq_len(nL)) {
    M <- reff_loc[[i]]
    for (t in seq_len(Tn)) {
      vals <- M[, t]
      qs <- weighted_quantiles(vals, member_w, probs)
      qmats[i, t, ] <- qs
      central[i, t] <- weighted_quantiles(vals, member_w, 0.5)
    }
  }

  list(qmats = qmats, central_mat = central, probs = probs,
       gate_rel_err_pct = rel_err_pct, gate_rel_err_max = rel_err_max,
       gate_agg_rel_err = agg_rel_err, gate_cor_median = cor_median,
       gate_cor_min = cor_min, gate_max_abs_diff = max_abs,
       gate_n_outliers = n_outliers, gate_frac = gate_frac,
       n_members = n_members,
       kernel_params = c(
         iota    = as.numeric(base_config$iota)[1],
         gamma_1 = as.numeric(base_config$gamma_1)[1],
         gamma_2 = as.numeric(base_config$gamma_2)[1],
         sigma   = as.numeric(base_config$sigma)[1]))
}

#' Coerce an engine channel to an nL-by-Tn matrix (orientation-robust)
#' @keywords internal
#' @noRd
.mosaic_reff_to_mat <- function(val, nL, Tn) {
  m <- suppressWarnings(as.numeric(unlist(val, use.names = FALSE)))
  L <- length(m); target <- nL * Tn
  if (L == target) {
    # Engine returns [nL, T]; unlist is column-major over an [nL, T] R matrix.
    if (nL == 1L) return(matrix(m, nrow = 1L, ncol = Tn))
    return(matrix(m, nrow = nL, ncol = Tn))
  }
  if (L %% nL == 0L) {
    t_eff <- L %/% nL
    mm <- matrix(m, nrow = nL, ncol = t_eff)
    if (t_eff >= Tn) return(mm[, seq_len(Tn), drop = FALSE])
    out <- matrix(NA_real_, nL, Tn); out[, seq_len(t_eff)] <- mm
    return(out)
  }
  if (nL == 1L) {
    out <- rep(NA_real_, Tn); n <- min(L, Tn); out[seq_len(n)] <- m[seq_len(n)]
    return(matrix(out, nrow = 1L))
  }
  matrix(NA_real_, nL, Tn)
}

#' Per-member posterior reduction of R_eff via weighted_quantiles
#'
#' Reconstructs each retained member's daily R_eff series from the per-member
#' (daily-consecutive) incidence \code{lines}, then reduces per (location, t)
#' cell with \code{\link{weighted_quantiles}} (weight applied once). The kernel
#' \code{g} is held fixed at the medoid config across members (documented
#' approximation, plan section G.2).
#'
#' @param weights Optional named/positional global per-member weight vector. When
#'   supplied it is indexed by \strong{member id}, not by per-location position:
#'   a named vector (\code{names(weights)} = member ids) is looked up by name; an
#'   unnamed vector is assumed indexed by \code{member_id} value (\code{weights[
#'   as.character(id)]} / \code{weights[id]}). This is required because the
#'   \code{is.finite} member set can differ across locations (a member dropped in
#'   one location shifts every later position), so position indexing silently
#'   mis-aligns weights to the wrong members. \code{NULL} (default) uses each
#'   member's own carried \code{mm$weight[1]}, which is already member-aligned.
#' @param infectiousness_floor Passed through to \code{\link{.cori_reff}}.
#'
#' @keywords internal
#' @noRd
.mosaic_reff_member_quantiles <- function(inc_lines, g, loc_names, t_present,
                                          nL, Tn, probs, weights = NULL,
                                          infectiousness_floor = 1) {
  qmats <- array(NA_real_, dim = c(nL, Tn, length(probs)))
  # t_present is a daily-consecutive run but may not start at 1 or cover all Tn;
  # only those columns get a CI (others stay NA).
  t_min <- min(t_present); t_max <- max(t_present)

  # Resolve a global weights lookup keyed by member id (NOT position). Named
  # vectors match on names(weights); unnamed vectors are treated as indexed by
  # the integer member_id value.
  w_named   <- !is.null(weights) && !is.null(names(weights))
  for (i in seq_len(nL)) {
    li <- inc_lines[inc_lines$location == loc_names[i], , drop = FALSE]
    if (nrow(li) == 0L) next
    members <- unique(li$member_id)
    # Per-member R_eff series on the present (daily-consecutive) t-window.
    n_present <- t_max - t_min + 1L
    reff_by_member <- matrix(NA_real_, nrow = length(members), ncol = n_present)
    mw <- numeric(length(members))
    for (mi in seq_along(members)) {
      id  <- members[mi]
      mm  <- li[li$member_id == id, , drop = FALSE]
      ord <- order(mm$t)
      mm  <- mm[ord, , drop = FALSE]
      # Place into a dense daily vector indexed by (t - t_min + 1).
      inc_vec <- rep(NA_real_, n_present)
      inc_vec[mm$t - t_min + 1L] <- mm$value
      reff_by_member[mi, ] <- .cori_reff(inc_vec, g,
                                         infectiousness_floor = infectiousness_floor)
      # Weight lookup keyed by member id, never by position.
      w <- if (!is.null(weights)) {
        if (w_named) weights[as.character(id)] else weights[id]
      } else {
        mm$weight[1]
      }
      mw[mi] <- if (length(w) && is.finite(w[1])) w[1] else NA_real_
    }
    # Reduce per present-time column.
    for (k in seq_len(n_present)) {
      tt <- t_min + k - 1L
      vals <- reff_by_member[, k]
      qs <- weighted_quantiles(vals, mw, probs)
      qmats[i, tt, ] <- qs
    }
  }
  qmats
}
