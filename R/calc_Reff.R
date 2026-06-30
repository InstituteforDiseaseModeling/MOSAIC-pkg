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
