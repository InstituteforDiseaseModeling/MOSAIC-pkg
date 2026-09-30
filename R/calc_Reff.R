# -----------------------------------------------------------------------------
# Route-decomposed Cori effective reproductive number on simulated incidence
# -----------------------------------------------------------------------------
# Analytic reduction over the ensemble trajectories the run already captured
# (no extra simulation on the direct path). The estimand is the Cori (2013)
# instantaneous INFECTION reproductive number, split by transmission route:
#
#   Lambda_hum[t] = expected human-route infectiousness at t of all past
#                   infections (latent -> Isym/Iasym, both at equal weight)
#   Lambda_env[t] = expected environmental infectiousness at t of all past
#                   infections (latent -> shedding -> survival in W at the
#                   psi-dependent decay rate delta_jt)
#   R_hum[t] = incidence_human[t] / Lambda_hum[t]
#   R_env[t] = incidence_env[t]   / Lambda_env[t]
#   R_eff[t] = R_hum[t] + R_env[t]
#
# Both Lambdas are driven by TOTAL infection incidence: every infection is
# infectious through both routes whichever route produced it (single E
# compartment). Each route's per-cohort infectivity profile sums to 1, so each
# R is secondary infections per infection.
#
# The kernels are DERIVED from the engine's own daily transition probabilities
# and phase order (R/sim_components.R: Exposed -> Infectious -> HumanToHuman ->
# EnvToHuman -> Environmental), not transcribed from continuous-time moments.
# The estimate is INSTANTANEOUS: the reservoir is rebuilt from the actual past
# decay path and one infection's lifetime reservoir contribution is valued at
# today's delta_jt (Cori: "if conditions stayed as at t"). An earlier draft
# normalized each cohort by its lifetime contribution under the FUTURE decay
# path, which made R_env[t] depend on delta months after t (see
# .mosaic_reff_infectiousness).
#
# Canonical theory: MOSAIC-docs/04-model-description.Rmd, "The effective
# reproductive number" (eq:R, eq:I-star and the route-decomposition equations).
#
# CAVEAT: computed on SIMULATED incidence, so it DESCRIBES the model trajectory
# (comparable to a surveillance-derived R_eff computed the same way); it is not
# a first-principles invasion threshold. The renewal assumes transmission is
# linear in infectiousness; the human FOI uses I^alpha_1 and the environmental
# dose saturates at W/N ~ kappa, so both R are trajectory descriptors, not
# per-contact constants. Suitability psi enters R_env twice -- through
# beta_jt_env and through the reservoir lifetime 1/delta_jt -- and R_env values
# one infection's lifetime at TODAY's delta, so R_env > 1 in a high-psi season
# is not a growth threshold: that survival will not last the infection's
# lifetime. The renewal is per location: infectious people arriving
# through mobility (tau_i, pi_ij) drive the destination's human FOI but are not
# in its Lambda_hum, so in multi-location runs imported spread is credited to
# the destination's R_hum.
# -----------------------------------------------------------------------------

.MOSAIC_REFF_ESTIMANDS <- c("R_eff", "R_hum", "R_env")

#' Cori ratio of a route numerator to its infectiousness (pure core)
#'
#' \deqn{R_{t} = \mathrm{numerator}_{t} / \Lambda_{t}}
#' reported only where \eqn{\Lambda_{t}} is finite, positive and at least
#' \code{infectiousness_floor}.
#'
#' @param numerator Numeric vector of route-specific infection incidence.
#' @param Lambda Numeric vector (same length) of normalized past
#'   infectiousness, in effective past infections.
#' @param infectiousness_floor Numeric scalar \eqn{\ge 0}. Minimum
#'   \eqn{\Lambda_{t}} required to report \eqn{R_{t}}. Guards the tiny-denominator
#'   spikes at series start and the \eqn{R \approx 0} artefacts of deep
#'   inter-epidemic troughs. \code{0} keeps only the positive-denominator guard.
#' @return Numeric vector of \eqn{R_{t}} with \code{NA} where undefined.
#' @keywords internal
#' @noRd
.cori_reff <- function(numerator, Lambda, infectiousness_floor = 1) {
  if (!is.numeric(numerator) || !is.numeric(Lambda))
    stop(".cori_reff: numerator and Lambda must be numeric")
  if (length(numerator) != length(Lambda))
    stop(".cori_reff: numerator and Lambda must have the same length")
  if (!is.numeric(infectiousness_floor) || length(infectiousness_floor) != 1L ||
      !is.finite(infectiousness_floor) || infectiousness_floor < 0)
    stop(".cori_reff: infectiousness_floor must be a single finite scalar >= 0")
  ok <- is.finite(numerator) & is.finite(Lambda) & Lambda > 0 &
    Lambda >= infectiousness_floor
  out <- rep(NA_real_, length(numerator))
  out[ok] <- numerator[ok] / Lambda[ok]
  out
}

#' Engine-derived route kernel parameters
#'
#' Builds the daily cohort-state probabilities of one infection under the
#' engine's discrete-time transitions: after entering E on day 0 it progresses
#' with probability \code{1 - exp(-iota)} per day, splits symptomatic with
#' probability \code{sigma}, and recovers with \code{1 - exp(-gamma_k)} per day.
#' Mortality is ignored. Since v0.96.0 a fraction \code{p_fatal} of symptomatic
#' onsets dies at onset and never enters Isym, while survivors' dwell is
#' unchanged; omitting it is exact at constant incidence and biases R_hum by
#' +0.2-0.4% in growth at the median \code{p_fatal} (2.8%), R_env by under
#' 0.1%.
#'
#' @param iota,gamma_1,gamma_2 Positive scalar daily rates.
#' @param sigma Scalar in \[0, 1\], symptomatic proportion.
#' @param zeta_1,zeta_2 Non-negative scalar shedding rates (symptomatic,
#'   asymptomatic); only their ratio enters the kernel. Must not both be 0.
#' @param tail Remaining cohort mass at which the state tables are truncated.
#' @return A list with the per-day probabilities \code{p_i}, \code{p1},
#'   \code{p2}, \code{sigma}, the shedding weights \code{w1}, \code{w2}
#'   (summing to 1), \code{D_h} (expected infectious person-days per infection),
#'   and \code{Ps}, \code{Pa}: P(in Isym / Iasym) at \code{k = 1..K} days after
#'   the infection day.
#' @keywords internal
#' @noRd
.mosaic_reff_route_kernel <- function(iota, gamma_1, gamma_2, sigma,
                                      zeta_1, zeta_2, tail = 1e-10) {
  chk <- function(x, nm, lower = 0, strict = TRUE) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
        (strict && x <= lower) || (!strict && x < lower))
      stop(".mosaic_reff_route_kernel: `", nm, "` must be a finite scalar ",
           if (strict) "> " else ">= ", lower, call. = FALSE)
  }
  chk(iota, "iota"); chk(gamma_1, "gamma_1"); chk(gamma_2, "gamma_2")
  chk(sigma, "sigma", strict = FALSE)
  if (sigma > 1) stop(".mosaic_reff_route_kernel: `sigma` must be <= 1", call. = FALSE)
  chk(zeta_1, "zeta_1", strict = FALSE); chk(zeta_2, "zeta_2", strict = FALSE)
  if (sigma * zeta_1 + (1 - sigma) * zeta_2 <= 0)
    stop(".mosaic_reff_route_kernel: sigma * zeta_1 + (1 - sigma) * zeta_2 ",
         "must be > 0 (the environmental route needs shedding).", call. = FALSE)

  # The engine's per-tick probabilities (sim_params.R: *_prob <- -expm1(-rate)).
  p_i <- -expm1(-iota); p1 <- -expm1(-gamma_1); p2 <- -expm1(-gamma_2)

  # Cohort recursion in the engine's order: on each day the E stock progresses
  # into I and the I stock recovers; arrivals are not recovered on arrival.
  K <- min(1e5, ceiling(log(tail) / log1p(-min(p_i, p1, p2))) + 1L)
  Ps <- Pa <- numeric(K)
  e <- 1; is <- 0; ia <- 0
  for (k in seq_len(K)) {
    prog <- p_i * e
    is <- is * (1 - p1) + sigma * prog
    ia <- ia * (1 - p2) + (1 - sigma) * prog
    e  <- e - prog
    Ps[k] <- is; Pa[k] <- ia
    if (e + is + ia < tail) { Ps <- Ps[seq_len(k)]; Pa <- Pa[seq_len(k)]; break }
  }

  w1 <- zeta_1 / (zeta_1 + zeta_2)
  list(p_i = p_i, p1 = p1, p2 = p2, sigma = sigma,
       w1 = w1, w2 = 1 - w1,
       D_h = sigma / p1 + (1 - sigma) / p2,
       Ps = Ps, Pa = Pa)
}

#' Route infectiousness Lambda_hum / Lambda_env from an incidence series
#'
#' Mean-field propagation of past infections through the engine's
#' latent/infectious states and the environmental reservoir, on the engine's
#' daily grid (result index t: an infection recorded at t is infectious from
#' t + 1 and drives new infections from t + 2; the reservoir at t drives
#' environmental infections at t + 1 and decays at \code{delta[t + 1]}).
#'
#' \strong{Instantaneous (frozen-at-t) normalization.} Cori's instantaneous R is
#' the number of secondary infections one infection would cause if conditions
#' stayed as they are at t. For the human route the infectivity profile does not
#' depend on t, so \eqn{\Lambda^{hum}_t = \hat I_{t-1} / D_h}. For the
#' environmental route the reservoir \eqn{\hat W} is built from the actual past
#' decay path, and one infection's lifetime reservoir contribution is evaluated
#' at today's decay rate: \eqn{S_w / \delta_t}, with \eqn{S_w = \sum_k (w_1
#' P^s_k + w_2 P^a_k)} its expected shedding. So \eqn{\Lambda^{env}_t = \hat
#' W_{t-1}\, \delta_t / S_w}. Nothing after t is used, and at constant decay this
#' equals convolution with the environmental generation-interval kernel.
#'
#' \strong{Initial conditions.} People already latent or infectious at the start
#' are real sources of infection that no recorded incidence explains, so they
#' are propagated through the same filters and included in both Lambdas. The
#' engine starts the reservoir empty, so they reach W only by shedding; result
#' index 1 already holds their first day of it, approximated from the
#' result-index-1 stocks (the engine sheds from the unrecorded seed row).
#'
#' @param incidence Numeric vector of total infection incidence (NA treated 0).
#' @param delta Numeric vector (same length) of daily decay rates delta_jt;
#'   values above 1 are capped at 1 (the engine clamps decay to the stock).
#' @param kern Output of \code{.mosaic_reff_route_kernel()}.
#' @param init Optional \code{c(E, Isym, Iasym)} initial stocks at result
#'   index 1, excluding that day's new infections.
#' @param shed_abs Optional length-2 numeric \code{(1 - theta) * c(zeta_1,
#'   zeta_2)}; when given, also returns the absolute reconstructed reservoir.
#' @return List: \code{Lambda_hum}, \code{Lambda_env}, \code{I_hat}
#'   (reconstructed Isym + Iasym) and, when \code{shed_abs} is given,
#'   \code{W_hat} (reconstructed reservoir, cells).
#' @keywords internal
#' @noRd
.mosaic_reff_infectiousness <- function(incidence, delta, kern, init = NULL,
                                        shed_abs = NULL) {
  Tn <- length(incidence)
  if (length(delta) != Tn)
    stop(".mosaic_reff_infectiousness: delta must match incidence length")
  if (any(!is.finite(delta)) || any(delta <= 0))
    stop(".mosaic_reff_infectiousness: delta must be finite and > 0")
  if (Tn == 0L)
    return(list(Lambda_hum = numeric(0), Lambda_env = numeric(0),
                I_hat = numeric(0), W_hat = numeric(0)))
  delta <- pmin(as.numeric(delta), 1)
  x <- as.numeric(incidence); x[!is.finite(x)] <- 0
  e0 <- 0; is0 <- 0; ia0 <- 0
  if (!is.null(init)) {
    init <- pmax(0, as.numeric(init)); init[!is.finite(init)] <- 0
    e0 <- init[1L]; is0 <- init[2L]; ia0 <- init[3L]
  }

  # E/I propagation is linear and time-invariant, so it is a recursive filter.
  # Arrivals into I at t come from the E stock at t - 1; initial stocks enter
  # as the first element of each filter's input.
  x[1L] <- x[1L] + e0
  E  <- as.numeric(stats::filter(x, 1 - kern$p_i, method = "recursive"))
  fl <- c(0, E[-Tn]) * kern$p_i
  in_s <- kern$sigma * fl;       in_s[1L] <- in_s[1L] + is0
  in_a <- (1 - kern$sigma) * fl; in_a[1L] <- in_a[1L] + ia0
  Is <- as.numeric(stats::filter(in_s, 1 - kern$p1, method = "recursive"))
  Ia <- as.numeric(stats::filter(in_a, 1 - kern$p2, method = "recursive"))

  # Reservoir with the time-varying decay: W[t+1] = W[t](1 - delta[t+1]) +
  # shedding from I[t] (sim_phase_environmental). Linear with a time-varying
  # coefficient, so a loop rather than a filter.
  reservoir <- function(shed, w1_init) {
    W <- numeric(Tn)
    W[1L] <- w1_init
    if (Tn >= 2L) for (t in seq_len(Tn - 1L))
      W[t + 1L] <- W[t] * (1 - delta[t + 1L]) + shed[t]
    W
  }
  lag1 <- function(v) c(0, v[-Tn])

  I_hat <- Is + Ia
  S_w <- kern$w1 * kern$sigma / kern$p1 + kern$w2 * (1 - kern$sigma) / kern$p2
  # The engine starts the reservoir empty, but result index 1 is state row 2, so
  # W[1] already holds one day of shedding. The engine sheds it from the state-row-1
  # stocks, which the results do not carry; the result-index-1 stocks stand in
  # for them. The difference is one day of shedding and is gone after burn-in.
  Wn <- reservoir(kern$w1 * Is + kern$w2 * Ia, kern$w1 * is0 + kern$w2 * ia0)
  out <- list(Lambda_hum = lag1(I_hat) / kern$D_h,
              Lambda_env = lag1(Wn) * delta / S_w,
              I_hat = I_hat)
  if (!is.null(shed_abs))
    out$W_hat <- reservoir(shed_abs[1L] * Is + shed_abs[2L] * Ia,
                           shed_abs[1L] * is0 + shed_abs[2L] * ia0)
  out
}

#' Daily route kernels at a constant decay rate (for provenance and plots)
#'
#' Generation-interval pmfs by lag (days from infector's infection to the
#' infectee's recorded infection) obtained by propagating a unit impulse
#' through \code{.mosaic_reff_infectiousness()} at a constant \code{delta}.
#'
#' @param kern Output of \code{.mosaic_reff_route_kernel()}.
#' @param delta Constant daily decay rate (capped at 1).
#' @param tail Mass left untabulated in the environmental tail.
#' @return List with \code{hum} and \code{env} pmfs (element L = lag L days)
#'   and their means \code{mean_hum}, \code{mean_env}.
#' @keywords internal
#' @noRd
.mosaic_reff_kernel_pmf <- function(kern, delta, tail = 1e-6) {
  delta <- min(delta, 1)
  horizon <- length(kern$Ps) +
    (if (delta < 1) ceiling(log(tail) / log1p(-delta)) else 0L) + 2L
  imp <- c(1, numeric(horizon))
  lam <- .mosaic_reff_infectiousness(imp, rep(delta, horizon + 1L), kern)
  hum <- lam$Lambda_hum[-1L]; env <- lam$Lambda_env[-1L]
  lags <- seq_along(hum)
  list(hum = hum, env = env,
       mean_hum = sum(lags * hum) / sum(hum),
       mean_env = sum(lags * env) / sum(env))
}

#' Initial latent/infectious stocks from simulated stocks at result index 1
#'
#' @param E1,Is1,Ia1 Simulated E, Isym, Iasym at result index 1 (\code{NULL}
#'   when the channel is unavailable, treated as 0).
#' @param inc1 Total infections recorded at result index 1 (already in E1).
#' @return \code{c(E, Isym, Iasym)}.
#' @keywords internal
#' @noRd
.mosaic_reff_init <- function(E1, Is1, Ia1, inc1) {
  v <- function(x) if (is.null(x) || !is.finite(x)) 0 else as.numeric(x)
  c(max(0, v(E1) - v(inc1)), v(Is1), v(Ia1))
}

#' Route-decomposed R for one location series
#'
#' A route is reported where its infectiousness is at least
#' \code{infectiousness_floor}. A route below the floor with no infections of
#' its own contributes 0 to the total rather than blanking it (otherwise a
#' near-silent route would hide the other one); with infections it leaves the
#' total undefined.
#'
#' @param inc_hum,inc_env Route incidence vectors (their sum is the source
#'   series).
#' @param delta Daily decay rates (same length).
#' @param kern Output of \code{.mosaic_reff_route_kernel()}.
#' @param infectiousness_floor Minimum route infectiousness (effective
#'   infectious infections) required to report that route.
#' @param init Optional initial stocks from \code{.mosaic_reff_init()}.
#' @param window Integer smoothing window in days (Cori's tau). \code{1} is the
#'   daily ratio; \code{w > 1} sums the numerator and the infectiousness over
#'   the trailing \code{w} days (\code{NA} until \code{w} days are available),
#'   and the floor then applies to the window-mean infectiousness.
#' @return List with \code{R_eff}, \code{R_hum}, \code{R_env}.
#' @keywords internal
#' @noRd
.mosaic_reff_routes <- function(inc_hum, inc_env, delta, kern,
                                infectiousness_floor = 1, init = NULL,
                                window = 1L) {
  inc_hum <- as.numeric(inc_hum); inc_env <- as.numeric(inc_env)
  lam <- .mosaic_reff_infectiousness(inc_hum + inc_env, as.numeric(delta), kern,
                                     init = init)
  window <- as.integer(window)
  if (length(window) != 1L || is.na(window) || window < 1L)
    stop(".mosaic_reff_routes: `window` must be a single integer >= 1")
  if (window > 1L) {
    trail <- function(v) {
      out <- as.numeric(stats::filter(v, rep(1 / window, window), sides = 1L))
      out[!is.finite(out)] <- NA_real_
      out
    }
    inc_hum <- trail(inc_hum); inc_env <- trail(inc_env)
    lam$Lambda_hum <- trail(lam$Lambda_hum); lam$Lambda_env <- trail(lam$Lambda_env)
  }
  R_hum <- .cori_reff(inc_hum, lam$Lambda_hum, infectiousness_floor)
  R_env <- .cori_reff(inc_env, lam$Lambda_env, infectiousness_floor)
  part <- function(R, num) ifelse(is.finite(R), R,
                                  ifelse(is.finite(num) & num == 0, 0, NA_real_))
  tot <- part(R_hum, inc_hum) + part(R_env, inc_env)
  tot[!is.finite(R_hum) & !is.finite(R_env)] <- NA_real_
  list(R_eff = tot, R_hum = R_hum, R_env = R_env)
}

#' Build the route kernel from a (medoid or member) config
#' @keywords internal
#' @noRd
.mosaic_reff_config_kernel <- function(config) {
  need <- c("iota", "gamma_1", "gamma_2", "sigma", "zeta_1", "zeta_2")
  miss <- need[vapply(need, function(nm) is.null(config[[nm]]), logical(1))]
  if (length(miss))
    stop("calc_Reff: config is missing kernel parameter(s): ",
         paste(miss, collapse = ", "), ".", call. = FALSE)
  s1 <- function(nm) as.numeric(config[[nm]])[1L]
  .mosaic_reff_route_kernel(s1("iota"), s1("gamma_1"), s1("gamma_2"),
                            s1("sigma"), s1("zeta_1"), s1("zeta_2"))
}

#' Decay-rate matrix delta_jt [nL x Tn] from a config, via the engine itself
#' @keywords internal
#' @noRd
.mosaic_reff_config_delta <- function(config, nL, Tn, location_names = NULL,
                                      date_start = NULL) {
  if (!is.null(location_names) && !is.null(config$location_name) &&
      !identical(as.character(location_names), as.character(config$location_name)))
    stop("calc_Reff: config location_name (", paste(config$location_name, collapse = ","),
         ") does not match the trajectory locations (",
         paste(location_names, collapse = ","), ").", call. = FALSE)
  if (!is.null(date_start) && !is.null(config$date_start) &&
      !isTRUE(as.Date(date_start) == as.Date(config$date_start)))
    stop("calc_Reff: config date_start ", config$date_start, " does not match ",
         "the trajectory date_start ", date_start, ".", call. = FALSE)
  par <- tryCatch(sim_params(config), error = function(e)
    stop("calc_Reff: could not rebuild delta_jt from config (",
         conditionMessage(e), ").", call. = FALSE))
  d <- t(if (!is.null(par$delta_jt)) par$delta_jt else sim_delta_jt(par))
  if (nrow(d) != nL || ncol(d) < Tn)
    stop("calc_Reff: config delta_jt is [", nrow(d), "x", ncol(d),
         "], trajectories need [", nL, "x", Tn, "].", call. = FALSE)
  pmin(d[, seq_len(Tn), drop = FALSE], 1)
}

#' Summary provenance for a route kernel
#' @keywords internal
#' @noRd
.mosaic_reff_kernel_params <- function(config, kern, delta) {
  dr <- range(delta[is.finite(delta)])
  fast <- .mosaic_reff_kernel_pmf(kern, dr[2L])
  s1 <- function(nm) as.numeric(config[[nm]])[1L]
  c(iota = s1("iota"), gamma_1 = s1("gamma_1"), gamma_2 = s1("gamma_2"),
    sigma = s1("sigma"), zeta_1 = s1("zeta_1"), zeta_2 = s1("zeta_2"),
    mean_hum     = fast$mean_hum,
    mean_env_min = fast$mean_env,
    mean_env_max = .mosaic_reff_kernel_pmf(kern, dr[1L])$mean_env)
}

#' Assemble the long reproductive_numbers table
#'
#' @param locs Location names.
#' @param dates Date vector of length Tn.
#' @param central Named list (by estimand) of nL x Tn matrices.
#' @param qmats Named list (by estimand) of nL x Tn x length(probs) arrays.
#' @param probs Quantile probabilities.
#' @return data.frame ordered by estimand (R_eff, R_hum, R_env), location, t.
#' @keywords internal
#' @noRd
.mosaic_reff_assemble <- function(locs, dates, central, qmats, probs) {
  prob_cols <- .mosaic_reff_prob_colnames(probs)
  Tn <- length(dates)
  parts <- list()
  for (est in .MOSAIC_REFF_ESTIMANDS) for (i in seq_along(locs)) {
    df <- data.frame(location = locs[i], date = dates, t = seq_len(Tn),
                     estimand = est, central = central[[est]][i, ],
                     stringsAsFactors = FALSE)
    for (k in seq_along(probs)) df[[prob_cols[k]]] <- qmats[[est]][i, , k]
    parts[[length(parts) + 1L]] <- df
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' Route-decomposed Cori effective reproductive number from ensemble trajectories
#'
#' Computes the per-location, time-varying Cori (2013) instantaneous
#' \strong{infection} reproductive number split by transmission route,
#' \eqn{R^{\mathrm{eff}}_{jt} = R^{\mathrm{hum}}_{jt} + R^{\mathrm{env}}_{jt}},
#' by an analytic reduction over the ensemble trajectories the run already
#' captured (no extra simulation).
#'
#' \strong{Estimand.} Each route's numerator is its own infection incidence
#' (\code{incidence_human}, \code{incidence_env}); both denominators are driven
#' by total incidence, because every infection is infectious through both
#' routes. \eqn{\Lambda^{hum}} propagates past infections through the latent and
#' infectious states (symptomatic and asymptomatic at equal weight, as in the
#' human force of infection). \eqn{\Lambda^{env}} additionally routes their
#' shedding (weighted by \code{zeta_1}, \code{zeta_2}) through the reservoir,
#' which decays at the \eqn{\psi}-dependent rate \eqn{\delta_{jt}}, so the
#' environmental generation interval (tens to hundreds of days) and its seasonal
#' variation are represented. Both kernels are derived from the engine's own
#' daily transition probabilities and phase order. The estimate is
#' \strong{instantaneous}: the reservoir is built from the actual past decay path
#' and one infection's lifetime reservoir contribution is evaluated at today's
#' \eqn{\delta_{jt}} (secondary infections per infection if conditions stayed as
#' they are at t). Nothing after t enters, so truncating the series does not
#' change earlier values. WASH (\code{theta_j}), the absolute
#' shedding scale, \code{kappa} and the transmission rates cancel from the
#' kernels and live in the R values.
#'
#' \strong{Initial conditions.} People already latent or infectious at the start
#' (the \code{E}, \code{Isym}, \code{Iasym} stocks on the first day) are included
#' in both infectiousness terms, so the series is defined from the start. R in
#' roughly the first \eqn{1/\delta} days still reflects the reservoir filling
#' from empty and is best read after a burn-in (\code{add_reproductive_numbers()}
#' applies one).
#'
#' \strong{Caveat.} This describes the simulated trajectory (comparable to a
#' surveillance-derived R_eff computed the same way); it is not an invasion
#' threshold. The renewal assumes transmission is linear in infectiousness; the
#' human FOI uses \eqn{I^{\alpha_1}} and the environmental dose saturates, so both
#' route values are trajectory descriptors, not per-contact constants.
#' Suitability \eqn{\psi} enters \eqn{R^{env}} twice, through
#' \code{beta_jt_env} and through the reservoir lifetime \eqn{1/\delta_{jt}},
#' and one infection's lifetime is valued at today's \eqn{\delta_{jt}}. Under
#' seasonal \eqn{\psi}, \eqn{R^{env} > 1} is therefore not a growth threshold: in
#' a high-\eqn{\psi} season it assumes survival that will not last the
#' infection's lifetime (up to ~200 days), and between seasons the reverse. R
#' here is also not comparable to literature cholera R estimated with a ~5-day
#' serial interval: for the same growth rate a longer generation interval gives
#' a larger R. The
#' renewal is per location: infectious people arriving through mobility
#' (\code{tau_i}, \code{pi_ij}) drive the destination's human force of infection
#' but are not in its \eqn{\Lambda^{hum}}, so in multi-location runs imported
#' spread is credited to the destination's R_hum.
#'
#' @param ensemble A \code{mosaic_trajectories} artifact
#'   (\code{2_calibration/trajectories_ensemble.rds}) or a \code{mosaic_ensemble}
#'   carrying one in \code{$trajectories}. Must provide weighted-median
#'   \code{incidence_human} and \code{incidence_env} channels; \code{E},
#'   \code{Isym} and \code{Iasym} supply the initial infectious stocks.
#' @param config The medoid \code{config} list: kernel parameters \code{iota},
#'   \code{gamma_1}, \code{gamma_2}, \code{sigma}, \code{zeta_1}, \code{zeta_2},
#'   plus the fields the engine needs to rebuild \eqn{\delta_{jt}} (\code{psi_jt},
#'   \code{decay_*}).
#' @param weights Optional per-member weights for the posterior reduction,
#'   indexed by member id. \code{NULL} (default) uses the weights in \code{lines}.
#' @param probs Credible-interval quantile probabilities.
#' @param infectiousness_floor Numeric scalar \eqn{\ge 0}. Minimum route
#'   infectiousness (effective past infections) required to report that route's
#'   R at a step (a route below it with no infections of its own contributes 0
#'   to the total). Default \code{1}; \code{0} is the pure Cori convention.
#' @param verbose Logical; emit progress messages.
#'
#' @return A tidy long \code{data.frame} (\code{reproductive_numbers} schema):
#'   \code{location}, \code{date}, \code{t}, \code{estimand} (\code{"R_eff"},
#'   \code{"R_hum"}, \code{"R_env"}), \code{central} (renewal on the
#'   weighted-median route incidences) and one column per quantile. Attributes:
#'   \code{central_matrix} (R_eff, nL x T), \code{route_central} (list of R_hum
#'   and R_env matrices), \code{env_share}, \code{kernel}
#'   (\code{"route_instantaneous"}), \code{kernel_params}, \code{ci_source},
#'   \code{caveat}.
#'
#' @details
#' \strong{Central on this path is a calendar-date descriptor.} The renewal on
#' weighted-median incidence is phase-smoothed across members and reads closer
#' to 1 than any coherent trajectory. \code{\link{add_reproductive_numbers}(
#' recompute_ci = TRUE)} reports the MEDOID trajectory's R_t and the per-member
#' peak statistic instead.
#'
#' \strong{Posterior CI.} Needs daily-consecutive per-member \code{lines} for
#' both route channels starting on day 1; production artifacts thin \code{lines}
#' on a stride, so the quantile columns are \code{NA} there (\code{ci_source =
#' "unavailable_strided_lines"}). Use the re-simulation path for a CI. A cell's
#' quantiles are reported only when members holding at least half the weight are
#' defined there.
#'
#' @references Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new framework
#'   and software to estimate time-varying reproduction numbers during epidemics.
#'   American Journal of Epidemiology 178(9):1505-1512.
#'
#' @seealso \code{\link{weighted_quantiles}}, \code{\link{plot_Reff}}
#' @export
calc_Reff <- function(ensemble,
                      config,
                      weights  = NULL,
                      probs    = c(0.025, 0.25, 0.5, 0.75, 0.975),
                      infectiousness_floor = 1,
                      verbose  = TRUE) {

  caveat <- paste0("Route-decomposed Cori R_eff (R_hum + R_env) computed on ",
                   "SIMULATED infection incidence: a descriptor of the model ",
                   "trajectory, NOT a first-principles reproductive number.")

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
    stop("calc_Reff: `config` must be the medoid config list.")
  if (!is.numeric(probs) || length(probs) == 0L || any(!is.finite(probs)) ||
      any(probs < 0) || any(probs > 1))
    stop("calc_Reff: `probs` must be finite numerics in [0, 1].")
  if (!is.numeric(infectiousness_floor) || length(infectiousness_floor) != 1L ||
      !is.finite(infectiousness_floor) || infectiousness_floor < 0)
    stop("calc_Reff: `infectiousness_floor` must be a single finite scalar >= 0.")

  loc_names <- traj$location_names
  nL <- traj$n_locations
  Tn <- traj$n_time_points
  med <- function(ch, required = TRUE) {
    m <- traj$summary[[ch]]$median
    if (is.null(m) || !is.matrix(m)) {
      if (!required) return(NULL)
      stop("calc_Reff: trajectory artifact has no `", ch, "` channel median ",
           "(summary$", ch, "$median). Route-decomposed R_eff needs the ",
           "incidence_human and incidence_env channels.", call. = FALSE)
    }
    if (nrow(m) != nL || ncol(m) != Tn)
      stop("calc_Reff: `", ch, "` median dims [", nrow(m), "x", ncol(m),
           "] do not match n_locations/n_time_points [", nL, "x", Tn, "].")
    m
  }
  inc_h <- med("incidence_human"); inc_e <- med("incidence_env")
  E_m <- med("E", FALSE); Is_m <- med("Isym", FALSE); Ia_m <- med("Iasym", FALSE)
  first <- function(m, i) if (is.null(m)) NULL else m[i, 1L]
  init <- lapply(seq_len(nL), function(i)
    .mosaic_reff_init(first(E_m, i), first(Is_m, i), first(Ia_m, i),
                      inc_h[i, 1L] + inc_e[i, 1L]))

  kern  <- .mosaic_reff_config_kernel(config)
  delta <- .mosaic_reff_config_delta(config, nL, Tn, loc_names, traj$date_start)

  central <- stats::setNames(lapply(.MOSAIC_REFF_ESTIMANDS, function(e)
    matrix(NA_real_, nL, Tn)), .MOSAIC_REFF_ESTIMANDS)
  for (i in seq_len(nL)) {
    rr <- .mosaic_reff_routes(inc_h[i, ], inc_e[i, ], delta[i, ], kern,
                              infectiousness_floor, init = init[[i]])
    for (e in .MOSAIC_REFF_ESTIMANDS) central[[e]][i, ] <- rr[[e]]
  }

  d0    <- tryCatch(as.Date(traj$date_start), error = function(e) NA)
  dates <- if (!is.na(d0)) d0 + (seq_len(Tn) - 1L) else as.Date(NA) + seq_len(Tn)

  qmats <- stats::setNames(lapply(.MOSAIC_REFF_ESTIMANDS, function(e)
    array(NA_real_, dim = c(nL, Tn, length(probs)))), .MOSAIC_REFF_ESTIMANDS)
  ci_source <- "weighted_quantiles_per_member"
  lines <- traj$lines
  route_lines <- if (is.data.frame(lines) && nrow(lines) > 0L)
    lines[lines$channel %in% c("incidence_human", "incidence_env"), , drop = FALSE] else
      NULL
  if (is.null(route_lines) || nrow(route_lines) == 0L ||
      !all(c("incidence_human", "incidence_env") %in% route_lines$channel)) {
    ci_source <- "unavailable_no_incidence_lines"
    if (verbose)
      message("calc_Reff: no per-member route incidence lines in artifact; ",
              "credible-interval columns returned as NA.")
  } else {
    t_present <- sort(unique(route_lines$t))
    if (length(t_present) >= 2L && all(diff(t_present) == 1L) && t_present[1L] != 1L) {
      ci_source <- "unavailable_lines_not_from_start"
      warning("calc_Reff: per-member `lines` start on day ", t_present[1L],
              "; the environmental generation interval spans months, so R ",
              "cannot be rebuilt without the earlier history. Returning the ",
              "point estimate with NA credible-interval columns.", call. = FALSE)
    } else if (!(length(t_present) >= 2L && all(diff(t_present) == 1L))) {
      ci_source <- "unavailable_strided_lines"
      warning("calc_Reff: per-member trajectory `lines` are time-strided ",
              "(stride != 1 day), on which the daily renewal is undefined; ",
              "returning the point estimate with NA credible-interval columns. ",
              "Use add_reproductive_numbers(recompute_ci = TRUE) for a ",
              "posterior CI.", call. = FALSE)
    } else {
      qmats <- .mosaic_reff_member_quantiles(
        route_lines = route_lines, kern = kern, delta = delta,
        loc_names = loc_names, t_present = t_present, nL = nL, Tn = Tn,
        probs = probs, weights = weights,
        infectiousness_floor = infectiousness_floor, init = init)
    }
  }

  out <- .mosaic_reff_assemble(loc_names, dates, central, qmats, probs)
  tot <- rowSums(inc_h + inc_e, na.rm = TRUE)
  attr(out, "central_matrix") <- central$R_eff
  attr(out, "route_central")  <- central[c("R_hum", "R_env")]
  attr(out, "location_names") <- loc_names
  attr(out, "dates")          <- dates
  attr(out, "env_share")      <- stats::setNames(ifelse(tot > 0,
                                                 rowSums(inc_e, na.rm = TRUE) / tot,
                                                 NA_real_), loc_names)
  attr(out, "kernel")         <- "route_instantaneous"
  attr(out, "series")         <- "infection_incidence"
  attr(out, "kernel_params")  <- .mosaic_reff_kernel_params(config, kern, delta)
  attr(out, "probs")          <- probs
  attr(out, "ci_source")      <- ci_source
  attr(out, "caveat")         <- caveat
  class(out) <- c("reproductive_numbers", "data.frame")

  if (verbose)
    message(sprintf("calc_Reff: R_eff = R_hum + R_env for %d location(s) x %d step(s); ",
                    nL, Tn), "CI source: ", ci_source, ".")
  out
}

#' Quantile-column names for the reproductive_numbers schema
#' @keywords internal
#' @noRd
.mosaic_reff_prob_colnames <- function(probs) {
  # Round away the 1-ulp error of products such as 0.07 * 100 before testing for
  # a whole percentage, and strip a trailing "." along with trailing zeros, so
  # 0.07 -> "q7" (not "q7.") and 0.025 -> "q2.5".
  pct <- round(probs * 100, 8)
  lab <- ifelse(pct == round(pct), sprintf("%d", as.integer(round(pct))),
                sub("\\.?0+$", "", sprintf("%.4f", pct)))
  paste0("q", lab)
}

#' Faithfulness diagnostics of one re-simulated member against its saved slice
#'
#' Over the cells where both are finite: relative total-case error \code{re},
#' Pearson correlation \code{cc}, the two totals and the max absolute cell
#' difference. A constant saved series (e.g. an all-zero extinct member) has no
#' defined correlation; when the re-simulation reproduces it exactly
#' (\code{max_abs == 0}) it is a perfect match and \code{cc = 1}, so the gate's
#' median-correlation criterion does not refuse a bit-exact re-simulation.
#' @param rv,sv Re-simulated and saved reported cases (flattened, same length).
#' @return List \code{re}, \code{cc}, \code{ssum}, \code{rsum}, \code{max_abs}.
#' @keywords internal
#' @noRd
.mosaic_reff_faithfulness <- function(rv, sv) {
  ok <- is.finite(rv) & is.finite(sv)
  out <- list(re = NA_real_, cc = NA_real_, ssum = NA_real_, rsum = NA_real_,
              max_abs = 0)
  if (!any(ok)) return(out)
  ssum <- sum(sv[ok]); rsum <- sum(rv[ok])
  mx <- max(abs(rv[ok] - sv[ok]))
  cc <- suppressWarnings(stats::cor(rv[ok], sv[ok]))
  if (!is.finite(cc) && mx == 0) cc <- 1
  list(re = if (ssum > 0) abs(rsum - ssum) / ssum else abs(rsum - ssum),
       cc = cc, ssum = ssum, rsum = rsum, max_abs = mx)
}

#' Re-simulate one posterior member for the R_eff CI
#'
#' One (param, stoch) member: rebuild its config from its seed, simulate, and
#' return its per-location route-decomposed R series (with its OWN kernel,
#' engine decay rates and initial stocks) plus the faithfulness diagnostics.
#'
#' Defined at FILE scope so the parallel dispatcher ships only the task, not the
#' calling frame. The heavy inputs come from the worker's global environment,
#' put there once by \code{clusterExport}, and the config is REBUILT from its
#' seed rather than broadcast.
#'
#' @param task List with \code{p}, \code{s} and \code{saved} (the saved
#'   \code{cases_array[, , p, s]} slice for this member).
#' @param ctx The shared inputs. Passed explicitly on the serial route; on the
#'   parallel route the caller exports it once per worker as \code{.rr_ctx} and
#'   leaves this \code{NULL} so the worker reads it from its own global
#'   environment rather than shipping a copy per task. (v0.85.0 read the globals
#'   unconditionally, which broke the serial route.)
#' @return A list of per-member results, or a \code{$error} string.
#' @noRd
.mosaic_reff_resim_member <- function(task, ctx = NULL) {
  tryCatch({
    if (is.null(ctx)) ctx <- get(".rr_ctx", envir = globalenv())
    nL <- ctx$nL; Tn <- ctx$Tn

    p <- task$p; s <- task$s
    cfg <- MOSAIC:::.mosaic_clamp_transmission_params(
      MOSAIC::sample_parameters(PATHS = ctx$paths, priors = ctx$priors,
                                config = ctx$base_config, seed = ctx$seeds[p],
                                sample_args = ctx$sampling, verbose = FALSE))
    kern <- MOSAIC:::.mosaic_reff_config_kernel(cfg)

    run_cfg <- cfg
    run_cfg$seed <- (p * 1000L) + s
    model <- MOSAIC::run_simulation(config = run_cfg, seed = run_cfg$seed, quiet = TRUE)
    res <- model$results
    mat <- function(ch) MOSAIC:::.mosaic_reff_to_mat(res[[ch]], nL, Tn)
    inc_h <- mat("incidence_human"); inc_e <- mat("incidence_env")
    delta <- mat("delta_jt"); E <- mat("E"); Is <- mat("Isym"); Ia <- mat("Iasym")
    rc_m  <- mat("reported_cases")
    inc_m <- mat("incidence")
    if (any(abs(inc_m - inc_h - inc_e) > 0, na.rm = TRUE))
      stop("incidence != incidence_human + incidence_env for member (", p, ",", s, ")")

    saved <- matrix(as.numeric(task$saved), nrow = nL, ncol = Tn)
    fd <- MOSAIC:::.mosaic_reff_faithfulness(as.numeric(rc_m), as.numeric(saved))
    re <- fd$re; cc <- fd$cc; ssum <- fd$ssum; rsum <- fd$rsum; mx <- fd$max_abs
    ests <- MOSAIC:::.MOSAIC_REFF_ESTIMANDS
    reff <- stats::setNames(lapply(ests, function(e) vector("list", nL)), ests)
    peak <- stats::setNames(lapply(ests, function(e) rep(NA_real_, nL)), ests)
    burn <- if (ctx$burn_in >= 1L) seq_len(min(ctx$burn_in, Tn)) else integer(0)
    for (i in seq_len(nL)) {
      init <- MOSAIC:::.mosaic_reff_init(E[i, 1L], Is[i, 1L], Ia[i, 1L],
                                         inc_m[i, 1L])
      rr <- MOSAIC:::.mosaic_reff_routes(inc_h[i, ], inc_e[i, ], delta[i, ],
                                         kern, ctx$floor, init = init)
      rw <- MOSAIC:::.mosaic_reff_routes(inc_h[i, ], inc_e[i, ], delta[i, ],
                                         kern, ctx$floor, init = init,
                                         window = ctx$peak_window)
      for (e in ests) {
        reff[[e]][[i]] <- rr[[e]]
        v <- rw[[e]]
        if (length(burn)) v[burn] <- NA_real_
        v <- v[is.finite(v)]
        if (length(v)) peak[[e]][i] <- max(v)
      }
    }
    kp <- vapply(c("iota", "gamma_1", "gamma_2", "sigma", "zeta_1", "zeta_2"),
                 function(nm) as.numeric(cfg[[nm]])[1L], numeric(1))

    list(p = p, s = s, reff = reff, peak = peak, re = re, cc = cc,
         ssum = ssum, rsum = rsum, max_abs = mx, kernel_params = kp)
  }, error = function(e) list(p = task$p, s = task$s, error = conditionMessage(e)))
}

#' Re-simulate the saved posterior ensemble and build a per-member R_eff CI
#'
#' Faithful re-simulation path for the route-decomposed R_eff posterior. The
#' persisted artifacts do not carry daily per-member route incidence, so the
#' exact posterior members are RE-RUN: each member's config is rebuilt with the
#' recipe \code{calc_model_ensemble()} uses (\code{sample_parameters(...,
#' seed = parameter_seeds[p])} then \code{.mosaic_clamp_transmission_params()}),
#' simulated with seed \code{param_idx * 1000L + stoch_idx}, and its
#' \code{reported_cases} compared with the saved \code{cases_array} (the
#' statistical-equivalence FAITHFULNESS GATE). Each member's R series uses its
#' own kernel, its own engine \code{delta_jt}, and its own initial stocks.
#'
#' \strong{Headline = the MEDOID trajectory's R_t (phase-coherent)}, selected by
#' \code{run_MOSAIC()}'s criterion. The per-calendar-day cross-member quantiles
#' are the calendar-date envelope (they regress toward 1 because member peaks
#' are phase-misaligned). \code{peak_Rt} holds the weighted quantiles of each
#' member's time-max of its \code{peak_window}-day Cori R_t.
#'
#' @param ensemble A \code{mosaic_ensemble} with \code{seeds},
#'   \code{parameter_weights}, \code{cases_array}, \code{n_param_sets},
#'   \code{n_simulations_per_config}, \code{location_names}, \code{date_start},
#'   \code{cases_median}.
#' @param base_config Base config the members were sampled from.
#' @param priors Priors object (\code{1_inputs/priors.json}).
#' @param sampling_args \code{control$sampling} used at calibration.
#' @param PATHS \code{get_paths()} result.
#' @param probs Quantile probabilities.
#' @param infectiousness_floor Passed to \code{.cori_reff}.
#' @param burn_in_days Leading days NA-masked before the per-member time-max.
#' @param peak_window Days in the trailing Cori window whose time-max is each
#'   member's peak R_t (default 7).
#' @param cases_central_method Central method used to select the medoid.
#' @param member_param_weights Optional length-\code{n_param_sets} weights, indexed
#'   like the ensemble's parameter dimension, that REPLACE
#'   \code{parameter_weights} -- e.g. the optimized subset's Gibbs weights mapped
#'   onto the candidate by seed, 0 outside the subset. \code{NULL} (default) uses
#'   \code{parameter_weights}.
#' @param param_subset Optional integer indices (into the parameter dimension) of
#'   the parameter sets in the posterior being described; the others are neither
#'   re-simulated nor eligible as the medoid. \code{NULL} (default) = all.
#' @param medoid_cases_central Optional central cases series (\code{[nL, T]}) to
#'   select the medoid against, overriding the ensemble's own; pass the final
#'   (optimized) ensemble's central series with \code{member_param_weights}.
#' @param gate_rel_tol,gate_frac,gate_cor_min Faithfulness-gate thresholds:
#'   the \code{gate_frac}-percentile and ensemble-aggregate relative total-case
#'   error must be \eqn{\le} \code{gate_rel_tol} and the median per-member cases
#'   correlation \eqn{\ge} \code{gate_cor_min}.
#' @param verbose Logical.
#' @param cl Optional cluster.
#' @return List with \code{qmats}, \code{central} (named lists by estimand),
#'   \code{central_definition}, \code{peak_Rt} (per location x estimand),
#'   \code{peak_window},
#'   \code{medoid_member}, \code{probs}, gate diagnostics, \code{n_members}
#'   (members actually re-simulated: final-posterior parameter sets x reruns
#'   with saved cases, not \code{nP * nS}),
#'   \code{kernel_params}.
#' @keywords internal
#' @noRd
.mosaic_reff_resim_ci <- function(ensemble, base_config, priors, sampling_args,
                                  PATHS,
                                  probs = c(0.025, 0.5, 0.975),
                                  infectiousness_floor = 1,
                                  burn_in_days = 0L,
                                  peak_window = 7L,
                                  cases_central_method = "mean",
                                  member_param_weights = NULL,
                                  param_subset = NULL,
                                  medoid_cases_central = NULL,
                                  gate_rel_tol = 0.05, gate_frac = 0.95,
                                  gate_cor_min = 0.95, verbose = TRUE,
                                  cl = NULL) {
  if (!inherits(ensemble, "mosaic_ensemble"))
    stop(".mosaic_reff_resim_ci: `ensemble` must be a mosaic_ensemble object.")
  for (nm in c("seeds", "parameter_weights", "cases_array", "n_param_sets",
               "n_simulations_per_config", "location_names"))
    if (is.null(ensemble[[nm]]))
      stop(".mosaic_reff_resim_ci: ensemble is missing `", nm, "`.")

  # This path drives the engine outside run_MOSAIC(), so pin threads here.
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
  if (!is.null(member_param_weights)) {
    pw <- as.numeric(member_param_weights)
    if (length(pw) != nP || any(!is.finite(pw)) || any(pw < 0) || sum(pw) <= 0)
      stop(".mosaic_reff_resim_ci: `member_param_weights` must be ", nP,
           " finite non-negative weights with a positive sum.")
  }
  in_subset <- rep(TRUE, nP)
  if (!is.null(param_subset)) {
    ps <- as.integer(param_subset)
    if (!length(ps) || anyNA(ps) || any(ps < 1L | ps > nP))
      stop(".mosaic_reff_resim_ci: `param_subset` must index 1..", nP, ".")
    in_subset[] <- FALSE; in_subset[ps] <- TRUE
  }
  bid <- suppressWarnings(as.integer(burn_in_days))
  if (length(bid) != 1L || is.na(bid) || bid < 0L) bid <- 0L
  pw_days <- suppressWarnings(as.integer(peak_window))
  if (length(pw_days) != 1L || is.na(pw_days) || pw_days < 1L)
    stop(".mosaic_reff_resim_ci: `peak_window` must be a single integer >= 1.")
  ests <- .MOSAIC_REFF_ESTIMANDS

  # Member index m = (s - 1) * nP + p ; weight = pw[p] / nS.
  n_members <- nP * nS
  reff_loc <- stats::setNames(lapply(ests, function(e)
    lapply(seq_len(nL), function(i) matrix(NA_real_, n_members, Tn))), ests)
  member_w <- numeric(n_members)
  peak_loc <- stats::setNames(lapply(ests, function(e)
    matrix(NA_real_, n_members, nL)), ests)
  # Statistical-equivalence gate. The R engine is bitwise-reproducible across
  # processes (test-sim_rng_contract.R), but the gate is kept robust rather than
  # exact because it is what catches a MIS-RECONSTRUCTED member config (wrong
  # seed or sampling recipe), and a lone near-critical member can flip
  # outbreak/no-outbreak without the posterior being wrong.
  re_vec  <- rep(NA_real_, n_members)
  cc_vec  <- rep(NA_real_, n_members)
  ssum_v  <- rep(NA_real_, n_members)
  rsum_v  <- rep(NA_real_, n_members)
  max_abs <- 0

  # A member is re-simulated only when it belongs to the posterior being
  # described (in_subset) AND has a saved slice to be faithful to. A member whose
  # saved cases are entirely non-finite failed at calibration: calc_model_ensemble
  # left it as an NA slice and renormalised over the survivors, so it gets weight
  # 0 here too (it would otherwise enter the band although the published
  # ensemble excluded it), and is not re-run (a deterministic engine failure
  # would recur and abort the whole CI).
  active <- logical(n_members)
  tasks <- vector("list", n_members)
  for (p in seq_len(nP)) for (s in seq_len(nS)) {
    m <- (s - 1L) * nP + p
    active[m] <- in_subset[p] && any(is.finite(ca[, , p, s]))
    member_w[m] <- if (active[m]) pw[p] / nS else 0
    if (active[m])
      tasks[[m]] <- list(p = p, s = s,
                         saved = matrix(as.numeric(ca[, , p, s, drop = FALSE]),
                                        nrow = nL, ncol = Tn))
  }
  if (!any(active))
    stop(".mosaic_reff_resim_ci: no ensemble member has saved cases to re-simulate.")
  n_skipped <- n_members - sum(active) - sum(!rep(in_subset, times = nS))
  if (verbose && n_skipped > 0L)
    message("    skipping ", n_skipped, " member(s) with no saved cases ",
            "(failed at calibration; weight 0, as in the published ensemble)")
  active_idx <- which(active)
  tasks <- tasks[active_idx]
  n_active <- length(active_idx)
  if (verbose) message("  Re-simulating ", n_active, " members (", sum(in_subset), " x ", nS,
                       " parameter sets x reruns", if (n_active < n_members)
                         paste0(", of ", n_members, " in the ensemble") else "", ")...")

  ctx <- list(base_config = base_config, priors = priors,
              sampling = sampling_args, paths = PATHS,
              seeds = parameter_seeds, floor = infectiousness_floor,
              burn_in = bid, peak_window = pw_days, nL = nL, Tn = Tn)

  use_cl <- !is.null(cl) && inherits(cl, "cluster") && length(cl) > 1L && n_active > 1L
  if (use_cl) {
    .rr_env <- new.env(parent = emptyenv())
    assign(".rr_ctx", ctx, envir = .rr_env)
    parallel::clusterExport(cl, ".rr_ctx", envir = .rr_env)
    parallel::clusterEvalQ(cl, MOSAIC:::.mosaic_set_blas_threads(1L))
    if (verbose) message("    on ", length(cl), " workers")
    # Reparent: a namespace binding serialises by REFERENCE, so a worker running
    # a different build would fail to resolve it (as in .mosaic_run_batch()).
    .w <- .mosaic_reff_resim_member
    environment(.w) <- globalenv()
    # Worker-death-robust gather: a worker killed at the OS level (OOM,
    # segfault) would otherwise block parLapplyLB forever on Linux. A dead
    # worker's task comes back with `$error` and fails the run below.
    res <- .mosaic_cluster_lapply_robust(
      cl, tasks, .w,
      idle_timeout_sec = as.numeric(getOption("MOSAIC.ensemble_worker_timeout_sec", 1800)),
      progress = isTRUE(verbose), label = ".mosaic_reff_resim_ci")
  } else {
    res <- lapply(tasks, function(tk) .mosaic_reff_resim_member(tk, ctx))
  }

  # Only members that have valid saved cases were dispatched, so any failure
  # here is a member the published ensemble contains: refuse.
  failed <- vapply(res, function(r) !is.list(r) || !is.null(r$error), logical(1))
  if (any(failed)) {
    r1 <- res[[which(failed)[1]]]
    stop(sprintf(".mosaic_reff_resim_ci: %d of %d members failed to re-simulate; first: %s",
                 sum(failed), length(res),
                 if (is.list(r1)) r1$error else paste(as.character(r1), collapse = " ")),
         call. = FALSE)
  }

  rm(tasks)
  member_kp <- vector("list", n_members)
  for (k in seq_along(res)) {
    m <- active_idx[k]
    r <- res[[k]]
    re_vec[m] <- r$re; cc_vec[m] <- r$cc
    ssum_v[m] <- r$ssum; rsum_v[m] <- r$rsum
    max_abs   <- max(max_abs, r$max_abs)
    member_kp[[m]] <- r$kernel_params
    for (e in ests) {
      for (i in seq_len(nL)) reff_loc[[e]][[i]][m, ] <- r$reff[[e]][[i]]
      peak_loc[[e]][m, ] <- r$peak[[e]]
    }
    res[k] <- list(NULL)   # free as we go: the gathered list is as large as reff_loc
  }
  rm(res); gc(FALSE)

  if (verbose)
    message(sprintf("    members done: %d/%d | rel_err med=%.4f p%.0f=%.4f | cor med=%.4f",
                    n_active, n_active, stats::median(re_vec, na.rm = TRUE),
                    gate_frac * 100,
                    stats::quantile(re_vec, gate_frac, na.rm = TRUE, names = FALSE),
                    stats::median(cc_vec, na.rm = TRUE)))

  # FAITHFULNESS GATE: percentile per-member error, ensemble-weighted aggregate
  # error, and median correlation. A systematic reconstruction bug fails all
  # three; a lone near-critical member fails none.
  n_compared <- sum(is.finite(re_vec))
  if (n_compared == 0L)
    stop(".mosaic_reff_resim_ci: FAITHFULNESS GATE FAILED -- no overlapping ",
         "reported_cases cells to compare against the saved cases_array.")
  rel_err_pct  <- stats::quantile(re_vec, gate_frac, na.rm = TRUE, names = FALSE)
  rel_err_max  <- max(re_vec, na.rm = TRUE)
  cor_median   <- stats::median(cc_vec, na.rm = TRUE)
  cor_min      <- min(cc_vec, na.rm = TRUE)
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

  # Calendar-date envelope per estimand.
  qmats <- stats::setNames(lapply(ests, function(e) {
    q <- array(NA_real_, dim = c(nL, Tn, length(probs)))
    for (i in seq_len(nL))
      q[i, , ] <- .mosaic_reff_cell_quantiles(reff_loc[[e]][[i]], member_w, probs)
    q
  }), ests)

  # Phase-coherent headline: the MEDOID member (run_MOSAIC criterion on the
  # saved cases_array, same per-channel central_method as calibration).
  cases_central <- if (!is.null(medoid_cases_central)) {
    medoid_cases_central
  } else if (identical(cases_central_method, "mean") &&
             !is.null(ensemble$cases_mean)) {
    ensemble$cases_mean
  } else {
    if (identical(cases_central_method, "mean"))
      warning(".mosaic_reff_resim_ci: central_method for cases is 'mean' but the ensemble ",
              "carries no cases_mean; the medoid target uses cases_median instead.",
              call. = FALSE)
    ensemble$cases_median
  }
  # The medoid is chosen among the parameter sets of the posterior being
  # described (the optimized subset when member_param_weights restricts it),
  # exactly as run_MOSAIC() chooses it on its final ensemble; indices are then
  # mapped back to this ensemble's parameter dimension.
  sub_p <- which(in_subset)
  medoid_sel <- .mosaic_reff_select_medoid_member(ca[, , sub_p, , drop = FALSE],
                                                  cases_central, length(sub_p), nS)
  if (!is.na(medoid_sel$param_idx)) {
    medoid_sel$param_idx <- sub_p[medoid_sel$param_idx]
    medoid_sel$member_id <- (medoid_sel$stoch_idx - 1L) * nP + medoid_sel$param_idx
    if (!active[medoid_sel$member_id]) medoid_sel$member_id <- NA_integer_
  }
  central_definition <- "medoid_trajectory"
  m_medoid <- medoid_sel$member_id

  # Per-member peak R_t (explosivity), per estimand: the time-max of the
  # peak_window-day Cori ratio after burn-in, computed on the worker. A daily
  # ratio's maximum lands on low-count days and measures Poisson noise.
  peak_prob_cols <- .mosaic_reff_prob_colnames(probs)
  peak_parts <- list()
  for (e in ests) for (i in seq_len(nL)) {
    pk <- peak_loc[[e]][, i]
    pq <- weighted_quantiles(pk, member_w, probs)
    row <- data.frame(location = locs[i], estimand = e, stringsAsFactors = FALSE)
    for (k in seq_along(probs)) row[[peak_prob_cols[k]]] <- pq[k]
    row$n_members <- sum(is.finite(pk) & is.finite(member_w) & member_w > 0)
    peak_parts[[length(peak_parts) + 1L]] <- row
  }
  peak_Rt <- do.call(rbind, peak_parts)
  rownames(peak_Rt) <- NULL

  # Fallback: the member whose R_eff peak is the weighted median (location 1).
  if (is.na(m_medoid)) {
    central_definition <- "member_with_median_peak_Rt"
    peaks1 <- peak_loc$R_eff[, 1L]
    med_peak <- weighted_quantiles(peaks1, member_w, 0.5)
    ok <- is.finite(peaks1)
    if (any(ok) && is.finite(med_peak))
      m_medoid <- which(ok)[which.min(abs(peaks1[ok] - med_peak))]
  }
  central <- stats::setNames(lapply(ests, function(e) {
    M <- matrix(NA_real_, nL, Tn)
    if (!is.na(m_medoid)) for (i in seq_len(nL)) M[i, ] <- reff_loc[[e]][[i]][m_medoid, ]
    M
  }), ests)

  list(qmats = qmats, central = central, probs = probs,
       central_definition = central_definition,
       peak_Rt = peak_Rt, peak_window = pw_days,
       medoid_member = list(member_id = m_medoid,
                            param_idx = medoid_sel$param_idx,
                            stoch_idx = medoid_sel$stoch_idx,
                            seed = if (!is.na(medoid_sel$param_idx))
                              parameter_seeds[medoid_sel$param_idx] else NA_integer_),
       gate_rel_err_pct = rel_err_pct, gate_rel_err_max = rel_err_max,
       gate_agg_rel_err = agg_rel_err, gate_cor_median = cor_median,
       gate_cor_min = cor_min, gate_max_abs_diff = max_abs,
       gate_n_outliers = n_outliers, gate_frac = gate_frac,
       n_members = n_active,
       kernel_params = if (!is.na(m_medoid)) member_kp[[m_medoid]] else NULL)
}

#' Select the medoid re-simulated member (run_MOSAIC criterion)
#'
#' Reuses the medoid definition from \code{run_MOSAIC()}: the param set whose
#' stochastic-MEDIAN \code{reported_cases} at location 1 minimises the log-scale
#' MAE (eps = 1) to the ensemble central cases series, then -- within that param
#' set -- the stochastic rerun closest (same metric) to that param set's own
#' median. Returns the flattened member id \code{m = (s - 1) * nP + p} aligned to
#' the resim path's member indexing, plus the (param_idx, stoch_idx). On failure
#' (no central, degenerate arrays) returns \code{member_id = NA} so the caller can
#' fall back to the median-peak member.
#'
#' @param ca Saved \code{cases_array} \code{[nL, T, nP, nS]} (reported_cases).
#' @param cases_central Ensemble central cases series (\code{[nL, T]} matrix or
#'   length-T vector); location 1 is used (matching run_MOSAIC()).
#' @param nP,nS Number of param sets / stochastic reruns.
#' @keywords internal
#' @noRd
.mosaic_reff_select_medoid_member <- function(ca, cases_central, nP, nS) {
  na_out <- list(member_id = NA_integer_, param_idx = NA_integer_,
                 stoch_idx = NA_integer_)
  if (is.null(cases_central)) return(na_out)
  cen <- if (is.matrix(cases_central)) as.numeric(cases_central[1L, ]) else
    as.numeric(cases_central)
  Tn <- dim(ca)[2L]
  if (length(cen) != Tn) return(na_out)
  eps <- 1.0
  param_dist <- vapply(seq_len(nP), function(p) {
    med_p <- apply(matrix(ca[1L, , p, , drop = TRUE], nrow = Tn, ncol = nS),
                   1L, stats::median, na.rm = TRUE)
    mean(abs(log(med_p + eps) - log(cen + eps)), na.rm = TRUE)
  }, numeric(1L))
  if (all(!is.finite(param_dist))) return(na_out)
  p_med <- which.min(param_dist)
  # NOTE: log-MAE distance vs a raw-space median is asymmetric, so at nS = 2
  # (median = the two reruns' raw mean) the larger rerun wins an exact tie. Both
  # are faithful coherent members of the medoid param set, so this only changes
  # which equivalent member is the headline; it is not a correctness issue.
  med_p <- apply(matrix(ca[1L, , p_med, , drop = TRUE], nrow = Tn, ncol = nS),
                 1L, stats::median, na.rm = TRUE)
  stoch_dist <- vapply(seq_len(nS), function(s) {
    mean(abs(log(ca[1L, , p_med, s] + eps) - log(med_p + eps)), na.rm = TRUE)
  }, numeric(1L))
  s_med <- if (all(!is.finite(stoch_dist))) 1L else which.min(stoch_dist)
  list(member_id = (s_med - 1L) * nP + p_med,
       param_idx = p_med, stoch_idx = s_med)
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

#' Per-cell weighted quantiles over members, requiring enough defined weight
#'
#' \code{weighted_quantiles()} drops members that are \code{NA} in a cell, so a
#' cell's band would otherwise describe whichever members happen to be defined
#' that day. Cells where the defined members hold less than \code{min_weight}
#' of the total weight are returned as \code{NA}.
#'
#' @param M Members x time matrix.
#' @param w Member weights.
#' @param probs Quantile probabilities.
#' @param min_weight Minimum defined weight fraction.
#' @return Time x length(probs) matrix.
#' @keywords internal
#' @noRd
.mosaic_reff_cell_quantiles <- function(M, w, probs, min_weight = 0.5) {
  Tn <- ncol(M)
  out <- matrix(NA_real_, Tn, length(probs))
  w_ok <- is.finite(w) & w > 0
  tot <- sum(w[w_ok])
  if (tot <= 0) return(out)
  for (t in seq_len(Tn)) {
    def <- is.finite(M[, t]) & w_ok
    if (sum(w[def]) >= min_weight * tot)
      out[t, ] <- weighted_quantiles(M[def, t], w[def], probs)
  }
  out
}

#' Per-member posterior reduction of route R via weighted_quantiles
#'
#' Reconstructs each member's daily route R series from its daily-consecutive
#' \code{incidence_human} / \code{incidence_env} \code{lines} (starting on day
#' 1), then reduces per (location, t) cell with
#' \code{.mosaic_reff_cell_quantiles()}. The kernel, decay rates and initial
#' stocks are held at the medoid config and the weighted-median stocks (the
#' lines do not carry per-member parameters or stocks). A member with a missing
#' day is dropped at that location rather than having the gap read as zero
#' infections.
#'
#' @param weights Optional per-member weights indexed by \strong{member id}
#'   (named: by name; unnamed: by id value), never by position -- the member set
#'   can differ across locations. \code{NULL} uses each member's carried weight.
#' @param init List (per location) of initial stocks from the central pass.
#' @keywords internal
#' @noRd
.mosaic_reff_member_quantiles <- function(route_lines, kern, delta, loc_names,
                                          t_present, nL, Tn, probs,
                                          weights = NULL,
                                          infectiousness_floor = 1,
                                          init = NULL) {
  ests <- .MOSAIC_REFF_ESTIMANDS
  qmats <- stats::setNames(lapply(ests, function(e)
    array(NA_real_, dim = c(nL, Tn, length(probs)))), ests)
  t_max <- max(t_present)
  cols <- seq_len(t_max)
  w_named <- !is.null(weights) && !is.null(names(weights))
  for (i in seq_len(nL)) {
    li <- route_lines[route_lines$location == loc_names[i], , drop = FALSE]
    if (nrow(li) == 0L) next
    members <- unique(li$member_id)
    mats <- stats::setNames(lapply(ests, function(e)
      matrix(NA_real_, length(members), t_max)), ests)
    mw <- rep(NA_real_, length(members))
    for (mi in seq_along(members)) {
      id <- members[mi]
      mm <- li[li$member_id == id, , drop = FALSE]
      dense <- function(ch) {
        v <- rep(NA_real_, t_max)
        r <- mm[mm$channel == ch, , drop = FALSE]
        v[r$t] <- r$value
        v
      }
      # Record the weight BEFORE the gap check: a dropped member keeps its
      # weight (with an all-NA row), so the "at least half the weight" rule in
      # .mosaic_reff_cell_quantiles() is measured against the total posterior
      # weight, not only the members that happen to be complete.
      w <- if (!is.null(weights)) {
        if (w_named) weights[as.character(id)] else weights[id]
      } else {
        mm$weight[1]
      }
      mw[mi] <- if (length(w) && is.finite(w[1])) w[1] else NA_real_
      ih <- dense("incidence_human"); ie <- dense("incidence_env")
      if (anyNA(ih) || anyNA(ie)) next
      rr <- .mosaic_reff_routes(ih, ie, delta[i, cols], kern, infectiousness_floor,
                                init = if (!is.null(init)) init[[i]])
      for (e in ests) mats[[e]][mi, ] <- rr[[e]]
    }
    for (e in ests)
      qmats[[e]][i, cols, ] <- .mosaic_reff_cell_quantiles(mats[[e]], mw, probs)
  }
  qmats
}
