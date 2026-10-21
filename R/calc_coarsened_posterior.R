# =============================================================================
# The coarsened posterior and its ensembles
# =============================================================================
#
# The calibration posterior over every draw with a finite log-likelihood is
# the coarsened (generalised-Bayes) posterior
#
#     w_i  propto  exp(eta * (ll_i - max ll)),   eta = alpha / n_obs,
#
# where n_obs is the number of scored observation cells (.mosaic_n_scored_obs())
# and alpha = control$targets$coarsening_alpha is the effective number of
# independent observations the likelihood is trusted with (Miller & Dunson
# 2019; Bissiri, Holmes & Walker 2016). It is weight_all under every scheme.
#
# The ensemble members (is_best_subset / weight_best), from which the parameter
# posterior, the predictions, the medoid and the trajectories are all built,
# depend on control$targets$posterior_weighting:
#   "bestfit" (default since 1.4.0): the best-fit selection below, carrying
#     their coarsened weights renormalised;
#   "coarsened" (the 1.3.0 default, opt-in): a systematic
#     sampling-importance-resample of K draws, collapsed to unique draws
#     weighted by count / K.
# See MOSAIC-docs/05-model-calibration.Rmd.

#' Learning rate of the coarsened posterior
#'
#' @param alpha Effective number of independent observations
#'   (\code{control$targets$coarsening_alpha}).
#' @param n_obs Number of scored observation cells.
#' @return \code{alpha / n_obs}, or \code{NA_real_} when \code{n_obs} is not a
#'   positive finite number.
#' @noRd
.mosaic_coarsened_eta <- function(alpha, n_obs) {
  if (length(n_obs) != 1L || !is.finite(n_obs) || n_obs <= 0) return(NA_real_)
  alpha / n_obs
}

#' Seed of the systematic resample, derived from the run's config seed
#'
#' The resample must be reproducible for a given run and must not move the
#' caller's RNG stream. The seed is a fixed affine map of \code{config$seed}
#' (0 when the config has none).
#' @param config Model config list.
#' @return A single integer seed.
#' @noRd
.mosaic_resample_seed <- function(config) {
  base <- suppressWarnings(as.numeric(config$seed)[1])
  if (length(base) != 1L || !is.finite(base)) base <- 0
  as.integer((abs(round(base)) * 7919 + 104729) %% 2147483647)
}

#' Which bound set the resample size K under "coarsened"
#'
#' @param ess Kish ESS of the coarsened weights.
#' @param min_K,max_K The bounds (\code{control$targets$min_ensemble_members} /
#'   \code{max_ensemble_members}).
#' @return \code{"min_members"}, \code{"max_members"} or \code{"ratio"} (no
#'   bound binds: K is the rounded ESS), the summary.json
#'   \code{ensemble_binding} values.
#' @noRd
.mosaic_resample_binding <- function(ess, min_K, max_K) {
  r <- round(ess)
  if (r < min_K) "min_members" else if (r > max_K) "max_members" else "ratio"
}

#' Systematic resampling
#'
#' Draws \code{K} indices proportional to \code{w} with a single uniform offset
#' (Kitagawa 1996): positions \eqn{(u + k)/K}, \eqn{k = 0, \dots, K-1}, with
#' \eqn{u \sim U(0, 1)}. Each index \eqn{i} is selected
#' \eqn{\lfloor K w_i \rfloor} or \eqn{\lceil K w_i \rceil} times, which is the
#' lowest-variance unbiased resampler of the standard schemes. The caller's RNG
#' stream is left untouched.
#'
#' @param w Non-negative weights (normalised internally; non-finite treated as 0).
#' @param K Number of draws.
#' @param seed Integer seed for the single uniform offset.
#' @return Integer vector of counts, one per element of \code{w}, summing to \code{K}.
#' @noRd
.mosaic_systematic_resample <- function(w, K, seed) {
  w <- as.numeric(w)
  w[!is.finite(w) | w < 0] <- 0
  if (!(sum(w) > 0)) stop("systematic resample: weights sum to zero.", call. = FALSE)
  K <- as.integer(K)
  if (length(K) != 1L || is.na(K) || K < 1L)
    stop("systematic resample: K must be a positive integer.", call. = FALSE)
  # Rounding can carry the cumulative sum a few ulp past 1 before its last
  # element; capping keeps it non-decreasing (findInterval requires that) and
  # pins the end at exactly 1.
  cw <- pmin(cumsum(w / sum(w)), 1)
  cw[length(cw)] <- 1
  u0 <- .mosaic_with_fixed_seed(seed, function() stats::runif(1L))
  pos <- (u0 + (seq_len(K) - 1L)) / K
  # First index i with cw[i] >= pos: zero-weight entries share their
  # predecessor's cumulative weight and can never be the first to reach it.
  idx <- findInterval(pos, cw, left.open = TRUE) + 1L
  tabulate(idx, nbins = length(w))
}

#' Coarsened posterior over all draws, plus its systematic resample
#'
#' @param log_lik Per-draw log-likelihoods (non-finite get zero weight).
#' @param n_obs Number of scored observation cells.
#' @param alpha \code{control$targets$coarsening_alpha}.
#' @param min_K,max_K Bounds on the number of resampled draws
#'   (\code{control$targets$min_ensemble_members} /
#'   \code{control$targets$max_ensemble_members}).
#' @param seed Seed of the resample (\code{.mosaic_resample_seed()}).
#' @return A list: \code{eta}, \code{weights} (normalised, one per draw),
#'   \code{ess_kish} (of \code{weights}), \code{K} (draws resampled),
#'   \code{counts} (integer, one per draw), \code{member_weights}
#'   (\code{counts / K}, one per draw), \code{n_unique} and \code{seed}.
#' @noRd
.mosaic_coarsened_posterior <- function(log_lik, n_obs, alpha, min_K, max_K, seed) {
  eta <- .mosaic_coarsened_eta(alpha, n_obs)
  if (!is.finite(eta))
    stop("coarsened posterior: n_obs (scored observation cells) is not a positive number, ",
         "so eta = coarsening_alpha / n_obs is undefined.", call. = FALSE)
  if (sum(is.finite(log_lik)) < 1L)
    stop("coarsened posterior: no draw has a finite log-likelihood.", call. = FALSE)
  w <- .mosaic_tempered_weights(log_lik, eta)
  ess <- calc_model_ess(w, method = "kish")
  K <- as.integer(min(max(round(ess), min_K), max_K))
  counts <- .mosaic_systematic_resample(w, K, seed)
  list(eta = eta, weights = w, ess_kish = ess, K = K, counts = counts,
       member_weights = counts / K, n_unique = sum(counts > 0L), seed = seed)
}

# =============================================================================
# Best-fit ensemble selection and its Monte Carlo error (MOSAIC 1.4.0)
# =============================================================================
#
# The best-fit ensemble is the draws taken in descending log-likelihood until
# the Kish ESS of their coarsened weights, renormalised within the subset,
# reaches best_subset_ratio x ESS_all (the Kish ESS of the same weights over
# every valid draw), bounded by [min_members, max_members]. Members carry their
# coarsened posterior weights. The share of posterior weight inside the subset
# is m, and KL(ensemble || posterior) = -log(m). See
# claude/reporting_audit/SPEC_CONVERGENCE.md sections 2 and 4.

#' Best-fit ensemble selection on the coarsened posterior
#'
#' Sorts the valid draws by log-likelihood (descending; ties broken by
#' ascending \code{sim}) and adds them until the Kish ESS of their weights
#' \eqn{w_i = \exp\{\eta(\ell_i - \ell_{max})\}}, renormalised within the
#' subset, reaches \code{ratio * ESS_all}. Adding a draw whose weight is no
#' larger than any already selected never lowers the subset ESS, so the first
#' crossing is well defined and the full set always reaches the target
#' (\code{ESS_all >= ratio * ESS_all}). The size is then bounded by
#' \code{[min_members, max_members]}.
#'
#' Edge cases: all log-likelihoods equal gives equal weights, subset ESS = n,
#' and the \code{ceiling(ratio * n_valid)} lowest sim ids; fewer valid draws
#' than \code{min_members} makes every valid draw a member
#' (\code{binding = "min_members"}); a target not reached within
#' \code{max_members} stops at \code{max_members} (\code{binding =
#' "max_members"}, \code{ess_best < ess_target}); NA, NaN and \eqn{\pm}Inf
#' log-likelihoods are excluded; no valid draw, a \code{ratio} outside (0, 1],
#' a non-positive \code{eta} or \code{min_members > max_members} is an error.
#'
#' @param log_lik Per-draw log-likelihoods.
#' @param eta Learning rate of the coarsened posterior (\code{alpha / n_obs}).
#' @param ratio Target share of ESS_all, in (0, 1]
#'   (\code{control$targets$best_subset_ratio}).
#' @param min_members,max_members Bounds on the number of members (integers,
#'   \code{1 <= min_members <= max_members}).
#' @param sim Simulation ids, one per draw, used to break ties. Default
#'   \code{seq_along(log_lik)}.
#' @return A list: \code{idx} (member positions in \code{log_lik}, best first),
#'   \code{weights} (member weights renormalised to sum to 1, aligned with
#'   \code{idx}), \code{ess_all}, \code{ess_best} (Kish ESS of \code{weights}),
#'   \code{ess_target} (\code{ratio * ess_all}), \code{mass} (share of the
#'   posterior weight held by the members), \code{n_members}, \code{n_valid},
#'   \code{binding} (\code{"ratio"}, \code{"min_members"} or
#'   \code{"max_members"}), \code{capped} (\code{TRUE} when a bound, not the
#'   ratio, set the size), \code{capped_reason} (\code{NA} when not capped),
#'   \code{ll_cut} (log-likelihood of the last member), \code{eta} and
#'   \code{ratio}.
#' @noRd
.mosaic_bestfit_select <- function(log_lik, eta, ratio, min_members, max_members, sim = NULL) {
  log_lik <- suppressWarnings(as.numeric(log_lik))
  N <- length(log_lik)
  if (length(eta) != 1L || !is.numeric(eta) || !is.finite(eta) || eta <= 0)
    stop("best-fit selection: eta must be a single positive finite number.", call. = FALSE)
  if (length(ratio) != 1L || !is.numeric(ratio) || !is.finite(ratio) || ratio <= 0 || ratio > 1)
    stop("best-fit selection: ratio must be a single number in (0, 1].", call. = FALSE)
  ok_int <- function(x) length(x) == 1L && is.numeric(x) && is.finite(x) && x >= 1 && x == round(x)
  if (!ok_int(min_members) || !ok_int(max_members))
    stop("best-fit selection: min_members and max_members must be positive whole numbers.", call. = FALSE)
  min_members <- as.integer(min_members); max_members <- as.integer(max_members)
  if (min_members > max_members)
    stop("best-fit selection: min_members (", min_members, ") exceeds max_members (",
         max_members, ").", call. = FALSE)
  if (is.null(sim)) sim <- seq_len(N)
  if (length(sim) != N)
    stop("best-fit selection: sim must have one entry per draw.", call. = FALSE)

  valid <- which(is.finite(log_lik))
  n_valid <- length(valid)
  if (!n_valid) stop("best-fit selection: no draw has a finite log-likelihood.", call. = FALSE)

  o <- valid[order(-log_lik[valid], sim[valid])]
  ll <- log_lik[o]
  w <- exp(eta * (ll - ll[1L]))
  cs <- cumsum(w); cs2 <- cumsum(w^2)
  ess_all <- cs[n_valid]^2 / cs2[n_valid]
  target <- ratio * ess_all
  # The subset ESS is non-decreasing in n and equals ess_all at n_valid; the
  # relative tolerance only absorbs cumulative-sum rounding when ratio = 1.
  n_ratio <- which(cs^2 / cs2 >= target * (1 - 1e-12))[1L]
  if (is.na(n_ratio)) n_ratio <- n_valid

  capped_reason <- NA_character_
  if (n_valid < min_members) {
    n <- n_valid; binding <- "min_members"
    capped_reason <- sprintf("only %d valid draws, fewer than min_members = %d: every valid draw is a member",
                             n_valid, min_members)
  } else if (n_ratio > max_members) {
    n <- max_members; binding <- "max_members"
    capped_reason <- sprintf(paste0("the ensemble ESS target %.1f (ratio %g x ESS_all %.1f) is not reached ",
                                    "within max_members = %d"), target, ratio, ess_all, max_members)
  } else if (n_ratio < min_members) {
    n <- min_members; binding <- "min_members"
    capped_reason <- sprintf("the ensemble ESS target is reached with %d draws; raised to min_members = %d",
                             n_ratio, min_members)
  } else {
    n <- n_ratio; binding <- "ratio"
  }
  n <- as.integer(n)
  ess_best <- cs[n]^2 / cs2[n]
  if (identical(binding, "max_members"))
    capped_reason <- sprintf("%s (ensemble ESS %.1f)", capped_reason, ess_best)

  list(idx = o[seq_len(n)], weights = w[seq_len(n)] / cs[n],
       ess_all = ess_all, ess_best = ess_best, ess_target = target,
       mass = cs[n] / cs[n_valid], n_members = n, n_valid = n_valid,
       binding = binding, capped = !identical(binding, "ratio"), capped_reason = capped_reason,
       ll_cut = ll[n], eta = eta, ratio = ratio)
}

#' Type-7 quantile that tolerates infinite values
#'
#' \code{stats::quantile()} interpolates \code{0 * Inf} to \code{NaN}; here an
#' exact order statistic is returned unchanged and interpolation towards an
#' infinite value gives \code{Inf}.
#' @param x Numeric vector without NA.
#' @param p Single probability.
#' @return Numeric scalar (\code{NA_real_} for an empty \code{x}).
#' @noRd
.mosaic_quantile_inf <- function(x, p) {
  x <- sort(x)
  n <- length(x)
  if (!n) return(NA_real_)
  h <- (n - 1) * p + 1
  lo <- floor(h); hi <- ceiling(h)
  if (lo == hi || x[lo] == x[hi]) return(x[lo])
  if (is.infinite(x[hi])) return(x[hi])
  x[lo] + (h - lo) * (x[hi] - x[lo])
}

#' Seed-batch Monte Carlo error of the ensemble members' posterior means
#'
#' Members are split into \code{n_batches} seed batches by simulation id,
#' \code{(sim - 1) mod n_batches}; draw \code{sim} is sampled with seed
#' \code{sim}, so each batch is an independent sub-run. For each parameter the
#' Monte Carlo error is the SD of the within-batch weighted means divided by
#' the members' weighted posterior SD and by \eqn{\sqrt{B'}}, with \eqn{B'}
#' the number of batches holding at least \code{min_batch_weight} of the
#' members' weight (\code{n_batches} unless some batch is empty or nearly
#' so): the batch-means standard error of the ensemble's posterior mean in
#' posterior SDs (Flegal & Jones 2010). Batches below the floor are left out
#' of both \eqn{B'} and the between-batch SD (red-team round 1, T5). The
#' ratio is \code{.mosaic_batch_ratio()}. A parameter with zero posterior
#' spread among the members gives \code{NA} and is left out of the summaries;
#' fewer than two batches counted gives \code{Inf} for every parameter.
#'
#' It is a standard error: two independent reruns differ by about
#' \eqn{\sqrt{2}} times it (SD). Its true value is nearly the same for every
#' parameter (about \eqn{1/\sqrt{ESS_{best}}}), and each per-parameter value
#' is estimated from \eqn{B'} batches (about 9 degrees of freedom), so the
#' spread over parameters is mostly estimation noise and the 90th percentile is
#' about \eqn{\sqrt{\chi^2_{0.9, 9} / 9} \approx 1.28} times the typical
#' value (red-team round 1, T4).
#'
#' @param P Numeric matrix (or data frame) of parameter values, one row per
#'   draw, rows aligned with \code{sim}.
#' @param member_idx Row positions of the members in \code{P}.
#' @param member_w Non-negative member weights aligned with \code{member_idx}.
#' @param sim Simulation ids, one per row of \code{P}.
#' @param n_batches Number of seed batches (integer >= 2). Default 10.
#' @param min_batch_weight Smallest share of the members' weight for a batch
#'   to count. Default 0.01.
#' @return A list: \code{per_parameter} (named numeric), \code{median},
#'   \code{p90} (type-7 90th percentile), \code{max}, \code{n_parameters}
#'   (finite or infinite values summarised), \code{n_batches} and
#'   \code{n_batches_used}.
#' @noRd
.mosaic_member_mce <- function(P, member_idx, member_w, sim, n_batches = 10L, min_batch_weight = 0.01) {
  if (is.data.frame(P)) {
    num <- vapply(P, is.numeric, logical(1))
    P <- as.matrix(P[, num, drop = FALSE])
  }
  P <- as.matrix(P)
  if (nrow(P) != length(sim))
    stop("member MCE: P must have one row per entry of sim.", call. = FALSE)
  n_batches <- suppressWarnings(as.integer(n_batches))
  if (length(n_batches) != 1L || is.na(n_batches) || n_batches < 2L)
    stop("member MCE: n_batches must be an integer >= 2.", call. = FALSE)
  member_idx <- as.integer(member_idx)
  if (!length(member_idx) || anyNA(member_idx) || any(member_idx < 1L | member_idx > nrow(P)))
    stop("member MCE: member_idx must index rows of P.", call. = FALSE)
  member_w <- as.numeric(member_w)
  if (length(member_w) != length(member_idx))
    stop("member MCE: member_w must have one entry per member.", call. = FALSE)
  if (any(!is.finite(member_w) | member_w < 0) || !(sum(member_w) > 0))
    stop("member MCE: member_w must be finite, non-negative and sum to a positive number.", call. = FALSE)

  w <- member_w / sum(member_w)
  batch <- (as.integer(sim[member_idx]) - 1L) %% n_batches + 1L
  n_used <- sum(tapply(w, batch, sum) >= min_batch_weight)
  br <- .mosaic_batch_ratio(w, P[member_idx, , drop = FALSE], batch, min_batch_weight)
  mce <- br / sqrt(max(n_used, 1L))
  v <- mce[!is.na(mce)]
  v <- unname(v)
  list(per_parameter = mce,
       median = if (length(v)) stats::median(v) else NA_real_,
       p90 = .mosaic_quantile_inf(v, 0.9),
       max = if (length(v)) max(v) else NA_real_,
       n_parameters = length(v), n_batches = n_batches, n_batches_used = as.integer(n_used))
}

#' Pareto k-hat of the tempered importance weights
#'
#' The coarsened posterior reweights prior draws by
#' \eqn{\exp(\eta \ell_i)}, so its log importance ratios are
#' \eqn{\eta \ell_i} and its k-hat is \code{calc_is_diagnostics(eta * log_lik)}.
#' This is the validity guard of the convergence status; the k-hat of the
#' untempered likelihood (\eqn{\eta = 1}) is always huge and carries no
#' information about the posterior actually built.
#' @param log_lik Per-draw log-likelihoods (non-finite entries dropped).
#' @param eta Learning rate.
#' @return Numeric scalar k-hat with attribute \code{"status"}; \code{NA_real_}
#'   when the tail cannot be fitted (e.g. weights nearly flat or fewer than 20
#'   draws) or \code{eta} is not a positive finite number.
#' @noRd
.mosaic_khat_tempered <- function(log_lik, eta) {
  if (length(eta) != 1L || !is.numeric(eta) || !is.finite(eta) || eta <= 0) {
    out <- NA_real_; attr(out, "status") <- "eta undefined"; return(out)
  }
  ll <- as.numeric(log_lik)
  ll <- ll[is.finite(ll)]
  d <- calc_is_diagnostics(eta * ll)
  out <- as.numeric(d$khat)
  attr(out, "status") <- d$khat_status
  out
}

#' Trend of the tempered k-hat as draws accumulate
#'
#' The tempered weights \eqn{\exp\{\eta(\ell_i - \ell_{max})\}} are bounded,
#' so the true tail shape is \eqn{\le 0} and k-hat is pre-asymptotic: it falls
#' as draws accumulate (ZAF 2.78 at 5k draws to 0.57 at 50k on the production
#' pools; red-team round 1, T3). The trend is read from k-hat on the first
#' \eqn{n} valid draws in simulation order at \code{n_points} log-spaced sizes
#' from \eqn{N/4} to \eqn{N} (two doublings): the least-squares slope of k-hat
#' on \eqn{\log_2 n}. For an adaptive run these prefixes are the draws its own
#' checks saw, so this is the trend over the run's history; a fixed-budget run
#' and a backfill get the same quantity.
#' @param log_lik Per-draw log-likelihoods (non-finite entries dropped).
#' @param sim Simulation ids (draw order). Default \code{seq_along(log_lik)}.
#' @param eta Learning rate of the coarsened posterior.
#' @param n_points Number of prefix sizes. Default 5.
#' @param min_drop Fall per doubling of the draws needed to call k-hat falling.
#'   Default 0.02.
#' The label is \code{"uncertain"} when the slope is within twice its OLS
#' standard error of zero (round 2, TN2), \code{"falling"} when it falls by at
#' least \code{min_drop} per doubling, and \code{"not falling"} otherwise.
#' @return A list: \code{trend} (\code{"falling"}, \code{"not falling"},
#'   \code{"uncertain"} or \code{NA} with fewer than three finite k-hats on
#'   prefixes of at least 100 draws), \code{se} (OLS standard error of the
#'   slope; 0 when the fit is exact), \code{slope} (change of
#'   k-hat per doubling), \code{n} and \code{khat} (the prefix sizes and their
#'   k-hats).
#' @noRd
.mosaic_khat_trend <- function(log_lik, sim = NULL, eta, n_points = 5L, min_drop = 0.02) {
  ll <- suppressWarnings(as.numeric(log_lik))
  if (is.null(sim)) sim <- seq_along(ll)
  none <- list(trend = NA_character_, slope = NA_real_, se = NA_real_, n = numeric(0), khat = numeric(0))
  if (length(eta) != 1L || !is.numeric(eta) || !is.finite(eta) || eta <= 0) return(none)
  ok <- is.finite(ll)
  ll <- ll[ok][order(sim[ok])]
  N <- length(ll)
  ns <- sort(unique(round(N * 2^(-seq(0, 2, length.out = max(3L, as.integer(n_points)))))))
  ns <- ns[ns >= 100]                      # k-hat on fewer draws is noise
  if (length(ns) < 3L) return(none)
  kh <- vapply(ns, function(n) as.numeric(.mosaic_pareto_khat(eta * ll[seq_len(n)])), numeric(1))
  fin <- is.finite(kh)
  if (sum(fin) < 3L) return(utils::modifyList(none, list(n = ns, khat = kh)))
  x <- log2(ns[fin]); y <- kh[fin]
  sxx <- sum((x - mean(x))^2)
  b <- sum((x - mean(x)) * (y - mean(y))) / sxx
  res <- y - mean(y) - b * (x - mean(x))
  se <- if (length(x) > 2L) sqrt(sum(res^2) / (length(x) - 2L) / sxx) else NA_real_
  trend <- if (is.finite(se) && abs(b) < 2 * se) "uncertain"
           else if (b <= -min_drop) "falling" else "not falling"
  list(trend = trend, slope = b, se = se, n = ns, khat = kh)
}

#' Log-log growth slope of the posterior ESS
#'
#' Ordinary least-squares slope of \eqn{\log ESS} on \eqn{\log n} over the last
#' \code{n_last} points (by \code{n}). A slope near 1 means the ESS grows in
#' proportion to the draws, which is what makes the projection
#' \eqn{N \times target / ESS} valid; below 0.8 the growth is stalled. The
#' callers (\code{.mosaic_gate_slope()}) set \code{n_last} to the points at or
#' beyond a quarter of the largest draw count (at least three).
#' @param n Draw counts (positive).
#' @param ess Posterior ESS at those counts (positive).
#' @param n_last Number of trailing points used (>= 3). Default 3.
#' @return Numeric scalar, or \code{NA_real_} with fewer than three usable
#'   points (finite, positive, distinct \code{n}).
#' @noRd
.mosaic_ess_growth_slope <- function(n, ess, n_last = 3L) {
  n <- suppressWarnings(as.numeric(n)); ess <- suppressWarnings(as.numeric(ess))
  if (length(n) != length(ess))
    stop("ESS growth slope: n and ess must have the same length.", call. = FALSE)
  n_last <- max(3L, as.integer(n_last))
  ok <- is.finite(n) & is.finite(ess) & n > 0 & ess > 0
  n <- n[ok]; ess <- ess[ok]
  o <- order(n); n <- n[o]; ess <- ess[o]
  keep <- !duplicated(n, fromLast = TRUE)          # one point per draw count: the latest
  n <- n[keep]; ess <- ess[keep]
  if (length(n) < 3L) return(NA_real_)
  i <- utils::tail(seq_along(n), n_last)
  x <- log(n[i]); y <- log(ess[i])
  sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
}

#' Draws projected to reach the ESS_all target
#'
#' Because the posterior ESS grows in proportion to the draws, the total
#' needed is \eqn{N \times target / ESS_{all}}.
#' @param n_draws Draws simulated so far.
#' @param ess_all Kish ESS of the posterior over all draws.
#' @param ess_all_target Target ESS_all (\code{ESS_best / best_subset_ratio}).
#' @return Numeric scalar (total draws), or \code{NA_real_} when an input is
#'   missing or not positive.
#' @noRd
.mosaic_projected_draws <- function(n_draws, ess_all, ess_all_target) {
  v <- suppressWarnings(as.numeric(c(n_draws[1], ess_all[1], ess_all_target[1])))
  if (length(v) != 3L || any(!is.finite(v)) || any(v <= 0)) return(NA_real_)
  v[1] * v[3] / v[2]
}
