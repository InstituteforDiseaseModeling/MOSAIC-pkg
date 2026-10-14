# =============================================================================
# The coarsened (generalised-Bayes) posterior and its resampled ensemble
# =============================================================================
#
# Since MOSAIC 1.3.0 the calibration posterior is, by default
# (control$targets$best_subset_weighting = "coarsened"),
#
#     w_i  propto  exp(eta * (ll_i - max ll)),   eta = alpha / n_obs,
#
# over every draw with a finite log-likelihood, where n_obs is the number of
# scored observation cells (.mosaic_n_scored_obs()) and alpha =
# control$targets$coarsening_alpha is the effective number of independent
# observations the likelihood is trusted with (Miller & Dunson 2019; Bissiri,
# Holmes & Walker 2016). The ensemble members are a systematic
# sampling-importance-resample of K draws from these weights, collapsed to
# unique draws weighted by count / K, so every member-level consumer (ensemble,
# medoid, subset optimiser, trajectories) keeps working on a small set of
# equally-reasoned members. The posterior quantiles and densities are weighted
# summaries over those members (or the optimised subset), the draws the
# predictions use; the all-draws weights (weight_all) feed weight health, the
# auto-mode stopping rule and parameter_ess.csv. See
# MOSAIC-docs/05-model-calibration.Rmd.

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

#' Upper bound on the ensemble size under the coarsened posterior
#'
#' @param targets \code{control$targets} (merged).
#' @return \code{min(max_best_subset, max_ensemble_members)}; a targets list
#'   without \code{max_ensemble_members} is bounded by \code{max_best_subset}.
#' @noRd
.mosaic_ensemble_K_max <- function(targets) {
  cap <- targets$max_ensemble_members %||% Inf
  as.integer(min(targets$max_best_subset, cap))
}

#' Which bound set the ensemble size K
#'
#' @param ess Kish ESS of the coarsened weights.
#' @param targets \code{control$targets} (merged).
#' @return A short label for the run log.
#' @noRd
.mosaic_ensemble_K_binding <- function(ess, targets) {
  r <- round(ess)
  cap <- targets$max_ensemble_members %||% Inf
  if (r < targets$min_best_subset) return("bound by min_best_subset")
  if (r <= min(targets$max_best_subset, cap)) return("Kish ESS (no bound binds)")
  if (cap < targets$max_best_subset) "bound by max_ensemble_members" else "bound by max_best_subset"
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
#'   (\code{control$targets$min_best_subset} /
#'   \code{.mosaic_ensemble_K_max(control$targets)}).
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
