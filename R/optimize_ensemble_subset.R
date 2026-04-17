#' Optimize Ensemble Subset Size
#'
#' @description
#' Post-ensemble optimization that evaluates every possible top-N subset
#' (ranked by likelihood) and selects the N that maximizes prediction quality.
#' Produces a separate \code{mosaic_ensemble} object at the optimal subset size,
#' leaving the original ensemble untouched.
#'
#' For each candidate size N (from \code{min_n} to the full ensemble), the
#' function re-computes Gibbs weights within the top-N subset, re-computes
#' weighted median predictions from the 4D arrays, and scores with the selected
#' objective function.
#'
#' @param ensemble A \code{mosaic_ensemble} object returned by
#'   \code{\link{calc_model_ensemble}}.
#' @param likelihoods Numeric vector of log-likelihoods, one per parameter set
#'   in the ensemble. Must have length \code{ensemble$n_param_sets}.
#' @param seeds Optional numeric/integer vector of simulation seeds aligned
#'   with \code{likelihoods}. When supplied, the function returns
#'   \code{optimal_seeds} so callers can map the optimized subset back to the
#'   original \code{samples.parquet} rows without re-deriving the internal sort.
#' @param min_n Minimum subset size to evaluate. Default \code{30L} to guard
#'   against small-subset KDE degeneracy when the optimized subset drives
#'   posterior artifacts. Fox et al. (2024) report 4 as a statistical minimum
#'   for accuracy, but posterior density estimation needs larger N.
#' @param objective Scoring function: \code{"mae"} (default, normalized MAE),
#'   \code{"r2_bias"} (R-squared plus bias penalty), or \code{"wis"} (normalized
#'   Weighted Interval Score).
#' @param verbose Logical; if \code{TRUE}, emit progress messages.
#'
#' @return An S3 object of class \code{mosaic_subset_optimization} containing:
#'   \describe{
#'     \item{evaluation_table}{Data frame with one row per N evaluated.}
#'     \item{optimal_n}{Selected subset size.}
#'     \item{optimal_score}{Score at optimal N.}
#'     \item{optimal_weights}{Re-computed Gibbs weights for the optimal subset.}
#'     \item{optimal_indices}{Integer indices into the original ensemble arrays.}
#'     \item{optimal_seeds}{Simulation seeds of the optimal subset (when
#'       \code{seeds} supplied), else \code{NULL}.}
#'     \item{ensemble_optimized}{Complete \code{mosaic_ensemble} object at optimal N.}
#'     \item{stability_flag}{TRUE if score profile was flat.}
#'     \item{diagnostics_n}{Original diagnostics-selected N.}
#'     \item{diagnostics_score}{Score at the diagnostics-selected N.}
#'     \item{objective}{Which objective was used.}
#'   }
#'
#' @references
#' Bracher J et al. (2021). Evaluating epidemic forecasts in an interval format.
#' \emph{PLOS Computational Biology}, 17(2), e1008618.
#'
#' Gneiting T & Raftery AE (2007). Strictly Proper Scoring Rules, Prediction,
#' and Estimation. \emph{JASA}, 102(477), 359--378.
#'
#' @export
optimize_ensemble_subset <- function(ensemble,
                                     likelihoods,
                                     seeds = NULL,
                                     min_n = 30L,
                                     objective = c("mae", "r2_bias", "wis"),
                                     verbose = TRUE) {

  objective <- match.arg(objective)

  # ── 1. VALIDATE INPUTS ────────────────────────────────────────────────

  if (!inherits(ensemble, "mosaic_ensemble")) {
    stop("'ensemble' must be a mosaic_ensemble object.", call. = FALSE)
  }

  n_params <- ensemble$n_param_sets
  n_stoch  <- ensemble$n_simulations_per_config
  n_locs   <- ensemble$n_locations
  n_times  <- ensemble$n_time_points

  if (length(likelihoods) != n_params) {
    stop(sprintf("Length of 'likelihoods' (%d) must match ensemble$n_param_sets (%d).",
                 length(likelihoods), n_params), call. = FALSE)
  }
  if (any(!is.finite(likelihoods))) {
    stop("'likelihoods' must contain only finite values.", call. = FALSE)
  }

  if (!is.null(seeds)) {
    if (length(seeds) != n_params) {
      stop(sprintf("Length of 'seeds' (%d) must match ensemble$n_param_sets (%d).",
                   length(seeds), n_params), call. = FALSE)
    }
  }

  min_n <- max(as.integer(min_n), 1L)
  if (min_n > n_params) {
    warning(sprintf("min_n (%d) exceeds n_param_sets (%d); clamping to %d.",
                    min_n, n_params, n_params), call. = FALSE)
    min_n <- n_params
  }

  # ── 2. SORT BY LIKELIHOOD DESCENDING ──────────────────────────────────

  sort_order <- order(likelihoods, decreasing = TRUE)
  needs_sort <- !identical(sort_order, seq_len(n_params))

  if (needs_sort) {
    likelihoods  <- likelihoods[sort_order]
    cases_array  <- ensemble$cases_array[, , sort_order, , drop = FALSE]
    deaths_array <- ensemble$deaths_array[, , sort_order, , drop = FALSE]
    if (!is.null(seeds)) seeds <- seeds[sort_order]
  } else {
    cases_array  <- ensemble$cases_array
    deaths_array <- ensemble$deaths_array
  }

  # ── 3. PRE-COMPUTE CONSTANTS ──────────────────────────────────────────

  obs_c_flat <- as.numeric(ensemble$obs_cases)
  obs_d_flat <- as.numeric(ensemble$obs_deaths)
  mean_obs_c <- mean(obs_c_flat, na.rm = TRUE)
  mean_obs_d <- mean(obs_d_flat, na.rm = TRUE)

  # Guard against zero means for normalization
  if (!is.finite(mean_obs_c) || mean_obs_c == 0) mean_obs_c <- 1
  if (!is.finite(mean_obs_d) || mean_obs_d == 0) mean_obs_d <- 1

  envelope_quantiles <- ensemble$envelope_quantiles %||% c(0.025, 0.25, 0.75, 0.975)
  n_ci_pairs <- length(envelope_quantiles) / 2L
  wis_probs  <- c(0.025, 0.25, 0.5, 0.75, 0.975)

  max_n   <- n_params
  n_evals <- max_n - min_n + 1L

  # Pre-allocate evaluation table
  eval_table <- data.frame(
    n          = integer(n_evals),
    r2_cases   = numeric(n_evals),
    r2_deaths  = numeric(n_evals),
    bias_cases = numeric(n_evals),
    bias_deaths = numeric(n_evals),
    mae_cases  = numeric(n_evals),
    mae_deaths = numeric(n_evals),
    ess        = numeric(n_evals),
    score      = numeric(n_evals),
    stringsAsFactors = FALSE
  )

  if (verbose) message(sprintf("Optimizing ensemble subset: N = %d to %d (%s objective)...",
                               min_n, max_n, objective))

  # ── 4. EVALUATION LOOP ───────────────────────────────────────────────

  for (idx in seq_len(n_evals)) {

    n <- min_n + idx - 1L

    # 4a. Re-compute Gibbs weights for top-n
    ll_n    <- likelihoods[1:n]
    aic_n   <- -2 * ll_n
    delta_n <- aic_n - min(aic_n)
    actual_range <- diff(range(delta_n))

    if (actual_range < .Machine$double.eps) {
      weights_n <- rep(1 / n, n)
    } else {
      eta_n <- 0.5 * (4.0 / actual_range)
      weights_n <- calc_model_weights_gibbs(x = delta_n, eta = eta_n)
    }

    # 4b. Sim weights: each param weight divided among stochastic runs
    sim_weights_n <- rep(weights_n, each = n_stoch) / n_stoch

    # 4c. Compute weighted medians (and quantiles for WIS)
    median_c <- matrix(NA_real_, n_locs, n_times)
    median_d <- matrix(NA_real_, n_locs, n_times)

    if (objective == "wis") {
      q025_c <- q25_c <- q75_c <- q975_c <- matrix(NA_real_, n_locs, n_times)
      q025_d <- q25_d <- q75_d <- q975_d <- matrix(NA_real_, n_locs, n_times)

      for (i in seq_len(n_locs)) {
        for (j in seq_len(n_times)) {
          vals_c <- as.vector(cases_array[i, j, 1:n, ])
          vals_d <- as.vector(deaths_array[i, j, 1:n, ])
          qc <- weighted_quantiles(vals_c, sim_weights_n, wis_probs)
          qd <- weighted_quantiles(vals_d, sim_weights_n, wis_probs)
          q025_c[i, j] <- qc[1]; q25_c[i, j] <- qc[2]; median_c[i, j] <- qc[3]
          q75_c[i, j]  <- qc[4]; q975_c[i, j] <- qc[5]
          q025_d[i, j] <- qd[1]; q25_d[i, j] <- qd[2]; median_d[i, j] <- qd[3]
          q75_d[i, j]  <- qd[4]; q975_d[i, j] <- qd[5]
        }
      }
    } else {
      for (i in seq_len(n_locs)) {
        for (j in seq_len(n_times)) {
          vals_c <- as.vector(cases_array[i, j, 1:n, ])
          vals_d <- as.vector(deaths_array[i, j, 1:n, ])
          median_c[i, j] <- weighted_quantiles(vals_c, sim_weights_n, 0.5)
          median_d[i, j] <- weighted_quantiles(vals_d, sim_weights_n, 0.5)
        }
      }
    }

    # 4d. Flatten medians for scoring
    med_c_flat <- as.numeric(median_c)
    med_d_flat <- as.numeric(median_d)

    # 4e. Score with selected objective
    score_n <- switch(objective,
      "mae" = {
        mae_c <- mean(abs(med_c_flat - obs_c_flat), na.rm = TRUE)
        mae_d <- mean(abs(med_d_flat - obs_d_flat), na.rm = TRUE)
        -(mae_c / mean_obs_c + mae_d / mean_obs_d)
      },
      "r2_bias" = {
        r2_c   <- calc_model_R2(obs_c_flat, med_c_flat)
        r2_d   <- calc_model_R2(obs_d_flat, med_d_flat)
        bias_c <- calc_bias_ratio(obs_c_flat, med_c_flat)
        bias_d <- calc_bias_ratio(obs_d_flat, med_d_flat)
        lbc <- if (is.finite(bias_c) && bias_c > 0) abs(log(bias_c)) else 10
        lbd <- if (is.finite(bias_d) && bias_d > 0) abs(log(bias_d)) else 10
        r2_c + r2_d - 0.5 * (lbc + lbd)
      },
      "wis" = {
        wis_c <- .compute_wis_from_quantiles(obs_c_flat,
                   as.numeric(q025_c), as.numeric(q25_c), med_c_flat,
                   as.numeric(q75_c), as.numeric(q975_c))
        wis_d <- .compute_wis_from_quantiles(obs_d_flat,
                   as.numeric(q025_d), as.numeric(q25_d), med_d_flat,
                   as.numeric(q75_d), as.numeric(q975_d))
        -(wis_c / mean_obs_c + wis_d / mean_obs_d)
      }
    )

    # 4f. Record ALL metrics regardless of which drives selection
    mae_c_n  <- mean(abs(med_c_flat - obs_c_flat), na.rm = TRUE)
    mae_d_n  <- mean(abs(med_d_flat - obs_d_flat), na.rm = TRUE)
    r2_c_n   <- calc_model_R2(obs_c_flat, med_c_flat)
    r2_d_n   <- calc_model_R2(obs_d_flat, med_d_flat)
    bias_c_n <- calc_bias_ratio(obs_c_flat, med_c_flat)
    bias_d_n <- calc_bias_ratio(obs_d_flat, med_d_flat)
    ess_n    <- calc_model_ess(weights_n)

    eval_table[idx, ] <- list(
      n = n, r2_cases = r2_c_n, r2_deaths = r2_d_n,
      bias_cases = bias_c_n, bias_deaths = bias_d_n,
      mae_cases = mae_c_n, mae_deaths = mae_d_n,
      ess = ess_n, score = score_n
    )
  }

  # ── 5. SELECT OPTIMAL N ──────────────────────────────────────────────

  stability_tol  <- 0.02
  scores         <- eval_table$score
  max_score      <- max(scores, na.rm = TRUE)
  score_range    <- max_score - min(scores, na.rm = TRUE)
  stability_flag <- (score_range <= stability_tol)

  if (stability_flag) {
    # Flat profile: pick largest N (regularization tiebreaker)
    optimal_idx <- n_evals
  } else {
    optimal_idx <- which.max(scores)
  }

  optimal_n       <- eval_table$n[optimal_idx]
  optimal_score   <- eval_table$score[optimal_idx]
  diagnostics_n   <- max_n
  diagnostics_score <- eval_table$score[n_evals]

  if (verbose) {
    message(sprintf("  Optimal N = %d (score = %.4f), diagnostics N = %d (score = %.4f)%s",
                    optimal_n, optimal_score, diagnostics_n, diagnostics_score,
                    if (stability_flag) " [flat profile]" else ""))
  }

  # ── 6. BUILD OPTIMIZED ENSEMBLE ──────────────────────────────────────

  optimal_indices <- 1:optimal_n
  optimal_seeds   <- if (!is.null(seeds)) seeds[optimal_indices] else NULL

  # Re-compute Gibbs weights at optimal N
  ll_opt    <- likelihoods[optimal_indices]
  aic_opt   <- -2 * ll_opt
  delta_opt <- aic_opt - min(aic_opt)
  range_opt <- diff(range(delta_opt))

  if (range_opt < .Machine$double.eps) {
    optimal_weights <- rep(1 / optimal_n, optimal_n)
  } else {
    eta_opt <- 0.5 * (4.0 / range_opt)
    optimal_weights <- calc_model_weights_gibbs(x = delta_opt, eta = eta_opt)
  }

  # Slice arrays
  opt_cases  <- cases_array[, , optimal_indices, , drop = FALSE]
  opt_deaths <- deaths_array[, , optimal_indices, , drop = FALSE]

  # Re-compute all statistics (medians, means, CIs)
  sim_weights_opt <- rep(optimal_weights, each = n_stoch) / n_stoch

  cases_mean_m   <- matrix(NA_real_, n_locs, n_times)
  cases_median_m <- matrix(NA_real_, n_locs, n_times)
  deaths_mean_m   <- matrix(NA_real_, n_locs, n_times)
  deaths_median_m <- matrix(NA_real_, n_locs, n_times)

  cases_ci <- lapply(seq_len(n_ci_pairs), function(ci_idx) {
    list(lower = matrix(NA_real_, n_locs, n_times),
         upper = matrix(NA_real_, n_locs, n_times))
  })
  deaths_ci <- lapply(seq_len(n_ci_pairs), function(ci_idx) {
    list(lower = matrix(NA_real_, n_locs, n_times),
         upper = matrix(NA_real_, n_locs, n_times))
  })

  for (i in seq_len(n_locs)) {
    for (j in seq_len(n_times)) {
      vals_c <- as.vector(opt_cases[i, j, , ])
      vals_d <- as.vector(opt_deaths[i, j, , ])

      cases_mean_m[i, j]   <- sum(vals_c * sim_weights_opt, na.rm = TRUE)
      cases_median_m[i, j] <- weighted_quantiles(vals_c, sim_weights_opt, 0.5)
      deaths_mean_m[i, j]   <- sum(vals_d * sim_weights_opt, na.rm = TRUE)
      deaths_median_m[i, j] <- weighted_quantiles(vals_d, sim_weights_opt, 0.5)

      all_q_c <- weighted_quantiles(vals_c, sim_weights_opt, envelope_quantiles)
      all_q_d <- weighted_quantiles(vals_d, sim_weights_opt, envelope_quantiles)

      for (ci_idx in seq_len(n_ci_pairs)) {
        lower_idx <- ci_idx
        upper_idx <- length(envelope_quantiles) - ci_idx + 1L
        cases_ci[[ci_idx]]$lower[i, j]  <- all_q_c[lower_idx]
        cases_ci[[ci_idx]]$upper[i, j]  <- all_q_c[upper_idx]
        deaths_ci[[ci_idx]]$lower[i, j] <- all_q_d[lower_idx]
        deaths_ci[[ci_idx]]$upper[i, j] <- all_q_d[upper_idx]
      }
    }
  }

  ensemble_optimized <- structure(
    list(
      cases_array               = opt_cases,
      deaths_array              = opt_deaths,
      cases_mean                = cases_mean_m,
      cases_median              = cases_median_m,
      deaths_mean               = deaths_mean_m,
      deaths_median             = deaths_median_m,
      ci_bounds                 = list(cases = cases_ci, deaths = deaths_ci),
      obs_cases                 = ensemble$obs_cases,
      obs_deaths                = ensemble$obs_deaths,
      parameter_weights         = optimal_weights,
      n_param_sets              = as.integer(optimal_n),
      n_simulations_per_config  = n_stoch,
      n_successful              = as.integer(optimal_n * n_stoch),
      location_names            = ensemble$location_names,
      n_locations               = n_locs,
      n_time_points             = n_times,
      date_start                = ensemble$date_start,
      date_stop                 = ensemble$date_stop,
      envelope_quantiles        = envelope_quantiles
    ),
    class = "mosaic_ensemble"
  )

  # ── 7. RETURN ─────────────────────────────────────────────────────────

  structure(
    list(
      evaluation_table  = eval_table,
      optimal_n         = as.integer(optimal_n),
      optimal_score     = optimal_score,
      optimal_weights   = optimal_weights,
      optimal_indices   = optimal_indices,
      optimal_seeds     = optimal_seeds,
      ensemble_optimized = ensemble_optimized,
      stability_flag    = stability_flag,
      diagnostics_n     = as.integer(diagnostics_n),
      diagnostics_score = diagnostics_score,
      objective         = objective
    ),
    class = "mosaic_subset_optimization"
  )
}


# ── Internal: WIS from empirical quantiles ──────────────────────────────
#
# Implements the Bracher et al. (2021) Weighted Interval Score formula
# using pre-computed empirical quantiles from the ensemble.
#
# @param obs_flat Flattened observed values.
# @param q025_flat,q25_flat,q50_flat,q75_flat,q975_flat Flattened quantile vectors.
# @return Scalar WIS (lower is better).
# @keywords internal
.compute_wis_from_quantiles <- function(obs_flat, q025_flat, q25_flat,
                                        q50_flat, q75_flat, q975_flat) {
  # Bracher et al. 2021: WIS = (1/(K+0.5)) * [0.5*MAE + sum_k (alpha_k/2)*IS_k]
  # K = 2 interval pairs: 50% (q25-q75) and 95% (q025-q975)

  # MAE term (0.5 coefficient per Bracher)
  mae_term <- 0.5 * mean(abs(obs_flat - q50_flat), na.rm = TRUE)

  # 50% interval (alpha = 0.5)
  alpha_50 <- 0.5
  width_50 <- q75_flat - q25_flat
  pen_lower_50 <- (2 / alpha_50) * pmax(q25_flat - obs_flat, 0)
  pen_upper_50 <- (2 / alpha_50) * pmax(obs_flat - q75_flat, 0)
  is_50 <- mean(width_50 + pen_lower_50 + pen_upper_50, na.rm = TRUE)

  # 95% interval (alpha = 0.05)
  alpha_95 <- 0.05
  width_95 <- q975_flat - q025_flat
  pen_lower_95 <- (2 / alpha_95) * pmax(q025_flat - obs_flat, 0)
  pen_upper_95 <- (2 / alpha_95) * pmax(obs_flat - q975_flat, 0)
  is_95 <- mean(width_95 + pen_lower_95 + pen_upper_95, na.rm = TRUE)

  K <- 2
  (mae_term + (alpha_50 / 2) * is_50 + (alpha_95 / 2) * is_95) / (K + 0.5)
}
