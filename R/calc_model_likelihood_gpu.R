###############################################################################
## calc_model_likelihood_gpu.R
## Batched GPU-accelerated model likelihood via torch
###############################################################################

#' Batched GPU-Accelerated Model Likelihood
#'
#' Computes \code{\link{calc_model_likelihood}} for many simulations at once
#' using \pkg{torch} tensors. The NB core, guardrails, cumulative NB, and
#' max terms run on GPU; peak timing/magnitude and WIS remain on CPU.
#'
#' @param obs_cases Matrix \code{[n_loc, n_time]} of observed cases.
#' @param obs_deaths Matrix \code{[n_loc, n_time]} of observed deaths.
#' @param est_cases_3d Array \code{[n_sims, n_loc, n_time]} of estimated cases.
#' @param est_deaths_3d Array \code{[n_sims, n_loc, n_time]} of estimated deaths.
#' @param likelihood_settings List with likelihood control fields (from control$likelihood).
#' @param config Optional LASER config list (needed for peak timing/magnitude).
#' @param device Character: \code{"cuda"} or \code{"cpu"}. If \code{NULL}, auto-detected.
#' @return Numeric vector of length \code{n_sims} with total log-likelihoods.
#' @export
calc_model_likelihood_gpu <- function(obs_cases,
                                      obs_deaths,
                                      est_cases_3d,
                                      est_deaths_3d,
                                      likelihood_settings,
                                      config = NULL,
                                      device = NULL) {

  # --- Availability check ---
  if (!requireNamespace("torch", quietly = TRUE) || !torch::torch_is_installed()) {
    message("torch not available; falling back to serial CPU likelihood.")
    return(.gpu_cpu_fallback(obs_cases, obs_deaths, est_cases_3d, est_deaths_3d,
                             likelihood_settings, config))
  }

  if (is.null(device)) {
    device <- if (torch::cuda_is_available()) "cuda" else "cpu"
  }

  # --- Extract dimensions ---
  n_sims <- dim(est_cases_3d)[1]
  n_loc  <- dim(est_cases_3d)[2]
  n_time <- dim(est_cases_3d)[3]

  # --- Resolve settings ---
  ls <- likelihood_settings
  w_time_r <- if (!is.null(ls$.weights_time_resolved)) ls$.weights_time_resolved else rep(1, n_time)
  w_loc_r  <- if (!is.null(ls$weights_location)) ls$weights_location else rep(1, n_loc)
  weight_cases  <- if (!is.null(ls$weight_cases))  ls$weight_cases  else 1
  weight_deaths <- if (!is.null(ls$weight_deaths)) ls$weight_deaths else 1
  floor_ll      <- if (!is.null(ls$floor_likelihood)) ls$floor_likelihood else -999999999
  enable_guardrails <- isTRUE(ls$enable_guardrails)
  min_obs_for_ratio <- if (!is.null(ls$min_obs_for_ratio)) ls$min_obs_for_ratio else 1
  min_obs_for_ratio_deaths <- if (!is.null(ls$min_obs_for_ratio_deaths)) ls$min_obs_for_ratio_deaths else 10
  max_timestep_ratio <- if (!is.null(ls$max_timestep_ratio)) ls$max_timestep_ratio else 100
  min_timestep_ratio <- if (!is.null(ls$min_timestep_ratio)) ls$min_timestep_ratio else 0.01
  cumulative_over_ratio  <- if (!is.null(ls$cumulative_over_ratio))  ls$cumulative_over_ratio  else 10
  cumulative_under_ratio <- if (!is.null(ls$cumulative_under_ratio)) ls$cumulative_under_ratio else 0.1
  min_cumulative_for_check <- if (!is.null(ls$min_cumulative_for_check)) ls$min_cumulative_for_check else 100
  neg_corr_threshold <- if (!is.null(ls$negative_correlation_threshold)) ls$negative_correlation_threshold else 0
  nb_k_min_cases  <- if (!is.null(ls$nb_k_min_cases))  ls$nb_k_min_cases  else 3
  nb_k_min_deaths <- if (!is.null(ls$nb_k_min_deaths)) ls$nb_k_min_deaths else 3

  add_max_terms        <- isTRUE(ls$add_max_terms)
  add_peak_timing      <- isTRUE(ls$add_peak_timing)
  add_peak_magnitude   <- isTRUE(ls$add_peak_magnitude)
  add_cumulative_total <- isTRUE(ls$add_cumulative_total)
  add_wis              <- isTRUE(ls$add_wis)

  weight_max_terms        <- if (!is.null(ls$weight_max_terms))        ls$weight_max_terms        else 0.5
  weight_peak_timing      <- if (!is.null(ls$weight_peak_timing))      ls$weight_peak_timing      else 0.5
  weight_peak_magnitude   <- if (!is.null(ls$weight_peak_magnitude))   ls$weight_peak_magnitude   else 0.5
  weight_cumulative_total <- if (!is.null(ls$weight_cumulative_total)) ls$weight_cumulative_total else 0.3
  weight_wis              <- if (!is.null(ls$weight_wis))              ls$weight_wis              else 0.8

  # --- Precompute NB dispersion k per location (CPU, once) ---
  k_cases_r  <- numeric(n_loc)
  k_deaths_r <- numeric(n_loc)
  for (j in seq_len(n_loc)) {
    k_cases_r[j]  <- nb_size_from_obs_weighted(obs_cases[j, ],  w_time_r, k_min = nb_k_min_cases)
    k_deaths_r[j] <- nb_size_from_obs_weighted(obs_deaths[j, ], w_time_r, k_min = nb_k_min_deaths)
  }
  is_poisson_c <- is.infinite(k_cases_r)
  is_poisson_d <- is.infinite(k_deaths_r)
  # Replace Inf with a large finite value for tensor (Poisson handled by mask)
  k_cases_finite  <- ifelse(is_poisson_c, 1e10, k_cases_r)
  k_deaths_finite <- ifelse(is_poisson_d, 1e10, k_deaths_r)

  # --- Precompute weight masks (CPU) ---
  # mask_weights: zero out where obs or est is non-finite
  # For GPU path, precompute per-location finite masks from obs (shared across sims)
  obs_c_finite <- is.finite(obs_cases)
  obs_d_finite <- is.finite(obs_deaths)

  # --- Convert to torch tensors ---
  D <- device
  FP <- torch::torch_float64()

  # Handle NAs: replace with 0 before sending to GPU, use weight masks
  obs_c_clean <- obs_cases;  obs_c_clean[!obs_c_finite] <- 0
  obs_d_clean <- obs_deaths; obs_d_clean[!obs_d_finite] <- 0
  est_c_clean <- est_cases_3d;  est_c_clean[!is.finite(est_c_clean)] <- 0
  est_d_clean <- est_deaths_3d; est_d_clean[!is.finite(est_d_clean)] <- 0

  t_obs_c <- torch::torch_tensor(obs_c_clean, dtype = FP, device = D)$unsqueeze(1)  # [1, n_loc, n_time]
  t_obs_d <- torch::torch_tensor(obs_d_clean, dtype = FP, device = D)$unsqueeze(1)
  t_est_c <- torch::torch_tensor(est_c_clean, dtype = FP, device = D)               # [n_sims, n_loc, n_time]
  t_est_d <- torch::torch_tensor(est_d_clean, dtype = FP, device = D)

  t_w_time <- torch::torch_tensor(w_time_r, dtype = FP, device = D)  # [n_time]
  t_w_loc  <- torch::torch_tensor(w_loc_r,  dtype = FP, device = D)  # [n_loc]

  # Weight mask: [1, n_loc, n_time] from obs finite flags, broadcast across sims
  obs_c_fin_arr <- array(as.numeric(obs_c_finite), dim = c(1, n_loc, n_time))
  obs_d_fin_arr <- array(as.numeric(obs_d_finite), dim = c(1, n_loc, n_time))
  w_mask_c <- torch::torch_tensor(obs_c_fin_arr, dtype = FP, device = D)
  w_mask_d <- torch::torch_tensor(obs_d_fin_arr, dtype = FP, device = D)
  # Also zero where est is non-finite (already cleaned to 0, but mark in mask)
  est_c_fin_arr <- array(as.numeric(is.finite(est_cases_3d)),  dim = c(n_sims, n_loc, n_time))
  est_d_fin_arr <- array(as.numeric(is.finite(est_deaths_3d)), dim = c(n_sims, n_loc, n_time))
  est_c_fin_mask <- torch::torch_tensor(est_c_fin_arr, dtype = FP, device = D)
  est_d_fin_mask <- torch::torch_tensor(est_d_fin_arr, dtype = FP, device = D)
  w_mask_c <- w_mask_c * est_c_fin_mask  # [n_sims, n_loc, n_time]
  w_mask_d <- w_mask_d * est_d_fin_mask

  # Time weights with mask: [n_sims, n_loc, n_time]
  t_w_time_3d <- t_w_time$unsqueeze(1)$unsqueeze(2)  # [1, 1, n_time]
  w_cases_eff  <- w_mask_c * t_w_time_3d
  w_deaths_eff <- w_mask_d * t_w_time_3d

  # k tensors: [1, n_loc, 1]
  t_k_c <- torch::torch_tensor(k_cases_finite,  dtype = FP, device = D)$unsqueeze(1)$unsqueeze(3)
  t_k_d <- torch::torch_tensor(k_deaths_finite, dtype = FP, device = D)$unsqueeze(1)$unsqueeze(3)
  t_pois_c <- torch::torch_tensor(as.numeric(is_poisson_c), dtype = FP, device = D)$unsqueeze(1)$unsqueeze(3)
  t_pois_d <- torch::torch_tensor(as.numeric(is_poisson_d), dtype = FP, device = D)$unsqueeze(1)$unsqueeze(3)

  # =========================================================================
  # GUARDRAILS (GPU-batched)
  # =========================================================================
  floored <- torch::torch_zeros(n_sims, dtype = torch::torch_bool(), device = D)

  if (enable_guardrails) {
    # Per-timestep ratio: cases
    ratio_c <- t_est_c / (t_obs_c + 1e-300)
    valid_c <- (t_obs_c >= min_obs_for_ratio) & (t_est_c > 0)
    bad_c <- valid_c & ((ratio_c > max_timestep_ratio) | (ratio_c < min_timestep_ratio))
    floored <- floored | bad_c$any(dim = 3L)$any(dim = 2L)  # [n_sims]

    # Per-timestep ratio: deaths
    ratio_d <- t_est_d / (t_obs_d + 1e-300)
    valid_d <- (t_obs_d >= min_obs_for_ratio_deaths) & (t_est_d > 0)
    bad_d <- valid_d & ((ratio_d > max_timestep_ratio) | (ratio_d < min_timestep_ratio))
    floored <- floored | bad_d$any(dim = 3L)$any(dim = 2L)

    # Cumulative over/under prediction
    obs_c_tot <- t_obs_c$sum(dim = 3L)  # [1, n_loc]
    est_c_tot <- t_est_c$sum(dim = 3L)  # [n_sims, n_loc]
    obs_d_tot <- t_obs_d$sum(dim = 3L)
    est_d_tot <- t_est_d$sum(dim = 3L)

    .check_cum <- function(obs_tot, est_tot) {
      checkable <- obs_tot >= min_cumulative_for_check
      r <- est_tot / (obs_tot + 1e-300)
      bad <- checkable & ((r >= cumulative_over_ratio) | (r <= cumulative_under_ratio))
      bad$any(dim = 2L)  # [n_sims]
    }
    floored <- floored | .check_cum(obs_c_tot, est_c_tot) | .check_cum(obs_d_tot, est_d_tot)

    # Correlation check (batched Pearson)
    .check_corr <- function(t_obs, t_est, fin_mask, min_finite = 10L) {
      n_fin <- fin_mask$sum(dim = 3L)  # [n_sims, n_loc]
      checkable <- n_fin >= min_finite

      obs_masked <- t_obs * fin_mask
      est_masked <- t_est * fin_mask
      n <- n_fin$clamp(min = 1)  # avoid div by 0

      sum_x  <- obs_masked$sum(dim = 3L)
      sum_y  <- est_masked$sum(dim = 3L)
      sum_xy <- (obs_masked * est_masked)$sum(dim = 3L)
      sum_x2 <- (obs_masked * obs_masked)$sum(dim = 3L)
      sum_y2 <- (est_masked * est_masked)$sum(dim = 3L)

      num <- n * sum_xy - sum_x * sum_y
      den <- torch::torch_sqrt((n * sum_x2 - sum_x$pow(2)) * (n * sum_y2 - sum_y$pow(2)) + 1e-30)
      corr <- num / den  # [n_sims, n_loc]

      neg <- checkable & (corr < neg_corr_threshold)
      neg$any(dim = 2L)  # [n_sims]
    }
    floored <- floored | .check_corr(t_obs_c, t_est_c, w_mask_c)
    floored <- floored | .check_corr(t_obs_d, t_est_d, w_mask_d)
  }

  # =========================================================================
  # CORE NB LOG-LIKELIHOOD (GPU-batched)
  # =========================================================================
  .nb_ll_tensor <- function(t_obs, t_est, t_k, t_pois, w_eff) {
    est_safe <- torch::torch_clamp(t_est, min = 1e-300)

    # NB formula
    ll_nb <- torch::torch_lgamma(t_obs + t_k) - torch::torch_lgamma(t_k) -
             torch::torch_lgamma(t_obs + 1) +
             t_k * torch::torch_log(t_k / (t_k + est_safe)) +
             t_obs * torch::torch_log(est_safe / (t_k + est_safe))

    # Poisson limit where k = Inf
    ll_pois <- t_obs * torch::torch_log(est_safe) - est_safe - torch::torch_lgamma(t_obs + 1)
    ll <- torch::torch_where(t_pois > 0.5, ll_pois, ll_nb)

    # Zero-prediction penalties
    zero_pred_pos_obs <- (t_est <= 0) & (t_obs > 0)
    penalty_val <- -t_obs * log(1e6)
    ll <- torch::torch_where(zero_pred_pos_obs, penalty_val, ll)

    zero_zero <- (t_est <= 0) & (t_obs == 0)
    ll <- torch::torch_where(zero_zero, torch::torch_zeros_like(ll), ll)

    # Weighted sum over time -> [n_sims, n_loc]
    (ll * w_eff)$sum(dim = 3L)
  }

  # Check minimum observations for meaningful likelihood
  # Per-location: count finite observations across time (rows = locations)
  have_cases_r  <- rowSums(obs_c_finite) >= 3
  have_deaths_r <- rowSums(obs_d_finite) >= 3
  t_have_c <- torch::torch_tensor(as.numeric(have_cases_r),  dtype = FP, device = D)$unsqueeze(1)  # [1, n_loc]
  t_have_d <- torch::torch_tensor(as.numeric(have_deaths_r), dtype = FP, device = D)$unsqueeze(1)

  ll_cases_loc  <- .nb_ll_tensor(t_obs_c, t_est_c, t_k_c, t_pois_c, w_cases_eff) * t_have_c
  ll_deaths_loc <- .nb_ll_tensor(t_obs_d, t_est_d, t_k_d, t_pois_d, w_deaths_eff) * t_have_d

  # =========================================================================
  # CUMULATIVE PROGRESSION NB (GPU-batched)
  # =========================================================================
  ll_cum_c <- torch::torch_zeros(c(n_sims, n_loc), dtype = FP, device = D)
  ll_cum_d <- torch::torch_zeros(c(n_sims, n_loc), dtype = FP, device = D)

  if (add_cumulative_total) {
    timepoints <- c(0.25, 0.5, 0.75, 1.0)
    n_tp <- length(timepoints)
    per_tp_floor <- -1e9

    # For cumulative NB, when k=Inf (Poisson), serial uses flat k_fallback (NOT scaled)
    # When k is finite, serial uses k * end_idx
    k_fallback <- getOption("MOSAIC.cumulative_k", 10)

    for (tp in timepoints) {
      end_idx <- min(n_time, max(1L, round(n_time * tp)))
      idx_range <- seq_len(end_idx)

      cum_obs_c <- t_obs_c[, , idx_range, drop = FALSE]$sum(dim = 3L)  # [1, n_loc]
      cum_est_c <- t_est_c[, , idx_range, drop = FALSE]$sum(dim = 3L)  # [n_sims, n_loc]
      cum_obs_d <- t_obs_d[, , idx_range, drop = FALSE]$sum(dim = 3L)
      cum_est_d <- t_est_d[, , idx_range, drop = FALSE]$sum(dim = 3L)

      # Match serial ll_cumulative_progressive_nb: k_data * end_idx if finite, else k_fallback
      cum_k_c_r <- ifelse(is_poisson_c, k_fallback, k_cases_r * end_idx)
      cum_k_d_r <- ifelse(is_poisson_d, k_fallback, k_deaths_r * end_idx)
      cum_k_c <- torch::torch_tensor(cum_k_c_r, dtype = FP, device = D)$unsqueeze(1)  # [1, n_loc]
      cum_k_d <- torch::torch_tensor(cum_k_d_r, dtype = FP, device = D)$unsqueeze(1)

      .cum_nb_ll <- function(o, e, k) {
        e_safe <- torch::torch_clamp(e, min = .Machine$double.eps)
        o_round <- torch::torch_round(torch::torch_clamp(o, min = 0))
        ll <- torch::torch_lgamma(o_round + k) - torch::torch_lgamma(k) -
              torch::torch_lgamma(o_round + 1) +
              k * torch::torch_log(k / (k + e_safe)) +
              o_round * torch::torch_log(e_safe / (k + e_safe))
        # Floor non-finite and zero-prediction
        bad <- (e <= 0 & o > 0) | !torch::torch_isfinite(ll)
        torch::torch_where(bad, torch::torch_full_like(ll, per_tp_floor), ll)
      }

      ll_cum_c <- ll_cum_c + .cum_nb_ll(cum_obs_c, cum_est_c, cum_k_c) * t_have_c
      ll_cum_d <- ll_cum_d + .cum_nb_ll(cum_obs_d, cum_est_d, cum_k_d) * t_have_d
    }
    ll_cum_c <- ll_cum_c / n_tp
    ll_cum_d <- ll_cum_d / n_tp
  }

  # =========================================================================
  # MAX TERMS (GPU-batched)
  # =========================================================================
  ll_max_c <- torch::torch_zeros(c(n_sims, n_loc), dtype = FP, device = D)
  ll_max_d <- torch::torch_zeros(c(n_sims, n_loc), dtype = FP, device = D)

  if (add_max_terms) {
    obs_max_c <- t_obs_c$max(dim = 3L)[[1]]  # [1, n_loc]
    est_max_c <- t_est_c$max(dim = 3L)[[1]]  # [n_sims, n_loc]
    obs_max_d <- t_obs_d$max(dim = 3L)[[1]]
    est_max_d <- t_est_d$max(dim = 3L)[[1]]

    .max_pois_ll <- function(o_max, e_max) {
      o_r <- torch::torch_round(torch::torch_clamp(o_max, min = 0))
      e_s <- torch::torch_clamp(e_max, min = 1e-10)
      o_r * torch::torch_log(e_s) - e_s - torch::torch_lgamma(o_r + 1)
    }
    ll_max_c <- .max_pois_ll(obs_max_c, est_max_c) * t_have_c
    ll_max_d <- .max_pois_ll(obs_max_d, est_max_d) * t_have_d
  }

  # =========================================================================
  # ASSEMBLY (GPU)
  # =========================================================================
  ll_loc <- weight_cases * ll_cases_loc + weight_deaths * ll_deaths_loc

  if (add_max_terms)
    ll_loc <- ll_loc + weight_max_terms * (weight_cases * ll_max_c + weight_deaths * ll_max_d)
  if (add_cumulative_total)
    ll_loc <- ll_loc + weight_cumulative_total * (weight_cases * ll_cum_c + weight_deaths * ll_cum_d)

  # Weight by location, sum over locations -> [n_sims]
  ll_gpu <- (ll_loc * t_w_loc$unsqueeze(1))$sum(dim = 2L)
  ll_result <- as.numeric(ll_gpu$cpu())

  # =========================================================================
  # PEAK TIMING/MAGNITUDE + WIS (CPU, per-sim, only for non-floored sims)
  # =========================================================================
  floored_vec <- as.logical(floored$cpu())
  need_cpu <- (add_peak_timing || add_peak_magnitude || add_wis) && any(!floored_vec)

  if (need_cpu) {
    cpu_indices <- which(!floored_vec)

    # Precompute peak indices once (same as calc_model_likelihood)
    peak_indices_by_loc <- NULL
    if ((add_peak_timing || add_peak_magnitude) && !is.null(config)) {
      location_names <- config$location_name
      date_start_cfg <- config$date_start
      date_stop_cfg  <- config$date_stop
      if (!is.null(location_names) && !is.null(date_start_cfg) && !is.null(date_stop_cfg)) {
        date_seq <- seq(as.Date(date_start_cfg), as.Date(date_stop_cfg), by = "day")
        if (length(date_seq) != n_time) {
          date_seq <- seq(as.Date(date_start_cfg), as.Date(date_stop_cfg), by = "week")
          if (length(date_seq) != n_time) date_seq <- NULL
        }
        if (!is.null(date_seq)) {
          epidemic_peaks <- MOSAIC::epidemic_peaks
          peak_indices_by_loc <- vector("list", n_loc)
          for (j_pk in seq_len(n_loc)) {
            iso_code <- if (j_pk <= length(location_names)) location_names[j_pk] else NA_character_
            if (is.na(iso_code)) { peak_indices_by_loc[[j_pk]] <- integer(0); next }
            loc_peaks <- epidemic_peaks[epidemic_peaks$iso_code == iso_code, ]
            if (nrow(loc_peaks) == 0) { peak_indices_by_loc[[j_pk]] <- integer(0); next }
            idx <- vapply(loc_peaks$peak_date, function(pd) which.min(abs(date_seq - as.Date(pd))), integer(1))
            peak_indices_by_loc[[j_pk]] <- idx[idx > 0L & idx <= n_time]
          }
        }
      }
    }

    sigma_peak_time <- if (!is.null(ls$sigma_peak_time)) ls$sigma_peak_time else 1
    sigma_peak_log  <- if (!is.null(ls$sigma_peak_log))  ls$sigma_peak_log  else 0.5
    penalty_unmatched <- if (!is.null(ls$penalty_unmatched_peak)) ls$penalty_unmatched_peak else -3
    wis_quantiles <- if (!is.null(ls$wis_quantiles)) ls$wis_quantiles else c(0.025, 0.25, 0.5, 0.75, 0.975)
    cumulative_timepoints <- if (!is.null(ls$cumulative_timepoints)) ls$cumulative_timepoints else c(0.25, 0.5, 0.75, 1.0)

    for (si in cpu_indices) {
      est_c_mat <- est_cases_3d[si, , , drop = FALSE]
      dim(est_c_mat) <- c(n_loc, n_time)
      est_d_mat <- est_deaths_3d[si, , , drop = FALSE]
      dim(est_d_mat) <- c(n_loc, n_time)

      ll_cpu_add <- 0

      for (j in seq_len(n_loc)) {
        obs_c <- obs_cases[j, ];  est_c <- est_c_mat[j, ]
        obs_d <- obs_deaths[j, ]; est_d <- est_d_mat[j, ]
        have_c <- have_cases_r[j]
        have_d <- have_deaths_r[j]

        ll_pt_c <- ll_pt_d <- 0
        ll_pm_c <- ll_pm_d <- 0
        ll_wis_c <- ll_wis_d <- 0

        if ((add_peak_timing || add_peak_magnitude) && !is.null(peak_indices_by_loc)) {
          loc_peak_idx <- peak_indices_by_loc[[j]]
          if (length(loc_peak_idx) > 0) {
            if (add_peak_timing) {
              if (have_c) ll_pt_c <- .calc_peak_timing_from_indices(est_c, loc_peak_idx, sigma_peak_time, penalty_unmatched)
              if (have_d) ll_pt_d <- .calc_peak_timing_from_indices(est_d, loc_peak_idx, sigma_peak_time, penalty_unmatched)
            }
            if (add_peak_magnitude) {
              if (have_c) ll_pm_c <- .calc_peak_magnitude_from_indices(obs_c, est_c, loc_peak_idx, sigma_peak_log, penalty_unmatched)
              if (have_d) ll_pm_d <- .calc_peak_magnitude_from_indices(obs_d, est_d, loc_peak_idx, sigma_peak_log, penalty_unmatched)
            }
          }
        }

        if (add_wis) {
          if (have_c) {
            wis_c <- compute_wis_parametric_row(obs_c, est_c, w_time_r, wis_quantiles, k_use = k_cases_r[j])
            if (is.finite(wis_c)) ll_wis_c <- -wis_c
          }
          if (have_d) {
            wis_d <- compute_wis_parametric_row(obs_d, est_d, w_time_r, wis_quantiles, k_use = k_deaths_r[j])
            if (is.finite(wis_d)) ll_wis_d <- -wis_d
          }
        }

        ll_peaks <- weight_peak_timing    * (weight_cases * ll_pt_c + weight_deaths * ll_pt_d) +
                    weight_peak_magnitude * (weight_cases * ll_pm_c + weight_deaths * ll_pm_d)
        ll_wis_loc <- weight_wis * (weight_cases * ll_wis_c + weight_deaths * ll_wis_d)

        contrib <- 0
        if (add_peak_timing || add_peak_magnitude) contrib <- contrib + ll_peaks
        if (add_wis) contrib <- contrib + ll_wis_loc

        ll_cpu_add <- ll_cpu_add + w_loc_r[j] * contrib
      }

      ll_result[si] <- ll_result[si] + ll_cpu_add
    }
  }

  # Apply guardrail floor and non-finite cleanup
  ll_result[floored_vec] <- floor_ll
  ll_result[!is.finite(ll_result)] <- floor_ll

  ll_result
}


# ---------------------------------------------------------------------------
# CPU fallback: serial loop calling existing calc_model_likelihood()
# ---------------------------------------------------------------------------
.gpu_cpu_fallback <- function(obs_cases, obs_deaths, est_cases_3d, est_deaths_3d,
                              likelihood_settings, config) {
  ls <- likelihood_settings
  n_sims <- dim(est_cases_3d)[1]
  ll_vec <- numeric(n_sims)
  for (i in seq_len(n_sims)) {
    ll_vec[i] <- tryCatch(
      calc_model_likelihood(
        config       = config,
        obs_cases    = obs_cases,
        est_cases    = matrix(est_cases_3d[i, , ], nrow = nrow(obs_cases)),
        obs_deaths   = obs_deaths,
        est_deaths   = matrix(est_deaths_3d[i, , ], nrow = nrow(obs_deaths)),
        weights_time          = ls$.weights_time_resolved,
        weights_location      = ls$weights_location,
        nb_k_min_cases        = ls$nb_k_min_cases,
        nb_k_min_deaths       = ls$nb_k_min_deaths,
        add_max_terms         = ls$add_max_terms,
        add_peak_timing       = ls$add_peak_timing,
        add_peak_magnitude    = ls$add_peak_magnitude,
        add_cumulative_total  = ls$add_cumulative_total,
        add_wis               = ls$add_wis,
        weight_cases          = ls$weight_cases,
        weight_deaths         = ls$weight_deaths,
        weight_max_terms      = ls$weight_max_terms,
        weight_peak_timing    = ls$weight_peak_timing,
        weight_peak_magnitude = ls$weight_peak_magnitude,
        weight_cumulative_total = ls$weight_cumulative_total,
        weight_wis            = ls$weight_wis,
        sigma_peak_time       = ls$sigma_peak_time,
        sigma_peak_log        = ls$sigma_peak_log,
        penalty_unmatched_peak = ls$penalty_unmatched_peak,
        enable_guardrails     = ls$enable_guardrails,
        floor_likelihood      = ls$floor_likelihood,
        guardrail_verbose     = FALSE
      ),
      error = function(e) NA_real_
    )
  }
  ll_vec
}
