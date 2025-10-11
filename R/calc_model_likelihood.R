###############################################################################
## calc_model_likelihood.R  (Balanced design; NB uses weighted k with k_min)
###############################################################################

#' Compute the total model likelihood (Simplified)
#'
#' A simplified, robust wrapper for scoring model fits in MOSAIC.
#' The core is a Negative Binomial (NB) time-series log-likelihood per
#' location and outcome (cases, deaths) with a weighted MoM dispersion
#' estimate and a small \code{k_min} floor.
#'
#' By default, three "shape" terms are included with modest weights:
#' (1) multi-peak timing (Normal on time differences for matched peaks),
#' (2) multi-peak magnitude (log-Normal on ratios with adaptive sigma), and
#' (3) cumulative progression (NB at a few cumulative fractions).
#'
#' Minimal inline guardrails floor the score on egregious fits (cumulative
#' over/under prediction, per-timestep caps, and negative correlation).
#' Optional max terms and WIS are kept but OFF by default.
#'
#'
#' @param obs_cases,est_cases Matrices \code{n_locations x n_time_steps} of observed
#'   and estimated cases.
#' @param obs_deaths,est_deaths Matrices \code{n_locations x n_time_steps} of observed
#'   and estimated deaths.
#' @param weight_cases,weight_deaths Optional scalar weights for case/death blocks.
#'   Default 1.
#' @param weights_location Optional length-\code{n_locations} non-negative weights.
#' @param weights_time Optional length-\code{n_time_steps} non-negative weights.
#' @param config Optional LASER configuration list containing location_name, date_start, and date_stop.
#' @param nb_k_min Numeric; minimum NB dispersion floor used for the core NB likelihood
#'   and WIS quantiles. Default \code{3}.
#' @param zero_buffer Kept for backward compatibility (not used by the NB core).
#' @param verbose Logical; if \code{TRUE}, prints component summaries per location.
#'
#' @param add_max_terms Logical; legacy max Poisson terms. Default \code{FALSE}.
#' @param add_peak_timing,add_peak_magnitude,add_cumulative_total Logical; default \code{TRUE}.
#' @param add_wis Logical; default \code{FALSE}.
#'
#' @param weight_max_terms Component weight for max terms. Default \code{0.5}.
#' @param weight_peak_timing,weight_peak_magnitude,weight_cumulative_total
#'   Component weights. Defaults \code{0.5, 0.5, 0.3}.
#' @param weight_wis Component weight for optional WIS term.
#'
#' @param sigma_peak_time SD (weeks) for peak timing Normal; default \code{1}.
#' @param sigma_peak_log Base SD on log-scale for peak magnitude; default \code{0.5}. 
#'   Automatically scaled by sqrt(100/max(peak_obs,100)) to allow more variance for smaller peaks.
#' @param penalty_unmatched_peak Log-likelihood penalty for unmatched peaks; default \code{-3}.
#'
#' @param wis_quantiles Quantiles for WIS if enabled.
#' @param cumulative_timepoints Fractions for cumulative progression; default \code{c(0.25,0.5,0.75,1)}.
#'
#' @param enable_guardrails Logical; default \code{TRUE}.
#' @param floor_likelihood Numeric; hard floor returned on violations. Default \code{-999999999}.
#' @param guardrail_verbose Logical; print guardrail reasons.
#' @param cumulative_over_ratio,cumulative_under_ratio Cumulative ratio bounds (default \code{10}, \code{0.1}).
#' @param min_cumulative_for_check Minimum observed total to apply ratio checks; default \code{100}.
#' @param max_timestep_ratio Maximum ratio of estimated to observed per timestep (default \code{100}).
#' @param min_timestep_ratio Minimum ratio of estimated to observed per timestep (default \code{0.01}).
#' @param min_obs_for_ratio Minimum observed value to apply ratio checks (default \code{1}).
#' @param negative_correlation_threshold Correlation floor; default \code{0} (requires positive correlation).
#'
#' @return Scalar total log-likelihood (finite) or \code{floor_likelihood} if floored.
#'   May be \code{NA_real_} if all locations contribute nothing.
#' @export
calc_model_likelihood <- function(obs_cases,
                                  est_cases,
                                  obs_deaths,
                                  est_deaths,
                                  weight_cases     = NULL,
                                  weight_deaths    = NULL,
                                  weights_location = NULL,
                                  weights_time     = NULL,
                                  config           = NULL,
                                  nb_k_min         = 3,
                                  zero_buffer      = TRUE,   # kept for compatibility
                                  verbose          = FALSE,
                                  # ---- toggles (Balanced defaults) ----
                                  add_max_terms         = TRUE,
                                  add_peak_timing       = TRUE,
                                  add_peak_magnitude    = TRUE,
                                  add_cumulative_total  = TRUE,
                                  add_wis               = TRUE,
                                  # ---- component weights ----
                                  weight_max_terms         = 0.5,
                                  weight_peak_timing       = 0.5,
                                  weight_peak_magnitude    = 0.5,
                                  weight_cumulative_total  = 0.3,
                                  weight_wis               = 0.8,
                                  # ---- peak controls ----
                                  sigma_peak_time  = 1,  
                                  sigma_peak_log   = 0.5,
                                  penalty_unmatched_peak = -3,
                                  # ---- WIS (optional) ----
                                  wis_quantiles      = c(0.0275, 0.25, 0.5, 0.75, 0.975),
                                  # ---- cumulative progression ----
                                  cumulative_timepoints = c(0.25, 0.5, 0.75, 1.0),
                                  # ---- inline guardrails ----
                                  enable_guardrails = TRUE,
                                  floor_likelihood = -999999999,
                                  guardrail_verbose = FALSE,
                                  cumulative_over_ratio = 10,
                                  cumulative_under_ratio = 0.1,
                                  min_cumulative_for_check = 100,
                                  negative_correlation_threshold = 0,
                                  max_timestep_ratio = 100,
                                  min_timestep_ratio = 0.01,
                                  min_obs_for_ratio = 1)
{
     # --- basic checks ---
     if (!is.matrix(obs_cases) || !is.matrix(est_cases) ||
         !is.matrix(obs_deaths) || !is.matrix(est_deaths)) {
          stop("all inputs must be matrices.")
     }

     # Validation: Check for negative estimated values
     if (any(est_cases < 0, na.rm = TRUE) || any(est_deaths < 0, na.rm = TRUE)) {
          stop("Estimated values must be non-negative.")
     }

     n_locations  <- nrow(obs_cases)
     n_time_steps <- ncol(obs_cases)

     if (any(dim(est_cases)   != c(n_locations, n_time_steps)) ||
         any(dim(obs_deaths)  != c(n_locations, n_time_steps)) ||
         any(dim(est_deaths)  != c(n_locations, n_time_steps))) {
          stop("All matrices must have the same dimensions (n_locations x n_time_steps).")
     }

     if (is.null(weights_location)) weights_location <- rep(1, n_locations)
     if (is.null(weights_time))     weights_time     <- rep(1, n_time_steps)
     if (is.null(weight_cases))     weight_cases     <- 1
     if (is.null(weight_deaths))    weight_deaths    <- 1

     if (length(weights_location) != n_locations) stop("weights_location must match n_locations.")
     if (length(weights_time)     != n_time_steps) stop("weights_time must match n_time_steps.")
     if (any(weights_location < 0) || any(weights_time < 0)) stop("All weights must be >= 0.")
     if (sum(weights_location) == 0 || sum(weights_time) == 0) stop("weights_location and weights_time must not all be zero.")

     # --- inline guardrails ---
     if (enable_guardrails) {

          # Vectorized per-timestep ratio checks
          # Check cases
          ratio_cases <- est_cases / obs_cases
          valid_cases <- is.finite(ratio_cases) & obs_cases >= min_obs_for_ratio
          bad_cases <- which(valid_cases & (ratio_cases > max_timestep_ratio | ratio_cases < min_timestep_ratio),
                            arr.ind = TRUE)
          if (nrow(bad_cases) > 0) {
               if (guardrail_verbose) {
                    first <- bad_cases[1,]
                    message(sprintf("Guardrail: timestep ratio violation at location %d, time %d (cases ratio=%.2f)",
                                  first[1], first[2], ratio_cases[first[1], first[2]]))
               }
               return(floor_likelihood)
          }

          # Check deaths
          ratio_deaths <- est_deaths / obs_deaths
          valid_deaths <- is.finite(ratio_deaths) & obs_deaths >= min_obs_for_ratio
          bad_deaths <- which(valid_deaths & (ratio_deaths > max_timestep_ratio | ratio_deaths < min_timestep_ratio),
                             arr.ind = TRUE)
          if (nrow(bad_deaths) > 0) {
               if (guardrail_verbose) {
                    first <- bad_deaths[1,]
                    message(sprintf("Guardrail: timestep ratio violation at location %d, time %d (deaths ratio=%.2f)",
                                  first[1], first[2], ratio_deaths[first[1], first[2]]))
               }
               return(floor_likelihood)
          }

          obs_cases_tot  <- rowSums(obs_cases,  na.rm = TRUE)
          est_cases_tot  <- rowSums(est_cases,  na.rm = TRUE)
          obs_deaths_tot <- rowSums(obs_deaths, na.rm = TRUE)
          est_deaths_tot <- rowSums(est_deaths, na.rm = TRUE)

          check_ratio <- function(o, e) {
               ok <- is.finite(o) & is.finite(e) & (o >= min_cumulative_for_check)
               r  <- rep(1, length(o)); r[ok] <- e[ok] / o[ok]  # Removed unnecessary pmax
               any(r >= cumulative_over_ratio | r <= cumulative_under_ratio)
          }
          if (check_ratio(obs_cases_tot, est_cases_tot) ||
              check_ratio(obs_deaths_tot, est_deaths_tot)) {
               if (guardrail_verbose) message("Guardrail: cumulative over/under prediction.")
               return(floor_likelihood)
          }

          # correlation screen
          for (j in seq_len(n_locations)) {
               fin_c <- is.finite(obs_cases[j, ]) & is.finite(est_cases[j, ])
               fin_d <- is.finite(obs_deaths[j, ]) & is.finite(est_deaths[j, ])
               if (sum(fin_c) >= 10) {  # Increased from 4 to 10 for reliable correlation
                    cc <- suppressWarnings(stats::cor(obs_cases[j, fin_c], est_cases[j, fin_c]))
                    if (is.finite(cc) && cc < negative_correlation_threshold) {
                         if (guardrail_verbose) message(sprintf("Guardrail: negative correlation (cases) at location %d: %.3f", j, cc))
                         return(floor_likelihood)
                    }
               }
               if (sum(fin_d) >= 10) {  # Increased from 4 to 10 for reliable correlation
                    cd <- suppressWarnings(stats::cor(obs_deaths[j, fin_d], est_deaths[j, fin_d]))
                    if (is.finite(cd) && cd < negative_correlation_threshold) {
                         if (guardrail_verbose) message(sprintf("Guardrail: negative correlation (deaths) at location %d: %.3f", j, cd))
                         return(floor_likelihood)
                    }
               }
          }
     }

     # --- main loop ---
     ll_locations <- rep(NA_real_, n_locations)

     for (j in seq_len(n_locations)) {

          obs_c <- obs_cases[j, ]; est_c <- est_cases[j, ]
          obs_d <- obs_deaths[j, ]; est_d <- est_deaths[j, ]

          # Require minimum observations for meaningful likelihood
          min_obs_for_likelihood <- 3
          have_cases  <- sum(is.finite(obs_c)) >= min_obs_for_likelihood
          have_deaths <- sum(is.finite(obs_d)) >= min_obs_for_likelihood

          # Weighted NB dispersion (k) with floor; Poisson limit if Inf
          k_c <- if (have_cases)  nb_size_from_obs_weighted(obs_c, weights_time, k_min = nb_k_min) else Inf
          k_d <- if (have_deaths) nb_size_from_obs_weighted(obs_d, weights_time, k_min = nb_k_min) else Inf

          # Core NB time series LL (pass k and k_min explicitly)
          ll_cases  <- if (have_cases) MOSAIC::calc_log_likelihood(
               observed  = obs_c,
               estimated = est_c,
               family    = "negbin",
               weights   = mask_weights(weights_time, obs_c, est_c),
               k         = k_c,
               k_min     = nb_k_min,
               verbose   = FALSE
          ) else 0

          ll_deaths <- if (have_deaths) MOSAIC::calc_log_likelihood(
               observed  = obs_d,
               estimated = est_d,
               family    = "negbin",
               weights   = mask_weights(weights_time, obs_d, est_d),
               k         = k_d,
               k_min     = nb_k_min,
               verbose   = FALSE
          ) else 0

          # Peak-based likelihoods using epidemic_peaks data
          ll_peak_time_c <- ll_peak_time_d <- 0
          ll_peak_mag_c <- ll_peak_mag_d <- 0
          
          if ((add_peak_timing || add_peak_magnitude) && !is.null(config)) {
               # Extract needed info from config
               location_names <- config$location_name
               date_start <- config$date_start
               date_stop <- config$date_stop
               
               if (!is.null(location_names) && !is.null(date_start) && !is.null(date_stop)) {
                    # Get location name for this index (assuming it's an ISO code)
                    iso_code <- if (j <= length(location_names)) location_names[j] else NULL
                    
                    if (!is.null(iso_code)) {
                         if (add_peak_timing) {
                              if (have_cases) {
                                   ll_peak_time_c <- calc_multi_peak_timing_ll(
                                        obs_c, est_c, iso_code, date_start, date_stop,
                                        sigma_peak_time, penalty_unmatched_peak
                                   )
                              }
                              if (have_deaths) {
                                   ll_peak_time_d <- calc_multi_peak_timing_ll(
                                        obs_d, est_d, iso_code, date_start, date_stop,
                                        sigma_peak_time, penalty_unmatched_peak
                                   )
                              }
                         }
                         
                         if (add_peak_magnitude) {
                              if (have_cases) {
                                   ll_peak_mag_c <- calc_multi_peak_magnitude_ll(
                                        obs_c, est_c, iso_code, date_start, date_stop,
                                        sigma_peak_log, penalty_unmatched_peak
                                   )
                              }
                              if (have_deaths) {
                                   ll_peak_mag_d <- calc_multi_peak_magnitude_ll(
                                        obs_d, est_d, iso_code, date_start, date_stop,
                                        sigma_peak_log, penalty_unmatched_peak
                                   )
                              }
                         }
                    }
               }
          }

          # Cumulative progression (using data-driven k)
          ll_cum_tot_c <- ll_cum_tot_d <- 0
          if (add_cumulative_total) {
               if (have_cases)  ll_cum_tot_c <- ll_cumulative_progressive_nb(obs_c, est_c, cumulative_timepoints, k_c, weights_time)
               if (have_deaths) ll_cum_tot_d <- ll_cumulative_progressive_nb(obs_d, est_d, cumulative_timepoints, k_d, weights_time)
          }


          # Legacy max terms
          ll_max_cases <- ll_max_deaths <- 0
          if (add_max_terms) {
               if (have_cases)  ll_max_cases  <- max_ll_poisson(obs_c, est_c)
               if (have_deaths) ll_max_deaths <- max_ll_poisson(obs_d, est_d)
          }

          # WIS (optional)
          ll_wis_cases <- ll_wis_deaths <- 0
          if (add_wis) {
               if (have_cases) {
                    wis_c <- compute_wis_parametric_row(obs_c, est_c, weights_time, wis_quantiles, k_use = k_c)
                    if (is.finite(wis_c)) ll_wis_cases <- - weight_wis * wis_c
               }
               if (have_deaths) {
                    wis_d <- compute_wis_parametric_row(obs_d, est_d, weights_time, wis_quantiles, k_use = k_d)
                    if (is.finite(wis_d)) ll_wis_deaths <- - weight_wis * wis_d
               }
          }


          # Assemble location total
          ll_loc_core <-
               weight_cases  * ll_cases +
               weight_deaths * ll_deaths

          ll_loc_max <-
               weight_max_terms * (weight_cases * ll_max_cases + weight_deaths * ll_max_deaths)

          ll_loc_peaks <-
               weight_peak_timing    * (weight_cases * ll_peak_time_c + weight_deaths * ll_peak_time_d) +
               weight_peak_magnitude * (weight_cases * ll_peak_mag_c  + weight_deaths * ll_peak_mag_d)

          ll_loc_cum <-
               weight_cumulative_total * (weight_cases * ll_cum_tot_c + weight_deaths * ll_cum_tot_d)

          ll_loc_wis <- (weight_cases * ll_wis_cases) + (weight_deaths * ll_wis_deaths)

          ll_loc_total <- ll_loc_core
          if (add_max_terms)                         ll_loc_total <- ll_loc_total + ll_loc_max
          if (add_peak_timing || add_peak_magnitude) ll_loc_total <- ll_loc_total + ll_loc_peaks
          if (add_cumulative_total)                  ll_loc_total <- ll_loc_total + ll_loc_cum
          if (add_wis)                               ll_loc_total <- ll_loc_total + ll_loc_wis

          if (!is.finite(ll_loc_total)) {
               if (guardrail_verbose) message(sprintf("Non-finite LL at location %d; applying per-location penalty.", j))
               ll_locations[j] <- weights_location[j] * (-1e9)  # large negative penalty
               next
          }

          ll_locations[j] <- weights_location[j] * ll_loc_total

          if (verbose) {
               message(sprintf(
                    "Location %d: core=%.2f | max=%.2f | peaks=%.2f | cum=%.2f | wis=%.2f -> weighted=%.2f",
                    j, ll_loc_core,
                    if (add_max_terms) ll_loc_max else 0,
                    if ((add_peak_timing || add_peak_magnitude)) ll_loc_peaks else 0,
                    if (add_cumulative_total) ll_loc_cum else 0,
                    if (add_wis) ll_loc_wis else 0,
                    weights_location[j] * ll_loc_total
               ))
          }
     }

     if (all(is.na(ll_locations))) {
          if (verbose) message("All locations contributed NA — returning NA.")
          return(NA_real_)
     }

     ll_total <- sum(ll_locations, na.rm = TRUE)
     if (!is.finite(ll_total)) ll_total <- floor_likelihood
     if (verbose) message(sprintf("Overall total log-likelihood: %.2f", ll_total))
     ll_total
}

###############################################################################
## Helpers (ALL defined outside the main function)
###############################################################################

# Mask weights on non-finite entries
mask_weights <- function(w, obs_vec, est_vec = NULL) {
     w2 <- w
     bad <- !is.finite(obs_vec) | (!is.null(est_vec) & !is.finite(est_vec))
     if (any(bad)) w2[bad] <- 0
     w2
}



# Peak timing likelihood using epidemic_peaks data
calc_multi_peak_timing_ll <- function(obs_vec, est_vec, iso_code = NULL,
                                     date_start = NULL, date_stop = NULL,
                                     sigma_peak_time = 1,
                                     penalty_unmatched = -3) {
     # Load epidemic_peaks data
     if (!exists("epidemic_peaks")) {
          data("epidemic_peaks", package = "MOSAIC", envir = environment())
          epidemic_peaks <- get("epidemic_peaks", envir = environment())
     }
     
     # If required info missing, return 0
     if (is.null(iso_code) || is.null(date_start) || is.null(date_stop)) return(0)
     
     # Get peaks for this location
     loc_peaks <- epidemic_peaks[epidemic_peaks$iso_code == iso_code, ]
     if (nrow(loc_peaks) == 0) return(0)
     
     # Create date sequence for the time series
     date_seq <- seq(as.Date(date_start), as.Date(date_stop), by = "day")
     if (length(date_seq) != length(obs_vec)) {
          # Try weekly if daily doesn't match
          date_seq <- seq(as.Date(date_start), as.Date(date_stop), by = "week")
          if (length(date_seq) != length(obs_vec)) return(0)  # Can't match dates
     }
     
     # Convert peak dates to indices
     peak_indices <- numeric(nrow(loc_peaks))
     for (i in 1:nrow(loc_peaks)) {
          idx <- which.min(abs(date_seq - as.Date(loc_peaks$peak_date[i])))
          if (length(idx) > 0) peak_indices[i] <- idx[1]
     }
     peak_indices <- peak_indices[peak_indices > 0 & peak_indices <= length(obs_vec)]
     
     if (length(peak_indices) == 0) return(0)
     
     # Find peaks in estimated data near the expected peak times
     ll_total <- 0
     n_matched <- 0
     
     for (peak_idx in peak_indices) {
          # Look for peak in estimated data within window
          window <- max(1, peak_idx - 14):min(length(est_vec), peak_idx + 14)  # +/- 2 weeks
          if (length(window) > 2) {
               est_peak_idx <- window[which.max(est_vec[window])]
               # Calculate timing difference in weeks
               time_diff <- (est_peak_idx - peak_idx) / 7
               # Calculate log-likelihood for this peak timing
               ll_total <- ll_total + stats::dnorm(time_diff, 0, sigma_peak_time, log = TRUE)
               n_matched <- n_matched + 1
          }
     }
     
     # Penalize if not all peaks were matched
     n_unmatched <- length(peak_indices) - n_matched
     ll_total <- ll_total + n_unmatched * penalty_unmatched
     
     return(ll_total)
}

# Peak magnitude likelihood using epidemic_peaks data
calc_multi_peak_magnitude_ll <- function(obs_vec, est_vec, iso_code = NULL,
                                        date_start = NULL, date_stop = NULL,
                                        sigma_peak_log = 0.5,
                                        penalty_unmatched = -3) {
     # Load epidemic_peaks data
     if (!exists("epidemic_peaks")) {
          data("epidemic_peaks", package = "MOSAIC", envir = environment())
          epidemic_peaks <- get("epidemic_peaks", envir = environment())
     }
     
     # If required info missing, return 0
     if (is.null(iso_code) || is.null(date_start) || is.null(date_stop)) return(0)
     
     # Get peaks for this location
     loc_peaks <- epidemic_peaks[epidemic_peaks$iso_code == iso_code, ]
     if (nrow(loc_peaks) == 0) return(0)
     
     # Create date sequence for the time series
     date_seq <- seq(as.Date(date_start), as.Date(date_stop), by = "day")
     if (length(date_seq) != length(obs_vec)) {
          # Try weekly if daily doesn't match
          date_seq <- seq(as.Date(date_start), as.Date(date_stop), by = "week")
          if (length(date_seq) != length(obs_vec)) return(0)  # Can't match dates
     }
     
     # Convert peak dates to indices
     peak_indices <- numeric(nrow(loc_peaks))
     for (i in 1:nrow(loc_peaks)) {
          idx <- which.min(abs(date_seq - as.Date(loc_peaks$peak_date[i])))
          if (length(idx) > 0) peak_indices[i] <- idx[1]
     }
     peak_indices <- peak_indices[peak_indices > 0 & peak_indices <= length(obs_vec)]
     
     if (length(peak_indices) == 0) return(0)
     
     # Calculate magnitude likelihood for peaks
     ll_total <- 0
     n_matched <- 0
     
     for (peak_idx in peak_indices) {
          # Get observed magnitude at expected peak time
          window <- max(1, peak_idx - 14):min(length(obs_vec), peak_idx + 14)  # +/- 2 weeks
          if (length(window) > 2) {
               obs_peak_val <- max(obs_vec[window], na.rm = TRUE)
               est_peak_val <- max(est_vec[window], na.rm = TRUE)
               
               if (is.finite(obs_peak_val) && is.finite(est_peak_val) && 
                   obs_peak_val > 0 && est_peak_val > 0) {
                    # Adaptive sigma that scales with peak size
                    adaptive_sigma <- sigma_peak_log * sqrt(100 / max(obs_peak_val, 100))
                    # Log-normal likelihood on the ratio
                    ll_mag <- stats::dnorm(log(est_peak_val) - log(obs_peak_val), 
                                         0, adaptive_sigma, log = TRUE)
                    ll_total <- ll_total + ll_mag
                    n_matched <- n_matched + 1
               }
          }
     }
     
     # Penalize if not all peaks were matched
     n_unmatched <- length(peak_indices) - n_matched
     ll_total <- ll_total + n_unmatched * penalty_unmatched
     
     return(ll_total)
}

# Robust cumulative NB progression
ll_cumulative_progressive_nb <- function(obs_vec,
                                         est_vec,
                                         timepoints = c(0.25, 0.5, 0.75, 1.0),
                                         k_data = NULL,
                                         weights_time = NULL,
                                         per_tp_ll_floor = -1e9) {
     n <- length(obs_vec)
     vals <- numeric(0L)
     
     # Use weights if provided
     if (is.null(weights_time)) weights_time <- rep(1, n)
     
     # Use data-driven k if provided, otherwise fall back to option/default
     # Scale k up for cumulative (sums have higher variance than individual points)
     cum_k <- if (!is.null(k_data) && is.finite(k_data)) {
          k_data * 2  # Scale up for cumulative sums
     } else {
          getOption("MOSAIC.cumulative_k", 10)
     }
     
     for (tp in timepoints) {
          # Fix: Ensure index is at least 1 and at most n
          end_idx <- min(n, max(1L, round(n * tp)))
          
          # Apply weights to cumulative sums
          idx_range <- 1:end_idx
          o_cum <- sum(obs_vec[idx_range] * weights_time[idx_range], na.rm = TRUE) / sum(weights_time[idx_range], na.rm = TRUE) * end_idx
          e_cum <- sum(est_vec[idx_range] * weights_time[idx_range], na.rm = TRUE) / sum(weights_time[idx_range], na.rm = TRUE) * end_idx
          
          if (!is.finite(o_cum) || !is.finite(e_cum)) next
          if (e_cum <= 0 && o_cum > 0) { vals <- c(vals, per_tp_ll_floor); next }
          e_cum <- if (e_cum <= 0) .Machine$double.eps else e_cum
          ll_tp <- stats::dnbinom(round(o_cum), mu = e_cum, size = cum_k, log = TRUE)
          if (!is.finite(ll_tp)) ll_tp <- per_tp_ll_floor
          vals <- c(vals, ll_tp)
     }
     if (!length(vals)) return(per_tp_ll_floor)
     mean(vals)
}


# Legacy max-term Poisson
max_ll_poisson <- function(obs_vec, est_vec) {
     obs_max <- suppressWarnings(max(obs_vec, na.rm = TRUE))
     est_max <- suppressWarnings(max(est_vec, na.rm = TRUE))
     if (!is.finite(obs_max) || !is.finite(est_max)) return(0)
     MOSAIC::calc_log_likelihood(
          observed  = round(pmax(obs_max, 0)),
          estimated = pmax(est_max, 1e-10),
          family    = "poisson",
          weights   = NULL,
          verbose   = FALSE
     )
}

# WIS helper (uses fixed k from core, or Poisson if Inf)
compute_wis_parametric_row <- function(y, est, w_time, probs, k_use) {
     # Early return for all-NA cases
     if (all(!is.finite(y)) || all(!is.finite(est))) return(NA_real_)
     
     w_use <- w_time
     bad <- !is.finite(y) | !is.finite(est)
     if (any(bad)) w_use[bad] <- 0
     if (sum(w_use) == 0) return(NA_real_)
     
     est_eval <- pmax(est, 1e-12)
     
     # Vectorized quantile functions
     qfun <- if (is.infinite(k_use)) {
          function(p) stats::qpois(p, lambda = est_eval)
     } else {
          function(p) stats::qnbinom(p, mu = est_eval, size = k_use)
     }
     
     probs  <- sort(unique(probs))
     has_med <- any(abs(probs - 0.5) < 1e-8)
     mae_term <- 0
     if (has_med) {
          # Fix: qfun returns a vector, need element-wise operations
          q_med <- qfun(0.5)
          mae_term <- sum(abs(y - q_med) * w_use, na.rm = TRUE) / sum(w_use)
     }
     
     lowers <- probs[probs < 0.5]
     uppers <- probs[probs > 0.5]
     pairs <- lapply(lowers, function(p) c(p, if ((1 - p) %in% uppers) (1 - p) else uppers[which.min(abs(uppers - (1 - p)))]))
     K <- length(pairs)
     sum_IS <- 0
     
     if (K > 0) {
          for (pq in pairs) {
               pL <- pq[1]
               pU <- pq[2]
               # Fix: These return vectors, need element-wise operations
               qL <- qfun(pL)
               qU <- qfun(pU)
               alpha <- 1 - (pU - pL)
               # Vectorized operations
               width <- qU - qL
               under <- pmax(0, qL - y) * (2/alpha)
               over  <- pmax(0, y - qU) * (2/alpha)
               IS    <- width + under + over
               contrib <- sum(IS * w_use, na.rm = TRUE) / sum(w_use)
               sum_IS  <- sum_IS + (alpha/2) * contrib
          }
     }
     denom <- (K + 0.5)
     (mae_term + sum_IS) / denom
}

# Weighted method-of-moments NB dispersion (k) with floor
#' @keywords internal
nb_size_from_obs_weighted <- function(x, w, k_min = 3, k_max = 1e5) {
     ok <- is.finite(x) & is.finite(w) & (w > 0)
     if (!any(ok)) return(Inf)
     x <- x[ok]; w <- w[ok]
     sw <- sum(w)
     m  <- sum(w * x) / sw
     v  <- sum(w * (x - m)^2) / sw
     if (!is.finite(m) || !is.finite(v) || m <= 0 || v <= m) return(Inf)
     k  <- (m * m) / (v - m)
     k  <- max(min(k, k_max), k_min)
     k
}

