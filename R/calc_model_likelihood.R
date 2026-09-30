###############################################################################
## calc_model_likelihood.R  (Core NB + optional shape terms; no guardrails)
###############################################################################

#' Compute the total model likelihood
#'
#' Scores model fits against observed data using a Negative Binomial (NB)
#' time-series log-likelihood per location and outcome (cases, deaths) with
#' a per-location NB dispersion estimated by \code{\link{est_nb_dispersion}}.
#'
#' Optional shape terms are enabled by setting their weight > 0: peak timing
#' (Normal), peak magnitude (log-Normal with adaptive sigma), cumulative
#' progression (NB at cumulative fractions), and Weighted Interval Score (WIS).
#' All weights default to 0 (OFF).
#'
#' Each shape term helper returns a per-evaluation value, which is scaled up to
#' the size of the NB core by \code{N_obs / N_eval}, where \code{N_obs} is the
#' number of time steps with a finite observation in either channel and
#' \code{N_eval} is the number of evaluations of that term: peaks are scaled by
#' \code{N_obs / N_peaks}, WIS by \code{N_obs / length(wis_quantiles)} and the
#' cumulative term by \code{N_obs / length(cumulative_timepoints)}. Because the
#' WIS and cumulative helpers already average over their quantiles and
#' timepoints, a given weight on those two terms carries less influence than the
#' same weight on the peak terms (with the defaults, 1/5 and 1/4 of \code{N_obs}
#' times the per-cell value), and changing the number of quantiles or timepoints
#' changes their influence.
#'
#' Non-finite per-location LL values are replaced with \code{-Inf} (zero
#' importance weight). The NB likelihood naturally produces very negative
#' scores for bad fits without needing artificial guardrails.
#'
#' @param obs_cases,est_cases Matrices \code{n_locations x n_time_steps}.
#' @param obs_deaths,est_deaths Matrices \code{n_locations x n_time_steps}.
#' @param weight_cases,weight_deaths Scalar weights for case/death blocks. Default 1.
#' @param weights_location Length-\code{n_locations} non-negative weights.
#' @param weights_time Length-\code{n_time_steps} non-negative weights.
#' @param weights_obs_cases,weights_obs_deaths Optional per-observation
#'   confidence-weight matrices (\code{n_locations x n_time_steps}, values in
#'   \code{[0,1]}; \code{NA} where the corresponding cell is \code{NA}). When
#'   supplied, the per-cell weight multiplies \code{weights_time} for the NB
#'   cases/deaths term respectively, and the resulting per-location weight vector
#'   is renormalized to preserve the current masked-\code{weights_time} mass so
#'   only the trust SHAPE matters (cross-location balance stays with
#'   \code{weights_location}). Default \code{NULL} (no per-cell weighting; the
#'   exact unweighted code path is used, byte-identical to prior behavior). A row
#'   that is all-1 on finite-obs cells is also routed through the exact unweighted
#'   path. Only the NB cases/deaths terms are weighted; shape terms are not (v1).
#' @param config Optional simulation config list (location_name, date_start, date_stop).
#' @param nb_k_cases NB dispersion for the cases channel: a scalar applied to
#'   every location, or a vector with one entry per location. \code{Inf} selects
#'   the Poisson limit. When \code{NULL} (default) the dispersion is estimated
#'   from \code{obs_cases} via \code{\link{est_nb_dispersion}}; in
#'   \code{run_MOSAIC()} it is precomputed once and supplied here.
#' @param nb_k_deaths NB dispersion for the deaths channel; see
#'   \code{nb_k_cases}.
#' @param eps_rel_cases,eps_rel_deaths Positive scalars. Within each location the
#'   predicted mean is floored at \code{max(1e-4, eps_rel * mean(obs))} before the
#'   NB density is evaluated, separately per channel. The floor is not cosmetic:
#'   production scores a SINGLE stochastic realisation, so a low-count series is
#'   full of cells where the realisation is 0 against a positive observation, and
#'   the size of the floor is what the likelihood pays for such a cell. Too small a
#'   floor makes zeros ruinous and the optimum moves to a draw that over-predicts
#'   the level (a Jensen gap: \code{E_seed[LL(est)]} peaks well above
#'   \code{LL(E_seed[est])}). Cases default \code{0.02}; deaths default
#'   \code{0.25}, sized by sweep so the deaths level at the likelihood optimum is
#'   unbiased. Cases are far less exposed: 13.6 percent of scored deaths cells
#'   predict zero against a positive observation, versus 1.7 percent of cases
#'   cells.
#' @param ll_deaths_core Optional numeric vector, one value per location: the
#'   deaths log-likelihood computed with the reported case fatality ratio
#'   integrated out (\code{calc_log_likelihood_deaths_integrated()$ll}). When
#'   supplied it replaces the negative-binomial deaths core, so
#'   \code{eps_rel_deaths} and \code{nb_k_deaths} are not used for the core;
#'   \code{run_MOSAIC()} always supplies it. The level-dependent deaths shape
#'   terms (peak magnitude, cumulative, WIS) are then dropped, with a warning,
#'   because \code{est_deaths} is drawn at the prior CFR; deaths peak timing,
#'   which does not depend on the level, is kept.
#' @param verbose If \code{TRUE}, prints component summaries per location.
#' @param weight_peak_timing,weight_peak_magnitude Weights for peak terms, scaled
#'   by \code{N_obs / N_peaks}. Default \code{0} (OFF); set > 0 to enable.
#' @param weight_cumulative_total Weight for cumulative progression, scaled by
#'   \code{N_obs / length(cumulative_timepoints)}. Default \code{0} (OFF).
#' @param weight_wis Weight for the negated WIS term, scaled by
#'   \code{N_obs / length(wis_quantiles)}. Default \code{0} (OFF).
#' @param sigma_peak_time SD (weeks) for peak timing Normal; default \code{1}.
#' @param sigma_peak_log Base SD on log-scale for peak magnitude; default \code{0.5}.
#' @param wis_quantiles Quantiles for WIS if enabled.
#' @param cumulative_timepoints Fractions for cumulative progression.
#'
# NOTE: percentages in this roxygen block are spelled out. A backslash-escaped
# percent sign here is re-escaped by roxygen2 into a double backslash in the .Rd,
# which the Rd parser reads as a comment, silently dropping the rest of the line
# (v0.95.0 lost the closing brace of an argument entry that way).
#' @return Scalar total log-likelihood (finite), \code{-Inf} if non-finite,
#'   or \code{NA_real_} if no location has data to score (fewer than three
#'   usable observations in both channels, and no integrated deaths score).
#' @export
calc_model_likelihood <- function(obs_cases,
                                  est_cases,
                                  obs_deaths,
                                  est_deaths,
                                  weight_cases     = NULL,
                                  weight_deaths    = NULL,
                                  weights_location = NULL,
                                  weights_time     = NULL,
                                  weights_obs_cases  = NULL,
                                  weights_obs_deaths = NULL,
                                  config           = NULL,
                                  nb_k_cases       = NULL,
                                  nb_k_deaths      = NULL,
                                  eps_rel_cases    = 0.02,
                                  eps_rel_deaths   = 0.25,
                                  ll_deaths_core   = NULL,
                                  verbose          = FALSE,
                                  # ---- shape term weights (0 = OFF; scaling in Details) ----
                                  weight_peak_timing       = 0,
                                  weight_peak_magnitude    = 0,
                                  weight_cumulative_total  = 0,
                                  weight_wis               = 0,
                                  # ---- peak controls ----
                                  sigma_peak_time  = 1,
                                  sigma_peak_log   = 0.5,
                                  # ---- WIS (optional) ----
                                  wis_quantiles      = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                  # ---- cumulative progression ----
                                  cumulative_timepoints = c(0.25, 0.5, 0.75, 1.0))
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

     # Per-channel epsilon floor. NULL means "caller did not set it", which for a
     # scoring knob must resolve to the documented default rather than being
     # dropped -- run_MOSAIC() forwards control$likelihood entries that may be
     # absent from an older control list. Anything else non-usable is an error.
     if (is.null(eps_rel_cases))  eps_rel_cases  <- 0.02
     if (is.null(eps_rel_deaths)) eps_rel_deaths <- 0.25
     eps_rel_cases  <- .check_eps_rel(eps_rel_cases)
     eps_rel_deaths <- .check_eps_rel(eps_rel_deaths)

     if (length(weights_location) != n_locations) stop("weights_location must match n_locations.")
     if (!is.null(ll_deaths_core) &&
         (!is.numeric(ll_deaths_core) || length(ll_deaths_core) != n_locations))
          stop("ll_deaths_core must be a numeric vector with one value per location.")
     if (!is.null(ll_deaths_core) &&
         (weight_peak_magnitude > 0 || weight_cumulative_total > 0 || weight_wis > 0)) {
          .mosaic_warn_once("deaths_shape_terms_integrated", paste0(
               "With the reported CFR integrated out of the deaths likelihood, the deaths ",
               "components of the peak-magnitude, cumulative and WIS shape terms are dropped: ",
               "they would score the engine's deaths at the prior mu_jt and put the prior CFR ",
               "level back into selection. The cases components and deaths peak timing are kept."))
     }

     # NB dispersion. Accepts a scalar (recycled) or one value per location, the
     # same contract as weights_location. k depends only on the OBSERVATIONS, so
     # run_MOSAIC() estimates it once per calibration and passes it in; a
     # standalone call estimates it here so the function stays self-contained.
     .expand_k <- function(k, nm) {
          k <- as.numeric(k)
          if (length(k) == 1L) k <- rep(k, n_locations)
          if (length(k) != n_locations)
               stop(sprintf("%s must be length 1 or n_locations (%d), got %d.",
                            nm, n_locations, length(k)))
          # NA or non-positive k would silently drive the whole log-likelihood to
          # -Inf for every simulation rather than erroring.
          bad <- !((is.finite(k) & k > 0) | is.infinite(k))
          if (any(bad))
               stop(sprintf("%s must be finite and positive, or Inf (Poisson); bad at index %s.",
                            nm, paste(which(bad), collapse = ", ")))
          k
     }
     if (is.null(nb_k_cases) || is.null(nb_k_deaths)) {
          .ds <- if (!is.null(config)) config$date_start else NULL
          if (is.null(.ds)) {
               # Without dates the series cannot be aggregated to its reporting
               # cadence, so dispersion is not estimable. Fall back to the
               # Poisson limit -- the well-defined boundary of the NB family --
               # and say so. run_MOSAIC() always supplies the precomputed
               # dispersion, so this path is for standalone/basic use only.
               warning("calc_model_likelihood(): no nb_k_cases/nb_k_deaths and no config$date_start, ",
                       "so dispersion cannot be estimated; scoring at the Poisson limit. ",
                       "Supply nb_k_cases/nb_k_deaths or a config with date_start.",
                       call. = FALSE)
               if (is.null(nb_k_cases))  nb_k_cases  <- Inf
               if (is.null(nb_k_deaths)) nb_k_deaths <- Inf
          } else {
               if (is.null(nb_k_cases))
                    nb_k_cases <- est_nb_dispersion(obs_cases, weights_obs_cases,
                                                    date_start = .ds)$k
               if (is.null(nb_k_deaths))
                    nb_k_deaths <- est_nb_dispersion(obs_deaths, weights_obs_deaths,
                                                     date_start = .ds)$k
          }
     }
     nb_k_cases  <- .expand_k(nb_k_cases,  "nb_k_cases")
     nb_k_deaths <- .expand_k(nb_k_deaths, "nb_k_deaths")
     if (length(weights_time)     != n_time_steps) stop("weights_time must match n_time_steps.")
     if (any(weights_location < 0) || any(weights_time < 0)) stop("All weights must be >= 0.")
     if (sum(weights_location) == 0 || sum(weights_time) == 0) stop("weights_location and weights_time must not all be zero.")

     # Per-observation confidence-weight matrices (optional). When supplied they
     # must be matrices matching the observation grid exactly. Negative entries
     # are invalid; NA is allowed (treated as a missing cell, masked out below).
     if (!is.null(weights_obs_cases)) {
          if (!is.matrix(weights_obs_cases)) stop("weights_obs_cases must be a matrix.")
          if (any(dim(weights_obs_cases) != c(n_locations, n_time_steps)))
               stop("weights_obs_cases must have the same dimensions as obs_cases (n_locations x n_time_steps).")
          if (any(weights_obs_cases < 0, na.rm = TRUE)) stop("weights_obs_cases must be >= 0.")
     }
     if (!is.null(weights_obs_deaths)) {
          if (!is.matrix(weights_obs_deaths)) stop("weights_obs_deaths must be a matrix.")
          if (any(dim(weights_obs_deaths) != c(n_locations, n_time_steps)))
               stop("weights_obs_deaths must have the same dimensions as obs_deaths (n_locations x n_time_steps).")
          if (any(weights_obs_deaths < 0, na.rm = TRUE)) stop("weights_obs_deaths must be >= 0.")
     }

     # --- precompute peak indices per location (once, not per call) ---
     peak_indices_by_loc <- NULL
     timestep_to_weeks <- 7  # default: daily timesteps, divide by 7 to get weeks
     if ((weight_peak_timing > 0 || weight_peak_magnitude > 0) && !is.null(config)) {
          location_names <- config$location_name
          date_start_cfg <- config$date_start
          date_stop_cfg <- config$date_stop

          if (!is.null(location_names) && !is.null(date_start_cfg) && !is.null(date_stop_cfg)) {
               # Build date sequence once; detect timestep resolution
               date_seq <- seq(as.Date(date_start_cfg), as.Date(date_stop_cfg), by = "day")
               if (length(date_seq) != n_time_steps) {
                    date_seq <- seq(as.Date(date_start_cfg), as.Date(date_stop_cfg), by = "week")
                    if (length(date_seq) != n_time_steps) {
                         date_seq <- NULL
                    } else {
                         timestep_to_weeks <- 1  # weekly data: 1 timestep = 1 week
                    }
               }

               if (!is.null(date_seq)) {
                    # Prefer config-supplied epidemic_peaks (matches the Python
                    # port's config.get("epidemic_peaks") path; ships in
                    # config_default v3.2+); fall back to the lazy-loaded
                    # package dataset when the field is absent so older
                    # configs continue to work.
                    epidemic_peaks <- if (!is.null(config$epidemic_peaks)) {
                         .mosaic_as_peaks_frame(config$epidemic_peaks)
                    } else {
                         MOSAIC::epidemic_peaks
                    }
                    # Only keep peaks whose date falls inside the simulation
                    # window; without this filter which.min() snaps out-of-window
                    # peaks to t=1 or t=n_time_steps, biasing peak-shape terms.
                    date_lo <- date_seq[1L]
                    date_hi <- date_seq[length(date_seq)]
                    peak_indices_by_loc <- vector("list", n_locations)
                    for (j_pk in seq_len(n_locations)) {
                         iso_code <- if (j_pk <= length(location_names)) location_names[j_pk] else NA_character_
                         if (is.na(iso_code)) { peak_indices_by_loc[[j_pk]] <- integer(0); next }
                         loc_peaks <- epidemic_peaks[epidemic_peaks$iso_code == iso_code, ]
                         if (nrow(loc_peaks) == 0) { peak_indices_by_loc[[j_pk]] <- integer(0); next }
                         pd <- as.Date(loc_peaks$peak_date)
                         in_window <- !is.na(pd) & pd >= date_lo & pd <= date_hi
                         if (!any(in_window)) { peak_indices_by_loc[[j_pk]] <- integer(0); next }
                         idx <- vapply(pd[in_window], function(d) {
                              which.min(abs(date_seq - d))
                         }, integer(1))
                         peak_indices_by_loc[[j_pk]] <- idx[idx > 0L & idx <= n_time_steps]
                    }
               }
          }
     }

     # --- main loop ---
     ll_locations <- rep(NA_real_, n_locations)

     for (j in seq_len(n_locations)) {

          obs_c <- obs_cases[j, ]; est_c <- est_cases[j, ]
          obs_d <- obs_deaths[j, ]; est_d <- est_deaths[j, ]

          # Per-cell confidence-weight rows for this location (NULL if absent).
          wobs_c_row <- if (!is.null(weights_obs_cases))  weights_obs_cases[j, ]  else NULL
          wobs_d_row <- if (!is.null(weights_obs_deaths)) weights_obs_deaths[j, ] else NULL

          # A row is "trivial" (all-1 on finite-obs cells) -> exact unweighted path.
          triv_c <- .weights_obs_row_trivial(wobs_c_row, obs_c)
          triv_d <- .weights_obs_row_trivial(wobs_d_row, obs_d)

          # Require minimum observations for meaningful likelihood.
          # NULL/trivial path: count of finite observations that carry positive
          # time weight >= 3 (with the default all-ones weights_time this is the
          # raw finite count). Counting cells that weights_time zeroes would pass
          # a location whose scoring weights are all zero on to the NB density,
          # which stops. Weighted path: effective-sample-size gate
          # sum(weights_obs[finite & weights_time > 0]) >= 3 (red-team M-3).
          min_obs_for_likelihood <- 3
          wt_pos <- is.finite(weights_time) & (weights_time > 0)
          if (triv_c) {
               have_cases <- sum(is.finite(obs_c) & wt_pos) >= min_obs_for_likelihood
          } else {
               sel_c <- is.finite(obs_c) & is.finite(weights_time) & (weights_time > 0)
               have_cases <- sum(wobs_c_row[sel_c], na.rm = TRUE) >= min_obs_for_likelihood
          }
          if (triv_d) {
               have_deaths <- sum(is.finite(obs_d) & wt_pos) >= min_obs_for_likelihood
          } else {
               sel_d <- is.finite(obs_d) & is.finite(weights_time) & (weights_time > 0)
               have_deaths <- sum(wobs_d_row[sel_d], na.rm = TRUE) >= min_obs_for_likelihood
          }

          # Effective scoring weights per channel. Trivial -> exact unweighted
          # (masked weights_time); weighted -> mass-preserving renorm (B-1).
          w_eff_c <- if (have_cases) {
               if (triv_c) .mask_weights(weights_time, obs_c, est_c)
               else        .weights_obs_effective(weights_time, wobs_c_row, obs_c, est_c)
          } else NULL
          w_eff_d <- if (have_deaths) {
               if (triv_d) .mask_weights(weights_time, obs_d, est_d)
               else        .weights_obs_effective(weights_time, wobs_d_row, obs_d, est_d)
          } else NULL

          # NB dispersion for this location. k is a property of the OBSERVATION
          # process, not of the model-observation mismatch, so it is identical
          # for every simulation and is estimated once (see est_nb_dispersion()).
          # Inf selects the Poisson limit, which is the intended result for
          # all-zero and other uninformative series.
          k_c <- if (have_cases)  nb_k_cases[j]  else Inf
          k_d <- if (have_deaths) nb_k_deaths[j] else Inf

          # Core NB time series LL (k supplied explicitly; already bounded)
          ll_cases  <- if (have_cases) MOSAIC::calc_log_likelihood(
               observed  = obs_c,
               estimated = est_c,
               family    = "negbin",
               weights   = w_eff_c,
               k         = k_c,
               eps_rel   = eps_rel_cases,
               verbose   = FALSE
          ) else 0

          ll_deaths <- if (!is.null(ll_deaths_core)) ll_deaths_core[j] else if (have_deaths) MOSAIC::calc_log_likelihood(
               observed  = obs_d,
               estimated = est_d,
               family    = "negbin",
               weights   = w_eff_d,
               k         = k_d,
               eps_rel   = eps_rel_deaths,
               verbose   = FALSE
          ) else 0

          # Peak-based likelihoods using precomputed peak indices
          ll_peak_time_c <- ll_peak_time_d <- 0
          ll_peak_mag_c <- ll_peak_mag_d <- 0

          if ((weight_peak_timing > 0 || weight_peak_magnitude > 0) && !is.null(peak_indices_by_loc)) {
               loc_peak_idx <- peak_indices_by_loc[[j]]
               if (length(loc_peak_idx) > 0) {
                    if (weight_peak_timing > 0) {
                         if (have_cases) {
                              ll_peak_time_c <- .calc_peak_timing_from_indices(
                                   est_c, loc_peak_idx, sigma_peak_time,
                                   timestep_to_weeks = timestep_to_weeks
                              )
                         }
                         if (have_deaths) {
                              ll_peak_time_d <- .calc_peak_timing_from_indices(
                                   est_d, loc_peak_idx, sigma_peak_time,
                                   timestep_to_weeks = timestep_to_weeks
                              )
                         }
                    }
                    if (weight_peak_magnitude > 0) {
                         if (have_cases) {
                              ll_peak_mag_c <- .calc_peak_magnitude_from_indices(
                                   obs_c, est_c, loc_peak_idx, sigma_peak_log
                              )
                         }
                         if (have_deaths) {
                              ll_peak_mag_d <- .calc_peak_magnitude_from_indices(
                                   obs_d, est_d, loc_peak_idx, sigma_peak_log
                              )
                         }
                    }
               }
          }

          # Cumulative progression (using data-driven k)
          ll_cum_tot_c <- ll_cum_tot_d <- 0
          if (weight_cumulative_total > 0) {
               if (have_cases)  ll_cum_tot_c <- .ll_cumulative_progressive_nb(obs_c, est_c, cumulative_timepoints, k_c,
                                                                              weights_time, eps_rel = eps_rel_cases,
                                                                              weights_obs = wobs_c_row)
               if (have_deaths) ll_cum_tot_d <- .ll_cumulative_progressive_nb(obs_d, est_d, cumulative_timepoints, k_d,
                                                                              weights_time, eps_rel = eps_rel_deaths,
                                                                              weights_obs = wobs_d_row)
          }


          # WIS (optional) -- raw negated WIS; weight_wis applied at assembly (like other components)
          ll_wis_cases <- ll_wis_deaths <- 0
          if (weight_wis > 0) {
               if (have_cases) {
                    wis_c <- .compute_wis_parametric_row(obs_c, est_c, weights_time, wis_quantiles, k_use = k_c)
                    if (is.finite(wis_c)) ll_wis_cases <- -wis_c
               }
               if (have_deaths) {
                    wis_d <- .compute_wis_parametric_row(obs_d, est_d, weights_time, wis_quantiles, k_use = k_d)
                    if (is.finite(wis_d)) ll_wis_deaths <- -wis_d
               }
          }


          # Assembly formula (per location j):
          #
          # Shape term scaling: N_obs / N_component_observations
          #
          # The scale factor is N_obs divided by the number of evaluations of the
          # component:
          #
          #   NB core:     N_obs observations -> no scaling (reference)
          #   Peaks:       SUM over N_peaks peaks -> scale by N_obs / N_peaks
          #   WIS:         per-cell WIS averaged over cells (it already includes
          #                the (K + 0.5) quantile-pair average) -> N_obs / N_quantiles
          #   Cumulative:  per-cell LL averaged over timepoints -> N_obs / N_eval_points
          #
          # The peak helpers return sums, so N_obs / N_peaks puts them on the
          # per-cell scale of the core. The WIS and cumulative helpers already
          # return per-cell averages, so their extra 1/N_quantiles and
          # 1/N_eval_points make a weight on them weaker than the same weight on
          # the peaks (v0.22.21 convention, documented in the roxygen and pinned
          # by tests).
          #
          #   ll_loc = wc * NB_cases + wd * NB_deaths
          #     + (N_obs/N_peaks)      * w_pt  * (wc * pt_c  + wd * pt_d)
          #     + (N_obs/N_peaks)      * w_pm  * (wc * pm_c  + wd * pm_d)
          #     + (N_obs/N_eval_pts)   * w_cum * (wc * cum_c + wd * cum_d)
          #     + (N_obs/N_quantiles)  * w_wis * (wc * wis_c + wd * wis_d)
          #
          # NOTE: weight_cases/weight_deaths apply multiplicatively to EVERY component.

          # With the reported CFR integrated out (ll_deaths_core), the deaths the
          # engine drew are at the PRIOR mu_jt, so any level-dependent deaths
          # shape term (peak magnitude, cumulative, WIS) would put the prior CFR
          # level back into selection. Those deaths components are dropped;
          # deaths peak timing is level-free and stays.
          if (!is.null(ll_deaths_core)) {
               ll_peak_mag_d <- 0; ll_cum_tot_d <- 0; ll_wis_deaths <- 0
          }

          # N_obs: count of timesteps with at least one finite observation
          N_obs <- sum(is.finite(obs_c) | is.finite(obs_d))

          # Component observation counts
          n_peaks_j     <- if (!is.null(peak_indices_by_loc)) length(peak_indices_by_loc[[j]]) else 0L
          n_wis_quant   <- length(wis_quantiles)
          n_cum_points  <- length(cumulative_timepoints)

          # Scale factors: N_obs / N_component_obs
          peak_scale <- if (n_peaks_j > 0) N_obs / n_peaks_j else 0
          wis_scale  <- if (n_wis_quant > 0) N_obs / n_wis_quant else 0
          cum_scale  <- if (n_cum_points > 0) N_obs / n_cum_points else 0

          ll_loc_core <-
               weight_cases  * ll_cases +
               weight_deaths * ll_deaths

          ll_loc_peaks <-
               peak_scale * weight_peak_timing    * (weight_cases * ll_peak_time_c + weight_deaths * ll_peak_time_d) +
               peak_scale * weight_peak_magnitude * (weight_cases * ll_peak_mag_c  + weight_deaths * ll_peak_mag_d)

          ll_loc_cum <-
               cum_scale * weight_cumulative_total * (weight_cases * ll_cum_tot_c + weight_deaths * ll_cum_tot_d)

          ll_loc_wis <-
               wis_scale * weight_wis * (weight_cases * ll_wis_cases + weight_deaths * ll_wis_deaths)

          ll_loc_total <- ll_loc_core + ll_loc_peaks + ll_loc_cum + ll_loc_wis

          # A location with no scorable data in either channel contributes
          # nothing: leave it NA so an all-missing input returns NA rather than a
          # score of 0 that is identical for every simulation. The integrated
          # deaths score is exactly 0 when it has no scored weeks.
          if (!have_cases && !have_deaths &&
              (is.null(ll_deaths_core) || isTRUE(ll_deaths_core[j] == 0))) {
               next
          }

          # Non-finite safety net: -Inf gets zero importance weight
          if (!is.finite(ll_loc_total)) {
               ll_locations[j] <- -Inf
               next
          }

          ll_locations[j] <- weights_location[j] * ll_loc_total

          if (verbose) {
               message(sprintf(
                    "Location %d: core=%.2f | peaks=%.2f | cum=%.2f | wis=%.2f -> weighted=%.2f",
                    j, ll_loc_core, ll_loc_peaks, ll_loc_cum, ll_loc_wis,
                    weights_location[j] * ll_loc_total
               ))
          }
     }

     if (all(is.na(ll_locations))) {
          if (verbose) message("All locations contributed NA \u2014 returning NA.")
          return(NA_real_)
     }

     ll_total <- sum(ll_locations, na.rm = TRUE)
     if (!is.finite(ll_total)) ll_total <- -Inf
     if (verbose) message(sprintf("Overall total log-likelihood: %.2f", ll_total))
     ll_total
}

###############################################################################
## Helpers (ALL defined outside the main function)
###############################################################################

# Mask weights on non-finite entries
.mask_weights <- function(w, obs_vec, est_vec = NULL) {
     w2 <- w
     bad <- !is.finite(obs_vec) | (!is.null(est_vec) & !is.finite(est_vec))
     if (any(bad)) w2[bad] <- 0
     w2
}

# Decide whether a per-cell confidence-weight row is "trivial" -- i.e. equal to
# 1 on every cell with a finite observation. A trivial row must route through the
# exact unweighted code path so that an all-ones matrix is byte-identical to NULL
# (red-team M-1). Non-finite weight cells are ignored here because they only
# matter where the observation is finite; the .mask_weights() call already zeros
# non-finite obs. A non-finite weight on a finite-obs cell is NOT trivial.
.weights_obs_row_trivial <- function(wobs_row, obs_vec) {
     if (is.null(wobs_row)) return(TRUE)
     fin <- is.finite(obs_vec)
     if (!any(fin)) return(TRUE)
     wf <- wobs_row[fin]
     all(is.finite(wf) & wf == 1)
}

# Build the mass-preserving effective weight vector for one location/channel
# (red-team B-1). Inputs are the (length-T) time weights and the per-cell
# confidence-weight row; obs/est mask out non-finite cells. The raw effective
# weight is weights_time * wobs (masked); it is then rescaled so its sum equals
# the masked-weights_time sum (target_j). This preserves each location's total
# LL mass exactly as in the unweighted path -- only the SHAPE (which cells are
# trusted) changes -- so weights_location stays the sole cross-location lever.
# Returns a length-T vector (zeros on masked cells).
.weights_obs_effective <- function(weights_time, wobs_row, obs_vec, est_vec) {
     masked_wt <- .mask_weights(weights_time, obs_vec, est_vec)
     target_j  <- sum(masked_wt)
     wobs_use  <- wobs_row
     wobs_use[!is.finite(wobs_use)] <- 0
     w_raw <- masked_wt * wobs_use
     s <- sum(w_raw)
     if (s <= 0 || !is.finite(target_j) || target_j <= 0) {
          # Fully-zeroed row (or degenerate target): contribute nothing.
          return(rep(0, length(weights_time)))
     }
     w_raw / s * target_j
}



# --- Fast peak helpers using precomputed indices (no date parsing) ---

# Peak timing likelihood from precomputed indices. A peak whose window holds a
# non-finite estimate is skipped: the calibration worker masks (NA) the cases
# head before score_start_cases, and which.max() over a fully masked window is
# integer(0) (a zero-length LL), while a partly masked one pulls the estimated
# peak to the unmasked side.
.calc_peak_timing_from_indices <- function(est_vec, peak_indices, sigma_peak_time = 1,
                                           timestep_to_weeks = 7) {
     ll_total <- 0
     n_ts <- length(est_vec)
     for (peak_idx in peak_indices) {
          window <- max(1L, peak_idx - 14L):min(n_ts, peak_idx + 14L)
          if (length(window) > 2L && all(is.finite(est_vec[window]))) {
               est_peak_idx <- window[which.max(est_vec[window])]
               time_diff <- (est_peak_idx - peak_idx) / timestep_to_weeks
               ll_total <- ll_total + stats::dnorm(time_diff, 0, sigma_peak_time, log = TRUE)
          }
     }
     ll_total
}

# Peak magnitude likelihood from precomputed indices
.calc_peak_magnitude_from_indices <- function(obs_vec, est_vec, peak_indices,
                                              sigma_peak_log = 0.5) {
     ll_total <- 0
     n_ts <- length(obs_vec)
     for (peak_idx in peak_indices) {
          window <- max(1L, peak_idx - 14L):min(n_ts, peak_idx + 14L)
          if (length(window) > 2L) {
               obs_peak_val <- max(obs_vec[window], na.rm = TRUE)
               est_peak_val <- max(est_vec[window], na.rm = TRUE)
               if (is.finite(obs_peak_val) && is.finite(est_peak_val) &&
                   obs_peak_val > 0 && est_peak_val > 0) {
                    adaptive_sigma <- sigma_peak_log * sqrt(100 / max(obs_peak_val, 100))
                    ll_total <- ll_total + stats::dnorm(
                         log(est_peak_val) - log(obs_peak_val), 0, adaptive_sigma, log = TRUE
                    )
               }
          }
     }
     ll_total
}

# Cumulative NB progression.
#
# At each fraction tp of the series, the observed and predicted counts are summed
# over the SAME scored cells of the prefix 1..round(n * tp): cells where the
# observation and prediction are finite, weights_time is positive and, when a
# per-cell confidence-weight row is supplied, its weight is positive (so the
# deaths-prefix and other zero-confidence cells are excluded too). Summing the
# prediction over cells whose observation is missing (or zero-weighted) would
# penalise a trajectory for predicting cases in a data gap. Each cell's prediction
# is floored at eps_j = max(1e-4, eps_rel * mean(obs over those cells)) before
# summing, the same form as the core's floor (whose mean runs over every non-NA
# observation, so the two coincide only when no cell is zero-weighted); a zero
# prediction therefore costs a bounded density rather than a count-proportional
# constant.
# The sum is scored as NB with dispersion k * n_used (the sum of n_used
# independent NB(mu, k) cells with equal means has size k * n_used), or Poisson
# when k is Inf; a NULL/NA k falls back to getOption("MOSAIC.cumulative_k", 10).
# Each timepoint's LL is divided by n_used (per-cell scale) and the timepoints are
# averaged.
.ll_cumulative_progressive_nb <- function(obs_vec,
                                         est_vec,
                                         timepoints = c(0.25, 0.5, 0.75, 1.0),
                                         k_data = NULL,
                                         weights_time = NULL,
                                         eps_rel = 0.02,
                                         weights_obs = NULL) {
     n <- length(obs_vec)
     if (is.null(weights_time)) weights_time <- rep(1, n)

     k_fallback <- getOption("MOSAIC.cumulative_k", 10)
     k_missing  <- is.null(k_data) || is.na(k_data)
     poisson    <- !k_missing && is.infinite(k_data)

     used <- is.finite(obs_vec) & is.finite(est_vec) &
          is.finite(weights_time) & (weights_time > 0)
     if (!is.null(weights_obs)) used <- used & !is.na(weights_obs) & (weights_obs > 0)
     if (!any(used)) return(0)

     eps_j <- max(1e-4, eps_rel * mean(obs_vec[used]))
     if (!is.finite(eps_j) || eps_j <= 0) eps_j <- 1e-4
     obs_r <- round(obs_vec)
     est_f <- pmax(est_vec, eps_j)

     vals <- numeric(length(timepoints))
     n_vals <- 0L

     for (tp in timepoints) {
          end_idx <- min(n, max(1L, round(n * tp)))
          sel <- used[seq_len(end_idx)]
          n_used <- sum(sel)
          if (n_used == 0L) next

          idx <- which(sel)
          o_cum <- sum(obs_r[idx])
          e_cum <- sum(est_f[idx])

          ll_tp <- if (poisson) {
               stats::dpois(o_cum, lambda = e_cum, log = TRUE)
          } else {
               cum_k <- if (k_missing) k_fallback else k_data * n_used
               stats::dnbinom(o_cum, mu = e_cum, size = cum_k, log = TRUE)
          }

          n_vals <- n_vals + 1L
          vals[n_vals] <- ll_tp / n_used
     }
     if (n_vals == 0L) return(0)
     mean(vals[seq_len(n_vals)])
}


# WIS helper (uses fixed k from core, or Poisson if Inf)
.compute_wis_parametric_row <- function(y, est, w_time, probs, k_use) {
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
          mae_term <- 0.5 * sum(abs(y - q_med) * w_use, na.rm = TRUE) / sum(w_use)
     }
     
     lowers <- probs[probs < 0.5]
     uppers <- probs[probs > 0.5]
     pairs <- lapply(lowers, function(p) {
          complement <- 1 - p
          match_idx <- which(abs(uppers - complement) < 1e-8)
          c(p, if (length(match_idx) > 0) uppers[match_idx[1]] else uppers[which.min(abs(uppers - complement))])
     })
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




# Locations with no finite observation in either channel inside the scored
# window (cases from min(idx_cases, idx_deaths), the worker's shared slice start;
# deaths from idx_deaths, since the worker zero-weights the deaths prefix).
# calc_model_likelihood() leaves such a location NA for every draw, and the
# integrated deaths score has no week to score there, so a calibration in which
# EVERY location is unscorable has nothing to weight. run_MOSAIC() calls this
# before launching workers and stops in that case. This is a sufficient
# condition for an NA location, not the full min-obs gate: a location with 1-2
# usable observations is also NA, but depends on weights resolved later.
.mosaic_unscorable_locations <- function(obs_cases, obs_deaths,
                                         idx_cases = 1L, idx_deaths = 1L) {
     as_mat <- function(x) if (is.matrix(x)) x else matrix(x, nrow = 1L)
     oc <- as_mat(obs_cases); od <- as_mat(obs_deaths)
     n_t <- ncol(oc)
     s_c <- min(idx_cases, idx_deaths)
     fin_c <- is.finite(oc[, s_c:n_t, drop = FALSE])
     fin_d <- is.finite(od[, min(idx_deaths, ncol(od)):ncol(od), drop = FALSE])
     rowSums(fin_c) == 0L & rowSums(fin_d) == 0L
}

#' Coerce a config's epidemic_peaks to a data frame
#'
#' A config read back from JSON does not always carry epidemic_peaks as a data
#' frame: a 0-row frame is written as \code{[]} and returns as \code{list()},
#' and \code{simplifyVector = FALSE} returns one list per peak. An empty value
#' means the config has no peaks, so it becomes a 0-row frame (never the
#' package dataset, which would re-introduce peaks the config excluded).
#' @param x \code{config$epidemic_peaks}.
#' @return A data frame with at least \code{iso_code} and \code{peak_date}.
#' @noRd
.mosaic_as_peaks_frame <- function(x) {
     empty <- data.frame(iso_code = character(0), peak_date = character(0),
                         stringsAsFactors = FALSE)
     if (is.data.frame(x)) return(if (nrow(x)) x else empty)
     if (length(x) == 0L) return(empty)
     if (is.list(x) && is.null(names(x)) && all(vapply(x, is.list, logical(1)))) {
          # One record per peak (simplifyVector = FALSE).
          x <- do.call(rbind, lapply(x, function(r)
               as.data.frame(lapply(r, function(v) if (is.null(v)) NA else unlist(v)),
                             stringsAsFactors = FALSE)))
     } else if (is.list(x)) {
          x <- as.data.frame(lapply(x, unlist), stringsAsFactors = FALSE)
     }
     if (!is.data.frame(x) || !all(c("iso_code", "peak_date") %in% names(x)))
          stop("config$epidemic_peaks must be a data frame (or its JSON form) with ",
               "iso_code and peak_date columns.", call. = FALSE)
     x
}
