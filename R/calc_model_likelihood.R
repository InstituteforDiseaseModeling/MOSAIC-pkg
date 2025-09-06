###############################################################################
## calc_model_likelihood.R  (Balanced design; NB uses weighted k with k_min)
###############################################################################

#' Compute the total model likelihood (Balanced design)
#'
#' A simplified, robust wrapper for scoring model fits in MOSAIC.
#' The core is a Negative Binomial (NB) time-series log-likelihood per
#' location and outcome (cases, deaths) with a weighted MoM dispersion
#' estimate and a small \code{k_min} floor (see
#' \code{\link{calc_log_likelihood_negbin}}).
#'
#' By default, three light "shape" terms are included with modest weights:
#' (1) peak timing (Normal on peak index difference),
#' (2) peak magnitude (log-Normal on ratios), and
#' (3) cumulative progression (NB at a few cumulative fractions).
#'
#' Minimal inline guardrails floor the score on egregious fits (cumulative
#' over/under prediction, per-timestep caps, negative correlation, and
#' zero-prediction mismatches). Duration, growth, maxima, and WIS are kept
#' but OFF by default to reduce complexity.
#'
#' Additionally, an optional **activity** component can be included: a small
#' Bernoulli log-likelihood on whether each week is nonzero, using the model-
#' implied nonzero probability under Poisson/NB. This sharply penalizes flat,
#' near-zero trajectories when observations are frequently nonzero.
#'
#' @param obs_cases,est_cases Matrices \code{n_locations x n_time_steps} of observed
#'   and estimated cases.
#' @param obs_deaths,est_deaths Matrices \code{n_locations x n_time_steps} of observed
#'   and estimated deaths.
#' @param weight_cases,weight_deaths Optional scalar weights for case/death blocks.
#'   Default 1.
#' @param weights_location Optional length-\code{n_locations} non-negative weights.
#' @param weights_time Optional length-\code{n_time_steps} non-negative weights.
#' @param nb_k_min Numeric; minimum NB dispersion floor used for the core NB likelihood
#'   and WIS quantiles. Default \code{3}.
#' @param zero_buffer Kept for backward compatibility (not used by the NB core).
#' @param verbose Logical; if \code{TRUE}, prints component summaries per location.
#'
#' @param add_max_terms Logical; legacy max Poisson terms. Default \code{FALSE}.
#' @param add_peak_timing,add_peak_magnitude,add_cumulative_total Logical; default \code{TRUE}.
#' @param add_growth_rate,add_duration,add_wis Logical; default \code{FALSE}.
#' @param add_activity Logical; include Bernoulli nonzero "activity" term. Default \code{TRUE}.
#'
#' @param weight_peak_timing,weight_peak_magnitude,weight_cumulative_total
#'   Component weights. Defaults \code{0.5, 0.5, 0.3}.
#' @param weight_growth_rate,weight_duration,weight_wis Component weights for optional terms.
#' @param weight_activity Component weight for activity term. Default \code{0.6}.
#'
#' @param peak_method \code{"smooth"} (3-pt moving average) or \code{"simple"}.
#' @param sigma_peak_time SD (weeks) for peak timing Normal; default \code{2}.
#' @param sigma_peak_log SD on log-scale for peak magnitude; default \code{0.5}.
#'
#' @param growth_method,growth_aggregation,smooth_window,sigma_r Controls if growth is used.
#' @param duration_method,duration_aggregation,sigma_duration_log Controls if duration is used.
#'
#' @param wis_quantiles Quantiles for WIS if enabled.
#' @param cumulative_timepoints Fractions for cumulative progression; default \code{c(0.25,0.5,0.75,1)}.
#'
#' @param enable_guardrails Logical; default \code{TRUE}.
#' @param floor_likelihood Numeric; hard floor returned on violations. Default \code{-999999999}.
#' @param guardrail_verbose Logical; print guardrail reasons.
#' @param cumulative_over_ratio,cumulative_under_ratio Cumulative ratio bounds (default \code{25}, \code{0.04}).
#' @param min_cumulative_for_check Minimum observed total to apply ratio checks; default \code{100}.
#' @param max_cases_per_timestep,max_deaths_per_timestep Hard per-timestep caps; defaults \code{1e6}, \code{1e5}.
#' @param negative_correlation_threshold Correlation floor; default \code{-0.1}.
#'
#' @param activity_obs_threshold Integer; counts \code{>=} this are "active" (default 1).
#' @param activity_eps Small clamp for probabilities in activity term (default \code{1e-12}).
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
                                  nb_k_min         = 3,
                                  zero_buffer      = TRUE,   # kept for compatibility
                                  verbose          = FALSE,
                                  # ---- toggles (Balanced defaults) ----
                                  add_max_terms         = FALSE,
                                  add_peak_timing       = TRUE,
                                  add_peak_magnitude    = TRUE,
                                  add_cumulative_total  = TRUE,
                                  add_growth_rate       = FALSE,
                                  add_duration          = FALSE,
                                  add_wis               = FALSE,
                                  add_activity          = TRUE,
                                  # ---- component weights ----
                                  weight_peak_timing       = 0.5,
                                  weight_peak_magnitude    = 0.5,
                                  weight_cumulative_total  = 0.3,
                                  weight_growth_rate       = 0.2,
                                  weight_duration          = 0.2,
                                  weight_wis               = 0.8,
                                  weight_activity          = 0.6,
                                  # ---- peak controls ----
                                  peak_method      = c("smooth", "simple"),
                                  sigma_peak_time  = 2,
                                  sigma_peak_log   = 0.5,
                                  # ---- growth controls (optional) ----
                                  growth_method      = c("derivative", "threshold"),
                                  growth_aggregation = c("mean","median","max"),
                                  smooth_window      = 7,
                                  sigma_r            = 0.2,
                                  # ---- duration controls (optional) ----
                                  duration_method      = c("epidemic_periods","main_wave","above_baseline"),
                                  duration_aggregation = c("mean","total","max"),
                                  sigma_duration_log   = 0.3,
                                  # ---- WIS (optional) ----
                                  wis_quantiles      = c(0.0275, 0.25, 0.5, 0.75, 0.975),
                                  # ---- cumulative progression ----
                                  cumulative_timepoints = c(0.25, 0.5, 0.75, 1.0),
                                  # ---- inline guardrails ----
                                  enable_guardrails = TRUE,
                                  floor_likelihood = -999999999,
                                  guardrail_verbose = FALSE,
                                  cumulative_over_ratio = 25,
                                  cumulative_under_ratio = 0.04,
                                  min_cumulative_for_check = 100,
                                  negative_correlation_threshold = -0.1,
                                  max_cases_per_timestep = 1e6,
                                  max_deaths_per_timestep = 1e5,
                                  # ---- activity controls ----
                                  activity_obs_threshold = 1L,
                                  activity_eps           = 1e-12)
{
     # --- basic checks ---
     if (!is.matrix(obs_cases) || !is.matrix(est_cases) ||
         !is.matrix(obs_deaths) || !is.matrix(est_deaths)) {
          stop("all inputs must be matrices.")
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

     peak_method        <- match.arg(peak_method)
     growth_method      <- match.arg(growth_method)
     growth_aggregation <- match.arg(growth_aggregation)
     duration_method    <- match.arg(duration_method)
     duration_aggregation <- match.arg(duration_aggregation)

     # --- inline guardrails ---
     if (enable_guardrails) {

          if (any(est_cases > max_cases_per_timestep,  na.rm = TRUE) ||
              any(est_deaths > max_deaths_per_timestep, na.rm = TRUE)) {
               if (guardrail_verbose) message("Guardrail: est exceeds per-timestep caps.")
               return(floor_likelihood)
          }

          obs_cases_tot  <- rowSums(obs_cases,  na.rm = TRUE)
          est_cases_tot  <- rowSums(est_cases,  na.rm = TRUE)
          obs_deaths_tot <- rowSums(obs_deaths, na.rm = TRUE)
          est_deaths_tot <- rowSums(est_deaths, na.rm = TRUE)

          check_ratio <- function(o, e) {
               ok <- is.finite(o) & is.finite(e) & (o >= min_cumulative_for_check)
               r  <- rep(1, length(o)); r[ok] <- e[ok] / pmax(o[ok], 1e-12)
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
               if (sum(fin_c) >= 4) {
                    cc <- suppressWarnings(stats::cor(obs_cases[j, fin_c], est_cases[j, fin_c]))
                    if (is.finite(cc) && cc < negative_correlation_threshold) {
                         if (guardrail_verbose) message("Guardrail: strongly negative correlation (cases).")
                         return(floor_likelihood)
                    }
               }
               if (sum(fin_d) >= 4) {
                    cd <- suppressWarnings(stats::cor(obs_deaths[j, fin_d], est_deaths[j, fin_d]))
                    if (is.finite(cd) && cd < negative_correlation_threshold) {
                         if (guardrail_verbose) message("Guardrail: strongly negative correlation (deaths).")
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

          have_cases  <- any(is.finite(obs_c))
          have_deaths <- any(is.finite(obs_d))

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

          # Peak timing
          ll_peak_time_c <- ll_peak_time_d <- 0
          if (add_peak_timing) {
               if (have_cases) {
                    tpo <- peak_index(obs_c, peak_method); tpe <- peak_index(est_c, peak_method)
                    if (is.finite(tpo) && is.finite(tpe)) ll_peak_time_c <- stats::dnorm(tpe - tpo, 0, sigma_peak_time, log = TRUE)
               }
               if (have_deaths) {
                    tpo <- peak_index(obs_d, peak_method); tpe <- peak_index(est_d, peak_method)
                    if (is.finite(tpo) && is.finite(tpe)) ll_peak_time_d <- stats::dnorm(tpe - tpo, 0, sigma_peak_time, log = TRUE)
               }
          }

          # Peak magnitude
          ll_peak_mag_c <- ll_peak_mag_d <- 0
          if (add_peak_magnitude) {
               if (have_cases) {
                    pv_o <- peak_value(obs_c, peak_method); pv_e <- peak_value(est_c, peak_method)
                    if (is.finite(pv_o) && is.finite(pv_e) && pv_o > 0 && pv_e > 0)
                         ll_peak_mag_c <- stats::dnorm(log(pv_e) - log(pv_o), 0, sigma_peak_log, log = TRUE)
               }
               if (have_deaths) {
                    pv_o <- peak_value(obs_d, peak_method); pv_e <- peak_value(est_d, peak_method)
                    if (is.finite(pv_o) && is.finite(pv_e) && pv_o > 0 && pv_e > 0)
                         ll_peak_mag_d <- stats::dnorm(log(pv_e) - log(pv_o), 0, sigma_peak_log, log = TRUE)
               }
          }

          # Cumulative progression
          ll_cum_tot_c <- ll_cum_tot_d <- 0
          if (add_cumulative_total) {
               if (have_cases)  ll_cum_tot_c <- ll_cumulative_progressive_nb(obs_c, est_c, cumulative_timepoints)
               if (have_deaths) ll_cum_tot_d <- ll_cumulative_progressive_nb(obs_d, est_d, cumulative_timepoints)
          }

          # Optional: growth
          ll_growth_c <- ll_growth_d <- 0
          if (add_growth_rate) {
               if (have_cases) {
                    periods_o <- if (growth_method == "derivative")
                         identify_growth_derivative(obs_c, smooth_window = smooth_window) else {
                              epi <- identify_epidemic_periods(obs_c); mapply(c, as.list(epi$starts), as.list(epi$ends), SIMPLIFY = FALSE)
                         }
                    periods_e <- if (growth_method == "derivative")
                         identify_growth_derivative(est_c, smooth_window = smooth_window) else {
                              epi <- identify_epidemic_periods(est_c); mapply(c, as.list(epi$starts), as.list(epi$ends), SIMPLIFY = FALSE)
                         }
                    r_obs <- growth_rate_from_periods(obs_c, periods_o, agg = growth_aggregation)
                    r_est <- growth_rate_from_periods(est_c, periods_e, agg = growth_aggregation)
                    if (is.finite(r_obs) && is.finite(r_est)) ll_growth_c <- stats::dnorm(r_est - r_obs, 0, sigma_r, log = TRUE)
               }
               if (have_deaths) {
                    periods_o <- if (growth_method == "derivative")
                         identify_growth_derivative(obs_d, smooth_window = smooth_window) else {
                              epi <- identify_epidemic_periods(obs_d); mapply(c, as.list(epi$starts), as.list(epi$ends), SIMPLIFY = FALSE)
                         }
                    periods_e <- if (growth_method == "derivative")
                         identify_growth_derivative(est_d, smooth_window = smooth_window) else {
                              epi <- identify_epidemic_periods(est_d); mapply(c, as.list(epi$starts), as.list(epi$ends), SIMPLIFY = FALSE)
                         }
                    r_obs <- growth_rate_from_periods(obs_d, periods_o, agg = growth_aggregation)
                    r_est <- growth_rate_from_periods(est_d, periods_e, agg = growth_aggregation)
                    if (is.finite(r_obs) && is.finite(r_est)) ll_growth_d <- stats::dnorm(r_est - r_obs, 0, sigma_r, log = TRUE)
               }
          }

          # Optional: duration
          ll_duration_c <- ll_duration_d <- 0
          if (add_duration) {
               if (have_cases) {
                    d_obs <- duration_stat(obs_c, method = duration_method, aggregation = duration_aggregation)
                    d_est <- duration_stat(est_c, method = duration_method, aggregation = duration_aggregation)
                    if (is.finite(d_obs) && is.finite(d_est) && d_obs > 0 && d_est > 0)
                         ll_duration_c <- stats::dnorm(log(d_est) - log(d_obs), 0, sigma_duration_log, log = TRUE)
               }
               if (have_deaths) {
                    d_obs <- duration_stat(obs_d, method = duration_method, aggregation = duration_aggregation)
                    d_est <- duration_stat(est_d, method = duration_method, aggregation = duration_aggregation)
                    if (is.finite(d_obs) && is.finite(d_est) && d_obs > 0 && d_est > 0)
                         ll_duration_d <- stats::dnorm(log(d_est) - log(d_obs), 0, sigma_duration_log, log = TRUE)
               }
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

          # Activity (optional)
          ll_activity_c <- ll_activity_d <- 0
          if (add_activity) {
               if (have_cases)  ll_activity_c <- ll_activity_bernoulli(
                    obs_vec = obs_c, mu_vec = est_c, k = k_c, w_time = weights_time,
                    obs_threshold = activity_obs_threshold, eps = activity_eps
               )
               if (have_deaths) ll_activity_d <- ll_activity_bernoulli(
                    obs_vec = obs_d, mu_vec = est_d, k = k_d, w_time = weights_time,
                    obs_threshold = activity_obs_threshold, eps = activity_eps
               )
          }

          # Assemble location total
          ll_loc_core <-
               weight_cases  * (ll_cases  + if (add_max_terms) ll_max_cases  else 0) +
               weight_deaths * (ll_deaths + if (add_max_terms) ll_max_deaths else 0)

          ll_loc_peaks <-
               weight_peak_timing    * (weight_cases * ll_peak_time_c + weight_deaths * ll_peak_time_d) +
               weight_peak_magnitude * (weight_cases * ll_peak_mag_c  + weight_deaths * ll_peak_mag_d)

          ll_loc_cum <-
               weight_cumulative_total * (weight_cases * ll_cum_tot_c + weight_deaths * ll_cum_tot_d)

          ll_loc_dyn <-
               weight_growth_rate * (weight_cases * ll_growth_c + weight_deaths * ll_growth_d) +
               weight_duration    * (weight_cases * ll_duration_c + weight_deaths * ll_duration_d)

          ll_loc_wis <- (weight_cases * ll_wis_cases) + (weight_deaths * ll_wis_deaths)

          ll_loc_activity <-
               weight_activity * (weight_cases * ll_activity_c + weight_deaths * ll_activity_d)

          ll_loc_total <- ll_loc_core
          if (add_peak_timing || add_peak_magnitude) ll_loc_total <- ll_loc_total + ll_loc_peaks
          if (add_cumulative_total)                  ll_loc_total <- ll_loc_total + ll_loc_cum
          if (add_growth_rate || add_duration)       ll_loc_total <- ll_loc_total + ll_loc_dyn
          if (add_wis)                               ll_loc_total <- ll_loc_total + ll_loc_wis
          if (add_activity)                          ll_loc_total <- ll_loc_total + ll_loc_activity

          if (!is.finite(ll_loc_total)) {
               if (guardrail_verbose) message(sprintf("Non-finite LL at location %d; applying per-location penalty.", j))
               ll_locations[j] <- weights_location[j] * (-1e6)  # or another large negative penalty
               next
          }

          ll_locations[j] <- weights_location[j] * ll_loc_total

          if (verbose) {
               message(sprintf(
                    "Location %d: core=%.2f | peaks=%.2f | cum=%.2f | dyn=%.2f | wis=%.2f | act=%.2f -> weighted=%.2f",
                    j, ll_loc_core,
                    if ((add_peak_timing || add_peak_magnitude)) ll_loc_peaks else 0,
                    if (add_cumulative_total) ll_loc_cum else 0,
                    if ((add_growth_rate || add_duration)) ll_loc_dyn else 0,
                    if (add_wis) ll_loc_wis else 0,
                    if (add_activity) ll_loc_activity else 0,
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

# Peak utilities
safe_which_max <- function(x) {
     ix <- which.max(ifelse(is.finite(x), x, -Inf))
     if (length(ix) == 0L) NA_integer_ else ix
}
peak_index <- function(x, method = "smooth") {
     if (all(!is.finite(x))) return(NA_integer_)
     if (method == "simple" || length(x) < 3L) return(safe_which_max(x))
     xs <- stats::filter(ifelse(is.finite(x), x, 0), rep(1/3, 3), sides = 2)
     safe_which_max(as.numeric(xs))
}
peak_value <- function(x, method = "smooth") {
     if (method == "simple" || length(x) < 3L) {
          suppressWarnings(max(x, na.rm = TRUE))
     } else {
          xs <- stats::filter(ifelse(is.finite(x), x, 0), rep(1/3, 3), sides = 2)
          suppressWarnings(max(xs, na.rm = TRUE))
     }
}

# Robust cumulative NB progression
ll_cumulative_progressive_nb <- function(obs_vec,
                                         est_vec,
                                         timepoints = c(0.25, 0.5, 0.75, 1.0),
                                         per_tp_ll_floor = -1e6) {
     n <- length(obs_vec)
     vals <- numeric(0L)
     cum_k <- getOption("MOSAIC.cumulative_k", 10)
     for (tp in timepoints) {
          end_idx <- max(1L, floor(n * tp))
          o_cum <- sum(obs_vec[1:end_idx], na.rm = TRUE)
          e_cum <- sum(est_vec[1:end_idx], na.rm = TRUE)
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

# Growth & duration helpers
identify_growth_derivative <- function(x, smooth_window = 5, min_derivative = 0.1, min_duration = 3) {
     if (length(x) < 4L) return(list())
     x_clean <- ifelse(is.finite(x), x, 0)
     effective_window <- min(smooth_window, length(x_clean))
     xs <- if (effective_window > 1L && length(x_clean) >= effective_window) {
          stats::filter(x_clean, rep(1/effective_window, effective_window), sides = 2)
     } else x_clean
     d <- diff(xs)
     periods <- list(); in_g <- FALSE; start <- NA_integer_
     for (i in seq_along(d)) {
          if (is.finite(d[i]) && d[i] > min_derivative) {
               if (!in_g) { in_g <- TRUE; start <- i }
          } else if (in_g) {
               len <- i - start
               if (len >= min_duration) periods[[length(periods) + 1L]] <- c(start, i - 1L)
               in_g <- FALSE
          }
     }
     if (in_g) {
          len <- length(d) - start + 1L
          if (len >= min_duration) periods[[length(periods) + 1L]] <- c(start, length(d))
     }
     periods
}
growth_rate_from_periods <- function(x, periods, agg = "mean") {
     if (length(periods) == 0L) return(NA_real_)
     rates <- numeric(0)
     for (p in periods) {
          s_d <- p[1]; e_d <- p[2]
          s_x <- max(1L, s_d); e_x <- min(length(x), e_d + 1L)
          if (e_x - s_x + 1L < 2L) next
          seg <- x[s_x:e_x]
          keep <- is.finite(seg) & seg > 0
          if (sum(keep) < 2L) next
          t  <- seq_len(sum(keep)); yc <- log(seg[keep])
          fit <- try(stats::lm(yc ~ t), silent = TRUE)
          if (!inherits(fit, "try-error")) {
               r <- stats::coef(fit)[2]
               if (is.finite(r) && r > 0) rates <- c(rates, as.numeric(r))
          }
     }
     if (length(rates) == 0L) return(NA_real_)
     switch(agg, mean = mean(rates), median = stats::median(rates), max = max(rates))
}
identify_epidemic_periods <- function(x, baseline_window = 8, threshold_multiplier = 2) {
     n <- length(x)
     if (n <= baseline_window) return(list(starts = integer(0), ends = integer(0)))
     base  <- rep(NA_real_, n)
     for (i in (baseline_window + 1L):n) base[i] <- mean(x[(i - baseline_window):(i - 1L)], na.rm = TRUE)
     thr   <- base * threshold_multiplier
     above <- is.finite(x) & is.finite(thr) & (x > thr)
     above2 <- above
     if (n >= 2L) for (i in 2:n) above2[i] <- above[i] & above[i - 1L]
     r <- rle(above2)
     ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1L
     epi_starts <- starts[r$values]; epi_ends <- ends[r$values]
     epi_starts <- pmax(epi_starts, baseline_window + 1L); epi_ends[epi_ends > n] <- n
     list(starts = as.integer(epi_starts), ends = as.integer(epi_ends))
}
duration_stat <- function(x, method = "epidemic_periods", aggregation = "mean") {
     if (method == "epidemic_periods") {
          epi <- identify_epidemic_periods(x); if (length(epi$starts) == 0) return(0)
          d <- mapply(function(s,e) e - s + 1L, epi$starts, epi$ends)
          switch(aggregation, mean = mean(d), total = sum(d), max = max(d))
     } else if (method == "main_wave") {
          tot <- sum(x, na.rm = TRUE); if (!is.finite(tot) || tot <= 0) return(0L)
          target <- tot * 0.8; n <- length(x); best <- n
          for (s in seq_len(n)) {
               csum <- 0
               for (e in seq.int(s, n)) {
                    csum <- csum + ifelse(is.finite(x[e]), x[e], 0)
                    if (csum >= target) { best <- min(best, e - s + 1L); break }
               }
          }
          best
     } else {
          n <- length(x); cnt <- 0L
          baseline_window <- 8; threshold_multiplier <- 2
          for (i in (baseline_window + 1L):n) {
               base <- mean(x[(i - baseline_window):(i-1L)], na.rm = TRUE)
               thr  <- base * threshold_multiplier
               if (is.finite(x[i]) && x[i] > thr) cnt <- cnt + 1L
          }
          cnt
     }
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
     w_use <- w_time
     bad <- !is.finite(y) | !is.finite(est); if (any(bad)) w_use[bad] <- 0
     if (sum(w_use) == 0) return(NA_real_)
     est_eval <- pmax(est, 1e-12)
     qfun <- if (is.infinite(k_use)) {
          function(p) stats::qpois(p, lambda = est_eval)
     } else {
          function(p) stats::qnbinom(p, mu = est_eval, size = k_use)
     }
     probs  <- sort(unique(probs))
     has_med <- any(abs(probs - 0.5) < 1e-8)
     mae_term <- 0
     if (has_med) {
          q_med <- qfun(0.5)
          mae_term <- sum(abs(y - q_med) * w_use, na.rm = TRUE) / sum(w_use)
     }
     lowers <- probs[probs < 0.5]; uppers <- probs[probs > 0.5]
     pairs <- lapply(lowers, function(p) c(p, if ((1 - p) %in% uppers) (1 - p) else uppers[which.min(abs(uppers - (1 - p)))]))
     K <- length(pairs); sum_IS <- 0
     if (K > 0) {
          for (pq in pairs) {
               pL <- pq[1]; pU <- pq[2]; qL <- qfun(pL); qU <- qfun(pU)
               alpha <- 1 - (pU - pL)
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

# Bernoulli log-likelihood for "activity" (nonzero weeks)
#' @keywords internal
ll_activity_bernoulli <- function(obs_vec, mu_vec, k, w_time,
                                  obs_threshold = 1L, eps = 1e-12) {
     # activity indicator
     z <- as.integer(pmax(obs_vec, 0) >= obs_threshold)

     # model-implied P(active) with improved numerical stability
     mu <- pmax(mu_vec, eps)
     if (is.infinite(k)) {
          # Poisson case: P(Y > 0) = 1 - exp(-mu)
          p1 <- 1 - exp(-mu)
     } else {
          # NB: P(Y=0) = (k/(k+mu))^k  => P(active)=1 - P0
          # For numerical stability, avoid very large or small k/(k+mu) ratios
          ratio <- k / (k + mu)
          # Clamp ratio to avoid numerical overflow/underflow in power calculation
          ratio <- pmin(pmax(ratio, 1e-10), 1 - 1e-10)
          p1 <- 1 - ratio^k
     }
     
     # Improved probability clamping with more reasonable bounds
     # Instead of machine epsilon, use bounds that avoid extreme log values
     min_prob <- 0.001   # log(0.001) ≈ -6.9, reasonable penalty
     max_prob <- 0.999   # log1p(-0.999) ≈ -6.9, reasonable penalty
     p1 <- pmin(pmax(p1, min_prob), max_prob)

     # mask weights on bad entries
     w <- w_time
     bad <- !is.finite(z) | !is.finite(p1)
     if (any(bad)) w[bad] <- 0

     sw <- sum(w)
     if (sw <= 0) return(0)

     # log-likelihood sum with additional safeguards
     log_p1 <- log(p1)
     log1_minus_p1 <- log1p(-p1)
     
     # Additional check for extreme values
     log_p1[!is.finite(log_p1)] <- log(min_prob)
     log1_minus_p1[!is.finite(log1_minus_p1)] <- log(1 - max_prob)
     
     sum(w * (z * log_p1 + (1 - z) * log1_minus_p1))
}
