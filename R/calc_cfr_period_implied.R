#' Period-weighted implied case fatality ratio (CFR) per ensemble member
#'
#' Computes the period-weighted implied CFR for each location from the
#' posterior ensemble's cases_array and deaths_array. For each ensemble
#' member (param_set x stochastic rerun), takes the sum of simulated
#' reported_deaths divided by the sum of simulated reported_cases over the
#' SCORED OBSERVED window -- days from \code{score_idx} on where both observed
#' series are finite, the same cells the observed CFR uses -- producing one CFR
#' value per member. The weighted distribution over members gives the posterior
#' on period-weighted CFR per country.
#'
#' This complements the posterior reported CFR by year
#' (\code{calc_model_ensemble()$cfr_posterior}), which is the calibrated
#' \code{mu_jt} itself. The period CFR here is what the members actually
#' produced over the window, so it also carries the case-reporting PPV switch
#' (reported CFR falls to \code{mu_jt * chi_endemic / chi_epidemic} on
#' endemic-PPV ticks) and the realised timing of cases and deaths.
#'
#' @param cases_array 4-D numeric array of simulated reported cases with
#'   dimensions \code{[n_locations, n_time, n_param_sets, n_stoch_per]}.
#' @param deaths_array 4-D numeric array of simulated reported deaths,
#'   same dimensions as \code{cases_array}.
#' @param obs_cases 2-D numeric matrix of observed reported cases with
#'   dimensions \code{[n_locations, n_time]}. NA values are dropped from
#'   the period totals.
#' @param obs_deaths 2-D numeric matrix of observed reported deaths.
#' @param location_names Character vector of ISO codes; length
#'   \code{n_locations}.
#' @param envelope_quantiles Numeric vector of 3 quantiles for CI summary.
#'   Default \code{c(0.025, 0.5, 0.975)} for 95% CI + median.
#' @param member_weights Optional parameter-set weights (length
#'   \code{n_param_sets}), shared equally across each set's stochastic reruns as
#'   in the ensemble predictions; \code{NULL} weights every member equally.
#' @param score_idx First scored time index (the burn-in is excluded).
#'
#' @return A named list keyed by location ISO code. Each element is a list
#'   with components:
#' \describe{
#'   \item{predicted_median}{Median period CFR across ensemble members.}
#'   \item{predicted_ci_lo}{Lower envelope quantile (default 2.5%).}
#'   \item{predicted_ci_hi}{Upper envelope quantile (default 97.5%).}
#'   \item{predicted_mean}{Mean across ensemble members.}
#'   \item{predicted_sd}{SD across ensemble members.}
#'   \item{n_members}{Number of finite ensemble-member CFR values.}
#'   \item{observed}{Observed period CFR (sum obs_deaths / sum obs_cases)
#'     over the scored window.}
#'   \item{predicted_total_cases}{Weighted median (across members) of total predicted reported cases over the scored window.}
#'   \item{predicted_total_deaths}{Weighted median total predicted reported deaths over the scored window.}
#'   \item{observed_total_cases}{Total observed reported cases over the scored window.}
#'   \item{observed_total_deaths}{Total observed reported deaths over the scored window.}
#' }
#'
#' @keywords internal
#' @noRd
.mosaic_calc_cfr_period_implied <- function(cases_array,
                                            deaths_array,
                                            obs_cases,
                                            obs_deaths,
                                            location_names,
                                            envelope_quantiles = c(0.025, 0.5, 0.975),
                                            member_weights = NULL,
                                            score_idx = 1L) {

     stopifnot(
          length(envelope_quantiles) == 3L,
          is.array(cases_array), is.array(deaths_array),
          identical(dim(cases_array), dim(deaths_array))
     )

     dims <- dim(cases_array)
     # Accept 4-D [loc, time, p, s] OR 3-D [loc, time, p] (collapse to p=members)
     if (length(dims) == 3L) {
          dim(cases_array)  <- c(dims, 1L)
          dim(deaths_array) <- c(dims, 1L)
          dims <- dim(cases_array)
     }
     stopifnot(length(dims) == 4L, dims[1] == length(location_names))

     # Treat obs_cases / obs_deaths as matrices indexed [loc, time]
     if (!is.matrix(obs_cases))  obs_cases  <- matrix(obs_cases,  nrow = 1)
     if (!is.matrix(obs_deaths)) obs_deaths <- matrix(obs_deaths, nrow = 1)

     # Fail fast on dimension mismatch — a transposed [n_time, n_loc] obs
     # matrix would otherwise silently produce wrong observed totals.
     stopifnot(
          "obs_cases rows must match location_names"  =
               nrow(obs_cases)  == length(location_names),
          "obs_deaths rows must match location_names" =
               nrow(obs_deaths) == length(location_names)
     )

     n_mem <- dims[3] * dims[4]
     w_mem <- if (is.null(member_weights)) rep(1, n_mem) else {
          if (length(member_weights) != dims[3])
               stop("member_weights must have one value per parameter set.")
          rep(as.numeric(member_weights), times = dims[4]) / dims[4]
     }
     w_mem[!is.finite(w_mem) | w_mem < 0] <- 0
     wq <- function(v, w, p) weighted_quantiles(v, w, p)
     wmean <- function(v, w) { ok <- is.finite(v) & w > 0; if (!any(ok)) NA_real_ else sum(v[ok] * w[ok]) / sum(w[ok]) }
     wsd <- function(v, w) {
          ok <- is.finite(v) & w > 0
          if (sum(ok) < 2L) return(NA_real_)
          m <- sum(v[ok] * w[ok]) / sum(w[ok])
          sqrt(sum(w[ok] * (v[ok] - m)^2) / sum(w[ok]))
     }

     # Minimum ensemble members below which the across-member CI is too
     # noisy to be informative. Below this we still report median/mean but
     # the CI is NA-filled.
     MIN_MEMBERS_FOR_CI <- 20L

     out <- list()
     for (i in seq_along(location_names)) {
          iso <- location_names[i]

          # The scored observed window: from score_idx on, where both observed
          # series are finite. Predicted and observed totals use the SAME cells,
          # so neither the burn-in nor the unobserved forecast tail enters.
          tt <- seq_len(dims[2])
          cells <- tt >= score_idx & is.finite(obs_cases[i, ]) & is.finite(obs_deaths[i, ])

          # Per-(param_set, stoch) sums over the window -> [p, s] -> vec
          mc <- apply(cases_array[i, cells, , , drop = FALSE],  c(3L, 4L), sum, na.rm = TRUE)
          md <- apply(deaths_array[i, cells, , , drop = FALSE], c(3L, 4L), sum, na.rm = TRUE)
          mem_cases_full  <- as.numeric(mc)
          mem_deaths_full <- as.numeric(md)
          # CFR ratio is only defined when a member produced any cases. Keep
          # the original totals for reporting; mask zero-case members from
          # the ratio computation.
          mem_cfr_all <- ifelse(mem_cases_full > 0,
                                mem_deaths_full / mem_cases_full,
                                NA_real_)
          keep_m  <- is.finite(mem_cfr_all) & w_mem > 0
          mem_cfr <- mem_cfr_all[keep_m]
          w_cfr   <- w_mem[keep_m]

          # Observed period totals over the same cells
          obs_c_sum <- sum(obs_cases[i, cells])
          obs_d_sum <- sum(obs_deaths[i, cells])
          cfr_obs   <- if (obs_c_sum > 0) obs_d_sum / obs_c_sum else NA_real_

          # Weighted median predicted totals across ALL ensemble members (not
          # just those with mem_cases > 0): masking would bias upward when rare
          # members produce zero cases.
          pred_c_tot <- wq(mem_cases_full,  w_mem, 0.5)
          pred_d_tot <- wq(mem_deaths_full, w_mem, 0.5)

          summary_loc <- list(
               predicted_median = if (length(mem_cfr) >= 1L)
                    wq(mem_cfr, w_cfr, 0.5) else NA_real_,
               predicted_ci_lo  = if (length(mem_cfr) >= MIN_MEMBERS_FOR_CI)
                    wq(mem_cfr, w_cfr, envelope_quantiles[1]) else NA_real_,
               predicted_ci_hi  = if (length(mem_cfr) >= MIN_MEMBERS_FOR_CI)
                    wq(mem_cfr, w_cfr, envelope_quantiles[3]) else NA_real_,
               predicted_mean   = if (length(mem_cfr) >= 1L)
                    wmean(mem_cfr, w_cfr) else NA_real_,
               predicted_sd     = if (length(mem_cfr) >= 2L)
                    wsd(mem_cfr, w_cfr) else NA_real_,
               n_members        = length(mem_cfr),
               n_param_sets     = dims[3],
               n_stoch_per      = dims[4],
               observed         = cfr_obs,
               predicted_total_cases  = pred_c_tot,
               predicted_total_deaths = pred_d_tot,
               observed_total_cases   = obs_c_sum,
               observed_total_deaths  = obs_d_sum
          )

          out[[iso]] <- summary_loc
     }

     out
}
