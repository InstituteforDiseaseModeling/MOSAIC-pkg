#' Period-weighted implied case fatality ratio (CFR) per ensemble member
#'
#' Computes the period-weighted implied CFR for each location from the
#' posterior ensemble's cases_array and deaths_array. For each ensemble
#' member (param_set x stochastic rerun), takes the sum of simulated
#' reported_deaths over the simulation window divided by the sum of
#' simulated reported_cases over the same window, producing one CFR value
#' per member. The distribution over members gives the posterior on
#' period-weighted CFR per country.
#'
#' This complements the algebraic implied CFR derived in
#' \code{.mosaic_add_implied_cfr_columns()}, which uses sampled
#' microparameter values directly via the steady-state identity. The
#' period CFR here is what the engine actually produced when fitting the
#' surveillance data, including time-varying chi switching and the
#' epidemic-period multiplier on mu_jt; the algebraic CFR is the
#' steady-state limit at the posterior parameter means under a clean
#' regime split (endemic vs epidemic).
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
#'
#' @return A named list keyed by location ISO code. Each element is a list
#'   with components:
#' \describe{
#'   \item{predicted_median}{Median period CFR across ensemble members.}
#'   \item{predicted_ci_lo}{Lower envelope quantile (default 2.5\%).}
#'   \item{predicted_ci_hi}{Upper envelope quantile (default 97.5\%).}
#'   \item{predicted_mean}{Mean across ensemble members.}
#'   \item{predicted_sd}{SD across ensemble members.}
#'   \item{n_members}{Number of finite ensemble-member CFR values.}
#'   \item{observed}{Observed period CFR (sum obs_deaths / sum obs_cases)
#'     over non-NA cells.}
#'   \item{predicted_total_cases}{Median (across members) of total predicted reported cases.}
#'   \item{predicted_total_deaths}{Median total predicted reported deaths.}
#'   \item{observed_total_cases}{Total observed reported cases (NA-omitted).}
#'   \item{observed_total_deaths}{Total observed reported deaths (NA-omitted).}
#' }
#'
#' @keywords internal
#' @noRd
.mosaic_calc_cfr_period_implied <- function(cases_array,
                                            deaths_array,
                                            obs_cases,
                                            obs_deaths,
                                            location_names,
                                            envelope_quantiles = c(0.025, 0.5, 0.975)) {

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

     # Minimum ensemble members below which the across-member CI is too
     # noisy to be informative. Below this we still report median/mean but
     # the CI is NA-filled.
     MIN_MEMBERS_FOR_CI <- 20L

     out <- list()
     for (i in seq_along(location_names)) {
          iso <- location_names[i]

          # Per-(param_set, stoch) sums over time
          # cases_array slice: [time, p, s] -> sum over time -> [p, s] -> vec
          mc <- apply(cases_array[i, , , , drop = FALSE],  c(3L, 4L), sum, na.rm = TRUE)
          md <- apply(deaths_array[i, , , , drop = FALSE], c(3L, 4L), sum, na.rm = TRUE)
          mem_cases_full  <- as.numeric(mc)
          mem_deaths_full <- as.numeric(md)
          # CFR ratio is only defined when a member produced any cases. Keep
          # the original totals for reporting; mask zero-case members from
          # the ratio computation.
          mem_cfr <- ifelse(mem_cases_full > 0,
                            mem_deaths_full / mem_cases_full,
                            NA_real_)
          mem_cfr <- mem_cfr[is.finite(mem_cfr)]

          # Observed period totals (NA-omitted)
          obs_c_sum <- sum(obs_cases[i, ],  na.rm = TRUE)
          obs_d_sum <- sum(obs_deaths[i, ], na.rm = TRUE)
          cfr_obs   <- if (obs_c_sum > 0) obs_d_sum / obs_c_sum else NA_real_

          # Median predicted totals across ALL ensemble members (not just
          # those with mem_cases > 0). This is the correct reporting unit
          # for the simulation window — masking would bias upward when
          # rare members produce zero cases.
          pred_c_tot <- stats::median(mem_cases_full,  na.rm = TRUE)
          pred_d_tot <- stats::median(mem_deaths_full, na.rm = TRUE)

          summary_loc <- list(
               predicted_median = if (length(mem_cfr) >= 1L)
                    stats::median(mem_cfr, na.rm = TRUE) else NA_real_,
               predicted_ci_lo  = if (length(mem_cfr) >= MIN_MEMBERS_FOR_CI)
                    unname(stats::quantile(mem_cfr, envelope_quantiles[1], na.rm = TRUE))
                    else NA_real_,
               predicted_ci_hi  = if (length(mem_cfr) >= MIN_MEMBERS_FOR_CI)
                    unname(stats::quantile(mem_cfr, envelope_quantiles[3], na.rm = TRUE))
                    else NA_real_,
               predicted_mean   = if (length(mem_cfr) >= 1L)
                    mean(mem_cfr, na.rm = TRUE) else NA_real_,
               predicted_sd     = if (length(mem_cfr) >= 2L)
                    stats::sd(mem_cfr, na.rm = TRUE) else NA_real_,
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
