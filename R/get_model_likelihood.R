#' Compute the total model likelihood
#'
#' This function expects matrices of observed and estimated cases and deaths (size
#' \code{n_locations} x \code{n_time_steps}), plus optional weight vectors for
#' locations and time steps (\code{weights_location} and \code{weights_time}).
#' It automatically selects Poisson or Negative Binomial for each location and outcome
#' based on whether \code{Var(observed) / Mean(observed) >= 1.5}. If the ratio is below
#' 1.5, it uses Poisson; otherwise it uses NegBin. Then it multiplies
#' the cases log-likelihood by \code{weight_cases} and the deaths log-likelihood by
#' \code{weight_deaths} for additional flexibility in balancing the two outcomes.
#'
#' @param obs_cases Matrix of observed cases, \code{n_locations} x \code{n_time_steps}.
#' @param est_cases Matrix of estimated cases, same dimension as \code{obs_cases}.
#' @param obs_deaths Matrix of observed deaths, \code{n_locations} x \code{n_time_steps}.
#' @param est_deaths Matrix of estimated deaths, same dimension as \code{obs_deaths}.
#' @param weight_cases Numeric scalar (or \code{NULL}), default \code{NULL} which sets to 1 internally.
#' @param weight_deaths Numeric scalar (or \code{NULL}), default \code{NULL} which sets to 1 internally.
#' @param weights_location Optional numeric vector of length \code{n_locations}, or \code{NULL}.
#'                         If \code{NULL}, all locations are weighted equally.
#' @param weights_time Optional numeric vector of length \code{n_time_steps}, or \code{NULL}.
#'                     If \code{NULL}, all time steps are weighted equally.
#' @param verbose Logical; if \code{TRUE}, prints summary messages. Default is \code{FALSE}.
#'
#' @return A single numeric value giving the total log-likelihood across all locations
#' and time steps, or \code{NA_real_} if all locations are skipped.
#'
#' @export
get_model_likelihood <- function(obs_cases, est_cases,
                                 obs_deaths, est_deaths,
                                 weight_cases     = NULL,
                                 weight_deaths    = NULL,
                                 weights_location = NULL,
                                 weights_time     = NULL,
                                 verbose = FALSE)
{
     # 1) Matrix dimension checks
     if (!is.matrix(obs_cases) || !is.matrix(est_cases) ||
         !is.matrix(obs_deaths) || !is.matrix(est_deaths)) {
          stop("obs_* and est_* must be matrices.")
     }

     n_locations  <- nrow(obs_cases)
     n_time_steps <- ncol(obs_cases)

     if (any(dim(est_cases)   != c(n_locations, n_time_steps)) ||
         any(dim(obs_deaths)  != c(n_locations, n_time_steps)) ||
         any(dim(est_deaths)  != c(n_locations, n_time_steps))) {
          stop("All obs_* and est_* matrices must have the same dimensions (n_locations x n_time_steps).")
     }

     # 2) Default location/time weights
     if (is.null(weights_location)) weights_location <- rep(1, n_locations)
     if (is.null(weights_time))     weights_time     <- rep(1, n_time_steps)
     if (is.null(weight_cases))     weight_cases     <- 1
     if (is.null(weight_deaths))    weight_deaths    <- 1

     if (length(weights_location) != n_locations) stop("weights_location must match the number of locations.")
     if (length(weights_time) != n_time_steps)   stop("weights_time must match the number of time steps.")
     if (any(weights_location < 0)) stop("All weights_location values must be >= 0.")
     if (any(weights_time < 0))     stop("All weights_time values must be >= 0.")
     if (sum(weights_location) == 0 || sum(weights_time) == 0) stop("weights_location and weights_time must not all be zero.")
     if (weight_cases < 0)  stop("weight_cases must be >= 0.")
     if (weight_deaths < 0) stop("weight_deaths must be >= 0.")

     # Initialize a vector to store per-location log-likelihood
     ll_locations <- rep(NA_real_, n_locations)

     for (j in seq_len(n_locations)) {

          mean_cases <- mean(obs_cases[j, ], na.rm = TRUE)
          var_cases  <- var(obs_cases[j, ], na.rm = TRUE)

          # If cases row fully NA, skip
          if (is.na(mean_cases) || is.na(var_cases)) {
               if (verbose) message(sprintf("Location %d (cases): all NA — skipping.", j))
               next
          }
          # Decide family for cases
          family_cases <- if (var_cases / mean_cases >= 1.5) "negbin" else "poisson"

          mean_deaths <- mean(obs_deaths[j, ], na.rm = TRUE)
          var_deaths  <- var(obs_deaths[j, ], na.rm = TRUE)

          # If deaths row fully NA, skip
          if (is.na(mean_deaths) || is.na(var_deaths)) {
               if (verbose) message(sprintf("Location %d (deaths): all NA — skipping.", j))
               next
          }

          # Decide family for deaths
          family_deaths <- if (var_deaths / mean_deaths >= 1.5) "negbin" else "poisson"

          # Calculate log-likelihood for cases
          ll_cases <- MOSAIC::calc_log_likelihood(
               observed  = obs_cases[j, ],
               estimated = est_cases[j, ],
               family    = family_cases,
               weights   = weights_time,
               verbose   = FALSE
          )

          ll_max_cases <- MOSAIC::calc_log_likelihood(
               observed  = max(obs_cases[j, ], na.rm = TRUE),
               estimated = max(est_cases[j, ], na.rm = TRUE),
               family    = "poisson",
               weights   = NULL,
               verbose   = FALSE
          )

          # Calculate log-likelihood for deaths
          ll_deaths <- MOSAIC::calc_log_likelihood(
               observed  = obs_deaths[j, ],
               estimated = est_deaths[j, ],
               family    = family_deaths,
               weights   = weights_time,
               verbose   = FALSE
          )

          ll_max_deaths <- MOSAIC::calc_log_likelihood(
               observed  = max(obs_deaths[j, ], na.rm = TRUE),
               estimated = max(est_deaths[j, ], na.rm = TRUE),
               family    = "poisson",
               weights   = NULL,
               verbose   = FALSE
          )

          # Weighted sum for location j
          ll_location_tmp <- weights_location[j] * (weight_cases * ll_cases + weight_cases * ll_max_cases + weight_deaths * ll_deaths + weight_deaths * ll_max_deaths)
          ll_locations[j] <- ll_location_tmp

          if (verbose) {
               message(sprintf(
                    "Location %d:\n  Cases: var=%.2f, mean=%.2f => %s, LL=%.2f;\n  Deaths: var=%.2f, mean=%.2f => %s, LL=%.2f;\n  Weighted=%.2f",
                    j,
                    var_cases, mean_cases, family_cases, ll_cases,
                    var_deaths, mean_deaths, family_deaths, ll_deaths,
                    ll_location_tmp
               ))
          }
     }

     # If everything was skipped
     if (all(is.na(ll_locations))) {
          if (verbose) message("All locations skipped — returning NA.")
          return(NA_real_)
     }

     ll_total <- sum(ll_locations, na.rm = TRUE)

     if (verbose) {
          message(sprintf("Overall total log-likelihood: %.2f", ll_total))
     }

     return(ll_total)
}
