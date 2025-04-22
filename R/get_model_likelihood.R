#' Compute the total model likelihood over multiple locations and time steps,
#' auto-selecting Poisson or NegBin for each location/outcome
#'
#' This function expects matrices of observed and estimated cases and deaths (size
#' \code{n_locations} x \code{n_time_steps}), plus optional weight vectors for
#' locations and time steps (\code{weights_location} and \code{weights_time}).
#' It automatically selects Poisson or Negative Binomial for each location and outcome
#' based on whether \code{Var(observed) > Mean(observed)}.
#' it uses Poisson; otherwise it uses NegBin. Then it multiplies
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
#' @details
#' For each location \eqn{j} and each outcome (cases/deaths):
#' \enumerate{
#'   \item If \code{Var(obs_*) > Mean(obs_*)}, use Negative Binomial. Otherwise, use Poisson.
#'   \item Compute the log-likelihood with \code{\link{calc_log_likelihood}}, passing
#'         \code{weights_time} as the observation-level weights.
#' }
#' Then it scales the cases-likelihood by \code{weight_cases} and the deaths-likelihood
#' by \code{weight_deaths}, sums them up, and multiplies by \code{weights_location[j]}
#' for location-level weighting.
#'
#' @return A single numeric value giving the total log-likelihood across all locations
#' and time steps, with each outcome using Poisson or NB automatically.
#'
#' @export
#'
#' @examples
#' # 3 locations, 4 time steps
#' obsC <- matrix(c(0,1,2,1, 4,5,6,5, 9,8,7,9), nrow=3, byrow=TRUE)
#' estC <- obsC + 0.2
#' obsD <- matrix(c(0,0,1,1, 1,0,0,2, 0,1,0,1), nrow=3, byrow=TRUE)
#' estD <- obsD + 0.1
#'
#' w_loc <- c(1, 2, 1)
#' w_time <- c(1, 1, 1, 1)
#'
#' get_model_likelihood_auto(obsC, estC, obsD, estD, w_loc, w_time, verbose=TRUE)
#'

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
     if (is.null(weights_location)) {
          weights_location <- rep(1, n_locations)
     }
     if (is.null(weights_time)) {
          weights_time <- rep(1, n_time_steps)
     }

     # 3) Default outcome weights
     if (is.null(weight_cases))  weight_cases <- 1
     if (is.null(weight_deaths)) weight_deaths <- 1

     # 4) Validate weighting arrays
     if (length(weights_location) != n_locations) {
          stop("weights_location must match the number of locations.")
     }
     if (length(weights_time) != n_time_steps) {
          stop("weights_time must match the number of time steps.")
     }
     if (any(weights_location < 0)) stop("All weights_location values must be >= 0.")
     if (any(weights_time < 0)) stop("All weights_time values must be >= 0.")
     if (sum(weights_location) == 0 || sum(weights_time) == 0) {
          stop("weights_location and weights_time must not all be zero.")
     }
     if (weight_cases < 0) {
          stop("weight_cases must be >= 0.")
     }
     if (weight_deaths < 0) {
          stop("weight_deaths must be >= 0.")
     }

     ll_total <- 0

     for (j in seq_len(n_locations)) {
          # Inline check for cases
          family_cases <- if (var(obs_cases[j, ], na.rm=TRUE) > mean(obs_cases[j, ], na.rm=TRUE))
               "negbin" else "poisson"

          ll_cases <- calc_log_likelihood(
               observed  = obs_cases[j, ],
               estimated = est_cases[j, ],
               family    = family_cases,
               weights   = weights_time,
               verbose   = FALSE
          )

          # Inline check for deaths
          family_deaths <- if (var(obs_deaths[j, ], na.rm=TRUE) > mean(obs_deaths[j, ], na.rm=TRUE))
               "negbin" else "poisson"

          ll_deaths <- calc_log_likelihood(
               observed  = obs_deaths[j, ],
               estimated = est_deaths[j, ],
               family    = family_deaths,
               weights   = weights_time,
               verbose   = FALSE
          )

          # Weighted sum for this location
          ll_location <- (weight_cases * ll_cases) + (weight_deaths * ll_deaths)
          ll_total <- ll_total + weights_location[j] * ll_location

          if (verbose) {
               # We can optionally track var/mean for debugging:
               vC <- var(obs_cases[j, ], na.rm=TRUE)
               mC <- mean(obs_cases[j, ], na.rm=TRUE)
               vD <- var(obs_deaths[j, ], na.rm=TRUE)
               mD <- mean(obs_deaths[j, ], na.rm=TRUE)

               message(sprintf(
                    "Location %d:\n  Cases: var=%.2f, mean=%.2f => %s, LL=%.2f;\n  Deaths: var=%.2f, mean=%.2f => %s, LL=%.2f;\n  Weighted=%.2f",
                    j,
                    vC, mC, family_cases, ll_cases,
                    vD, mD, family_deaths, ll_deaths,
                    weights_location[j] * ll_location
               ))
          }
     }

     if (verbose) {
          message(sprintf("Overall total log-likelihood: %.2f", ll_total))
     }

     return(ll_total)
}
