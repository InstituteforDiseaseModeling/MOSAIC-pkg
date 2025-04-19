#' Calculate Poisson or Negative Binomial log-likelihood
#'
#' Computes the log-likelihood for observed counts given model estimates under either
#' the Poisson or Negative Binomial distribution. Dispersion for Negative Binomial is
#' estimated using the method of moments comparing mean and variance of the observed data.
#'
#' @param observed Integer vector of observed counts.
#' @param estimated Numeric vector of expected values from a model.
#' @param family Character; either \"poisson\" or \"negbin\".
#' @param verbose Logical; if TRUE, print diagnostic messages.
#'
#' @return A scalar log-likelihood.
#' @export
#'

calc_log_likelihood <- function(observed,
                                estimated,
                                family = c("poisson", "negbin"),
                                verbose = TRUE) {

     family <- match.arg(family)

     # Remove NAs
     valid_idx <- which(!is.na(observed) & !is.na(estimated))
     observed <- observed[valid_idx]
     estimated <- estimated[valid_idx]

     # Validate
     if (length(observed) != length(estimated)) stop("observed and estimated must be the same length.")
     if (any(observed < 0 | observed %% 1 != 0)) stop("observed must contain non-negative integers.")

     # Internal log-likelihood functions
     log_nb <- function(y, mu, k) {
          lgamma(y + k) - lgamma(k) - lgamma(y + 1) + k * log(k / (k + mu)) + y * log(mu / (k + mu))
     }

     log_pois <- function(y, mu) {
          y * log(mu) - mu - lgamma(y + 1)
     }

     # Compute log-likelihood
     if (family == "poisson") {

          if (verbose) message("Using Poisson model")
          ll <- sum(log_pois(observed, estimated))

     } else {

          # Estimate dispersion
          mu <- mean(observed)
          s2 <- var(observed)

          if (s2 <= mu) {

               if (verbose) message(sprintf("Var/Mean = %.2f → switching to Poisson (k = Inf)", s2 / mu))
               ll <- sum(log_pois(observed, estimated))

          } else {

               k <- mu^2 / (s2 - mu)

               if (verbose) {

                    message(sprintf("Using Negative Binomial model with estimated k = %.2f", k))
                    if (k < 1.5) warning(sprintf("Estimated k = %.2f indicates near-Poisson dispersion", k))

               }

               ll <- sum(log_nb(observed, estimated, k))

          }
     }

     if (verbose) message(sprintf("Log-likelihood: %.2f", ll))
     return(ll)
}
