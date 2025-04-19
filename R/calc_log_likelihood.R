#' Calculate Poisson or Negative Binomial log-likelihood
#'
#' Computes the log-likelihood of observed counts given expected values under either
#' the Poisson or Negative Binomial distribution. Dispersion for NB is estimated via
#' method-of-moments unless \code{family = "poisson"} is used.
#'
#' @param observed Integer vector of observed counts (e.g., cases or deaths).
#' @param estimated Numeric vector of expected values from the model.
#' @param family Character string: one of \code{"auto"}, \code{"poisson"}, or \code{"negbin"}.
#' @param verbose Logical; if \code{TRUE}, prints messages about model choice, dispersion, and likelihood.
#'
#' @details
#' The choice of likelihood family is handled as follows:
#'
#' \tabular{llll}{
#' \strong{Family}   \tab \strong{Variance/Mean} \tab \strong{Behavior}               \tab \strong{Notes} \cr
#' "poisson" \tab any               \tab Poisson used                \tab Warns if Var/Mean > 1.5 \cr
#' "negbin"  \tab > 1               \tab NegBin used with estimated k\tab Errors if Var ≤ Mean \cr
#' "auto"    \tab ≤ 1               \tab Poisson used                \tab Safe fallback \cr
#' "auto"    \tab > 1               \tab NegBin used with estimated k\tab Smart selection
#' }
#'
#' The NB log-likelihood is calculated using the gamma-function form. When \code{family = "negbin"}
#' or \code{"auto"} and \eqn{\text{Var}(Y) > \mathbb{E}(Y)}, the dispersion is estimated as:
#'
#' \deqn{
#' k = \frac{\mu^2}{\text{Var} - \mu}
#' }
#'
#' @return A scalar log-likelihood value.
#' @export
calc_log_likelihood <- function(observed,
                                estimated,
                                family = c("auto", "poisson", "negbin"),
                                verbose = TRUE) {
     family <- match.arg(family)

     # Remove NA pairs
     valid_idx <- which(!is.na(observed) & !is.na(estimated))
     observed <- observed[valid_idx]
     estimated <- estimated[valid_idx]

     # Validate
     if (length(observed) != length(estimated)) stop("observed and estimated must be the same length.")
     if (any(observed < 0 | observed %% 1 != 0)) stop("observed must contain non-negative integers.")

     # Internal log-likelihood functions
     log_nb <- function(y, mu, k) {
          lgamma(y + k) - lgamma(k) - lgamma(y + 1) +
               k * log(k / (k + mu)) +
               y * log(mu / (k + mu))
     }

     log_pois <- function(y, mu) {
          y * log(mu) - mu - lgamma(y + 1)
     }

     # Compute moments
     mu <- mean(observed)
     s2 <- var(observed)
     disp_ratio <- s2 / mu

     # Family logic
     if (family == "poisson") {
          if (verbose) message("Using Poisson model (forced).")
          if (disp_ratio > 1.5) {
               warning(sprintf("Var/Mean = %.2f suggests overdispersion. Consider family = 'negbin'.", disp_ratio))
          }
          ll <- sum(log_pois(observed, estimated))

     } else if (family == "negbin") {
          if (disp_ratio <= 1) {
               stop(sprintf("Negative Binomial requested but Var/Mean = %.2f ≤ 1 — invalid dispersion.", disp_ratio))
          }
          k <- mu^2 / (s2 - mu)
          if (verbose) {
               message(sprintf("Using Negative Binomial model (forced) with estimated k = %.2f", k))
               if (k < 1.5) warning(sprintf("k = %.2f indicates near-Poisson dispersion", k))
          }
          ll <- sum(log_nb(observed, estimated, k))

     } else if (family == "auto") {
          if (disp_ratio <= 1) {
               if (verbose) message(sprintf("Auto: Var/Mean = %.2f ≤ 1 → using Poisson", disp_ratio))
               ll <- sum(log_pois(observed, estimated))
          } else {
               k <- mu^2 / (s2 - mu)
               if (verbose) {
                    message(sprintf("Auto: Var/Mean = %.2f → using Negative Binomial with k = %.2f", disp_ratio, k))
                    if (k < 1.5) warning(sprintf("k = %.2f indicates near-Poisson dispersion", k))
               }
               ll <- sum(log_nb(observed, estimated, k))
          }
     }

     if (verbose) message(sprintf("Log-likelihood: %.2f", ll))
     return(ll)
}
