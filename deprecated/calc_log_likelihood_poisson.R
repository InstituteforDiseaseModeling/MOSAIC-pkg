#' Calculate log-likelihood for Poisson-distributed count data
#'
#' Computes the total log-likelihood for integer count data under the Poisson distribution.
#' Appropriate for observed data that are non-negative integer counts and have approximately equal mean and variance.
#'
#' @param observed Integer vector of observed non-negative counts (e.g., cases, deaths).
#' @param estimated Numeric vector of expected values from the model (same length as \code{observed}).
#' @param verbose Logical; if \code{TRUE}, prints diagnostics and total log-likelihood.
#'
#' @details
#' The Poisson distribution assumes that the variance equals the mean, \(\text{Var}(Y) = \mu\).
#'
#' Under a Bayesian framework, the total log-likelihood is interpreted as the log-probability
#' of observing the data \(\mathbf{y}\) given the model-predicted means \(\mathbf{\mu}\):
#' \[
#' \log \mathcal{L}(\mathbf{\mu} \mid \mathbf{y}) = \sum_{t=1}^{n} \left[ y_t \log(\mu_t) - \mu_t - \log(y_t!) \right].
#' \]
#'
#' A warning is issued if the variance-to-mean ratio significantly exceeds 1, as this may indicate overdispersion.
#'
#' @return A scalar representing the total log-likelihood (numeric).
#' @export
#'
#' @examples
#' calc_log_likelihood_poisson(c(2, 3, 4), c(2.2, 2.9, 4.1))
calc_log_likelihood_poisson <- function(observed, estimated, verbose = TRUE) {

     # Remove NA
     idx <- which(!is.na(observed) & !is.na(estimated))
     observed <- observed[idx]
     estimated <- estimated[idx]

     # Checks
     if (length(observed) != length(estimated)) {
          stop("observed and estimated must be the same length.")
     }
     if (any(observed < 0 | observed %% 1 != 0)) {
          stop("observed must contain non-negative integers.")
     }

     mu <- mean(observed)
     s2 <- var(observed)
     disp_ratio <- s2 / mu

     if (disp_ratio > 1.5) {
          warning(sprintf("Var/Mean = %.2f suggests overdispersion. Consider Negative Binomial.", disp_ratio))
     }

     ll <- sum(observed * log(estimated) - estimated - lgamma(observed + 1))

     if (verbose) {
          message(sprintf("Poisson log-likelihood: %.2f", ll))
     }
     return(ll)
}
