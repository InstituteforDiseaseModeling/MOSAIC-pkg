#' Calculate log-likelihood for Negative Binomial-distributed count data
#'
#' Computes the total log-likelihood for count data under the Negative Binomial distribution,
#' using the gamma-function formulation. This distribution is suitable for count data exhibiting
#' overdispersion (variance > mean).
#'
#' @param observed Integer vector of observed non-negative counts (e.g., cases, deaths).
#' @param estimated Numeric vector of expected values from the model (same length as \code{observed}).
#' @param k Numeric scalar; dispersion parameter. If \code{NULL}, it is estimated via method of moments.
#' @param verbose Logical; if \code{TRUE}, prints diagnostics including estimated dispersion and total log-likelihood.
#'
#' @details
#' The Negative Binomial distribution accounts for overdispersion with:
#' \\deqn{
#' \\text{Var}(Y) = \\mu + \\frac{\\mu^2}{k}.
#' }
#'
#' The total log-likelihood, under a Bayesian interpretation, represents the log-probability of the data
#' given parameters \\((\\mathbf{\\mu}, k)\\):
#' \\deqn{
#' \\log \\mathcal{L}(\\mathbf{\\mu}, k \\mid \\mathbf{y}) =
#' \\sum_{t=1}^{n} \\left[ \\log \\Gamma(y_t + k) - \\log \\Gamma(k) - \\log \\Gamma(y_t + 1)
#' + k \\log\\left(\\frac{k}{k + \\mu_t}\\right)
#' + y_t \\log\\left(\\frac{\\mu_t}{k + \\mu_t}\\right) \\right].
#' }
#'
#' If \\code{k = NULL}, the dispersion parameter \\( k \\) is estimated using method-of-moments:
#' \\deqn{
#' k = \\frac{\\mu^2}{\\text{Var}(Y) - \\mu}.
#' }
#' This requires \\(\\text{Var}(Y) > \\mu\\); otherwise, the function throws an error.
#'
#' @return A scalar representing the total log-likelihood (numeric).
#' @export
#'
#' @examples
#' calc_log_likelihood_negbin(c(0, 5, 9), c(3, 4, 5))
calc_log_likelihood_negbin <- function(observed, estimated, k = NULL, verbose = TRUE) {

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

     # Possibly estimate k
     mu <- mean(observed)
     s2 <- var(observed)
     if (is.null(k)) {
          if (s2 <= mu) {
               stop(sprintf("Cannot estimate dispersion: Var = %.2f, Mean = %.2f (Var <= Mean)", s2, mu))
          }
          k <- mu^2 / (s2 - mu)
          if (verbose) {
               message(sprintf("Estimated k = %.2f (from Var = %.2f, Mean = %.2f)", k, s2, mu))
          }
     } else {
          if (verbose) message(sprintf("Using provided k = %.2f", k))
     }

     if (k < 1.5 && verbose) {
          warning(sprintf("k = %.2f indicates near-Poisson dispersion.", k))
     }

     ll <- sum(
          lgamma(observed + k) - lgamma(k) - lgamma(observed + 1) +
               k * log(k / (k + estimated)) +
               observed * log(estimated / (k + estimated))
     )

     if (verbose) {
          message(sprintf("Negative Binomial log-likelihood: %.2f", ll))
     }
     return(ll)
}
