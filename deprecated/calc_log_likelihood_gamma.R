#' Calculate log-likelihood for Gamma-distributed data
#'
#' Computes the total log-likelihood for continuous positive data under the Gamma distribution. The shape
#' parameter for the Gamma distribution is estimated via the method of moments.
#'
#' @param observed Numeric vector of observed positive values.
#' @param estimated Numeric vector of expected means from the model (same length as \code{observed}).
#' @param verbose Logical; if \code{TRUE}, prints estimated shape parameter and total log-likelihood.
#'
#' @details
#' The Gamma distribution is parameterized as:
#' \deqn{
#' Y \\sim \\text{Gamma}(\\alpha, \\theta).
#' }
#' with mean and variance:
#' \deqn{
#' \\mu = \\alpha \\theta, \\quad \\sigma^2 = \\alpha \\theta^2.
#' }
#'
#' This can equivalently be expressed using the shape parameter \\(\\alpha\\) and mean \\(\\mu\\):
#' \deqn{
#' \\text{Var}(Y) = \\frac{\\mu^2}{\\alpha} \\quad\\Rightarrow\\quad \\alpha = \\frac{\\mu^2}{\\text{Var}(Y)}.
#' }
#'
#' The total log-likelihood for a vector of observed values \\(\\mathbf{y}\\) given
#' corresponding estimated means \\(\\mathbf{\\mu}\\) and a common shape parameter \\(\\alpha\\)
#' (estimated via method-of-moments) is:
#' \deqn{
#' \\log \\mathcal{L}(\\mathbf{y} \\mid \\mathbf{\\mu}, \\alpha) =
#' \\sum_{t=1}^{n} \\left[ \\alpha \\log(\\alpha) - \\alpha \\log(\\mu_t) - \\log\\Gamma(\\alpha)\n#' + (\\alpha - 1)\\log(y_t) - \\frac{\\alpha y_t}{\\mu_t} \\right].
#' }
#'
#' @return A scalar representing the total log-likelihood (numeric).
#' @export
#'
#' @examples
#' calc_log_likelihood_gamma(c(2.5, 3.2, 1.8), c(2.4, 3.0, 2.0))
calc_log_likelihood_gamma <- function(observed, estimated, verbose = TRUE) {

     # Remove NA
     idx <- which(!is.na(observed) & !is.na(estimated))
     observed <- observed[idx]
     estimated <- estimated[idx]

     # Checks
     if (length(observed) != length(estimated)) stop("observed and estimated must be the same length.")
     if (any(observed <= 0)) stop("All observed values must be strictly positive.")
     if (any(estimated <= 0)) stop("All estimated values must be strictly positive.")

     # Estimate shape (alpha) via method of moments
     mu <- mean(observed)
     s2 <- var(observed)
     shape <- mu^2 / s2  # alpha
     scale <- estimated / shape  # theta = mu / alpha

     if (verbose) {
          message(sprintf("Gamma shape (α) = %.2f", shape))
     }

     # Compute log-likelihood
     ll <- sum(dgamma(observed, shape = shape, scale = scale, log = TRUE))

     if (verbose) {
          message(sprintf("Gamma log-likelihood: %.2f", ll))
     }
     return(ll)
}
