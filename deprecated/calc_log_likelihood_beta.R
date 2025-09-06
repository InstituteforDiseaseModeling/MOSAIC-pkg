#' Calculate log-likelihood for Beta-distributed proportions
#'
#' Computes the total log-likelihood for proportion data under the Beta distribution.
#' Supports either the mean–precision parameterization (default) or the standard
#' shape parameterization. In both cases, a single global set of shape parameters is
#' estimated from the data using the method of moments.
#'
#' @param observed Numeric vector of observed values in (0, 1).
#' @param estimated Numeric vector of model-predicted values in (0, 1).
#' @param mean_precision Logical; if \code{TRUE} (default), use mean–precision parameterization.
#'                       If \code{FALSE}, estimate shape parameters directly from the observed vector.
#' @param verbose Logical; if \code{TRUE}, prints shape parameter estimates and total log-likelihood.
#'
#' @details
#' When \code{mean_precision = TRUE}, the precision parameter \eqn{\phi} is estimated as:
#' \deqn{
#' \phi = \frac{\mu (1 - \mu)}{\text{Var}(Y)} - 1
#' }
#' Then:
#' \deqn{
#' \text{shape}_1 = \mu_t \phi, \quad \text{shape}_2 = (1 - \mu_t) \phi
#' }
#'
#' This mean–precision parameterization is commonly used in Bayesian modeling to adjust
#' the Beta distribution shape based on the clustering of values around the mean
#' (see [Gelman et al. *Bayesian Data Analysis* (3rd ed.)](https://sites.stat.columbia.edu/gelman/book/BDA3.pdf)).
#'
#' When \code{mean_precision = FALSE}, a single global \eqn{\text{shape}_1}, \eqn{\text{shape}_2}
#' is estimated from the observed vector:
#' \deqn{
#' \text{shape}_1 = \left( \frac{1 - \mu}{\sigma^2} - \frac{1}{\mu} \right) \mu^2
#' }
#' \deqn{
#' \text{shape}_2 = \text{shape}_1 \left( \frac{1}{\mu} - 1 \right)
#' }
#'
#' @return A scalar log-likelihood (numeric).
#' @export
#'
#' @examples
#' calc_log_likelihood_beta(c(0.2, 0.6, 0.4), c(0.25, 0.55, 0.35))
calc_log_likelihood_beta <- function(observed,
                                     estimated,
                                     mean_precision = TRUE,
                                     verbose = TRUE) {

     # Remove NA pairs
     idx <- which(!is.na(observed) & !is.na(estimated))
     observed <- observed[idx]
     estimated <- estimated[idx]

     # Checks
     if (length(observed) != length(estimated)) {
          stop("observed and estimated must be the same length.")
     }
     if (any(observed <= 0 | observed >= 1)) {
          stop("observed must be strictly between 0 and 1.")
     }
     if (any(estimated <= 0 | estimated >= 1)) {
          stop("estimated must be strictly between 0 and 1.")
     }

     if (mean_precision) {
          # Method: mean–precision
          residuals <- observed - estimated
          sigma2 <- var(residuals)
          if (sigma2 <= 0) stop("Residual variance is non-positive — cannot estimate phi.")

          mu <- mean(observed)
          phi <- (mu * (1 - mu)) / sigma2 - 1
          if (phi <= 0) stop("Estimated phi must be > 0 — data may be too dispersed or flat.")

          shape_1 <- estimated * phi
          shape_2 <- (1 - estimated) * phi

          if (verbose) {
               message(sprintf("Mean–precision mode: estimated phi = %.2f", phi))
          }

     } else {
          # Standard (global) shape parameters
          mu <- mean(observed)
          sigma2 <- var(observed)
          shape_1 <- ((1 - mu) / sigma2 - 1 / mu) * mu^2
          shape_2 <- shape_1 * (1 / mu - 1)

          if (shape_1 <= 0 || shape_2 <= 0) {
               stop("Estimated shape parameters must be positive — check observed values.")
          }
          if (verbose) {
               message(sprintf("Standard shape mode: shape_1 = %.2f, shape_2 = %.2f", shape_1, shape_2))
          }

          # replicate for length of observed
          shape_1 <- rep(shape_1, length(observed))
          shape_2 <- rep(shape_2, length(observed))
     }

     ll <- sum(dbeta(observed, shape1 = shape_1, shape2 = shape_2, log = TRUE))

     if (verbose) {
          message(sprintf("Beta log-likelihood: %.2f", ll))
     }
     return(ll)
}
