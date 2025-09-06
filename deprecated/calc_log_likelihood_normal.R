#' Calculate log-likelihood for Normally-distributed continuous data
#'
#' Computes the total log-likelihood for continuous data under the Normal distribution.
#' The standard deviation is estimated from residuals. Also performs the Shapiro-Wilk test
#' to assess normality of residuals.
#'
#' @param observed Numeric vector of observed continuous values.
#' @param estimated Numeric vector of model-predicted means.
#' @param verbose Logical; if \code{TRUE}, prints estimated sigma, Shapiro-Wilk p-value, and log-likelihood.
#'
#' @details
#' The residuals (\code{observed - estimated}) are checked for normality via the Shapiro-Wilk test.
#' If the p-value is below 0.05, a warning is issued, indicating the residuals deviate from normality.
#'
#' The total log-likelihood is computed as:
#' \\deqn{
#' \\log \\mathcal{L} = \\sum_t \\left[ -\\frac{1}{2} \\log(2\\pi) - \\log(\\sigma)\n#' - \\frac{(y_t - \\mu_t)^2}{2\\sigma^2} \\right].
#' }
#'
#' @return The function returns a list invisibly with components:\n#' \\item{\\code{log_likelihood}}{The total log-likelihood}\n#' \\item{\\code{sigma}}{The estimated residual standard deviation}\n#' \\item{\\code{shapiro_p}}{The Shapiro-Wilk test p-value (or NA if not applicable)}\n
#' @export
#'
#' @examples
#' res <- calc_log_likelihood_normal(c(1.2, 2.8, 3.1), c(1.0, 3.0, 3.2))
#' print(res$log_likelihood)
calc_log_likelihood_normal <- function(observed,
                                       estimated,
                                       verbose = TRUE) {

     # Remove NA
     idx <- which(!is.na(observed) & !is.na(estimated))
     observed <- observed[idx]
     estimated <- estimated[idx]

     if (length(observed) != length(estimated)) {
          stop("observed and estimated must be the same length.")
     }
     if (length(observed) < 3) {
          stop("At least 3 non-missing observations are required.")
     }

     residuals <- observed - estimated
     sigma <- sd(residuals)
     if (sigma <= 0) {
          stop("Standard deviation of residuals is non-positive.")
     }

     # Normality check
     shapiro_p <- NA
     if (length(residuals) >= 3 && length(residuals) <= 5000) {
          shw <- shapiro.test(residuals)
          shapiro_p <- shw$p.value
          if (shapiro_p < 0.05) {
               warning(sprintf(
                    "Shapiro-Wilk p = %.4f: residuals deviate from normality (p < 0.05).",
                    shapiro_p
               ))
          } else if (verbose) {
               message(sprintf("Shapiro-Wilk p = %.4f: residuals are consistent with normality.", shapiro_p))
          }
     }

     ll <- sum(dnorm(observed, mean = estimated, sd = sigma, log = TRUE))

     if (verbose) {
          message(sprintf("Estimated σ = %.4f", sigma))
          message(sprintf("Normal log-likelihood: %.2f", ll))
     }

     invisible(list(
          log_likelihood = ll,
          sigma = sigma,
          shapiro_p = shapiro_p
     ))
}
