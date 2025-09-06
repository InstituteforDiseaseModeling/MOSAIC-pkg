#' Calculate log-likelihood for Binomial-distributed data
#'
#' Computes the total log-likelihood for integer counts of successes under the Binomial distribution.
#' This is appropriate when the data represent successes out of known trials.
#'
#' @param observed Integer vector of observed successes (e.g., number of positives).
#' @param estimated Numeric vector of expected probabilities of success in (0, 1).
#' @param trials Integer vector of total trials (same length as \code{observed}).
#' @param verbose Logical; if \code{TRUE}, prints total log-likelihood and checks for data consistency.
#'
#' @details
#' The Binomial distribution models the number of successes in a fixed number of independent trials:
#' \deqn{
#' Y \\sim \\text{Binomial}(n, p).
#' }
#' The total log-likelihood is:
#' \deqn{
#' \\log \\mathcal{L}(\\mathbf{y} \\mid n, p) = \\sum_t \\left[ \\log \\binom{n_t}{y_t} + y_t \\log(p_t) + (n_t - y_t) \\log(1 - p_t) \\right].
#' }
#'
#' @return A scalar log-likelihood (numeric).
#' @export
#'
#' @examples
#' calc_log_likelihood_binomial(c(3, 4, 2), c(0.3, 0.5, 0.25), c(10, 10, 8))
calc_log_likelihood_binomial <- function(observed, estimated, trials, verbose = TRUE) {

     # Remove NA
     idx <- which(!is.na(observed) & !is.na(estimated) & !is.na(trials))
     observed <- observed[idx]
     estimated <- estimated[idx]
     trials <- trials[idx]

     # Checks
     if (length(observed) != length(estimated) || length(observed) != length(trials)) {
          stop("observed, estimated, and trials must all be the same length.")
     }
     if (any(observed < 0 | observed > trials | observed %% 1 != 0)) {
          stop("observed must be integer counts between 0 and trials.")
     }
     if (any(trials < 1 | trials %% 1 != 0)) {
          stop("trials must be positive integers.")
     }
     if (any(estimated <= 0 | estimated >= 1)) {
          stop("estimated probabilities must be in (0, 1).")
     }

     ll <- sum(dbinom(observed, size = trials, prob = estimated, log = TRUE))

     if (verbose) {
          message(sprintf("Binomial log-likelihood: %.2f", ll))
     }
     return(ll)
}
