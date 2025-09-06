###############################################################################
## calc_log_likelihood_beta.R
###############################################################################

#' Calculate log-likelihood for Beta-distributed proportions
#'
#' Computes the total log-likelihood for proportion data under the Beta distribution.
#' Supports either the mean–precision parameterization (default) or the standard
#' shape parameterization. In both cases, a single global set of shape parameters is
#' estimated from the data using the method of moments. Each observation can be
#' weighted via \code{weights}.
#'
#' @param observed Numeric vector of observed values in (0, 1).
#' @param estimated Numeric vector of model-predicted values in (0, 1).
#' @param mean_precision Logical; if \code{TRUE} (default), use mean–precision parameterization.
#'                       If \code{FALSE}, estimate shape parameters directly from the observed vector.
#' @param weights Optional numeric vector of non-negative weights, same length as \code{observed}.
#'                Default is \code{NULL}, which sets all weights to 1.
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
#' When \code{mean_precision = FALSE}, a single global \eqn{\text{shape}_1}, \eqn{\text{shape}_2}
#' is estimated from the observed vector:
#' \deqn{
#' \text{shape}_1 = \left( \frac{1 - \mu}{\sigma^2} - \frac{1}{\mu} \right) \mu^2
#' }
#' \deqn{
#' \text{shape}_2 = \text{shape}_1 \left( \frac{1}{\mu} - 1 \right)
#' }
#'
#' The total log-likelihood is then summed across observations, each multiplied by \code{weights[i]}.
#'
#' @return A scalar log-likelihood (numeric).
#' @export
#'
#' @examples
#' calc_log_likelihood_beta(c(0.2, 0.6, 0.4), c(0.25, 0.55, 0.35))
calc_log_likelihood_beta <- function(observed,
                                     estimated,
                                     mean_precision = TRUE,
                                     weights = NULL,
                                     verbose = TRUE) {

     if (length(observed) != length(estimated)) {
          stop("Lengths of observed and estimated must match.")
     }

     # Default weights if NULL
     if (is.null(weights)) {
          weights <- rep(1, length(observed))
     }

     # Remove NA triplets
     idx <- which(!is.na(observed) & !is.na(estimated) & !is.na(weights))
     observed  <- observed[idx]
     estimated <- estimated[idx]
     weights   <- weights[idx]

     # Handle empty input after NA removal
     if (length(observed) == 0 || length(estimated) == 0 || length(weights) == 0) {
          if (verbose) message("No usable data (all NA) — returning NA for log-likelihood.")
          return(NA_real_)
     }

     # Check lengths
     n <- length(observed)
     if (length(estimated) != n || length(weights) != n) {
          stop("Lengths of observed, estimated, and weights must all match.")
     }

     # Check weights
     if (any(weights < 0)) {
          stop("All weights must be >= 0.")
     }
     if (sum(weights) == 0) {
          stop("All weights are zero, cannot compute likelihood.")
     }

     # Beta domain checks
     if (any(observed <= 0 | observed >= 1)) {
          stop("observed must be strictly between 0 and 1 for Beta distribution.")
     }
     if (any(estimated <= 0 | estimated >= 1)) {
          stop("estimated must be strictly between 0 and 1 for Beta distribution.")
     }

     # Parameter estimation
     if (mean_precision) {
          residuals <- observed - estimated
          sigma2 <- var(residuals)
          if (sigma2 <= 0) {
               stop("Residual variance is non-positive — cannot estimate phi.")
          }

          mu <- mean(observed)
          phi <- (mu * (1 - mu)) / sigma2 - 1
          if (phi <= 0) {
               stop("Estimated phi must be > 0 — data may be too dispersed or flat.")
          }

          shape_1 <- estimated * phi
          shape_2 <- (1 - estimated) * phi

          if (verbose) {
               message(sprintf("Mean–precision mode: estimated phi = %.2f", phi))
          }

     } else {
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

          # replicate for each observation
          shape_1 <- rep(shape_1, n)
          shape_2 <- rep(shape_2, n)
     }

     # Weighted likelihood
     ll_vec <- dbeta(observed, shape1 = shape_1, shape2 = shape_2, log = TRUE)
     ll <- sum(weights * ll_vec)

     if (verbose) {
          message(sprintf("Beta log-likelihood: %.2f", ll))
     }
     return(ll)
}


###############################################################################
## calc_log_likelihood_binomial.R
###############################################################################

#' Calculate log-likelihood for Binomial-distributed data
#'
#' Computes the total log-likelihood for integer counts of successes under the Binomial distribution,
#' optionally weighting each observation via \code{weights}.
#'
#' @param observed Integer vector of observed successes (e.g., number of positives).
#' @param estimated Numeric vector of expected probabilities of success in (0, 1).
#' @param trials Integer vector of total trials (same length as \code{observed}).
#' @param weights Optional numeric vector of non-negative weights, same length as \code{observed}.
#'                Default is \code{NULL}, which sets all weights to 1.
#' @param verbose Logical; if \code{TRUE}, prints total log-likelihood and checks for data consistency.
#'
#' @details
#' The Binomial distribution models the number of successes in fixed trials:
#' \deqn{
#' Y \\sim \\text{Binomial}(n, p).
#' }
#' Weighted by \code{weights[i]} for each observation \code{i}.
#'
#' @return A scalar log-likelihood (numeric).
#' @export
#'
#' @examples
#' calc_log_likelihood_binomial(c(3, 4, 2), c(0.3, 0.5, 0.25), c(10, 10, 8))
calc_log_likelihood_binomial <- function(observed,
                                         estimated,
                                         trials,
                                         weights = NULL,
                                         verbose = TRUE) {

     if (length(observed) != length(estimated)) {
          stop("Lengths of observed and estimated must match.")
     }

     if (is.null(weights)) {
          weights <- rep(1, length(observed))
     }

     # Remove NA quadruples
     idx <- which(!is.na(observed) & !is.na(estimated) &
                       !is.na(trials)  & !is.na(weights))
     observed  <- observed[idx]
     estimated <- estimated[idx]
     trials    <- trials[idx]
     weights   <- weights[idx]

     # Handle empty input after NA removal
     if (length(observed) == 0 || length(estimated) == 0 || length(weights) == 0) {
          if (verbose) message("No usable data (all NA) — returning NA for log-likelihood.")
          return(NA_real_)
     }

     n <- length(observed)
     if (length(estimated) != n || length(trials) != n || length(weights) != n) {
          stop("Lengths of observed, estimated, trials, and weights must all match.")
     }

     # Weights checks
     if (any(weights < 0)) {
          stop("All weights must be >= 0.")
     }
     if (sum(weights) == 0) {
          stop("All weights are zero, cannot compute likelihood.")
     }

     # Domain checks
     if (any(observed < 0 | observed > trials | observed %% 1 != 0)) {
          stop("observed must be integer counts between 0 and trials.")
     }
     if (any(trials < 1 | trials %% 1 != 0)) {
          stop("trials must be positive integers.")
     }
     if (any(estimated <= 0 | estimated >= 1)) {
          stop("estimated probabilities must be in (0, 1).")
     }

     ll_vec <- dbinom(observed, size = trials, prob = estimated, log = TRUE)
     ll <- sum(weights * ll_vec)

     if (verbose) {
          message(sprintf("Binomial log-likelihood: %.2f", ll))
     }
     return(ll)
}


###############################################################################
## calc_log_likelihood_gamma.R
###############################################################################

#' Calculate log-likelihood for Gamma-distributed data
#'
#' Computes the total log-likelihood for continuous positive data under the Gamma distribution.
#' The shape parameter is estimated via the method of moments. Each observation can be weighted.
#'
#' @param observed Numeric vector of observed positive values.
#' @param estimated Numeric vector of expected means from the model (same length as \code{observed}).
#' @param weights Optional numeric vector of non-negative weights, same length as \code{observed}.
#'                Default is \code{NULL}, which sets all weights to 1.
#' @param verbose Logical; if \code{TRUE}, prints estimated shape parameter and total log-likelihood.
#'
#' @details
#' Weighted log-likelihood is summed over each observation. The shape \eqn{\\alpha} is estimated
#' via method of moments from \code{observed}.
#'
#' @return A scalar representing the total log-likelihood (numeric).
#' @export
#'
#' @examples
#' calc_log_likelihood_gamma(c(2.5, 3.2, 1.8), c(2.4, 3.0, 2.0))
calc_log_likelihood_gamma <- function(observed,
                                      estimated,
                                      weights = NULL,
                                      verbose = TRUE) {

     if (length(observed) != length(estimated)) {
          stop("Lengths of observed and estimated must match.")
     }

     if (is.null(weights)) {
          weights <- rep(1, length(observed))
     }

     # Remove NA
     idx <- which(!is.na(observed) & !is.na(estimated) & !is.na(weights))
     observed  <- observed[idx]
     estimated <- estimated[idx]
     weights   <- weights[idx]

     # Handle empty input after NA removal
     if (length(observed) == 0 || length(estimated) == 0 || length(weights) == 0) {
          if (verbose) message("No usable data (all NA) — returning NA for log-likelihood.")
          return(NA_real_)
     }

     n <- length(observed)
     if (length(estimated) != n || length(weights) != n) {
          stop("Lengths of observed, estimated, and weights must all match.")
     }

     # Weights checks
     if (any(weights < 0)) {
          stop("All weights must be >= 0.")
     }
     if (sum(weights) == 0) {
          stop("All weights are zero, cannot compute likelihood.")
     }

     # Domain checks
     if (any(observed <= 0)) stop("All observed values must be strictly positive.")
     if (any(estimated <= 0)) stop("All estimated values must be strictly positive.")

     mu <- mean(observed)
     s2 <- var(observed)
     shape <- mu^2 / s2
     scale <- estimated / shape

     if (verbose) {
          message(sprintf("Gamma shape (α) = %.2f", shape))
     }

     ll_vec <- dgamma(observed, shape = shape, scale = scale, log = TRUE)
     ll <- sum(weights * ll_vec)

     if (verbose) {
          message(sprintf("Gamma log-likelihood: %.2f", ll))
     }
     return(ll)
}




###############################################################################
## calc_log_likelihood_negbin.R  (patched: adds k_min)
###############################################################################

#' Calculate log-likelihood for Negative Binomial-distributed count data
#'
#' Computes the total log-likelihood for count data under the Negative Binomial
#' distribution, using the gamma-function formulation. Each observation can be
#' weighted.
#'
#' @param observed Integer vector of observed non-negative counts (e.g., cases, deaths).
#' @param estimated Numeric vector of expected values from the model (same length as \code{observed}).
#' @param k Numeric scalar; dispersion parameter. If \code{NULL}, it is estimated via method of moments.
#' @param k_min Numeric scalar; minimum dispersion floor applied when \code{k} is finite
#'   (either supplied or estimated). Default \code{3}. If \code{k = Inf} (Poisson limit),
#'   no flooring is applied.
#' @param weights Optional numeric vector of non-negative weights, same length as \code{observed}.
#'                Default is \code{NULL}, which sets all weights to 1.
#' @param verbose Logical; if \code{TRUE}, prints diagnostics including the (floored) dispersion and total log-likelihood.
#'
#' @details
#' If \code{k} is not supplied, it is estimated as \eqn{k = \bar x^2 / (s^2 - \bar x)} from
#' \code{observed}. When this estimate is finite, it is constrained to be at least \code{k_min}.
#' If \code{s^2 \le \bar x}, the function uses the Poisson limit (\code{k = Inf}).
#'
#' @return A scalar representing the total log-likelihood (numeric).
#' @export
#'
#' @examples
#' # Default k_min = 3
#' calc_log_likelihood_negbin(c(0, 5, 9), c(3, 4, 5))
#' # Provide k but allow flooring if too small
#' calc_log_likelihood_negbin(c(0, 5, 9), c(3, 4, 5), k = 1.2)
#'

calc_log_likelihood_negbin <- function(observed,
                                       estimated,
                                       k       = NULL,
                                       k_min   = 3,
                                       weights = NULL,
                                       verbose = TRUE) {

     if (length(observed) != length(estimated)) {
          stop("Lengths of observed and estimated must match.")
     }

     if (is.null(weights)) weights <- rep(1, length(observed))

     # Keep only finite observed/estimated/weights
     idx <- which(!is.na(observed) & !is.na(estimated) & !is.na(weights))
     observed  <- observed[idx]
     estimated <- estimated[idx]
     weights   <- weights[idx]

     # Cushion for zero/negative predictions
     estimated[estimated <= 0] <- .Machine$double.eps

     # Handle empty after filtering
     if (length(observed) == 0 || length(estimated) == 0 || length(weights) == 0) {
          if (verbose) message("No usable data (all NA) — returning NA for log-likelihood.")
          return(NA_real_)
     }

     n <- length(observed)
     if (length(estimated) != n || length(weights) != n) {
          stop("Lengths of observed, estimated, and weights must all match.")
     }
     if (any(weights < 0)) stop("All weights must be >= 0.")
     if (sum(weights) == 0) stop("All weights are zero, cannot compute likelihood.")
     if (any(observed < 0 | observed %% 1 != 0)) stop("observed must contain non-negative integer counts.")

     # Estimate k if not supplied
     if (is.null(k)) {
          mu <- mean(observed, na.rm = TRUE)
          s2 <- var(observed,  na.rm = TRUE)
          if (!is.finite(mu) || !is.finite(s2) || mu <= 0 || s2 <= mu) {
               k <- Inf
               if (verbose) message(sprintf("Var = %.2f <= Mean = %.2f: using Poisson (k = Inf)", s2, mu))
          } else {
               k <- mu^2 / (s2 - mu)
               if (verbose) message(sprintf("Estimated k = %.3f (from Var = %.3f, Mean = %.3f)", k, s2, mu))
          }
     } else {
          if (verbose) message(sprintf("Using provided k = %.3f", k))
     }

     # Apply minimum k floor when k is finite
     if (is.finite(k) && k < k_min) {
          if (verbose) message(sprintf("k = %.3f < k_min = %.3f; using k_min.", k, k_min))
          k <- k_min
     }

     # Compute weighted log-likelihood
     if (is.infinite(k)) {
          # Poisson limit
          ll_vec <- observed * log(estimated) - estimated - lgamma(observed + 1)
     } else {
          ll_vec <- lgamma(observed + k) - lgamma(k) - lgamma(observed + 1) +
               k * log(k / (k + estimated)) +
               observed * log(estimated / (k + estimated))
     }

     ll <- sum(weights * ll_vec)

     if (verbose) {
          msg_k <- if (is.infinite(k)) "Inf (Poisson)" else sprintf("%.3f", k)
          message(sprintf("Negative Binomial log-likelihood (k=%s): %.2f", msg_k, ll))
     }

     return(ll)
}





###############################################################################
## calc_log_likelihood_normal.R
###############################################################################

#' Calculate log-likelihood for Normally-distributed continuous data
#'
#' Computes the total log-likelihood for continuous data under the Normal distribution.
#' Also performs the Shapiro-Wilk test to check normality of residuals. Each observation
#' can be weighted via \code{weights}.
#'
#' @param observed Numeric vector of observed continuous values.
#' @param estimated Numeric vector of model-predicted means.
#' @param weights Optional numeric vector of non-negative weights, same length as \code{observed}.
#'                Default is \code{NULL}, which sets all weights to 1.
#' @param verbose Logical; if \code{TRUE}, prints estimated sigma, Shapiro-Wilk p-value, and log-likelihood.
#'
#' @details
#' Weighted log-likelihood is computed across all observations, and the residuals are tested
#' for normality via the Shapiro-Wilk test:
#' \itemize{
#'   \item If \code{p < 0.05}, a warning is issued indicating non-normal residuals.
#'   \item Otherwise, if \code{verbose = TRUE}, a message is printed showing the p-value.
#' }
#'
#' @return A single numeric value representing the total log-likelihood.
#' @export
#'
#' @examples
#' ll <- calc_log_likelihood_normal(c(1.2, 2.8, 3.1), c(1.0, 3.0, 3.2))
#' print(ll)
#'

calc_log_likelihood_normal <- function(observed,
                                       estimated,
                                       weights = NULL,
                                       verbose = TRUE) {

     if (length(observed) != length(estimated)) {
          stop("Lengths of observed and estimated must match.")
     }

     # Default weights = all 1
     if (is.null(weights)) {
          weights <- rep(1, length(observed))
     }

     # Remove NA across all three vectors
     idx <- which(!is.na(observed) & !is.na(estimated) & !is.na(weights))
     observed  <- observed[idx]
     estimated <- estimated[idx]
     weights   <- weights[idx]

     # Handle empty input after NA removal
     if (length(observed) == 0 || length(estimated) == 0 || length(weights) == 0) {
          if (verbose) message("No usable data (all NA) — returning NA for log-likelihood.")
          return(NA_real_)
     }

     n <- length(observed)
     if (length(estimated) != n || length(weights) != n) {
          stop("Lengths of observed, estimated, and weights must all match.")
     }
     if (n < 3) {
          stop("At least 3 non-missing observations are required for Normal likelihood.")
     }

     # Check weights
     if (any(weights < 0)) {
          stop("All weights must be >= 0.")
     }
     if (sum(weights) == 0) {
          stop("All weights are zero, cannot compute likelihood.")
     }

     # Estimate residual SD
     residuals <- observed - estimated
     sigma <- sd(residuals)
     if (sigma <= 0) {
          stop("Standard deviation of residuals is non-positive.")
     }

     # Shapiro-Wilk normality check
     if (n <= 5000) {
          sw <- shapiro.test(residuals)
          shapiro_p <- sw$p.value
          if (shapiro_p < 0.05) {
               warning(sprintf("Shapiro-Wilk p = %.4f: residuals deviate from normality (p < 0.05).",
                               shapiro_p))
          } else if (verbose) {
               message(sprintf("Shapiro-Wilk p = %.4f: residuals are consistent with normality.",
                               shapiro_p))
          }
     }

     # Weighted log-likelihood
     ll_vec <- dnorm(observed, mean = estimated, sd = sigma, log = TRUE)
     ll <- sum(weights * ll_vec)

     if (verbose) {
          message(sprintf("Estimated σ = %.4f", sigma))
          message(sprintf("Normal log-likelihood: %.2f", ll))
     }

     return(ll)
}



###############################################################################
## calc_log_likelihood_poisson.R
###############################################################################

#' Calculate log-likelihood for Poisson-distributed count data
#'
#' Computes the total log-likelihood for integer count data under the Poisson distribution.
#' Each observation can be weighted via \code{weights}, defaulting to equal weights.
#'
#' @param observed Integer vector of observed non-negative counts (e.g., cases, deaths).
#' @param estimated Numeric vector of expected values from the model (same length as \code{observed}).
#' @param weights Optional numeric vector of non-negative weights, same length as \code{observed}.
#'                Default is \code{NULL}, which sets all weights to 1.
#' @param zero_buffer Logical; if \code{TRUE} (default), rounds observed values to integers and
#'                    adds small buffer to avoid zero estimates. If \code{FALSE}, enforces
#'                    strict integer requirements.
#' @param verbose Logical; if \code{TRUE}, prints diagnostics and total log-likelihood.
#'
#' @details
#' The Poisson distribution assumes that the variance equals the mean, \\( \\text{Var}(Y) = \\mu \\).
#' Weighted log-likelihood is summed across observations. If the variance/mean ratio is > 1.5,
#' a warning about possible overdispersion is issued.
#'
#' @return A scalar representing the total log-likelihood (numeric).
#' @export
#'
#' @examples
#' calc_log_likelihood_poisson(c(2, 3, 4), c(2.2, 2.9, 4.1))
#'

calc_log_likelihood_poisson <- function(observed,
                                        estimated,
                                        weights = NULL,
                                        zero_buffer = TRUE,
                                        verbose = TRUE) {

     if (length(observed) != length(estimated)) {
          stop("Lengths of observed and estimated must match.")
     }

     if (is.null(weights)) {
          weights <- rep(1, length(observed))
     }

     idx <- which(!is.na(observed) & !is.na(estimated) & !is.na(weights))
     observed  <- observed[idx]
     estimated <- estimated[idx]
     weights   <- weights[idx]

     # No cushion for Poisson - zero predictions should be properly penalized
     # If any estimated = 0 and corresponding observed > 0, return -Inf
     if (any(estimated <= 0 & observed > 0)) {
          if (verbose) {
               n_bad <- sum(estimated <= 0 & observed > 0)
               message(sprintf("Poisson: %d cases with zero predictions but positive observations - returning -Inf", n_bad))
          }
          return(-Inf)
     }

     # For cases where estimated = 0 and observed = 0, set estimated to small positive value
     estimated[estimated <= 0] <- .Machine$double.eps

     # Handle empty input after NA removal
     if (length(observed) == 0 || length(estimated) == 0 || length(weights) == 0) {
          if (verbose) message("No usable data (all NA) — returning NA for log-likelihood.")
          return(NA_real_)
     }

     n <- length(observed)
     if (length(estimated) != n || length(weights) != n) {
          stop("Lengths of observed, estimated, and weights must all match.")
     }

     # Weights checks
     if (any(weights < 0)) {
          stop("All weights must be >= 0.")
     }
     if (sum(weights) == 0) {
          stop("All weights are zero, cannot compute likelihood.")
     }

     # Apply zero buffer if requested
     if (zero_buffer) {
          # Round observed to integers and add small buffer to avoid zeros
          observed <- round(observed)
          estimated <- pmax(estimated, 1e-10)  # Avoid zero estimates
     } else {
          if (any(observed < 0 | observed %% 1 != 0)) {
               stop("observed must contain non-negative integer counts for Poisson.")
          }
     }

     if (n > 1) {
          mu <- mean(observed, na.rm = TRUE)
          s2 <- var(observed, na.rm = TRUE)

          if (is.na(mu) || mu == 0) {
               message("All observations are zero (or NA).")
          } else {
               disp_ratio <- s2 / mu
               if (disp_ratio > 1.5) {
                    warning(
                         sprintf(
                              "Var/Mean = %.2f suggests overdispersion. Consider Negative Binomial.",
                              disp_ratio
                         )
                    )
               }
          }
     }


     ll_vec <- observed * log(estimated) - estimated - lgamma(observed + 1)
     ll <- sum(weights * ll_vec)

     if (verbose) {
          message(sprintf("Poisson log-likelihood: %.2f", ll))
     }
     return(ll)
}
