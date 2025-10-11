#' Calculate Kullback-Leibler Divergence Between Two Distributions
#'
#' @description
#' Computes the Kullback-Leibler (KL) divergence between two probability
#' distributions represented by weighted samples. The KL divergence measures
#' how one probability distribution diverges from a reference distribution.
#'
#' @param samples1 Numeric vector of samples from the first distribution (P).
#' @param weights1 Numeric vector of weights for \code{samples1}. Must be the
#'   same length as \code{samples1}. If NULL, uniform weights are used.
#' @param samples2 Numeric vector of samples from the second distribution (Q).
#' @param weights2 Numeric vector of weights for \code{samples2}. Must be the
#'   same length as \code{samples2}. If NULL, uniform weights are used.
#' @param n_points Integer specifying the number of points for kernel density
#'   estimation. Higher values provide more accurate estimates but require more
#'   computation. Default is 1000.
#' @param eps Numeric value for numerical stability. Small positive value added
#'   to densities to avoid log(0). Default is 1e-10.
#'
#' @details
#' The KL divergence KL(P||Q) is calculated as:
#' \deqn{KL(P||Q) = \sum_i P(x_i) \log(P(x_i) / Q(x_i))}
#'
#' where P represents the distribution from \code{samples1} and Q represents
#' the distribution from \code{samples2}.
#'
#' The function uses weighted kernel density estimation to approximate the
#' continuous distributions from the discrete samples, then evaluates the
#' KL divergence using numerical integration.
#'
#' Note that KL divergence is not symmetric: KL(P||Q) ≠ KL(Q||P).
#'
#' @return A non-negative numeric value representing the KL divergence.
#'   Returns 0 when the distributions are identical, and larger values
#'   indicate greater divergence.
#'
#' @examples
#' # Example 1: Compare two normal distributions
#' set.seed(123)
#' samples1 <- rnorm(1000, mean = 0, sd = 1)
#' samples2 <- rnorm(1000, mean = 0.5, sd = 1.2)
#' kl_div <- calc_kl_divergence(samples1, NULL, samples2, NULL)
#' print(paste("KL divergence:", round(kl_div, 4)))
#'
#' # Example 2: Using weighted samples
#' samples1 <- rnorm(500)
#' weights1 <- runif(500, 0.5, 1.5)
#' samples2 <- rnorm(500, mean = 1)
#' weights2 <- runif(500, 0.5, 1.5)
#' kl_div_weighted <- calc_kl_divergence(samples1, weights1, samples2, weights2)
#'
#' # Example 3: Comparing posterior to prior in Bayesian analysis
#' # prior_samples <- rnorm(1000, mean = 0, sd = 2)  # Prior
#' # posterior_samples <- rnorm(1000, mean = 1, sd = 0.5)  # Posterior
#' # kl_div <- calc_kl_divergence(posterior_samples, NULL, prior_samples, NULL)
#'
#' @export
#'

calc_kl_divergence <- function(samples1,
                               weights1 = NULL,
                               samples2,
                               weights2 = NULL,
                               n_points = 1000,
                               eps = 1e-10) {

     # Input validation
     if (!is.numeric(samples1) || !is.numeric(samples2)) {
          stop("samples1 and samples2 must be numeric vectors")
     }

     if (length(samples1) == 0 || length(samples2) == 0) {
          stop("samples1 and samples2 must not be empty")
     }

     if (length(samples1) < 2 || length(samples2) < 2) {
          stop("samples1 and samples2 must have at least 2 points for density estimation")
     }

     if (any(!is.finite(samples1)) || any(!is.finite(samples2))) {
          stop("samples1 and samples2 must contain only finite values")
     }

     # Handle weights
     if (is.null(weights1)) {
          weights1 <- rep(1 / length(samples1), length(samples1))
     } else {
          if (!is.numeric(weights1)) {
               stop("weights1 must be numeric or NULL")
          }
          if (length(weights1) != length(samples1)) {
               stop("weights1 must have the same length as samples1")
          }
          if (any(weights1 < 0)) {
               stop("weights1 must be non-negative")
          }
          if (sum(weights1) == 0) {
               stop("weights1 must have non-zero sum")
          }
          # Normalize weights
          weights1 <- weights1 / sum(weights1)
     }

     if (is.null(weights2)) {
          weights2 <- rep(1 / length(samples2), length(samples2))
     } else {
          if (!is.numeric(weights2)) {
               stop("weights2 must be numeric or NULL")
          }
          if (length(weights2) != length(samples2)) {
               stop("weights2 must have the same length as samples2")
          }
          if (any(weights2 < 0)) {
               stop("weights2 must be non-negative")
          }
          if (sum(weights2) == 0) {
               stop("weights2 must have non-zero sum")
          }
          # Normalize weights
          weights2 <- weights2 / sum(weights2)
     }

     # Validate n_points
     if (!is.numeric(n_points) || length(n_points) != 1 || n_points <= 0) {
          stop("n_points must be a positive integer")
     }
     n_points <- as.integer(n_points)
     if (n_points < 2) {
          stop("n_points must be at least 2")
     }

     # Validate eps
     if (!is.numeric(eps) || length(eps) != 1 || eps <= 0) {
          stop("eps must be a positive numeric value")
     }

     # Get range for evaluation
     x_min <- min(c(samples1, samples2))
     x_max <- max(c(samples1, samples2))

     # Handle edge case where all samples are identical
     if (x_min == x_max) {
          warning("All samples have the same value. Returning 0.")
          return(0)
     }

     # Compute weighted kernel density estimates
     dens1 <- density(samples1, weights = weights1, from = x_min, to = x_max, n = n_points)
     dens2 <- density(samples2, weights = weights2, from = x_min, to = x_max, n = n_points)

     # Ensure densities are positive (add small epsilon for numerical stability)
     p <- pmax(dens1$y, eps)
     q <- pmax(dens2$y, eps)

     # Normalize to ensure they sum to 1 (treating as discrete probability distributions)
     p <- p / sum(p)
     q <- q / sum(q)

     # Calculate KL divergence: KL(P||Q) = sum(P * log(P/Q))
     kl_div <- sum(p * log(p / q))

     # Check for numerical issues
     if (!is.finite(kl_div)) {
          warning("KL divergence calculation resulted in non-finite value. Returning NA.")
          return(NA_real_)
     }

     # KL divergence should be non-negative (can be slightly negative due to numerical errors)
     if (kl_div < -eps) {
          warning(sprintf("KL divergence is negative (%g). Setting to 0.", kl_div))
          kl_div <- 0
     } else if (kl_div < 0) {
          kl_div <- 0
     }

     return(kl_div)
}
