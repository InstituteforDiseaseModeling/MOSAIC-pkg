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
#' @param n_points Integer minimum number of grid points over the support of \code{samples1} (default 1000); the grid is refined further when needed to resolve the P bandwidth.
#' @param eps Numeric floor applied to the Q density before taking logs (default 1e-10).
#'
#' @details
#' The KL divergence KL(P||Q) is calculated as:
#' \deqn{KL(P||Q) = \int p(x) \log(p(x) / q(x)) dx}
#'
#' where P represents the distribution from \code{samples1} and Q represents
#' the distribution from \code{samples2}.
#'
#' Both densities are weighted kernel density estimates whose bandwidths use
#' the weighted Silverman rule with the Kish effective sample size
#' \eqn{n_{eff} = (\sum w)^2 / \sum w^2}, so a concentrated weight vector
#' yields a narrow density even when the draws are spread out (with equal
#' weights this is \code{stats::bw.nrd0()}). The integral
#' \eqn{\int p \log(p/q)} is evaluated by the trapezoidal rule on a grid over
#' the support of P only (where the integrand is non-zero), with Q interpolated
#' from a full-range KDE, so the value does not level off at
#' \code{log(n_points)} when P is much narrower than Q.
#'
#' Before v0.99.11 both densities used unweighted bandwidths on one grid over
#' the pooled range and the densities were renormalised as discrete
#' probabilities, so a narrow P saturated near \code{log(n_points)}.
#'
#' Note that KL divergence is not symmetric: KL(P||Q) ≠ KL(Q||P).
#'
#' @return A non-negative numeric value representing the KL divergence.
#'   Returns 0 when the distributions are identical, and larger values
#'   indicate greater divergence. Returns \code{NA} with a warning when either
#'   weight vector puts all its mass on one value (the KDE bandwidth is then
#'   undefined).
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

     # Handle edge case where all samples are identical
     if (min(c(samples1, samples2)) == max(c(samples1, samples2))) {
          warning("All samples have the same value. Returning 0.")
          return(0)
     }

     kl_div <- .kl_divergence_kde(samples1, weights1, samples2, weights2,
                                  n_grid = n_points, q_floor = eps)

     if (!is.finite(kl_div)) {
          warning("KL divergence calculation resulted in non-finite value. Returning NA.")
          return(NA_real_)
     }

     kl_div
}

#' Weighted Silverman (nrd0) kernel bandwidth
#'
#' \code{stats::bw.nrd0()} with the weighted SD and weighted IQR in place of
#' the unweighted ones and the Kish effective sample size
#' \eqn{n_{eff} = 1/\sum w_i^2} (normalised weights) in place of \eqn{n}:
#' \eqn{0.9 \min(\hat\sigma_w, \mathrm{IQR}_w/1.34) n_{eff}^{-1/5}}. The
#' weighted variance is bias-corrected by \eqn{n_{eff}/(n_{eff}-1)}. With equal
#' weights it returns \code{stats::bw.nrd0(x)} exactly. Returns \code{NA_real_}
#' when the weighted spread is zero (all mass on one value).
#'
#' @param x Numeric finite values.
#' @param w Non-negative weights aligned with \code{x}, positive sum.
#' @return Numeric scalar bandwidth, or \code{NA_real_}.
#' @keywords internal
#' @noRd
.bw_nrd0_weighted <- function(x, w) {
     if (length(x) < 2L) return(NA_real_)
     w <- w / sum(w)
     if (max(w) - min(w) <= 1e-12 * max(w)) return(stats::bw.nrd0(x))
     n_eff <- 1 / sum(w^2)
     if (n_eff <= 1 + 1e-12) return(NA_real_)
     mu <- sum(w * x)
     sd_w <- sqrt(sum(w * (x - mu)^2) * n_eff / (n_eff - 1))
     q <- weighted_quantiles(x, w, c(0.25, 0.75))
     lo <- min(sd_w, (q[2] - q[1]) / 1.34)
     if (!is.finite(lo) || lo <= 0) lo <- sd_w
     if (!is.finite(lo) || lo <= 0) return(NA_real_)
     0.9 * lo * n_eff^(-0.2)
}

#' KL(P || Q) from two weighted samples by kernel density estimation
#'
#' Shared core of \code{calc_kl_divergence()} and the posterior-quantile KL.
#' Both KDEs use \code{.bw_nrd0_weighted()}, so a concentrated weight vector
#' gives a narrow density even when the draws themselves are spread out. The
#' integrand \eqn{p \log(p/q)} vanishes where \eqn{p = 0}, so it is integrated
#' by the trapezoidal rule on a grid over P's own support (draws holding
#' non-negligible weight, \eqn{\pm 4} bandwidths), sized to resolve P's
#' bandwidth (at least \code{n_grid}, at most \eqn{2^{16}} points); \eqn{q} is
#' interpolated from a full-range KDE resolving Q's bandwidth. A single grid
#' over the pooled range levels off near \code{log(n_grid)} once P is narrower
#' than one grid cell.
#'
#' @param p_x,q_x Numeric finite samples of P and Q.
#' @param p_w,q_w Non-negative weights aligned with the samples, positive sum.
#' @param n_grid Minimum number of grid points over P's support.
#' @param q_floor Floor applied to the Q density before taking logs.
#' @return Non-negative numeric scalar, or \code{NA_real_} when a bandwidth is
#'   undefined (all weight on one value) or the result is not finite.
#' @keywords internal
#' @noRd
.kl_divergence_kde <- function(p_x, p_w, q_x, q_w, n_grid = 1024L,
                               q_floor = .Machine$double.xmin) {
     p_w <- p_w / sum(p_w)
     q_w <- q_w / sum(q_w)
     bw_p <- .bw_nrd0_weighted(p_x, p_w)
     bw_q <- .bw_nrd0_weighted(q_x, q_w)
     if (!is.finite(bw_p) || !is.finite(bw_q) || bw_p <= 0 || bw_q <= 0) {
          return(NA_real_)
     }
     n_cap <- 2^16

     # P on its own support: draws whose weight could move the density
     sig <- p_w > 1e-10 * max(p_w)
     lo <- min(p_x[sig]) - 4 * bw_p
     hi <- max(p_x[sig]) + 4 * bw_p
     n_p <- min(n_cap, max(as.integer(n_grid),
                           2^ceiling(log2(8 * (hi - lo) / bw_p))))
     p_dens <- suppressWarnings(stats::density(p_x, weights = p_w, bw = bw_p,
                                               from = lo, to = hi, n = n_p))

     # Q: full-range KDE whose grid resolves Q's bandwidth
     q_lo <- min(q_x, lo) - 4 * bw_q
     q_hi <- max(q_x, hi) + 4 * bw_q
     n_q <- min(n_cap, max(1024, 2^ceiling(log2(8 * (q_hi - q_lo) / bw_q))))
     q_dens <- suppressWarnings(stats::density(q_x, weights = q_w, bw = bw_q,
                                               from = q_lo, to = q_hi, n = n_q))

     x <- p_dens$x
     dx <- diff(x)
     trapz <- function(f) sum(dx * (f[-1] + f[-length(f)]) / 2)
     p <- pmax(p_dens$y, 0)
     p <- p / trapz(p)
     q <- pmax(stats::approx(q_dens$x, q_dens$y, xout = x, rule = 2)$y, q_floor)
     integrand <- ifelse(p > 0, p * (log(p) - log(q)), 0)
     kl <- trapz(integrand)
     if (!is.finite(kl)) NA_real_ else max(kl, 0)
}

#' @rdname calc_kl_divergence
#' @export
calculate_kl_divergence <- function(samples1, weights1 = NULL, samples2, weights2 = NULL,
                                    n_points = 1000, eps = 1e-10) {
     .Deprecated("calc_kl_divergence")
     calc_kl_divergence(samples1, weights1, samples2, weights2, n_points, eps)
}
