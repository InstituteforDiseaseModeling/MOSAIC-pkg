#' Kullback-Leibler divergence of a weighted sample from a reference sample
#'
#' @description
#' Computes KL(P || Q), how far the distribution P of \code{samples1}
#' (weighted by \code{weights1}) has moved from the reference distribution Q of
#' \code{samples2} (weighted by \code{weights2}), in nats. In MOSAIC, P is a
#' parameter's posterior and Q its prior, both represented by draws. The
#' estimate is made on Q's own percentile scale, so it does not depend on the
#' parameter's units, on heavy prior tails, on integer values or on bounds.
#'
#' @param samples1 Numeric vector of samples from P (e.g. the posterior).
#' @param weights1 Non-negative weights for \code{samples1}, same length. If
#'   NULL, uniform weights are used.
#' @param samples2 Numeric vector of samples from Q (e.g. the prior).
#' @param weights2 Non-negative weights for \code{samples2}, same length. If
#'   NULL, uniform weights are used.
#' @param n_points Ignored since MOSAIC 1.4.0 (kept so existing calls work; a
#'   warning is issued when supplied).
#' @param eps Ignored since MOSAIC 1.4.0 (as \code{n_points}).
#' @param n_bins Number of equal-probability bins on Q's percentile scale.
#'   Default 20.
#' @param seed Integer seed of the random tie-breaking. The caller's RNG stream
#'   is restored afterwards. Default 1.
#'
#' @details
#' \strong{Percentile-scale (PIT) estimator.} Each draw of P is mapped to its
#' percentile under Q, \eqn{u = F_Q(x)}. Under Q itself \eqn{u} is uniform, so
#' the KL divergence is unchanged by this monotone map and becomes the
#' divergence of the distribution of \eqn{u} from U(0, 1). The unit interval is
#' cut into \eqn{B} = \code{n_bins} equal bins, \eqn{p_b} is the P-weight in
#' bin \eqn{b}, and
#' \deqn{\widehat{KL} = \max\left(0,\ \sum_b p_b \log(p_b B) - \frac{B - 1}{2\,ESS_P}\right),}
#' where \eqn{ESS_P = (\sum w)^2 / \sum w^2} is the Kish effective sample size
#' of \code{weights1}. The subtracted term is the leading finite-sample bias of
#' the plug-in divergence of a \eqn{B}-cell histogram (the Miller-Madow
#' correction), so an unmoved posterior scores about 0 rather than
#' \eqn{(B - 1)/(2 ESS_P)}.
#'
#' \strong{Percentiles.} When \code{samples1} and \code{samples2} are the same
#' draws (the posterior is a reweighting of the prior draws), the percentile of
#' a draw is the midpoint of its cumulative Q-weight after ordering by value,
#' ties in random order; with uniform \code{weights2} this is
#' \code{(rank(x, ties.method = "random") - 0.5) / n}. Otherwise it is the
#' randomised probability integral transform against Q's weighted empirical
#' CDF, \eqn{u = F_Q(x^-) + U\,[F_Q(x) - F_Q(x^-)]} with \eqn{U \sim U(0, 1)}.
#' Random tie-breaking spreads an integer-valued or point-mass parameter evenly
#' over the percentiles its atom occupies.
#'
#' \strong{Range.} Binning can only lose information, so the estimate is at
#' most \eqn{\log B} (3.0 nats at \eqn{B = 20}), reached when P falls into a
#' single bin (1/\eqn{B} of Q's mass); it ranks parameters by how much they
#' moved up to that ceiling. It is computed when \eqn{ESS_P \ge B}; below that
#' \code{NA} is returned with a warning. P-mass outside Q's range is counted in
#' the outer bins. Near \eqn{ESS_P = B} the correction leaves a positive bias
#' for an unmoved posterior (about 0.05 nats at \eqn{ESS_P = 40}), so MOSAIC
#' reports the descriptive bands (\code{.mosaic_kl_band()}) only from
#' \eqn{ESS_P \ge 100}.
#'
#' Before MOSAIC 1.4.0 both densities were weighted kernel density estimates
#' integrated on a grid. That estimator failed for heavy-tailed priors
#' (bandwidth set by the tail), integer-valued parameters (spikes) and priors
#' bounded near the posterior (boundary bias), and was unbounded; values from
#' the two estimators are not comparable.
#'
#' Note that KL divergence is not symmetric: KL(P||Q) != KL(Q||P).
#'
#' @return A non-negative numeric value, at most \code{log(n_bins)}: about 0
#'   when P equals Q. \code{NA} with a warning when the Kish effective sample
#'   size of \code{weights1} is below \code{n_bins}; 0 with a warning when all
#'   samples are equal.
#'
#' @examples
#' # Posterior as a reweighting of prior draws
#' set.seed(123)
#' prior <- rnorm(20000)
#' w <- dnorm(prior, mean = 0.5, sd = 0.5) / dnorm(prior)   # target N(0.5, 0.5^2)
#' calc_kl_divergence(prior, w, prior, NULL)
#' log(1 / 0.5) + (0.5^2 + 0.5^2) / 2 - 0.5                  # analytic KL
#'
#' # Two separate samples
#' calc_kl_divergence(rnorm(5000, 1, 0.5), NULL, rnorm(5000), NULL)
#'
#' @export
calc_kl_divergence <- function(samples1,
                               weights1 = NULL,
                               samples2,
                               weights2 = NULL,
                               n_points = 1000,
                               eps = 1e-10,
                               n_bins = 20L,
                               seed = 1L) {

     if (!missing(n_points) || !missing(eps))
          warning("calc_kl_divergence(): n_points and eps are ignored since MOSAIC 1.4.0 ",
                  "(percentile-scale estimator).", call. = FALSE)

     if (!is.numeric(samples1) || !is.numeric(samples2)) {
          stop("samples1 and samples2 must be numeric vectors")
     }
     if (length(samples1) == 0 || length(samples2) == 0) {
          stop("samples1 and samples2 must not be empty")
     }
     if (length(samples1) < 2 || length(samples2) < 2) {
          stop("samples1 and samples2 must have at least 2 points")
     }
     if (any(!is.finite(samples1)) || any(!is.finite(samples2))) {
          stop("samples1 and samples2 must contain only finite values")
     }
     weights1 <- .mosaic_kl_weights(weights1, samples1, "weights1")
     weights2 <- .mosaic_kl_weights(weights2, samples2, "weights2")
     n_bins <- suppressWarnings(as.integer(n_bins))
     if (length(n_bins) != 1L || is.na(n_bins) || n_bins < 2L) {
          stop("n_bins must be an integer >= 2")
     }
     if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed)) {
          stop("seed must be a single finite number")
     }

     if (min(c(samples1, samples2)) == max(c(samples1, samples2))) {
          warning("All samples have the same value. Returning 0.")
          return(0)
     }

     ess <- 1 / sum(weights1^2)
     # Relative tolerance: n equal weights give 1 / sum(w^2) a few ulp below n.
     if (ess < n_bins * (1 - 1e-9)) {
          warning(sprintf(paste0("calc_kl_divergence(): Kish ESS of weights1 is %.1f, below n_bins = %d; ",
                                 "the percentile-scale estimate is unreliable, returning NA."),
                          ess, n_bins), call. = FALSE)
          return(NA_real_)
     }

     same <- length(samples1) == length(samples2) && all(samples1 == samples2)
     u <- .mosaic_with_fixed_seed(seed, function() {
          if (same) .mosaic_pit_same_draws(samples1, weights2)
          else .mosaic_pit_reference(samples1, samples2, weights2)
     })
     .mosaic_kl_pit(u, weights1, n_bins, ess)
}

#' Validate and normalise a KL weight vector (NULL means uniform)
#' @noRd
.mosaic_kl_weights <- function(w, x, nm) {
     if (is.null(w)) return(rep(1 / length(x), length(x)))
     if (!is.numeric(w)) stop(nm, " must be numeric or NULL")
     if (length(w) != length(x)) stop(nm, " must have the same length as samples", substr(nm, 8, 8))
     if (any(!is.finite(w))) stop(nm, " must be finite")
     if (any(w < 0)) stop(nm, " must be non-negative")
     if (sum(w) == 0) stop(nm, " must have non-zero sum")
     w / sum(w)
}

#' Prior percentiles of the prior's own draws
#'
#' Midpoint of each draw's cumulative reference weight after ordering by value,
#' ties in random order (\code{rank(ties.method = "random")}); with uniform
#' weights this is \code{(rank - 0.5) / n}. Draws the RNG: call inside
#' \code{.mosaic_with_fixed_seed()}.
#' @param x Numeric draws.
#' @param w Normalised reference weights aligned with \code{x}.
#' @return Numeric percentiles in (0, 1).
#' @noRd
.mosaic_pit_same_draws <- function(x, w) {
     o <- order(rank(x, ties.method = "random"))
     u <- numeric(length(x))
     u[o] <- cumsum(w[o]) - w[o] / 2
     u
}

#' Randomised probability integral transform against a weighted reference sample
#'
#' \eqn{u = F(x^-) + U [F(x) - F(x^-)]} with \eqn{F} the weighted empirical CDF
#' of \code{ref}. Draws the RNG: call inside \code{.mosaic_with_fixed_seed()}.
#' @param x Numeric points to transform.
#' @param ref Numeric reference draws.
#' @param w Normalised weights aligned with \code{ref}.
#' @return Numeric percentiles in the unit interval.
#' @noRd
.mosaic_pit_reference <- function(x, ref, w) {
     o <- order(ref)
     r <- ref[o]
     cw <- c(0, pmin(cumsum(w[o]), 1))
     below <- cw[findInterval(x, r, left.open = TRUE) + 1L]     # F(x-): weight strictly below x
     upto  <- cw[findInterval(x, r) + 1L]                       # F(x): weight at or below x
     below + stats::runif(length(x)) * (upto - below)
}

#' Binned KL of percentile-scale draws from U(0, 1), with the finite-ESS correction
#' @param u Percentiles in the unit interval.
#' @param w Normalised weights aligned with \code{u}.
#' @param n_bins Number of equal bins.
#' @param ess Kish ESS of \code{w}.
#' @return Non-negative numeric scalar.
#' @noRd
.mosaic_kl_pit <- function(u, w, n_bins, ess) {
     b <- pmin(pmax(ceiling(u * n_bins), 1L), n_bins)
     p <- vapply(seq_len(n_bins), function(k) sum(w[b == k]), numeric(1))
     p <- p / sum(p)
     kl <- sum(ifelse(p > 0, p * log(p * n_bins), 0))
     max(0, kl - (n_bins - 1) / (2 * ess))
}

#' Descriptive band of a prior-to-posterior KL divergence
#'
#' "largely prior" below \code{edges[1]}, "partly informed" below
#' \code{edges[2]}, "data-informed" from \code{edges[2]}. The bands are
#' suppressed (\code{NA}) when the Kish ESS behind the posterior is below
#' \code{ess_min}: the finite-ESS correction leaves a null bias of about 0.05
#' nats at ESS 40, so a quarter of unmoved parameters would land in "partly
#' informed" (red-team round 1, T12).
#' @param kl Numeric vector of KL values (nats).
#' @param ess Kish ESS of the posterior weights the KL values were computed
#'   with (the ensemble ESS, \code{ess_best}); a single number.
#' @param edges The two band edges. Default \code{c(0.05, 0.5)}.
#' @param ess_min Smallest ESS for which bands are given. Default 100.
#' @return Character vector parallel to \code{kl} (\code{NA} where \code{kl}
#'   is \code{NA} or the ESS is below \code{ess_min} or unknown).
#' @noRd
.mosaic_kl_band <- function(kl, ess, edges = c(0.05, 0.5), ess_min = 100) {
     kl <- suppressWarnings(as.numeric(kl))
     ess <- suppressWarnings(as.numeric(unlist(ess)))
     out <- rep(NA_character_, length(kl))
     if (length(ess) != 1L || !is.finite(ess) || ess < ess_min) return(out)
     edges <- sort(as.numeric(edges))
     ok <- is.finite(kl)
     out[ok] <- ifelse(kl[ok] < edges[1], "largely prior",
                       ifelse(kl[ok] < edges[2], "partly informed", "data-informed"))
     out
}

#' @rdname calc_kl_divergence
#' @export
calculate_kl_divergence <- function(samples1, weights1 = NULL, samples2, weights2 = NULL,
                                    n_points = 1000, eps = 1e-10) {
     .Deprecated("calc_kl_divergence")
     calc_kl_divergence(samples1, weights1, samples2, weights2)
}
