#' Exact importance-sampling diagnostics for BFRS draws
#'
#' Reports the importance-sampling (IS) quality of a set of scored draws
#' WITHOUT the \eqn{\Delta}AIC truncation that \code{run_MOSAIC()} applies when
#' it builds the best-subset posterior. Because MOSAIC draws parameters from the
#' prior and scores them with the likelihood, the (unnormalised) importance
#' ratio of draw \eqn{i} is \eqn{r_i \propto \mathcal{L}(\Theta^{(i)})}, so the
#' ratios follow directly from the log-likelihoods.
#'
#' Two numbers are returned:
#'
#' \describe{
#'   \item{\code{ess_is}}{the exact IS effective sample size. This is the
#'     quantity that says whether the importance sampler has actually explored
#'     the posterior. It is deliberately NOT the \code{ESS_B} reported by the
#'     convergence gate, which is computed on truncated weights and is therefore
#'     bounded away from its worst case by construction.}
#'   \item{\code{khat}}{the Pareto \eqn{\hat k} shape statistic of PSIS
#'     (Vehtari et al. 2024), estimated with the empirical-Bayes profile
#'     likelihood of Zhang & Stephens (2009). \eqn{\hat k < 0.5} indicates
#'     finite IS variance; \eqn{0.5 \le \hat k < 0.7} is marginal;
#'     \eqn{\hat k \ge 0.7} means the IS estimate is unreliable.}
#' }
#'
#' In BFRS runs at production scale the likelihood is evaluated over
#' \eqn{O(10^5)} observations, so \eqn{\Delta}AIC values across prior draws are
#' routinely \eqn{O(10^6)}. The raw ratios then underflow to zero for all but a
#' handful of draws. That is not a numerical defect to be worked around -- it is
#' the finding -- so the degenerate case is reported explicitly via
#' \code{khat_status} rather than silently smoothed.
#'
#' @param log_lik Numeric vector of log-likelihoods, one per draw. Non-finite
#'   entries are dropped.
#' @param method Character; ESS estimator passed to \code{\link{calc_model_ess}}.
#'   One of \code{"kish"} (default) or \code{"perplexity"}.
#'
#' @return A named list with \code{n}, \code{ess_is}, \code{ess_is_prop},
#'   \code{khat}, \code{khat_status} and \code{n_positive_ratios}.
#'
#' @references
#' Vehtari A, Simpson D, Gelman A, Yao Y, Gabry J (2024). Pareto smoothed
#' importance sampling. \emph{JMLR} 25(72):1-58.
#'
#' Zhang J, Stephens MA (2009). A new and efficient estimation method for the
#' generalized Pareto distribution. \emph{Technometrics} 51(3):316-325.
#'
#' @examples
#' # A well-behaved sampler: ESS is a healthy fraction of n, khat is small
#' set.seed(1)
#' calc_is_diagnostics(rnorm(1000, sd = 0.5))
#'
#' @export
calc_is_diagnostics <- function(log_lik, method = c("kish", "perplexity")) {

     method  <- match.arg(method)
     log_lik <- as.numeric(log_lik)
     log_lik <- log_lik[is.finite(log_lik)]
     n       <- length(log_lik)

     if (n < 2L) {
          return(list(n = n, ess_is = NA_real_, ess_is_prop = NA_real_,
                      khat = NA_real_, khat_status = "insufficient draws",
                      n_positive_ratios = NA_integer_))
     }

     # Log importance ratios, shifted so the best draw sits at 0. The shift
     # cancels in the normalised weights and keeps exp() in range.
     log_r <- log_lik - max(log_lik)

     # Exact (untruncated) IS ESS. Computed via a softmax so it stays stable
     # when the ratios span hundreds of thousands of log units.
     w  <- exp(log_r)
     sw <- sum(w)
     ess_is <- if (is.finite(sw) && sw > 0) {
          calc_model_ess(w / sw, method = method)
     } else {
          NA_real_
     }

     n_pos <- sum(w > 0)

     khat <- .mosaic_pareto_khat(log_r)

     list(n                 = n,
          ess_is            = ess_is,
          ess_is_prop       = if (is.finite(ess_is)) ess_is / n else NA_real_,
          khat              = as.numeric(khat),
          khat_status       = attr(khat, "status"),
          n_positive_ratios = as.integer(n_pos))
}


#' Pareto k-hat for a vector of log importance ratios
#'
#' Fits a generalized Pareto distribution to the upper tail of the importance
#' ratios and returns its shape parameter. Returns \code{NA_real_} with a
#' \code{"status"} attribute when the tail carries too little information to
#' support a fit, which is the expected outcome when the ratios have collapsed
#' onto a single draw.
#'
#' @param log_r Numeric vector of log importance ratios (any additive shift).
#' @return Numeric scalar \eqn{\hat k}, with attribute \code{"status"}.
#' @keywords internal
#' @noRd
.mosaic_pareto_khat <- function(log_r) {

     bad <- function(msg) {
          out <- NA_real_
          attr(out, "status") <- msg
          out
     }

     log_r <- log_r[is.finite(log_r)]
     n     <- length(log_r)
     if (n < 20L) return(bad("insufficient draws for a tail fit"))

     # PSIS tail length (Vehtari et al. 2024 section 3).
     tail_len <- if (n > 225L) ceiling(3 * sqrt(n)) else ceiling(n / 5)
     tail_len <- min(tail_len, n - 1L)
     if (tail_len < 5L) return(bad("tail shorter than 5 draws"))

     ord    <- sort.int(log_r, method = "quick")
     cutoff <- ord[n - tail_len]
     # Exceedances on the natural scale, relative to the tail threshold.
     x <- exp(ord[(n - tail_len + 1L):n]) - exp(cutoff)
     x <- x[is.finite(x) & x > 0]

     # A tail that has collapsed to a single spike carries no shape information.
     if (length(x) < 5L)             return(bad("degenerate: importance ratios underflow to zero"))
     if (length(unique(x)) < 5L)     return(bad("degenerate: fewer than 5 distinct tail ratios"))

     x  <- sort.int(x, method = "quick")
     nx <- length(x)

     xstar <- x[max(1L, floor(nx / 4 + 0.5))]
     if (!is.finite(xstar) || xstar <= 0) {
          return(bad("degenerate: tail quartile is zero"))
     }

     prior <- 3
     m     <- 30L + floor(sqrt(nx))
     jj    <- seq_len(m)
     theta <- 1 / x[nx] + (1 - sqrt(m / (jj - 0.5))) / prior / xstar

     k_j <- vapply(theta, function(th) mean(log1p(-th * x)), numeric(1))
     ok  <- is.finite(k_j) & k_j != 0 & is.finite(theta) & (-theta / k_j) > 0
     if (!any(ok)) return(bad("profile likelihood undefined on the theta grid"))

     theta <- theta[ok]; k_j <- k_j[ok]
     l_theta <- nx * (log(-theta / k_j) - k_j - 1)
     w_theta <- exp(l_theta - .mosaic_logsumexp(l_theta))
     theta_hat <- sum(theta * w_theta)

     k <- mean(log1p(-theta_hat * x))

     # Weakly informative prior on k, as in Vehtari et al.: shrinks toward 0.5
     # with prior weight 10. Matters only for short tails.
     a <- 10
     k <- k * nx / (nx + a) + a * 0.5 / (nx + a)

     if (!is.finite(k)) return(bad("non-finite shape estimate"))
     attr(k, "status") <- "ok"
     k
}


#' Numerically stable log-sum-exp
#' @param x Numeric vector.
#' @return Numeric scalar.
#' @keywords internal
#' @noRd
.mosaic_logsumexp <- function(x) {
     x <- x[is.finite(x)]
     if (!length(x)) return(-Inf)
     m <- max(x)
     m + log(sum(exp(x - m)))
}


#' Empty IS-diagnostics record
#'
#' Placeholder used when the best subset is too small to score, so downstream
#' consumers always see the same field names.
#'
#' @return A named list matching \code{calc_is_diagnostics()}'s shape.
#' @keywords internal
#' @noRd
.mosaic_empty_is_diag <- function() {
     list(n = NA_integer_, ess_is = NA_real_, ess_is_prop = NA_real_,
          khat = NA_real_, khat_status = "not computed",
          n_positive_ratios = NA_integer_)
}
