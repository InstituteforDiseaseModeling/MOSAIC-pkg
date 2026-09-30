#' Fit Gompertz Distribution from Mode and Probability Interval
#'
#' This function estimates the parameters of a Gompertz distribution on [0, Inf)
#' with pdf f(x; b, eta) = b * eta * exp(b*x) * exp(-eta*(exp(b*x) - 1))
#' so that its quantiles at \code{probs} match a target interval (default: the
#' central 95 percent).
#'
#' The two quantiles determine the distribution: the ratio
#' \eqn{Q(p_2)/Q(p_1) = \log(1 + c_2/\eta) / \log(1 + c_1/\eta)}, with
#' \eqn{c_k = -\log(1 - p_k)}, depends on \eqn{\eta} alone and increases
#' monotonically from 1 (\eqn{\eta \to 0}) to \eqn{c_2/c_1} (\eqn{\eta \to \infty},
#' the exponential limit; about 146 for the central 95 percent), so \eqn{\eta} is
#' solved from the target ratio and \eqn{b} from the scale. A ratio beyond that
#' limit (or \code{ci_lower = 0}) is matched as closely as the family allows,
#' anchored on \code{ci_upper}.
#'
#' Setting the derivative of log f to zero gives the mode x* = -log(eta) / b,
#' which is interior only when eta < 1; for eta >= 1 the density is monotone
#' decreasing and the mode is 0. \code{mode_val} does not constrain the fit (a
#' sample-based mode near zero is poorly determined) and may lie outside the
#' interval, as it does for a monotone-decreasing target whose KDE mode falls
#' below the lower quantile; the mode of the fitted density is returned as
#' \code{fitted_mode}.
#'
#' @param mode_val Numeric >= 0. Reference mode, reported next to the fitted mode; it does not constrain the fit and need not lie inside the interval.
#' @param ci_lower Numeric greater than or equal to 0. Lower bound of the target interval (e.g., 2.5 percent quantile).
#' @param ci_upper Numeric greater than ci_lower. Upper bound of the target interval (e.g., 97.5 percent quantile).
#' @param probs Numeric length-2 vector in (0, 1). Probability levels for the target bounds. Defaults to c(0.025, 0.975).
#' @param verbose Logical. If TRUE, prints a diagnostic summary.
#'
#' @return A list containing:
#' \itemize{
#'   \item b: Gompertz shape parameter
#'   \item eta: Gompertz rate parameter
#'   \item f0: Density at zero (finite and positive)
#'   \item fitted_mode: The mode of the fitted density, -log(eta)/b (0 when eta >= 1)
#'   \item fitted_ci: Named vector of fitted quantiles at probs
#'   \item fitted_mean: Numerical estimate of the expected value via quadrature
#'   \item fitted_sd: Numerical estimate of the standard deviation via quadrature
#'   \item probs: The probability levels used
#'   \item input_mode: Echo of mode_val
#'   \item input_ci: Echo of c(lower = ci_lower, upper = ci_upper)
#' }
#'
#' @examples
#' # Example: Fit Gompertz for small positive quantity
#' result <- fit_gompertz_from_ci(
#'   mode_val = 1e-8,
#'   ci_lower = 1e-9,
#'   ci_upper = 1e-6,
#'   probs = c(0.025, 0.975)
#' )
#' print(result)
#'
#' @export
fit_gompertz_from_ci <- function(mode_val,
                                 ci_lower,
                                 ci_upper,
                                 probs    = c(0.025, 0.975),
                                 verbose  = FALSE) {

     # ---- validate inputs ----
     if (!is.numeric(mode_val) || length(mode_val) != 1L || !is.finite(mode_val) || mode_val < 0) {
          stop("`mode_val` must be a single finite numeric value >= 0.")
     }
     if (!is.numeric(ci_lower) || length(ci_lower) != 1L || !is.finite(ci_lower) || ci_lower < 0) {
          stop("`ci_lower` must be a single finite numeric value >= 0.")
     }
     if (!is.numeric(ci_upper) || length(ci_upper) != 1L || !is.finite(ci_upper) || ci_upper <= ci_lower) {
          stop("`ci_upper` must be a single finite numeric value > `ci_lower`.")
     }
     # Validate that mode is within confidence interval bounds
     if (!is.numeric(probs) || length(probs) != 2L || any(!is.finite(probs)) ||
         any(probs <= 0) || any(probs >= 1)) {
          stop("`probs` must be a numeric length-2 vector with values strictly between 0 and 1.")
     }
     # sort probs and targets together
     o <- order(probs)
     probs   <- probs[o]
     targets <- c(ci_lower, ci_upper)[o]


     # ---- internal helpers (no export) ----
     qgompertz_ <- function(p, b, eta) {
          # Q(p) = (1/b) * log(1 + (-log(1-p))/eta)
          (1 / b) * log1p((-log1p(-p)) / eta)
     }
     dgompertz_ <- function(x, b, eta) {
          ifelse(x < 0, 0, b * eta * exp(b * x) * exp(-eta * (exp(b * x) - 1)))
     }

     # Solve eta from the quantile ratio (a function of eta alone, increasing
     # from 1 to c2/c1), then b from the scale of the interval.
     c_p <- -log1p(-probs)
     ratio_of <- function(eta) log1p(c_p[2] / eta) / log1p(c_p[1] / eta)
     eta_max <- 1e8
     target_ratio <- if (targets[1] > 0) targets[2] / targets[1] else Inf
     if (target_ratio >= ratio_of(eta_max)) {
          eta_hat <- eta_max
          b_hat <- log1p(c_p[2] / eta_hat) / targets[2]
     } else if (target_ratio <= ratio_of(1e-12)) {
          eta_hat <- 1e-12
          b_hat <- log1p(c_p[1] / eta_hat) / targets[1]
     } else {
          root <- stats::uniroot(function(le) ratio_of(exp(le)) - target_ratio,
                                 interval = c(log(1e-12), log(eta_max)), tol = 1e-12)
          eta_hat <- exp(root$root)
          b_hat <- log1p(c_p[1] / eta_hat) / targets[1]
     }

     # ---- fitted summaries ----
     fitted_mode <- if (eta_hat < 1) -log(eta_hat) / b_hat else 0
     fitted_ci_vals <- qgompertz_(probs, b_hat, eta_hat)

     names(fitted_ci_vals) <- if (length(probs) == 2L) {
          c("lower", "upper")
     } else {
          paste0("p", probs)
     }

     # numerical mean & sd via quadrature up to near-1 quantile
     upper_q <- qgompertz_(0.999999, b_hat, eta_hat)
     m1 <- try(integrate(function(x) x * dgompertz_(x, b_hat, eta_hat),
                         lower = 0, upper = upper_q,
                         rel.tol = 1e-8, subdivisions = 1000L)$value, silent = TRUE)
     m2 <- try(integrate(function(x) x * x * dgompertz_(x, b_hat, eta_hat),
                         lower = 0, upper = upper_q,
                         rel.tol = 1e-8, subdivisions = 1000L)$value, silent = TRUE)
     fitted_mean <- if (inherits(m1, "try-error")) NA_real_ else m1
     fitted_var  <- if (inherits(m2, "try-error") || is.na(fitted_mean)) NA_real_ else max(m2 - fitted_mean^2, 0)
     fitted_sd   <- if (is.na(fitted_var)) NA_real_ else sqrt(fitted_var)

     out <- list(
          b            = b_hat,
          eta          = eta_hat,
          f0           = b_hat * eta_hat,
          fitted_mode  = fitted_mode,
          fitted_ci    = fitted_ci_vals,
          fitted_mean  = fitted_mean,
          fitted_sd    = fitted_sd,
          probs        = probs,
          input_mode   = mode_val,
          input_ci     = c(lower = ci_lower, upper = ci_upper)
     )

     if (verbose) {
          cat("\n=== Gompertz Distribution Fitting ===\n")
          cat(sprintf("Input: mode = %.10g, interval[%g, %g] = [%.10g, %.10g]\n",
                      mode_val, probs[1], probs[2], ci_lower, ci_upper))
          cat(sprintf("Fitted parameters: b = %.6g, eta = %.6g, f(0) = b*eta = %.6g\n",
                      out$b, out$eta, out$f0))
          cat(sprintf("Fitted mode: %.10g (reference mode_val: %.10g)\n",
                      out$fitted_mode, mode_val))
          cat(sprintf("Fitted %g%%-interval: [%.10g, %.10g]\n",
                      diff(probs) * 100, out$fitted_ci[1], out$fitted_ci[2]))
          cat(sprintf("Target  %g%%-interval: [%.10g, %.10g]\n",
                      diff(probs) * 100, ci_lower, ci_upper))
          if (is.finite(fitted_mean)) {
               cat(sprintf("Fitted mean: %.10g, SD: %.10g\n", out$fitted_mean, out$fitted_sd))
          } else {
               cat("Fitted mean/SD: NA (quadrature did not converge)\n")
          }
          cat("\n")
     }

     out
}

#' Generate Random Gompertz Variates
#'
#' Generate random variates from a Gompertz distribution with parameters b and eta.
#' Uses the inverse CDF method: Q(p) = (1/b) * log(1 + (-log(1-p))/eta)
#'
#' @param n Integer. Number of random variates to generate.
#' @param b Numeric. Shape parameter (b > 0).
#' @param eta Numeric. Rate parameter (eta > 0).
#'
#' @return Numeric vector of length n containing random Gompertz variates.
#'
#' @export
rgompertz <- function(n, b, eta) {
     # Input validation
     if (!is.numeric(n) || length(n) != 1L || n < 1 || n != floor(n)) {
          stop("`n` must be a positive integer.")
     }
     if (!is.numeric(b) || length(b) != 1L || !is.finite(b) || b <= 0) {
          stop("`b` must be a single finite positive numeric value.")
     }
     if (!is.numeric(eta) || length(eta) != 1L || !is.finite(eta) || eta <= 0) {
          stop("`eta` must be a single finite positive numeric value.")
     }

     # Generate uniform random variates
     u <- runif(n)

     # Apply inverse CDF transformation
     # Q(p) = (1/b) * log(1 + (-log(1-p))/eta)
     # Using log1p for numerical stability
     (1 / b) * log1p((-log1p(-u)) / eta)
}

#' Gompertz Distribution Density Function
#'
#' Compute the probability density function of a Gompertz distribution.
#'
#' @param x Numeric vector. Values at which to evaluate the density.
#' @param b Numeric. Shape parameter (b > 0).
#' @param eta Numeric. Rate parameter (eta > 0).
#' @param log Logical. If TRUE, return log density.
#'
#' @return Numeric vector of density values.
#'
#' @export
dgompertz <- function(x, b, eta, log = FALSE) {
     # Input validation
     if (!is.numeric(b) || length(b) != 1L || !is.finite(b) || b <= 0) {
          stop("`b` must be a single finite positive numeric value.")
     }
     if (!is.numeric(eta) || length(eta) != 1L || !is.finite(eta) || eta <= 0) {
          stop("`eta` must be a single finite positive numeric value.")
     }

     # Compute density
     # f(x; b, eta) = b * eta * exp(b*x) * exp(-eta*(exp(b*x) - 1))
     dens <- ifelse(x < 0, 0, b * eta * exp(b * x) * exp(-eta * (exp(b * x) - 1)))

     if (log) {
          return(log(dens))
     } else {
          return(dens)
     }
}

#' Gompertz Distribution Cumulative Distribution Function
#'
#' Compute the cumulative distribution function of a Gompertz distribution.
#'
#' @param q Numeric vector. Quantiles at which to evaluate the CDF.
#' @param b Numeric. Shape parameter (b > 0).
#' @param eta Numeric. Rate parameter (eta > 0).
#' @param lower.tail Logical. If TRUE, return P(X <= q), else P(X > q).
#' @param log.p Logical. If TRUE, return log probability.
#'
#' @return Numeric vector of probabilities.

#' @export
pgompertz <- function(q, b, eta, lower.tail = TRUE, log.p = FALSE) {
     # Input validation
     if (!is.numeric(b) || length(b) != 1L || !is.finite(b) || b <= 0) {
          stop("`b` must be a single finite positive numeric value.")
     }
     if (!is.numeric(eta) || length(eta) != 1L || !is.finite(eta) || eta <= 0) {
          stop("`eta` must be a single finite positive numeric value.")
     }

     # Compute CDF
     # F(x; b, eta) = 1 - exp(-eta * (exp(b*x) - 1))
     p <- ifelse(q < 0, 0, 1 - exp(-eta * (exp(b * q) - 1)))

     if (!lower.tail) {
          p <- 1 - p
     }

     if (log.p) {
          return(log(p))
     } else {
          return(p)
     }
}

#' Gompertz Distribution Quantile Function
#'
#' Compute the quantile function (inverse CDF) of a Gompertz distribution.
#'
#' @param p Numeric vector. Probabilities in \[0,1\].
#' @param b Numeric. Shape parameter (b > 0).
#' @param eta Numeric. Rate parameter (eta > 0).
#' @param lower.tail Logical. If TRUE, probabilities are P(X <= x).
#' @param log.p Logical. If TRUE, probabilities are given as log(p).
#'
#' @return Numeric vector of quantiles.
#'
#' @export
qgompertz <- function(p, b, eta, lower.tail = TRUE, log.p = FALSE) {
     # Input validation
     if (!is.numeric(b) || length(b) != 1L || !is.finite(b) || b <= 0) {
          stop("`b` must be a single finite positive numeric value.")
     }
     if (!is.numeric(eta) || length(eta) != 1L || !is.finite(eta) || eta <= 0) {
          stop("`eta` must be a single finite positive numeric value.")
     }

     if (log.p) {
          p <- exp(p)
     }

     if (!lower.tail) {
          p <- 1 - p
     }

     # Validate probabilities
     if (any(p < 0 | p > 1, na.rm = TRUE)) {
          stop("Probabilities must be in [0, 1].")
     }

     # Compute quantiles
     # Q(p) = (1/b) * log(1 + (-log(1-p))/eta)
     # Using log1p for numerical stability
     ifelse(p == 0, 0,
            ifelse(p == 1, Inf,
                   (1 / b) * log1p((-log1p(-p)) / eta)))
}
