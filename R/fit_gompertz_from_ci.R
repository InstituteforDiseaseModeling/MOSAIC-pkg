#' Fit Gompertz Distribution from Mode and Probability Interval
#'
#' This function estimates the parameters of a Gompertz distribution on [0, Inf)
#' with pdf f(x; b, eta) = b * eta * exp(b*x) * exp(-eta*(exp(b*x) - 1)),
#' given a target interior mode and a two-sided interval (default: central 95 percent).
#' The interior mode condition is enforced by eta = b * exp(b*mode), which holds
#' exactly for the Gompertz mode when eta > b.
#'
#' @param mode_val Numeric greater than 0. Target mode of the distribution (very near zero is allowed).
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
#'   \item fitted_mode: The implied mode (matches mode_val up to numeric error)
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
     if (mode_val < ci_lower || mode_val > ci_upper) {
          stop("`mode_val` must be between `ci_lower` and `ci_upper`.")
     }
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

     # SSE objective over log(b) to avoid numeric pathologies
     obj_t <- function(t) {
          b <- exp(t)
          # eta = b * exp(b * mode) enforces interior mode at mode_val
          # use expm1 for exp(b * mode) - 1 when small, but we need only exp(...)
          bm <- b * mode_val
          if (!is.finite(bm)) return(1e50)
          eta <- b * exp(bm)
          if (!is.finite(eta) || eta <= 0) return(1e50)

          # compute quantiles at probs
          # note: quantities remain well-behaved for very small/large b using log1p
          xhat <- (1 / b) * log1p((-log1p(-probs)) / eta)
          if (any(!is.finite(xhat))) return(1e50)

          sum((xhat - targets)^2)
     }

     # ---- optimize over log(b) ----
     opt <- optimize(f = obj_t, interval = c(-30, 30))
     t_hat <- opt$minimum
     b_hat <- exp(t_hat)
     eta_hat <- b_hat * exp(b_hat * mode_val)

     # ---- fitted summaries ----
     fitted_mode <- (1 / b_hat) * log(eta_hat / b_hat) # should equal mode_val numerically
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
          cat(sprintf("Fitted mode: %.10g (target: %.10g, diff: %.3g%%)\n",
                      out$fitted_mode, mode_val, 100 * (out$fitted_mode - mode_val) / mode_val))
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
