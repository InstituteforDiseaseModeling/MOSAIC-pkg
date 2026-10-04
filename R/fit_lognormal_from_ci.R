#' Fit Lognormal Distribution from Mode and 95% Confidence Intervals
#'
#' This function calculates the meanlog and sdlog parameters of a lognormal
#' distribution that best matches a given mode and 95% confidence intervals.
#'
#' @param mode_val Numeric. The mode of the distribution.
#' @param ci_lower Numeric. The lower bound of the 95% confidence interval.
#' @param ci_upper Numeric. The upper bound of the 95% confidence interval.
#' @param method Character. Method to use: "moment_matching" (default) or "optimization".
#' @param verbose Logical. If TRUE, print diagnostic information.
#'
#' @return A list containing:
#' \itemize{
#'   \item meanlog: The mean of the logarithm (mu) of the lognormal distribution
#'   \item sdlog: The standard deviation of the logarithm (sigma) of the lognormal distribution
#'   \item mean: The mean of the distribution (not meanlog)
#'   \item sd: The standard deviation of the distribution (not sdlog)
#'   \item fitted_ci: The 95% CI of the fitted distribution
#'   \item mode: The mode of the fitted distribution
#' }
#'
#' @details
#' For a lognormal distribution with parameters meanlog (\eqn{\mu}{mu}) and sdlog (\eqn{\sigma}{sigma}):
#' \itemize{
#'   \item Mode = \eqn{\exp(\mu - \sigma^2)}{exp(mu - sigma^2)}
#'   \item Mean = \eqn{\exp(\mu + \sigma^2/2)}{exp(mu + sigma^2/2)}
#'   \item Variance = \eqn{(\exp(\sigma^2) - 1) \exp(2\mu + \sigma^2)}{(exp(sigma^2) - 1) * exp(2 mu + sigma^2)}
#' }
#'
#' The moment matching method (default) matches the 95% CI exactly on the
#' log scale: a lognormal is fully determined by two quantiles, so
#' \code{meanlog} is the midpoint of \code{log(ci_lower)} and
#' \code{log(ci_upper)} and \code{sdlog} is their distance divided by
#' \code{2 * qnorm(0.975)}. \code{mode_val} is validated but does not move
#' the fit; the implied mode \code{exp(meanlog - sdlog^2)} is returned as
#' \code{mode}. Anchoring on a sample-based mode (for example a KDE mode of
#' posterior draws) and then adding \code{sdlog^2} shifted wide CIs upward by
#' orders of magnitude, which is why the CI is authoritative here.
#'
#' The optimization method fits \code{meanlog} and \code{sdlog} jointly to the
#' mode and both quantiles, with all errors measured on the log scale and the
#' mode weighted 10x; use it when \code{mode_val} is a trusted anchor.
#'
#' @examples
#' # Example 1: Fit lognormal distribution
#' result <- fit_lognormal_from_ci(mode_val = 1,
#'                                  ci_lower = 0.5,
#'                                  ci_upper = 3)
#' print(result)
#'
#' # Example 2: Using optimization method
#' result <- fit_lognormal_from_ci(mode_val = 10,
#'                                  ci_lower = 2,
#'                                  ci_upper = 50,
#'                                  method = "optimization")
#'
#' @export
fit_lognormal_from_ci <- function(mode_val, ci_lower, ci_upper,
                                  method = "moment_matching",
                                  verbose = FALSE) {

  # Validate inputs
  if (!is.numeric(mode_val) || !is.numeric(ci_lower) || !is.numeric(ci_upper)) {
    stop("All inputs must be numeric")
  }

  if (mode_val <= 0 || ci_lower <= 0 || ci_upper <= 0) {
    stop("For lognormal distribution, all values must be positive")
  }

  if (ci_lower >= ci_upper) {
    stop("ci_lower must be less than ci_upper")
  }

  if (method == "moment_matching") {
    # Two quantiles determine a lognormal exactly (log X ~ Normal).
    z <- stats::qnorm(0.975)
    meanlog_val <- (log(ci_lower) + log(ci_upper)) / 2
    sdlog_val <- (log(ci_upper) - log(ci_lower)) / (2 * z)

  } else if (method == "optimization") {
    # Joint fit to mode and quantiles, all on the log scale (relative errors).
    log_targets <- log(c(ci_lower, ci_upper))
    log_mode <- log(mode_val)

    objective <- function(params) {
      meanlog <- params[1]
      sdlog <- exp(params[2])
      q <- stats::qnorm(c(0.025, 0.975), mean = meanlog, sd = sdlog)
      10 * ((meanlog - sdlog^2) - log_mode)^2 + sum((q - log_targets)^2)
    }

    init_sdlog <- (log(ci_upper) - log(ci_lower)) / (2 * stats::qnorm(0.975))
    init_meanlog <- (log(ci_lower) + log(ci_upper)) / 2
    result <- stats::optim(c(init_meanlog, log(init_sdlog)), objective,
                           method = "Nelder-Mead",
                           control = list(maxit = 2000, reltol = 1e-12))

    meanlog_val <- result$par[1]
    sdlog_val <- exp(result$par[2])

    if (verbose) {
      message(sprintf("Optimization converged: %s",
                     ifelse(result$convergence == 0, "Yes", "No")))
    }

  } else {
    stop("Method must be 'moment_matching' or 'optimization'")
  }

  # Calculate fitted values
  fitted_mode <- exp(meanlog_val - sdlog_val^2)
  fitted_mean <- exp(meanlog_val + sdlog_val^2/2)
  fitted_var <- (exp(sdlog_val^2) - 1) * exp(2*meanlog_val + sdlog_val^2)
  fitted_sd <- sqrt(fitted_var)
  fitted_ci <- c(
    qlnorm(0.025, meanlog = meanlog_val, sdlog = sdlog_val),
    qlnorm(0.975, meanlog = meanlog_val, sdlog = sdlog_val)
  )

  # Prepare output
  output <- list(
    meanlog = meanlog_val,
    sdlog = sdlog_val,
    mean = fitted_mean,
    sd = fitted_sd,
    fitted_ci = fitted_ci,
    mode = fitted_mode
  )

  if (verbose) {
    message("Fitted Lognormal Distribution:")
    message(sprintf("  Meanlog (mu): %.6f", meanlog_val))
    message(sprintf("  SDlog (sigma): %.6f", sdlog_val))
    message(sprintf("  Mean: %.6f", fitted_mean))
    message(sprintf("  Mode: %.6f", fitted_mode))
    message(sprintf("  Fitted 95%% CI: [%.6f, %.6f]", fitted_ci[1], fitted_ci[2]))
    message(sprintf("  Target 95%% CI: [%.6f, %.6f]", ci_lower, ci_upper))
  }

  return(output)
}
#' Truncated-lognormal helpers
#'
#' A lognormal prior may carry \code{lower}/\code{upper} truncation bounds
#' (e.g. zeta_ratio, lower = 1). \code{meanlog}/\code{sdlog} are then the
#' parameters of the parent (untruncated) lognormal, and the distribution is
#' the parent restricted to \code{[lower, upper]}, as \code{sample_from_prior()}
#' draws it.
#'
#' \code{.fit_truncated_lognormal_ci()} solves for the parent
#' \code{meanlog}/\code{sdlog} whose TRUNCATED distribution has the given
#' quantiles (two equations, two unknowns). Fitting an untruncated lognormal to
#' quantiles of truncated draws and then re-attaching the bound would truncate
#' twice and shift the distribution away from the bound at every stage.
#'
#' @param ci_lower,ci_upper Target quantiles of the truncated distribution.
#' @param lower,upper Truncation bounds (\code{NULL} = 0 / \code{Inf}).
#' @param probs Probabilities of the two target quantiles.
#' @param start Optional \code{c(meanlog, sdlog)} starting point.
#' @return \code{.fit_truncated_lognormal_ci()}: list with \code{meanlog},
#'   \code{sdlog}, \code{lower}, \code{upper} (as given, \code{NULL} dropped)
#'   and \code{max_error} (largest absolute log-quantile residual).
#' @noRd
.fit_truncated_lognormal_ci <- function(ci_lower, ci_upper, lower = NULL, upper = NULL,
                                        probs = c(0.025, 0.975), start = NULL) {
  lo <- if (is.null(lower)) 0 else as.numeric(lower)
  hi <- if (is.null(upper)) Inf else as.numeric(upper)
  if (!(is.finite(ci_lower) && is.finite(ci_upper) && ci_lower < ci_upper))
    stop("ci_lower < ci_upper (finite) required")
  if (!(ci_lower > lo && ci_upper < hi))
    stop("target quantiles must lie strictly inside (lower, upper)")
  target <- unname(log(c(ci_lower, ci_upper)))
  llo <- if (lo > 0) log(lo) else -Inf
  lhi <- log(hi)
  qtrunc <- function(m, s) {
    a <- stats::pnorm((llo - m) / s); b <- stats::pnorm((lhi - m) / s)
    m + s * stats::qnorm(a + probs * (b - a))
  }
  obj <- function(par) {
    q <- qtrunc(par[1], exp(par[2]))
    if (any(!is.finite(q))) return(1e10)
    sum((q - target)^2)
  }
  if (is.null(start)) {
    start <- c(mean(target), diff(target) / (2 * stats::qnorm(0.975)))
  }
  best <- NULL
  # A few starts: the parent may sit well below the bound when the truncated
  # mass piles up against it.
  for (m0 in c(start[1], start[1] - start[2], start[1] - 3 * start[2])) {
    fit <- stats::optim(c(m0, log(start[2])), obj, method = "Nelder-Mead",
                        control = list(maxit = 4000, reltol = 1e-14))
    if (is.null(best) || fit$value < best$value) best <- fit
  }
  m <- unname(best$par[1]); s <- unname(exp(best$par[2]))
  out <- list(meanlog = m, sdlog = s)
  if (!is.null(lower)) out$lower <- lower
  if (!is.null(upper)) out$upper <- upper
  out$max_error <- max(abs(qtrunc(m, s) - target))
  out
}

#' @rdname dot-fit_truncated_lognormal_ci
#' @param meanlog,sdlog Parent lognormal parameters.
#' @return \code{.lognormal_trunc_mean()}: mean of the truncated distribution.
#' @noRd
.lognormal_trunc_mean <- function(meanlog, sdlog, lower = NULL, upper = NULL) {
  lo <- if (is.null(lower)) 0 else as.numeric(lower)
  hi <- if (is.null(upper)) Inf else as.numeric(upper)
  if (lo <= 0 && !is.finite(hi)) return(exp(meanlog + sdlog^2 / 2))
  zl <- if (lo > 0) (log(lo) - meanlog) / sdlog else -Inf
  zh <- (log(hi) - meanlog) / sdlog
  mass <- stats::pnorm(zh) - stats::pnorm(zl)
  exp(meanlog + sdlog^2 / 2) * (stats::pnorm(zh - sdlog) - stats::pnorm(zl - sdlog)) / mass
}

#' @rdname dot-fit_truncated_lognormal_ci
#' @param x Evaluation points.
#' @return \code{.dlnorm_trunc()}: density of the truncated distribution (zero
#'   outside the bounds).
#' @noRd
.dlnorm_trunc <- function(x, meanlog, sdlog, lower = NULL, upper = NULL) {
  lo <- if (is.null(lower)) 0 else as.numeric(lower)
  hi <- if (is.null(upper)) Inf else as.numeric(upper)
  mass <- stats::plnorm(hi, meanlog, sdlog) - stats::plnorm(lo, meanlog, sdlog)
  d <- stats::dlnorm(x, meanlog, sdlog) / mass
  d[x < lo | x > hi] <- 0
  d
}

#' @rdname dot-fit_truncated_lognormal_ci
#' @param p Probabilities.
#' @return \code{.qlnorm_trunc()}: quantiles of the truncated distribution.
#' @noRd
.qlnorm_trunc <- function(p, meanlog, sdlog, lower = NULL, upper = NULL) {
  lo <- if (is.null(lower)) 0 else as.numeric(lower)
  hi <- if (is.null(upper)) Inf else as.numeric(upper)
  a <- stats::plnorm(lo, meanlog, sdlog); b <- stats::plnorm(hi, meanlog, sdlog)
  stats::qlnorm(a + p * (b - a), meanlog, sdlog)
}
