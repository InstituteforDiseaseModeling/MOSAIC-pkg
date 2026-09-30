#' Fit Beta Distribution from Mode and 95% Confidence Intervals
#'
#' This function calculates the shape parameters (alpha and beta) of a beta distribution
#' that best matches a given mode and 95% confidence intervals.
#'
#' @param mode_val Numeric. The mode of the distribution (must be in (0,1)).
#' @param ci_lower Numeric. The lower bound of the 95% confidence interval (must be in (0,1)).
#' @param ci_upper Numeric. The upper bound of the 95% confidence interval (must be in (0,1)).
#' @param method Character. Method to use: "moment_matching" (default) or "optimization".
#' @param verbose Logical. If TRUE, print diagnostic information.
#'
#' @return A list containing:
#' \itemize{
#'   \item shape1: The alpha shape parameter of the beta distribution
#'   \item shape2: The beta shape parameter of the beta distribution
#'   \item fitted_mode: The mode of the fitted distribution
#'   \item fitted_mean: The mean of the fitted distribution
#'   \item fitted_var: The variance of the fitted distribution
#'   \item fitted_ci: The 95% CI of the fitted distribution
#'   \item input_mode: The input mode value
#'   \item input_ci: The input confidence interval
#' }
#'
#' @details
#' \code{"moment_matching"} (default) keeps the mode exact: every Beta with
#' both shapes above 1 and mode \eqn{m} is \eqn{\mathrm{Beta}(1 + mk, 1 + (1 - m)k)}
#' for some concentration \eqn{k > 0}, and \eqn{k} is chosen to minimise the
#' squared error of the fitted 2.5% and 97.5% quantiles against the CI on the
#' logit scale. Logit-scale errors are relative errors for small proportions, so
#' a CI around 1e-6 is matched as closely as one around 0.5. When the CI is
#' wider than any unimodal Beta with that mode allows, the widest achievable
#' interval is returned.
#'
#' \code{"optimization"} fits both shapes freely to the mode and the two
#' quantiles (all on the logit scale, mode weighted 100x), so the mode is matched
#' closely but not exactly.
#'
#' @examples
#' # Example 1: Fit beta for phi_1 (vaccine effectiveness)
#' result <- fit_beta_from_ci(mode_val = 0.788, 
#'                             ci_lower = 0.753, 
#'                             ci_upper = 0.822)
#' print(result)
#'
#' # Example 2: Using optimization method
#' result <- fit_beta_from_ci(mode_val = 0.65, 
#'                             ci_lower = 0.50, 
#'                             ci_upper = 0.78,
#'                             method = "optimization")
#'
#' @export
fit_beta_from_ci <- function(mode_val, ci_lower, ci_upper, 
                             method = "moment_matching", 
                             verbose = FALSE) {
  
  # Validate inputs
  if (mode_val <= 0 || mode_val >= 1) {
    stop("Mode must be in (0, 1) for beta distribution")
  }
  
  if (ci_lower <= 0 || ci_lower >= 1) {
    stop("ci_lower must be in (0, 1) for beta distribution")
  }
  
  if (ci_upper <= 0 || ci_upper >= 1) {
    stop("ci_upper must be in (0, 1) for beta distribution")
  }
  
  if (ci_lower >= ci_upper) {
    stop("ci_lower must be less than ci_upper")
  }
  
  if (mode_val <= ci_lower || mode_val >= ci_upper) {
    if (verbose) {
      warning("Mode is outside the confidence interval - this may lead to poor fits")
    }
  }
  
  if (method == "moment_matching") {
    # Mode-constrained quantile matching on the logit scale.
    #
    # Any Beta with shape1, shape2 > 1 and mode m can be written as
    #   shape1 = 1 + m * k,  shape2 = 1 + (1 - m) * k,  k = shape1 + shape2 - 2 > 0,
    # so the mode is matched exactly and the single free parameter k (the
    # concentration) is chosen to bring the fitted 2.5%/97.5% quantiles as close
    # as possible to the target CI. Errors are measured on the logit scale, i.e.
    # as relative errors for small proportions (and for 1 - p near 1), so the fit
    # is scale-free: a CI around 1e-6 is matched as well as one around 0.5.
    # (The pre-v0.99.11 version clamped the mean to [ci_lower + 0.01,
    # ci_upper - 0.01] -- an absolute offset -- which discarded the CI for any
    # quantity below ~0.02.)
    #
    # If the target CI is wider than any unimodal Beta with this mode allows
    # (k -> 0 is the widest), the widest achievable interval is returned.
    shapes <- .fit_beta_mode_ci_k(mode_val, ci_lower, ci_upper)
    alpha <- shapes[1]
    beta <- shapes[2]

  } else if (method == "optimization") {
    # Free two-parameter fit of (shape1, shape2) > 1 to the mode and both
    # quantiles, all measured on the logit scale (relative errors near 0 and 1).
    # The mode is weighted 100x the quantiles, so it is matched closely but not
    # exactly; start from the mode-constrained solution.
    target_q <- stats::qlogis(c(ci_lower, ci_upper))
    target_m <- stats::qlogis(mode_val)

    objective <- function(params) {
      a <- 1 + exp(params[1])
      b <- 1 + exp(params[2])
      q <- stats::qbeta(c(0.025, 0.975), shape1 = a, shape2 = b)
      m <- (a - 1) / (a + b - 2)
      err <- 100 * (stats::qlogis(m) - target_m)^2 +
             sum((stats::qlogis(q) - target_q)^2)
      if (!is.finite(err)) 1e10 else err
    }

    init <- .fit_beta_mode_ci_k(mode_val, ci_lower, ci_upper)
    result <- stats::optim(log(init - 1), objective, method = "Nelder-Mead",
                           control = list(maxit = 2000, reltol = 1e-12))

    alpha <- 1 + exp(result$par[1])
    beta <- 1 + exp(result$par[2])

  } else {
    stop("Method must be 'moment_matching' or 'optimization'")
  }
  
  # Calculate fitted statistics
  fitted_mean <- alpha / (alpha + beta)
  fitted_var <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
  fitted_sd <- sqrt(fitted_var)
  fitted_lower <- qbeta(0.025, shape1 = alpha, shape2 = beta)
  fitted_upper <- qbeta(0.975, shape1 = alpha, shape2 = beta)
  
  # Mode (only exists if both shape parameters > 1)
  fitted_mode <- if (alpha > 1 && beta > 1) {
    (alpha - 1) / (alpha + beta - 2)
  } else {
    NA
  }
  
  # Prepare output
  output <- list(
    shape1 = alpha,
    shape2 = beta,
    fitted_mode = fitted_mode,
    fitted_mean = fitted_mean,
    fitted_var = fitted_var,
    fitted_sd = fitted_sd,
    fitted_ci = c(lower = fitted_lower, upper = fitted_upper),
    input_mode = mode_val,
    input_ci = c(lower = ci_lower, upper = ci_upper)
  )
  
  if (verbose) {
    cat("\n=== Beta Distribution Fitting ===\n")
    cat(sprintf("Input: mode = %.4f, 95%% CI = [%.4f, %.4f]\n", 
                mode_val, ci_lower, ci_upper))
    cat(sprintf("Method: %s\n", method))
    cat("\nFitted parameters:\n")
    cat(sprintf("  Alpha (shape1): %.4f\n", alpha))
    cat(sprintf("  Beta (shape2): %.4f\n", beta))
    cat("\nFitted statistics:\n")
    if (!is.na(fitted_mode)) {
      cat(sprintf("  Mode: %.4f (target: %.4f, diff: %.2f%%)\n", 
                  fitted_mode, mode_val, 100*(fitted_mode - mode_val)/mode_val))
    }
    cat(sprintf("  Mean: %.4f\n", fitted_mean))
    cat(sprintf("  SD: %.4f\n", fitted_sd))
    cat(sprintf("  95%% CI: [%.4f, %.4f]\n", fitted_lower, fitted_upper))
    cat(sprintf("  Target CI: [%.4f, %.4f]\n", ci_lower, ci_upper))
    cat("\n")
  }
  
  return(output)
}

# Mode-constrained Beta fit: shape1 = 1 + m*k, shape2 = 1 + (1-m)*k, with k > 0
# chosen by a log-spaced grid search refined by optimize() to minimise the
# squared logit-scale error of the fitted 2.5%/97.5% quantiles against the CI.
# Returns c(shape1, shape2).
.fit_beta_mode_ci_k <- function(mode_val, ci_lower, ci_upper) {
  target <- stats::qlogis(c(ci_lower, ci_upper))
  obj <- function(log_k) {
    k <- exp(log_k)
    q <- stats::qbeta(c(0.025, 0.975),
                      shape1 = 1 + mode_val * k,
                      shape2 = 1 + (1 - mode_val) * k)
    err <- sum((stats::qlogis(q) - target)^2)
    if (!is.finite(err)) Inf else err
  }
  grid <- seq(log(1e-6), log(1e15), length.out = 211L)
  vals <- vapply(grid, obj, numeric(1))
  if (!any(is.finite(vals))) {
    stop("fit_beta_from_ci: no finite Beta fit for mode = ", mode_val,
         ", CI = [", ci_lower, ", ", ci_upper, "]")
  }
  i <- which.min(vals)
  lo <- grid[max(1L, i - 1L)]
  hi <- grid[min(length(grid), i + 1L)]
  opt <- stats::optimize(obj, interval = c(lo, hi), tol = 1e-10)
  log_k <- if (opt$objective <= vals[i]) opt$minimum else grid[i]
  k <- exp(log_k)
  c(1 + mode_val * k, 1 + (1 - mode_val) * k)
}
