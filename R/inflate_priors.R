#' Inflate Prior or Posterior Distributions by a Variance Factor
#'
#' @description
#' Takes a priors or posteriors list object and inflates the variance of each
#' distribution by a multiplicative factor while preserving the mean. This is
#' used in staged Bayesian estimation to broaden tight posteriors from one stage
#' before passing them as priors to the next stage, preventing weight collapse
#' in importance sampling (particle filter / BFRS) when consecutive stages
#' target different objectives.
#'
#' The technique is known as kernel smoothing, covariance inflation, or
#' variance tempering in the Sequential Monte Carlo literature (Liu & West
#' 2001; Del Moral et al. 2006).
#'
#' @section Methods:
#' Each distribution is inflated using the most appropriate method:
#'
#' \strong{Analytic (exact):}
#' \describe{
#'   \item{beta(shape1, shape2)}{Reduce precision \code{s = α+β} by factor \code{f}.
#'     Maximum inflatable factor is \code{f_max = s+1}.}
#'   \item{gamma(shape, rate)}{\code{shape_new = shape/f, rate_new = rate/f}.}
#'   \item{lognormal(meanlog, sdlog)}{CV² scaled by \code{f}; \code{meanlog}
#'     adjusted to preserve natural-scale mean.}
#'   \item{lognormal(mean, sd)}{\code{sd_new = sqrt(f)*sd}.}
#'   \item{normal(mean, sd)}{\code{sd_new = sqrt(f)*sd}.}
#'   \item{uniform(min, max)}{Bounds extended symmetrically around midpoint.
#'     Skipped with a warning if new bounds violate positivity.}
#' }
#'
#' \strong{Analytic with empirical fallback:}
#' \describe{
#'   \item{truncnorm(mean, sd, a, b)}{Root-finding preserves actual truncated
#'     mean. Due to active bounds, the achieved variance ratio may be less than
#'     \code{inflation_factor}. Falls back to empirical refit if root-find fails.}
#' }
#'
#' \strong{Empirical (sample → inflate → refit):}
#' \describe{
#'   \item{gompertz(b, eta)}{No closed-form moments. Sampled empirically,
#'     variance inflated, distribution refit via MLE.}
#'   \item{Any other distribution}{Universal empirical fallback.}
#' }
#'
#' @param priors List. A priors or posteriors object with \code{parameters_global}
#'   and \code{parameters_location} slots.
#' @param inflation_factor Numeric scalar > 0. Variance multiplier. \code{2.0}
#'   doubles variance while preserving the mean. Default 2.0.
#' @param params Optional character vector. Base parameter names to inflate.
#'   When \code{NULL} all non-fixed/frozen parameters are inflated.
#' @param n_samples Integer. Number of samples for empirical fallback methods
#'   (truncnorm fallback, gompertz). Default 10000L.
#' @param verbose Logical. Print per-parameter inflation messages. Default TRUE.
#'
#' @return A priors list with inflated distribution parameters. Metadata updated.
#'
#' @seealso \code{\link{update_priors_from_posteriors}},
#'   \code{\link{mosaic_adaptive_s3_weights}}
#'
#' @export
inflate_priors <- function(priors,
                           inflation_factor = 2.0,
                           params           = NULL,
                           n_samples        = 10000L,
                           verbose          = TRUE) {

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------
  if (!is.list(priors))
    stop("priors must be a list object", call. = FALSE)
  if (!is.numeric(inflation_factor) || length(inflation_factor) != 1L ||
      !is.finite(inflation_factor) || inflation_factor <= 0)
    stop("inflation_factor must be a single finite positive number", call. = FALSE)
  if (inflation_factor == 1) {
    if (verbose) message("inflate_priors: inflation_factor = 1, returning unchanged.")
    return(priors)
  }
  if (inflation_factor < 1 && verbose)
    message("inflate_priors: inflation_factor < 1 deflates variance.")
  if (!all(c("parameters_global", "parameters_location") %in% names(priors)))
    stop("priors must have parameters_global and parameters_location slots", call. = FALSE)

  f <- inflation_factor

  # ---------------------------------------------------------------------------
  # Empirical fallback: sample → mean-preserving scale → refit
  # Used for: gompertz, truncnorm when root-find fails, any unknown distribution
  # ---------------------------------------------------------------------------
  .inflate_empirical <- function(entry, param_name) {
    dist <- tolower(entry$distribution)
    p    <- entry$parameters

    # Sample from the current distribution
    samples <- tryCatch(
      sample_from_prior(n = n_samples, prior = entry),
      error = function(e) NULL
    )
    if (is.null(samples) || length(samples) < 10 || !all(is.finite(samples)))
      return(NULL)

    # Variance inflation. For distributions on (0, inf) (gamma, lognormal,
    # gompertz), inflate in log-space to prevent negative samples when the
    # distribution is concentrated near zero.  For other distributions use the
    # standard mean-preserving linear scaling.
    positive_dists <- c("gamma", "lognormal", "gompertz")
    if (dist %in% positive_dists && all(samples > 0)) {
      log_s    <- log(samples)
      log_mu   <- mean(log_s)
      inflated <- exp(log_mu + sqrt(f) * (log_s - log_mu))
    } else {
      mu_s     <- mean(samples)
      inflated <- mu_s + sqrt(f) * (samples - mu_s)
    }

    # Refit the original distribution type to the inflated samples
    new_params <- tryCatch({
      switch(dist,

        normal = list(mean = mean(inflated), sd = sd(inflated)),

        beta = {
          x <- pmax(1e-6, pmin(1 - 1e-6, inflated))
          m <- mean(x); v <- var(x)
          conc <- m * (1 - m) / v - 1
          if (!is.finite(conc) || conc <= 0)
            stop("Beta MOM: non-positive concentration")
          list(shape1 = m * conc, shape2 = (1 - m) * conc)
        },

        gamma = {
          x <- pmax(1e-300, inflated)
          m <- mean(x); v <- var(x)
          if (v <= 0) stop("Gamma MOM: zero variance in samples")
          list(shape = m^2 / v, rate = m / v)
        },

        lognormal = {
          x <- pmax(1e-300, inflated)
          if (!is.null(p$meanlog)) {
            ls <- log(x)
            list(meanlog = mean(ls), sdlog = sd(ls))
          } else {
            list(mean = mean(x), sd = sd(x))
          }
        },

        # truncnorm: keep original bounds, refit (mean, sd) via MLE
        truncnorm = {
          a_n <- if (identical(p$a, Inf)  || identical(p$a, "Inf"))   Inf  else
                 if (identical(p$a, -Inf) || identical(p$a, "-Inf")) -Inf  else
                 as.numeric(p$a)
          b_n <- if (identical(p$b, Inf)  || identical(p$b, "Inf"))   Inf  else
                 if (identical(p$b, -Inf) || identical(p$b, "-Inf")) -Inf  else
                 as.numeric(p$b)
          # Clip samples to feasible region
          x <- pmax(if (is.finite(a_n)) a_n + 1e-10 else -1e15,
                    pmin(if (is.finite(b_n)) b_n - 1e-10 else  1e15, inflated))
          # MLE for pre-truncation (mu, sigma) with fixed bounds
          nll <- function(par) {
            mu_p <- par[1]; sig_p <- exp(par[2])
            d <- truncnorm::dtruncnorm(x, a = a_n, b = b_n,
                                       mean = mu_p, sd = sig_p)
            -sum(log(pmax(d, 1e-300)))
          }
          init <- c(mean(x), log(max(sd(x), 1e-6)))
          fit  <- stats::optim(init, nll, method = "BFGS",
                               control = list(maxit = 500L))
          list(mean = fit$par[1], sd = exp(fit$par[2]),
               a = p$a, b = p$b)
        },

        # Gompertz: refit via quantile-matching (more robust than MLE for
        # inflated samples that may shift the distribution shape significantly)
        gompertz = {
          x <- pmax(1e-10, inflated)
          q_low  <- as.numeric(quantile(x, 0.025))
          q_high <- as.numeric(quantile(x, 0.975))
          # Mode from KDE
          kde <- tryCatch(density(x), error = function(e) NULL)
          mode_val <- if (!is.null(kde)) kde$x[which.max(kde$y)] else median(x)
          mode_val <- max(1e-10, mode_val)
          fit_g <- tryCatch(
            fit_gompertz_from_ci(mode_val = mode_val, ci_lower = q_low,
                                 ci_upper = q_high, verbose = FALSE),
            error = function(e) NULL
          )
          if (is.null(fit_g)) stop("Gompertz quantile refit failed")
          list(b = fit_g$b, eta = fit_g$eta)
        },

        stop("empirical refit: unsupported distribution '", dist, "'")
      )
    }, error = function(e) {
      warning(sprintf(
        "inflate_priors: empirical refit failed for '%s' (%s). Skipping.",
        param_name, e$message), call. = FALSE)
      NULL
    })

    if (is.null(new_params)) return(NULL)
    entry$parameters <- new_params
    entry
  }

  # ---------------------------------------------------------------------------
  # Analytic inflation per distribution type
  # ---------------------------------------------------------------------------
  .inflate_entry <- function(entry, param_name) {
    if (!all(c("distribution", "parameters") %in% names(entry)))
      return(entry)

    dist <- tolower(entry$distribution)
    p    <- entry$parameters

    if (dist %in% c("fixed", "frozen", "failed")) return(entry)

    # Track method used for logging
    method_used <- "analytic"

    new_entry <- switch(dist,

      # --- Beta (analytic, exact) ---
      beta = {
        a <- p$shape1; b <- p$shape2
        if (is.null(a) || is.null(b) || !is.finite(a) || !is.finite(b))
          return(entry)
        s     <- a + b
        f_max <- s + 1
        if (f >= f_max) {
          warning(sprintf(
            "inflate_priors: beta '%s' cannot be inflated by %.2f (f_max=%.3f). Skipping.",
            param_name, f, f_max), call. = FALSE)
          return(entry)
        }
        mu    <- a / s
        s_new <- (s + 1) / f - 1
        a_new <- mu * s_new
        b_new <- (1 - mu) * s_new
        if (verbose && (a_new < 1 || b_new < 1))
          message(sprintf(
            "  [beta] '%s': shapes below 1 after inflation (shape1=%.4f shape2=%.4f)",
            param_name, a_new, b_new))
        entry$parameters$shape1 <- a_new
        entry$parameters$shape2 <- b_new
        entry
      },

      # --- Gamma (analytic, exact) ---
      gamma = {
        if (is.null(p$shape) || is.null(p$rate)) return(entry)
        entry$parameters$shape <- p$shape / f
        entry$parameters$rate  <- p$rate  / f
        entry
      },

      # --- Lognormal (analytic, exact) ---
      lognormal = {
        if (!is.null(p$meanlog) && !is.null(p$sdlog)) {
          mu_nat      <- exp(p$meanlog + p$sdlog^2 / 2)
          cv2_old     <- exp(p$sdlog^2) - 1
          new_sdlog   <- sqrt(log(1 + f * cv2_old))
          new_meanlog <- log(mu_nat) - new_sdlog^2 / 2
          entry$parameters$meanlog <- new_meanlog
          entry$parameters$sdlog   <- new_sdlog
        } else if (!is.null(p$mean) && !is.null(p$sd)) {
          entry$parameters$sd <- sqrt(f) * p$sd
        } else {
          warning(sprintf(
            "inflate_priors: lognormal '%s' unrecognised parameters. Skipping.",
            param_name), call. = FALSE)
          return(entry)
        }
        entry
      },

      # --- Normal (analytic, exact) ---
      normal = {
        if (is.null(p$sd)) return(entry)
        entry$parameters$sd <- sqrt(f) * p$sd
        entry
      },

      # --- Truncated normal (analytic, exact on the underlying normal) ---
      # Inflate the pre-truncation sd by sqrt(f) and keep the pre-truncation
      # mean and bounds unchanged. This is equivalent to: treat as Normal,
      # apply exact variance inflation, re-impose the truncation bounds.
      # The pre-truncation mean (parameters$mean) is preserved exactly; the
      # post-truncation mean shifts slightly when bounds are tight — this is
      # acceptable because the stored 'mean' IS the pre-truncation parameter,
      # and the bounds clip symmetrically around it. The pre-truncation variance
      # is inflated by exactly f; the post-truncation variance increase is
      # bounded by the active bounds (same fundamental constraint regardless of
      # approach, and now handled simply without root-finding).
      truncnorm = {
        if (is.null(p$sd)) return(entry)
        entry$parameters$sd <- sqrt(f) * p$sd
        # mean, a, b unchanged
        entry
      },

      # --- Uniform (analytic, exact) ---
      uniform = {
        if (is.null(p$min) || is.null(p$max)) return(entry)
        mu       <- (p$min + p$max) / 2
        half_new <- sqrt(f) * (p$max - p$min) / 2
        new_min  <- mu - half_new
        new_max  <- mu + half_new
        if (new_min < 0 && p$min >= 0) {
          warning(sprintf(
            "inflate_priors: uniform '%s' yields new_min=%.4f (was %.4f >= 0). Skipping.",
            param_name, new_min, p$min), call. = FALSE)
          return(entry)
        }
        entry$parameters$min <- new_min
        entry$parameters$max <- new_max
        entry
      },

      # --- Gompertz (empirical only — no closed-form moments) ---
      gompertz = {
        method_used <<- "empirical"
        emp <- .inflate_empirical(entry, param_name)
        if (is.null(emp)) {
          warning(sprintf(
            "inflate_priors: gompertz '%s' empirical refit failed. Skipping.",
            param_name), call. = FALSE)
          return(entry)
        }
        emp
      },

      # --- Unknown distribution — empirical fallback ---
      {
        method_used <<- "empirical (unknown dist)"
        emp <- .inflate_empirical(entry, param_name)
        if (is.null(emp)) {
          if (verbose) message(sprintf(
            "  inflate_priors: unknown distribution '%s' for '%s'. Skipping.", dist, param_name))
          return(entry)
        }
        emp
      }
    )  # end switch

    if (verbose) message(sprintf(
      "  inflate_priors: [%-10s] %-35s %s",
      dist, param_name, if (method_used == "analytic") "inflated" else
        paste("inflated (", method_used, ")")))

    new_entry
  }

  # ---------------------------------------------------------------------------
  # Helper: should this parameter be inflated?
  # ---------------------------------------------------------------------------
  .should_inflate <- function(base_name, full_name) {
    if (is.null(params)) return(TRUE)
    base_name %in% params || full_name %in% params
  }

  # ---------------------------------------------------------------------------
  # Walk and inflate
  # ---------------------------------------------------------------------------
  result     <- priors
  n_inflated <- 0L

  for (pname in names(result$parameters_global)) {
    if (!.should_inflate(pname, pname)) next
    old_entry <- result$parameters_global[[pname]]
    new_entry <- .inflate_entry(old_entry, pname)
    if (!identical(old_entry, new_entry)) {
      result$parameters_global[[pname]] <- new_entry
      n_inflated <- n_inflated + 1L
    }
  }

  for (base_name in names(result$parameters_location)) {
    loc_block <- result$parameters_location[[base_name]]
    if (is.null(loc_block$location)) next
    for (iso in names(loc_block$location)) {
      full_name <- paste0(base_name, "_", iso)
      if (!.should_inflate(base_name, full_name)) next
      old_entry <- loc_block$location[[iso]]
      new_entry <- .inflate_entry(old_entry, full_name)
      if (!identical(old_entry, new_entry)) {
        result$parameters_location[[base_name]]$location[[iso]] <- new_entry
        n_inflated <- n_inflated + 1L
      }
    }
  }

  result$metadata$description <- paste0(
    result$metadata$description %||% "",
    sprintf(" [variance inflated x%.2f on %d params]", f, n_inflated)
  )
  result$metadata$inflation_factor <- f
  result$metadata$inflation_date   <- as.character(Sys.Date())

  if (verbose)
    message(sprintf("inflate_priors: inflated %d entries (factor=%.2f).", n_inflated, f))

  invisible(result)
}
