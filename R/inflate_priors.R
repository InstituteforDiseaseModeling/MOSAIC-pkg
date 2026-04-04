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
#' 2001; Del Moral et al. 2006). For staged cholera calibration, it is
#' typically applied to CFR parameters after the deaths-only stage and before
#' the joint stage, allowing the joint stage to explore parameter combinations
#' that the deaths-only stage could not anticipate.
#'
#' @param priors List. A priors or posteriors object as returned by
#'   \code{\link{get_location_priors}} or \code{\link{update_priors_from_posteriors}}.
#'   Must have \code{parameters_global} and \code{parameters_location} slots.
#' @param inflation_factor Numeric scalar > 0. Variance multiplier. A value of
#'   2.0 doubles the variance of each distribution while preserving its mean.
#'   Values < 1.0 deflate (not recommended). Default 2.0.
#' @param params Optional character vector. Base parameter names to inflate
#'   (e.g. \code{c("mu_j_baseline", "mu_j_slope")}). When \code{NULL} (default)
#'   all non-fixed, non-frozen parameters are inflated. Location-specific
#'   parameters are matched on base name (inflating \code{"mu_j_baseline"}
#'   inflates all locations).
#' @param verbose Logical. Print per-parameter inflation diagnostics. Default
#'   \code{TRUE}.
#'
#' @return A priors list with the same structure as \code{priors} but with
#'   inflated distribution parameters. Metadata is updated to record the
#'   inflation factor applied.
#'
#' @details
#' \strong{Supported distributions and inflation algebra:}
#'
#' \describe{
#'   \item{beta(shape1, shape2)}{Precision \code{s = shape1+shape2} reduced by
#'     \code{f}: \code{s_new = (s+1)/f - 1}. Maximum inflatable factor is
#'     \code{f_max = s + 1}; entries exceeding this are skipped with a warning.}
#'   \item{gamma(shape, rate)}{Both halved by \code{f}:
#'     \code{shape_new = shape/f}, \code{rate_new = rate/f}. Always valid.}
#'   \item{lognormal(meanlog, sdlog)}{CV² scaled by \code{f}:
#'     \code{sdlog_new = sqrt(log(1 + f*(exp(sdlog^2)-1)))}; \code{meanlog}
#'     adjusted to preserve natural-scale mean.}
#'   \item{lognormal(mean, sd)}{Natural-scale sd scaled: \code{sd_new = sqrt(f)*sd}.}
#'   \item{normal(mean, sd)}{\code{sd_new = sqrt(f)*sd}. Mean unchanged.}
#'   \item{truncnorm(mean, sd, a, b)}{sd scaled, then pre-truncation mean
#'     found by root-finding to preserve the actual truncated mean.}
#'   \item{uniform(min, max)}{Bounds extended symmetrically around midpoint.
#'     Skipped with a warning if new bounds violate positivity (min was >= 0
#'     but new_min < 0).}
#'   \item{fixed / frozen / failed / gompertz}{Skipped silently (fixed/frozen)
#'     or with a warning (gompertz).}
#' }
#'
#' @references
#' Liu, J. S., & West, M. (2001). Combined parameter and state estimation in
#' simulation-based filtering. In Sequential Monte Carlo Methods in Practice
#' (pp. 197-223). Springer.
#'
#' Del Moral, P., Doucet, A., & Jasra, A. (2006). Sequential Monte Carlo
#' samplers. Journal of the Royal Statistical Society: Series B, 68(3),
#' 411-436.
#'
#' @seealso \code{\link{update_priors_from_posteriors}},
#'   \code{\link{mosaic_adaptive_s3_weights}}
#'
#' @export
inflate_priors <- function(priors,
                           inflation_factor = 2.0,
                           params           = NULL,
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
  # Bias deficiency transform — used internally to check entry validity only
  # ---------------------------------------------------------------------------

  .inflate_entry <- function(entry, param_name) {
    if (!all(c("distribution", "parameters") %in% names(entry)))
      return(entry)

    dist <- tolower(entry$distribution)
    p    <- entry$parameters

    # Skip degenerate / non-distribution states
    if (dist %in% c("fixed", "frozen", "failed")) return(entry)

    new_entry <- switch(dist,

      # --- Beta ---
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

      # --- Gamma ---
      gamma = {
        if (is.null(p$shape) || is.null(p$rate)) return(entry)
        entry$parameters$shape <- p$shape / f
        entry$parameters$rate  <- p$rate  / f
        entry
      },

      # --- Lognormal ---
      lognormal = {
        if (!is.null(p$meanlog) && !is.null(p$sdlog)) {
          # Standard log-scale parameterisation
          mu_nat  <- exp(p$meanlog + p$sdlog^2 / 2)
          cv2_old <- exp(p$sdlog^2) - 1
          new_sdlog   <- sqrt(log(1 + f * cv2_old))
          new_meanlog <- log(mu_nat) - new_sdlog^2 / 2
          entry$parameters$meanlog <- new_meanlog
          entry$parameters$sdlog   <- new_sdlog
        } else if (!is.null(p$mean) && !is.null(p$sd)) {
          # Natural-scale parameterisation (e.g. epsilon)
          entry$parameters$sd <- sqrt(f) * p$sd
        } else {
          warning(sprintf("inflate_priors: lognormal '%s' has unrecognised parameters. Skipping.",
                          param_name), call. = FALSE)
          return(entry)
        }
        entry
      },

      # --- Normal ---
      normal = {
        if (is.null(p$sd)) return(entry)
        entry$parameters$sd <- sqrt(f) * p$sd
        entry
      },

      # --- Truncated normal ---
      truncnorm = {
        if (!all(c("mean", "sd", "a", "b") %in% names(p))) return(entry)
        a_bound <- if (identical(p$a, Inf) || identical(p$a, "Inf"))   Inf  else
                   if (identical(p$a, -Inf)|| identical(p$a, "-Inf")) -Inf  else
                   as.numeric(p$a)
        b_bound <- if (identical(p$b, Inf) || identical(p$b, "Inf"))   Inf  else
                   if (identical(p$b, -Inf)|| identical(p$b, "-Inf")) -Inf  else
                   as.numeric(p$b)
        sd_new   <- sqrt(f) * p$sd
        mu_trunc <- tryCatch(
          truncnorm::etruncnorm(a_bound, b_bound, p$mean, p$sd),
          error = function(e) p$mean  # fall back to pre-truncation mean
        )
        # Root-find pre-truncation mean that preserves truncated mean
        new_mean <- tryCatch({
          lo <- if (is.finite(a_bound)) a_bound - 20 * sd_new else mu_trunc - 20 * sd_new
          hi <- if (is.finite(b_bound)) b_bound + 20 * sd_new else mu_trunc + 20 * sd_new
          stats::uniroot(
            function(m) truncnorm::etruncnorm(a_bound, b_bound, m, sd_new) - mu_trunc,
            interval = c(lo, hi),
            tol = 1e-8
          )$root
        }, error = function(e) {
          warning(sprintf(
            "inflate_priors: truncnorm '%s' root-find failed (%s). Using scaled sd, original mean.",
            param_name, e$message), call. = FALSE)
          p$mean
        })
        entry$parameters$mean <- new_mean
        entry$parameters$sd   <- sd_new
        entry
      },

      # --- Uniform ---
      uniform = {
        if (is.null(p$min) || is.null(p$max)) return(entry)
        mu           <- (p$min + p$max) / 2
        half_new     <- sqrt(f) * (p$max - p$min) / 2
        new_min      <- mu - half_new
        new_max      <- mu + half_new
        if (new_min < 0 && p$min >= 0) {
          warning(sprintf(
            "inflate_priors: uniform '%s' inflation yields new_min=%.4f (was %.4f >= 0). Skipping.",
            param_name, new_min, p$min), call. = FALSE)
          return(entry)
        }
        entry$parameters$min <- new_min
        entry$parameters$max <- new_max
        entry
      },

      # --- Gompertz ---
      gompertz = {
        warning(sprintf(
          "inflate_priors: gompertz '%s' inflation not implemented. Skipping.",
          param_name), call. = FALSE)
        entry
      },

      # --- Unknown ---
      {
        if (verbose)
          message(sprintf("  inflate_priors: unknown distribution '%s' for '%s'. Skipping.",
                          dist, param_name))
        entry
      }
    )  # end switch

    new_entry
  }  # end .inflate_entry

  # ---------------------------------------------------------------------------
  # Helper: should this parameter be inflated?
  # ---------------------------------------------------------------------------
  .should_inflate <- function(base_name, full_name) {
    if (is.null(params)) return(TRUE)  # inflate all
    base_name %in% params || full_name %in% params
  }

  # ---------------------------------------------------------------------------
  # Walk and inflate
  # ---------------------------------------------------------------------------
  result   <- priors
  n_inflated <- 0L

  # Global parameters
  for (pname in names(result$parameters_global)) {
    if (!.should_inflate(pname, pname)) next
    old_entry <- result$parameters_global[[pname]]
    new_entry <- .inflate_entry(old_entry, pname)
    if (!identical(old_entry, new_entry)) {
      result$parameters_global[[pname]] <- new_entry
      n_inflated <- n_inflated + 1L
      if (verbose) message(sprintf(
        "  inflate_priors: [global] %-30s dist=%-10s inflated",
        pname, old_entry$distribution))
    }
  }

  # Location-specific parameters
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
        if (verbose) message(sprintf(
          "  inflate_priors: [location] %-30s dist=%-10s inflated",
          full_name, old_entry$distribution))
      }
    }
  }

  # Update metadata
  result$metadata$description <- paste0(
    result$metadata$description %||% "",
    sprintf(" [variance inflated x%.2f on %d params]", f, n_inflated)
  )
  result$metadata$inflation_factor <- f
  result$metadata$inflation_date   <- as.character(Sys.Date())

  if (verbose)
    message(sprintf("inflate_priors: inflated %d parameter entries (factor=%.2f).",
                    n_inflated, f))

  invisible(result)
}
