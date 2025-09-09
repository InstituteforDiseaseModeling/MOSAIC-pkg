#' Sample from Prior Distribution (Simplified)
#'
#' A simplified unified function to sample values from prior distributions.
#' Expects all priors to have consistent structure with $distribution and $parameters slots.
#'
#' @param n Integer. Number of samples to draw (default: 1).
#' @param prior List. Prior object containing 'distribution' and 'parameters' slots.
#' @param verbose Logical. If TRUE, print warnings for NULL or invalid priors (default: FALSE).
#'
#' @return Numeric vector of length n containing samples from the prior distribution.
#'         Returns NA values if prior is NULL/invalid.
#'
#' @details
#' This simplified version expects every prior to have a consistent structure:
#' \itemize{
#'   \item \code{$distribution}: Character string naming the distribution type
#'   \item \code{$parameters}: List containing the distribution parameters
#' }
#'
#' Supported distributions:
#' \itemize{
#'   \item \strong{beta}: parameters$shape1, parameters$shape2
#'   \item \strong{gamma}: parameters$shape, parameters$rate  
#'   \item \strong{lognormal}: parameters$meanlog, parameters$sdlog OR parameters$mean, parameters$sd
#'   \item \strong{normal}: parameters$mean, parameters$sd
#'   \item \strong{uniform}: parameters$min, parameters$max
#'   \item \strong{gompertz}: parameters$b, parameters$eta
#' }
#'
#' @examples
#' # Sample from a beta distribution
#' prior <- list(
#'   distribution = "beta",
#'   parameters = list(shape1 = 2, shape2 = 5)
#' )
#' sample_from_prior(n = 10, prior = prior)
#'
#' @export
#' @importFrom stats rbeta rgamma rlnorm rnorm runif
sample_from_prior <- function(n = 1, prior, verbose = FALSE) {
  
  # ---- Input validation ----
  if (!is.numeric(n) || n < 1 || n != round(n)) {
    stop("n must be a positive integer")
  }
  
  # ---- Handle NULL or missing priors ----
  if (missing(prior) || is.null(prior)) {
    if (verbose) message("Prior is NULL")
    return(rep(NA_real_, n))
  }
  
  # ---- Validate prior structure ----
  if (!is.list(prior)) {
    if (verbose) message("Prior is not a list")
    return(rep(NA_real_, n))
  }
  
  if (!all(c("distribution", "parameters") %in% names(prior))) {
    if (verbose) message("Prior missing required slots: 'distribution' and 'parameters'")
    return(rep(NA_real_, n))
  }
  
  dist_type <- tolower(prior$distribution)
  params <- prior$parameters
  
  # ---- Sample from distribution ----
  tryCatch({
    
    switch(dist_type,
      
      beta = {
        if (!all(c("shape1", "shape2") %in% names(params))) {
          stop("Beta distribution requires shape1 and shape2")
        }
        if (is.na(params$shape1) || is.na(params$shape2)) {
          if (verbose) message("Beta parameters contain NA values")
          return(rep(NA_real_, n))
        }
        if (params$shape1 <= 0 || params$shape2 <= 0) {
          stop("Beta requires shape1 > 0 and shape2 > 0")
        }
        rbeta(n, shape1 = params$shape1, shape2 = params$shape2)
      },
      
      gamma = {
        if (!all(c("shape", "rate") %in% names(params))) {
          stop("Gamma distribution requires shape and rate")
        }
        if (is.na(params$shape) || is.na(params$rate)) {
          if (verbose) message("Gamma parameters contain NA values")
          return(rep(NA_real_, n))
        }
        if (params$shape <= 0 || params$rate <= 0) {
          stop("Gamma requires shape > 0 and rate > 0")
        }
        rgamma(n, shape = params$shape, rate = params$rate)
      },
      
      lognormal = {
        if (!is.null(params$meanlog) && !is.null(params$sdlog)) {
          # Standard parameterization
          if (params$sdlog <= 0) stop("Lognormal requires sdlog > 0")
          rlnorm(n, meanlog = params$meanlog, sdlog = params$sdlog)
          
        } else if (!is.null(params$mean) && !is.null(params$sd)) {
          # Convert from mean/sd
          if (params$mean <= 0) stop("Lognormal mean must be positive")
          if (params$sd <= 0) stop("Lognormal sd must be positive")
          
          cv2 <- (params$sd / params$mean)^2
          sdlog <- sqrt(log(1 + cv2))
          meanlog <- log(params$mean) - sdlog^2/2
          rlnorm(n, meanlog = meanlog, sdlog = sdlog)
          
        } else {
          stop("Lognormal requires (meanlog, sdlog) or (mean, sd)")
        }
      },
      
      normal = {
        if (!all(c("mean", "sd") %in% names(params))) {
          stop("Normal distribution requires mean and sd")
        }
        # Check for NA values
        if (is.na(params$mean) || is.na(params$sd)) {
          if (verbose) message("Normal parameters contain NA values")
          return(rep(NA_real_, n))
        }
        if (params$sd < 0) stop("Normal requires sd >= 0")
        rnorm(n, mean = params$mean, sd = params$sd)
      },
      
      uniform = {
        if (!all(c("min", "max") %in% names(params))) {
          stop("Uniform distribution requires min and max")
        }
        if (params$min >= params$max) {
          stop("Uniform requires min < max")
        }
        runif(n, min = params$min, max = params$max)
      },
      
      gompertz = {
        if (!all(c("b", "eta") %in% names(params))) {
          stop("Gompertz distribution requires b and eta")
        }
        if (is.na(params$b) || is.na(params$eta)) {
          if (verbose) message("Gompertz parameters contain NA values")
          return(rep(NA_real_, n))
        }
        if (params$b <= 0 || params$eta <= 0) {
          stop("Gompertz requires b > 0 and eta > 0")
        }
        
        # Check if rgompertz function exists (from fit_gompertz_from_ci.R)
        if (exists("rgompertz", mode = "function")) {
          rgompertz(n, b = params$b, eta = params$eta)
        } else {
          # Fallback: implement basic Gompertz sampling using inverse transform
          u <- runif(n)
          (1/params$b) * log(1 - (params$b/params$eta) * log(1 - u))
        }
      },
      
      # Default case for unknown distribution
      stop(sprintf("Unknown distribution type: %s", dist_type))
    )
    
  }, error = function(e) {
    if (verbose) message(sprintf("Error sampling: %s", e$message))
    rep(NA_real_, n)
  })
}