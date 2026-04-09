#' Canonical distribution parameter fields
#'
#' Returns the canonical parameter field names for each supported distribution
#' family. Used by posterior fitting and prior updating to strip diagnostic
#' metadata and keep only the fields required by \code{\link{sample_from_prior}}.
#'
#' @return Named list where each element is a character vector of canonical
#'   parameter names for that distribution family.
#'
#' @keywords internal
.mosaic_dist_core_fields <- function() {
  list(
    beta       = c("shape1", "shape2"),
    gamma      = c("shape", "rate"),
    # Lognormal always normalizes to meanlog/sdlog after posterior fitting.
    # Priors defined with mean/sd are converted on first round-trip through
    # the posterior pipeline. Both forms are supported by sample_from_prior()
    # and inflate_priors(); the meanlog/sdlog form gives more precise analytic
    # inflation (CV-scaling vs approximate sd-scaling).
    lognormal  = c("meanlog", "sdlog"),
    normal     = c("mean", "sd"),
    truncnorm  = c("mean", "sd", "a", "b"),
    uniform    = c("min", "max"),
    gompertz   = c("b", "eta"),
    fixed      = c("value"),
    frozen     = c("value")
  )
}
