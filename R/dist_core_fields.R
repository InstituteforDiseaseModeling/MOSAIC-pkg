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
    lognormal  = c("meanlog", "sdlog"),
    normal     = c("mean", "sd"),
    truncnorm  = c("mean", "sd", "a", "b"),
    uniform    = c("min", "max"),
    gompertz   = c("b", "eta"),
    fixed      = c("value"),
    frozen     = c("value")
  )
}
