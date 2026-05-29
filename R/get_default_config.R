#' Create Default LASER Configuration
#'
#' Generates a default configuration for running LASER model simulations.
#' Automatically defines default values of all model parameters and initial
#' conditions for the cholera metapopulation transmission model. The function uses \code{\link{make_LASER_config}} to check
#' that all parameter values meet model specifications.
#'
#' @param PATHS Optional. Retained for backwards compatibility; no longer used.
#'        The default config is now located via `system.file()` so the function
#'        works from any installed MOSAIC package.
#'
#' @return
#' A list object containing all necessary parameters and initial conditions for LASER
#' model simulation.
#'
#' @examples
#' \dontrun{
#' default_config <- get_default_config()
#' }
#'
#' @export
#'

get_default_config <- function(PATHS = NULL) {

     path <- system.file("extdata", "default_parameters.json", package = "MOSAIC")
     if (!nzchar(path)) {
          stop("Could not locate default_parameters.json in installed MOSAIC package.")
     }
     MOSAIC::read_json_to_list(path)

}
