#' Create Default LASER Configuration
#'
#' Generates a default configuration for running LASER model simulations.
#' Automatically defines default values of all model parameters and initial
#' conditions for the cholera metapopulation transmission model. The function uses \code{\link{make_LASER_config}} to check
#' that all parameter values meet model specifications.
#'
#' @param PATHS A named list of file path components required for accessing
#' input data. Expected list elements include:
#' \describe{
#'   \item{MODEL_INPUT}{Character string. Path to directory containing model input CSV files.}
#'   \item{DATA_WHO_DAILY}{Character string. Path to directory containing processed WHO cholera daily CSV data.}
#' }
#'
#' @return
#' A list object containing all necessary parameters and initial conditions for LASER
#' model simulation.
#'
#' @examples
#' \dontrun{
#' PATHS <- list(
#'   MODEL_INPUT = "path/to/model_input",
#'   DATA_WHO_DAILY = "path/to/daily_data"
#' )
#'
#' default_config <- get_default_config(PATHS)
#' }
#'
#' @export
#'

get_default_config <- function(PATHS) {

     path <- file.path(PATHS$ROOT,"MOSAIC-pkg", "inst", "extdata", "default_parameters.json")
     out <- jsonlite::fromJSON(path)
     return(out)

}
