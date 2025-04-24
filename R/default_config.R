#' Default LASER Configuration
#'
#' A pre-generated default configuration object for running LASER model simulations.
#' This object contains default values of all model parameters and initial conditions
#' required for the cholera metapopulation transmission model.
#'
#' @format A list object with named elements defining model parameters and initial conditions.
#'
#' @details
#' This object includes predefined defaults suitable for running LASER simulations across
#' multiple locations. These defaults assume the existence of input data structures and
#' formats compatible with the MOSAIC framework. The object includes references to expected
#' file paths, such as:
#' \describe{
#'   \item{MODEL_INPUT}{Character string. Path to directory containing model input CSV files.}
#'   \item{DATA_WHO_DAILY}{Character string. Path to directory containing processed WHO cholera daily CSV data.}
#' }
#'
#' @usage
#' default_config
#'
#' @seealso \code{\link{make_LASER_config}}
#'
#' @keywords datasets
"default_config"
