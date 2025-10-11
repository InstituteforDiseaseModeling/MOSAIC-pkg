#' Estimated Parameters Inventory
#'
#' A comprehensive inventory of all stochastic parameters that are estimated
#' through Bayesian sampling in the MOSAIC cholera transmission modeling framework.
#' This dataset contains metadata, categorization, and ordering information for
#' parameters that have prior distributions and are part of the parameter
#' estimation process.
#'
#' @format A data frame with 43 rows and 11 columns:
#' \describe{
#'   \item{parameter_name}{Character. Parameter names as used in configuration files and sampling functions}
#'   \item{display_name}{Character. Human-readable display names for plotting and reporting}
#'   \item{description}{Character. Detailed descriptions of what each parameter represents}
#'   \item{units}{Character. Units of measurement for each parameter}
#'   \item{distribution}{Character. Prior distribution type (beta, gamma, lognormal, uniform, normal, gompertz, truncnorm, derived)}
#'   \item{scale}{Character. Parameter scale: "global" (same value across all locations) or "location" (varies by location)}
#'   \item{category}{Character. Biological/functional category: transmission, environmental, disease, immunity, surveillance, mobility, spatial, initial_conditions, seasonality}
#'   \item{order}{Integer. Overall ordering for systematic presentation (1-43)}
#'   \item{order_scale}{Character. Scale-based ordering (01 = global, 02 = location)}
#'   \item{order_category}{Character. Category-based ordering within each scale}
#'   \item{order_parameter}{Character. Parameter-based ordering within each category}
#' }
#'
#' @details
#' This inventory focuses exclusively on **estimated parameters** - those with
#' prior distributions that are sampled during Bayesian parameter estimation.
#' It does not include:
#' \itemize{
#'   \item Fixed model constants
#'   \item Non-stochastic derived quantities
#'   \item Intermediate calculations
#'   \item Data inputs (observed cases, demographics, etc.)
#' }
#'
#' The inventory is organized hierarchically:
#' \itemize{
#'   \item **Global parameters** (22): Same value across all locations
#'   \item **Location-specific parameters** (21): Vary by geographic location
#' }
#'
#' Categories reflect biological processes in cholera transmission:
#' \itemize{
#'   \item **transmission**: Population mixing, frequency dependence
#'   \item **environmental**: V. cholerae survival, shedding, dose-response
#'   \item **disease**: Recovery rates, incubation, symptom proportions
#'   \item **immunity**: Natural and vaccine-induced protection
#'   \item **surveillance**: Reporting rates and delays
#'   \item **mobility**: Human movement parameters
#'   \item **spatial**: Geographic covariates (WASH coverage)
#'   \item **initial_conditions**: Starting compartment proportions
#'   \item **seasonality**: Temporal transmission patterns
#' }
#'
#' @section Usage:
#' This dataset is used by:
#' \itemize{
#'   \item \code{\link{sample_parameters}} for Bayesian parameter sampling
#'   \item Plotting functions for consistent parameter ordering
#'   \item Model validation and convergence diagnostics
#'   \item Documentation and reporting functions
#' }
#'
#' @section Data Sources:
#' Parameter metadata compiled from:
#' \itemize{
#'   \item \code{\link{priors_default}} - Prior distributions
#'   \item \code{\link{config_default}} - Configuration templates
#'   \item Literature on cholera transmission modeling
#'   \item Expert knowledge of epidemic processes
#' }
#'
#' @examples
#' # Load the estimated parameters inventory
#' data(estimated_parameters)
#'
#' # View parameter structure
#' str(estimated_parameters)
#'
#' # Global vs location-specific parameters
#' table(estimated_parameters$scale)
#'
#' # Parameters by biological category
#' table(estimated_parameters$category)
#'
#' # Parameters by distribution type
#' table(estimated_parameters$distribution)
#'
#' # Transmission parameters
#' subset(estimated_parameters, category == "transmission")
#'
#' # Location-specific initial conditions
#' subset(estimated_parameters,
#'        scale == "location" & category == "initial_conditions")
#'
#' @seealso
#' \code{\link{sample_parameters}}, \code{\link{priors_default}}, \code{\link{config_default}}
#'
#' @source Created by \code{data-raw/make_parameters_inventory.R}
#' @author John Giles
#' @keywords datasets
"estimated_parameters"