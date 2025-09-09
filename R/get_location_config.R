#' Get Location-Specific Configuration
#'
#' Extracts configuration for specific location(s) from a config object.
#' Systematically subsets all location-specific parameters to only include
#' the requested location(s).
#'
#' @param config A configuration list object in the format of MOSAIC::config_default.
#'   If NULL, will use MOSAIC::config_default.
#' @param iso Character vector of ISO3 country codes to extract (e.g., "ETH" or c("ETH", "KEN")).
#'
#' @return A config list object with the same structure as the input, but with
#'   all location-specific parameters subset to only the requested locations.
#'
#' @details
#' This function systematically identifies and subsets all location-specific
#' parameters in the config object. Location-specific parameters are identified
#' as vectors with length matching the number of locations in the original config.
#'
#' For single location extractions, vector parameters are converted to scalars
#' to maintain consistency with single-location configurations.
#'
#' Node-level parameters (N, S, V1, V2, E, I, R) are also properly subset based
#' on the cumulative node indices for each location.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract config for Ethiopia
#' eth_config <- get_location_config(iso = "ETH")
#'
#' # Extract config for multiple countries
#' east_africa_config <- get_location_config(
#'   config = MOSAIC::config_default,
#'   iso = c("ETH", "KEN", "UGA", "TZA")
#' )
#'
#' # Use with sampled parameters
#' config_sampled <- sample_parameters(seed = 123)
#' config_eth <- get_location_config(config_sampled, iso = "ETH")
#' }
get_location_config <- function(config = NULL, iso) {

     # ============================================================================
     # Input validation
     # ============================================================================

     if (missing(iso) || is.null(iso)) {
          stop("iso argument is required")
     }

     if (!is.character(iso)) {
          stop("iso must be a character vector")
     }

     if (length(iso) == 0) {
          stop("iso must contain at least one location")
     }

     # Load default config if not provided
     if (is.null(config)) {
          if (!requireNamespace("MOSAIC", quietly = TRUE)) {
               stop("MOSAIC package not found. Please provide config object explicitly.")
          }
          config <- MOSAIC::config_default
     }

     if (!is.list(config)) {
          stop("config must be a list object")
     }

     if (is.null(config$location_name)) {
          stop("config must contain 'location_name' field")
     }

     # ============================================================================
     # Identify location indices
     # ============================================================================

     # Get original locations
     original_locations <- config$location_name
     n_original_locations <- length(original_locations)

     # Ensure ISO codes are uppercase
     iso <- toupper(iso)

     # Check that all requested locations exist
     missing_locations <- setdiff(iso, original_locations)
     if (length(missing_locations) > 0) {
          stop(paste0(
               "The following location(s) not found in config: ",
               paste(missing_locations, collapse = ", "),
               "\nAvailable locations: ",
               paste(sort(original_locations), collapse = ", ")
          ))
     }

     sel <- which(config$location_name %in% iso)

     # ============================================================================
     # Start with full config and systematically update it
     # ============================================================================

     out <- config
     out$location_name <- out$location_name[sel]

     location_params <- c(
          names(out)[grep( '_initial', names(out))],
          "longitude", "latitude",
          "tau_i", "theta_j",
          "beta_j0_hum", "beta_j0_env",
          "beta_j0_tot", "p_beta",  # Added: handle new transmission parameters
          "a_1_j", "a_2_j", "b_1_j", "b_2_j",
          "mu_j", "mu_j_slope"  # Added: handle location-specific CFR parameters
     )

     for (l in location_params) {

          out[[l]] <- out[[l]][sel]

     }


     location_params <- c(
          names(out)[grep( '_jt', names(out))],
          "reported_cases", "reported_deaths"
     )

     for (l in location_params) {
          # Use drop=FALSE to maintain matrix structure even for single location
          out[[l]] <- out[[l]][sel, , drop = FALSE]
     }



     return(out)
}
