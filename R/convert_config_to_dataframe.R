#' Convert Config to DataFrame
#'
#' Converts a complex MOSAIC config list object into a single-row dataframe with
#' organized named columns. This function handles both scalar parameters and
#' location-specific parameters, creating appropriately named columns for each.
#'
#' @param config A configuration list object in the format of MOSAIC::config_default.
#'   The config object should contain both global parameters (scalars) and
#'   location-specific parameters (vectors/lists).
#'
#' @return A single-row data.frame where:
#'   \itemize{
#'     \item Scalar parameters become single columns with their original names
#'     \item Vector parameters for multiple locations become columns named as
#'           "parameter_location" (e.g., "beta_j0_env_ETH", "beta_j0_env_KEN")
#'     \item Node-level parameters (matching population nodes) are expanded with
#'           node indices (e.g., "N_1", "N_2", etc.)
#'   }
#'
#' @details
#' The function intelligently handles different parameter types:
#' \itemize{
#'   \item Character/numeric scalars: Direct conversion to columns
#'   \item Named vectors/lists: Expanded to location-specific columns
#'   \item Unnamed vectors matching node count: Expanded to node-indexed columns
#'   \item Date fields: Converted to character representation
#'   \item NULL values: Skipped
#' }
#'
#' Location-specific parameters are identified by having names that match ISO3
#' country codes. Node-level parameters are identified by having length matching
#' the number of population nodes (N parameter).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert default config
#' df <- convert_config_to_dataframe(MOSAIC::config_default)
#'
#' # Convert sampled config
#' config_sampled <- sample_parameters(PATHS, seed = 123)
#' df_sampled <- convert_config_to_dataframe(config_sampled)
#'
#' # Convert location-specific config
#' config_eth <- get_location_config(iso = "ETH")
#' df_eth <- convert_config_to_dataframe(config_eth)
#'
#' # View structure
#' str(df)
#' names(df)
#' }
convert_config_to_dataframe <- function(config) {

     # ============================================================================
     # Input validation
     # ============================================================================

     if (missing(config) || is.null(config)) {
          stop("config argument is required and cannot be NULL")
     }

     if (!is.list(config)) {
          stop("config must be a list object")
     }


     params_keep <- c(
          "seed",
          "location_name",
          "N_j_initial",
          "S_j_initial",
          "E_j_initial",
          "I_j_initial",
          "R_j_initial",
          "V1_j_initial",
          "V2_j_initial",
          "phi_1",
          "phi_2",
          "omega_1",
          "omega_2",
          "iota",
          "gamma_1",
          "gamma_2",
          "epsilon",
          "rho",
          "sigma",
          "mobility_omega",
          "mobility_gamma",
          "tau_i",
          "beta_j0_hum",
          "beta_j0_env",
          "beta_j0_tot",        # Added: Total transmission rate (primary parameter)
          "p_beta",             # Added: Proportion of human-to-human transmission
          "theta_j",            # Added: WASH coverage parameter
          "a_1_j",
          "a_2_j",
          "b_1_j",
          "b_2_j",
          "a1",                 # Added: Alternative seasonality parameter names
          "a2",
          "b1",
          "b2",
          "alpha_1",
          "alpha_2",
          "zeta_1",
          "zeta_2",
          "kappa",
          "decay_days_short",
          "decay_days_long",
          "decay_shape_1",
          "decay_shape_2",
          "mu_j",               # Added: Case fatality ratio
          "psi_star_a",         # Added: psi_star calibration parameters
          "psi_star_b",
          "psi_star_z",
          "psi_star_k"
     )

     config_subset <- config[names(config) %in% params_keep]

     # ============================================================================
     # Create output dataframe
     # ============================================================================

     # Initialize empty list to hold all columns
     df_list <- list()

     # Get location names if available
     location_names <- config_subset$location_name
     n_locations <- length(location_names)

     # ============================================================================
     # Process each parameter
     # ============================================================================

     for (param_name in names(config_subset)) {
          param_value <- config_subset[[param_name]]

          # Skip NULL values
          if (is.null(param_value)) next

          # Skip location_name itself as it's metadata
          if (param_name == "location_name") next

          # Define location-specific parameter base names (same as in calibration script)
          location_params_base <- c(
               "S_j_initial", "E_j_initial", "I_j_initial",
               "R_j_initial", "V1_j_initial", "V2_j_initial",
               "beta_j0_env", "beta_j0_hum",
               "tau_i", "theta_j",
               "a_1_j", "a_2_j", "b_1_j", "b_2_j"
          )
          
          # Check if this parameter is location-specific
          is_location_param <- param_name %in% location_params_base
          
          # Handle scalar parameters or single-location vectors
          if (length(param_value) == 1) {
               # If it's a location-specific parameter and we have location names, add suffix
               if (is_location_param && !is.null(location_names) && n_locations == 1) {
                    col_name <- paste0(param_name, "_", location_names[1])
                    df_list[[col_name]] <- param_value
               } else {
                    # Otherwise keep as is (global parameter or no location info)
                    df_list[[param_name]] <- param_value
               }
          }

          # Handle vector parameters (location-specific or node-level)
          else if (length(param_value) > 1) {

               # Check if length matches number of locations
               if (!is.null(location_names) && length(param_value) == n_locations) {
                    # Create columns with location suffix
                    for (i in seq_along(param_value)) {
                         col_name <- paste0(param_name, "_", location_names[i])
                         df_list[[col_name]] <- param_value[i]
                    }

               } else {

                    # For other vectors, create indexed columns
                    for (i in seq_along(param_value)) {
                         col_name <- paste0(param_name, "_", i)
                         df_list[[col_name]] <- param_value[i]
                    }

               }
          }
     }

     # ============================================================================
     # Convert to single-row dataframe
     # ============================================================================

     if (length(df_list) == 0) {
          warning("No valid parameters found in config object")
          return(data.frame())
     }

     # Create dataframe with single row
     df <- as.data.frame(df_list, stringsAsFactors = FALSE)

     return(df)
}
