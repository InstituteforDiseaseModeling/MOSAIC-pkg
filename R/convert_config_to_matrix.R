#' Convert Config to Numeric Vector for Matrix Storage
#'
#' Converts a complex MOSAIC config list object directly into a numeric vector
#' suitable for storage in a matrix. This function is optimized for calibration
#' workflows where parameters need to be stored efficiently in pre-allocated matrices.
#'
#' @param config A configuration list object in the format of MOSAIC::config_default.
#'   The config object should contain both global parameters (scalars) and
#'   location-specific parameters (vectors/lists).
#'
#' @return A named numeric vector where:
#'   \itemize{
#'     \item All parameter values are converted to numeric representation
#'     \item Logical values become 0 (FALSE) or 1 (TRUE)
#'     \item Character values are converted to NA with a warning
#'     \item Vector parameters are expanded with appropriate suffixes
#'     \item Names preserve the parameter structure for later reconstruction
#'   }
#'
#' @details
#' This function is more efficient than \code{convert_config_to_dataframe()} when
#' the goal is to store parameters in a numeric matrix, as it avoids the overhead
#' of creating an intermediate data.frame. It processes parameters directly into
#' a numeric vector while maintaining the same naming convention.
#'
#' The function handles:
#' \itemize{
#'   \item Scalar parameters: Direct numeric conversion
#'   \item Location-specific vectors: Expanded with location suffixes
#'   \item Node-level parameters: Expanded with numeric indices
#'   \item Logical values: Converted to 0/1
#'   \item NULL values: Skipped
#'   \item Non-numeric values: Converted to NA with warning
#' }
#'
#' @seealso \code{\link{convert_config_to_dataframe}} for data.frame output
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert sampled config for matrix storage
#' config_sampled <- sample_parameters(PATHS, seed = 123)
#' vec_params <- convert_config_to_matrix(config_sampled)
#' 
#' # Use in calibration workflow
#' n_sim <- 100
#' n_params <- length(vec_params)
#' results_matrix <- matrix(NA_real_, nrow = n_sim, ncol = 4 + n_params)
#' results_matrix[1, 5:ncol(results_matrix)] <- vec_params
#' 
#' # Compare with dataframe approach (slower)
#' df_params <- convert_config_to_dataframe(config_sampled)
#' vec_params_slow <- as.numeric(df_params)
#' all.equal(vec_params, vec_params_slow)  # Should be TRUE
#' }
convert_config_to_matrix <- function(config) {
     
     # ============================================================================
     # Input validation
     # ============================================================================
     
     if (missing(config) || is.null(config)) {
          stop("config argument is required and cannot be NULL")
     }
     
     if (!is.list(config)) {
          stop("config must be a list object")
     }
     
     # ============================================================================
     # Define parameters to keep (same as convert_config_to_dataframe)
     # ============================================================================
     
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
          "prop_S_initial",     # Added: Initial condition proportions
          "prop_E_initial",
          "prop_I_initial",
          "prop_R_initial",
          "prop_V1_initial",
          "prop_V2_initial",
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
     # Initialize output vector
     # ============================================================================
     
     output_vec <- numeric()
     output_names <- character()
     
     # Get location names if available
     location_names <- config_subset$location_name
     n_locations <- length(location_names)
     
     # Define location-specific parameter base names
     location_params_base <- c(
          "S_j_initial", "E_j_initial", "I_j_initial",
          "R_j_initial", "V1_j_initial", "V2_j_initial",
          "prop_S_initial", "prop_E_initial", "prop_I_initial",   # Added: Initial condition proportions
          "prop_R_initial", "prop_V1_initial", "prop_V2_initial",
          "beta_j0_env", "beta_j0_hum", "beta_j0_tot", "p_beta",
          "tau_i", "theta_j",
          "a_1_j", "a_2_j", "b_1_j", "b_2_j",
          "a1", "a2", "b1", "b2",
          "mu_j",                                        # Case fatality ratio
          "psi_star_a", "psi_star_b", "psi_star_z", "psi_star_k"  # psi_star calibration parameters
     )
     
     # ============================================================================
     # Process each parameter
     # ============================================================================
     
     for (param_name in names(config_subset)) {
          param_value <- config_subset[[param_name]]
          
          # Skip NULL values
          if (is.null(param_value)) next
          
          # Skip location_name itself as it's metadata
          if (param_name == "location_name") next
          
          # Check if this parameter is location-specific
          is_location_param <- param_name %in% location_params_base
          
          # Convert to numeric (handles logical -> 0/1 conversion)
          if (is.logical(param_value)) {
               param_value <- as.integer(param_value)
          } else if (is.character(param_value)) {
               warning(paste("Skipping character parameter:", param_name))
               next
          }
          
          # Handle scalar parameters or single-location vectors
          if (length(param_value) == 1) {
               # If it's a location-specific parameter and we have location names, add suffix
               if (is_location_param && !is.null(location_names) && n_locations == 1) {
                    col_name <- paste0(param_name, "_", location_names[1])
               } else {
                    # Otherwise keep as is (global parameter or no location info)
                    col_name <- param_name
               }
               output_vec <- c(output_vec, as.numeric(param_value))
               output_names <- c(output_names, col_name)
          }
          
          # Handle vector parameters (location-specific or node-level)
          else if (length(param_value) > 1) {
               
               # Check if length matches number of locations
               if (!is.null(location_names) && length(param_value) == n_locations) {
                    # Create elements with location suffix
                    for (i in seq_along(param_value)) {
                         col_name <- paste0(param_name, "_", location_names[i])
                         output_vec <- c(output_vec, as.numeric(param_value[i]))
                         output_names <- c(output_names, col_name)
                    }
                    
               } else {
                    # For other vectors, create indexed elements
                    for (i in seq_along(param_value)) {
                         col_name <- paste0(param_name, "_", i)
                         output_vec <- c(output_vec, as.numeric(param_value[i]))
                         output_names <- c(output_names, col_name)
                    }
               }
          }
     }
     
     # ============================================================================
     # Return named numeric vector
     # ============================================================================
     
     if (length(output_vec) == 0) {
          warning("No valid numeric parameters found in config object")
          return(numeric())
     }
     
     names(output_vec) <- output_names
     return(output_vec)
}