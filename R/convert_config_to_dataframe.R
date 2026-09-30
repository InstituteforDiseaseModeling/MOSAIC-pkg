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


     # Same parameter set as convert_config_to_matrix(), so the two agree column for column
     config_subset <- config[names(config) %in% .MOSAIC_CONFIG_PARAMS_KEEP]

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

          # Check if this parameter is location-specific
          is_location_param <- param_name %in% .MOSAIC_CONFIG_LOCATION_PARAMS
          
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
