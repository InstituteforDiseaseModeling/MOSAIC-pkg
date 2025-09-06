#' Get Parameter Names from MOSAIC Objects
#'
#' Extracts parameter names from MOSAIC model configuration objects or results matrices,
#' organizing them into global parameters and location-specific parameters by ISO code.
#' This function is useful for organizing and categorizing parameters for analysis and
#' visualization workflows.
#'
#' @param object A MOSAIC configuration list object (like \code{config_default}) or
#'   a named numeric vector/matrix created by \code{convert_config_to_matrix()}.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{all}{Character vector of all parameter names found in the object}
#'   \item{global}{Character vector of global (scalar) parameter names}
#'   \item{location}{Named list where each element is a character vector of
#'     location-specific parameter names for that ISO code}
#' }
#'
#' @details
#' The function distinguishes between:
#' \itemize{
#'   \item \strong{Global parameters}: Scalar parameters that apply to all locations
#'     (e.g., "phi_1", "omega_2", "epsilon")
#'   \item \strong{Location-specific parameters}: Parameters that vary by location,
#'     either stored as vectors (in config objects) or with ISO suffixes (in results matrices)
#' }
#'
#' For config objects, location-specific parameters are identified by their base names
#' and the presence of location names. For results matrices, location-specific parameters
#' are identified by their "_ISO" suffix pattern (e.g., "S_j_initial_ETH").
#'
#' @examples
#' \dontrun{
#' # With a config object
#' config <- MOSAIC::config_default
#' param_names <- get_param_names(config)
#'
#' # View all parameter names
#' param_names$all
#'
#' # View global parameters only
#' param_names$global
#'
#' # View parameters for specific location
#' param_names$location$ETH
#'
#' # With a results matrix
#' config_sampled <- sample_parameters(seed = 123)
#' results_vec <- convert_config_to_matrix(config_sampled)
#' param_names <- get_param_names(results_vec)
#' }
#'
#' @seealso
#' \code{\link{convert_config_to_matrix}}, \code{\link{convert_config_to_dataframe}}
#'
#' @export
get_param_names <- function(object) {

    # ============================================================================
    # Input validation
    # ============================================================================

    if (missing(object) || is.null(object)) {
        stop("object argument is required and cannot be NULL")
    }

    # Check if object is a list (config) or vector/matrix (results)
    is_config <- is.list(object)
    is_results <- (is.vector(object) && !is.list(object)) || is.matrix(object)

    if (!is_config && !is_results) {
        stop("object must be either a list (config) or named vector/matrix (results)")
    }

    # ============================================================================
    # Define parameter sets (same as convert_config_to_matrix)
    # ============================================================================

    # All valid MOSAIC parameters
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
        "chi",
        "rho",
        "sigma",
        "mobility_omega",
        "mobility_gamma",
        "tau_i",
        "beta_j0_tot",
        "p_beta",
        "beta_j0_hum",
        "a_1_j",
        "a_2_j",
        "b_1_j",
        "b_2_j",
        "alpha_1",
        "alpha_2",
        "beta_j0_env",
        "zeta_1",
        "zeta_2",
        "kappa",
        "decay_days_short",
        "decay_days_long",
        "decay_shape_1",
        "decay_shape_2"
    )

    # Location-specific parameter base names
    location_params_base <- c(
        "S_j_initial", "E_j_initial", "I_j_initial",
        "R_j_initial", "V1_j_initial", "V2_j_initial",
        "N_j_initial",
        "beta_j0_tot", "p_beta",
        "beta_j0_env", "beta_j0_hum",
        "tau_i", "theta_j",
        "a_1_j", "a_2_j", "b_1_j", "b_2_j"
    )

    # ============================================================================
    # Process config object
    # ============================================================================

    if (is_config) {

        # Get parameter names that exist in the config
        all_names <- intersect(names(object), params_keep)
        # Remove location_name as it's metadata
        all_names <- setdiff(all_names, "location_name")

        # Get location names
        location_names <- object$location_name
        if (is.null(location_names)) {
            location_names <- character(0)
        }

        # Identify global vs location-specific parameters
        global_names <- character()
        location_list <- list()

        # Initialize location lists
        if (length(location_names) > 0) {
            for (iso in location_names) {
                location_list[[iso]] <- character()
            }
        }

        for (param_name in all_names) {
            param_value <- object[[param_name]]

            # Skip NULL values
            if (is.null(param_value)) next

            # Check if this is a location-specific parameter base name
            is_location_param <- param_name %in% location_params_base

            # Determine if global or location-specific based on length and type
            if (is_location_param && length(param_value) > 1 && length(param_value) == length(location_names)) {
                # Location-specific parameter (vector matching location count)
                for (iso in location_names) {
                    location_list[[iso]] <- c(location_list[[iso]], param_name)
                }
            } else if (!is_location_param) {
                # Global parameter
                global_names <- c(global_names, param_name)
            } else {
                # Single location or unclear - treat as global
                global_names <- c(global_names, param_name)
            }
        }

        # Remove empty location entries
        location_list <- location_list[lengths(location_list) > 0]

    } else {

        # ========================================================================
        # Process results matrix/vector
        # ========================================================================

        # Get all parameter names
        if (is.matrix(object)) {
            all_names <- colnames(object)
        } else {
            all_names <- names(object)
        }

        if (is.null(all_names)) {
            stop("Results object must have named columns or elements")
        }

        # Initialize outputs
        global_names <- character()
        location_list <- list()

        # Extract location codes from parameter names
        iso_pattern <- "_([A-Z]{3})$"
        location_params_with_iso <- all_names[grepl(iso_pattern, all_names)]
        location_isos <- unique(gsub(".*_([A-Z]{3})$", "\\1", location_params_with_iso))

        # Initialize location lists
        for (iso in location_isos) {
            location_list[[iso]] <- character()
        }

        # Categorize parameters
        for (param_name in all_names) {

            if (grepl(iso_pattern, param_name)) {
                # Location-specific parameter with ISO suffix
                iso_code <- gsub(".*_([A-Z]{3})$", "\\1", param_name)
                base_name <- gsub("_[A-Z]{3}$", "", param_name)
                location_list[[iso_code]] <- c(location_list[[iso_code]], param_name)
            } else {
                # Global parameter (no ISO suffix)
                global_names <- c(global_names, param_name)
            }
        }

        # Remove empty location entries
        location_list <- location_list[lengths(location_list) > 0]
    }

    # ============================================================================
    # Return results
    # ============================================================================

    # Sort all outputs
    all_param_names <- sort(c(global_names, unlist(location_list)))
    global_names <- sort(global_names)
    location_list <- lapply(location_list, sort)

    return(list(
        all = all_param_names,
        global = global_names,
        location = location_list
    ))
}
