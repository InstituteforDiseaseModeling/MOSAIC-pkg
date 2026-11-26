#' Get Parameter Names from MOSAIC Objects
#'
#' Extracts parameter names from MOSAIC model configuration objects or results matrices,
#' organizing them into global parameters and location-specific parameters by location.
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
#'     location-specific parameter names for that location (an ISO code for national
#'     configs, or any location name)}
#' }
#'
#' @details
#' The function distinguishes between:
#' \itemize{
#'   \item \strong{Global parameters}: Scalar parameters that apply to all locations
#'     (e.g., "phi_1", "omega_2", "epsilon")
#'   \item \strong{Location-specific parameters}: Parameters that vary by location,
#'     either stored as vectors (in config objects) or with location suffixes (in results matrices)
#' }
#'
#' For config objects, location-specific parameters are identified by their base names
#' and the presence of location names. For results matrices, location-specific parameters
#' are identified by their location suffix (e.g., "S_j_initial_ETH" or
#' "S_j_initial_MOZ_Cabo_Delgado"); location names need not be ISO codes.
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

    params_keep <- .MOSAIC_CONFIG_PARAMS_KEEP

    # Location-specific parameter base names. N_j_initial and alpha_1 are added
    # here because a config classifies by vector length: they are per-location
    # whenever the config holds more than one location.
    location_params_base <- c(.MOSAIC_CONFIG_LOCATION_PARAMS, "N_j_initial", "alpha_1")

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

        # Location of each parameter name ("<base>_<location>"; NA for global ones)
        split <- .mosaic_split_param_names(all_names)

        # Initialize location lists
        for (loc in unique(split$location[!is.na(split$location)])) {
            location_list[[loc]] <- character()
        }

        # Categorize parameters
        for (k in seq_along(all_names)) {
            loc <- split$location[k]
            if (!is.na(loc)) {
                location_list[[loc]] <- c(location_list[[loc]], all_names[k])
            } else {
                global_names <- c(global_names, all_names[k])
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
