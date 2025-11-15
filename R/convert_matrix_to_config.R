#' Convert Matrix Row to Config Object
#'
#' Converts a numeric vector (typically from a calibration results matrix row)
#' back into a MOSAIC config list structure. This is the inverse operation of
#' [convert_config_to_matrix()].
#'
#' @param param_vector Named numeric vector with parameter values, as returned
#'   by [convert_config_to_matrix()].
#' @param config_base Base config object to use as template. Parameters not
#'   found in param_vector will keep their base values.
#' @param col_params Optional integer vector specifying which columns correspond
#'   to parameters. If NULL, assumes all elements of param_vector are parameters.
#' @param sampling_flags Optional named list specifying which parameters were 
#'   originally sampled (TRUE) vs fixed (FALSE). Parameters with sampling_flags = FALSE
#'   will NOT be updated from param_vector, preserving original fixed values.
#'   If NULL, all parameters in param_vector will be updated (legacy behavior).
#'
#' @return A MOSAIC config list object with parameter values updated from
#'   param_vector, respecting original sampling intentions.
#'
#' @details
#' This function reconstructs a config object by:
#' \enumerate{
#'   \item Starting with config_base as template
#'   \item Parsing parameter names to identify structure (e.g., "beta_j0_hum_ETH")
#'   \item Checking sampling_flags to determine which parameters should be updated
#'   \item Updating only parameters that were originally sampled (if sampling_flags provided)
#'   \item Handling both scalar and vector parameters appropriately
#' }
#'
#' **IMPORTANT**: The sampling_flags parameter addresses a critical bug where parameters
#' intended to be fixed (sample_* = FALSE) were being overwritten during matrix reconstruction,
#' making them appear stochastic in downstream analyses.
#'
#' Parameter naming conventions expected:
#' \itemize{
#'   \item Scalar parameters: "param_name" (e.g., "phi_1", "gamma_2")
#'   \item Location-specific: "param_name_LOCATION" (e.g., "beta_j0_hum_ETH")
#'   \item Indexed parameters: "param_name_1", "param_name_2", etc.
#' }
#'
#' @seealso [convert_config_to_matrix()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Legacy usage (updates all parameters)
#' config_orig <- sample_parameters(PATHS, seed = 123)
#' param_vec <- convert_config_to_matrix(config_orig)
#' config_reconstructed <- convert_matrix_to_config(param_vec, config_orig)
#' 
#' # Preserving sampling intent (recommended)
#' sampling_flags <- list(alpha_1 = FALSE, phi_1 = TRUE, beta_j0_hum = TRUE)
#' config_best <- convert_matrix_to_config(param_row, config_base, 
#'                                        sampling_flags = sampling_flags)
#' 
#' # Use with calibration results
#' param_row <- results[best_idx, col_params]
#' names(param_row) <- param_names
#' config_best <- convert_matrix_to_config(param_row, config_base,
#'                                        sampling_flags = original_sampling_flags)
#' }
convert_matrix_to_config <- function(param_vector, config_base, col_params = NULL, 
                                   sampling_flags = NULL) {
    
    # ============================================================================
    # Input validation
    # ============================================================================
    
    if (missing(param_vector) || is.null(param_vector)) {
        stop("param_vector argument is required and cannot be NULL")
    }
    
    if (missing(config_base) || is.null(config_base) || !is.list(config_base)) {
        stop("config_base must be a valid config list object")
    }
    
    # Validate sampling_flags if provided
    if (!is.null(sampling_flags) && !is.list(sampling_flags)) {
        stop("sampling_flags must be a named list or NULL")
    }
    
    # Extract relevant parameters if col_params specified
    if (!is.null(col_params)) {
        if (is.numeric(col_params) && all(col_params <= length(param_vector))) {
            param_vector <- param_vector[col_params]
        } else {
            stop("col_params must be valid indices for param_vector")
        }
    }
    
    # Ensure param_vector has names
    if (is.null(names(param_vector))) {
        stop("param_vector must have parameter names")
    }
    
    # ============================================================================
    # Initialize output config from base
    # ============================================================================
    
    config_updated <- config_base
    param_names <- names(param_vector)
    
    # Get location names for handling location-specific parameters
    location_names <- config_base$location_name
    n_locations <- length(location_names)
    
    # ============================================================================
    # Helper function to check if parameter should be updated
    # ============================================================================
    
    should_update_parameter <- function(param_name, sampling_flags) {
        # If no sampling flags provided, update all parameters (legacy behavior)
        if (is.null(sampling_flags)) return(TRUE)
        
        # Check if this specific parameter was sampled
        if (param_name %in% names(sampling_flags)) {
            return(isTRUE(sampling_flags[[param_name]]))
        }
        
        # For location-specific parameters, check base parameter name
        # e.g., "beta_j0_hum_ETH" -> check "beta_j0_hum"
        for (j in seq_along(location_names)) {
            loc_suffix <- paste0("_", location_names[j])
            if (endsWith(param_name, loc_suffix)) {
                base_param <- substr(param_name, 1, nchar(param_name) - nchar(loc_suffix))
                if (base_param %in% names(sampling_flags)) {
                    return(isTRUE(sampling_flags[[base_param]]))
                }
            }
        }
        
        # For indexed parameters, check base parameter name
        # e.g., "param_name_1" -> check "param_name"
        if (grepl("_\\d+$", param_name)) {
            idx_match <- regmatches(param_name, regexpr("_\\d+$", param_name))
            base_param <- substr(param_name, 1, nchar(param_name) - nchar(idx_match))
            if (base_param %in% names(sampling_flags)) {
                return(isTRUE(sampling_flags[[base_param]]))
            }
        }
        
        # Default: if not found in sampling_flags, assume it was sampled (legacy behavior)
        return(TRUE)
    }
    
    # ============================================================================
    # Parse and update parameters
    # ============================================================================
    
    for (i in seq_along(param_vector)) {
        param_name <- param_names[i]
        param_value <- as.numeric(param_vector[i])
        
        # Skip if value is NA
        if (is.na(param_value)) next
        
        # **CRITICAL FIX**: Check if this parameter should be updated
        # This prevents overwriting parameters that were intended to be fixed
        if (!should_update_parameter(param_name, sampling_flags)) {
            next  # Skip updating this parameter - keep config_base value
        }
        
        # Check for location-specific parameters (end with location name)
        is_location_param <- FALSE
        base_param_name <- param_name
        location_idx <- NULL
        
        if (!is.null(location_names)) {
            for (j in seq_along(location_names)) {
                loc_suffix <- paste0("_", location_names[j])
                if (endsWith(param_name, loc_suffix)) {
                    is_location_param <- TRUE
                    base_param_name <- substr(param_name, 1, nchar(param_name) - nchar(loc_suffix))
                    location_idx <- j
                    break
                }
            }
        }
        
        # Check for indexed parameters (end with "_1", "_2", etc.)
        is_indexed_param <- FALSE
        element_idx <- NULL
        
        if (!is_location_param) {
            # Look for pattern ending with "_[number]"
            if (grepl("_\\d+$", param_name)) {
                idx_match <- regmatches(param_name, regexpr("_\\d+$", param_name))
                element_idx <- as.numeric(substr(idx_match, 2, nchar(idx_match)))
                base_param_name <- substr(param_name, 1, nchar(param_name) - nchar(idx_match))
                is_indexed_param <- TRUE
            }
        }
        
        # Update config based on parameter type
        if (is_location_param && !is.null(location_idx)) {
            # Location-specific parameter
            if (base_param_name %in% names(config_updated)) {
                if (is.null(config_updated[[base_param_name]])) {
                    # Initialize as vector if NULL
                    config_updated[[base_param_name]] <- rep(NA_real_, n_locations)
                }
                if (length(config_updated[[base_param_name]]) >= location_idx) {
                    config_updated[[base_param_name]][location_idx] <- param_value
                }
            }
            
        } else if (is_indexed_param && !is.null(element_idx)) {
            # Indexed parameter
            if (base_param_name %in% names(config_updated)) {
                if (is.null(config_updated[[base_param_name]])) {
                    # Initialize as vector if NULL
                    config_updated[[base_param_name]] <- rep(NA_real_, element_idx)
                } else if (length(config_updated[[base_param_name]]) < element_idx) {
                    # Extend vector if needed
                    length(config_updated[[base_param_name]]) <- element_idx
                }
                config_updated[[base_param_name]][element_idx] <- param_value
            }
            
        } else {
            # Scalar parameter
            if (base_param_name %in% names(config_updated)) {
                config_updated[[base_param_name]] <- param_value
            }
        }
    }
    
    # ============================================================================
    # Convert logical flags back from 0/1
    # ============================================================================
    
    # Define parameters that should be logical
    logical_params <- c("sample_initial_conditions", "sample_global_parameters", 
                       "sample_location_parameters", "use_vaccination")
    
    for (logical_param in logical_params) {
        if (logical_param %in% names(config_updated) && 
            is.numeric(config_updated[[logical_param]])) {
            config_updated[[logical_param]] <- as.logical(config_updated[[logical_param]])
        }
    }
    
    # ============================================================================
    # Return updated config
    # ============================================================================
    
    return(config_updated)
}

#' Detect Parameter Sampling Flags from Config
#'
#' Attempts to infer which parameters were originally sampled vs fixed by analyzing
#' parameter variance across multiple configs. This is a fallback when original
#' sampling flags are not available.
#'
#' @param configs List of config objects from parameter sampling
#' @param variance_threshold Parameters with variance below this threshold are
#'   considered "fixed". Default 1e-10.
#'
#' @return Named list of logical values indicating which parameters appear to be sampled
#'
#' @details
#' This function compares parameter values across multiple config objects to identify
#' which parameters show variation (likely sampled) vs those that remain constant
#' (likely fixed). This is a heuristic approach when original sampling metadata is lost.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate multiple configs with some fixed parameters
#' configs <- lapply(1:10, function(i) {
#'   sample_parameters(PATHS, seed = i, sample_alpha_1 = FALSE, sample_phi_1 = TRUE)
#' })
#' sampling_flags <- detect_sampled_parameters_from_configs(configs)
#' }
detect_sampled_parameters_from_configs <- function(configs, variance_threshold = 1e-10) {
    
    if (length(configs) < 2) {
        warning("Need at least 2 configs to detect parameter sampling patterns")
        return(NULL)
    }
    
    # Convert all configs to matrices
    param_matrices <- lapply(configs, function(config) {
        tryCatch({
            convert_config_to_matrix(config)
        }, error = function(e) {
            warning("Failed to convert config to matrix: ", e$message)
            NULL
        })
    })
    
    # Remove NULL entries
    param_matrices <- param_matrices[!sapply(param_matrices, is.null)]
    
    if (length(param_matrices) < 2) {
        warning("Not enough valid parameter matrices to analyze")
        return(NULL)
    }
    
    # Ensure all matrices have same parameters
    param_names <- names(param_matrices[[1]])
    all_same_params <- all(sapply(param_matrices[-1], function(m) {
        identical(names(m), param_names)
    }))
    
    if (!all_same_params) {
        warning("Parameter matrices have different structures - results may be unreliable")
    }
    
    # Calculate variance for each parameter
    param_variances <- sapply(param_names, function(pname) {
        values <- sapply(param_matrices, function(m) m[[pname]])
        if (all(is.na(values))) return(NA)
        var(values, na.rm = TRUE)
    })
    
    # Classify parameters as sampled (high variance) or fixed (low variance)
    sampling_flags <- param_variances > variance_threshold
    names(sampling_flags) <- param_names
    
    return(as.list(sampling_flags))
}

#' Store Sampling Metadata in Config
#'
#' Adds sampling flag metadata to a config object for later retrieval during
#' matrix reconstruction. This enables preservation of sampling intent throughout
#' the parameter handling pipeline.
#'
#' @param config Config object to annotate
#' @param ... Sampling flag arguments (e.g., sample_alpha_1 = FALSE)
#'
#' @return Config object with sampling metadata stored in \$__sampling_metadata__
#'
#' @details
#' This function stores the sampling flags used during parameter generation so they
#' can be retrieved later when reconstructing configs from parameter matrices.
#' The metadata is stored in a special field to avoid conflicts with model parameters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config <- sample_parameters(PATHS, seed = 123, sample_alpha_1 = FALSE)
#' config <- store_sampling_metadata(config, sample_alpha_1 = FALSE, sample_phi_1 = TRUE)
#'
#' # Later retrieve the metadata
#' sampling_flags <- extract_sampling_metadata(config)
#' }
store_sampling_metadata <- function(config, ...) {
    
    sampling_args <- list(...)
    
    if (length(sampling_args) > 0) {
        config$`__sampling_metadata__` <- sampling_args
    }
    
    return(config)
}

#' Extract Sampling Metadata from Config
#'
#' Retrieves sampling flag metadata from a config object that was created with
#' [store_sampling_metadata()].
#'
#' @param config Config object containing sampling metadata
#'
#' @return Named list of sampling flags, or NULL if no metadata found
#'
#' @export
#'
#' @examples
#' \dontrun{
#' config <- sample_parameters(PATHS, seed = 123)
#' config <- store_sampling_metadata(config, sample_alpha_1 = FALSE)
#' 
#' sampling_flags <- extract_sampling_metadata(config)
#' print(sampling_flags$sample_alpha_1)  # FALSE
#' }
extract_sampling_metadata <- function(config) {
    
    if (is.null(config) || !is.list(config)) {
        return(NULL)
    }
    
    metadata <- config$`__sampling_metadata__`
    
    if (is.null(metadata) || !is.list(metadata)) {
        return(NULL)
    }
    
    return(metadata)
}