#' Get Location-Specific Priors
#'
#' Extracts priors for specific location(s) from a priors object while maintaining
#' the exact structure of MOSAIC::priors_default. Keeps all global parameters and
#' filters location-specific parameters to only include the requested location(s).
#'
#' @param iso Character vector of ISO3 country codes to extract (e.g., "ETH" or c("ETH", "KEN")).
#' @param priors A priors list object in the format of MOSAIC::priors_default.
#'   If NULL, will use MOSAIC::priors_default.
#'
#' @return A priors list object with the same structure as the input, containing:
#'   \itemize{
#'     \item All metadata (unchanged)
#'     \item All global parameters (unchanged)
#'     \item Location-specific parameters filtered to only the requested location(s)
#'   }
#'
#' @details
#' This function preserves the exact structure of the priors object:
#' \itemize{
#'   \item \code{metadata}: Copied unchanged
#'   \item \code{parameters_global}: All global parameters preserved
#'   \item \code{parameters_location}: Filtered to include only the specified ISO codes
#' }
#'
#' The function validates that all requested locations exist in the priors before
#' extracting. If any location is missing, an error is thrown listing the missing
#' locations and available locations.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract priors for Ethiopia
#' eth_priors <- get_location_priors(iso = "ETH")
#' 
#' # Extract priors for multiple countries
#' east_africa_priors <- get_location_priors(
#'   iso = c("ETH", "KEN", "UGA", "TZA")
#' )
#' 
#' # Use with custom priors object
#' my_priors <- get_location_priors(
#'   priors = my_custom_priors,
#'   iso = "SEN"
#' )
#' 
#' # Use extracted priors with sample_parameters
#' config_sampled <- sample_parameters(
#'   priors = eth_priors,
#'   config = my_config,
#'   seed = 123
#' )
#' }
get_location_priors <- function(iso, priors = NULL) {
  
  # ============================================================================
  # Input validation
  # ============================================================================
  
  # Check iso argument
  if (missing(iso) || is.null(iso)) {
    stop("iso argument is required")
  }
  
  if (!is.character(iso)) {
    stop("iso must be a character vector")
  }
  
  if (length(iso) == 0) {
    stop("iso must contain at least one location")
  }
  
  # Remove any duplicates and ensure uppercase
  iso <- unique(toupper(iso))
  
  # Load default priors if not provided
  if (is.null(priors)) {
    if (!requireNamespace("MOSAIC", quietly = TRUE)) {
      stop("MOSAIC package not found. Please provide priors object explicitly.")
    }
    
    # Check if priors_default exists in MOSAIC
    if (!exists("priors_default", where = "package:MOSAIC")) {
      stop("MOSAIC::priors_default not found. Please provide priors object explicitly.")
    }
    
    priors <- MOSAIC::priors_default
  }
  
  # Validate priors structure
  if (!is.list(priors)) {
    stop("priors must be a list object")
  }
  
  if (is.null(priors$parameters_location)) {
    stop("priors object is missing 'parameters_location' component")
  }
  
  # ============================================================================
  # Find available locations in priors
  # ============================================================================
  
  # Get available locations from the first location-specific parameter
  available_locations <- NULL
  
  if (length(priors$parameters_location) > 0) {
    # Get first parameter
    first_param <- priors$parameters_location[[1]]
    
    # Check for location structure
    if (!is.null(first_param$location)) {
      available_locations <- names(first_param$location)
    }
  }
  
  if (is.null(available_locations) || length(available_locations) == 0) {
    stop("No location-specific parameters found in priors object")
  }
  
  # ============================================================================
  # Validate requested locations exist
  # ============================================================================
  
  missing_locations <- setdiff(iso, available_locations)
  
  if (length(missing_locations) > 0) {
    stop(paste0(
      "The following location(s) not found in priors: ",
      paste(missing_locations, collapse = ", "),
      "\nAvailable locations: ",
      paste(sort(available_locations), collapse = ", ")
    ))
  }
  
  # ============================================================================
  # Create filtered priors object
  # ============================================================================
  
  # Initialize new priors object
  priors_filtered <- list()
  
  # Copy metadata unchanged
  if (!is.null(priors$metadata)) {
    priors_filtered$metadata <- priors$metadata
  }
  
  # Copy all global parameters unchanged
  if (!is.null(priors$parameters_global)) {
    priors_filtered$parameters_global <- priors$parameters_global
  }
  
  # Filter location-specific parameters
  if (!is.null(priors$parameters_location)) {
    priors_filtered$parameters_location <- list()
    
    # Process each location-specific parameter
    for (param_name in names(priors$parameters_location)) {
      param <- priors$parameters_location[[param_name]]
      
      # Create filtered parameter structure
      param_filtered <- list(
        description = param$description,
        location = list()
      )
      
      # Copy any other attributes that might exist
      other_attrs <- setdiff(names(param), 
                            c("description", "location"))
      for (attr in other_attrs) {
        param_filtered[[attr]] <- param[[attr]]
      }
      
      # Extract only the requested locations
      if (!is.null(param$location)) {
        for (loc in iso) {
          if (!is.null(param$location[[loc]])) {
            param_filtered$location[[loc]] <- 
              param$location[[loc]]
          }
        }
      }
      
      # Only add the parameter if it has at least one location
      if (length(param_filtered$location) > 0) {
        priors_filtered$parameters_location[[param_name]] <- param_filtered
      }
    }
  }
  
  # ============================================================================
  # Validate output
  # ============================================================================
  
  # Check that we have some location parameters
  if (length(priors_filtered$parameters_location) == 0) {
    warning("No location-specific parameters were extracted. This may indicate an issue with the priors structure.")
  }
  
  # Add class to match original
  class(priors_filtered) <- class(priors)
  
  return(priors_filtered)
}

#' Print summary of location priors
#' 
#' @param x A priors object returned by get_location_priors
#' @param ... Additional arguments (ignored)
#' @export
print.mosaic_priors <- function(x, ...) {
  cat("MOSAIC Priors Object\n")
  cat("====================\n")
  
  # Print metadata if available
  if (!is.null(x$metadata)) {
    if (!is.null(x$metadata$version)) {
      cat("Version:", x$metadata$version, "\n")
    }
    if (!is.null(x$metadata$date)) {
      cat("Date:", x$metadata$date, "\n")
    }
  }
  
  # Count parameters
  n_global <- length(x$parameters_global)
  n_location <- length(x$parameters_location)
  
  cat("\nParameters:\n")
  cat("  Global:", n_global, "\n")
  cat("  Location-specific:", n_location, "\n")
  
  # Get locations if available
  if (n_location > 0 && !is.null(x$parameters_location[[1]]$location)) {
    locations <- names(x$parameters_location[[1]]$location)
    cat("  Locations included:", paste(locations, collapse = ", "), "\n")
  }
  
  invisible(x)
}