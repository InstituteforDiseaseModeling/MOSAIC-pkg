#' Sample Parameters from Prior Distributions
#'
#' This function samples parameter values from prior distributions and creates a MOSAIC config file
#' with the sampled values. It supports all distribution types used in MOSAIC priors.
#'
#' @param PATHS A list containing paths to various directories. If NULL, will use get_paths().
#' @param priors A priors list object. If NULL, will use MOSAIC::priors.
#' @param config A config template list. If NULL, will use MOSAIC::config_default.
#' @param seed Random seed for reproducible sampling.
#' @param verbose Logical indicating whether to print progress messages.
#'
#' @return A MOSAIC config list with sampled parameter values.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom stats rbeta rgamma rlnorm rnorm runif
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with defaults
#' PATHS <- get_paths()
#' config_sampled <- sample_parameters(PATHS, seed = 123)
#'
#' # Use custom priors
#' my_priors <- fromJSON("custom_priors.json")
#' config_sampled <- sample_parameters(PATHS, priors = my_priors, seed = 456)
#' }
sample_parameters <- function(PATHS = NULL,
                            priors = NULL,
                            config = NULL,
                            seed,
                            verbose = TRUE) {

  # Set seed for reproducibility
  set.seed(seed)

  # Get PATHS if not provided
  if (is.null(PATHS)) {
    if (verbose) cat("Getting PATHS using get_paths()...\n")
    PATHS <- get_paths()
  }

  # Load priors if not provided
  if (is.null(priors)) {
    # Try to load from MOSAIC package data first
    if (requireNamespace("MOSAIC", quietly = TRUE) && exists("priors", where = "package:MOSAIC")) {
      if (verbose) cat("Loading MOSAIC::priors from package data...\n")
      priors <- MOSAIC::priors
    } else {
      # Fall back to loading from JSON file
      priors_file <- file.path(dirname(dirname(PATHS$MODEL_INPUT)), "inst/extdata/priors.json")
      if (!file.exists(priors_file)) {
        # Try alternate location
        priors_file <- "inst/extdata/priors.json"
      }
      if (!file.exists(priors_file)) {
        stop("priors not found. Please either:\n",
             "1. Rebuild the package after running make_priors.R to include priors data object, or\n",
             "2. Ensure inst/extdata/priors.json exists")
      }
      if (verbose) cat("Loading priors from:", priors_file, "\n")
      priors <- jsonlite::fromJSON(priors_file)
    }
  }

  # Load config template if not provided
  if (is.null(config)) {
    if (verbose) cat("Loading MOSAIC::config_default as template...\n")
    if (!requireNamespace("MOSAIC", quietly = TRUE)) {
      stop("MOSAIC package not found. Please install or load it.")
    }
    config <- MOSAIC::config_default
  }

  # Create a copy of config to modify
  config_sampled <- config

  # Helper function to sample from a distribution
  sample_from_distribution <- function(dist_info) {
    dist_type <- dist_info$distribution
    params <- dist_info$parameters

    if (dist_type == "beta") {
      return(rbeta(1, params$shape1, params$shape2))
    } else if (dist_type == "gamma") {
      return(rgamma(1, shape = params$shape, rate = params$rate))
    } else if (dist_type == "lognormal") {
      # Handle both parameterizations
      if (!is.null(params$meanlog) && !is.null(params$sdlog)) {
        return(rlnorm(1, meanlog = params$meanlog, sdlog = params$sdlog))
      } else if (!is.null(params$mean) && !is.null(params$sd)) {
        # Convert from mean/sd of the lognormal to meanlog/sdlog
        # This assumes mean and sd are for the lognormal distribution itself
        # For a lognormal: CV = sd/mean, and we can derive meanlog and sdlog
        cv <- params$sd / params$mean
        sdlog <- sqrt(log(1 + cv^2))
        meanlog <- log(params$mean) - sdlog^2/2
        return(rlnorm(1, meanlog = meanlog, sdlog = sdlog))
      } else {
        stop("Lognormal distribution requires either meanlog/sdlog or mean/sd parameters")
      }
    } else if (dist_type == "normal") {
      return(rnorm(1, mean = params$mean, sd = params$sd))
    } else if (dist_type == "uniform") {
      return(runif(1, min = params$min, max = params$max))
    } else {
      stop("Unknown distribution type: ", dist_type)
    }
  }

  # Get location names from config
  locations <- config$location_name
  n_locations <- length(locations)

  if (verbose) {
    cat("\nSampling parameters for", n_locations, "locations...\n")
    cat("Random seed:", seed, "\n\n")
  }

  # Sample global parameters
  if (verbose) cat("Sampling global parameters...\n")

  global_params <- priors$parameters_global
  for (param_name in names(global_params)) {
    if (verbose) cat("  -", param_name, "\n")
    sampled_value <- sample_from_distribution(global_params[[param_name]])

    # Map to config structure
    if (param_name %in% names(config_sampled)) {
      config_sampled[[param_name]] <- sampled_value
    } else {
      # Handle special parameter name mappings if needed
      # For example, omega_1 -> omega_1, phi_1 -> phi_1, etc.
      config_sampled[[param_name]] <- sampled_value
    }
  }

  # Sample location-specific parameters
  if (verbose) cat("\nSampling location-specific parameters...\n")

  location_params <- priors$parameters_location
  for (param_name in names(location_params)) {
    if (verbose) cat("  -", param_name, "for all locations\n")

    param_info <- location_params[[param_name]]

    # Initialize vector/matrix for this parameter if needed
    if (param_name == "beta_j0_env" || param_name == "beta_j0_hum") {
      # These are vectors with one value per location
      sampled_values <- numeric(n_locations)
    } else if (param_name == "tau_i") {
      # tau_i is a vector with one value per location
      sampled_values <- numeric(n_locations)
    } else if (param_name %in% c("a1", "a2", "b1", "b2")) {
      # Seasonality parameters - map to a_1_j, a_2_j, b_1_j, b_2_j in config
      sampled_values <- numeric(n_locations)
    }

    # Sample for each location
    for (i in seq_along(locations)) {
      iso <- locations[i]

      # Get distribution info for this location
      if (!is.null(param_info$parameters$location[[iso]])) {
        dist_info <- list(
          distribution = param_info$distribution,
          parameters = param_info$parameters$location[[iso]]
        )
        sampled_values[i] <- sample_from_distribution(dist_info)
      } else {
        warning("No prior found for ", param_name, " in location ", iso)
        sampled_values[i] <- NA
      }
    }

    # Map to config structure with correct names
    if (param_name == "beta_j0_env") {
      config_sampled$beta_j0_env <- sampled_values
    } else if (param_name == "beta_j0_hum") {
      config_sampled$beta_j0_hum <- sampled_values
    } else if (param_name == "tau_i") {
      config_sampled$tau_i <- sampled_values
    } else if (param_name == "a1") {
      config_sampled$a_1_j <- sampled_values
    } else if (param_name == "a2") {
      config_sampled$a_2_j <- sampled_values
    } else if (param_name == "b1") {
      config_sampled$b_1_j <- sampled_values
    } else if (param_name == "b2") {
      config_sampled$b_2_j <- sampled_values
    }
  }

  # Update seed in config
  config_sampled$seed <- seed

  if (verbose) {
    cat("\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
    cat("Parameter sampling complete!\n")
    cat("Config contains", length(config_sampled), "elements\n")

    # Count total parameters
    n_global <- length(global_params)
    n_location_specific <- length(location_params) * n_locations
    cat("Sampled", n_global, "global parameters\n")
    cat("Sampled", n_location_specific, "location-specific parameter values\n")
    cat("  (", length(location_params), "parameters × ", n_locations, "locations)\n")
    cat("Total:", n_global + n_location_specific, "parameter values sampled\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
  }

  return(config_sampled)
}

#' Validate Sampled Config
#'
#' Helper function to validate that all required parameters are present in the sampled config
#' and have valid values.
#'
#' @param config_sampled The sampled config to validate
#' @param verbose Logical indicating whether to print validation messages
#'
#' @return Logical indicating whether the config is valid
#'
#' @export
validate_sampled_config <- function(config_sampled, verbose = TRUE) {

  valid <- TRUE

  # Check global parameters
  required_global <- c("phi_1", "phi_2", "omega_1", "omega_2", "iota",
                      "gamma_1", "gamma_2", "epsilon", "rho", "sigma",
                      "mobility_omega", "mobility_gamma", "zeta_1", "zeta_2",
                      "kappa", "alpha_1", "alpha_2")

  for (param in required_global) {
    if (!(param %in% names(config_sampled))) {
      if (verbose) cat("Missing global parameter:", param, "\n")
      valid <- FALSE
    } else if (is.na(config_sampled[[param]]) || is.null(config_sampled[[param]])) {
      if (verbose) cat("Invalid value for global parameter:", param, "\n")
      valid <- FALSE
    }
  }

  # Check location-specific parameters
  n_locations <- length(config_sampled$location_name)

  required_location <- c("beta_j0_env", "beta_j0_hum", "tau_i",
                        "a_1_j", "a_2_j", "b_1_j", "b_2_j")

  for (param in required_location) {
    if (!(param %in% names(config_sampled))) {
      if (verbose) cat("Missing location parameter:", param, "\n")
      valid <- FALSE
    } else if (length(config_sampled[[param]]) != n_locations) {
      if (verbose) cat("Wrong length for location parameter:", param,
                      "(expected", n_locations, "got", length(config_sampled[[param]]), ")\n")
      valid <- FALSE
    }
  }

  if (valid && verbose) {
    cat("✓ Config validation passed!\n")
  }

  return(valid)
}

#' Compare Sampled Values to Priors
#'
#' Helper function to compare sampled parameter values against their prior distributions
#' to verify sampling is working correctly.
#'
#' @param config_sampled The sampled config
#' @param priors The priors list
#' @param param_name Name of parameter to check
#' @param location Optional location code for location-specific parameters
#'
#' @return Data frame with parameter info and sampled value
#'
#' @export
check_sampled_parameter <- function(config_sampled, priors, param_name, location = NULL) {

  result <- data.frame(
    parameter = param_name,
    location = ifelse(is.null(location), "global", location),
    sampled_value = NA,
    distribution = NA,
    expected_mean = NA,
    in_95CI = NA
  )

  # Get sampled value from config
  if (is.null(location)) {
    # Global parameter
    if (param_name %in% names(config_sampled)) {
      result$sampled_value <- config_sampled[[param_name]]
    }

    # Get prior info
    if (param_name %in% names(priors$parameters_global)) {
      prior_info <- priors$parameters_global[[param_name]]
      result$distribution <- prior_info$distribution

      # Calculate expected mean based on distribution
      if (prior_info$distribution == "beta") {
        s1 <- prior_info$parameters$shape1
        s2 <- prior_info$parameters$shape2
        result$expected_mean <- s1 / (s1 + s2)
      } else if (prior_info$distribution == "gamma") {
        result$expected_mean <- prior_info$parameters$shape / prior_info$parameters$rate
      } else if (prior_info$distribution == "lognormal") {
        result$expected_mean <- exp(prior_info$parameters$meanlog + prior_info$parameters$sdlog^2/2)
      }
    }
  } else {
    # Location-specific parameter
    # Handle parameter name mappings
    config_param_name <- param_name
    if (param_name == "a1") config_param_name <- "a_1_j"
    if (param_name == "a2") config_param_name <- "a_2_j"
    if (param_name == "b1") config_param_name <- "b_1_j"
    if (param_name == "b2") config_param_name <- "b_2_j"

    if (config_param_name %in% names(config_sampled)) {
      loc_idx <- which(config_sampled$location_name == location)
      if (length(loc_idx) > 0) {
        result$sampled_value <- config_sampled[[config_param_name]][loc_idx]
      }
    }

    # Get prior info
    if (param_name %in% names(priors$parameters_location)) {
      prior_info <- priors$parameters_location[[param_name]]
      result$distribution <- prior_info$distribution

      if (!is.null(prior_info$parameters$location[[location]])) {
        loc_params <- prior_info$parameters$location[[location]]

        if (prior_info$distribution == "gamma") {
          result$expected_mean <- loc_params$shape / loc_params$rate
        } else if (prior_info$distribution == "beta") {
          s1 <- loc_params$shape1
          s2 <- loc_params$shape2
          result$expected_mean <- s1 / (s1 + s2)
        } else if (prior_info$distribution == "normal") {
          result$expected_mean <- loc_params$mean
        }
      }
    }
  }

  return(result)
}
