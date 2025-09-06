#' Sample Parameters from Prior Distributions
#'
#' This function samples parameter values from prior distributions and creates a MOSAIC config file
#' with the sampled values. It supports all distribution types used in MOSAIC priors and provides
#' explicit control over which parameters to sample.
#'
#' @param PATHS A list containing paths to various directories. If NULL, will use get_paths().
#' @param priors A priors list object. If NULL, will use MOSAIC::priors_default.
#' @param config A config template list. If NULL, will use MOSAIC::config_default.
#' @param seed Random seed for reproducible sampling (required).
#'
#' @param sample_alpha_1 Sample alpha_1 parameter (population mixing within metapops). Default TRUE.
#' @param sample_alpha_2 Sample alpha_2 parameter (degree of frequency driven transmission). Default TRUE.
#' @param sample_decay_days_long Sample decay_days_long parameter (maximum V. cholerae survival). Default TRUE.
#' @param sample_decay_days_short Sample decay_days_short parameter (minimum V. cholerae survival). Default TRUE.
#' @param sample_decay_shape_1 Sample decay_shape_1 parameter (first Beta shape for decay). Default TRUE.
#' @param sample_decay_shape_2 Sample decay_shape_2 parameter (second Beta shape for decay). Default TRUE.
#' @param sample_epsilon Sample epsilon parameter (immunity). Default TRUE.
#' @param sample_gamma_1 Sample gamma_1 parameter (recovery rate). Default TRUE.
#' @param sample_gamma_2 Sample gamma_2 parameter (recovery rate). Default TRUE.
#' @param sample_iota Sample iota parameter (importation rate). Default TRUE.
#' @param sample_kappa Sample kappa parameter (spatial correlation). Default TRUE.
#' @param sample_mobility_gamma Sample mobility_gamma parameter. Default TRUE.
#' @param sample_mobility_omega Sample mobility_omega parameter. Default TRUE.
#' @param sample_omega_1 Sample omega_1 parameter (infection). Default TRUE.
#' @param sample_omega_2 Sample omega_2 parameter (infection). Default TRUE.
#' @param sample_phi_1 Sample phi_1 parameter (incubation). Default TRUE.
#' @param sample_phi_2 Sample phi_2 parameter (incubation). Default TRUE.
#' @param sample_rho Sample rho parameter (immunity waning). Default TRUE.
#' @param sample_sigma Sample sigma parameter (immunity). Default TRUE.
#' @param sample_zeta_1 Sample zeta_1 parameter (spatial). Default TRUE.
#' @param sample_zeta_2 Sample zeta_2 parameter (spatial). Default TRUE.
#'
#' @param sample_beta_j0_tot Sample beta_j0_tot parameter (total transmission rate). Default TRUE.
#' @param sample_p_beta Sample p_beta parameter (proportion of human-to-human transmission). Default TRUE.
#' @param sample_tau_i Sample tau_i parameter (diffusion). Default TRUE.
#' @param sample_theta_j Sample theta_j parameter (WASH coverage). Default TRUE.
#' @param sample_a1 Sample a1 parameter (seasonality). Default TRUE.
#' @param sample_a2 Sample a2 parameter (seasonality). Default TRUE.
#' @param sample_b1 Sample b1 parameter (seasonality). Default TRUE.
#' @param sample_b2 Sample b2 parameter (seasonality). Default TRUE.
#'
#' @param sample_initial_conditions Sample initial condition proportions for all compartments. Default TRUE.
#'
#' @param verbose Logical indicating whether to print progress messages. Default TRUE.
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
#' # Sample all parameters (default)
#' config_sampled <- sample_parameters(seed = 123)
#'
#' # Sample only disease progression parameters
#' config_sampled <- sample_parameters(
#'   seed = 123,
#'   sample_mobility_omega = FALSE,
#'   sample_mobility_gamma = FALSE,
#'   sample_kappa = FALSE
#' )
#'
#' # Use helper for common patterns
#' args <- create_sampling_args("disease_only", seed = 123)
#' config_sampled <- do.call(sample_parameters, args)
#' }
sample_parameters <- function(
  # Required and core arguments
  PATHS = NULL,
  priors = NULL,
  config = NULL,
  seed,

  # Global parameter sampling controls (21 parameters)
  sample_alpha_1 = TRUE,
  sample_alpha_2 = TRUE,
  sample_decay_days_long = TRUE,
  sample_decay_days_short = TRUE,
  sample_decay_shape_1 = TRUE,
  sample_decay_shape_2 = TRUE,
  sample_epsilon = TRUE,
  sample_gamma_1 = TRUE,
  sample_gamma_2 = TRUE,
  sample_iota = TRUE,
  sample_kappa = TRUE,
  sample_mobility_gamma = TRUE,
  sample_mobility_omega = TRUE,
  sample_omega_1 = TRUE,
  sample_omega_2 = TRUE,
  sample_phi_1 = TRUE,
  sample_phi_2 = TRUE,
  sample_rho = TRUE,
  sample_sigma = TRUE,
  sample_zeta_1 = TRUE,
  sample_zeta_2 = TRUE,

  # Location-specific parameter sampling controls (8 parameters)
  sample_beta_j0_tot = TRUE,
  sample_p_beta = TRUE,
  sample_tau_i = TRUE,
  sample_theta_j = TRUE,
  sample_a1 = TRUE,
  sample_a2 = TRUE,
  sample_b1 = TRUE,
  sample_b2 = TRUE,

  # Initial conditions sampling control
  sample_initial_conditions = TRUE,

  # Other options
  verbose = TRUE
) {

  # Input validation
  if (missing(seed) || !is.numeric(seed) || length(seed) != 1) {
    stop("seed must be a single numeric value")
  }

  # Preserve RNG state
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  })

  # Set seed for reproducibility
  set.seed(seed)

  # Load PATHS if not provided
  PATHS <- load_paths_safely(PATHS, verbose)

  # Load priors if not provided
  priors <- load_priors_safely(priors, PATHS, verbose)

  # Load config template if not provided
  config <- load_config_safely(config, verbose)

  # Create sampling flags list for cleaner internal handling
  sampling_flags <- list(
    # Global parameters
    alpha_1 = sample_alpha_1,
    alpha_2 = sample_alpha_2,
    decay_days_long = sample_decay_days_long,
    decay_days_short = sample_decay_days_short,
    decay_shape_1 = sample_decay_shape_1,
    decay_shape_2 = sample_decay_shape_2,
    epsilon = sample_epsilon,
    gamma_1 = sample_gamma_1,
    gamma_2 = sample_gamma_2,
    iota = sample_iota,
    kappa = sample_kappa,
    mobility_gamma = sample_mobility_gamma,
    mobility_omega = sample_mobility_omega,
    omega_1 = sample_omega_1,
    omega_2 = sample_omega_2,
    phi_1 = sample_phi_1,
    phi_2 = sample_phi_2,
    rho = sample_rho,
    sigma = sample_sigma,
    zeta_1 = sample_zeta_1,
    zeta_2 = sample_zeta_2,
    # Location parameters
    beta_j0_tot = sample_beta_j0_tot,
    p_beta = sample_p_beta,
    tau_i = sample_tau_i,
    theta_j = sample_theta_j,
    a1 = sample_a1,
    a2 = sample_a2,
    b1 = sample_b1,
    b2 = sample_b2
  )

  # Create a copy of config to modify
  config_sampled <- config

  # Get location information
  locations <- config$location_name
  n_locations <- length(locations)

  if (verbose) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n", sep = "")
    cat("Starting parameter sampling\n")
    cat("Random seed:", seed, "\n")
    cat("Number of locations:", n_locations, "\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n", sep = "")
  }

  # Sample global parameters
  config_sampled <- sample_global_parameters_impl(
    config_sampled,
    priors$parameters_global,
    sampling_flags,
    verbose
  )

  # Sample location-specific parameters
  config_sampled <- sample_location_parameters_impl(
    config_sampled,
    priors$parameters_location,
    locations,
    sampling_flags,
    verbose
  )

  # Sample and normalize initial conditions if requested
  if (sample_initial_conditions) {
    config_sampled <- sample_initial_conditions_impl(
      config_sampled,
      priors$parameters_location,
      locations,
      verbose
    )
  }

  # Update seed in config
  config_sampled$seed <- seed

  # Validate the sampled config
  is_valid <- validate_sampled_config(config_sampled, verbose)

  if (!is_valid) {
    warning("Sampled config failed validation checks. Please review the warnings above.")
  }

  if (verbose) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n", sep = "")
    cat("Parameter sampling complete!\n")

    # Count sampled parameters
    n_global_sampled <- sum(unlist(sampling_flags[intersect(names(sampling_flags), names(priors$parameters_global))]))
    n_location_sampled <- sum(unlist(sampling_flags[intersect(names(sampling_flags), names(priors$parameters_location))])) * n_locations

    cat("Sampled", n_global_sampled, "global parameters\n")
    cat("Sampled", n_location_sampled, "location-specific parameter values\n")
    cat(paste(rep("=", 50), collapse = ""), "\n", sep = "")
  }

  # ============================================================================
  # Store sampling metadata for later retrieval
  # ============================================================================

  # Store sampling flags in the config for later use in matrix reconstruction
  # This enables convert_matrix_to_config to respect original sampling intent
  config_sampled$`__sampling_metadata__` <- sampling_flags

  if (verbose) {
    cat("Stored sampling metadata for", length(sampling_flags), "parameters\n")
  }

  return(config_sampled)
}

#' Helper function to load PATHS safely
#' @noRd
load_paths_safely <- function(PATHS, verbose) {
  if (is.null(PATHS)) {
    if (verbose) cat("Getting PATHS using get_paths()...\n")

    tryCatch({
      PATHS <- get_paths()
    }, error = function(e) {
      stop("Failed to get PATHS: ", e$message,
           "\nPlease provide PATHS explicitly or ensure set_root_directory() has been called.")
    })
  }

  if (!is.list(PATHS)) {
    stop("PATHS must be a list")
  }

  return(PATHS)
}

#' Helper function to load priors safely
#' @noRd
load_priors_safely <- function(priors, PATHS, verbose) {
  if (is.null(priors)) {
    # Try to load from MOSAIC package data first
    if (requireNamespace("MOSAIC", quietly = TRUE) &&
        exists("priors_default", where = "package:MOSAIC")) {
      if (verbose) cat("Loading MOSAIC::priors_default from package data...\n")
      priors <- MOSAIC::priors_default
    } else {
      # Fall back to loading from JSON file
      priors_file <- file.path(
        dirname(dirname(PATHS$MODEL_INPUT)),
        "inst/extdata/priors.json"
      )

      if (!file.exists(priors_file)) {
        # Try alternate location
        priors_file <- "inst/extdata/priors.json"
      }

      if (!file.exists(priors_file)) {
        stop("priors not found. Please either:\n",
             "1. Rebuild the package after running make_priors.R to include priors data object, or\n",
             "2. Ensure inst/extdata/priors.json exists, or\n",
             "3. Provide priors explicitly as an argument")
      }

      if (verbose) cat("Loading priors from:", priors_file, "\n")
      priors <- jsonlite::fromJSON(priors_file)
    }
  }

  if (!is.list(priors)) {
    stop("priors must be a list")
  }

  if (!all(c("parameters_global", "parameters_location") %in% names(priors))) {
    stop("priors must contain 'parameters_global' and 'parameters_location' elements")
  }

  return(priors)
}

#' Helper function to load config safely
#' @noRd
load_config_safely <- function(config, verbose) {
  if (is.null(config)) {
    # Try to load from MOSAIC package data first
    if (requireNamespace("MOSAIC", quietly = TRUE) &&
        exists("config_default", where = "package:MOSAIC")) {
      if (verbose) cat("Loading MOSAIC::config_default from package data...\n")
      config <- MOSAIC::config_default
    } else {
      # Fall back to loading from JSON file
      config_file <- "inst/extdata/default_parameters.json"

      if (!file.exists(config_file)) {
        # Try alternate location (when running from within package)
        config_file <- system.file("extdata", "default_parameters.json", package = "MOSAIC")
      }

      if (!file.exists(config_file) || config_file == "") {
        stop("config not found. Please either:\n",
             "1. Ensure MOSAIC package is installed with config_default data object, or\n",
             "2. Ensure inst/extdata/default_parameters.json exists, or\n",
             "3. Provide config explicitly as an argument")
      }

      if (verbose) cat("Loading config from:", config_file, "\n")
      config <- jsonlite::fromJSON(config_file)
    }
  }

  if (!is.list(config)) {
    stop("config must be a list")
  }

  if (!"location_name" %in% names(config)) {
    stop("config must contain 'location_name' element")
  }

  return(config)
}

#' Sample from a distribution with error handling
#' @noRd
sample_from_distribution <- function(dist_info, param_name = "unknown") {

  if (!is.list(dist_info) || !"distribution" %in% names(dist_info)) {
    stop("Invalid distribution info for parameter: ", param_name)
  }

  dist_type <- dist_info$distribution
  params <- dist_info$parameters

  tryCatch({

    if (dist_type == "beta") {
      if (!all(c("shape1", "shape2") %in% names(params))) {
        stop("Beta distribution requires shape1 and shape2 parameters")
      }
      value <- rbeta(1, params$shape1, params$shape2)

    } else if (dist_type == "gamma") {
      if (!all(c("shape", "rate") %in% names(params))) {
        stop("Gamma distribution requires shape and rate parameters")
      }
      value <- rgamma(1, shape = params$shape, rate = params$rate)

    } else if (dist_type == "lognormal") {
      # Handle both parameterizations
      if (!is.null(params$meanlog) && !is.null(params$sdlog)) {
        value <- rlnorm(1, meanlog = params$meanlog, sdlog = params$sdlog)
      } else if (!is.null(params$mean) && !is.null(params$sd)) {
        # Validate to prevent division by zero
        if (params$mean <= 0) {
          stop("Lognormal mean must be positive")
        }
        # Convert from mean/sd of the lognormal to meanlog/sdlog
        cv <- params$sd / params$mean
        sdlog <- sqrt(log(1 + cv^2))
        meanlog <- log(params$mean) - sdlog^2/2
        value <- rlnorm(1, meanlog = meanlog, sdlog = sdlog)
      } else {
        stop("Lognormal distribution requires either meanlog/sdlog or mean/sd parameters")
      }

    } else if (dist_type == "normal") {
      if (!all(c("mean", "sd") %in% names(params))) {
        stop("Normal distribution requires mean and sd parameters")
      }
      value <- rnorm(1, mean = params$mean, sd = params$sd)

    } else if (dist_type == "uniform") {
      if (!all(c("min", "max") %in% names(params))) {
        stop("Uniform distribution requires min and max parameters")
      }
      if (params$min >= params$max) {
        stop("Uniform distribution requires min < max")
      }
      value <- runif(1, min = params$min, max = params$max)

    } else if (dist_type == "gompertz") {
      if (!all(c("b", "eta") %in% names(params))) {
        stop("Gompertz distribution requires b and eta parameters")
      }
      if (params$b <= 0) {
        stop("Gompertz distribution requires b > 0")
      }
      if (params$eta <= 0) {
        stop("Gompertz distribution requires eta > 0")
      }
      # Use the rgompertz function from fit_gompertz_from_ci.R
      value <- rgompertz(1, b = params$b, eta = params$eta)

    } else {
      stop("Unknown distribution type: ", dist_type)
    }

    # Validate sampled value
    value <- validate_sampled_value(value, param_name, dist_type)

    return(value)

  }, error = function(e) {
    stop("Failed to sample parameter '", param_name, "': ", e$message)
  })
}

#' Validate a sampled value
#' @noRd
validate_sampled_value <- function(value, param_name, distribution) {

  # Check for NA/NaN/Inf
  if (is.na(value) || is.nan(value) || is.infinite(value)) {
    stop("Invalid value sampled for parameter '", param_name, "': ", value)
  }

  # Distribution-specific bounds checking
  if (distribution == "beta" && (value < 0 || value > 1)) {
    stop("Beta parameter '", param_name, "' out of bounds [0,1]: ", value)
  }

  if (distribution == "gamma" && value < 0) {
    stop("Gamma parameter '", param_name, "' must be non-negative: ", value)
  }

  if (distribution == "lognormal" && value <= 0) {
    stop("Lognormal parameter '", param_name, "' must be positive: ", value)
  }

  if (distribution == "gompertz" && value < 0) {
    stop("Gompertz parameter '", param_name, "' must be non-negative: ", value)
  }

  return(value)
}

#' Format value for verbose output
#' @noRd
format_verbose_value <- function(value, max_length = 50) {
  if (is.null(value)) {
    return("NULL")
  }

  # For single values
  if (length(value) == 1) {
    if (is.numeric(value)) {
      # Format based on magnitude
      if (abs(value) >= 0.01 && abs(value) <= 10000) {
        return(format(round(value, 4), scientific = FALSE))
      } else {
        return(format(value, scientific = TRUE, digits = 3))
      }
    } else {
      return(as.character(value))
    }
  }

  # For vectors
  if (length(value) <= 4) {
    # Show all values if 4 or fewer
    formatted <- sapply(value, function(v) {
      if (is.numeric(v)) {
        if (abs(v) >= 0.01 && abs(v) <= 10000) {
          format(round(v, 4), scientific = FALSE)
        } else {
          format(v, scientific = TRUE, digits = 3)
        }
      } else {
        as.character(v)
      }
    })
    return(paste("[", paste(formatted, collapse = ", "), "]", sep = ""))
  } else {
    # Show first 3 and last value with ellipsis
    first_three <- value[1:3]
    last_one <- value[length(value)]

    formatted_first <- sapply(first_three, function(v) {
      if (is.numeric(v)) {
        if (abs(v) >= 0.01 && abs(v) <= 10000) {
          format(round(v, 4), scientific = FALSE)
        } else {
          format(v, scientific = TRUE, digits = 3)
        }
      } else {
        as.character(v)
      }
    })

    formatted_last <- if (is.numeric(last_one)) {
      if (abs(last_one) >= 0.01 && abs(last_one) <= 10000) {
        format(round(last_one, 4), scientific = FALSE)
      } else {
        format(last_one, scientific = TRUE, digits = 3)
      }
    } else {
      as.character(last_one)
    }

    return(paste("[", paste(formatted_first, collapse = ", "),
                 ", ... (n=", length(value), "), ",
                 formatted_last, "]", sep = ""))
  }
}

#' Sample global parameters implementation
#' @noRd
sample_global_parameters_impl <- function(config_sampled, global_params,
                                         sampling_flags, verbose) {

  if (verbose) cat("Processing global parameters...\n")

  for (param_name in names(global_params)) {

    # Check if we should sample this parameter
    should_sample <- sampling_flags[[param_name]]

    # Default to TRUE if flag not found (for backward compatibility)
    if (is.null(should_sample)) should_sample <- TRUE

    if (should_sample) {
      sampled_value <- sample_from_distribution(
        global_params[[param_name]],
        param_name
      )

      config_sampled[[param_name]] <- sampled_value

      if (verbose) {
        cat("  - Sampling:", param_name, "=",
            format_verbose_value(sampled_value), "\n")
      }

    } else {
      if (verbose) {
        default_value <- config_sampled[[param_name]]
        cat("  - Keeping default:", param_name, "=",
            format_verbose_value(default_value), "\n")
      }
      # Verify the parameter exists in config
      if (!(param_name %in% names(config_sampled))) {
        warning("Parameter '", param_name,
                "' not found in config template and not sampled")
      }
    }
  }

  return(config_sampled)
}

#' Sample location-specific parameters implementation
#' @noRd
sample_location_parameters_impl <- function(config_sampled, location_params,
                                           locations, sampling_flags, verbose) {

  if (verbose) cat("\nProcessing location-specific parameters...\n")

  n_locations <- length(locations)

  # Parameter name mappings
  PARAM_MAPPINGS <- list(
    a1 = "a_1_j",
    a2 = "a_2_j",
    b1 = "b_1_j",
    b2 = "b_2_j"
  )

  for (param_name in names(location_params)) {

    # Check if we should sample this parameter
    should_sample <- sampling_flags[[param_name]]

    # Default to TRUE if flag not found (for backward compatibility)
    if (is.null(should_sample)) should_sample <- TRUE

    if (should_sample) {
      param_info <- location_params[[param_name]]

      # Initialize vector for this parameter - FIX for uninitialized variable bug
      sampled_values <- numeric(n_locations)

      # Track any sampling failures
      failed_locations <- character()

      # Sample for each location
      for (i in seq_along(locations)) {
        iso <- locations[i]

        # Get distribution info for this location
        if (!is.null(param_info$parameters$location[[iso]])) {

          dist_info <- list(
            distribution = param_info$distribution,
            parameters = param_info$parameters$location[[iso]]
          )

          tryCatch({
            sampled_values[i] <- sample_from_distribution(dist_info,
                                                         paste0(param_name, "_", iso))
          }, error = function(e) {
            warning("Failed to sample ", param_name, " for location ", iso, ": ", e$message)
            sampled_values[i] <- NA
            failed_locations <<- c(failed_locations, iso)
          })

        } else {
          warning("No prior found for ", param_name, " in location ", iso)
          sampled_values[i] <- NA
          failed_locations <- c(failed_locations, iso)
        }
      }

      # Check for sampling failures
      if (length(failed_locations) > 0) {
        stop("Failed to sample parameter '", param_name,
             "' for locations: ", paste(failed_locations, collapse = ", "),
             "\nPlease check priors configuration.")
      }

      # Map to config structure with correct names
      config_param_name <- PARAM_MAPPINGS[[param_name]]
      if (is.null(config_param_name)) {
        config_param_name <- param_name
      }

      config_sampled[[config_param_name]] <- sampled_values

      if (verbose) {
        cat("  - Sampling:", param_name, "=",
            format_verbose_value(sampled_values), "\n")
      }

    } else {
      if (verbose) {
        config_param_name <- PARAM_MAPPINGS[[param_name]]
        if (is.null(config_param_name)) {
          config_param_name <- param_name
        }
        default_values <- config_sampled[[config_param_name]]
        cat("  - Keeping defaults:", param_name, "=",
            format_verbose_value(default_values), "\n")
      }

      # Verify the parameter exists in config
      config_param_name <- PARAM_MAPPINGS[[param_name]]
      if (is.null(config_param_name)) {
        config_param_name <- param_name
      }

      if (!(config_param_name %in% names(config_sampled))) {
        warning("Parameter '", config_param_name,
                "' not found in config template and not sampled")
      }
    }
  }

  # Derive beta_j0_hum and beta_j0_env from beta_j0_tot and p_beta if sampled
  if ("beta_j0_tot" %in% names(config_sampled) && "p_beta" %in% names(config_sampled)) {

    if (verbose) cat("\n  Deriving transmission components from beta_j0_tot and p_beta...\n")

    # Initialize vectors if they don't exist
    if (is.null(config_sampled$beta_j0_hum)) {
      config_sampled$beta_j0_hum <- numeric(n_locations)
    }
    if (is.null(config_sampled$beta_j0_env)) {
      config_sampled$beta_j0_env <- numeric(n_locations)
    }

    # Derive transmission components for each location
    for (i in seq_along(locations)) {
      config_sampled$beta_j0_hum[i] <- config_sampled$p_beta[i] * config_sampled$beta_j0_tot[i]
      config_sampled$beta_j0_env[i] <- (1 - config_sampled$p_beta[i]) * config_sampled$beta_j0_tot[i]
    }

    if (verbose) {
      cat("  - Derived beta_j0_hum =", format_verbose_value(config_sampled$beta_j0_hum), "\n")
      cat("  - Derived beta_j0_env =", format_verbose_value(config_sampled$beta_j0_env), "\n")
    }
  }

  return(config_sampled)
}

#' Sample Initial Conditions Implementation
#'
#' Sample initial condition proportions and convert to counts with normalization
#' @noRd
sample_initial_conditions_impl <- function(config_sampled, location_params,
                                          locations, verbose) {

  if (verbose) cat("\nProcessing initial conditions...\n")

  n_locations <- length(locations)

  # Define IC compartments - NOTE: S is calculated as residual, not sampled
  ic_compartments_all <- c("S", "V1", "V2", "E", "I", "R")
  ic_compartments_sample <- c("V1", "V2", "E", "I", "R")  # Only sample these
  ic_param_names_sample <- paste0("prop_", ic_compartments_sample, "_initial")

  # Initialize matrix to store sampled proportions
  sampled_props <- matrix(NA_real_, nrow = n_locations, ncol = length(ic_compartments_all))
  colnames(sampled_props) <- ic_compartments_all
  rownames(sampled_props) <- locations

  # Sample proportions for V1, V2, E, I, R only (NOT S)
  for (i in seq_along(ic_compartments_sample)) {

    comp <- ic_compartments_sample[i]
    param_name <- ic_param_names_sample[i]

    # Track values for verbose output
    sampled_values <- numeric(n_locations)

    param_info <- location_params[[param_name]]

    if (is.null(param_info)) {

         stop("Cannot find parameter info for: ", comp)

    } else {

      # Sample for each location
      for (j in seq_along(locations)) {

        iso <- locations[j]

        # Get distribution info for this location
        if (!is.null(param_info$parameters$location[[iso]])) {

          dist_info <- list(
            distribution = param_info$distribution,
            parameters = param_info$parameters$location[[iso]]
          )

          sampled_props[j, comp] <- sample_from_distribution(
            dist_info,
            paste0(param_name, "_", iso)
          )

          sampled_values[j] <- sampled_props[j, comp]

        } else {

             stop("Cannot find parameter info for: ", locations[j])

        }
      }
    }

    if (verbose) {
      cat("  - Sampling:", param_name, "=",
          format_verbose_value(sampled_values), "\n")
    }
  }

  # Calculate S as residual to ensure exact constraint: S = 1 - (V1 + V2 + E + I + R)
  if (verbose) cat("  - Calculating S as residual (S = 1 - sum of other compartments)...\n")

  # Determine the precision needed based on smallest non-zero proportions
  # This ensures we maintain enough precision for accurate count conversion
  min_nonzero_prop <- Inf
  for (j in seq_along(locations)) {
    for (comp in ic_compartments_sample) {
      if (sampled_props[j, comp] > 0) {
        min_nonzero_prop <- min(min_nonzero_prop, sampled_props[j, comp])
      }
    }
  }

  # Calculate required precision (number of significant digits needed)
  # We want to preserve at least 2 significant figures in the smallest proportion
  if (is.finite(min_nonzero_prop) && min_nonzero_prop > 0) {
    # For precision that ensures smallest_prop * N gives meaningful counts
    required_precision <- ceiling(-log10(min_nonzero_prop)) + 2
    if (verbose) {
      cat(sprintf("  - Smallest non-zero proportion: %.2e (maintaining %d decimal places)\n",
                  min_nonzero_prop, required_precision))
    }
  } else {
    required_precision <- 10  # Default precision
  }

  for (j in seq_along(locations)) {
    # Sum the sampled compartments (V1, V2, E, I, R) with high precision
    other_sum <- sum(sampled_props[j, ic_compartments_sample])

    # Calculate S as the residual (may be negative or > 1, we'll normalize next)
    sampled_props[j, "S"] <- 1.0 - other_sum

    # Now normalize ALL proportions to sum to exactly 1
    # This handles cases where the raw sum isn't 1
    total_sum <- sum(sampled_props[j, ])  # S + V1 + V2 + E + I + R

    # Normalize each compartment while maintaining precision
    for (comp in ic_compartments_all) {
      sampled_props[j, comp] <- sampled_props[j, comp] / total_sum
    }

    # Verify normalization worked
    normalized_sum <- sum(sampled_props[j, ])
    if (abs(normalized_sum - 1.0) > 1e-10) {
      stop("CRITICAL: Normalization failed for location ", locations[j],
           ". Sum after normalization = ", normalized_sum)
    }

    if (verbose && j == 1) {  # Show example for first location
      # Use dynamic formatting based on required precision
      format_str <- paste0("    - Example (%s): S=%.", min(4, required_precision), "f, ",
                          "V1=%.", min(4, required_precision), "f, ",
                          "V2=%.", min(4, required_precision), "f, ",
                          "E=%.", required_precision, "f, ",
                          "I=%.", required_precision, "f, ",
                          "R=%.", min(4, required_precision), "f (sum=%.10f)\n")
      cat(sprintf(format_str,
                  locations[j],
                  sampled_props[j, "S"], sampled_props[j, "V1"], sampled_props[j, "V2"],
                  sampled_props[j, "E"], sampled_props[j, "I"], sampled_props[j, "R"],
                  sum(sampled_props[j, ])))
    }
  }

  # Convert proportions to counts
  if (verbose) cat("  - Converting to counts (preserving precision for small compartments)...\n")

  N_j <- config_sampled$N_j_initial

  for (comp in ic_compartments_all) {  # Use ic_compartments_all which includes S

    config_field <- paste0(comp, "_j_initial")
    counts <- numeric(length(N_j))

    for (j in seq_along(N_j)) {

      # Calculate exact count with full precision
      exact_count <- sampled_props[j, comp] * N_j[j]

      # Round to nearest integer
      counts[j] <- round(exact_count)

      # Ensure we don't completely lose very small but non-zero compartments
      # If the proportion was non-zero but rounds to 0, set to 1
      if (counts[j] == 0 && sampled_props[j, comp] > 0 && exact_count >= 0.5) {
        counts[j] <- 1
      }
    }

    config_sampled[[config_field]] <- as.integer(counts)
  }

  # Adjust for rounding errors to ensure sum equals N exactly
  if (verbose) cat("  - Adjusting for rounding errors to ensure sum = N...\n")

  for (j in seq_along(locations)) {
    actual_sum <- sum(
      config_sampled$S_j_initial[j],
      config_sampled$V1_j_initial[j],
      config_sampled$V2_j_initial[j],
      config_sampled$E_j_initial[j],
      config_sampled$I_j_initial[j],
      config_sampled$R_j_initial[j]
    )

    diff <- N_j[j] - actual_sum

    if (diff != 0) {
      # Adjust the S compartment (as it's calculated as residual) to absorb rounding error
      config_sampled$S_j_initial[j] <- config_sampled$S_j_initial[j] + diff

      # If S becomes negative, adjust the largest compartment instead
      if (config_sampled$S_j_initial[j] < 0) {
        # Revert S adjustment
        config_sampled$S_j_initial[j] <- config_sampled$S_j_initial[j] - diff

        # Find and adjust the largest compartment
        compartment_values <- c(
          S = config_sampled$S_j_initial[j],
          V1 = config_sampled$V1_j_initial[j],
          V2 = config_sampled$V2_j_initial[j],
          E = config_sampled$E_j_initial[j],
          I = config_sampled$I_j_initial[j],
          R = config_sampled$R_j_initial[j]
        )

        largest_comp <- names(which.max(compartment_values))
        largest_field <- paste0(largest_comp, "_j_initial")
        config_sampled[[largest_field]][j] <- config_sampled[[largest_field]][j] + diff

        if (verbose && abs(diff) > 1) {
          cat("    - Location ", locations[j], ": adjusted ", largest_comp, " by ", diff,
              " to match N=", N_j[j], "\n", sep = "")
        }
      } else {
        if (verbose && abs(diff) > 1) {
          cat("    - Location ", locations[j], ": adjusted S by ", diff,
              " to match N=", N_j[j], "\n", sep = "")
        }
      }
    }

    # Final verification
    final_sum <- sum(
      config_sampled$S_j_initial[j],
      config_sampled$V1_j_initial[j],
      config_sampled$V2_j_initial[j],
      config_sampled$E_j_initial[j],
      config_sampled$I_j_initial[j],
      config_sampled$R_j_initial[j]
    )

    if (final_sum != N_j[j]) {
      stop("CRITICAL: Failed to match population constraint for location ", locations[j],
           ". Sum=", final_sum, ", N=", N_j[j])
    }
  }

  if (verbose) {
    cat("  ✓ Initial conditions sampled and normalized\n")

    # Show detailed summary with full precision proportions and integer counts
    cat("\n  Summary of initial conditions:\n")
    cat("\n  Full precision proportions (after normalization):\n")
    for (j in 1:min(3, n_locations)) {
      cat(sprintf("    %s: S=%.15f, V1=%.15f, V2=%.15f, E=%.15f, I=%.15f, R=%.15f (sum=%.15f)\n",
                  locations[j],
                  sampled_props[j, "S"], sampled_props[j, "V1"], sampled_props[j, "V2"],
                  sampled_props[j, "E"], sampled_props[j, "I"], sampled_props[j, "R"],
                  sum(sampled_props[j, ])))
    }

    cat("\n  Integer counts (after rounding and adjustment):\n")
    for (j in 1:min(3, n_locations)) {
      cat(sprintf("    %s (N=%d): S=%d, V1=%d, V2=%d, E=%d, I=%d, R=%d (sum=%d)\n",
                  locations[j], N_j[j],
                  config_sampled$S_j_initial[j], config_sampled$V1_j_initial[j],
                  config_sampled$V2_j_initial[j], config_sampled$E_j_initial[j],
                  config_sampled$I_j_initial[j], config_sampled$R_j_initial[j],
                  sum(config_sampled$S_j_initial[j], config_sampled$V1_j_initial[j],
                      config_sampled$V2_j_initial[j], config_sampled$E_j_initial[j],
                      config_sampled$I_j_initial[j], config_sampled$R_j_initial[j])))
    }

    cat("\n  Rounded proportions (for readability):\n")
    for (j in 1:min(3, n_locations)) {
      cat("    ", locations[j], ": S=", round(sampled_props[j, "S"], 3),
          ", V1=", round(sampled_props[j, "V1"], 3),
          ", V2=", round(sampled_props[j, "V2"], 3),
          ", E=", round(sampled_props[j, "E"], 5),
          ", I=", round(sampled_props[j, "I"], 5),
          ", R=", round(sampled_props[j, "R"], 3), "\n", sep = "")
    }

    if (n_locations > 3) {
      cat("    ... (", n_locations - 3, " more locations)\n", sep = "")
    }
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
  required_global <- c(
    "phi_1", "phi_2", "omega_1", "omega_2", "iota",
    "gamma_1", "gamma_2", "epsilon", "rho", "sigma",
    "mobility_omega", "mobility_gamma", "zeta_1", "zeta_2",
    "kappa", "alpha_1", "alpha_2",
    "decay_days_long", "decay_days_short",
    "decay_shape_1", "decay_shape_2"
  )

  for (param in required_global) {
    if (!(param %in% names(config_sampled))) {
      if (verbose) cat("  ⚠ Missing global parameter:", param, "\n")
      valid <- FALSE
    } else if (is.na(config_sampled[[param]]) ||
               is.null(config_sampled[[param]]) ||
               is.infinite(config_sampled[[param]])) {
      if (verbose) cat("  ⚠ Invalid value for global parameter:", param,
                      "=", config_sampled[[param]], "\n")
      valid <- FALSE
    }
  }

  # Check location-specific parameters
  n_locations <- length(config_sampled$location_name)

  required_location <- c(
    "beta_j0_env", "beta_j0_hum", "tau_i", "theta_j",
    "a_1_j", "a_2_j", "b_1_j", "b_2_j"
  )

  for (param in required_location) {
    if (!(param %in% names(config_sampled))) {
      if (verbose) cat("  ⚠ Missing location parameter:", param, "\n")
      valid <- FALSE
    } else if (length(config_sampled[[param]]) != n_locations) {
      if (verbose) cat("  ⚠ Wrong length for location parameter:", param,
                      "(expected", n_locations, "got",
                      length(config_sampled[[param]]), ")\n")
      valid <- FALSE
    } else if (any(is.na(config_sampled[[param]]) |
                   is.infinite(config_sampled[[param]]))) {
      if (verbose) cat("  ⚠ Invalid values in location parameter:", param, "\n")
      valid <- FALSE
    }
  }

  # Validate new transmission parameters if present
  if ("beta_j0_tot" %in% names(config_sampled)) {
    if (any(config_sampled$beta_j0_tot <= 0, na.rm = TRUE)) {
      if (verbose) cat("  ⚠ beta_j0_tot contains non-positive values\n")
      valid <- FALSE
    }
  }

  if ("p_beta" %in% names(config_sampled)) {
    if (any(config_sampled$p_beta < 0 | config_sampled$p_beta > 1, na.rm = TRUE)) {
      if (verbose) cat("  ⚠ p_beta contains values outside [0,1]\n")
      valid <- FALSE
    }
  }

  # Validate derived beta values
  if ("beta_j0_hum" %in% names(config_sampled)) {
    if (any(config_sampled$beta_j0_hum <= 0, na.rm = TRUE)) {
      if (verbose) cat("  ⚠ Derived beta_j0_hum contains non-positive values\n")
      valid <- FALSE
    }
  }

  if ("beta_j0_env" %in% names(config_sampled)) {
    if (any(config_sampled$beta_j0_env <= 0, na.rm = TRUE)) {
      if (verbose) cat("  ⚠ Derived beta_j0_env contains non-positive values\n")
      valid <- FALSE
    }
  }

  if (valid && verbose) {
    cat("  ✓ Config validation passed!\n")
  } else if (!valid && verbose) {
    cat("  ✗ Config validation failed - see warnings above\n")
  }

  return(valid)
}

#' Create Sampling Arguments for Common Patterns
#'
#' Helper function to create argument lists for common sampling scenarios,
#' making it easier to work with the many parameters.
#'
#' @param pattern Character string specifying the pattern. Options:
#'   - "all": Sample all parameters (default)
#'   - "none": Don't sample any parameters
#'   - "disease_only": Sample only disease progression and immunity parameters
#'   - "transmission_only": Sample only transmission parameters
#'   - "mobility_only": Sample only mobility parameters
#'   - "spatial_only": Sample only spatial parameters
#'   - "initial_conditions_only": Sample only initial condition proportions
#' @param seed Random seed for sampling
#' @param custom Named list of custom overrides for specific parameters
#' @param PATHS Optional PATHS object
#' @param priors Optional priors object
#' @param config Optional config object
#'
#' @return Named list of arguments suitable for do.call(sample_parameters, ...)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Sample only disease parameters
#' args <- create_sampling_args("disease_only", seed = 123)
#' config <- do.call(sample_parameters, args)
#'
#' # Sample all except mobility
#' args <- create_sampling_args("all", seed = 123,
#'   custom = list(sample_mobility_omega = FALSE,
#'                 sample_mobility_gamma = FALSE))
#' config <- do.call(sample_parameters, args)
#' }
create_sampling_args <- function(pattern = "all",
                               seed,
                               custom = list(),
                               PATHS = NULL,
                               priors = NULL,
                               config = NULL) {

  if (missing(seed) || !is.numeric(seed)) {
    stop("seed must be provided and numeric")
  }

  # Define all parameters with default values
  all_params <- list(
    # Global parameters
    sample_alpha_1 = TRUE,
    sample_alpha_2 = TRUE,
    sample_decay_days_long = TRUE,
    sample_decay_days_short = TRUE,
    sample_decay_shape_1 = TRUE,
    sample_decay_shape_2 = TRUE,
    sample_epsilon = TRUE,
    sample_gamma_1 = TRUE,
    sample_gamma_2 = TRUE,
    sample_iota = TRUE,
    sample_kappa = TRUE,
    sample_mobility_gamma = TRUE,
    sample_mobility_omega = TRUE,
    sample_omega_1 = TRUE,
    sample_omega_2 = TRUE,
    sample_phi_1 = TRUE,
    sample_phi_2 = TRUE,
    sample_rho = TRUE,
    sample_sigma = TRUE,
    sample_zeta_1 = TRUE,
    sample_zeta_2 = TRUE,
    # Location parameters
    sample_beta_j0_tot = TRUE,
    sample_p_beta = TRUE,
    sample_tau_i = TRUE,
    sample_theta_j = TRUE,
    sample_a1 = TRUE,
    sample_a2 = TRUE,
    sample_b1 = TRUE,
    sample_b2 = TRUE,
    # Initial conditions
    sample_initial_conditions = TRUE
  )

  # Apply pattern
  if (pattern == "all") {
    # Keep all as TRUE (default)

  } else if (pattern == "none") {
    # Set all to FALSE
    all_params <- lapply(all_params, function(x) FALSE)

  } else if (pattern == "disease_only") {
    # Only disease progression and immunity
    disease_params <- c("sample_phi_1", "sample_phi_2",
                       "sample_omega_1", "sample_omega_2",
                       "sample_gamma_1", "sample_gamma_2",
                       "sample_epsilon", "sample_rho",
                       "sample_sigma", "sample_iota")
    param_names <- names(all_params)
    all_params <- lapply(param_names, function(n) {
      n %in% disease_params
    })
    names(all_params) <- param_names

  } else if (pattern == "transmission_only") {
    # Only transmission parameters
    trans_params <- c("sample_beta_j0_tot", "sample_p_beta",
                     "sample_alpha_1", "sample_alpha_2")
    param_names <- names(all_params)
    all_params <- lapply(param_names, function(n) {
      n %in% trans_params
    })
    names(all_params) <- param_names

  } else if (pattern == "mobility_only") {
    # Only mobility parameters
    mobility_params <- c("sample_mobility_omega", "sample_mobility_gamma",
                        "sample_tau_i")
    param_names <- names(all_params)
    all_params <- lapply(param_names, function(n) {
      n %in% mobility_params
    })
    names(all_params) <- param_names

  } else if (pattern == "spatial_only") {
    # Only spatial parameters
    spatial_params <- c("sample_zeta_1", "sample_zeta_2",
                       "sample_kappa", "sample_tau_i")
    param_names <- names(all_params)
    all_params <- lapply(param_names, function(n) {
      n %in% spatial_params
    })
    names(all_params) <- param_names

  } else if (pattern == "initial_conditions_only") {
    # Only initial conditions
    param_names <- names(all_params)
    all_params <- lapply(param_names, function(n) {
      n == "sample_initial_conditions"
    })
    names(all_params) <- param_names

  } else {
    stop("Unknown pattern: ", pattern,
         ". Choose from: all, none, disease_only, transmission_only, ",
         "mobility_only, spatial_only, initial_conditions_only")
  }

  # Apply custom overrides
  for (name in names(custom)) {
    if (name %in% names(all_params)) {
      all_params[[name]] <- custom[[name]]
    } else {
      warning("Unknown parameter in custom overrides: ", name)
    }
  }

  # Add required arguments
  all_params$seed <- seed
  if (!is.null(PATHS)) all_params$PATHS <- PATHS
  if (!is.null(priors)) all_params$priors <- priors
  if (!is.null(config)) all_params$config <- config

  return(all_params)
}

#' Check Sampled Parameter Against Prior
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
check_sampled_parameter <- function(config_sampled, priors,
                                   param_name, location = NULL) {

  result <- data.frame(
    parameter = param_name,
    location = ifelse(is.null(location), "global", location),
    sampled_value = NA_real_,
    distribution = NA_character_,
    expected_mean = NA_real_,
    stringsAsFactors = FALSE
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
      params <- prior_info$parameters
      if (prior_info$distribution == "beta") {
        s1 <- params$shape1
        s2 <- params$shape2
        result$expected_mean <- s1 / (s1 + s2)
      } else if (prior_info$distribution == "gamma") {
        result$expected_mean <- params$shape / params$rate
      } else if (prior_info$distribution == "lognormal") {
        if (!is.null(params$meanlog) && !is.null(params$sdlog)) {
          result$expected_mean <- exp(params$meanlog + params$sdlog^2/2)
        }
      } else if (prior_info$distribution == "normal") {
        result$expected_mean <- params$mean
      } else if (prior_info$distribution == "uniform") {
        result$expected_mean <- (params$min + params$max) / 2
      }
    }

  } else {
    # Location-specific parameter
    # Handle parameter name mappings
    PARAM_MAPPINGS <- list(
      a1 = "a_1_j",
      a2 = "a_2_j",
      b1 = "b_1_j",
      b2 = "b_2_j"
    )

    config_param_name <- PARAM_MAPPINGS[[param_name]]
    if (is.null(config_param_name)) {
      config_param_name <- param_name
    }

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
        } else if (prior_info$distribution == "uniform") {
          result$expected_mean <- (loc_params$min + loc_params$max) / 2
        }
      }
    }
  }

  return(result)
}
