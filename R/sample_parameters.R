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
#' @param sample_mu_j Sample mu_j parameter (location-specific case fatality ratio). Default TRUE.
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

  # Location-specific parameter sampling controls (9 parameters)
  sample_beta_j0_tot = TRUE,
  sample_p_beta = TRUE,
  sample_tau_i = TRUE,
  sample_theta_j = TRUE,
  sample_a1 = TRUE,
  sample_a2 = TRUE,
  sample_b1 = TRUE,
  sample_b2 = TRUE,
  sample_mu_j = TRUE,

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

  # Load required objects if not provided
  if (is.null(PATHS)) {
    if (verbose) cat("Getting PATHS using get_paths()...\n")
    PATHS <- get_paths()
  }
  
  if (is.null(priors)) {
    if (verbose) cat("Loading MOSAIC::priors_default from package data...\n")
    priors <- MOSAIC::priors_default
  }
  
  if (is.null(config)) {
    if (verbose) cat("Loading MOSAIC::config_default from package data...\n")
    config <- MOSAIC::config_default
  }

  # Extract sampling flags from function arguments
  sampling_flags <- extract_sampling_flags(environment())

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

#' Extract sampling flags from function environment
#' @noRd
extract_sampling_flags <- function(env) {
  # Get all objects from the environment
  all_vars <- ls(env)
  
  # Filter to only sample_* variables
  sample_vars <- grep("^sample_", all_vars, value = TRUE)
  
  # Exclude sample_initial_conditions as it's handled separately
  sample_vars <- setdiff(sample_vars, "sample_initial_conditions")
  
  # Create list with cleaned names
  flags <- lapply(sample_vars, function(var) {
    get(var, envir = env)
  })
  
  # Clean names (remove "sample_" prefix)
  names(flags) <- gsub("^sample_", "", sample_vars)
  
  return(flags)
}

# Removed unnecessary loading functions - objects are loaded directly in main function

#' Format value for verbose output
#' @noRd
format_verbose_value <- function(value) {
  if (is.null(value)) return("NULL")
  if (length(value) == 1) {
    if (is.na(value)) return("NA")
    if (!is.numeric(value)) return(as.character(value))
    # Auto-format single numeric
    if (abs(value) >= 0.01 && abs(value) <= 10000) {
      return(format(round(value, 4), scientific = FALSE))
    }
    return(format(value, scientific = TRUE, digits = 3))
  }
  
  # Vector formatting
  n <- length(value)
  if (n <= 4) {
    formatted <- format_numeric_vector(value)
    return(paste0("[", paste(formatted, collapse = ", "), "]"))
  }
  
  # Show first 3 and last for long vectors
  first <- format_numeric_vector(value[1:3])
  last <- format_numeric_vector(value[n])
  return(paste0("[", paste(first, collapse = ", "), 
                ", ... (n=", n, "), ", last, "]"))
}

#' Helper to format numeric vectors consistently
#' @noRd
format_numeric_vector <- function(vals) {
  sapply(vals, function(v) {
    if (is.na(v)) return("NA")
    if (!is.numeric(v)) return(as.character(v))
    if (abs(v) >= 0.01 && abs(v) <= 10000) {
      return(format(round(v, 4), scientific = FALSE))
    }
    format(v, scientific = TRUE, digits = 3)
  })
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
      sampled_value <- sample_from_prior(
        n = 1,
        prior = global_params[[param_name]],
        verbose = FALSE
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

          # The location-specific prior already has the correct structure
          # with distribution and parameters slots
          dist_info <- param_info$parameters$location[[iso]]

          tryCatch({
            sampled_value <- sample_from_prior(
              n = 1,
              prior = dist_info,
              verbose = FALSE
            )
            sampled_values[i] <- sampled_value
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
  
  # Sample proportions for each compartment
  sampled_props <- sample_ic_proportions(location_params, locations, verbose)
  
  # Normalize and convert to counts
  config_sampled <- props_to_counts(config_sampled, sampled_props, locations, verbose)
  
  return(config_sampled)
}

#' Sample initial condition proportions
#' @noRd
sample_ic_proportions <- function(location_params, locations, verbose) {
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

          # The location-specific prior already has the correct structure
          # with distribution and parameters slots
          dist_info <- param_info$parameters$location[[iso]]

          sampled_value <- tryCatch({
            sample_from_prior(
              n = 1,
              prior = dist_info,
              verbose = FALSE
            )
          }, error = function(e) {
            warning("Failed to sample ", param_name, " for location ", iso, 
                   ": ", e$message)
            NA
          })
          sampled_props[j, comp] <- sampled_value

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
      prop_val <- sampled_props[j, comp]
      if (!is.na(prop_val) && prop_val > 0) {
        min_nonzero_prop <- min(min_nonzero_prop, prop_val)
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
      prop_val <- sampled_props[j, comp]
      
      # Handle NA values
      if (is.na(prop_val)) {
        warning("NA proportion for compartment ", comp, " in location ", j, 
                ". Setting to 0.")
        counts[j] <- 0
      } else {
        exact_count <- prop_val * N_j[j]
        
        # Round to nearest integer
        counts[j] <- round(exact_count)
        
        # Ensure we don't completely lose very small but non-zero compartments
        # If the proportion was non-zero but rounds to 0, set to 1
        if (counts[j] == 0 && prop_val > 0 && exact_count >= 0.5) {
          counts[j] <- 1
        }
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
  
  # Define validation schema
  schema <- list(
    global = list(
      params = c("phi_1", "phi_2", "omega_1", "omega_2", "iota",
                "gamma_1", "gamma_2", "epsilon", "rho", "sigma",
                "mobility_omega", "mobility_gamma", "zeta_1", "zeta_2",
                "kappa", "alpha_1", "alpha_2", "decay_days_long", 
                "decay_days_short", "decay_shape_1", "decay_shape_2"),
      type = "scalar"
    ),
    location = list(
      params = c("beta_j0_env", "beta_j0_hum", "tau_i", "theta_j",
                "a_1_j", "a_2_j", "b_1_j", "b_2_j"),
      type = "vector"
    ),
    bounds = list(
      beta_j0_tot = c(0, Inf, FALSE),  # min, max, inclusive_min
      p_beta = c(0, 1, TRUE),
      beta_j0_hum = c(0, Inf, FALSE),
      beta_j0_env = c(0, Inf, FALSE)
    )
  )
  
  valid <- TRUE
  n_locations <- length(config_sampled$location_name)
  
  # Validate using schema
  for (param in schema$global$params) {
    issue <- validate_parameter(config_sampled, param, "scalar", n_locations)
    if (!is.null(issue)) {
      if (verbose) cat("  ⚠", issue, "\n")
      valid <- FALSE
    }
  }
  
  for (param in schema$location$params) {
    issue <- validate_parameter(config_sampled, param, "vector", n_locations)
    if (!is.null(issue)) {
      if (verbose) cat("  ⚠", issue, "\n")
      valid <- FALSE
    }
  }
  
  # Check bounds for special parameters
  for (param in names(schema$bounds)) {
    if (param %in% names(config_sampled)) {
      bounds <- schema$bounds[[param]]
      vals <- config_sampled[[param]]
      if (bounds[3]) {  # inclusive min
        if (any(vals < bounds[1] | vals > bounds[2], na.rm = TRUE)) {
          if (verbose) cat("  ⚠", param, "contains values outside [", 
                          bounds[1], ",", bounds[2], "]\n")
          valid <- FALSE
        }
      } else {  # exclusive min
        if (any(vals <= bounds[1] | vals > bounds[2], na.rm = TRUE)) {
          if (verbose) cat("  ⚠", param, "contains non-positive values\n")
          valid <- FALSE
        }
      }
    }
  }
  
  if (verbose) {
    cat(ifelse(valid, "  ✓ Config validation passed!\n", 
               "  ✗ Config validation failed - see warnings above\n"))
  }
  
  return(valid)
}

#' Helper to validate a single parameter
#' @noRd
validate_parameter <- function(config, param, type, n_locations) {
  if (!(param %in% names(config))) {
    return(paste("Missing", ifelse(type == "scalar", "global", "location"), 
                 "parameter:", param))
  }
  
  val <- config[[param]]
  
  if (type == "scalar") {
    if (is.na(val) || is.null(val) || is.infinite(val)) {
      return(paste("Invalid value for", param, "=", val))
    }
  } else {  # vector
    if (length(val) != n_locations) {
      return(paste("Wrong length for", param, 
                  "(expected", n_locations, "got", length(val), ")"))
    }
    if (any(is.na(val) | is.infinite(val))) {
      return(paste("Invalid values in", param))
    }
  }
  
  return(NULL)  # No issues
}

#' Get all sampling parameters with defaults
#' @noRd
get_all_sampling_params <- function() {
  # Extract from function formals
  formals_list <- formals(sample_parameters)
  
  # Filter to only sample_* parameters
  sample_params <- names(formals_list)[grep("^sample_", names(formals_list))]
  
  # Create list with all TRUE values
  params <- as.list(rep(TRUE, length(sample_params)))
  names(params) <- sample_params
  
  return(params)
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

  # Get all sampling parameters using function formals
  all_params <- get_all_sampling_params()

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
