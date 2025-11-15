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
#' @param sample_args Named list of logical values controlling which parameters to sample.
#'   Each element should be named as \code{sample_[parameter]} with a logical value.
#'   Available options include:
#'   \itemize{
#'     \item sample_alpha_1: Population mixing within metapops (default TRUE)
#'     \item sample_alpha_2: Degree of frequency driven transmission (default TRUE)
#'     \item sample_decay_days_long: Maximum V. cholerae survival (default TRUE)
#'     \item sample_decay_days_short: Minimum V. cholerae survival (default TRUE)
#'     \item sample_decay_shape_1: First Beta shape for decay (default TRUE)
#'     \item sample_decay_shape_2: Second Beta shape for decay (default TRUE)
#'     \item sample_epsilon: Immunity (default TRUE)
#'     \item sample_gamma_1: Recovery rate (default TRUE)
#'     \item sample_gamma_2: Recovery rate (default TRUE)
#'     \item sample_iota: Importation rate (default TRUE)
#'     \item sample_kappa: Spatial correlation (default TRUE)
#'     \item sample_mobility_gamma: Mobility parameter (default TRUE)
#'     \item sample_mobility_omega: Mobility parameter (default TRUE)
#'     \item sample_omega_1: Infection (default TRUE)
#'     \item sample_omega_2: Infection (default TRUE)
#'     \item sample_phi_1: Incubation (default TRUE)
#'     \item sample_phi_2: Incubation (default TRUE)
#'     \item sample_rho: Immunity waning (default TRUE)
#'     \item sample_sigma: Immunity (default TRUE)
#'     \item sample_zeta_1: Spatial (default TRUE)
#'     \item sample_zeta_2: Spatial (default TRUE)
#'     \item sample_beta_j0_tot: Total transmission rate (default TRUE)
#'     \item sample_p_beta: Proportion of human-to-human transmission (default TRUE)
#'     \item sample_tau_i: Diffusion (default TRUE)
#'     \item sample_theta_j: WASH coverage (default TRUE)
#'     \item sample_a1: Seasonality (default TRUE)
#'     \item sample_a2: Seasonality (default TRUE)
#'     \item sample_b1: Seasonality (default TRUE)
#'     \item sample_b2: Seasonality (default TRUE)
#'     \item sample_mu_j: Location-specific case fatality ratio (default TRUE)
#'     \item sample_psi_star_a: Suitability calibration shape/gain (default TRUE)
#'     \item sample_psi_star_b: Suitability calibration scale/offset (default TRUE)
#'     \item sample_psi_star_z: Suitability calibration smoothing (default TRUE)
#'     \item sample_psi_star_k: Suitability calibration time offset (default TRUE)
#'     \item sample_initial_conditions: Initial condition proportions (default TRUE)
#'   }
#'   If NULL, all parameters are sampled (default behavior).
#' @param ... Additional individual sample_* arguments for backward compatibility.
#'   These override values in sample_args if both are provided.
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
#' # Sample only disease progression parameters using sample_args
#' config_sampled <- sample_parameters(
#'   seed = 123,
#'   sample_args = list(
#'     sample_mobility_omega = FALSE,
#'     sample_mobility_gamma = FALSE,
#'     sample_kappa = FALSE
#'   )
#' )
#'
#' # Backward compatibility: still works with individual arguments
#' config_sampled <- sample_parameters(
#'   seed = 123,
#'   sample_mobility_omega = FALSE,
#'   sample_mobility_gamma = FALSE,
#'   sample_kappa = FALSE
#' )
#' }
sample_parameters <- function(
  # Required and core arguments
  PATHS = NULL,
  priors = NULL,
  config = NULL,
  seed,

  # New unified sampling control
  sample_args = NULL,

  # Other options
  verbose = TRUE,

  # Individual sampling controls for backward compatibility
  ...
) {

  # ============================================================================
  # Process sampling arguments
  # ============================================================================

  # Define all possible sampling parameters with defaults
  default_sample_args <- list(
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

    # Location-specific parameter sampling controls (13 parameters)
    sample_beta_j0_tot = TRUE,
    sample_p_beta = TRUE,
    sample_tau_i = TRUE,
    sample_theta_j = TRUE,
    sample_a1 = TRUE,
    sample_a2 = TRUE,
    sample_b1 = TRUE,
    sample_b2 = TRUE,
    sample_mu_j = TRUE,

    # psi_star calibration parameters
    sample_psi_star_a = TRUE,
    sample_psi_star_b = TRUE,
    sample_psi_star_z = TRUE,
    sample_psi_star_k = TRUE,

    # Initial conditions sampling control
    sample_initial_conditions = TRUE
  )

  # Start with defaults
  final_sample_args <- default_sample_args

  # Override with sample_args if provided
  if (!is.null(sample_args)) {
    for (name in names(sample_args)) {
      if (name %in% names(default_sample_args)) {
        final_sample_args[[name]] <- sample_args[[name]]
      } else {
        warning("Unknown sampling parameter: ", name)
      }
    }
  }

  # Override with individual arguments from ... for backward compatibility
  dots <- list(...)
  for (name in names(dots)) {
    if (name %in% names(default_sample_args)) {
      final_sample_args[[name]] <- dots[[name]]
    }
  }

  # Extract individual values for use in the function
  for (name in names(final_sample_args)) {
    assign(name, final_sample_args[[name]])
  }

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

  # Apply psi_star calibration to psi_jt matrix if psi_star parameters were sampled
  config_sampled <- apply_psi_star_calibration(config_sampled, sampling_flags, verbose)

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
        if (!is.null(param_info$location[[iso]])) {

          # The location-specific prior already has the correct structure
          # with distribution and parameters slots
          dist_info <- param_info$location[[iso]]

          tryCatch({
            sampled_value <- sample_from_prior(
              n = 1,
              prior = dist_info,
              verbose = FALSE
            )

            # Check if sampling returned NA (e.g., due to NA prior parameters)
            if (is.na(sampled_value)) {
              # Fall back to default config value
              config_param_name <- PARAM_MAPPINGS[[param_name]]
              if (is.null(config_param_name)) {
                config_param_name <- param_name
              }

              if (config_param_name %in% names(config_sampled)) {
                default_value <- config_sampled[[config_param_name]][i]
                if (verbose) {
                  message("Prior contains NA for ", param_name, " in ", iso,
                         ", using default value: ", default_value)
                }
                sampled_values[i] <- default_value
              } else {
                warning("Cannot fall back to default for ", param_name, " in ", iso)
                sampled_values[i] <- NA
                failed_locations <<- c(failed_locations, iso)
              }
            } else {
              sampled_values[i] <- sampled_value
            }
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

     # Define IC compartments - NOTE: S is calculated/adjusted, not sampled
     ic_compartments_all    <- c("S", "V1", "V2", "E", "I", "R")
     ic_compartments_sample <- c("V1", "V2", "E", "I", "R")  # Only sample these
     ic_param_names_sample  <- paste0("prop_", ic_compartments_sample, "_initial")

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
                    if (!is.null(param_info$location[[iso]])) {

                         # The location-specific prior already has the correct structure
                         # with distribution and parameters slots
                         dist_info <- param_info$location[[iso]]

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

                         # guard against negative draw due to a custom prior mistake
                         if (is.finite(sampled_value) && sampled_value < 0) sampled_value <- 0

                         sampled_props[j, comp] <- sampled_value
                         sampled_values[j]      <- sampled_value

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

     # ---------------------------------------------------------------------------
     # Simple constraint:
     # If total = S + V1 + V2 + E + I + R >= 1, set S = 0 and normalize the rest
     # Otherwise set S = 1 - (V1 + V2 + E + I + R)
     # This guarantees nonnegativity and sum-to-one.
     # ---------------------------------------------------------------------------

     if (verbose) cat("  - Enforcing IC constraint (S=0 when total >= 1; else S residual)...\n")

     for (j in seq_along(locations)) {

          # Sum of sampled non-S compartments
          other_sum <- sum(sampled_props[j, ic_compartments_sample])

          if (!is.finite(other_sum)) {
               stop("Non-finite initial condition proportion(s) at location ", locations[j])
          }

          if (other_sum >= 1) {
               # Overfull case: set S=0, normalize the others to sum to 1
               sampled_props[j, "S"] <- 0
               if (other_sum == 0) {
                    # Degenerate (all zeros) — put all mass in S (rare, but safe-guard)
                    sampled_props[j, ic_compartments_sample] <- 0
                    sampled_props[j, "S"] <- 1
               } else {
                    sampled_props[j, ic_compartments_sample] <-
                         sampled_props[j, ic_compartments_sample] / other_sum
               }
          } else {
               # Feasible: use residual for S
               sampled_props[j, "S"] <- 1 - other_sum
          }

          # Final numerical sanity
          total_sum <- sum(sampled_props[j, ])
          if (!is.finite(total_sum) || abs(total_sum - 1) > 1e-12) {
               stop("CRITICAL: Normalization failed for location ", locations[j],
                    ". Sum after constraint = ", total_sum)
          }

          # Nonnegativity guard (tiny negatives from floating point)
          sampled_props[j, ] <- pmax(sampled_props[j, ], 0)

          # (Optional) tiny re-normalization after pmax
          s <- sum(sampled_props[j, ])
          if (abs(s - 1) > 1e-12) {
               sampled_props[j, ] <- sampled_props[j, ] / s
          }
     }

     # Verbose preview (first few locations)
     if (verbose) {
          cat("  ✓ Initial conditions sampled and constrained\n")

          cat("\n  Summary of initial conditions (first up to 3 locations):\n")
          for (j in 1:min(3, n_locations)) {
               cat(sprintf("    %s: S=%.6f, V1=%.6f, V2=%.6f, E=%.8f, I=%.8f, R=%.6f (sum=%.12f)\n",
                           locations[j],
                           sampled_props[j, "S"],  sampled_props[j, "V1"], sampled_props[j, "V2"],
                           sampled_props[j, "E"],  sampled_props[j, "I"],  sampled_props[j, "R"],
                           sum(sampled_props[j, ])))
          }
          if (n_locations > 3) {
               cat("    ... (", n_locations - 3, " more locations)\n", sep = "")
          }
     }

     return(sampled_props)
}


#' Convert proportions to counts
#' @noRd
props_to_counts <- function(config_sampled, sampled_props, locations, verbose) {
  if (verbose) cat("  - Converting to counts...\n")

  N_j <- config_sampled$N_j_initial
  n_locations <- length(locations)
  ic_compartments_all <- colnames(sampled_props)

  # Convert proportions to integer counts
  for (comp in ic_compartments_all) {
    config_field <- paste0(comp, "_j_initial")
    props <- sampled_props[, comp]
    counts <- round(props * N_j)

    # Ensure very small compartments aren't completely lost
    small_nonzero <- (counts == 0) & (props > 0) & (props * N_j >= 0.5)
    counts[small_nonzero] <- 1

    config_sampled[[config_field]] <- as.integer(counts)
  }

  # Adjust for rounding errors to ensure sum equals N exactly
  for (j in seq_along(locations)) {
    compartment_fields <- c("S_j_initial", "V1_j_initial", "V2_j_initial",
                           "E_j_initial", "I_j_initial", "R_j_initial")
    counts <- sapply(compartment_fields, function(f) config_sampled[[f]][j])

    diff <- N_j[j] - sum(counts)
    if (diff != 0) {
      # Adjust S compartment (calculated as residual) for rounding
      config_sampled$S_j_initial[j] <- config_sampled$S_j_initial[j] + diff

      # If S becomes negative, adjust largest compartment instead
      if (config_sampled$S_j_initial[j] < 0) {
        config_sampled$S_j_initial[j] <- config_sampled$S_j_initial[j] - diff
        largest_idx <- which.max(counts)
        config_sampled[[compartment_fields[largest_idx]]][j] <-
          config_sampled[[compartment_fields[largest_idx]]][j] + diff
      }
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

#' Apply psi_star calibration to psi_jt matrix
#'
#' @description
#' Applies location-specific psi_star calibration parameters to transform the
#' environmental suitability matrix (psi_jt) using the calc_psi_star() function.
#' Each location gets its own calibration based on sampled psi_star_a, psi_star_b,
#' psi_star_z, and psi_star_k parameters.
#'
#' @param config_sampled The configuration object with sampled parameters
#' @param sampling_flags Named list of sampling flags to determine which parameters were sampled
#' @param verbose Logical for verbose output (default FALSE)
#'
#' @return Updated config_sampled object with calibrated psi_jt matrix
#'
#' @details
#' This function:
#' - Checks if any psi_star parameters were sampled via sampling flags
#' - Applies calc_psi_star() calibration to each location's psi_jt time series
#' - Uses location-specific parameters or defaults if not sampled
#' - Handles errors gracefully on a per-location basis
#' - Updates psi_jt matrix in-place for memory efficiency
#'
#' @noRd
apply_psi_star_calibration <- function(config_sampled, sampling_flags, verbose = FALSE) {

  # ============================================================================
  # Check if calibration should be applied
  # ============================================================================

  psi_star_params <- c("psi_star_a", "psi_star_b", "psi_star_z", "psi_star_k")

  # Check if any psi_star parameters were requested for sampling
  psi_star_flags_exist <- psi_star_params %in% names(sampling_flags)

  if (!any(psi_star_flags_exist)) {
    if (verbose) cat("  ℹ No psi_star sampling flags found, skipping calibration\n")
    return(config_sampled)
  }

  # Check if any psi_star parameters were actually enabled for sampling
  psi_star_enabled <- any(unlist(sampling_flags[psi_star_params[psi_star_flags_exist]]))

  if (!psi_star_enabled) {
    if (verbose) cat("  ℹ No psi_star parameters were enabled for sampling, skipping calibration\n")
    return(config_sampled)
  }

  # Check if psi_jt matrix exists
  if (!"psi_jt" %in% names(config_sampled)) {
    if (verbose) cat("  ⚠ psi_jt matrix not found in config, skipping psi_star calibration\n")
    return(config_sampled)
  }

  # Check if location names exist
  if (!"location_name" %in% names(config_sampled)) {
    warning("location_name not found in config, cannot apply psi_star calibration")
    return(config_sampled)
  }

  if (verbose) cat("\n🎯 Applying psi_star calibration to psi_jt matrix...\n")

  # ============================================================================
  # Setup calibration data
  # ============================================================================

  psi_original <- config_sampled$psi_jt
  locations <- config_sampled$location_name
  n_locations <- length(locations)
  n_days <- ncol(psi_original)

  if (verbose) {
    cat("  Matrix dimensions:", n_locations, "locations ×", n_days, "days\n")
  }

  # Validate matrix dimensions
  if (nrow(psi_original) != n_locations) {
    warning("psi_jt matrix rows (", nrow(psi_original), ") != number of locations (", n_locations, ")")
    return(config_sampled)
  }

  # ============================================================================
  # Apply calibration location by location
  # ============================================================================

  # Tracking statistics
  n_calibrated <- 0
  n_failed <- 0
  failed_locations <- character()
  calibration_effects <- data.frame(
    location = character(),
    a = numeric(), b = numeric(), z = numeric(), k = numeric(),
    original_min = numeric(), original_max = numeric(),
    calibrated_min = numeric(), calibrated_max = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(locations)) {
    iso <- locations[i]

    tryCatch({

      # Extract psi_star parameters for this location (with fallback defaults)
      a <- if("psi_star_a" %in% names(config_sampled) && !is.null(config_sampled$psi_star_a)) {
        config_sampled$psi_star_a[i]
      } else {
        1.0  # Default: no shape transformation
      }

      b <- if("psi_star_b" %in% names(config_sampled) && !is.null(config_sampled$psi_star_b)) {
        config_sampled$psi_star_b[i]
      } else {
        0.0  # Default: no scale offset
      }

      z <- if("psi_star_z" %in% names(config_sampled) && !is.null(config_sampled$psi_star_z)) {
        config_sampled$psi_star_z[i]
      } else {
        1.0  # Default: no smoothing
      }

      k <- if("psi_star_k" %in% names(config_sampled) && !is.null(config_sampled$psi_star_k)) {
        config_sampled$psi_star_k[i]
      } else {
        0.0  # Default: no time offset
      }

      # Validate parameters
      if (!is.finite(a) || a <= 0) {
        warning("Invalid psi_star_a for location ", iso, " (", a, "), using default 1.0")
        a <- 1.0
      }
      if (!is.finite(b)) {
        warning("Invalid psi_star_b for location ", iso, " (", b, "), using default 0.0")
        b <- 0.0
      }
      if (!is.finite(z) || z <= 0 || z > 1) {
        warning("Invalid psi_star_z for location ", iso, " (", z, "), using default 1.0")
        z <- 1.0
      }
      if (!is.finite(k)) {
        warning("Invalid psi_star_k for location ", iso, " (", k, "), using default 0.0")
        k <- 0.0
      }

      # Extract daily time series for this location
      psi_timeseries <- psi_original[i, ]

      # Skip locations with all-NA time series
      if (all(is.na(psi_timeseries))) {
        if (verbose && n_calibrated < 3) {
          cat("  -", iso, ": SKIPPED (all NA values)\n")
        }
        next
      }

      # Store original statistics
      orig_min <- min(psi_timeseries, na.rm = TRUE)
      orig_max <- max(psi_timeseries, na.rm = TRUE)

      # Apply psi_star calibration
      psi_calibrated <- calc_psi_star(
        psi = psi_timeseries,
        a = a, b = b, z = z, k = k,
        fill_method = "locf"
      )

      # Validate calibration result
      if (length(psi_calibrated) != n_days) {
        stop("calc_psi_star returned wrong length: ", length(psi_calibrated), " != ", n_days)
      }

      if (all(is.na(psi_calibrated))) {
        warning("calc_psi_star returned all NA values for location ", iso)
      }

      # Store calibrated statistics
      cal_min <- min(psi_calibrated, na.rm = TRUE)
      cal_max <- max(psi_calibrated, na.rm = TRUE)

      # Update the matrix in-place
      config_sampled$psi_jt[i, ] <- psi_calibrated
      n_calibrated <- n_calibrated + 1

      # Store calibration effects for reporting
      calibration_effects <- rbind(calibration_effects, data.frame(
        location = iso,
        a = a, b = b, z = z, k = k,
        original_min = orig_min, original_max = orig_max,
        calibrated_min = cal_min, calibrated_max = cal_max,
        stringsAsFactors = FALSE
      ))

      # Show detailed output for first few locations
      if (verbose && n_calibrated <= 3) {
        cat("  -", iso, ": a=", round(a,3), ", b=", round(b,3),
            ", z=", round(z,3), ", k=", round(k,1), "\n")
        cat("    Range: [", round(orig_min, 4), ",", round(orig_max, 4), "] → [",
            round(cal_min, 4), ",", round(cal_max, 4), "]\n")
      }

    }, error = function(e) {
      warning("Failed psi_star calibration for location ", iso, ": ", e$message, call. = FALSE)
      n_failed <<- n_failed + 1
      failed_locations <<- c(failed_locations, iso)
    })
  }

  # ============================================================================
  # Report calibration results
  # ============================================================================

  if (verbose) {
    if (n_calibrated > 0) {
      cat("  ✓ Successfully calibrated", n_calibrated, "of", n_locations, "locations\n")

      # Overall transformation statistics
      psi_final <- config_sampled$psi_jt
      cat("  📊 Overall psi_jt transformation:\n")
      cat("     Original range: [", round(min(psi_original, na.rm=TRUE), 4),
          ",", round(max(psi_original, na.rm=TRUE), 4), "]\n")
      cat("     Calibrated range: [", round(min(psi_final, na.rm=TRUE), 4),
          ",", round(max(psi_final, na.rm=TRUE), 4), "]\n")

      # Parameter usage summary
      if (nrow(calibration_effects) > 0) {
        cat("  📈 Parameter ranges used:\n")
        cat("     psi_star_a: [", round(min(calibration_effects$a), 3),
            ",", round(max(calibration_effects$a), 3), "]\n")
        cat("     psi_star_b: [", round(min(calibration_effects$b), 3),
            ",", round(max(calibration_effects$b), 3), "]\n")
        cat("     psi_star_z: [", round(min(calibration_effects$z), 3),
            ",", round(max(calibration_effects$z), 3), "]\n")
        cat("     psi_star_k: [", round(min(calibration_effects$k), 1),
            ",", round(max(calibration_effects$k), 1), "] days\n")
      }
    }

    if (n_failed > 0) {
      cat("  ⚠", n_failed, "locations failed calibration:",
          paste(head(failed_locations, 5), collapse=", "))
      if (n_failed > 5) cat(" (and", n_failed - 5, "more)")
      cat("\n")
    }

    if (n_calibrated == 0) {
      cat("  ⚠ No locations were successfully calibrated\n")
    }
  }

  return(config_sampled)
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

      if (!is.null(prior_info$location[[location]])) {
        loc_params <- prior_info$location[[location]]

        # Get distribution from location endpoint
        result$distribution <- loc_params$distribution

        # Calculate expected mean based on distribution type
        if (loc_params$distribution == "gamma") {
          result$expected_mean <- loc_params$parameters$shape / loc_params$parameters$rate
        } else if (loc_params$distribution == "beta") {
          s1 <- loc_params$parameters$shape1
          s2 <- loc_params$parameters$shape2
          result$expected_mean <- s1 / (s1 + s2)
        } else if (loc_params$distribution == "normal") {
          result$expected_mean <- loc_params$parameters$mean
        } else if (loc_params$distribution == "uniform") {
          result$expected_mean <- (loc_params$parameters$min + loc_params$parameters$max) / 2
        } else if (loc_params$distribution == "gompertz") {
          # For Gompertz, expected mean calculation would be more complex
          result$expected_mean <- NA
        }
      }
    }
  }

  return(result)
}
