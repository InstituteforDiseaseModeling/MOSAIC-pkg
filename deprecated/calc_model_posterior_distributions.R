#' Calculate Model Posterior Parameter Distributions
#'
#' Comprehensive posterior analysis function that generates quantiles tables and
#' structured posterior distributions from calibration results. Designed to integrate
#' seamlessly with MOSAIC calibration workflows.
#'
#' @param results Data frame containing simulation results with importance weights
#' @param priors_base List containing prior distributions (default: priors_default)
#' @param config_base Configuration list with location names and model settings
#' @param output_dir Directory to save output files (optional)
#' @param gibbs_temperature Temperature parameter for Gibbs weighting (default: 0.5)
#' @param quantile_probs Quantile probabilities to calculate (default: c(0.025, 0.25, 0.5, 0.75, 0.975))
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return List containing:
#' \describe{
#'   \item{quantiles_table}{Data frame with posterior statistics for each parameter}
#'   \item{posteriors_object}{Structured list exactly mirroring priors format with fitted posterior parameters}
#'   \item{files}{List of file paths created: quantiles_csv and posteriors_json}
#'   \item{weights_used}{Vector of importance weights used in calculations}
#'   \item{best_subset_data}{Data frame of best subset samples for downstream use}
#' }
#'
#' @details
#' This function performs comprehensive posterior analysis including:
#' \itemize{
#'   \item Weighted quantile calculation using importance sampling
#'   \item Theoretical distribution fitting matching prior families
#'   \item Location-specific parameter handling with proper naming
#'   \item Generation of CSV table and structured JSON output (JSON mirrors priors structure exactly)
#'   \item Extensive diagnostics and metadata preservation
#' }
#'
#' The function expects results to contain:
#' \itemize{
#'   \item \code{likelihood}: Log-likelihood values
#'   \item \code{importance_weight}: Pre-calculated importance weights
#'   \item \code{best_subset_B}: Boolean indicator for best subset membership
#'   \item Parameter columns to analyze
#' }
#'
#' @examples
#' \dontrun{
#' # Standard usage in calibration workflow
#' posterior_analysis <- calc_model_posterior_distributions(
#'   results = calibration_results,
#'   priors_base = priors_base,
#'   config_base = config_base,
#'   output_dir = "plots",
#'   gibbs_temperature = 0.5,
#'   verbose = TRUE
#' )
#'
#' # Access outputs
#' head(posterior_analysis$quantiles_table)
#' names(posterior_analysis$posteriors_object$parameters_global)
#' }
#'
#' @export
#' @importFrom stats weighted.mean var quantile density
#' @importFrom utils write.csv
calc_model_posterior_distributions <- function(results,
                                              priors_base = NULL,
                                              config_base = NULL,
                                              output_dir = NULL,
                                              gibbs_temperature = 0.5,
                                              quantile_probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                              verbose = TRUE) {

  # Start timing
  start_time <- Sys.time()

  # ============================================================================
  # VALIDATION AND SETUP
  # ============================================================================

  # Load default priors if not provided
  if (is.null(priors_base)) {
    if (!exists("priors_default")) {
      data("priors_default", package = "MOSAIC", envir = environment())
    }
    priors_base <- priors_default
    if (verbose) message("Using default priors from priors_default")
  }

  # Validate inputs
  if (!is.data.frame(results)) {
    stop("results must be a data frame")
  }

  if (nrow(results) == 0) {
    stop("results data frame is empty")
  }

  # Check for required columns
  required_cols <- c("likelihood", "importance_weight", "best_subset_B")
  missing_cols <- setdiff(required_cols, names(results))
  if (length(missing_cols) > 0) {
    stop("Required columns missing from results: ", paste(missing_cols, collapse = ", "))
  }

  # Validate quantile probabilities
  if (!is.numeric(quantile_probs) || any(quantile_probs < 0 | quantile_probs > 1)) {
    stop("quantile_probs must be numeric values between 0 and 1")
  }

  # Sort quantile probs for consistency
  quantile_probs <- sort(quantile_probs)

  # ============================================================================
  # IDENTIFY PARAMETERS
  # ============================================================================

  # Define columns to exclude from parameter analysis
  exclude_cols <- c(
    "sim", "iter", "seed", "seed_sim", "seed_iter",
    "likelihood", "log_likelihood", "ll", "loglik",
    "importance_weight", "importance_weights", "weights", "w", "w_tilde",
    "best_subset_B", "retained", "converged", "is_outlier",
    "delta_aic", "aic", "bic",
    "time", "timestamp", "runtime",
    "acceptance", "accept", "rejected",
    "N_j_initial"  # This gets converted to integer counts
  )

  # Get parameter columns
  param_names <- setdiff(names(results), exclude_cols)

  # Further filter to only numeric columns
  param_names <- param_names[sapply(results[param_names], is.numeric)]

  if (length(param_names) == 0) {
    stop("No parameter columns found in results after filtering")
  }

  if (verbose) {
    message(sprintf("Analyzing %d parameters from %d samples",
                   length(param_names), nrow(results)))
  }

  # ============================================================================
  # PREPARE WEIGHTS AND SUBSETS
  # ============================================================================

  # Use existing importance weights
  weights_all <- results$importance_weight

  # Validate weights
  if (any(is.na(weights_all) | !is.finite(weights_all) | weights_all < 0)) {
    n_invalid <- sum(is.na(weights_all) | !is.finite(weights_all) | weights_all < 0)
    warning(sprintf("Found %d invalid weights, setting to zero", n_invalid))
    weights_all[is.na(weights_all) | !is.finite(weights_all) | weights_all < 0] <- 0
  }

  # Normalize weights
  if (sum(weights_all) > 0) {
    weights_all <- weights_all / sum(weights_all)
  } else {
    stop("All weights are zero or invalid")
  }

  # Get best subset
  best_subset_idx <- results$best_subset_B
  if (!any(best_subset_idx)) {
    warning("No samples in best subset B, using all samples")
    best_subset_idx <- rep(TRUE, nrow(results))
  }

  best_subset_data <- results[best_subset_idx, ]
  weights_best <- weights_all[best_subset_idx]

  # Renormalize best subset weights
  if (sum(weights_best) > 0) {
    weights_best <- weights_best / sum(weights_best)
  }

  if (verbose) {
    message(sprintf("Using %d samples in best subset (%.1f%%)",
                   sum(best_subset_idx),
                   100 * sum(best_subset_idx) / nrow(results)))
    ess_all <- calc_model_ess(weights_all)
    ess_best <- calc_model_ess(weights_best)
    message(sprintf("Effective sample size: %.1f (all), %.1f (best subset)",
                   ess_all, ess_best))
  }

  # ============================================================================
  # CLASSIFY PARAMETERS
  # ============================================================================

  # Classify each parameter as global or location-specific
  param_classification <- classify_parameters(param_names, priors_base, config_base)

  n_global <- length(param_classification$global_params)
  n_location <- length(param_classification$location_params)

  if (verbose) {
    message(sprintf("Parameter types: %d global, %d location-specific",
                   n_global, n_location))
    if (n_location > 0 && !is.null(config_base)) {
      locations <- unique(param_classification$location_mapping$location[
        !is.na(param_classification$location_mapping$location)
      ])
      message(sprintf("Locations detected: %s", paste(locations, collapse = ", ")))
    }
  }

  # ============================================================================
  # CALCULATE QUANTILES TABLE
  # ============================================================================

  if (verbose) message("Calculating posterior quantiles...")

  quantiles_table <- tryCatch({
    calculate_quantiles_table(
      param_names = param_names,
      results = best_subset_data,
      weights = weights_best,
      priors_base = priors_base,
      param_classification = param_classification,
      quantile_probs = quantile_probs,
      config_base = config_base
    )
  }, error = function(e) {
    stop("Error in calculate_quantiles_table: ", e$message, "\n",
         "This usually occurs when there are issues with parameter data or prior definitions.")
  })

  # ============================================================================
  # CREATE POSTERIORS OBJECT
  # ============================================================================

  if (verbose) message("Fitting posterior distributions...")

  posteriors_object <- create_posteriors_object(
    param_names = param_names,
    results = best_subset_data,
    weights = weights_best,
    priors_base = priors_base,
    param_classification = param_classification,
    config_base = config_base,
    quantile_probs = quantile_probs,
    gibbs_temperature = gibbs_temperature
  )

  # ============================================================================
  # SAVE OUTPUT FILES
  # ============================================================================

  files_created <- list(
    quantiles_csv = NULL,
    posteriors_json = NULL
  )

  if (!is.null(output_dir)) {
    # Ensure output directory exists
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      if (verbose) message("Created directory: ", output_dir)
    }

    # Save quantiles table as CSV
    csv_path <- file.path(output_dir, "posterior_quantiles.csv")
    write.csv(quantiles_table, file = csv_path, row.names = FALSE)
    files_created$quantiles_csv <- csv_path

    # Save posteriors JSON
    json_path <- file.path(output_dir, "posteriors.json")
    jsonlite::write_json(
      posteriors_object,
      path = json_path,
      pretty = TRUE,
      auto_unbox = TRUE,
      na = "null",
      digits = 6
    )
    files_created$posteriors_json <- json_path

    if (verbose) {
      message("Files saved:")
      for (file in files_created) {
        if (!is.null(file)) message("  ", basename(file))
      }
    }
  }

  # ============================================================================
  # PREPARE RETURN VALUE
  # ============================================================================

  # Calculate runtime
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (verbose) {
    message(sprintf("Posterior analysis complete in %.1f seconds", runtime))
  }

  # Return comprehensive results
  return(list(
    quantiles_table = quantiles_table,
    posteriors_object = posteriors_object,
    files = files_created,
    weights_used = weights_all,
    best_subset_data = best_subset_data
  ))
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Classify parameters as global or location-specific
#' @noRd
classify_parameters <- function(param_names, priors_base, config_base) {

  global_params <- character()
  location_params <- character()
  location_mapping <- data.frame(
    parameter = character(),
    base_name = character(),
    location_index = integer(),
    location = character(),
    stringsAsFactors = FALSE
  )

  for (param in param_names) {
    # Check if it's a known global parameter
    if (param %in% names(priors_base$parameters_global)) {
      global_params <- c(global_params, param)
    }
    # Check for location-specific pattern: param_j_N or param_ISO
    else if (grepl("_j_\\d+$", param)) {
      # New style: mu_j_1, mu_j_2
      location_params <- c(location_params, param)
      base_name <- gsub("_\\d+$", "", param)
      location_idx <- as.numeric(gsub(".*_(\\d+)$", "\\1", param))

      location_name <- if (!is.null(config_base) && location_idx <= length(config_base$location_name)) {
        config_base$location_name[location_idx]
      } else {
        paste0("Location_", location_idx)
      }

      location_mapping <- rbind(location_mapping, data.frame(
        parameter = param,
        base_name = base_name,
        location_index = location_idx,
        location = location_name,
        stringsAsFactors = FALSE
      ))
    }
    else if (grepl("_[A-Z]{3}$", param)) {
      # Old style: beta_j0_ETH
      location_params <- c(location_params, param)
      base_name <- gsub("_[A-Z]{3}$", "", param)
      location_iso <- gsub(".*_([A-Z]{3})$", "\\1", param)

      # Try to find location index
      location_idx <- if (!is.null(config_base)) {
        which(config_base$iso == location_iso)[1]
      } else {
        NA_integer_
      }

      location_mapping <- rbind(location_mapping, data.frame(
        parameter = param,
        base_name = base_name,
        location_index = location_idx,
        location = location_iso,
        stringsAsFactors = FALSE
      ))
    }
    else {
      # Unknown parameter - treat as global
      global_params <- c(global_params, param)
    }
  }

  return(list(
    global_params = global_params,
    location_params = location_params,
    location_mapping = location_mapping
  ))
}

#' Calculate quantiles table for all parameters
#' @noRd
calculate_quantiles_table <- function(param_names, results, weights, priors_base,
                                     param_classification, quantile_probs, config_base = NULL) {

  quantile_names <- paste0("q", sprintf("%04.1f", quantile_probs * 100))
  quantiles_list <- list()

  for (param in param_names) {
    # Get parameter values
    param_values <- results[[param]]

    # Skip if all values are NA
    if (all(is.na(param_values))) {
      next
    }

    # Filter to valid values
    valid_idx <- !is.na(param_values) & is.finite(param_values)
    if (sum(valid_idx) == 0) {
      next
    }

    param_values_valid <- param_values[valid_idx]
    weights_valid <- weights[valid_idx]

    # Renormalize weights
    weights_valid <- weights_valid / sum(weights_valid)

    # Calculate statistics
    posterior_mean <- sum(param_values_valid * weights_valid)
    posterior_var <- sum(weights_valid * (param_values_valid - posterior_mean)^2)
    posterior_sd <- sqrt(posterior_var)

    # Calculate quantiles with error handling
    param_quantiles <- tryCatch({
      weighted_quantiles(param_values_valid, weights_valid, quantile_probs)
    }, error = function(e) {
      # If weighted_quantiles fails, return NA values
      rep(NA_real_, length(quantile_probs))
    })

    # Calculate ESS
    ess <- calc_model_ess(weights_valid)

    # Get prior information
    prior_info <- get_prior_info(param, priors_base, config_base)

    # Debug message if prior not found (commented out to reduce noise)
    # Uncomment the following lines if you need to debug missing priors
    # if (is.null(prior_info) && verbose) {
    #   message("  Note: No prior found for parameter: ", param)
    # }

    # Determine parameter type and location
    if (param %in% param_classification$global_params) {
      param_type <- "global"
      location <- NA_character_
    } else {
      param_type <- "location_specific"
      loc_info <- param_classification$location_mapping[
        param_classification$location_mapping$parameter == param,
      ]
      location <- if (nrow(loc_info) > 0) loc_info$location[1] else NA_character_
    }

    # Create row - handle NULL prior_info properly
    param_row <- data.frame(
      parameter = param,
      param_type = param_type,
      location = location,
      prior_distribution = if (!is.null(prior_info) && !is.null(prior_info$distribution)) {
        prior_info$distribution
      } else {
        NA_character_
      },
      prior_description = if (!is.null(prior_info) && !is.null(prior_info$description)) {
        prior_info$description
      } else {
        NA_character_
      },
      n_samples = length(param_values_valid),
      eff_sample_size = ess,
      posterior_mean = posterior_mean,
      posterior_sd = posterior_sd,
      stringsAsFactors = FALSE
    )

    # Add quantiles
    for (i in seq_along(quantile_names)) {
      param_row[[quantile_names[i]]] <- param_quantiles[i]
    }

    quantiles_list[[param]] <- param_row
  }

  # Combine all rows
  if (length(quantiles_list) > 0) {
    quantiles_table <- do.call(rbind, quantiles_list)
    rownames(quantiles_table) <- NULL
  } else {
    # Create empty table with correct structure
    quantiles_table <- data.frame(
      parameter = character(),
      param_type = character(),
      location = character(),
      prior_distribution = character(),
      prior_description = character(),
      n_samples = integer(),
      eff_sample_size = numeric(),
      posterior_mean = numeric(),
      posterior_sd = numeric(),
      stringsAsFactors = FALSE
    )
    for (q_name in quantile_names) {
      quantiles_table[[q_name]] <- numeric()
    }
  }

  # Add metadata as attributes
  attr(quantiles_table, "created") <- Sys.time()
  attr(quantiles_table, "quantile_probs") <- quantile_probs

  return(quantiles_table)
}

#' Create structured posteriors object
#' @noRd
create_posteriors_object <- function(param_names, results, weights, priors_base,
                                    param_classification, config_base,
                                    quantile_probs, gibbs_temperature) {

  # Initialize structure - mirror priors object exactly
  posteriors_object <- list(
    metadata = list(
      description = "Posterior distributions from MOSAIC calibration"
    ),
    parameters_global = list(),
    parameters_location = list()
  )

  quantile_names <- paste0("q", sprintf("%04.1f", quantile_probs * 100))

  for (param in param_names) {
    # Get parameter values
    param_values <- results[[param]]

    # Skip if all NA
    if (all(is.na(param_values))) {
      next
    }

    # Filter to valid values
    valid_idx <- !is.na(param_values) & is.finite(param_values)
    if (sum(valid_idx) == 0) {
      next
    }

    param_values_valid <- param_values[valid_idx]
    weights_valid <- weights[valid_idx]
    weights_valid <- weights_valid / sum(weights_valid)

    # Get prior information
    prior_info <- get_prior_info(param, priors_base, config_base)

    # Debug message if prior not found (commented out to reduce noise)
    # Uncomment the following lines if you need to debug missing priors
    # if (is.null(prior_info) && verbose) {
    #   message("  Note: No prior found for parameter: ", param)
    # }
    if (is.null(prior_info)) {
      next  # Skip parameters without prior information
    }

    # Calculate statistics
    posterior_mean <- sum(param_values_valid * weights_valid)
    posterior_var <- sum(weights_valid * (param_values_valid - posterior_mean)^2)
    posterior_sd <- sqrt(posterior_var)

    # Calculate quantiles with error handling
    param_quantiles <- tryCatch({
      weighted_quantiles(param_values_valid, weights_valid, quantile_probs)
    }, error = function(e) {
      # If weighted_quantiles fails, return NA values
      rep(NA_real_, length(quantile_probs))
    })
    names(param_quantiles) <- quantile_names

    # Calculate ESS
    ess <- calc_model_ess(weights_valid)

    # Attempt to fit theoretical distribution
    fitted_result <- fit_posterior_distribution(
      param_values_valid,
      weights_valid,
      prior_info,
      param_quantiles
    )

    # Create posterior entry - mirror priors structure exactly
    posterior_entry <- list(
      distribution = prior_info$distribution,
      description = if (!is.null(prior_info$description)) {
        prior_info$description
      } else {
        param
      },
      parameters = fitted_result$parameters
    )

    # Assign to appropriate section
    if (param %in% param_classification$global_params) {
      posteriors_object$parameters_global[[param]] <- posterior_entry
    } else {
      # For location-specific parameters, use descriptive name
      loc_info <- param_classification$location_mapping[
        param_classification$location_mapping$parameter == param,
      ]
      if (nrow(loc_info) > 0) {
        descriptive_name <- paste0(loc_info$base_name[1], "_", loc_info$location[1])
        posteriors_object$parameters_location[[descriptive_name]] <- posterior_entry
      } else {
        posteriors_object$parameters_location[[param]] <- posterior_entry
      }
    }
  }

  return(posteriors_object)
}

#' Get prior information for a parameter
#' @noRd
get_prior_info <- function(param, priors_base, config_base = NULL) {

  # 1. Direct match in global parameters
  if (param %in% names(priors_base$parameters_global)) {
    return(priors_base$parameters_global[[param]])
  }

  # 2. Extract location code if present (handles ETH, KEN, etc.)
  location_code <- NULL
  if (grepl("_[A-Z]{3}$", param)) {
    location_code <- gsub(".*_([A-Z]{3})$", "\\1", param)
  }

  # 3. Try multiple parameter name transformations
  # Define transformation rules to get potential prior names
  transformations <- list(
    # Original parameter name
    function(p) p,

    # Remove location code: tau_j_ETH -> tau_j
    function(p) gsub("_[A-Z]{3}$", "", p),

    # Remove _j_1 pattern: mu_j_1 -> mu_j
    function(p) gsub("_j_\\d+$", "_j", p),

    # Remove just the number: mu_j_1 -> mu_j
    function(p) gsub("_\\d+$", "", p),

    # For a_1_j_ETH -> a1
    function(p) {
      if (grepl("^[ab]_[12]_j_[A-Z]{3}$", p)) {
        gsub("^([ab])_([12])_j_[A-Z]{3}$", "\\1\\2", p)
      } else p
    },

    # For beta_j0_hum_ETH -> beta_j0_tot
    function(p) {
      if (grepl("beta_j0_hum", p)) {
        gsub("beta_j0_hum.*", "beta_j0_tot", p)
      } else p
    },

    # For tau_j_ETH -> tau_i (tau_j is mislabeled)
    function(p) gsub("tau_j", "tau_i", p),

    # Remove location and try base: tau_i_ETH -> tau_i
    function(p) gsub("_[ij]_[A-Z]{3}$", "_i", p),

    # Try without _j suffix: theta_j -> theta
    function(p) gsub("_j$", "", p),

    # Try adding _j if not present
    function(p) {
      if (!grepl("_j", p) && !grepl("_\\d", p)) {
        paste0(p, "_j")
      } else p
    }
  )

  # 4. Try each transformation
  for (transform in transformations) {
    candidate <- transform(param)

    # Skip if transformation didn't change anything or returned same as param
    if (is.null(candidate) || candidate == param && !identical(transform, transformations[[1]])) {
      next
    }

    # Check in parameters_location
    if (candidate %in% names(priors_base$parameters_location)) {
      prior_info <- priors_base$parameters_location[[candidate]]

      # Handle nested location-specific structure
      if (!is.null(prior_info$location)) {
        # Use extracted location code or from config
        loc_to_use <- location_code
        if (is.null(loc_to_use) && !is.null(config_base$location_name)) {
          loc_to_use <- config_base$location_name[1]
        }

        if (!is.null(loc_to_use) && loc_to_use %in% names(prior_info$location)) {
          return(list(
            description = prior_info$description,
            distribution = prior_info$location[[loc_to_use]]$distribution,
            parameters = prior_info$location[[loc_to_use]]$parameters
          ))
        }

        # No location match - return NULL (do NOT substitute another location!)
        return(NULL)
      }

      # Return the prior info if no nested structure
      return(prior_info)
    }
  }

  # 5. Check initial conditions mapping as last resort
  ic_mapping <- list(
    "S_j_initial" = "S_prop",
    "E_j_initial" = "E_prop",
    "I_j_initial" = "I_prop",
    "R_j_initial" = "R_prop",
    "V1_j_initial" = "V1_prop",
    "V2_j_initial" = "V2_prop"
  )

  base_param_ic <- gsub("_\\d+$", "", param)  # Remove location index
  if (base_param_ic %in% names(ic_mapping)) {
    prior_name <- ic_mapping[[base_param_ic]]
    if (!is.null(priors_base$parameters_initial) &&
        prior_name %in% names(priors_base$parameters_initial)) {
      return(priors_base$parameters_initial[[prior_name]])
    }
  }

  # No match found
  return(NULL)
}

#' Fit theoretical posterior distribution

#' @noRd
fit_posterior_distribution <- function(param_values, weights, prior_info, param_quantiles) {

  dist_type <- tolower(prior_info$distribution)
  fitted_params <- NULL
  fitting_success <- FALSE
  fitting_method <- "quantile_matching"
  kl_divergence <- NA_real_

  # Calculate moments
  posterior_mean <- sum(param_values * weights)
  posterior_var <- sum(weights * (param_values - posterior_mean)^2)
  posterior_sd <- sqrt(posterior_var)

  # Try to fit distribution based on type
  tryCatch({

    if (dist_type == "gamma" && all(param_values > 0)) {
      # Use method of moments for gamma
      # E[X] = shape/rate, Var[X] = shape/rate^2
      fitted_rate <- posterior_mean / posterior_var
      fitted_shape <- posterior_mean * fitted_rate

      if (fitted_shape > 0 && fitted_rate > 0) {
        fitted_params <- list(shape = fitted_shape, rate = fitted_rate)
        fitting_success <- TRUE
        fitting_method <- "moment_matching"
      }
    }

    else if (dist_type == "beta" && all(param_values >= 0 & param_values <= 1)) {
      # Use method of moments for beta
      # Avoid numerical issues at boundaries
      param_values_bounded <- pmax(1e-6, pmin(1 - 1e-6, param_values))
      mean_bounded <- sum(param_values_bounded * weights)
      var_bounded <- sum(weights * (param_values_bounded - mean_bounded)^2)

      if (var_bounded < mean_bounded * (1 - mean_bounded)) {
        common_factor <- (mean_bounded * (1 - mean_bounded) / var_bounded) - 1
        fitted_shape1 <- mean_bounded * common_factor
        fitted_shape2 <- (1 - mean_bounded) * common_factor

        if (fitted_shape1 > 0 && fitted_shape2 > 0) {
          fitted_params <- list(shape1 = fitted_shape1, shape2 = fitted_shape2)
          fitting_success <- TRUE
          fitting_method <- "moment_matching"
        }
      }
    }

    else if (dist_type == "normal") {
      fitted_params <- list(mean = posterior_mean, sd = posterior_sd)
      fitting_success <- TRUE
      fitting_method <- "moment_matching"
    }

    else if (dist_type == "lognormal" && all(param_values > 0)) {
      log_values <- log(param_values)
      log_mean <- sum(log_values * weights)
      log_var <- sum(weights * (log_values - log_mean)^2)

      fitted_params <- list(meanlog = log_mean, sdlog = sqrt(log_var))
      fitting_success <- TRUE
      fitting_method <- "log_scale_moments"
    }

    else if (dist_type == "uniform") {
      fitted_params <- list(
        min = min(param_values),
        max = max(param_values)
      )
      fitting_success <- TRUE
      fitting_method <- "range_matching"
    }

  }, error = function(e) {
    # Fitting failed - will use empirical fallback
  })

  # Fallback to empirical if fitting failed
  if (!fitting_success || is.null(fitted_params)) {
    fitted_params <- list(
      empirical_mean = posterior_mean,
      empirical_sd = posterior_sd,
      empirical_median = param_quantiles[["q50.0"]]
    )
    fitting_method <- "empirical"
    fitting_success <- FALSE
  }

  # Calculate KL divergence if fitting succeeded
  if (fitting_success && !is.null(prior_info$parameters)) {
    tryCatch({
      kl_divergence <- calculate_kl_divergence(
        param_values,
        weights,
        fitted_params,
        prior_info$parameters,
        dist_type
      )
    }, error = function(e) {
      kl_divergence <- NA_real_
    })
  }

  return(list(
    parameters = fitted_params,
    diagnostics = list(
      fitting_success = fitting_success,
      fitting_method = fitting_method,
      distribution_family = dist_type
    ),
    kl_divergence = kl_divergence
  ))
}

#' Calculate KL divergence between posterior and prior
#' @noRd
calculate_kl_divergence <- function(param_values, weights, posterior_params,
                                   prior_params, dist_type) {

  # Create evaluation grid
  x_range <- range(param_values)
  x_eval <- seq(x_range[1], x_range[2], length.out = 1000)

  # Calculate densities based on distribution type
  tryCatch({

    if (dist_type == "gamma") {
      prior_density <- dgamma(x_eval,
                             shape = prior_params$shape,
                             rate = prior_params$rate)
      posterior_density <- dgamma(x_eval,
                                shape = posterior_params$shape,
                                rate = posterior_params$rate)
    }

    else if (dist_type == "beta") {
      prior_density <- dbeta(x_eval,
                           shape1 = prior_params$shape1,
                           shape2 = prior_params$shape2)
      posterior_density <- dbeta(x_eval,
                               shape1 = posterior_params$shape1,
                               shape2 = posterior_params$shape2)
    }

    else if (dist_type == "normal") {
      prior_density <- dnorm(x_eval,
                           mean = prior_params$mean,
                           sd = prior_params$sd)
      posterior_density <- dnorm(x_eval,
                               mean = posterior_params$mean,
                               sd = posterior_params$sd)
    }

    else if (dist_type == "lognormal") {
      prior_density <- dlnorm(x_eval,
                            meanlog = prior_params$meanlog,
                            sdlog = prior_params$sdlog)
      posterior_density <- dlnorm(x_eval,
                                meanlog = posterior_params$meanlog,
                                sdlog = posterior_params$sdlog)
    }

    else if (dist_type == "uniform") {
      prior_density <- dunif(x_eval,
                           min = prior_params$min,
                           max = prior_params$max)
      posterior_density <- dunif(x_eval,
                               min = posterior_params$min,
                               max = posterior_params$max)
    }

    else {
      return(NA_real_)
    }

    # Calculate KL divergence: KL(posterior || prior)
    # Add small epsilon to avoid log(0)
    eps <- 1e-10
    posterior_density <- pmax(posterior_density, eps)
    prior_density <- pmax(prior_density, eps)

    # Normalize to ensure they sum to 1
    posterior_density <- posterior_density / sum(posterior_density)
    prior_density <- prior_density / sum(prior_density)

    # KL divergence
    kl <- sum(posterior_density * log(posterior_density / prior_density))

    return(kl)

  }, error = function(e) {
    return(NA_real_)
  })
}