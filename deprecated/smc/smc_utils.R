# ==============================================================================
# SMC Utility Functions
# ==============================================================================
# Helper functions for Sequential Monte Carlo exact posterior correction
# ==============================================================================

#' Parse Priors for SMC
#'
#' @description
#' Parses priors.json into flat structure compatible with evaluate_prior_densities().
#' Handles both global and location-specific parameters from nested JSON structure.
#' Supports dual lognormal parameterizations: (meanlog, sdlog) and (mean, sd).
#'
#' @param priors_json Raw priors loaded from JSON (nested structure)
#' @param param_names Optional character vector of parameter names to filter (NULL = all)
#' @param verbose Logical, print parsing diagnostics
#'
#' @return Named list of flattened prior specifications
#' @export
#'
#' @examples
#' \dontrun{
#' priors_raw <- jsonlite::read_json("priors.json")
#' priors_flat <- parse_priors_for_smc(priors_raw)
#' }
parse_priors_for_smc <- function(priors_json, param_names = NULL, verbose = FALSE) {

    if (verbose) message("Parsing priors for SMC...")

    priors_flat <- list()

    # Check for required structure
    if (!is.list(priors_json)) {
        stop("priors_json must be a list")
    }

    # -------------------------------------------------------------------
    # Parse global parameters
    # -------------------------------------------------------------------

    if ("parameters_global" %in% names(priors_json)) {
        global_params <- priors_json$parameters_global

        if (verbose) message("  Processing ", length(global_params), " global parameters")

        for (param_name in names(global_params)) {
            # Skip if parameter names filter provided and doesn't match
            if (!is.null(param_names) && !(param_name %in% param_names)) {
                next
            }

            prior_spec <- global_params[[param_name]]

            # Flatten nested structure
            priors_flat[[param_name]] <- .flatten_prior_spec(prior_spec, param_name)
        }
    }

    # -------------------------------------------------------------------
    # Parse location-specific parameters
    # -------------------------------------------------------------------

    if ("parameters_location" %in% names(priors_json)) {
        loc_params <- priors_json$parameters_location

        if (verbose) message("  Processing location-specific parameters")

        for (base_param in names(loc_params)) {
            # Location params have structure: parameters_location$tau_i$ETH
            locations <- loc_params[[base_param]]

            if (!is.list(locations)) {
                warning("Skipping malformed location parameter: ", base_param)
                next
            }

            for (location in names(locations)) {
                # Create parameter name: tau_i_ETH
                param_name <- paste0(base_param, "_", location)

                # Skip if filter provided and doesn't match
                if (!is.null(param_names) && !(param_name %in% param_names)) {
                    next
                }

                prior_spec <- locations[[location]]
                priors_flat[[param_name]] <- .flatten_prior_spec(prior_spec, param_name)
            }
        }
    }

    if (verbose) {
        message("  Parsed ", length(priors_flat), " parameter priors")
        if (!is.null(param_names)) {
            n_missing <- sum(!(param_names %in% names(priors_flat)))
            if (n_missing > 0) {
                missing_params <- param_names[!(param_names %in% names(priors_flat))]
                warning("  ", n_missing, " requested parameters not found in priors: ",
                       paste(head(missing_params, 5), collapse = ", "),
                       if (n_missing > 5) " ..." else "")
            }
        }
    }

    return(priors_flat)
}


#' Flatten Prior Specification (Internal Helper)
#'
#' @description
#' Converts nested prior spec to flat structure for density evaluation.
#' Supports both lognormal parameterizations: (meanlog, sdlog) and (mean, sd).
#'
#' @param prior_spec Nested prior specification from JSON
#' @param param_name Parameter name (for error messages)
#' @return Flattened prior specification
#' @keywords internal
.flatten_prior_spec <- function(prior_spec, param_name = "unknown") {

    if (!is.list(prior_spec)) {
        stop("Prior specification must be a list for parameter: ", param_name)
    }

    if (!("distribution" %in% names(prior_spec))) {
        stop("Prior specification missing 'distribution' field for parameter: ", param_name)
    }

    dist <- prior_spec$distribution

    # Start with distribution type
    flat <- list(distribution = dist)

    # Extract parameters (may be nested)
    if ("parameters" %in% names(prior_spec)) {
        params <- prior_spec$parameters
    } else {
        # Already flat structure
        params <- prior_spec
    }

    # Map nested parameters to flat structure based on distribution
    if (dist == "beta") {
        # Beta: shape1/shape2 → alpha/beta (R naming convention for dbeta)
        if ("shape1" %in% names(params) && "shape2" %in% names(params)) {
            flat$alpha <- as.numeric(params$shape1)
            flat$beta <- as.numeric(params$shape2)
        } else if ("alpha" %in% names(params) && "beta" %in% names(params)) {
            flat$alpha <- as.numeric(params$alpha)
            flat$beta <- as.numeric(params$beta)
        } else {
            stop("Beta distribution requires shape1/shape2 or alpha/beta for parameter: ", param_name)
        }

    } else if (dist == "gamma") {
        # Gamma: shape and rate
        if ("shape" %in% names(params) && "rate" %in% names(params)) {
            flat$shape <- as.numeric(params$shape)
            flat$rate <- as.numeric(params$rate)
        } else {
            stop("Gamma distribution requires shape and rate for parameter: ", param_name)
        }

    } else if (dist == "lognormal") {
        # Lognormal: supports both (meanlog, sdlog) and (mean, sd) parameterizations
        if ("meanlog" %in% names(params) && "sdlog" %in% names(params)) {
            # Format 1: meanlog/sdlog (log-scale parameters)
            flat$meanlog <- as.numeric(params$meanlog)
            flat$sdlog <- as.numeric(params$sdlog)
        } else if ("mean" %in% names(params) && "sd" %in% names(params)) {
            # Format 2: mean/sd (original-scale parameters)
            # Convert to meanlog/sdlog using standard lognormal formulas
            mean_val <- as.numeric(params$mean)
            sd_val <- as.numeric(params$sd)

            if (mean_val <= 0 || sd_val <= 0) {
                stop("Lognormal mean and sd must be positive for parameter: ", param_name)
            }

            # Coefficient of variation squared
            cv2 <- (sd_val / mean_val)^2
            # Convert to log-scale parameters
            flat$sdlog <- sqrt(log(1 + cv2))
            flat$meanlog <- log(mean_val) - flat$sdlog^2/2
        } else {
            stop("Lognormal distribution requires (meanlog, sdlog) OR (mean, sd) for parameter: ", param_name)
        }

    } else if (dist == "normal") {
        # Normal: mean and sd
        if ("mean" %in% names(params) && "sd" %in% names(params)) {
            flat$mean <- as.numeric(params$mean)
            flat$sd <- as.numeric(params$sd)
        } else {
            stop("Normal distribution requires mean and sd for parameter: ", param_name)
        }

    } else if (dist == "uniform") {
        # Uniform: min and max
        if ("min" %in% names(params) && "max" %in% names(params)) {
            flat$min <- as.numeric(params$min)
            flat$max <- as.numeric(params$max)
        } else {
            stop("Uniform distribution requires min and max for parameter: ", param_name)
        }

    } else {
        warning("Unknown distribution type for parameter ", param_name, ": ", dist)
        # Pass through all parameters as-is
        for (param_key in names(params)) {
            flat[[param_key]] <- params[[param_key]]
        }
    }

    return(flat)
}


#' Save SMC Results
#'
#' @description
#' Saves SMC posterior results to output directory with proper structure.
#'
#' @param smc_result SMC result object from compute_smc_posterior
#' @param output_dir Directory to save results
#' @param verbose Logical, print progress messages
#'
#' @return Invisible NULL
#' @keywords internal
.save_smc_results <- function(smc_result, output_dir, verbose = FALSE) {

    # Create output directory
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    if (verbose) message("Saving SMC results to: ", output_dir)

    # Save weights
    weights_file <- file.path(output_dir, "weights.parquet")
    weights_df <- data.frame(
        sample_id = 1:length(smc_result$weights$weights),
        weight = smc_result$weights$weights,
        log_weight = smc_result$weights$log_weights,
        log_weight_unnorm = smc_result$weights$log_weights_unnorm
    )
    arrow::write_parquet(weights_df, weights_file)
    if (verbose) message("  Weights saved: weights.parquet")

    # Save log components (for diagnostics)
    if (!is.null(smc_result$log_components)) {
        components_file <- file.path(output_dir, "log_components.parquet")
        components_df <- data.frame(
            sample_id = 1:length(smc_result$log_components$log_priors),
            log_prior = smc_result$log_components$log_priors,
            log_likelihood = smc_result$log_components$log_likelihoods,
            log_q_npe = smc_result$log_components$log_q_npe
        )
        arrow::write_parquet(components_df, components_file)
        if (verbose) message("  Log components saved: log_components.parquet")
    }

    # Save resampled posterior (if created)
    if (!is.null(smc_result$smc_posterior)) {
        posterior_file <- file.path(output_dir, "posterior_samples.parquet")
        arrow::write_parquet(as.data.frame(smc_result$smc_posterior), posterior_file)
        if (verbose) message("  Resampled posterior saved: posterior_samples.parquet")

        # Calculate and save quantiles (BFRS/NPE-compatible format)
        quantiles_file <- file.path(output_dir, "posterior_quantiles.csv")
        quantiles_df <- .calculate_smc_quantiles(
            smc_result$smc_posterior,
            c(0.025, 0.25, 0.5, 0.75, 0.975)
        )
        write.csv(quantiles_df, quantiles_file, row.names = FALSE)
        if (verbose) message("  Quantiles saved: posterior_quantiles.csv")
    }

    # Save diagnostics
    diagnostics_file <- file.path(output_dir, "diagnostics.json")

    # Calculate unique resampled samples if resampling was done
    n_unique_resampled <- if (!is.null(smc_result$smc_posterior)) {
        nrow(unique(smc_result$smc_posterior))
    } else {
        NA
    }

    # Determine reliability based on ESS
    ess_percent <- 100 * smc_result$weights$ess / smc_result$n_subset
    reliability <- if (ess_percent < 0.5) {
        "UNRELIABLE - extreme weight collapse"
    } else if (ess_percent < 10) {
        "POOR - low ESS suggests poor NPE approximation"
    } else if (ess_percent < 30) {
        "FAIR - moderate ESS"
    } else {
        "GOOD - sufficient ESS"
    }

    diagnostics <- list(
        n_samples = smc_result$n_samples,
        n_subset = smc_result$n_subset,
        ess = smc_result$weights$ess,
        ess_percent = ess_percent,
        reliability = reliability,
        n_unique_resampled = n_unique_resampled,
        resampled = smc_result$resampled,
        max_weight = max(smc_result$weights$weights),
        min_weight = min(smc_result$weights$weights),
        n_nonzero_weights = sum(smc_result$weights$weights > 1e-10),
        mean_log_prior = mean(smc_result$log_components$log_priors, na.rm = TRUE),
        mean_log_likelihood = mean(smc_result$log_components$log_likelihoods, na.rm = TRUE),
        mean_log_q_npe = mean(smc_result$log_components$log_q_npe, na.rm = TRUE),
        log_likelihood_range = diff(range(smc_result$log_components$log_likelihoods, na.rm = TRUE)),
        n_failed_simulations = sum(!is.finite(smc_result$log_components$log_likelihoods)),
        timestamp = Sys.time()
    )
    jsonlite::write_json(diagnostics, diagnostics_file, pretty = TRUE, auto_unbox = TRUE)
    if (verbose) message("  Diagnostics saved: diagnostics.json")

    return(invisible(NULL))
}


#' Calculate SMC Quantiles (Internal Helper)
#'
#' @description
#' Calculate quantiles for SMC posterior samples in BFRS/NPE-compatible long format.
#' Matches format of posterior_quantiles.csv from BFRS and NPE stages.
#'
#' @param samples Matrix of posterior samples
#' @param probs Quantile levels
#' @return Data frame with quantiles in BFRS-compatible long format
#' @keywords internal
.calculate_smc_quantiles <- function(samples, probs) {

    n_params <- ncol(samples)
    param_names <- colnames(samples)
    if (is.null(param_names)) {
        param_names <- paste0("param_", 1:n_params)
    }

    # Load estimated_parameters for metadata (with error handling)
    estimated_parameters <- NULL
    tryCatch({
        data(estimated_parameters, package = "MOSAIC", envir = environment())
    }, error = function(e) {
        # Fallback: create empty data frame if estimated_parameters is not available
        estimated_parameters <<- data.frame(
            parameter_name = character(0),
            description = character(0),
            category = character(0),
            param_type = character(0),
            stringsAsFactors = FALSE
        )
    })

    # Initialize results data frame (long format like BFRS/NPE)
    quantiles_df <- data.frame()

    # Process each parameter
    for (i in 1:n_params) {
        param_name <- param_names[i]
        param_values <- samples[, i]

        # Get parameter metadata from estimated_parameters
        param_info <- estimated_parameters[estimated_parameters$parameter_name == param_name, ]

        if (nrow(param_info) == 0) {
            # Parameter not in estimated_parameters, use defaults
            description <- param_name
            category <- "unknown"

            # Detect location-specific parameters by suffix pattern
            if (grepl("_[A-Z]{2,3}$", param_name)) {
                param_type <- "location"
                location <- sub(".*_([A-Z]{2,3})$", "\\1", param_name)
            } else {
                param_type <- "global"
                location <- NA_character_
            }
        } else {
            description <- if(length(param_info$description) > 0 && !is.na(param_info$description[1])) {
                as.character(param_info$description[1])
            } else {
                param_name
            }

            category <- if(length(param_info$category) > 0 && !is.na(param_info$category[1])) {
                as.character(param_info$category[1])
            } else {
                "unknown"
            }

            param_type <- if(length(param_info$param_type) > 0 && !is.na(param_info$param_type[1])) {
                as.character(param_info$param_type[1])
            } else {
                "global"
            }

            # Extract location from parameter name if it's a location-specific parameter
            location <- if(param_type == "location" && grepl("_[A-Z]{3}$", param_name)) {
                sub(".*_([A-Z]{3})$", "\\1", param_name)
            } else {
                NA_character_
            }
        }

        # Calculate quantiles for this parameter with validation
        if (length(param_values) == 0 || all(is.na(param_values))) {
            # Handle empty or all-NA parameter values
            q_values <- rep(NA_real_, length(probs))
            names(q_values) <- paste0("q", probs)
            param_mean <- NA_real_
            param_sd <- NA_real_
        } else {
            # Remove NAs for quantile calculation
            param_values_clean <- param_values[!is.na(param_values)]
            if (length(param_values_clean) > 0) {
                q_values <- quantile(param_values_clean, probs = probs, na.rm = TRUE)
                param_mean <- mean(param_values_clean, na.rm = TRUE)
                param_sd <- sd(param_values_clean, na.rm = TRUE)
            } else {
                q_values <- rep(NA_real_, length(probs))
                names(q_values) <- paste0("q", probs)
                param_mean <- NA_real_
                param_sd <- NA_real_
            }
        }

        # Ensure q_values has the expected length
        if (length(q_values) != length(probs)) {
            warning("Quantile calculation failed for parameter ", param_name, ", using NA values")
            q_values <- rep(NA_real_, length(probs))
            names(q_values) <- paste0("q", probs)
        }

        # Create row in BFRS/NPE format
        param_row <- data.frame(
            parameter = as.character(param_name),
            description = as.character(description),
            category = as.character(category),
            param_type = as.character(param_type),
            location = as.character(location),
            prior_distribution = NA_character_,
            type = "smc",
            mean = param_mean,
            sd = param_sd,
            mode = NA_real_,
            kl = NA_real_,
            stringsAsFactors = FALSE
        )

        # Add quantile columns with proper naming and validation
        for (j in seq_along(probs)) {
            col_name <- sprintf("q%s", probs[j])
            if (j <= length(q_values)) {
                param_row[[col_name]] <- q_values[j]
            } else {
                param_row[[col_name]] <- NA_real_
            }
        }

        quantiles_df <- rbind(quantiles_df, param_row)
    }

    return(quantiles_df)
}
