#' Calculate Model Posterior Distributions from Quantiles
#'
#' @description
#' Fits theoretical distributions to posterior quantiles and creates a posteriors.json
#' file that mirrors the structure of the priors.json file with updated parameter values.
#'
#' @importFrom MOSAIC fit_beta_from_ci fit_gamma_from_ci fit_gompertz_from_ci
#' @importFrom MOSAIC fit_lognormal_from_ci fit_normal_from_ci fit_truncnorm_from_ci fit_uniform_from_ci
#'
#' @param quantiles_file Path to the posterior_quantiles.csv file (default: "./results/posterior_quantiles.csv")
#' @param priors_file Path to the priors.json file to use as template
#' @param output_dir Directory to save posteriors.json (default: "./results")
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return List containing:
#' \describe{
#'   \item{posteriors}{The complete posteriors object}
#'   \item{n_parameters_updated}{Number of parameters successfully updated}
#'   \item{n_parameters_failed}{Number of parameters that failed to fit}
#'   \item{output_file}{Path to the created posteriors.json file}
#' }
#'
#' @details
#' The function:
#' 1. Reads the posterior_quantiles.csv file produced by calc_model_posterior_quantiles or est_npe_posterior
#' 2. Processes all quantile rows in the file (user controls what to include)
#' 3. Loads the priors.json as a template structure
#' 4. For each posterior parameter in the quantiles table:
#'    - Extracts the 2.5%, 50%, and 97.5% quantiles
#'    - Calls the appropriate fit_*_from_ci function based on distribution type
#'    - Updates the corresponding entry in the posteriors structure
#' 5. Preserves all non-estimated parameters from the priors unchanged
#' 6. Properly handles location-specific parameters for single or multiple countries
#' 7. Writes posteriors.json to the output directory
#'
#' @examples
#' \dontrun{
#' # Standard usage after running calc_model_posterior_quantiles
#' posterior_dists <- calc_model_posterior_distributions(
#'   quantiles_file = "./results/posterior_quantiles.csv",
#'   priors_file = "./config/priors.json",
#'   output_dir = "./results"
#' )
#' }
#'
#' @export
calc_model_posterior_distributions <- function(
    quantiles_file = "./results/posterior_quantiles.csv",
    priors_file,
    output_dir = "./results",
    verbose = TRUE
) {

    if (verbose) message("\n=== Calculating Posterior Distributions from Quantiles ===\n")

    # Validate inputs
    if (!file.exists(quantiles_file)) {
        stop("Quantiles file not found: ", quantiles_file)
    }
    if (!file.exists(priors_file)) {
        stop("Priors file not found: ", priors_file)
    }

    # Create output directory if needed
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        if (verbose) message("Created output directory: ", output_dir)
    }

    # Load quantiles table
    if (verbose) message("Loading posterior quantiles from: ", quantiles_file)
    quantiles_all <- read.csv(quantiles_file, stringsAsFactors = FALSE)

    # Filter to only posterior rows if type column exists
    # (quantiles file may contain both prior and posterior rows for comparison)
    if ("type" %in% names(quantiles_all)) {
        types_found <- unique(quantiles_all$type)
        if (verbose) {
            message("  Types found in file: ", paste(types_found, collapse = ", "))
        }

        # Keep only posterior rows for fitting
        if ("posterior" %in% types_found) {
            quantiles <- quantiles_all[quantiles_all$type == "posterior", ]
            if (verbose) {
                message("  Filtered to posterior rows: ", nrow(quantiles), " of ", nrow(quantiles_all))
            }
        } else {
            # No posterior rows found, use all rows
            quantiles <- quantiles_all
            if (verbose) {
                message("  No 'posterior' type found, using all rows")
            }
        }
    } else {
        # No type column, use all rows
        quantiles <- quantiles_all
    }

    if (verbose) {
        message("  Total rows to process: ", nrow(quantiles))
    }

    if (nrow(quantiles) == 0) {
        stop("No rows found in quantiles file")
    }

    # Check for duplicated parameters
    if ("parameter" %in% names(quantiles)) {
        duplicated_params <- quantiles$parameter[duplicated(quantiles$parameter)]
        if (length(duplicated_params) > 0) {
            warning("Found duplicated parameters in quantiles file: ",
                   paste(unique(duplicated_params), collapse = ", "),
                   "\nLater occurrences will overwrite earlier ones.")
            if (verbose) {
                dup_summary <- table(quantiles$parameter[quantiles$parameter %in% duplicated_params])
                message("  Duplicate parameter counts:")
                for (param in names(dup_summary)) {
                    message("    ", param, ": ", dup_summary[param], " occurrences")
                }
            }
        }
    }

    # Load priors as template
    if (verbose) message("Loading priors template from: ", priors_file)
    priors <- jsonlite::read_json(priors_file)

    # Create posteriors object as copy of priors
    posteriors <- priors

    # Update metadata
    posteriors$metadata$description <- "Posterior distributions fitted from calibration quantiles"
    posteriors$metadata$date <- Sys.Date()
    posteriors$metadata$source_priors <- priors_file
    posteriors$metadata$source_quantiles <- quantiles_file

    # Track fitting results
    n_updated <- 0
    n_failed <- 0
    failed_params <- character()

    # Track which parameters actually appear in the quantiles file so that
    # parameters absent from the file (and thus unchanged copies of the prior)
    # can be removed from posteriors after the loop.
    quantiles_global_params   <- character()
    quantiles_location_params <- character()  # "param_base:location"

    if (verbose) {
        message("\nProcessing ", nrow(quantiles), " posterior parameters...")
        message("------------------------------------------------")
    }

    # Process each parameter in the quantiles table
    for (i in seq_len(nrow(quantiles))) {
        param_row <- quantiles[i, ]
        param_name <- param_row$parameter

        # Get distribution type for posterior fitting.
        # Use posterior_distribution when present (allows decoupling the posterior
        # family from the prior family for parameters with uniform priors).
        # Falls back to prior_distribution for backward compatibility with CSV
        # files written before posterior_distribution was added.
        dist_type <- if ("posterior_distribution" %in% names(param_row) &&
                         !is.na(param_row$posterior_distribution) &&
                         nchar(trimws(param_row$posterior_distribution)) > 0) {
            param_row$posterior_distribution
        } else {
            param_row$prior_distribution
        }

        param_scale <- if ("param_type" %in% names(param_row)) param_row$param_type else "unknown"

        # Determine if location-specific
        location <- param_row$location
        if (is.na(location) || location == "") {
            location <- NULL
        }

        # Get the base parameter name (without location suffix)
        if (!is.null(location) && nchar(location) > 0) {
            param_base <- sub(paste0("_", location, "$"), "", param_name)
        } else {
            param_base <- param_name
        }

        # Seasonality Fourier coefficients are stored as a_1_j / a_2_j / b_1_j / b_2_j
        # in simulation results (matching the LASER config key names) but priors.json
        # uses the compact forms a1 / a2 / b1 / b2.  Convert here so the priors lookup
        # below finds the correct key.
        if (param_base %in% c("a_1_j", "a_2_j", "b_1_j", "b_2_j")) {
            param_base <- gsub("_j$", "", param_base)   # a_1_j -> a_1
            param_base <- gsub("_", "", param_base)      # a_1   -> a1
        }

        # Get quantiles
        q_low <- param_row$q0.025
        q_med <- param_row$q0.5
        q_high <- param_row$q0.975

        # Get mode if available, otherwise use median as proxy
        # Note: With small sample sizes (n<30), median is more stable than KDE mode
        if ("mode" %in% names(param_row) && !is.na(param_row$mode)) {
            mode_val <- param_row$mode
            # Check if mode is reasonable (within CI); always warn — this is a data quality signal
            if (mode_val < q_low || mode_val > q_high) {
                message("  Warning: Mode outside CI for ", param_name,
                        " (mode=", round(mode_val, 4), ", CI=[", round(q_low, 4), ",", round(q_high, 4), "]),",
                        " using median instead")
                mode_val <- q_med
            }
        } else {
            mode_val <- q_med
            if (verbose && i == 1) {
                message("  Note: Using median as proxy for mode (more stable with small samples)")
            }
        }

        # Track which parameters appear in the quantiles file
        if (param_scale == "global") {
            quantiles_global_params <- unique(c(quantiles_global_params, param_base))
        } else if (param_scale == "location" && !is.null(location)) {
            quantiles_location_params <- unique(c(quantiles_location_params,
                                                  paste0(param_base, ":", location)))
        }

        # Skip if quantiles are missing
        if (is.na(q_low) || is.na(q_med) || is.na(q_high)) {
            if (verbose) {
                message("  Skipping ", param_name, ": missing quantiles")
            }
            n_failed <- n_failed + 1
            failed_params <- c(failed_params, param_name)
            next
        }

        # Detect near-zero-variance parameters (fixed / not sampled).
        # When a parameter is held constant across all simulations, all three
        # quantiles are identical. The downstream bounds correction would
        # artificially expand the CI and produce a misleading "posterior".
        # Instead, remove the parameter from the posteriors object so the
        # distribution plot only shows the prior curve.
        relative_range <- if (abs(q_med) > 1e-10) {
            abs(q_high - q_low) / abs(q_med)
        } else {
            abs(q_high - q_low)
        }
        if (relative_range < 1e-4) {
            if (verbose) {
                message(sprintf("  [FIXED] %s - near-zero variance (q_low=%.6g, q_high=%.6g); storing as point value",
                                param_name, q_low, q_high))
            }
            # Store as a "fixed" marker so the distribution plotter can draw
            # a vertical line at the point value instead of a density curve.
            fixed_marker <- list(
                distribution = "fixed",
                parameters   = list(value = q_med)
            )
            if (param_scale == "global") {
                posteriors$parameters_global[[param_base]] <- fixed_marker
            } else if (param_scale == "location" && !is.null(location)) {
                posteriors$parameters_location[[param_base]]$location[[location]] <- fixed_marker
            }
            n_updated <- n_updated + 1
            next
        }

        # Apply bounds correction based on distribution type BEFORE fitting
        # This ensures values are in the valid range for each distribution
        if (dist_type == "beta") {
            # Beta requires [0, 1]
            mode_val <- max(0.001, min(0.999, mode_val))
            q_low <- max(0.001, min(0.999, q_low))
            q_high <- max(0.001, min(0.999, q_high))

            # Ensure ordering
            if (q_low >= q_high) {
                q_low <- mode_val - 0.1
                q_high <- mode_val + 0.1
                q_low <- max(0.001, min(0.999, q_low))
                q_high <- max(0.001, min(0.999, q_high))
            }
        } else if (dist_type %in% c("gamma", "lognormal")) {
            # Gamma and lognormal require positive values
            mode_val <- max(1e-6, mode_val)
            q_low <- max(1e-6, q_low)
            q_high <- max(1e-6, q_high)

            # Ensure ordering
            if (q_low >= q_high) {
                q_low <- mode_val * 0.5
                q_high <- mode_val * 1.5
                q_low <- max(1e-6, q_low)
                q_high <- max(q_low + 1e-6, q_high)
            }
        }

        # Fit distribution based on type
        fitted_dist <- NULL
        tryCatch({
            if (dist_type == "beta") {
                fitted_dist <- fit_beta_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    verbose = FALSE
                )
            } else if (dist_type == "gamma") {
                fitted_dist <- fit_gamma_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    verbose = FALSE
                )
            } else if (dist_type == "lognormal") {
                fitted_dist <- fit_lognormal_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    verbose = FALSE
                )
            } else if (dist_type == "normal") {
                fitted_dist <- fit_normal_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    verbose = FALSE
                )
            } else if (dist_type == "uniform") {
                fitted_dist <- fit_uniform_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    verbose = FALSE
                )
            } else if (dist_type == "truncnorm") {
                # Read hard bounds from the prior template first.
                # This preserves biological constraints in the posterior fit —
                # e.g., psi_star_a >= 0 and psi_star_k <= 0.
                # posterior_lower/upper columns override when present
                # (e.g., delta_reporting_* integer support).
                parse_bound <- function(x) {
                    if (is.null(x)) return(NULL)
                    if (is.character(x) && trimws(x) == "Inf")  return(Inf)
                    if (is.character(x) && trimws(x) == "-Inf") return(-Inf)
                    v <- suppressWarnings(as.numeric(x))
                    if (is.na(v)) NULL else v
                }
                prior_entry <- if (!is.null(location)) {
                    priors$parameters_location[[param_base]]$location[[location]]
                } else {
                    priors$parameters_global[[param_base]]
                }
                prior_a <- parse_bound(prior_entry$parameters$a)
                prior_b <- parse_bound(prior_entry$parameters$b)

                a_bound <- if ("posterior_lower" %in% names(param_row) &&
                               !is.na(param_row$posterior_lower))
                               as.numeric(param_row$posterior_lower) else prior_a
                b_bound <- if ("posterior_upper" %in% names(param_row) &&
                               !is.na(param_row$posterior_upper))
                               as.numeric(param_row$posterior_upper) else prior_b

                fitted_dist <- fit_truncnorm_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    a = a_bound,
                    b = b_bound,
                    verbose = FALSE
                )
            } else if (dist_type == "gompertz") {
                fitted_dist <- fit_gompertz_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    verbose = FALSE
                )
            } else if (dist_type == "derived") {
                # Derived parameters (e.g., beta_j0_hum, beta_j0_env) are rate parameters
                # Fit gamma distribution to empirical posterior quantiles
                fitted_dist <- fit_gamma_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    verbose = FALSE
                )
                if (verbose) {
                    message("  Fitted gamma distribution to derived parameter ", param_name)
                }
            } else {
                if (verbose) {
                    message("  Warning: Unknown distribution type '", dist_type, "' for ", param_name)
                }
            }
        }, error = function(e) {
            if (verbose) {
                message("  Error fitting ", dist_type, " for ", param_name, ": ", e$message)
            }
            fitted_dist <<- NULL
        })

        # Handle fitting failure by marking parameter with diagnostic information
        if (is.null(fitted_dist)) {

            # Create failed marker for posteriors.json
            failed_marker <- list(
                distribution = "failed",
                parameters = list(),
                metadata = list(
                    error = "Distribution fitting failed",
                    attempted_distribution = dist_type,
                    quantiles_used = list(
                        q_low = q_low,
                        q_med = q_med,
                        q_high = q_high
                    ),
                    timestamp = as.character(Sys.time())
                )
            )

            # Insert failed marker into posteriors structure
            success_marker <- FALSE
            if (param_scale == "global") {
                if (!is.null(posteriors$parameters_global[[param_base]])) {
                    posteriors$parameters_global[[param_base]] <- failed_marker
                    success_marker <- TRUE
                }
            } else if (param_scale == "location" && !is.null(location)) {
                if (!is.null(posteriors$parameters_location[[param_base]])) {
                    if (!is.null(posteriors$parameters_location[[param_base]]$location[[location]])) {
                        posteriors$parameters_location[[param_base]]$location[[location]] <- failed_marker
                        success_marker <- TRUE
                    }
                }
            }

            n_failed <- n_failed + 1
            failed_params <- c(failed_params, param_name)

            if (verbose) {
                if (success_marker) {
                    message(sprintf("  [FAILED] %s - marked in posteriors.json", param_name))
                } else {
                    message(sprintf("  [FAILED] %s - could not find in structure", param_name))
                }
            }
            next
        }

        # Update posteriors structure based on parameter type and location
        success <- FALSE

        # Create properly structured distribution object
        # The plot function expects {distribution: "type", parameters: {...}}
        structured_dist <- list(
            distribution = dist_type,
            parameters = fitted_dist
        )

        if (param_scale == "global") {
            # Update global parameter
            if (!is.null(posteriors$parameters_global[[param_base]])) {
                posteriors$parameters_global[[param_base]] <- structured_dist
                success <- TRUE
            }
        } else if (param_scale == "location" && !is.null(location)) {
            # Update location-specific parameter
            if (!is.null(posteriors$parameters_location[[param_base]])) {
                # Parameter structure exists - add or update location
                if (!is.null(posteriors$parameters_location[[param_base]]$location[[location]])) {
                    # Location exists - update it
                    posteriors$parameters_location[[param_base]]$location[[location]] <- structured_dist
                    success <- TRUE
                } else {
                    # Location doesn't exist - add it (handles derived params with multiple locations)
                    posteriors$parameters_location[[param_base]]$location[[location]] <- structured_dist
                    success <- TRUE
                }
            } else {
                # Parameter doesn't exist in priors template (e.g., derived parameters)
                # Create structure dynamically and add to posteriors
                if (verbose) {
                    message(sprintf("  Adding derived parameter '%s' to posteriors structure", param_base))
                }

                # Initialize parameter structure with location slot
                posteriors$parameters_location[[param_base]] <- list(
                    location = list()
                )

                # Add the fitted distribution for this location
                posteriors$parameters_location[[param_base]]$location[[location]] <- structured_dist
                success <- TRUE
            }
        }

        if (success) {
            n_updated <- n_updated + 1
            if (verbose && (n_updated %% 10 == 0 || n_updated == 1)) {
                message("  Updated ", n_updated, " parameters...")
            }
        } else {
            n_failed <- n_failed + 1
            failed_params <- c(failed_params, param_name)
            if (verbose) {
                message("  Warning: Could not find ", param_name, " in posteriors structure")
            }
        }
    }

    # -------------------------------------------------------------------------
    # Remove parameters that were never in the quantiles file.
    # posteriors was initialised as a full copy of priors, so any parameter
    # absent from the quantiles CSV would otherwise appear in the output JSON
    # with its prior distribution unchanged, causing the distribution plot to
    # draw both a "prior" curve and an identical "posterior" curve.
    # -------------------------------------------------------------------------

    # Global parameters not in quantiles file
    for (param in setdiff(names(posteriors$parameters_global), quantiles_global_params)) {
        posteriors$parameters_global[[param]] <- NULL
    }

    # Location-specific parameters not in quantiles file
    for (pb in names(posteriors$parameters_location)) {
        locs_in_quantiles <- sub(paste0("^", pb, ":"), "",
                                 grep(paste0("^", pb, ":"), quantiles_location_params, value = TRUE))
        locs_to_remove <- setdiff(names(posteriors$parameters_location[[pb]]$location),
                                  locs_in_quantiles)
        for (loc in locs_to_remove) {
            posteriors$parameters_location[[pb]]$location[[loc]] <- NULL
        }
        if (length(posteriors$parameters_location[[pb]]$location) == 0) {
            posteriors$parameters_location[[pb]] <- NULL
        }
    }

    n_pruned <- length(setdiff(names(priors$parameters_global), names(posteriors$parameters_global)))
    if (verbose && n_pruned > 0) {
        message("  Removed ", n_pruned, " global parameters absent from quantiles file (not sampled)")
    }

    # Write posteriors to JSON
    output_file <- file.path(output_dir, "posteriors.json")
    if (verbose) {
        message("\n------------------------------------------------")
        message("Writing posteriors to: ", output_file)
    }

    # Post-process JSON with auto_unbox = TRUE to remove single-element arrays
    json_content <- jsonlite::toJSON(posteriors, auto_unbox = TRUE, pretty = TRUE, digits = 10)

    # Write JSON directly (already prettified)
    writeLines(json_content, output_file)

    # Summary
    if (verbose) {
        message("\n=== Summary ===")
        message("Parameters updated: ", n_updated)
        message("Parameters failed: ", n_failed)
        if (n_failed > 0) {
            message("Failed parameters: ", paste(failed_params, collapse = ", "))
        }
        message("Output file: ", output_file)
        message("\n✓ Posterior distributions calculation complete")
    }

    # Return results
    invisible(list(
        posteriors = posteriors,
        n_parameters_updated = n_updated,
        n_parameters_failed = n_failed,
        failed_parameters = failed_params,
        output_file = output_file
    ))
}