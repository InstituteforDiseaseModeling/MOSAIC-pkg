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

    # Process all quantile rows - let the user decide what to include in the file
    quantiles <- quantiles_all

    if (verbose) {
        message("  Total rows to process: ", nrow(quantiles))
        if ("type" %in% names(quantiles)) {
            types_found <- unique(quantiles$type)
            message("  Types found: ", paste(types_found, collapse = ", "))
        }
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

    if (verbose) {
        message("\nProcessing ", nrow(quantiles), " posterior parameters...")
        message("------------------------------------------------")
    }

    # Process each parameter in the quantiles table
    for (i in seq_len(nrow(quantiles))) {
        param_row <- quantiles[i, ]
        param_name <- param_row$parameter

        # Get distribution type
        dist_type <- param_row$prior_distribution

        # Handle both column names (for backward compatibility)
        if ("scale" %in% names(param_row)) {
            param_scale <- param_row$scale
        } else if ("param_type" %in% names(param_row)) {
            param_scale <- param_row$param_type
        } else {
            param_scale <- "unknown"
        }

        # Determine if location-specific
        location <- param_row$location
        if (is.na(location) || location == "" || location == "NA") {
            location <- NULL
        }

        # Get the base parameter name (without location suffix)
        if (!is.null(location) && nchar(location) > 0) {
            param_base <- sub(paste0("_", location, "$"), "", param_name)
        } else {
            param_base <- param_name
        }

        # Map seasonality parameter names (handle both a_1_j and a1 formats)
        if (param_base %in% c("a_1_j", "a_2_j", "b_1_j", "b_2_j")) {
            param_base_original <- param_base
            param_base <- gsub("_j$", "", param_base)  # Remove _j suffix
            param_base <- gsub("_", "", param_base)    # Remove underscores: a_1 -> a1
            if (verbose && i == 1) {
                message("  Mapping seasonality parameter: ", param_base_original, " -> ", param_base)
            }
        }

        # Get quantiles
        q_low <- param_row$q0.025
        q_med <- param_row$q0.5
        q_high <- param_row$q0.975

        # Get mode if available, otherwise use median as proxy
        # Note: With small sample sizes (n<30), median is more stable than KDE mode
        if ("mode" %in% names(param_row) && !is.na(param_row$mode)) {
            mode_val <- param_row$mode
            # Check if mode is reasonable (within CI)
            if (mode_val < q_low || mode_val > q_high) {
                if (verbose) {
                    message("  Warning: Mode outside CI for ", param_name, ", using median instead")
                }
                mode_val <- q_med
            }
        } else {
            mode_val <- q_med
            if (verbose && i == 1) {
                message("  Note: Using median as proxy for mode (more stable with small samples)")
            }
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
                fitted_dist <- fit_truncnorm_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    verbose = FALSE
                )
            } else if (dist_type == "gompertz") {
                fitted_dist <- fit_gompertz_from_ci(
                    mode_val = mode_val,
                    ci_lower = q_low,
                    ci_upper = q_high,
                    verbose = FALSE
                )
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

        # Skip if fitting failed
        if (is.null(fitted_dist)) {
            n_failed <- n_failed + 1
            failed_params <- c(failed_params, param_name)
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
                if (!is.null(posteriors$parameters_location[[param_base]]$location[[location]])) {
                    posteriors$parameters_location[[param_base]]$location[[location]] <- structured_dist
                    success <- TRUE
                }
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