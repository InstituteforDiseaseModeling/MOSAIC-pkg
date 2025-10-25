#' Create NPE Configuration with Median Parameter Values
#'
#' Creates a configuration file where all NPE-estimated parameters are set to their
#' median posterior values from posterior_quantiles.csv. Non-estimated parameters
#' retain their original values from the base configuration.
#'
#' @param posterior_quantiles_file Path to NPE posterior_quantiles.csv file containing
#'   parameter quantiles from NPE estimation
#' @param config_base Base configuration object (list) or file path (character string)
#'   containing the original LASER model configuration
#' @param output_file Path where config_npe.json should be saved
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @return List containing the updated configuration with median posterior values
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Reads NPE posterior quantiles from CSV file
#'   \item Loads the base configuration
#'   \item Updates each NPE-estimated parameter with its median (q0.5) value
#'   \item Handles both global and location-specific parameters
#'   \item Preserves all non-estimated parameters from base configuration
#'   \item Adds metadata tracking the update process
#'   \item Saves the result as config_npe.json
#' }
#'
#' Location-specific parameters are identified by a 3-letter ISO code suffix
#' (e.g., "beta_j0_hum_ETH"). The function automatically maps these to the
#' correct location index in the configuration vectors.
#'
#' @examples
#' \dontrun{
#' # Create NPE configuration from posterior quantiles
#' config_npe <- create_config_npe(
#'   posterior_quantiles_file = "output/npe/posterior/posterior_quantiles.csv",
#'   config_base = config_base,
#'   output_file = "output/npe/config_npe.json"
#' )
#'
#' # Or using a config file path
#' config_npe <- create_config_npe(
#'   posterior_quantiles_file = "output/npe/posterior/posterior_quantiles.csv",
#'   config_base = "output/config_base.json",
#'   output_file = "output/npe/config_npe.json"
#' )
#' }
#'
#' @seealso
#' \code{\link{train_npe}} for training the NPE model
#' \code{\link{calc_npe_diagnostics}} for NPE model diagnostics
#'
#' @export
create_config_npe <- function(
    posterior_quantiles_file,
    config_base,
    output_file,
    verbose = TRUE
) {

    # Input validation
    if (!file.exists(posterior_quantiles_file)) {
        stop("NPE posterior quantiles file not found: ", posterior_quantiles_file)
    }

    # Handle config_base as either object or file path
    if (is.character(config_base)) {
        if (!file.exists(config_base)) {
            stop("Config base file not found: ", config_base)
        }
        if (verbose) {
            cat("  Loading base config from file:", config_base, "\n")
        }
        config_base <- read_model_json(config_base)
    }

    # Create output directory if needed
    output_dir <- dirname(output_file)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    if (verbose) {
        cat("\n=== Creating NPE Configuration ===\n")
        cat("  NPE quantiles file:", posterior_quantiles_file, "\n")
        cat("  Output file:", output_file, "\n")
    }

    # Load NPE posterior quantiles
    quantiles_data <- read.csv(posterior_quantiles_file, stringsAsFactors = FALSE)

    if (verbose) {
        cat("  Parameters found:", nrow(quantiles_data), "\n")
    }

    # Check required columns
    required_cols <- c("parameter", "q0.5")
    missing_cols <- setdiff(required_cols, names(quantiles_data))
    if (length(missing_cols) > 0) {
        stop("Missing required columns in quantiles file: ",
             paste(missing_cols, collapse = ", "))
    }

    # Start with base configuration
    config_npe <- config_base

    # Track updates
    n_updated <- 0
    n_failed <- 0
    failed_params <- character()
    updated_params <- character()

    if (verbose) {
        cat("\n  Updating configuration parameters...\n")
    }

    # Update each parameter with its median value
    for (i in 1:nrow(quantiles_data)) {
        param_name <- quantiles_data$parameter[i]
        median_val <- quantiles_data$q0.5[i]

        # Skip if median is NA
        if (is.na(median_val)) {
            if (verbose) cat("    Skipping", param_name, ": median value is NA\n")
            n_failed <- n_failed + 1
            failed_params <- c(failed_params, param_name)
            next
        }

        # Handle location-specific parameters (e.g., "beta_j0_hum_ETH")
        if (grepl("_[A-Z]{3}$", param_name)) {
            # Extract location code and base parameter name
            location <- regmatches(param_name, regexpr("[A-Z]{3}$", param_name))
            base_name <- sub("_[A-Z]{3}$", "", param_name)

            # Look for vector parameter in config
            if (base_name %in% names(config_npe)) {
                locations <- config_npe$location_name
                if (location %in% locations) {
                    loc_idx <- which(locations == location)

                    # Update the specific location index
                    if (is.list(config_npe[[base_name]])) {
                        config_npe[[base_name]][[loc_idx]] <- median_val
                    } else {
                        config_npe[[base_name]][loc_idx] <- median_val
                    }

                    if (verbose) {
                        cat(sprintf("    Updated %s[%s] = %.4f\n",
                                  base_name, location, median_val))
                    }
                    n_updated <- n_updated + 1
                    updated_params <- c(updated_params, param_name)
                } else {
                    if (verbose) {
                        cat("    Failed:", param_name, "- location", location,
                            "not found in config\n")
                    }
                    n_failed <- n_failed + 1
                    failed_params <- c(failed_params, param_name)
                }
            } else {
                if (verbose) {
                    cat("    Failed:", param_name, "- parameter", base_name,
                        "not found in config\n")
                }
                n_failed <- n_failed + 1
                failed_params <- c(failed_params, param_name)
            }

        } else {
            # Handle global/scalar parameters
            if (param_name %in% names(config_npe)) {
                config_npe[[param_name]] <- median_val
                if (verbose) {
                    cat(sprintf("    Updated %s = %.4f\n", param_name, median_val))
                }
                n_updated <- n_updated + 1
                updated_params <- c(updated_params, param_name)
            } else {
                if (verbose) {
                    cat("    Failed:", param_name, "- parameter not found in config\n")
                }
                n_failed <- n_failed + 1
                failed_params <- c(failed_params, param_name)
            }
        }
    }

    # Add metadata about the update
    if (is.null(config_npe$`__metadata__`)) {
        config_npe$`__metadata__` <- list()
    }

    config_npe$`__metadata__`$npe_median_update <- list(
        source_file = posterior_quantiles_file,
        update_date = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        n_parameters_updated = n_updated,
        n_parameters_failed = n_failed,
        updated_parameters = updated_params,
        failed_parameters = if (length(failed_params) > 0) failed_params else NULL,
        description = "Configuration updated with NPE median posterior values"
    )

    # Save to JSON file
    tryCatch({
        write_model_json(config_npe, output_file, type = "priors", validate = FALSE)
        if (verbose) {
            cat("\n  Configuration saved to:", output_file, "\n")
        }
    }, error = function(e) {
        stop("Failed to write configuration file: ", e$message)
    })

    # Summary
    if (verbose) {
        cat("\n=== NPE Configuration Summary ===\n")
        cat("  Parameters updated:", n_updated, "\n")
        cat("  Parameters failed:", n_failed, "\n")
        if (n_failed > 0) {
            cat("  Failed parameters:", paste(head(failed_params, 5), collapse = ", "))
            if (n_failed > 5) cat(", ...")
            cat("\n")
        }
        cat("  Output file:", output_file, "\n")
        cat("✓ NPE configuration created successfully\n\n")
    }

    return(invisible(config_npe))
}