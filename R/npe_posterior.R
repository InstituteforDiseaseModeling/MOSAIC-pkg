# ==============================================================================
# NPE Posterior Estimation Functions
# ==============================================================================
# Functions for posterior sampling, density evaluation, and distribution fitting
# Critical: Includes log_prob extraction for SMC support
# ==============================================================================

#' Estimate NPE Posterior
#'
#' @description
#' Generates posterior samples using a trained NPE model and CRITICALLY
#' extracts log q(theta|x) values needed for SMC importance weights.
#'
#' @param model Trained NPE model object
#' @param observed_data Data frame with observed outbreak data
#' @param n_samples Number of posterior samples to generate
#' @param return_log_probs If TRUE, return log q(theta|x) for SMC (default TRUE)
#' @param quantiles Quantile levels to compute
#' @param output_dir Directory to save results (NULL = don't save)
#' @param verbose Print progress messages
#' @param rejection_sampling Logical. If TRUE, perform rejection sampling to ensure
#'   all returned samples are within parameter bounds (default FALSE).
#' @param max_rejection_rate Numeric. Maximum acceptable rejection rate (0-1).
#'   If exceeded after 3 attempts, sampling stops early (default 0.20).
#' @param max_attempts Integer. Maximum number of sampling attempts before giving up
#'   (default 10).
#'
#' @return List containing:
#' \itemize{
#'   \item samples - Matrix of posterior samples (n_samples x n_params)
#'   \item log_probs - Vector of log q(theta|x) values for SMC
#'   \item quantiles - Data frame of parameter quantiles
#'   \item param_names - Names of parameters
#'   \item n_samples - Actual number of samples returned
#'   \item rejection_info - List with rejection sampling diagnostics (NULL if disabled):
#'     \itemize{
#'       \item enabled - TRUE
#'       \item requested - Target number of samples
#'       \item achieved - Actual samples obtained
#'       \item total_drawn - Total samples generated (including rejected)
#'       \item rejection_rate - Proportion of samples rejected
#'       \item n_attempts - Number of sampling iterations
#'     }
#' }
#' @export
estimate_npe_posterior <- function(
    model,
    observed_data,
    n_samples = 10000,
    return_log_probs = TRUE,
    quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
    output_dir = NULL,
    verbose = TRUE,
    rejection_sampling = FALSE,
    max_rejection_rate = 0.20,
    max_attempts = 10
) {

    if (verbose) message("Estimating NPE posterior...")

    # CRITICAL: Set BLAS threads to 1 BEFORE importing PyTorch
    # This prevents OpenMP/MKL threading conflicts on clusters
    if (exists(".mosaic_set_blas_threads", envir = asNamespace("MOSAIC"), inherits = FALSE)) {
        MOSAIC:::.mosaic_set_blas_threads(1L)
    }

    # Also set environment variables as backup
    Sys.setenv(
        OMP_NUM_THREADS = "1",
        MKL_NUM_THREADS = "1",
        OPENBLAS_NUM_THREADS = "1",
        NUMEXPR_NUM_THREADS = "1"
    )

    # Extract model components
    npe_model <- model$model
    normalization <- model$normalization
    device <- model$device

    # Import Python modules
    torch <- reticulate::import("torch")
    np <- reticulate::import("numpy")

    # Prepare observed data
    obs_vector <- .prepare_observed_data(observed_data)
    obs_tensor <- torch$tensor(
        as.numeric(obs_vector),
        dtype = torch$float32
    )$unsqueeze(0L)$to(device)

    # Normalize observations
    obs_norm <- (obs_tensor - normalization$y_mean) / normalization$y_std

    # Generate posterior samples
    if (verbose) {
        if (rejection_sampling) {
            message("  Generating ", n_samples, " samples with rejection sampling...")
        } else {
            message("  Generating ", n_samples, " samples...")
        }
    }

    # Initialize variables for rejection sampling
    rejection_info <- NULL

    # BRANCH: Standard sampling vs Rejection sampling
    if (!rejection_sampling) {
        # ═══════════════════════════════════════════════════════════════
        # STANDARD SAMPLING (Original behavior)
        # ═══════════════════════════════════════════════════════════════

        samples_result <- .draw_posterior_samples(
            npe_model = npe_model,
            obs_norm = obs_norm,
            n = n_samples,
            normalization = normalization,
            device = device,
            return_log_probs = return_log_probs,
            torch = torch
        )

        # Convert to R
        samples_numpy <- samples_result$samples$cpu()$numpy()
        samples <- .reshape_samples(samples_numpy, n_samples, model)

        # Extract log probs
        log_probs <- NULL
        if (return_log_probs) {
            log_probs <- as.numeric(samples_result$log_probs$cpu()$numpy())
        }

    } else {
        # ═══════════════════════════════════════════════════════════════
        # REJECTION SAMPLING (New behavior)
        # ═══════════════════════════════════════════════════════════════

        # Extract bounds from model
        bounds_info <- .extract_bounds_from_model(model)
        bounds_lower <- bounds_info$lower
        bounds_upper <- bounds_info$upper

        if (verbose) {
            message("  Bounds extracted for ", length(bounds_lower), " parameters")
            if (!is.null(bounds_info$source)) {
                message("  Bounds source: ", bounds_info$source)
            }
        }

        # Initialize collectors
        all_samples <- matrix(nrow = 0, ncol = length(bounds_lower))
        all_log_probs <- numeric(0)
        attempts <- 0
        total_drawn <- 0
        n_target <- n_samples

        # Rejection sampling loop
        while (nrow(all_samples) < n_target && attempts < max_attempts) {
            attempts <- attempts + 1
            n_remaining <- n_target - nrow(all_samples)

            # Oversample by 20% to reduce number of iterations
            n_to_draw <- ceiling(n_remaining * 1.2)
            total_drawn <- total_drawn + n_to_draw

            if (verbose && attempts > 1) {
                message("  Attempt ", attempts, ": drawing ", n_to_draw,
                       " samples (", nrow(all_samples), "/", n_target, " collected)")
            }

            # Draw batch
            batch_result <- .draw_posterior_samples(
                npe_model = npe_model,
                obs_norm = obs_norm,
                n = n_to_draw,
                normalization = normalization,
                device = device,
                return_log_probs = return_log_probs,
                torch = torch
            )

            # Convert to R
            batch_numpy <- batch_result$samples$cpu()$numpy()
            batch_samples <- .reshape_samples(batch_numpy, n_to_draw, model)

            batch_log_probs <- NULL
            if (return_log_probs) {
                batch_log_probs <- as.numeric(batch_result$log_probs$cpu()$numpy())
            }

            # Check bounds
            in_bounds <- .check_bounds_vectorized(
                batch_samples,
                bounds_lower,
                bounds_upper
            )

            n_valid <- sum(in_bounds)

            if (verbose && n_valid < nrow(batch_samples)) {
                rejection_pct <- 100 * (1 - n_valid / nrow(batch_samples))
                message("    Rejected ", nrow(batch_samples) - n_valid,
                       " samples (", sprintf("%.1f%%", rejection_pct), ")")
            }

            # Keep valid samples
            if (n_valid > 0) {
                valid_samples <- batch_samples[in_bounds, , drop = FALSE]
                valid_log_probs <- if (return_log_probs) batch_log_probs[in_bounds] else NULL

                # Take only what we need
                n_to_keep <- min(nrow(valid_samples), n_remaining)
                all_samples <- rbind(all_samples, valid_samples[seq_len(n_to_keep), , drop = FALSE])

                if (return_log_probs && !is.null(valid_log_probs)) {
                    all_log_probs <- c(all_log_probs, valid_log_probs[seq_len(n_to_keep)])
                }
            }

            # Check rejection rate for safety
            current_rejection_rate <- (total_drawn - nrow(all_samples)) / total_drawn
            if (current_rejection_rate > max_rejection_rate && attempts >= 3) {
                warning(
                    "Rejection rate (", sprintf("%.1f%%", current_rejection_rate * 100),
                    ") exceeds threshold (", sprintf("%.1f%%", max_rejection_rate * 100),
                    ") after ", attempts, " attempts.\n",
                    "Consider retraining model with wider bounds or different architecture.\n",
                    "Returning ", nrow(all_samples), " samples instead of ", n_target, "."
                )
                break
            }
        }

        # Check if we achieved target
        if (nrow(all_samples) < n_target) {
            warning(
                "Could not generate ", n_target, " valid samples after ", attempts,
                " attempts. Achieved ", nrow(all_samples), " samples.\n",
                "Final rejection rate: ",
                sprintf("%.1f%%", 100 * (total_drawn - nrow(all_samples)) / total_drawn)
            )
        }

        samples <- all_samples
        log_probs <- if (return_log_probs && length(all_log_probs) > 0) all_log_probs else NULL

        # Store rejection diagnostics
        rejection_info <- list(
            enabled = TRUE,
            requested = n_target,
            achieved = nrow(samples),
            total_drawn = total_drawn,
            rejection_rate = (total_drawn - nrow(samples)) / total_drawn,
            n_attempts = attempts
        )

        if (verbose) {
            message("  Rejection sampling complete:")
            message("    Requested: ", rejection_info$requested)
            message("    Achieved: ", rejection_info$achieved)
            message("    Total drawn: ", rejection_info$total_drawn)
            message("    Rejection rate: ", sprintf("%.1f%%", rejection_info$rejection_rate * 100))
            message("    Attempts: ", rejection_info$n_attempts)
        }
    }

    # Set parameter names if we have the right number of columns
    if (!is.null(model$architecture$param_names) &&
        ncol(samples) == length(model$architecture$param_names)) {
        colnames(samples) <- model$architecture$param_names
    }

    # Calculate quantiles in BFRS-compatible long format
    if (verbose) message("  Calculating quantiles...")
    quantiles_df <- .calculate_quantiles(samples, quantiles)

    # Save results if requested
    if (!is.null(output_dir)) {
        .save_posterior_results(
            samples = samples,
            log_probs = log_probs,
            quantiles = quantiles_df,
            output_dir = output_dir,
            verbose = verbose
        )

        # Save rejection sampling info if applicable
        if (!is.null(rejection_info)) {
            rejection_file <- file.path(output_dir, "rejection_sampling_info.json")
            jsonlite::write_json(
                rejection_info,
                rejection_file,
                pretty = TRUE,
                auto_unbox = TRUE
            )
            if (verbose) {
                message("  Rejection info saved: ", basename(rejection_file))
            }
        }
    }

    result <- list(
        samples = samples,
        log_probs = log_probs,  # CRITICAL FOR SMC
        quantiles = quantiles_df,  # Long format (BFRS-compatible)
        param_names = colnames(samples),
        n_samples = nrow(samples),  # CHANGED: Use actual nrow instead of n_samples arg
        rejection_info = rejection_info  # NEW: NULL if rejection_sampling=FALSE
    )

    return(result)
}

# ==============================================================================
# Rejection Sampling Helper Functions
# ==============================================================================

#' Extract Parameter Bounds from Model
#' @keywords internal
.extract_bounds_from_model <- function(model) {

    # Get parameter names from model
    param_names <- model$architecture$param_names
    if (is.null(param_names)) {
        stop("Model architecture does not contain param_names.")
    }

    # =========================================================================
    # TWO-TIER APPROACH: Try priors.json first, then fall back to cached bounds
    # =========================================================================

    bounds <- NULL
    bounds_source <- NULL

    # -------------------------------------------------------------------------
    # TIER 1: Try to load bounds from priors.json (source of truth)
    # -------------------------------------------------------------------------

    # Search for priors.json in likely locations
    priors_file <- NULL

    if (!is.null(model$output_dir) && dir.exists(model$output_dir)) {
        # Try multiple relative paths from model output directory
        search_paths <- c(
            file.path(dirname(model$output_dir), "..", "0_setup", "priors.json"),
            file.path(dirname(model$output_dir), "..", "setup", "priors.json"),
            file.path(dirname(dirname(model$output_dir)), "0_setup", "priors.json"),
            file.path(dirname(dirname(model$output_dir)), "setup", "priors.json"),
            file.path(dirname(model$output_dir), "priors.json"),
            file.path(dirname(dirname(model$output_dir)), "priors.json")
        )

        for (path in search_paths) {
            if (file.exists(path)) {
                priors_file <- normalizePath(path)
                break
            }
        }
    }

    # Try to load bounds from priors.json if found
    if (!is.null(priors_file)) {
        tryCatch({
            bounds_df <- get_npe_parameter_bounds(
                param_names = param_names,
                priors_file = priors_file,
                verbose = FALSE
            )
            bounds <- bounds_df
            bounds_source <- "priors.json"
        }, error = function(e) {
            warning("Failed to load bounds from priors.json: ", e$message,
                   "\nFalling back to cached model bounds.")
        })
    }

    # -------------------------------------------------------------------------
    # TIER 2: Fall back to cached bounds in model object
    # -------------------------------------------------------------------------

    if (is.null(bounds)) {
        if (!is.null(model$bounds)) {
            bounds <- model$bounds
            bounds_source <- "model cache"
        } else {
            stop("Cannot extract parameter bounds for rejection sampling.\n",
                 "  - No priors.json file found in expected locations\n",
                 "  - No cached bounds in model object\n",
                 "Ensure bounds were provided during model training (train_npe).")
        }
    }

    # -------------------------------------------------------------------------
    # Convert bounds to standard format
    # -------------------------------------------------------------------------

    # Handle data frame format (expected from get_npe_parameter_bounds)
    if (is.data.frame(bounds)) {
        n_params <- length(param_names)
        bounds_lower <- numeric(n_params)
        bounds_upper <- numeric(n_params)

        # Match bounds to parameter names
        for (i in seq_along(param_names)) {
            param_match <- which(bounds$parameter == param_names[i])

            if (length(param_match) == 0) {
                warning("No bounds found for parameter: ", param_names[i],
                       ". Using [-Inf, Inf]")
                bounds_lower[i] <- -Inf
                bounds_upper[i] <- Inf
            } else {
                bounds_lower[i] <- bounds$min[param_match[1]]
                bounds_upper[i] <- bounds$max[param_match[1]]
            }
        }

        return(list(
            lower = bounds_lower,
            upper = bounds_upper,
            param_names = param_names,
            source = bounds_source
        ))

    } else if (is.matrix(bounds)) {
        # Handle matrix format (rows = params, cols = [lower, upper])
        if (is.null(param_names)) {
            param_names <- paste0("param_", seq_len(nrow(bounds)))
        }

        return(list(
            lower = bounds[, 1],
            upper = bounds[, 2],
            param_names = param_names,
            source = bounds_source
        ))

    } else {
        stop("Bounds must be a data frame or matrix. Got: ", class(bounds)[1])
    }
}

#' Check if Samples are Within Bounds
#' @keywords internal
.check_bounds_vectorized <- function(samples, bounds_lower, bounds_upper) {

    n_params <- ncol(samples)
    all_in_bounds <- rep(TRUE, nrow(samples))

    # Check each parameter
    for (i in seq_len(n_params)) {
        # Only check finite bounds
        if (!is.infinite(bounds_lower[i])) {
            all_in_bounds <- all_in_bounds & (samples[, i] >= bounds_lower[i])
        }
        if (!is.infinite(bounds_upper[i])) {
            all_in_bounds <- all_in_bounds & (samples[, i] <= bounds_upper[i])
        }
    }

    all_in_bounds
}

#' Draw Posterior Samples (Helper for Rejection Sampling)
#' @keywords internal
.draw_posterior_samples <- function(
    npe_model,
    obs_norm,
    n,
    normalization,
    device,
    return_log_probs,
    torch
) {

    samples_list <- with(torch$no_grad(), {
        # Access model components
        embedding_net <- npe_model[0L]
        flow <- npe_model[1L]

        # Get embedding
        embedding <- embedding_net(obs_norm)

        # Sample from posterior (normalized space)
        posterior_dist <- flow(embedding)
        samples_norm <- posterior_dist$sample(list(as.integer(n)))

        # Denormalize
        samples_tensor <- samples_norm * normalization$X_std + normalization$X_mean

        # Extract log probabilities if requested
        log_probs_tensor <- NULL
        if (return_log_probs) {
            log_probs_tensor <- posterior_dist$log_prob(samples_norm)
            # Adjust for normalization
            log_jacobian <- -torch$sum(torch$log(normalization$X_std))
            log_probs_tensor <- log_probs_tensor - log_jacobian
        }

        list(
            samples = samples_tensor,
            log_probs = log_probs_tensor
        )
    })

    samples_list
}

#' Reshape Samples to Matrix (Helper)
#' @keywords internal
.reshape_samples <- function(samples_numpy, n_samples_batch, model) {

    # Handle different array dimensions
    if (length(dim(samples_numpy)) == 3) {
        # 3D: [n_samples, batch_size, n_params]
        samples <- as.matrix(samples_numpy[, 1, ])

    } else if (length(dim(samples_numpy)) == 2) {
        # 2D: [n_samples, n_params]
        samples <- as.matrix(samples_numpy)

    } else {
        # 1D: need to infer structure
        n_params_expected <- if (!is.null(model$architecture$n_params)) {
            model$architecture$n_params
        } else if (!is.null(model$architecture$param_names)) {
            length(model$architecture$param_names)
        } else {
            # Try to infer from length
            total_length <- length(samples_numpy)
            if (total_length %% n_samples_batch == 0) {
                total_length %/% n_samples_batch
            } else {
                1
            }
        }

        if (length(samples_numpy) %% n_params_expected != 0) {
            stop("Cannot reshape samples: length ", length(samples_numpy),
                 " not divisible by n_params ", n_params_expected)
        }

        samples <- matrix(samples_numpy, ncol = n_params_expected)
    }

    samples
}

# ==============================================================================

#' Create NPE Configuration
#'
#' @description
#' Creates a configuration file using posterior median values from NPE.
#' Automatically handles location-specific parameters by stripping location
#' suffixes (e.g., "tau_i_ETH" -> "tau_i") when matching to config fields.
#' Supports both single-location and multi-location configs.
#'
#' @param posterior_result NPE posterior result object containing:
#'   \itemize{
#'     \item samples - Matrix of posterior samples (with location suffixes)
#'     \item quantiles - Optional data frame with q0.5 column for medians
#'     \item n_samples - Number of posterior samples
#'   }
#' @param config_base Base configuration to update. Location-specific parameters
#'   should be named without location suffixes (e.g., "tau_i" not "tau_i_ETH")
#' @param output_file Path to save config JSON (NULL = don't save)
#' @param use_median Use median (TRUE) or mean (FALSE) for point estimates
#' @param verbose Print progress messages including number of parameters updated
#'
#' @return Updated configuration list with npe_metadata including:
#'   \itemize{
#'     \item n_parameters_updated - Number of parameters successfully updated
#'     \item n_parameters_skipped - Number of parameters not found in config
#'   }
#'
#' @details
#' For single-location configs, location-specific parameters are stored as scalars.
#' For multi-location configs, they are stored as vectors ordered by location_name.
#'
#' @export
create_config_npe <- function(
    posterior_result,
    config_base,
    output_file = NULL,
    use_median = TRUE,
    verbose = TRUE,
    debug = FALSE
) {

    if (verbose) message("Creating NPE configuration...")

    # Enable detailed logging if debug mode
    if (debug) {
        message("\n=== DEBUG MODE: Detailed parameter assignments ===\n")
    }

    # Get samples - handle both field names
    samples <- posterior_result$samples
    if (is.null(samples)) {
        # Try alternate field name from run_npe_workflow
        samples <- posterior_result$posterior_samples
    }
    if (is.null(samples)) {
        stop("No samples or posterior_samples found in posterior_result")
    }

    # Convert to matrix if needed
    if (!is.matrix(samples)) {
        if (is.vector(samples)) {
            # Single parameter case
            samples <- matrix(samples, ncol = 1)
        } else {
            samples <- as.matrix(samples)
        }
    }

    # Check dimensions
    if (verbose) {
        message("  Samples dimensions: ", paste(dim(samples), collapse = "x"))
    }

    if (length(dim(samples)) != 2 || nrow(samples) == 0 || ncol(samples) == 0) {
        stop("Invalid samples dimensions: ", paste(dim(samples), collapse = "x"))
    }

    # Get point estimates
    if (use_median) {
        # Check for long format quantiles
        quantiles_long <- NULL
        if ("quantiles" %in% names(posterior_result)) {
            quantiles_long <- posterior_result$quantiles
        } else if ("posterior_quantiles" %in% names(posterior_result)) {
            quantiles_long <- posterior_result$posterior_quantiles
        }

        if (!is.null(quantiles_long) && "q0.5" %in% names(quantiles_long)) {
            # Extract median from long format (q0.5 column)
            param_values <- quantiles_long$q0.5
            names(param_values) <- as.character(quantiles_long$parameter)
        } else {
            # Compute median from samples if quantiles not available
            param_values <- apply(samples, 2, median)
        }
    } else {
        param_values <- colMeans(samples)
    }

    # CRITICAL: Validate that param_values has names
    if (is.null(names(param_values)) || any(names(param_values) == "")) {
        stop(paste0(
            "Parameter values have no names!\n\n",
            "This usually means:\n",
            "1. posterior_result$samples has no column names\n",
            "2. posterior_result$quantiles$parameter is empty\n\n",
            "Cannot create config without parameter names.\n",
            "Check that NPE model training completed successfully."
        ))
    }

    # Update configuration with location-aware parameter matching
    config_npe <- config_base
    param_names <- names(param_values)

    # Extract location codes from parameter names
    location_codes <- .extract_location_codes(param_names)

    if (verbose && length(location_codes) > 0) {
        message("  Detected locations: ", paste(location_codes, collapse = ", "))
    }

    # Track updates for verbose reporting
    n_updated <- 0
    n_skipped <- 0
    skipped_params <- character(0)

    # Update configuration with location-aware mapping
    for (param in param_names) {
        parsed <- .parse_location_param(param)

        if (!is.null(parsed)) {
            # Location-specific parameter (e.g., tau_i_ETH -> base: tau_i, loc: ETH)
            base_name <- parsed$base
            location <- parsed$location

            if (base_name %in% names(config_npe)) {
                # Check if config has this parameter
                config_value <- config_npe[[base_name]]

                # Handle scalar (single location) vs vector (multi-location)
                if (length(config_value) == 1) {
                    # Single location: update the scalar value
                    new_val <- as.numeric(param_values[param])
                    if (debug) {
                        message(sprintf("  %s -> config$%s: %.6f", param, base_name, new_val))
                    }
                    config_npe[[base_name]] <- new_val
                    n_updated <- n_updated + 1
                } else {
                    # Multi-location: update the appropriate vector element
                    # Find the index for this location in config$location_name
                    if ("location_name" %in% names(config_npe)) {
                        loc_idx <- which(config_npe$location_name == location)
                        if (length(loc_idx) > 0) {
                            new_val <- as.numeric(param_values[param])
                            if (debug) {
                                message(sprintf("  %s -> config$%s[%d] (%s): %.6f",
                                               param, base_name, loc_idx, location, new_val))
                            }
                            config_npe[[base_name]][loc_idx] <- new_val
                            n_updated <- n_updated + 1
                        } else {
                            if (verbose) {
                                message("  Warning: Location ", location, " not found in config for ", base_name)
                            }
                            n_skipped <- n_skipped + 1
                            skipped_params <- c(skipped_params, param)
                        }
                    } else {
                        # No location_name field, treat as single location
                        new_val <- as.numeric(param_values[param])
                        if (debug) {
                            message(sprintf("  %s -> config$%s: %.6f", param, base_name, new_val))
                        }
                        config_npe[[base_name]] <- new_val
                        n_updated <- n_updated + 1
                    }
                }
            } else {
                n_skipped <- n_skipped + 1
                skipped_params <- c(skipped_params, param)
            }
        } else {
            # Global parameter (e.g., phi_1, gamma_2) - no location suffix
            if (param %in% names(config_npe)) {
                new_val <- as.numeric(param_values[param])
                if (debug) {
                    message(sprintf("  %s -> config$%s: %.6f", param, param, new_val))
                }
                config_npe[[param]] <- new_val
                n_updated <- n_updated + 1
            } else {
                n_skipped <- n_skipped + 1
                skipped_params <- c(skipped_params, param)
            }
        }
    }

    if (verbose) {
        message("  Updated ", n_updated, " parameters")
        if (n_skipped > 0) {
            message("  Skipped ", n_skipped, " parameters (not in config)")
            if (n_skipped <= 5) {
                message("    Skipped: ", paste(skipped_params, collapse = ", "))
            }
        }
    }

    # Add metadata
    config_npe$npe_metadata <- list(
        estimation_method = "NPE",
        point_estimate = if (use_median) "median" else "mean",
        n_samples = posterior_result$n_samples,
        n_parameters_updated = n_updated,
        n_parameters_skipped = n_skipped,
        timestamp = Sys.time()
    )

    # Save if requested
    if (!is.null(output_file)) {
        dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
        jsonlite::write_json(
            config_npe,
            output_file,
            pretty = TRUE,
            auto_unbox = TRUE
        )
        if (verbose) message("  Configuration saved to: ", output_file)
    }

    return(config_npe)
}

# ==============================================================================
# Helper Functions for Location Parameter Parsing
# ==============================================================================

#' Extract location codes from parameter names
#' @keywords internal
.extract_location_codes <- function(param_names) {
    # Extract unique location codes (2-3 letter ISO codes at end)
    location_pattern <- "_([A-Z]{2,3})$"
    matches <- regmatches(param_names, regexpr(location_pattern, param_names))
    locations <- unique(sub("^_", "", matches))
    locations <- locations[nzchar(locations)]
    return(locations)
}

#' Check if parameter is location-specific
#' @keywords internal
.is_location_specific <- function(param_name, location_codes) {
    if (length(location_codes) == 0) return(FALSE)
    # Check if parameter ends with any known location code
    pattern <- paste0("_", paste(location_codes, collapse = "|"), "$")
    grepl(pattern, param_name)
}

#' Parse location-specific parameter name
#' @keywords internal
.parse_location_param <- function(param_name) {
    # Extract base name and location code
    # E.g., "tau_i_ETH" -> list(base="tau_i", location="ETH")
    pattern <- "^(.+)_([A-Z]{2,3})$"
    if (grepl(pattern, param_name)) {
        base_name <- sub(pattern, "\\1", param_name)
        location <- sub(pattern, "\\2", param_name)
        return(list(base = base_name, location = location))
    }
    return(NULL)
}

#' Get parameter description from priors.json
#' @keywords internal
.get_param_description <- function(base_name, priors) {
    # Try to find description in priors
    if (!is.null(priors$parameters_global) && !is.null(priors$parameters_global[[base_name]])) {
        return(priors$parameters_global[[base_name]]$description)
    }
    if (!is.null(priors$parameters_location) && !is.null(priors$parameters_location[[base_name]])) {
        return(priors$parameters_location[[base_name]]$description)
    }
    return(NULL)
}

#' Fit Posterior Distributions
#'
#' @description
#' Fits parametric distributions to NPE posterior samples for each parameter.
#' Creates proper nested structure with parameters_global and parameters_location
#' to match BFRS format for plotting.
#'
#' @param posterior_samples Matrix of posterior samples
#' @param priors_file Path to priors JSON file (for distribution types)
#' @param output_file Path to save fitted distributions
#' @param verbose Print progress
#'
#' @return List of fitted distributions with proper global/location structure
#' @export
fit_posterior_distributions <- function(
    posterior_samples,
    priors_file = NULL,
    output_file = NULL,
    verbose = TRUE
) {

    if (verbose) message("Fitting posterior distributions...")

    # Load priors to get distribution types and descriptions
    if (!is.null(priors_file) && file.exists(priors_file)) {
        priors <- jsonlite::read_json(priors_file)
    } else {
        priors <- list()
    }

    param_names <- colnames(posterior_samples)

    # Extract location codes from parameter names
    location_codes <- .extract_location_codes(param_names)
    if (verbose && length(location_codes) > 0) {
        message("  Detected location codes: ", paste(location_codes, collapse = ", "))
    }

    # Initialize structures
    parameters_global <- list()
    parameters_location <- list()

    for (param in param_names) {
        if (verbose) message("  Fitting ", param, "...")

        samples <- posterior_samples[, param]

        # Determine distribution type from prior or data
        dist_type <- .infer_distribution_type(param, samples, priors)

        # Fit distribution
        fitted <- .fit_distribution(samples, dist_type)

        # Convert to BFRS format with nested parameters structure
        bfrs_format <- list(
            distribution = fitted$distribution,
            parameters = list()
        )

        # Map NPE parameter names to BFRS parameter names
        if (fitted$distribution == "beta") {
            bfrs_format$parameters$shape1 <- fitted$alpha
            bfrs_format$parameters$shape2 <- fitted$beta
            if (!is.null(fitted$mean)) bfrs_format$parameters$fitted_mean <- fitted$mean
        } else if (fitted$distribution == "gamma") {
            bfrs_format$parameters$shape <- fitted$shape
            bfrs_format$parameters$rate <- fitted$rate
            if (!is.null(fitted$mean)) bfrs_format$parameters$fitted_mean <- fitted$mean
        } else if (fitted$distribution == "lognormal") {
            bfrs_format$parameters$meanlog <- fitted$meanlog
            bfrs_format$parameters$sdlog <- fitted$sdlog
            if (!is.null(fitted$mean)) bfrs_format$parameters$fitted_mean <- fitted$mean
        } else if (fitted$distribution == "normal") {
            bfrs_format$parameters$mean <- fitted$mean
            bfrs_format$parameters$sd <- fitted$sd
        } else if (fitted$distribution == "uniform") {
            bfrs_format$parameters$min <- fitted$min
            bfrs_format$parameters$max <- fitted$max
        }

        # Check if parameter is location-specific
        if (.is_location_specific(param, location_codes)) {
            # Parse location parameter
            parsed <- .parse_location_param(param)

            if (!is.null(parsed)) {
                base_name <- parsed$base
                location <- parsed$location

                # Initialize nested structure if needed
                if (is.null(parameters_location[[base_name]])) {
                    # Try to get description from priors
                    description <- .get_param_description(base_name, priors)

                    parameters_location[[base_name]] <- list(
                        description = if (!is.null(description)) description else base_name,
                        location = list()
                    )
                }

                # Add to nested structure
                parameters_location[[base_name]]$location[[location]] <- bfrs_format
            } else {
                # Couldn't parse, add to global as fallback
                parameters_global[[param]] <- bfrs_format
            }
        } else {
            # Global parameter
            parameters_global[[param]] <- bfrs_format
        }
    }

    # Create final BFRS-compatible structure
    posteriors <- list(
        metadata = list(
            version = "7.0.0",
            date = format(Sys.Date(), "%Y-%m-%d"),
            description = "Posterior distributions fitted from NPE samples",
            source = "Neural Posterior Estimation (NPE)"
        ),
        parameters_global = parameters_global
    )

    # Only add parameters_location if non-empty
    if (length(parameters_location) > 0) {
        posteriors$parameters_location <- parameters_location
        if (verbose) {
            message("  Created nested structure for ", length(parameters_location),
                   " location-specific parameters")
        }
    }

    # Save if requested
    if (!is.null(output_file)) {
        dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
        jsonlite::write_json(
            posteriors,
            output_file,
            pretty = TRUE,
            auto_unbox = TRUE
        )
        if (verbose) message("  Posteriors saved to: ", output_file)
    }

    return(posteriors)
}

#' Get NPE Observed Data
#'
#' @description
#' Extracts and formats observed outbreak data for NPE conditioning.
#' Properly handles LASER config format where reported_cases can be:
#' - Vector: single location time series
#' - Matrix: rows = locations, columns = time points
#'
#' @param config Configuration with outbreak data
#' @param aggregate_locations Aggregate across locations (default FALSE)
#' @param verbose Print progress
#'
#' @return Data frame formatted for NPE with columns: j, t, cases
#' @export
get_npe_observed_data <- function(config, aggregate_locations = FALSE, verbose = TRUE) {

    if (verbose) message("Preparing observed data for NPE...")

    # Extract location identifiers if available
    location_names <- NULL
    if ("location_name" %in% names(config)) {
        location_names <- config$location_name
    } else if ("iso_code" %in% names(config)) {
        location_names <- config$iso_code
    }

    # Extract cases data
    if (!("reported_cases" %in% names(config))) {
        if ("observed_cases" %in% names(config)) {
            config$reported_cases <- config$observed_cases
        } else {
            stop("No reported_cases or observed_cases found in config")
        }
    }

    cases_data <- config$reported_cases

    # Determine data structure and convert to long format
    if (is.vector(cases_data) && !is.matrix(cases_data)) {
        # Single location case - vector format
        n_timesteps <- length(cases_data)
        n_locations <- 1

        if (verbose) {
            message("  Processing single location data (vector format)")
            message("  Time points: ", n_timesteps)
        }

        # Create long format data frame
        outbreak_data <- data.frame(
            j = if (!is.null(location_names) && length(location_names) >= 1) {
                rep(location_names[1], n_timesteps)
            } else {
                rep(1, n_timesteps)
            },
            t = 1:n_timesteps,
            cases = cases_data
        )

    } else if (is.matrix(cases_data)) {
        # Multi-location case - matrix format
        # IMPORTANT: In LASER format, rows are locations, columns are time points
        n_locations <- nrow(cases_data)
        n_timesteps <- ncol(cases_data)

        if (verbose) {
            message("  Processing multi-location data (matrix format)")
            message("  Locations: ", n_locations, ", Time points: ", n_timesteps)
        }

        # Create long format data frame
        df_list <- list()
        for (loc_idx in 1:n_locations) {
            location_id <- if (!is.null(location_names) && length(location_names) >= loc_idx) {
                location_names[loc_idx]
            } else {
                loc_idx
            }

            df_list[[loc_idx]] <- data.frame(
                j = rep(location_id, n_timesteps),
                t = 1:n_timesteps,
                cases = cases_data[loc_idx, ]  # Extract row for this location
            )
        }
        outbreak_data <- do.call(rbind, df_list)

    } else if (is.data.frame(cases_data)) {
        # Already a data frame - just ensure it has the right columns
        outbreak_data <- cases_data

        # Check for required columns
        if (!all(c("j", "t", "cases") %in% colnames(outbreak_data))) {
            if (verbose) message("  Data frame missing required columns, attempting to restructure...")

            # If it looks like wide format, try to convert
            if (ncol(outbreak_data) > 3) {
                # Assume columns are time points, rows are locations
                n_locations <- nrow(outbreak_data)
                n_timesteps <- ncol(outbreak_data)

                df_list <- list()
                for (loc_idx in 1:n_locations) {
                    df_list[[loc_idx]] <- data.frame(
                        j = loc_idx,
                        t = 1:n_timesteps,
                        cases = as.numeric(outbreak_data[loc_idx, ])
                    )
                }
                outbreak_data <- do.call(rbind, df_list)
            } else {
                stop("Data frame format not recognized. Need columns: j, t, cases")
            }
        }

    } else {
        stop("Unrecognized format for reported_cases: ", class(cases_data))
    }

    # Handle NA values
    if (any(is.na(outbreak_data$cases))) {
        if (verbose) message("  Converting ", sum(is.na(outbreak_data$cases)), " NA values to 0")
        outbreak_data$cases[is.na(outbreak_data$cases)] <- 0
    }

    # Aggregate across locations if requested
    if (aggregate_locations && length(unique(outbreak_data$j)) > 1) {
        if (verbose) message("  Aggregating across ", length(unique(outbreak_data$j)), " locations")

        aggregated <- aggregate(
            cases ~ t,
            data = outbreak_data,
            FUN = sum
        )
        aggregated$j <- 1
        outbreak_data <- aggregated[, c("j", "t", "cases")]
    }

    # Sort by location and time
    outbreak_data <- outbreak_data[order(outbreak_data$j, outbreak_data$t), ]

    # Add deaths if requested and available
    if ("reported_deaths" %in% names(config) && !is.null(config$reported_deaths)) {
        deaths_data <- config$reported_deaths

        if (is.vector(deaths_data) && !is.matrix(deaths_data)) {
            # Single location
            outbreak_data$deaths <- deaths_data[outbreak_data$t]
        } else if (is.matrix(deaths_data)) {
            # Multi-location - match structure
            deaths_long <- data.frame()
            for (loc_idx in 1:nrow(deaths_data)) {
                location_id <- if (!is.null(location_names) && length(location_names) >= loc_idx) {
                    location_names[loc_idx]
                } else {
                    loc_idx
                }

                deaths_long <- rbind(deaths_long, data.frame(
                    j = location_id,
                    t = 1:ncol(deaths_data),
                    deaths = deaths_data[loc_idx, ]
                ))
            }

            # Merge with cases data
            outbreak_data <- merge(outbreak_data, deaths_long, by = c("j", "t"), all.x = TRUE)
        }

        # Handle NA deaths
        if ("deaths" %in% colnames(outbreak_data)) {
            outbreak_data$deaths[is.na(outbreak_data$deaths)] <- 0
        }
    }

    if (verbose) {
        message("  Final data shape: ", nrow(outbreak_data), " rows")
        message("  Locations: ", length(unique(outbreak_data$j)))
        message("  Time points: ", length(unique(outbreak_data$t)))
        message("  Total cases: ", sum(outbreak_data$cases))
    }

    return(outbreak_data)
}

# ==============================================================================
# Internal Helper Functions
# ==============================================================================

#' @keywords internal
.prepare_observed_data <- function(observed_data) {
    # Convert observed data to vector matching training format

    if (is.data.frame(observed_data)) {
        # Data frame format with j, t, cases columns
        if (!all(c("j", "t", "cases") %in% names(observed_data))) {
            stop("Data frame must have columns: j, t, cases")
        }
        # Sort by location and time
        obs_sorted <- observed_data[order(observed_data$j, observed_data$t), ]
        # Extract cases as vector
        obs_vector <- obs_sorted$cases
    } else if (is.matrix(observed_data)) {
        # Matrix format: rows = timesteps, cols = locations
        # Flatten by column (location-major order)
        obs_vector <- as.vector(observed_data)
    } else if (is.vector(observed_data)) {
        # Already in vector format
        obs_vector <- observed_data
    } else {
        stop("observed_data must be a data frame, matrix, or vector")
    }

    return(as.numeric(obs_vector))
}

#' @keywords internal
.calculate_quantiles <- function(samples, quantile_levels) {
    # Calculate quantiles for each parameter in BFRS-compatible long format

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
            param_type = character(0)
        )
    })

    # Initialize results data frame (long format like BFRS)
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
            q_values <- rep(NA_real_, length(quantile_levels))
            names(q_values) <- paste0("q", quantile_levels)
            param_mean <- NA_real_
            param_sd <- NA_real_
        } else {
            # Remove NAs for quantile calculation
            param_values_clean <- param_values[!is.na(param_values)]
            if (length(param_values_clean) > 0) {
                q_values <- quantile(param_values_clean, probs = quantile_levels, na.rm = TRUE)
                param_mean <- mean(param_values_clean, na.rm = TRUE)
                param_sd <- sd(param_values_clean, na.rm = TRUE)
            } else {
                q_values <- rep(NA_real_, length(quantile_levels))
                names(q_values) <- paste0("q", quantile_levels)
                param_mean <- NA_real_
                param_sd <- NA_real_
            }
        }

        # Ensure q_values has the expected length
        if (length(q_values) != length(quantile_levels)) {
            warning("Quantile calculation failed for parameter ", param_name, ", using NA values")
            q_values <- rep(NA_real_, length(quantile_levels))
            names(q_values) <- paste0("q", quantile_levels)
        }

        # Create row in BFRS format
        param_row <- data.frame(
            parameter = as.character(param_name),
            description = as.character(description),
            category = as.character(category),
            param_type = as.character(param_type),
            location = as.character(location),
            prior_distribution = NA_character_,
            type = "npe",
            mean = param_mean,
            sd = param_sd,
            mode = NA_real_,
            kl = NA_real_,
            stringsAsFactors = FALSE
        )

        # Add quantile columns with proper naming and validation
        for (j in seq_along(quantile_levels)) {
            col_name <- sprintf("q%s", quantile_levels[j])
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

#' @keywords internal
.save_posterior_results <- function(samples, log_probs, quantiles, output_dir, verbose) {

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    # Save samples
    samples_file <- file.path(output_dir, "posterior_samples.parquet")
    arrow::write_parquet(as.data.frame(samples), samples_file)
    if (verbose) message("  Samples saved to: ", samples_file)

    # CRITICAL FOR SMC: Save log probabilities
    if (!is.null(log_probs)) {
        log_probs_file <- file.path(output_dir, "posterior_log_probs.parquet")
        arrow::write_parquet(
            data.frame(log_q_theta = log_probs),
            log_probs_file
        )
        if (verbose) message("  Log probabilities saved to: ", log_probs_file)
    }

    # Save quantiles (already in BFRS-compatible long format)
    quantiles_file <- file.path(output_dir, "posterior_quantiles.csv")
    write.csv(quantiles, quantiles_file, row.names = FALSE)
    if (verbose) message("  Quantiles saved to: ", quantiles_file)
}

#' @keywords internal
.infer_distribution_type <- function(param_name, samples, priors) {
    # Infer distribution type from parameter name or prior
    # Properly navigates nested priors JSON structure (parameters_global, parameters_location)

    # Parse parameter name for location-specific params
    parsed <- .parse_location_param(param_name)

    # Try to get distribution from priors structure
    dist_from_prior <- NULL

    if (!is.null(parsed)) {
        # Location-specific parameter
        base_name <- parsed$base
        location <- parsed$location

        # Try multiple base name variants for seasonality params
        base_variants <- c(base_name)
        if (base_name %in% c("a_1_j", "a_2_j", "b_1_j", "b_2_j")) {
            base_variants <- c(base_variants,
                             gsub("_j$", "", gsub("_", "", base_name)))
        }
        if (base_name %in% c("a1", "a2", "b1", "b2")) {
            base <- substr(base_name, 1, 1)
            num <- substr(base_name, 2, 2)
            base_variants <- c(base_variants, paste0(base, "_", num, "_j"))
        }

        # Check in parameters_location with all variants
        if (!is.null(priors$parameters_location)) {
            for (variant in base_variants) {
                if (variant %in% names(priors$parameters_location)) {
                    loc_structure <- priors$parameters_location[[variant]]
                    if (!is.null(loc_structure$location) &&
                        location %in% names(loc_structure$location)) {
                        loc_dist <- loc_structure$location[[location]]
                        if ("distribution" %in% names(loc_dist)) {
                            dist_from_prior <- loc_dist$distribution
                            break
                        }
                    }
                }
            }
        }
    } else {
        # Global parameter - check parameters_global
        if (!is.null(priors$parameters_global) &&
            param_name %in% names(priors$parameters_global)) {
            global_dist <- priors$parameters_global[[param_name]]
            if ("distribution" %in% names(global_dist)) {
                dist_from_prior <- global_dist$distribution
            }
        }
    }

    # Return distribution from priors if found
    if (!is.null(dist_from_prior)) {
        return(dist_from_prior)
    }

    # Fall back to pattern matching only if not found in priors
    # NOTE: Pattern matching is less reliable than priors, so only use as fallback
    if (grepl("^p_|^prop_|_prop$|_rate$", param_name)) {
        return("beta")  # Proportions/rates often Beta
    }
    if (grepl("^lambda_|_intensity$", param_name)) {
        return("gamma")  # Rate parameters often Gamma
    }
    if (grepl("^beta_|^alpha_|^delta_", param_name)) {
        return("lognormal")  # Transmission parameters often log-normal
    }

    # Infer from data range as last resort
    min_val <- min(samples, na.rm = TRUE)
    max_val <- max(samples, na.rm = TRUE)

    if (min_val >= 0 && max_val <= 1) {
        return("beta")
    } else if (min_val >= 0) {
        return("gamma")
    } else {
        return("normal")
    }
}

#' @keywords internal
.fit_distribution <- function(samples, dist_type) {
    # Fit specified distribution to samples

    result <- list(distribution = dist_type)

    if (dist_type == "beta") {
        # Method of moments for Beta
        m <- mean(samples)
        v <- var(samples)

        # Ensure values are in (0, 1)
        samples <- pmax(0.001, pmin(0.999, samples))
        m <- mean(samples)
        v <- var(samples)

        # Check for zero or near-zero variance (constant samples)
        if (v < 1e-10) {
            # Use very large shape parameters to approximate point mass
            # Beta distribution with large alpha, beta has low variance
            # This handles cases where NPE posterior collapses to a point
            result$alpha <- m * 1e6
            result$beta <- (1 - m) * 1e6
            result$mean <- m
            result$variance <- v
        } else {
            # Normal method of moments calculation
            common <- m * (1 - m) / v - 1
            alpha <- m * common
            beta <- (1 - m) * common

            result$alpha <- max(0.01, alpha)
            result$beta <- max(0.01, beta)
            result$mean <- m
            result$variance <- v
        }

    } else if (dist_type == "gamma") {
        # Method of moments for Gamma
        m <- mean(samples)
        v <- var(samples)

        # Check for zero or near-zero variance (constant samples)
        if (v < 1e-10) {
            # Use very large shape parameter to approximate point mass
            # Gamma(shape=large, rate=large) has low variance
            result$shape <- m * 1e6
            result$rate <- 1e6
            result$mean <- m
            result$variance <- v
        } else {
            # Normal method of moments calculation
            shape <- m^2 / v
            rate <- m / v

            result$shape <- max(0.01, shape)
            result$rate <- max(0.01, rate)
            result$mean <- m
            result$variance <- v
        }

    } else if (dist_type == "lognormal") {
        # MLE for log-normal
        log_samples <- log(pmax(1e-10, samples))
        meanlog <- mean(log_samples)
        sdlog <- sd(log_samples)

        result$meanlog <- meanlog
        result$sdlog <- max(0.01, sdlog)
        result$mean <- mean(samples)
        result$variance <- var(samples)

    } else {  # normal
        result$mean <- mean(samples)
        result$sd <- sd(samples)
        result$variance <- var(samples)
    }

    return(result)
}

#' Evaluate Prior Densities for SMC
#'
#' @description
#' Evaluates log p(theta) for each posterior sample using original priors.
#' Required for SMC importance weight calculation.
#'
#' @param samples Matrix of parameter samples
#' @param priors List of prior specifications
#' @param verbose Print progress
#'
#' @return Vector of log prior densities
#' @export
evaluate_prior_densities <- function(samples, priors, verbose = FALSE) {

    if (verbose) message("Evaluating prior densities for SMC...")

    n_samples <- nrow(samples)
    log_priors <- rep(0, n_samples)

    param_names <- colnames(samples)

    for (param in param_names) {
        if (!(param %in% names(priors))) {
            warning("No prior found for parameter: ", param)
            next
        }

        prior <- priors[[param]]
        values <- samples[, param]

        # Evaluate density based on distribution type
        if (prior$distribution == "beta") {
            log_p <- dbeta(values, prior$alpha, prior$beta, log = TRUE)
        } else if (prior$distribution == "gamma") {
            log_p <- dgamma(values, shape = prior$shape, rate = prior$rate, log = TRUE)
        } else if (prior$distribution == "lognormal") {
            log_p <- dlnorm(values, meanlog = prior$meanlog, sdlog = prior$sdlog, log = TRUE)
        } else if (prior$distribution == "normal") {
            log_p <- dnorm(values, mean = prior$mean, sd = prior$sd, log = TRUE)
        } else if (prior$distribution == "uniform") {
            log_p <- dunif(values, min = prior$min, max = prior$max, log = TRUE)
        } else {
            warning("Unknown distribution type for ", param, ": ", prior$distribution)
            log_p <- rep(0, length(values))
        }

        log_priors <- log_priors + log_p
    }

    return(log_priors)
}