#' Calculate Prior and Posterior Quantiles with KL Divergence
#'
#' Calculates quantiles for both prior (all simulations) and posterior (weighted best subset)
#' distributions, with KL divergence between them using KDE.
#'
#' @param results Data frame with calibration results containing parameter columns
#'   and index columns (is_finite, is_retained, is_best_subset)
#' @param probs Numeric vector of quantile probabilities (default: c(0.025, 0.25, 0.5, 0.75, 0.975))
#' @param output_dir Directory path to save results (default: "./results")
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return Data frame with both prior and posterior quantiles plus KL divergence
#'
#' @details
#' The function:
#' 1. Calculates unweighted quantiles from ALL finite simulations (prior)
#' 2. Calculates weighted quantiles from best subset (posterior)
#' 3. Computes KL divergence using KDE on empirical distributions
#' 4. Returns both prior and posterior rows with a 'type' identifier
#' 5. KL divergence appears only in posterior rows
#'
#' @seealso \code{\link{weighted_quantiles}}, \code{\link{calc_model_ess}}
#' @export
calc_model_posterior_quantiles <- function(results,
                                         probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                         output_dir = "./results",
                                         verbose = TRUE) {

    if (verbose) message("Calculating prior and posterior parameter quantiles...")

    # Validate inputs
    if (!is.data.frame(results) || nrow(results) == 0) {
        stop("Results must be a non-empty data frame")
    }

    required_cols <- c("is_finite", "is_retained", "is_best_subset", "weight_best")
    missing_cols <- setdiff(required_cols, names(results))
    if (length(missing_cols) > 0) {
        stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # Create output directory if needed
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Load estimated parameters template
    data(estimated_parameters, package = "MOSAIC", envir = environment())

    # Helper function to calculate mode from KDE
    calc_mode_kde <- function(samples, weights = NULL) {
        tryCatch({
            # Remove non-finite values
            samples <- samples[is.finite(samples)]

            if (length(samples) == 0) {
                return(NA_real_)
            }

            # Handle constant values
            if (length(unique(samples)) == 1) {
                return(unique(samples))
            }

            # Calculate KDE
            if (!is.null(weights) && length(weights) == length(samples)) {
                # Normalize weights
                weights <- weights / sum(weights)
                # Suppress expected warning about bandwidth not using weights (known R limitation)
                kde <- suppressWarnings(density(samples, weights = weights, n = 512))
            } else {
                kde <- density(samples, n = 512)
            }

            # Find the peak (mode)
            mode_idx <- which.max(kde$y)
            mode_val <- kde$x[mode_idx]

            return(mode_val)

        }, error = function(e) {
            return(NA_real_)
        })
    }

    # Helper function to calculate KL divergence using KDE
    calc_kl_kde <- function(prior_samples, posterior_samples, posterior_weights = NULL) {
        tryCatch({
            # Remove non-finite values
            prior_samples <- prior_samples[is.finite(prior_samples)]
            posterior_samples <- posterior_samples[is.finite(posterior_samples)]

            if (length(prior_samples) < 10 || length(posterior_samples) < 10) {
                return(NA_real_)
            }

            # Handle constant values
            if (length(unique(prior_samples)) == 1 && length(unique(posterior_samples)) == 1) {
                if (unique(prior_samples) == unique(posterior_samples)) {
                    return(0)  # Identical constants
                } else {
                    return(NA_real_)  # Different constants, KL undefined
                }
            }

            # Get reasonable bandwidth and range
            combined_range <- range(c(prior_samples, posterior_samples))
            range_width <- diff(combined_range)

            # Expand range slightly for KDE
            eval_from <- combined_range[1] - 0.1 * range_width
            eval_to <- combined_range[2] + 0.1 * range_width

            # Calculate KDE for prior (unweighted)
            kde_prior <- density(prior_samples,
                                from = eval_from,
                                to = eval_to,
                                n = 512)

            # Calculate KDE for posterior (weighted if weights provided)
            if (!is.null(posterior_weights) && length(posterior_weights) == length(posterior_samples)) {
                # Normalize weights
                posterior_weights <- posterior_weights / sum(posterior_weights)
                # Suppress expected warning about bandwidth not using weights (known R limitation)
                kde_posterior <- suppressWarnings(
                    density(posterior_samples,
                           weights = posterior_weights,
                           from = eval_from,
                           to = eval_to,
                           n = 512)
                )
            } else {
                kde_posterior <- density(posterior_samples,
                                        from = eval_from,
                                        to = eval_to,
                                        n = 512)
            }

            # Ensure same evaluation points
            x_eval <- kde_prior$x
            p_prior <- kde_prior$y
            p_posterior <- kde_posterior$y

            # Normalize to ensure they're proper probability densities
            dx <- diff(x_eval[1:2])
            p_prior <- p_prior / sum(p_prior * dx)
            p_posterior <- p_posterior / sum(p_posterior * dx)

            # Add small epsilon to avoid log(0)
            eps <- 1e-10
            p_prior[p_prior < eps] <- eps
            p_posterior[p_posterior < eps] <- eps

            # Calculate KL(Prior || Posterior)
            kl <- sum(p_prior * log(p_prior / p_posterior) * dx)

            # Cap at reasonable maximum
            if (kl > 20) kl <- 20

            return(kl)

        }, error = function(e) {
            return(NA_real_)
        })
    }

    # Get parameter columns from results, excluding metadata columns
    exclude_cols <- c("sim", "iter", "seed_sim", "seed_iter", "likelihood", "aic", "delta_aic",
                     "weight_retained", "weight_best", "is_finite", "is_valid", "is_outlier", "is_retained",
                     "is_best_subset", "is_best_model", "best_subset_B")

    # Also exclude columns that are not estimated parameters
    all_cols <- names(results)
    non_param_patterns <- c("^N_j_initial", "^time", "^timestamp", "^retained", "^converged")
    additional_exclude <- all_cols[grepl(paste(non_param_patterns, collapse = "|"), all_cols)]
    exclude_cols <- c(exclude_cols, additional_exclude)

    # Convert initial condition counts to proportions
    initial_condition_patterns <- c("^S_j_initial", "^E_j_initial", "^I_j_initial", "^R_j_initial", "^V1_j_initial", "^V2_j_initial")
    initial_cols <- all_cols[grepl(paste(initial_condition_patterns, collapse = "|"), all_cols)]

    if (length(initial_cols) > 0) {
        if (verbose) {
            message("Converting initial condition counts to proportions for ", length(initial_cols), " variables")
        }

        for (col in initial_cols) {
            # Extract location suffix (e.g., "_ETH" from "S_j_initial_ETH")
            location_suffix <- gsub(".*(_[A-Z]{3})$", "\\1", col)
            n_col <- paste0("N_j_initial", location_suffix)

            if (n_col %in% all_cols) {
                # Create proportion column name
                # Handle both patterns: S_j_initial, V1_j_initial, etc.
                prop_col <- gsub("^([A-Z]+[0-9]*)_j_initial", "prop_\\1_initial", col)

                # Calculate proportions
                results[[prop_col]] <- results[[col]] / results[[n_col]]

                if (verbose && any(is.finite(results[[prop_col]]))) {
                    prop_range <- range(results[[prop_col]], na.rm = TRUE)
                    message("  ", col, " -> ", prop_col, " (range: ",
                           round(prop_range[1], 4), " - ", round(prop_range[2], 4), ")")
                }
            }
        }

        # Add proportion columns to parameter list and exclude original count columns
        prop_cols <- gsub("^([A-Z]+[0-9]*)_j_initial", "prop_\\1_initial", initial_cols)
        exclude_cols <- c(exclude_cols, initial_cols)  # Exclude original count columns

        # Update all_cols to include the new proportion columns
        all_cols <- names(results)
    }

    param_cols <- setdiff(all_cols, exclude_cols)

    if (length(param_cols) == 0) {
        stop("No parameter columns found in results")
    }

    # Identify location patterns for multi-location handling
    location_suffixes <- unique(gsub(".*_([A-Z]{3})$", "\\1", param_cols[grepl("_[A-Z]{3}$", param_cols)]))

    if (verbose) {
        message("Parameter columns identified: ", length(param_cols))
        if (length(location_suffixes) > 0) {
            message("Locations detected: ", paste(location_suffixes, collapse = ", "))
        }
    }

    # Initialize results structure
    quantile_names <- paste0("q", probs)

    # Create parameter inventory based on estimated_parameters template
    param_inventory <- data.frame(
        parameter = character(0),
        display_name = character(0),
        category = character(0),
        scale = character(0),
        location = character(0),
        distribution = character(0),
        stringsAsFactors = FALSE
    )

    # Build parameter inventory following estimated_parameters order
    for (i in seq_len(nrow(estimated_parameters))) {
        param_base <- estimated_parameters$parameter_name[i]
        scale <- estimated_parameters$scale[i]

        if (scale == "global") {
            # Global parameters - single entry
            if (param_base %in% param_cols) {
                param_inventory <- rbind(param_inventory, data.frame(
                    parameter = param_base,
                    display_name = estimated_parameters$display_name[i],
                    category = estimated_parameters$category[i],
                    scale = scale,
                    location = "",
                    distribution = estimated_parameters$distribution[i],
                    stringsAsFactors = FALSE
                ))
            }
        } else if (scale == "location") {
            # Location-specific parameters - one entry per location
            for (iso in location_suffixes) {
                param_name <- paste0(param_base, "_", iso)
                if (param_name %in% param_cols) {
                    param_inventory <- rbind(param_inventory, data.frame(
                        parameter = param_name,
                        display_name = paste(estimated_parameters$display_name[i], iso),
                        category = estimated_parameters$category[i],
                        scale = scale,
                        location = iso,
                        distribution = estimated_parameters$distribution[i],
                        stringsAsFactors = FALSE
                    ))
                }
            }
        }
    }

    # Add any remaining parameters not in estimated_parameters template
    remaining_params <- setdiff(param_cols, param_inventory$parameter)
    if (length(remaining_params) > 0) {
        for (param in remaining_params) {
            param_inventory <- rbind(param_inventory, data.frame(
                parameter = param,
                display_name = param,
                category = "other",
                scale = "unknown",
                location = "",
                distribution = "unknown",
                stringsAsFactors = FALSE
            ))
        }
    }

    if (verbose) {
        message("Parameter inventory created: ", nrow(param_inventory), " parameters")
    }

    # Initialize results dataframe - will have 2x rows (prior + posterior for each param)
    total_rows <- nrow(param_inventory) * 2
    quantile_results <- data.frame(
        parameter = character(total_rows),
        display_name = character(total_rows),
        category = character(total_rows),
        scale = character(total_rows),
        iso_code = character(total_rows),
        distribution = character(total_rows),
        type = character(total_rows),  # NEW: "prior" or "posterior"
        mean = numeric(total_rows),
        sd = numeric(total_rows),
        mode = numeric(total_rows),     # NEW: mode from KDE
        kl = numeric(total_rows),      # NEW: KL divergence (NA for prior rows)
        stringsAsFactors = FALSE
    )

    # Initialize quantile columns
    for (qname in quantile_names) {
        quantile_results[[qname]] <- NA_real_
    }

    # Get indices for different subsets
    all_finite_idx <- results$is_finite
    best_subset_idx <- results$is_best_subset
    best_subset_weights <- results$weight_best[best_subset_idx]

    if (verbose) {
        message("Computing prior and posterior quantiles for ", nrow(param_inventory), " parameters...")
        message("All finite simulations (prior): ", sum(all_finite_idx), " models")
        message("Best subset (posterior): ", sum(best_subset_idx), " models")
    }

    # Process each parameter
    row_idx <- 0
    for (i in seq_len(nrow(param_inventory))) {
        param_name <- param_inventory$parameter[i]

        if (!param_name %in% names(results)) {
            if (verbose) message("Warning: Parameter not found in results: ", param_name)
            # Still need to increment row index
            row_idx <- row_idx + 2
            next
        }

        # Get parameter values
        all_values <- results[[param_name]]

        # PRIOR: All finite simulations (unweighted)
        row_idx <- row_idx + 1
        prior_values <- all_values[all_finite_idx]
        prior_values <- prior_values[is.finite(prior_values)]

        if (length(prior_values) > 0) {
            # Fill in metadata
            quantile_results$parameter[row_idx] <- param_inventory$parameter[i]
            quantile_results$display_name[row_idx] <- param_inventory$display_name[i]
            quantile_results$category[row_idx] <- param_inventory$category[i]
            quantile_results$scale[row_idx] <- param_inventory$scale[i]
            quantile_results$iso_code[row_idx] <- ifelse(param_inventory$location[i] == "", NA_character_, param_inventory$location[i])
            quantile_results$distribution[row_idx] <- param_inventory$distribution[i]
            quantile_results$type[row_idx] <- "prior"
            quantile_results$kl[row_idx] <- NA_real_  # No KL for prior row

            # Calculate unweighted statistics
            quantile_results$mean[row_idx] <- mean(prior_values)
            quantile_results$sd[row_idx] <- ifelse(length(prior_values) > 1, sd(prior_values), 0)
            quantile_results$mode[row_idx] <- calc_mode_kde(prior_values)

            # Calculate unweighted quantiles
            if (length(unique(prior_values)) <= 1) {
                # Constant parameter
                constant_value <- prior_values[1]
                for (qname in quantile_names) {
                    quantile_results[[qname]][row_idx] <- constant_value
                }
            } else {
                # Calculate quantiles
                quantiles <- quantile(prior_values, probs = probs)
                for (j in seq_along(probs)) {
                    quantile_results[[quantile_names[j]]][row_idx] <- quantiles[j]
                }
            }
        }

        # POSTERIOR: Best subset (weighted)
        row_idx <- row_idx + 1
        posterior_values <- all_values[best_subset_idx]
        posterior_weights <- best_subset_weights
        valid_idx <- is.finite(posterior_values) & is.finite(posterior_weights)

        if (sum(valid_idx) > 0) {
            posterior_values_valid <- posterior_values[valid_idx]
            weights_valid <- posterior_weights[valid_idx]

            # Fill in metadata
            quantile_results$parameter[row_idx] <- param_inventory$parameter[i]
            quantile_results$display_name[row_idx] <- param_inventory$display_name[i]
            quantile_results$category[row_idx] <- param_inventory$category[i]
            quantile_results$scale[row_idx] <- param_inventory$scale[i]
            quantile_results$iso_code[row_idx] <- ifelse(param_inventory$location[i] == "", NA_character_, param_inventory$location[i])
            quantile_results$distribution[row_idx] <- param_inventory$distribution[i]
            quantile_results$type[row_idx] <- "posterior"

            # Calculate KL divergence if we have both prior and posterior
            if (length(prior_values) > 0 && length(posterior_values_valid) > 0) {
                kl_value <- calc_kl_kde(prior_values, posterior_values_valid, weights_valid)
                quantile_results$kl[row_idx] <- kl_value
            } else {
                quantile_results$kl[row_idx] <- NA_real_
            }

            # Calculate weighted statistics
            weighted_mean <- weighted.mean(posterior_values_valid, weights_valid)
            quantile_results$mean[row_idx] <- weighted_mean

            # Calculate weighted standard deviation
            if (length(posterior_values_valid) > 1 && length(unique(posterior_values_valid)) > 1) {
                weighted_var <- sum(weights_valid * (posterior_values_valid - weighted_mean)^2) / sum(weights_valid)
                weighted_sd <- sqrt(weighted_var)
                quantile_results$sd[row_idx] <- weighted_sd
            } else {
                quantile_results$sd[row_idx] <- 0
            }

            # Calculate mode from weighted KDE
            quantile_results$mode[row_idx] <- calc_mode_kde(posterior_values_valid, weights_valid)

            # Calculate weighted quantiles
            if (length(unique(posterior_values_valid)) <= 1) {
                # Constant parameter
                constant_value <- posterior_values_valid[1]
                for (qname in quantile_names) {
                    quantile_results[[qname]][row_idx] <- constant_value
                }
            } else {
                # Calculate weighted quantiles
                quantiles <- MOSAIC::weighted_quantiles(posterior_values_valid, weights_valid, probs)
                for (j in seq_along(probs)) {
                    quantile_results[[quantile_names[j]]][row_idx] <- quantiles[j]
                }
            }
        }
    }

    # Remove any empty rows (if parameters were missing)
    quantile_results <- quantile_results[quantile_results$parameter != "", ]

    # Rename columns for output consistency
    output_df <- quantile_results
    names(output_df)[names(output_df) == "scale"] <- "param_type"
    names(output_df)[names(output_df) == "iso_code"] <- "location"
    names(output_df)[names(output_df) == "distribution"] <- "prior_distribution"
    names(output_df)[names(output_df) == "display_name"] <- "description"

    # Save quantiles as CSV
    output_file <- file.path(output_dir, "posterior_quantiles.csv")
    write.csv(output_df, output_file, row.names = FALSE)

    if (verbose) {
        n_params <- length(unique(output_df$parameter))
        n_prior <- sum(output_df$type == "prior")
        n_posterior <- sum(output_df$type == "posterior")
        mean_kl <- mean(output_df$kl[output_df$type == "posterior"], na.rm = TRUE)

        message("Results saved:")
        message("  File: ", basename(output_file))
        message("  Parameters: ", n_params)
        message("  Prior rows: ", n_prior)
        message("  Posterior rows: ", n_posterior)
        message("  Mean KL divergence: ", round(mean_kl, 3))
    }

    # Return results
    output_df
}