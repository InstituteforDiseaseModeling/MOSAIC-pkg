# ==============================================================================
# NPE Utility Functions
# ==============================================================================
# Internal helper functions for NPE workflow
# ==============================================================================

#' Get NPE Importance Weights
#'
#' @description
#' Retrieves or creates importance weights for NPE training from BFRS results.
#' This function selects the appropriate pre-calculated BFRS weights or creates
#' binary versions of them, avoiding recalculation issues.
#'
#' @param bfrs_results BFRS results object containing weight_retained and weight_best
#' @param strategy Weight strategy:
#'   "continuous_all" - Gibbs weights for all simulations (effective AIC range = 25)
#'   "binary_all" - Equal weights for all simulations
#'   "continuous_retained" - Use BFRS continuous weights for retained subset
#'   "binary_retained" - Equal weights for all retained simulations
#'   "continuous_best" - Use BFRS continuous weights for best subset
#'   "binary_best" - Equal weights for all best subset simulations
#' @param verbose Print weight statistics
#'
#' @return Vector of normalized weights
#' @export
get_npe_weights <- function(
    bfrs_results,
    strategy = "binary_best",
    verbose = FALSE
) {

    # Validate strategy
    valid_strategies <- c("continuous_all", "binary_all",
                         "continuous_retained", "binary_retained",
                         "continuous_best", "binary_best")

    if (!strategy %in% valid_strategies) {
        stop("Invalid strategy. Must be one of: ",
             paste(valid_strategies, collapse = ", "))
    }

    # =========================================================================
    # Strategy: binary_all (equal weights for all simulations)
    # =========================================================================
    if (strategy == "binary_all") {
        n_sims <- nrow(bfrs_results)
        weights <- rep(1.0 / n_sims, n_sims)

        if (verbose) {
            message("NPE Weight Strategy: binary_all")
            message("  Using all simulations with equal weights")
            message("  Total simulations: ", n_sims)
            message("  Weight per simulation: ", sprintf("%.6f", 1.0 / n_sims))
            message("  ESS = ", n_sims, " (maximum for uniform weights)")
        }

        return(weights)
    }

    # =========================================================================
    # Strategy: continuous_all (Gibbs weights for all simulations)
    # =========================================================================
    if (strategy == "continuous_all") {
        # Extract likelihoods for all simulations
        # BFRS results use 'likelihood' field (contains log-likelihood by default)
        if (!"likelihood" %in% names(bfrs_results)) {
            stop("bfrs_results must contain 'likelihood' field for continuous_all strategy")
        }

        likelihoods <- bfrs_results$likelihood
        n_sims <- length(likelihoods)

        # Convert to AIC
        aic <- -2 * likelihoods
        valid_idx <- is.finite(aic) & !is.na(aic)

        if (sum(valid_idx) == 0) {
            warning("No valid likelihoods for continuous_all, using equal weights")
            weights <- rep(1.0 / n_sims, n_sims)
            return(weights)
        }

        # Calculate delta AIC from best
        best_aic <- min(aic[valid_idx])
        delta_aic <- aic - best_aic

        # Remove outliers using Tukey's method
        q1 <- quantile(delta_aic[valid_idx], 0.25, na.rm = TRUE)
        q3 <- quantile(delta_aic[valid_idx], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        outlier_threshold <- q3 + 1.5 * iqr

        reasonable_idx <- valid_idx & delta_aic <= outlier_threshold
        if (sum(reasonable_idx) < 10) {
            reasonable_idx <- valid_idx  # Keep all if too few
        }

        # Calculate adaptive effective AIC range to prevent weight underflow
        # Find effective_range such that worst simulation gets weight >= floor
        actual_range <- diff(range(delta_aic[reasonable_idx], na.rm = TRUE))

        if (actual_range < 1e-6) {
            # All models essentially identical
            weights <- rep(1.0 / n_sims, n_sims)
        } else {
            # Set floor: minimum weight for any simulation (prevents underflow)
            weight_floor <- 1e-15

            # Find worst delta_AIC in group
            max_delta_aic <- max(delta_aic[reasonable_idx], na.rm = TRUE)

            # Calculate effective_range needed to keep worst weight >= floor
            # From: exp(-max_delta_aic / (2*temp)) >= floor
            # Solve: effective_range = actual_range * max_delta_aic / (-log(floor))
            effective_range <- actual_range * max_delta_aic / (-log(weight_floor))
            temperature <- 0.5 * (effective_range / actual_range)

            if (verbose) {
                message("  Adaptive effective range calculation:")
                message("    Actual range: ", sprintf("%.1f", actual_range))
                message("    Max delta AIC: ", sprintf("%.1f", max_delta_aic))
                message("    Weight floor: ", sprintf("%.2e", weight_floor))
                message("    → Effective range: ", sprintf("%.1f", effective_range))
            }

            # Calculate Gibbs weights
            weights <- .calc_gibbs_weights(aic, temperature)
        }

        if (verbose) {
            eff_n <- calc_model_ess(weights, method = "kish")
            perplexity_ess <- calc_model_ess(weights, method = "perplexity")

            message("NPE Weight Strategy: continuous_all")
            message("  Using all simulations with continuous Gibbs weights")
            message("  Total simulations: ", n_sims)
            message("  Valid simulations: ", sum(valid_idx))
            message("  ESS (Kish) = ", sprintf("%.1f", eff_n))
            message("  ESS (Perplexity) = ", sprintf("%.1f", perplexity_ess))
            message("  Temperature = ", sprintf("%.4f", temperature))
            message("  Effective AIC range = ", effective_range)
        }

        return(weights)
    }

    # =========================================================================
    # Strategy: retained or best subset (binary or continuous)
    # =========================================================================

    # Identify subset mask
    if (grepl("retained", strategy)) {
        if (!"is_retained" %in% names(bfrs_results)) {
            stop("bfrs_results must contain 'is_retained' field for retained strategies")
        }
        subset_mask <- bfrs_results$is_retained
        subset_name <- "retained"
        effective_range <- 25  # Standard for retained subset
    } else {
        if (!"is_best_subset" %in% names(bfrs_results)) {
            stop("bfrs_results must contain 'is_best_subset' field for best strategies")
        }
        subset_mask <- bfrs_results$is_best_subset
        subset_name <- "best"
        effective_range <- 4  # Tighter for best subset
    }

    subset_size <- sum(subset_mask)
    n_sims <- nrow(bfrs_results)

    if (subset_size == 0) {
        stop("No simulations in ", subset_name, " subset. Check BFRS filtering.")
    }

    # Initialize weights (zero for simulations outside subset)
    weights <- rep(0, n_sims)

    # Calculate weights for subset
    if (grepl("binary", strategy)) {
        # Binary: equal weights within subset
        weights[subset_mask] <- 1.0 / subset_size
        weight_type <- "binary"

        if (verbose) {
            message("NPE Weight Strategy: ", strategy)
            message("  Using ", subset_name, " subset (", subset_size, " simulations)")
            message("  Weight type: binary (equal weights)")
            message("  Weight per simulation: ", sprintf("%.6f", 1.0 / subset_size))
            message("  ESS = ", subset_size, " (maximum for uniform weights)")
        }

    } else {
        # Continuous: recalculate Gibbs weights from likelihoods
        if (!"likelihood" %in% names(bfrs_results)) {
            stop("bfrs_results must contain 'likelihood' field for continuous strategies")
        }

        # Extract likelihoods for subset
        likelihoods <- bfrs_results$likelihood[subset_mask]

        # Convert to AIC
        aic <- -2 * likelihoods
        valid_idx <- is.finite(aic) & !is.na(aic)

        if (sum(valid_idx) == 0) {
            warning("No valid likelihoods in ", subset_name, " subset, using equal weights")
            weights[subset_mask] <- 1.0 / subset_size
        } else {
            # Calculate delta AIC from best
            best_aic <- min(aic[valid_idx])
            delta_aic <- aic - best_aic

            # Remove outliers using Tukey's method
            q1 <- quantile(delta_aic[valid_idx], 0.25, na.rm = TRUE)
            q3 <- quantile(delta_aic[valid_idx], 0.75, na.rm = TRUE)
            iqr <- q3 - q1
            outlier_threshold <- q3 + 1.5 * iqr

            reasonable_idx <- valid_idx & delta_aic <= outlier_threshold
            if (sum(reasonable_idx) < 10) {
                reasonable_idx <- valid_idx  # Keep all if too few
            }

            # Calculate adaptive effective AIC range to prevent weight underflow
            actual_range <- diff(range(delta_aic[reasonable_idx], na.rm = TRUE))

            if (actual_range < 1e-6) {
                # All models essentially identical
                weights[subset_mask] <- 1.0 / subset_size
                effective_range_adaptive <- NA
                temperature <- NA
                max_delta_aic <- NA
                weight_floor <- NA
            } else {
                # Set floor: minimum weight for any simulation (prevents underflow)
                weight_floor <- 1e-15

                # Find worst delta_AIC in subset
                max_delta_aic <- max(delta_aic[reasonable_idx], na.rm = TRUE)

                # Calculate effective_range needed to keep worst weight >= floor
                # From: exp(-max_delta_aic / (2*temp)) >= floor
                # Solve: effective_range = actual_range * max_delta_aic / (-log(floor))
                effective_range_adaptive <- actual_range * max_delta_aic / (-log(weight_floor))
                temperature <- 0.5 * (effective_range_adaptive / actual_range)

                # Calculate Gibbs weights for subset
                subset_weights <- .calc_gibbs_weights(aic, temperature)

                # Assign to full weight vector
                weights[subset_mask] <- subset_weights
            }
        }

        weight_type <- "continuous"

        if (verbose) {
            eff_n <- calc_model_ess(weights, method = "kish")
            perplexity_ess <- calc_model_ess(weights, method = "perplexity")

            message("NPE Weight Strategy: ", strategy)
            message("  Using ", subset_name, " subset (", subset_size, " simulations)")
            message("  Weight type: continuous (Gibbs weights)")
            if (!is.na(effective_range_adaptive)) {
                message("  Adaptive effective range calculation:")
                message("    Actual range: ", sprintf("%.1f", actual_range))
                message("    Max delta AIC: ", sprintf("%.1f", max_delta_aic))
                message("    Weight floor: ", sprintf("%.2e", weight_floor))
                message("    → Effective range: ", sprintf("%.1f", effective_range_adaptive))
                message("  Temperature = ", sprintf("%.4f", temperature))
            }
            message("  ESS (Kish) = ", sprintf("%.1f", eff_n))
            message("  ESS (Perplexity) = ", sprintf("%.1f", perplexity_ess))
        }
    }

    return(weights)
}

#' Calculate NPE Importance Weights (Deprecated)
#'
#' @description
#' DEPRECATED: Use get_npe_weights() for BFRS-based weights or direct assignment.
#' This function calculates importance weights from scratch, which can lead to
#' inconsistencies with BFRS weights.
#'
#' Calculates importance weights for NPE training. Can either compute new weights
#' from likelihood values or use pre-calculated BFRS weights.
#'
#' @param likelihoods Vector of likelihood values from BFRS (optional if using BFRS weights)
#' @param strategy Weight strategy: "binary", "continuous", "uniform",
#'   "continuous_retained", "binary_retained", "continuous_best", "binary_best"
#' @param bfrs_results BFRS results object containing weight_retained and weight_best
#'   (required for BFRS weight strategies)
#' @param temperature Temperature parameter for continuous weights (if NULL, computed automatically)
#' @param effective_range Effective AIC range for temperature calculation (default 25)
#' @param verbose Print weight statistics
#'
#' @return Vector of normalized weights
#' @export
calc_npe_weights <- function(
    likelihoods = NULL,
    strategy = "continuous",
    bfrs_results = NULL,
    temperature = NULL,
    effective_range = 30,
    verbose = FALSE
) {

    # Check if using BFRS weight strategies
    bfrs_strategies <- c("continuous_retained", "binary_retained",
                        "continuous_best", "binary_best")

    if (strategy %in% bfrs_strategies) {
        # Issue deprecation warning
        warning("calc_npe_weights() is deprecated for BFRS strategies. ",
                "Use get_npe_weights() instead:\n",
                "  get_npe_weights(bfrs_results, strategy = '", strategy, "')")

        # Using BFRS weights directly
        if (is.null(bfrs_results)) {
            stop("bfrs_results must be provided when using BFRS weight strategies")
        }

        # Extract appropriate weights
        if (grepl("retained", strategy)) {
            weights <- bfrs_results$weight_retained
            subset_name <- "retained"
            subset_size <- sum(bfrs_results$is_retained)
        } else {
            weights <- bfrs_results$weight_best
            subset_name <- "best"
            subset_size <- sum(bfrs_results$is_best_subset)
        }

        # Convert to binary if requested
        if (grepl("binary", strategy)) {
            # Binary: equal weights within subset, zero outside
            weights[weights > 0] <- 1 / sum(weights > 0)
            weight_type <- "binary"
        } else {
            # Continuous: use BFRS-calculated weights as-is
            weight_type <- "continuous"
        }

        if (verbose) {
            # Calculate ESS
            eff_n <- calc_model_ess(weights, method = "kish")
            perplexity_ess <- calc_model_ess(weights, method = "perplexity")

            message("  Strategy: ", strategy)
            message("  Using BFRS ", subset_name, " subset (", subset_size, " simulations)")
            message("  Weight type: ", weight_type)
            message("  ESS (Kish) = ", sprintf("%.1f", eff_n))
            message("  ESS (Perplexity) = ", sprintf("%.1f", perplexity_ess))

            # Show effective range used in BFRS calculation
            if (weight_type == "continuous") {
                if (subset_name == "retained") {
                    message("  BFRS effective AIC range: 25")
                } else {
                    message("  BFRS effective AIC range: 4")
                }
            }
        }

        return(weights)
    }

    # Original calculation methods (backward compatibility)
    if (is.null(likelihoods)) {
        stop("likelihoods must be provided for non-BFRS weight strategies")
    }

    n <- length(likelihoods)
    valid_idx <- is.finite(likelihoods) & !is.na(likelihoods)

    if (sum(valid_idx) == 0) {
        warning("No valid likelihoods, using uniform weights")
        return(rep(1/n, n))
    }

    if (strategy == "uniform") {
        weights <- rep(1/n, n)

    } else if (strategy == "binary") {
        # Binary weights: only best subset
        aic <- -2 * likelihoods
        aic[!valid_idx] <- Inf

        # Select best 20% by AIC
        threshold <- quantile(aic[valid_idx], 0.2)
        weights <- numeric(n)
        weights[aic <= threshold] <- 1
        weights <- weights / sum(weights)

        if (verbose) {
            # Calculate ESS using Kish method
            eff_n <- calc_model_ess(weights, method = "kish")
            message("  Binary weights: ", sum(weights > 0), " simulations selected")
            message("  ESS (Kish) = ", sprintf("%.1f", eff_n))
        }

    } else if (strategy == "continuous") {
        # Continuous weights based on likelihood
        aic <- -2 * likelihoods
        aic[!valid_idx] <- Inf

        # Remove outliers using Tukey's method
        q1 <- quantile(aic[valid_idx], 0.25)
        q3 <- quantile(aic[valid_idx], 0.75)
        iqr <- q3 - q1
        outlier_threshold <- q3 + 1.5 * iqr

        reasonable_idx <- valid_idx & aic <= outlier_threshold
        if (sum(reasonable_idx) < 10) {
            reasonable_idx <- valid_idx  # Keep all if too few
        }

        # Calculate temperature if not provided
        if (is.null(temperature)) {
            best_aic <- min(aic[valid_idx])
            delta_aic <- aic - best_aic
            actual_range <- diff(range(delta_aic[reasonable_idx]))

            # Use effective_range parameter (default 25)
            # Based on Burnham & Anderson (2002) AIC differences:
            # ΔAIC < 2: substantial support
            # ΔAIC 4-7: considerably less support
            # ΔAIC > 10: essentially no support
            # Default of 25 includes models with some meaningful support

            temperature <- 0.5 * (effective_range / actual_range)
        }

        # Calculate Gibbs weights
        weights <- .calc_gibbs_weights(aic, temperature)

        if (verbose) {
            # Calculate ESS using existing MOSAIC function
            eff_n <- calc_model_ess(weights, method = "perplexity")

            # Also calculate Kish ESS for comparison
            kish_ess <- calc_model_ess(weights, method = "kish")

            message("  Continuous weights: ESS = ", sprintf("%.1f", eff_n), " (perplexity)")
            message("  Kish ESS = ", sprintf("%.1f", kish_ess))
            message("  Temperature = ", sprintf("%.4f", temperature))
            message("  Effective AIC range = ", sprintf("%.1f", effective_range))
        }

    } else {
        warning("Unknown weight strategy: ", strategy, ". Using uniform.")
        weights <- rep(1/n, n)
    }

    return(weights)
}

#' Load NPE Model
#'
#' @description
#' Loads a saved NPE model from disk.
#'
#' @param model_dir Directory containing saved model
#' @param device Device to load model to ("cpu" or "cuda")
#'
#' @return NPE model object
#' @export
load_npe_model <- function(model_dir, device = "cpu") {

    torch <- reticulate::import("torch")

    # Load model file
    model_file <- file.path(model_dir, "npe_model.pt")
    if (!file.exists(model_file)) {
        stop("Model file not found: ", model_file)
    }

    # Load metadata
    metadata_file <- file.path(model_dir, "npe_metadata.json")
    if (file.exists(metadata_file)) {
        metadata <- jsonlite::read_json(metadata_file)
    } else {
        metadata <- list()
    }

    # Load PyTorch checkpoint
    checkpoint <- torch$load(model_file, map_location = device)

    # Reconstruct model architecture
    architecture <- checkpoint$architecture
    if (is.null(architecture)) {
        architecture <- metadata$architecture
    }

    # Create model structure
    n_params <- architecture$n_params
    n_obs <- architecture$n_timesteps * architecture$n_locations
    embedding_dim <- architecture$embedding_dim

    # Rebuild embedding network
    embedding_net <- .create_embedding_network(
        input_dim = n_obs,
        output_dim = embedding_dim,
        architecture = architecture,
        device = device
    )

    # Rebuild flow
    flow <- .create_normalizing_flow(
        n_params = n_params,
        context_dim = embedding_dim,
        architecture = architecture,
        device = device
    )

    # Combine into model
    model <- torch$nn$Sequential(embedding_net, flow)$to(device)

    # Load weights
    model$load_state_dict(checkpoint$model_state_dict)
    model$eval()

    return(list(
        model = model,
        architecture = architecture,
        normalization = checkpoint$normalization,
        device = device,
        metadata = metadata
    ))
}

# ==============================================================================
# Internal Utility Functions
# ==============================================================================

#' @keywords internal
.calc_gibbs_weights <- function(aic_values, temperature) {
    # Calculate Gibbs/Boltzmann weights

    valid_idx <- is.finite(aic_values)
    weights <- numeric(length(aic_values))

    if (sum(valid_idx) == 0) {
        return(rep(1/length(aic_values), length(aic_values)))
    }

    # Shift for numerical stability
    best_aic <- min(aic_values[valid_idx])
    delta_aic <- aic_values - best_aic

    # Calculate weights
    weights[valid_idx] <- exp(-delta_aic[valid_idx] / (2 * temperature))
    weights[!valid_idx] <- 0

    # Normalize
    weights <- weights / sum(weights)

    return(weights)
}

#' @keywords internal
.create_logger <- function(verbose = TRUE, log_file = NULL) {
    # Create simple logger object

    logger <- list(
        verbose = verbose,
        log_file = log_file,
        log = function(level, msg, ...) {
            if (verbose || level %in% c("ERROR", "WARNING")) {
                formatted_msg <- sprintf(msg, ...)
                timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                full_msg <- paste0("[", timestamp, "] [", level, "] ", formatted_msg)

                # Print to console
                if (level == "ERROR") {
                    stop(formatted_msg)
                } else if (level == "WARNING") {
                    warning(formatted_msg)
                } else if (verbose) {
                    message(formatted_msg)
                }

                # Write to file
                if (!is.null(log_file)) {
                    cat(full_msg, "\n", file = log_file, append = TRUE)
                }
            }
        }
    )

    return(logger)
}

#' @keywords internal
.validate_inputs <- function(X, y, weights, bounds) {
    # Validate inputs for NPE training

    # Check dimensions
    if (nrow(X) != nrow(y)) {
        stop("X and y must have same number of rows")
    }

    if (!is.null(weights) && length(weights) != nrow(X)) {
        stop("weights must have same length as number of samples")
    }

    if (nrow(bounds) != ncol(X)) {
        stop("bounds must have one row per parameter")
    }

    # Check for NaN/Inf
    if (any(!is.finite(X))) {
        warning("X contains non-finite values")
    }

    if (any(!is.finite(y))) {
        warning("y contains non-finite values")
    }

    # Check bounds
    for (i in 1:ncol(X)) {
        param_range <- range(X[, i], na.rm = TRUE)
        if (param_range[1] < bounds[i, 1] || param_range[2] > bounds[i, 2]) {
            warning("Parameter ", i, " has values outside specified bounds")
        }
    }

    return(TRUE)
}

#' @keywords internal
.logsumexp <- function(x) {
    # Numerically stable log-sum-exp

    if (length(x) == 0) return(-Inf)
    if (length(x) == 1) return(x)

    max_x <- max(x, na.rm = TRUE)
    if (!is.finite(max_x)) return(max_x)

    return(max_x + log(sum(exp(x - max_x), na.rm = TRUE)))
}

#' @keywords internal
.standardize_data <- function(X) {
    # Standardize data to zero mean and unit variance

    X_mean <- colMeans(X, na.rm = TRUE)
    X_sd <- apply(X, 2, sd, na.rm = TRUE)
    X_sd[X_sd == 0] <- 1  # Avoid division by zero

    X_standardized <- sweep(sweep(X, 2, X_mean, "-"), 2, X_sd, "/")

    return(list(
        X = X_standardized,
        mean = X_mean,
        sd = X_sd
    ))
}

#' @keywords internal
.destandardize_data <- function(X_standardized, mean, sd) {
    # Reverse standardization

    X <- sweep(sweep(X_standardized, 2, sd, "*"), 2, mean, "+")
    return(X)
}

#' @keywords internal
.format_time <- function(seconds) {
    # Format seconds into human-readable time

    if (seconds < 60) {
        return(sprintf("%.1f seconds", seconds))
    } else if (seconds < 3600) {
        return(sprintf("%.1f minutes", seconds / 60))
    } else {
        return(sprintf("%.1f hours", seconds / 3600))
    }
}

#' @keywords internal
.get_npe_config <- function(config_override = NULL) {
    # Get NPE configuration with defaults

    config <- list(
        # Training defaults
        n_epochs = 1000,
        batch_size = 256,
        learning_rate = 1e-3,
        validation_split = 0.2,
        early_stopping = TRUE,
        patience = 50,

        # Architecture defaults
        n_transforms = 10,
        n_bins = 12,
        hidden_features = 128,
        embedding_dim = 128,
        tcn_blocks = 4,

        # v5.2 features
        transform_ramping = TRUE,
        gradient_clip = 1.0,
        scheduler_patience = 15,
        autotune_one_shot = TRUE,

        # Guards
        large_j_threshold = 25,
        long_t_threshold = 700
    )

    # Apply overrides
    if (!is.null(config_override)) {
        for (key in names(config_override)) {
            config[[key]] <- config_override[[key]]
        }
    }

    return(config)
}

#' @keywords internal
.cleanup_python_resources <- function() {
    # Clean up Python/PyTorch resources

    tryCatch({
        torch <- reticulate::import("torch")
        if (torch$cuda$is_available()) {
            torch$cuda$empty_cache()
        }
        gc()  # R garbage collection
    }, error = function(e) {
        # Ignore cleanup errors
    })
}

#' Get NPE Parameter Bounds
#'
#' Extracts mathematically correct bounds for NPE training from prior distributions,
#' applying universal constraints based on parameter type. This ensures NPE training
#' respects theoretical parameter constraints rather than using empirical data ranges.
#'
#' @param param_names Character vector of parameter names
#' @param priors_file Path to priors.json file (optional)
#' @param safety_buffer Numeric buffer for numerical stability (default: 1e-10)
#' @param verbose Logical whether to print diagnostics
#' @return Data frame with columns: parameter, min, max
#' @export
get_npe_parameter_bounds <- function(
    param_names,
    priors_file = NULL,
    safety_buffer = 1e-10,
    verbose = FALSE
) {

    if (verbose) {
        message("Extracting NPE parameter bounds for ", length(param_names), " parameters")
    }

    # Initialize with infinite bounds
    bounds <- data.frame(
        parameter = param_names,
        min = -Inf,
        max = Inf,
        stringsAsFactors = FALSE
    )

    # Load priors if provided
    priors <- NULL
    if (!is.null(priors_file) && file.exists(priors_file)) {
        priors <- jsonlite::read_json(priors_file)
        if (verbose) {
            message("  Loaded priors from: ", basename(priors_file))
        }
    } else if (verbose) {
        message("  No priors file, using parameter-based rules only")
    }

    for (i in seq_along(param_names)) {
        param <- param_names[i]
        base_param <- gsub("_[A-Z]{3}$", "", param)

        # Get distribution info from priors if available
        dist_info <- NULL
        if (!is.null(priors)) {
            dist_info <- .get_prior_distribution_info(param, priors)
        }

        # Apply mathematical bounds based on distribution type
        # IMPORTANT: Only use mathematical support, not prior ranges
        # - Beta: [0, 1] is the mathematical support
        # - Gamma/Lognormal: [0, Inf) is the mathematical support
        # - Uniform/Normal/Truncnorm: Prior ranges are NOT mathematical constraints
        if (!is.null(dist_info)) {
            bounds[i, c("min", "max")] <- switch(dist_info$distribution,
                "beta" = c(0, 1),           # Mathematical support
                "gamma" = c(0, Inf),        # Mathematical support
                "lognormal" = c(0, Inf),    # Mathematical support (strictly >0, use 0 in practice)
                "uniform" = c(-Inf, Inf),   # Prior range is not a mathematical constraint
                "normal" = c(-Inf, Inf),    # Mathematical support
                "truncnorm" = c(-Inf, Inf), # Truncation is a prior choice, not mathematical
                c(-Inf, Inf)  # Default
            )
        }

        # Override with parameter-specific mathematical constraints
        # These take precedence over distribution bounds

        # Alpha parameters: [0, 1] (Beta distribution support)
        # NOTE: Changed from [0.01, 0.99] to avoid falsely rejecting valid samples
        if (base_param %in% c("alpha_1", "alpha_2")) {
            bounds$min[i] <- 0
            bounds$max[i] <- 1
        }

        # tau_i parameters are proportions [0, 1] (FIXED from original)
        else if (grepl("^tau_i", base_param)) {
            bounds$min[i] <- 0
            bounds$max[i] <- 1
        }

        # Proportions and probabilities: [0, 1]
        else if (grepl("^(prop_|phi_|rho|sigma|p_)", base_param)) {
            bounds$min[i] <- 0
            bounds$max[i] <- 1
        }

        # Rates and positive parameters: [0, Inf]
        # Includes: transmission rates, recovery rates, waning rates, mobility parameters, etc.
        else if (grepl("(rate|gamma|omega|epsilon|iota|eta|mu_j|decay|mobility)", base_param)) {
            bounds$min[i] <- 0
            bounds$max[i] <- Inf
        }

        # Counts must be non-negative [0, Inf]
        # Matches: S_j_initial, E_j_initial, I_j_initial, etc.
        else if (grepl("_j_initial", base_param)) {
            bounds$min[i] <- 0
            bounds$max[i] <- Inf
        }

        # Environmental/bacterial parameters: [0, Inf]
        # (bacterial concentrations and shedding rates must be non-negative)
        else if (base_param %in% c("zeta_1", "zeta_2", "kappa")) {
            bounds$min[i] <- 0
            bounds$max[i] <- Inf
        }

        # NOTE: No safety buffer clamping - keep infinite bounds as Inf
        # Rejection sampling will handle infinite bounds correctly
    }

    # Validate bounds
    invalid <- bounds$min >= bounds$max
    if (any(invalid)) {
        warning(sprintf("Invalid bounds for parameters: %s",
                       paste(param_names[invalid], collapse = ", ")))
        # Fix by adding small range
        bounds$max[invalid] <- bounds$min[invalid] + 1
    }

    if (verbose) {
        message("  Bounds set for ", nrow(bounds), " parameters")

        # Report bound types
        n_finite <- sum(is.finite(bounds$min) & is.finite(bounds$max))
        n_semi <- sum(is.finite(bounds$min) | is.finite(bounds$max)) - n_finite
        n_inf <- nrow(bounds) - n_finite - n_semi

        message("  Finite bounds: ", n_finite, ", Semi-infinite: ", n_semi, ", Infinite: ", n_inf)

        # Show bounds for key parameters if verbose
        key_params <- param_names[grepl("tau_i|phi_|alpha_|prop_", param_names)]
        if (length(key_params) > 0) {
            message("  Key parameter bounds:")
            for (key_param in head(key_params, 5)) {
                idx <- which(bounds$parameter == key_param)
                if (length(idx) > 0) {
                    message("    ", key_param, ": [", bounds$min[idx], ", ", bounds$max[idx], "]")
                }
            }
        }
    }

    return(bounds)
}

#' Helper function to extract distribution info from priors
#'
#' @param param_name Parameter name
#' @param priors Priors list from JSON
#' @return Distribution information or NULL
#' @keywords internal
.get_prior_distribution_info <- function(param_name, priors) {
    # Check if location-specific
    if (grepl("_[A-Z]{3}$", param_name)) {
        base_name <- gsub("_[A-Z]{3}$", "", param_name)
        location <- regmatches(param_name, regexpr("[A-Z]{3}$", param_name))

        # Handle seasonality parameter naming mismatch
        # Calibration uses: a_1_j, a_2_j, b_1_j, b_2_j
        # Priors use: a1, a2, b1, b2
        if (base_name == "a_1_j") {
            base_name <- "a1"
        } else if (base_name == "a_2_j") {
            base_name <- "a2"
        } else if (base_name == "b_1_j") {
            base_name <- "b1"
        } else if (base_name == "b_2_j") {
            base_name <- "b2"
        }

        # Try location-specific
        if (!is.null(priors$parameters_location[[base_name]]$location[[location]])) {
            return(priors$parameters_location[[base_name]]$location[[location]])
        }
    }

    # Try global
    base_name <- gsub("_[A-Z]{3}$", "", param_name)
    if (!is.null(priors$parameters_global[[base_name]])) {
        return(priors$parameters_global[[base_name]])
    }

    return(NULL)
}
