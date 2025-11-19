# =============================================================================
# NPE Architecture Specification - Consolidated Implementation
# =============================================================================

#' Calculate NPE Architecture Specification
#'
#' Calculates optimal NPE architecture based on problem dimensions, with support
#' for automatic tier selection, flow complexity, and protective guards.
#'
#' @param n_sims Number of simulations available for training
#' @param n_params Number of parameters to estimate
#' @param n_timesteps Number of timesteps in observations
#' @param n_locations Number of locations
#' @param tier Architecture tier: "auto", "minimal", "small", "medium", "large", "xlarge"
#' @param flow_complexity "simple", "medium", "complex", or "auto"
#' @param transform_ramping List controlling transform ramping based on parameter count
#' @param guards List with long_t and large_j guard configurations
#' @param training_params List with training configuration overrides
#' @param device Device type for optimization ("cuda", "mps", or "cpu")
#' @param verbose Print diagnostic information
#'
#' @return List containing complete NPE architecture specification
#'
#' @export
calc_npe_architecture <- function(
    n_sims,
    n_params,
    n_timesteps,
    n_locations,
    tier = "auto",
    flow_complexity = "auto",
    transform_ramping = list(
        enabled = TRUE,
        per_params = 25,
        start_at = 50,
        cap = 20
    ),
    guards = list(
        long_t = list(enabled = TRUE, threshold = 700, multiplier = 1.2),
        large_j = list(enabled = TRUE, threshold = 25, min_heads = 8)
    ),
    training_params = list(),
    device = "cpu",
    verbose = FALSE
) {

    # -------------------------------------------------------------------------
    # AUTO-SELECT TIER IF NEEDED
    # -------------------------------------------------------------------------

    if (tier == "auto") {
        tier <- .select_tier_auto(n_sims, n_params, n_timesteps, n_locations, verbose)
    }

    # -------------------------------------------------------------------------
    # BASE ARCHITECTURE DEFINITIONS
    # -------------------------------------------------------------------------

    specs <- list(
        minimal = list(
            embedding = list(
                embedding_dim = 32,
                tcn_blocks = 2,
                tcn_channels = 32,
                tcn_kernel_size = 3,
                tcn_dropout = 0.1
            ),
            pooling = list(
                attention_heads = 2,
                pooling_type = "attention"
            ),
            flow = list(
                hidden_features = 64,
                num_transforms = 3,
                num_bins = 8
            ),
            training = list(
                batch_size = 512,
                max_epochs = 50,
                validation_split = 0.2,
                n_ensembles = 1
            ),
            optimization = list(
                learning_rate = 5e-3,
                weight_decay = 1e-4,
                gradient_clip_value = 1.0
            )
        ),

        small = list(
            embedding = list(
                embedding_dim = 64,
                tcn_blocks = 3,
                tcn_channels = 64,
                tcn_kernel_size = 3,
                tcn_dropout = 0.15
            ),
            pooling = list(
                attention_heads = 4,
                pooling_type = "attention"
            ),
            flow = list(
                hidden_features = 128,
                num_transforms = 5,
                num_bins = 16
            ),
            training = list(
                batch_size = 256,
                max_epochs = 100,
                validation_split = 0.2,
                n_ensembles = 3
            ),
            optimization = list(
                learning_rate = 1e-3,
                weight_decay = 1e-4,
                gradient_clip_value = 1.0
            )
        ),

        medium = list(
            embedding = list(
                embedding_dim = 128,
                tcn_blocks = 4,
                tcn_channels = 128,
                tcn_kernel_size = 3,
                tcn_dropout = 0.2
            ),
            pooling = list(
                attention_heads = 8,
                pooling_type = "attention"
            ),
            flow = list(
                hidden_features = 256,
                num_transforms = 8,
                num_bins = 32
            ),
            training = list(
                batch_size = 128,
                max_epochs = 150,
                validation_split = 0.2,
                n_ensembles = 5
            ),
            optimization = list(
                learning_rate = 5e-4,
                weight_decay = 1e-4,
                gradient_clip_value = 1.0
            )
        ),

        large = list(
            embedding = list(
                embedding_dim = 256,
                tcn_blocks = 5,
                tcn_channels = 256,
                tcn_kernel_size = 5,
                tcn_dropout = 0.25
            ),
            pooling = list(
                attention_heads = 16,
                pooling_type = "attention"
            ),
            flow = list(
                hidden_features = 512,
                num_transforms = 12,
                num_bins = 64
            ),
            training = list(
                batch_size = 64,
                max_epochs = 200,
                validation_split = 0.2,
                n_ensembles = 5
            ),
            optimization = list(
                learning_rate = 1e-4,
                weight_decay = 1e-4,
                gradient_clip_value = 1.0
            )
        ),

        xlarge = list(
            embedding = list(
                embedding_dim = 512,
                tcn_blocks = 6,
                tcn_channels = 512,
                tcn_kernel_size = 7,
                tcn_dropout = 0.3
            ),
            pooling = list(
                attention_heads = 32,
                pooling_type = "attention"
            ),
            flow = list(
                hidden_features = 1024,
                num_transforms = 16,
                num_bins = 128
            ),
            training = list(
                batch_size = 32,
                max_epochs = 300,
                validation_split = 0.2,
                n_ensembles = 5
            ),
            optimization = list(
                learning_rate = 5e-5,
                weight_decay = 1e-4,
                gradient_clip_value = 0.5
            )
        )
    )

    # Select base specification
    spec <- specs[[tier]]
    spec$tier <- tier

    # -------------------------------------------------------------------------
    # APPLY FLOW COMPLEXITY
    # -------------------------------------------------------------------------

    if (flow_complexity == "auto") {
        # Auto-select based on parameter count
        if (n_params <= 10) {
            flow_complexity <- "simple"
        } else if (n_params <= 30) {
            flow_complexity <- "medium"
        } else {
            flow_complexity <- "complex"
        }

        if (verbose) {
            message(sprintf("Auto-selected flow complexity: %s (for %d parameters)",
                          flow_complexity, n_params))
        }
    }

    # Apply complexity adjustments
    complexity_multipliers <- list(
        simple = list(transforms = 0.75, bins = 0.5),
        medium = list(transforms = 1.0, bins = 1.0),
        complex = list(transforms = 1.25, bins = 1.5)
    )

    mult <- complexity_multipliers[[flow_complexity]]
    spec$flow$num_transforms <- round(spec$flow$num_transforms * mult$transforms)
    spec$flow$num_bins <- round(spec$flow$num_bins * mult$bins)

    # -------------------------------------------------------------------------
    # APPLY TRANSFORM RAMPING
    # -------------------------------------------------------------------------

    if (transform_ramping$enabled && n_params > 0) {
        # Calculate ramped transforms based on parameter count
        if (n_params < transform_ramping$start_at) {
            # Use default transforms for small parameter counts
            transforms_base <- spec$flow$num_transforms
        } else {
            # Ramp up transforms with parameter count
            params_above_threshold <- n_params - transform_ramping$start_at
            additional_transforms <- floor(params_above_threshold / transform_ramping$per_params)
            transforms_base <- spec$flow$num_transforms + additional_transforms

            # Apply cap
            transforms_base <- min(transforms_base, transform_ramping$cap)
        }

        spec$flow$num_transforms <- transforms_base

        if (verbose) {
            message(sprintf("Transform ramping: %d transforms for %d parameters",
                          spec$flow$num_transforms, n_params))
        }
    }

    # -------------------------------------------------------------------------
    # APPLY GUARDS
    # -------------------------------------------------------------------------

    # Long-T Guard: Adjust for long time series
    if (guards$long_t$enabled && n_timesteps > guards$long_t$threshold) {
        multiplier <- guards$long_t$multiplier

        # Increase TCN capacity for long sequences
        spec$embedding$tcn_blocks <- ceiling(spec$embedding$tcn_blocks * multiplier)
        spec$embedding$tcn_channels <- round(spec$embedding$tcn_channels * multiplier)

        # Ensure minimum blocks
        spec$embedding$tcn_blocks <- max(spec$embedding$tcn_blocks, 7)

        if (verbose) {
            message(sprintf("Long-T guard activated: %d timesteps > %d threshold",
                          n_timesteps, guards$long_t$threshold))
        }
    }

    # Large-J Guard: Adjust for many locations
    if (guards$large_j$enabled && n_locations > guards$large_j$threshold) {
        # Ensure sufficient attention heads for many locations
        min_heads <- guards$large_j$min_heads
        spec$pooling$attention_heads <- max(spec$pooling$attention_heads, min_heads)

        # Scale heads with locations if needed
        heads_per_location <- max(1, floor(n_locations / 10))
        suggested_heads <- min(32, heads_per_location * 4)
        spec$pooling$attention_heads <- max(spec$pooling$attention_heads, suggested_heads)

        if (verbose) {
            message(sprintf("Large-J guard activated: %d locations > %d threshold, using %d heads",
                          n_locations, guards$large_j$threshold, spec$pooling$attention_heads))
        }
    }

    # -------------------------------------------------------------------------
    # APPLY TRAINING PARAMETER OVERRIDES
    # -------------------------------------------------------------------------

    if (length(training_params) > 0) {
        for (category in names(training_params)) {
            if (category %in% names(spec$training)) {
                spec$training[[category]] <- training_params[[category]]
            } else if (category %in% names(spec$optimization)) {
                spec$optimization[[category]] <- training_params[[category]]
            }
        }
    }

    # -------------------------------------------------------------------------
    # DEVICE-SPECIFIC ADJUSTMENTS
    # -------------------------------------------------------------------------

    if (device == "mps") {
        # MPS (Apple Silicon) optimizations
        spec$training$batch_size <- min(spec$training$batch_size, 128)
        spec$optimization$gradient_clip_value <- 1.0
    } else if (device == "cpu") {
        # CPU optimizations
        spec$training$batch_size <- min(spec$training$batch_size, 64)
        spec$training$n_ensembles <- min(spec$training$n_ensembles, 3)
    }

    # -------------------------------------------------------------------------
    # ADD METADATA AND RATIONALE
    # -------------------------------------------------------------------------

    spec$rationale <- list(
        tier_selection = sprintf("Tier '%s' selected for %d sims, %d params, %d timesteps, %d locations",
                                tier, n_sims, n_params, n_timesteps, n_locations),
        flow_complexity = flow_complexity,
        transforms_ramp_applied = transform_ramping$enabled,
        guards_applied = list(
            long_t = guards$long_t$enabled && n_timesteps > guards$long_t$threshold,
            large_j = guards$large_j$enabled && n_locations > guards$large_j$threshold
        ),
        device_optimizations = device
    )

    # Calculate total model capacity
    spec$capacity <- .calculate_model_capacity(spec)

    if (verbose) {
        message("\nFinal NPE Architecture:")
        message(sprintf("  Tier: %s", spec$tier))
        message(sprintf("  Embedding: %d dim, %d TCN blocks, %d channels",
                      spec$embedding$embedding_dim, spec$embedding$tcn_blocks,
                      spec$embedding$tcn_channels))
        message(sprintf("  Flow: %d transforms, %d bins, %d hidden",
                      spec$flow$num_transforms, spec$flow$num_bins,
                      spec$flow$hidden_features))
        message(sprintf("  Training: batch=%d, epochs=%d, ensembles=%d",
                      spec$training$batch_size, spec$training$max_epochs,
                      spec$training$n_ensembles))
        message(sprintf("  Total capacity: %.2fM parameters", spec$capacity / 1e6))
    }

    return(spec)
}

#' Recommend NPE Specification
#'
#' Convenience wrapper that recommends optimal NPE architecture specification
#' based on problem characteristics.
#'
#' @param n_sims Number of simulations
#' @param n_params Number of parameters
#' @param n_timesteps Number of timesteps
#' @param n_locations Number of locations
#' @param ... Additional arguments passed to calc_npe_architecture
#'
#' @return NPE architecture specification
#'
#' @export
recommend_npe_spec <- function(n_sims, n_params, n_timesteps, n_locations, ...) {
    calc_npe_architecture(
        n_sims = n_sims,
        n_params = n_params,
        n_timesteps = n_timesteps,
        n_locations = n_locations,
        tier = "auto",
        ...
    )
}

# =============================================================================
# INTERNAL HELPER FUNCTIONS
# =============================================================================

#' Auto-select architecture tier based on problem size
#' @keywords internal
.select_tier_auto <- function(n_sims, n_params, n_timesteps, n_locations, verbose = FALSE) {

    # Calculate problem complexity score
    complexity_score <- 0

    # Simulation count contribution
    if (n_sims < 1000) {
        complexity_score <- complexity_score + 1
    } else if (n_sims < 5000) {
        complexity_score <- complexity_score + 2
    } else if (n_sims < 10000) {
        complexity_score <- complexity_score + 3
    } else if (n_sims < 50000) {
        complexity_score <- complexity_score + 4
    } else {
        complexity_score <- complexity_score + 5
    }

    # Parameter count contribution
    if (n_params < 10) {
        complexity_score <- complexity_score + 1
    } else if (n_params < 20) {
        complexity_score <- complexity_score + 2
    } else if (n_params < 50) {
        complexity_score <- complexity_score + 3
    } else if (n_params < 100) {
        complexity_score <- complexity_score + 4
    } else {
        complexity_score <- complexity_score + 5
    }

    # Observation complexity contribution
    obs_complexity <- n_timesteps * n_locations
    if (obs_complexity < 100) {
        complexity_score <- complexity_score + 1
    } else if (obs_complexity < 500) {
        complexity_score <- complexity_score + 2
    } else if (obs_complexity < 2000) {
        complexity_score <- complexity_score + 3
    } else if (obs_complexity < 10000) {
        complexity_score <- complexity_score + 4
    } else {
        complexity_score <- complexity_score + 5
    }

    # Map score to tier
    if (complexity_score <= 4) {
        tier <- "minimal"
    } else if (complexity_score <= 7) {
        tier <- "small"
    } else if (complexity_score <= 10) {
        tier <- "medium"
    } else if (complexity_score <= 13) {
        tier <- "large"
    } else {
        tier <- "xlarge"
    }

    if (verbose) {
        message(sprintf("Auto-selected tier: %s (complexity score: %d)",
                      tier, complexity_score))
    }

    return(tier)
}

#' Calculate total model capacity
#' @keywords internal
.calculate_model_capacity <- function(spec) {

    # Embedding network parameters
    embedding_params <- spec$embedding$embedding_dim * spec$embedding$tcn_channels *
                       spec$embedding$tcn_blocks * 2  # Approximate

    # Attention pooling parameters
    attention_params <- spec$pooling$attention_heads * spec$embedding$embedding_dim * 4

    # Flow network parameters
    flow_params <- spec$flow$num_transforms *
                  (spec$flow$hidden_features * spec$flow$num_bins * 4)

    total_params <- embedding_params + attention_params + flow_params

    # Account for ensembles
    total_params <- total_params * spec$training$n_ensembles

    return(round(total_params))
}

#' Apply auto-tune adjustments to architecture
#'
#' @param spec Original architecture specification
#' @param diagnostics Diagnostic results triggering auto-tune
#'
#' @return Adjusted architecture specification
#'
#' @export
apply_npe_autotune <- function(spec, diagnostics) {

    adjusted <- spec

    # Poor coverage suggests under-capacity
    if (diagnostics$coverage$mean_coverage < 0.7) {
        # Increase flow capacity
        adjusted$flow$num_transforms <- ceiling(spec$flow$num_transforms * 1.5)
        adjusted$flow$num_bins <- min(128, spec$flow$num_bins * 2)
        adjusted$flow$hidden_features <- ceiling(spec$flow$hidden_features * 1.25)
    }

    # SBC failure suggests poor calibration
    if (diagnostics$sbc$ks_pvalue < 0.05) {
        # Increase training duration and ensemble size
        adjusted$training$max_epochs <- ceiling(spec$training$max_epochs * 1.5)
        adjusted$training$n_ensembles <- min(10, spec$training$n_ensembles + 2)

        # Reduce learning rate for more stable training
        adjusted$optimization$learning_rate <- spec$optimization$learning_rate * 0.5
    }

    # High validation loss suggests overfitting
    if (diagnostics$validation$loss_ratio > 1.5) {
        # Increase regularization
        adjusted$embedding$tcn_dropout <- min(0.5, spec$embedding$tcn_dropout + 0.1)
        adjusted$optimization$weight_decay <- spec$optimization$weight_decay * 2

        # Reduce model capacity slightly
        adjusted$flow$num_transforms <- max(3, spec$flow$num_transforms - 1)
    }

    # Add auto-tune metadata
    adjusted$rationale$autotune_adjustments <- list(
        coverage_adjustment = diagnostics$coverage$mean_coverage < 0.7,
        sbc_adjustment = diagnostics$sbc$ks_pvalue < 0.05,
        overfitting_adjustment = diagnostics$validation$loss_ratio > 1.5,
        timestamp = Sys.time()
    )

    adjusted$rationale$autotune_applied <- TRUE

    return(adjusted)
}