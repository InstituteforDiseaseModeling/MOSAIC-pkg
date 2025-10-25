# =============================================================================
# NPE Architecture v5.2 - Enhanced with production-ready improvements
# =============================================================================

#' Calculate NPE Architecture Specification for MOSAIC Models (Enhanced v5.2)
#'
#' Dynamically determines optimal Neural Posterior Estimator architecture based on
#' data characteristics and complexity with advanced capacity controls, automatic
#' guards for extreme conditions, and rationale tracking.
#'
#' @param n_sims Integer, number of training simulations available. Used to determine
#'   data regime and select appropriate regularization.
#' @param n_params Integer, number of parameters to estimate. Affects network capacity
#'   and embedding dimensions.
#' @param n_timesteps Integer, number of time points in data series (optional).
#'   When provided, optimizes temporal processing components.
#' @param n_locations Integer, number of spatial locations (optional).
#'   Influences spatial embedding and attention mechanisms.
#' @param tier Character string to override automatic tier selection. Options:
#'   \code{"tiny", "small", "medium", "large", "xlarge"}. If NULL (default),
#'   automatically determined from data characteristics.
#' @param preset Character string for predefined architecture configurations.
#' @param flow_complexity Character. One of "auto", "high", "max".
#'   "high" adds +2 transforms and +2 bins to the auto choice.
#'   "max" sets transforms/bins to the upper safe caps for the tier.
#' @param transforms_ramp List controlling depth scaling with parameter count.
#'   Default ramp adds +1 transform for every 25 parameters beyond 50, capped at 20.
#' @param heads_min_for_largeJ Integer. Minimum attention heads when J >= heads_J_threshold.
#' @param heads_J_threshold Integer. Default 25. Threshold for large J guard.
#' @param t_long_threshold Integer. Default 700. When T > threshold, triggers long-T guard.
#' @param tcn_blocks_floor_longT Integer. Default 7. Minimum TCN blocks for long sequences.
#' @param tcn_longT_multiplier Numeric. Default 1.2. Channel multiplier for long sequences.
#' @param gradient_clip_value Numeric. Max gradient norm (L2). Default 1.0.
#' @param scheduler_patience Integer. Plateau scheduler patience (epochs). Default 15.
#' @param device Character string specifying computing device.
#' @param verbose Logical indicating whether to print architecture details.
#'
#' @return Named list containing complete NPE architecture specification with:
#'   \itemize{
#'     \item \code{tier} — Selected architecture tier
#'     \item \code{embedding} — Time series embedding configuration
#'     \item \code{flow} — Normalizing flow parameters
#'     \item \code{training} — Training hyperparameters
#'     \item \code{optimization} — Optimizer settings including gradient clipping
#'     \item \code{device} — Target computing device
#'     \item \code{rationale} — Detailed reasoning for all architecture decisions
#'   }
#'
#' @section Capacity Controls:
#' The transform ramp provides smooth scaling with parameter count, adding +1 transform
#' for every 25 parameters beyond 50. This prevents under-capacity in the critical
#' 50-100 parameter range where many models fall.
#'
#' @section Long T / Large J Guards:
#' Long sequences (T>700) trigger increased TCN blocks and channels to ensure adequate
#' receptive field. Many locations (J>=25) trigger attention head adjustments for
#' proper spatial modeling.
#'
#' @examples
#' \dontrun{
#' # High-complexity configuration
#' spec <- calc_npe_spec_v5_2(
#'   n_sims = 12000,
#'   n_params = 72,
#'   n_timesteps = 365,
#'   n_locations = 18,
#'   flow_complexity = "high"
#' )
#'
#' # Long sequence with auto-guards
#' spec_long <- calc_npe_spec_v5_2(
#'   n_sims = 10000,
#'   n_params = 50,
#'   n_timesteps = 900,  # Triggers long-T guard
#'   n_locations = 30     # Triggers large-J guard
#' )
#' }
#'
#' @export
calc_npe_spec_v5_2 <- function(
    n_sims,
    n_params,
    n_timesteps = NULL,
    n_locations = NULL,
    tier = NULL,
    preset = NULL,
    flow_complexity = "auto",
    transforms_ramp = list(enabled = TRUE, per_params = 25, start_at = 50, cap = 20),
    heads_min_for_largeJ = 8,
    heads_J_threshold = 25,
    t_long_threshold = 700,
    tcn_blocks_floor_longT = 7,
    tcn_longT_multiplier = 1.2,
    gradient_clip_value = 1.0,
    scheduler_patience = 15,
    device = "cpu",
    verbose = TRUE
) {

    # =============================================================================
    # INPUT VALIDATION
    # =============================================================================

    stopifnot(
        "n_sims must be positive" = n_sims > 0,
        "n_params must be positive" = n_params > 0,
        "flow_complexity must be auto/high/max" = flow_complexity %in% c("auto", "high", "max"),
        "gradient_clip_value must be positive" = gradient_clip_value > 0,
        "scheduler_patience must be positive integer" = scheduler_patience > 0
    )

    if (!is.null(n_timesteps)) {
        stopifnot("n_timesteps must be positive" = n_timesteps > 0)
    }

    if (!is.null(n_locations)) {
        stopifnot("n_locations must be positive" = n_locations > 0)
    }

    # Initialize rationale tracking
    rationale <- list(
        tier_decision = NULL,
        transforms_ramp_applied = FALSE,
        flow_complexity = flow_complexity,
        longT_guard = NULL,
        largeJ_guard = NULL,
        device_memory_adjustments = NULL
    )

    # Warnings for extreme values
    if (n_sims < 1000) {
        warning(sprintf("Low simulation count (%d) may lead to overfitting", n_sims))
        rationale$warnings <- c(rationale$warnings, "low_sim_count")
    }
    if (n_params > 500) {
        warning(sprintf("High parameter count (%d) may require custom tuning", n_params))
        rationale$warnings <- c(rationale$warnings, "high_param_count")
    }

    # =============================================================================
    # PRESET CONFIGURATIONS (if specified)
    # =============================================================================

    if (!is.null(preset)) {
        spec <- switch(preset,
            "epidemic_small" = list(
                tier = "small",
                n_timesteps = 52,
                n_locations = 4,
                embedding_dim = 256,
                tcn_blocks = 6,
                tcn_channels = 64,
                attention_heads = 2,
                hidden_features = 256,
                num_transforms = 8,
                num_bins = 10,
                batch_size = 256,
                val_split = 0.2
            ),
            "epidemic_large" = list(
                tier = "large",
                n_timesteps = 913,
                n_locations = 40,
                embedding_dim = 512,
                tcn_blocks = 9,
                tcn_channels = 192,
                attention_heads = 8,
                hidden_features = 512,
                num_transforms = 12,
                num_bins = 16,
                batch_size = 1024,
                val_split = 0.1
            ),
            "endemic" = list(
                tier = "medium",
                n_timesteps = 365,
                n_locations = 10,
                embedding_dim = 384,
                tcn_blocks = 8,
                tcn_channels = 128,
                attention_heads = 4,
                hidden_features = 384,
                num_transforms = 10,
                num_bins = 12,
                batch_size = 512,
                val_split = 0.1
            ),
            stop("Unknown preset: ", preset)
        )

        # Override with preset values
        if (!is.null(spec$n_timesteps)) n_timesteps <- spec$n_timesteps
        if (!is.null(spec$n_locations)) n_locations <- spec$n_locations
        tier <- spec$tier
        rationale$preset_used <- preset
    }

    # =============================================================================
    # TIER DETERMINATION
    # =============================================================================

    if (is.null(tier)) {
        # Automatic tier selection biased towards efficiency
        data_complexity <- n_sims * sqrt(n_params)

        tier <- if (data_complexity < 100000) {
            "tiny"
        } else if (data_complexity < 800000) {
            "small"
        } else if (data_complexity < 3000000) {
            "medium"
        } else if (data_complexity < 10000000) {
            "large"
        } else {
            "xlarge"
        }

        # Override for small datasets
        if (n_sims < 10000) tier <- "tiny"
        if (n_sims < 2000) tier <- "tiny"

        rationale$tier_decision <- list(
            data_complexity = data_complexity,
            n_sims = n_sims,
            n_params = n_params,
            selected_tier = tier
        )
    }

    # =============================================================================
    # BASE CONFIGURATION BY TIER
    # =============================================================================

    base_config <- switch(tier,
        tiny = list(
            embedding_dim = 96,
            hidden_features = 96,
            num_transforms = 4,
            num_bins = 6,
            tcn_channels = 24,
            tcn_blocks = 3,
            tcn_dropout = 0.15,
            attention_heads = 2,
            batch_size = 96,
            val_split = 0.3,
            weight_decay = 5e-4,
            n_ensembles = 1,
            max_epochs = 80,
            learning_rate = 5e-4
        ),
        small = list(
            embedding_dim = 192,
            hidden_features = 192,
            num_transforms = 6,
            num_bins = 8,
            tcn_channels = 48,
            tcn_blocks = 4,
            tcn_dropout = 0.1,
            attention_heads = 4,
            batch_size = 128,
            val_split = 0.2,
            weight_decay = 1e-4,
            n_ensembles = 1,
            max_epochs = 120,
            learning_rate = 3e-4
        ),
        medium = list(
            embedding_dim = 384,
            hidden_features = 384,
            num_transforms = 10,
            num_bins = 12,
            tcn_channels = 96,
            tcn_blocks = 6,
            tcn_dropout = 0.1,
            attention_heads = 4,
            batch_size = 512,
            val_split = 0.15,
            weight_decay = 1e-5,
            n_ensembles = 1,
            max_epochs = 200,
            learning_rate = 2e-4
        ),
        large = list(
            embedding_dim = 512,
            hidden_features = 512,
            num_transforms = 12,
            num_bins = 16,
            tcn_channels = 128,
            tcn_blocks = 7,
            tcn_dropout = 0.1,
            attention_heads = 8,
            batch_size = 1024,
            val_split = 0.1,
            weight_decay = 1e-5,
            n_ensembles = 3,
            max_epochs = 250,
            learning_rate = 1e-4
        ),
        xlarge = list(
            embedding_dim = 768,
            hidden_features = 768,
            num_transforms = 15,
            num_bins = 20,
            tcn_channels = 192,
            tcn_blocks = 8,
            tcn_dropout = 0.15,
            attention_heads = 8,
            batch_size = 2048,
            val_split = 0.1,
            weight_decay = 5e-6,
            n_ensembles = 5,
            max_epochs = 300,
            learning_rate = 5e-5
        )
    )

    # Extract base configuration
    embedding_dim <- base_config$embedding_dim
    hidden_features <- base_config$hidden_features
    num_transforms <- base_config$num_transforms
    num_bins <- base_config$num_bins
    tcn_channels <- base_config$tcn_channels
    tcn_blocks <- base_config$tcn_blocks
    tcn_dropout <- base_config$tcn_dropout
    attention_heads <- base_config$attention_heads
    batch_size <- base_config$batch_size
    val_split <- base_config$val_split
    weight_decay <- base_config$weight_decay
    n_ensembles <- base_config$n_ensembles
    max_epochs <- base_config$max_epochs
    learning_rate <- base_config$learning_rate

    # =============================================================================
    # TRANSFORM RAMPING AND FLOW COMPLEXITY
    # =============================================================================

    # Store original values for rationale
    original_transforms <- num_transforms
    original_bins <- num_bins

    # Apply smooth transform ramp based on parameter count
    if (!is.null(n_params) && transforms_ramp$enabled) {
        if (n_params > transforms_ramp$start_at) {
            extra <- ceiling((n_params - transforms_ramp$start_at) / transforms_ramp$per_params)
            num_transforms <- min(num_transforms + extra, transforms_ramp$cap)

            if (extra > 0) {
                rationale$transforms_ramp_applied <- TRUE
                rationale$transform_ramp_details <- list(
                    n_params = n_params,
                    base_transforms = original_transforms,
                    extra_transforms = extra,
                    final_transforms = num_transforms
                )
            }
        }
    }

    # Apply flow complexity override
    if (flow_complexity %in% c("high", "max")) {
        if (flow_complexity == "high") {
            num_transforms <- min(num_transforms + 2L, 20L)
            num_bins <- min(num_bins + 2L, 32L)
            rationale$flow_complexity_adjustment <- "high: +2 transforms, +2 bins"
        } else { # "max"
            # Set to tier-appropriate maximum
            tier_max_transforms <- switch(tier,
                tiny = 8L, small = 12L, medium = 16L, large = 18L, xlarge = 20L, 20L
            )
            tier_max_bins <- switch(tier,
                tiny = 12L, small = 16L, medium = 24L, large = 28L, xlarge = 32L, 32L
            )
            num_transforms <- tier_max_transforms
            num_bins <- tier_max_bins
            rationale$flow_complexity_adjustment <- sprintf("max: %d transforms, %d bins",
                                                           num_transforms, num_bins)
        }
    }

    # =============================================================================
    # LONG-T GUARD
    # =============================================================================

    if (!is.null(n_timesteps) && n_timesteps > t_long_threshold) {
        # Ensure adequate TCN blocks for long sequences
        tcn_blocks <- max(tcn_blocks, tcn_blocks_floor_longT)

        # Scale channels for increased capacity
        tcn_channels_new <- ceiling(tcn_channels * tcn_longT_multiplier)

        # Ensure GroupNorm compatibility
        tcn_channels <- .ensure_groupnorm_divisibility(tcn_channels_new)

        rationale$longT_guard <- list(
            T = n_timesteps,
            triggered = TRUE,
            blocks = tcn_blocks,
            original_channels = base_config$tcn_channels,
            scaled_channels = tcn_channels,
            multiplier = tcn_longT_multiplier
        )

        if (verbose) {
            message(sprintf("Long-T guard activated: T=%d, blocks=%d, channels=%d",
                          n_timesteps, tcn_blocks, tcn_channels))
        }
    }

    # =============================================================================
    # LARGE-J GUARD
    # =============================================================================

    if (!is.null(n_locations) && n_locations >= heads_J_threshold) {
        # Ensure adequate attention heads for many locations
        attention_heads <- max(attention_heads, heads_min_for_largeJ)

        # Ensure divisibility: channels must be divisible by heads
        if (tcn_channels %% attention_heads != 0) {
            # Bump channels to next multiple of heads
            tcn_channels <- attention_heads * ceiling(tcn_channels / attention_heads)

            # Re-ensure GroupNorm compatibility
            tcn_channels <- .ensure_groupnorm_divisibility(tcn_channels)
        }

        # Also check if we need to reduce heads to maintain divisibility
        while (tcn_channels %% attention_heads != 0 && attention_heads > 1) {
            attention_heads <- attention_heads - 1
        }

        rationale$largeJ_guard <- list(
            J = n_locations,
            triggered = TRUE,
            heads = attention_heads,
            channels = tcn_channels,
            adjustment_reason = "channels_bumped_for_divisibility"
        )

        if (verbose) {
            message(sprintf("Large-J guard activated: J=%d, heads=%d, channels=%d",
                          n_locations, attention_heads, tcn_channels))
        }
    }

    # =============================================================================
    # MEMORY AND DEVICE ADJUSTMENTS
    # =============================================================================

    # Adjust batch size based on device
    if (device == "mps" || device == "cpu") {
        # Reduce batch size on memory-limited devices
        batch_size <- min(batch_size, 256)
        max_epochs <- min(max_epochs, 100)

        rationale$device_memory_adjustments <- list(
            device = device,
            batch_size_adjusted = TRUE,
            max_epochs_reduced = TRUE
        )
    }

    # Reduce for very high parameter models on any device
    if (n_params > 200 && device != "cuda") {
        batch_size <- min(batch_size, 128)
        rationale$high_param_batch_reduction <- TRUE
    }

    # =============================================================================
    # BUILD FINAL SPECIFICATION
    # =============================================================================

    spec <- list(
        tier = tier,

        embedding = list(
            embedding_dim = as.integer(embedding_dim),
            tcn_blocks = as.integer(tcn_blocks),
            tcn_channels = as.integer(tcn_channels),
            tcn_kernel_size = 3L,
            tcn_dropout = tcn_dropout,
            use_batch_norm = FALSE,
            use_layer_norm = TRUE
        ),

        pooling = list(
            pooling_type = "attention",
            attention_heads = as.integer(attention_heads),
            attention_dropout = 0.1
        ),

        flow = list(
            hidden_features = as.integer(hidden_features),
            num_transforms = as.integer(num_transforms),
            num_bins = as.integer(num_bins),
            tail_bound = 10.0,
            dropout = 0.0,
            use_batch_norm = FALSE
        ),

        training = list(
            batch_size = as.integer(batch_size),
            validation_split = val_split,
            max_epochs = as.integer(max_epochs),
            early_stopping_patience = 30L,
            n_ensembles = as.integer(n_ensembles)
        ),

        optimization = list(
            learning_rate = learning_rate,
            weight_decay = weight_decay,
            gradient_clip_value = gradient_clip_value,
            scheduler_patience = as.integer(scheduler_patience),
            scheduler_factor = 0.5,
            scheduler_mode = "min"
        ),

        preprocessing = list(
            standardize_inputs = TRUE,
            use_log1p = FALSE
        ),

        device = list(
            device_type = device,
            use_amp = (device == "cuda"),
            pin_memory = (device == "cuda")
        ),

        architecture_type = sprintf("%s_tier_%dt_%dj_%dp",
                                   tier,
                                   ifelse(is.null(n_timesteps), 0, n_timesteps),
                                   ifelse(is.null(n_locations), 0, n_locations),
                                   n_params),

        rationale = rationale,

        version = "5.2",

        created_at = Sys.time()
    )

    # =============================================================================
    # VERBOSE OUTPUT
    # =============================================================================

    if (verbose) {
        cat("\n=== NPE Architecture Specification v5.2 ===\n")
        cat(sprintf("Tier: %s\n", tier))
        cat(sprintf("Parameters: %d, Simulations: %d\n", n_params, n_sims))

        if (!is.null(n_timesteps)) cat(sprintf("Timesteps: %d\n", n_timesteps))
        if (!is.null(n_locations)) cat(sprintf("Locations: %d\n", n_locations))

        cat("\n--- Flow Architecture ---\n")
        cat(sprintf("Transforms: %d", num_transforms))
        if (rationale$transforms_ramp_applied) {
            cat(sprintf(" (ramped from %d)", original_transforms))
        }
        cat("\n")

        cat(sprintf("Bins: %d", num_bins))
        if (flow_complexity != "auto") {
            cat(sprintf(" (%s complexity)", flow_complexity))
        }
        cat("\n")

        cat(sprintf("Hidden features: %d\n", hidden_features))

        cat("\n--- Embedding Network ---\n")
        cat(sprintf("TCN blocks: %d, channels: %d\n", tcn_blocks, tcn_channels))
        cat(sprintf("Attention heads: %d\n", attention_heads))
        cat(sprintf("Embedding dim: %d\n", embedding_dim))

        if (!is.null(rationale$longT_guard) && rationale$longT_guard$triggered) {
            cat("*Long-T guard activated\n")
        }
        if (!is.null(rationale$largeJ_guard) && rationale$largeJ_guard$triggered) {
            cat("*Large-J guard activated\n")
        }

        cat("\n--- Training Setup ---\n")
        cat(sprintf("Batch size: %d\n", batch_size))
        cat(sprintf("Max epochs: %d\n", max_epochs))
        cat(sprintf("Learning rate: %.2e\n", learning_rate))
        cat(sprintf("Gradient clip: %.1f\n", gradient_clip_value))
        cat(sprintf("Scheduler patience: %d\n", scheduler_patience))
        cat(sprintf("Ensembles: %d\n", n_ensembles))
        cat("==============================\n\n")
    }

    return(spec)
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Ensure TCN channels are divisible by valid GroupNorm group size
#' @keywords internal
.ensure_groupnorm_divisibility <- function(channels) {
    valid_groups <- c(1, 2, 4, 8, 16, 32)

    # Try to find a valid divisor, preferring larger groups
    for (g in rev(valid_groups)) {
        if (channels %% g == 0 && g <= channels) {
            return(channels)
        }
    }

    # If no valid divisor, bump to next valid multiple
    for (g in c(8, 4, 2)) {
        if (g <= channels) {
            return(g * ceiling(channels / g))
        }
    }

    # Fallback to nearest even number
    return(2 * ceiling(channels / 2))
}

#' Recommend NPE specification without training
#'
#' @description Returns the spec and a one-line human message explaining the choice.
#' @inheritParams calc_npe_spec_v5_2
#' @return List with spec and recommendation message
#' @export
recommend_npe_spec <- function(n_sims, n_params, n_timesteps = NULL,
                              n_locations = NULL, flow_complexity = "auto") {

    spec <- calc_npe_spec_v5_2(
        n_sims = n_sims,
        n_params = n_params,
        n_timesteps = n_timesteps,
        n_locations = n_locations,
        flow_complexity = flow_complexity,
        verbose = FALSE
    )

    # Generate recommendation message
    sim_per_param <- n_sims / n_params

    if (sim_per_param < 100) {
        message <- sprintf(
            "WARNING: Very low data (%d sims/param). Using %s tier with %d transforms. Consider more simulations.",
            round(sim_per_param), spec$tier, spec$flow$num_transforms
        )
    } else if (sim_per_param < 500) {
        message <- sprintf(
            "Low data regime (%d sims/param). Using %s tier with %d transforms. Consider ensemble or more data.",
            round(sim_per_param), spec$tier, spec$flow$num_transforms
        )
    } else if (sim_per_param < 2000) {
        message <- sprintf(
            "Medium data regime (%d sims/param). Using %s tier with %d transforms. Configuration should work well.",
            round(sim_per_param), spec$tier, spec$flow$num_transforms
        )
    } else {
        message <- sprintf(
            "High data regime (%d sims/param). Using %s tier with %d transforms. Excellent for learning.",
            round(sim_per_param), spec$tier, spec$flow$num_transforms
        )
    }

    # Add guard notifications
    if (!is.null(spec$rationale$longT_guard) && spec$rationale$longT_guard$triggered) {
        message <- paste(message, "Long-T guard activated.")
    }
    if (!is.null(spec$rationale$largeJ_guard) && spec$rationale$largeJ_guard$triggered) {
        message <- paste(message, "Large-J guard activated.")
    }

    return(list(
        spec = spec,
        recommendation = message
    ))
}