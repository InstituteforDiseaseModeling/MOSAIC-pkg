# =============================================================================
# MOSAIC: run_mosaic_helpers.R
# Internal helper functions for run_mosaic()
# =============================================================================

# All functions prefixed with . (not exported - internal use only)

# Constants for validation
.MOSAIC_MAX_ITERATIONS <- 100L
.MOSAIC_MIN_BATCH_SIZE <- 10L
.MOSAIC_MAX_BATCH_SIZE <- 10000L
.MOSAIC_MIN_SIMULATIONS <- 100L
.MOSAIC_MAX_SIMULATIONS <- 1000000L

# =============================================================================
# VALIDATION
# =============================================================================

#' Validate and Merge Control Settings
#' @noRd
.mosaic_validate_and_merge_control <- function(control) {
  def <- mosaic_control_defaults()

  # Deep merge user control into defaults
  for (nm in names(control)) {
    if (is.list(control[[nm]]) && nm %in% names(def) && is.list(def[[nm]])) {
      # Nested list: merge recursively
      for (sub_nm in names(control[[nm]])) {
        def[[nm]][[sub_nm]] <- control[[nm]][[sub_nm]]
      }
    } else {
      # Top-level: direct replacement
      def[[nm]] <- control[[nm]]
    }
  }

  # TYPE VALIDATION
  stopifnot(
    "parallel$enable must be logical" =
      is.logical(def$parallel$enable) && length(def$parallel$enable) == 1,
    "parallel$n_cores must be positive integer" =
      is.numeric(def$parallel$n_cores) && def$parallel$n_cores > 0,
    "parallel$type must be 'PSOCK' or 'FORK'" =
      def$parallel$type %in% c("PSOCK", "FORK"),
    "calibration$max_simulations must be positive integer" =
      is.numeric(def$calibration$max_simulations) && def$calibration$max_simulations > 0,
    "calibration$batch_size must be positive integer" =
      is.numeric(def$calibration$batch_size) && def$calibration$batch_size > 0,
    "calibration$min_batches must be positive integer" =
      is.numeric(def$calibration$min_batches) && def$calibration$min_batches > 0,
    "calibration$max_batches must be positive integer" =
      is.numeric(def$calibration$max_batches) && def$calibration$max_batches > 0,
    "calibration$target_r2 must be in [0, 1]" =
      is.numeric(def$calibration$target_r2) &&
      def$calibration$target_r2 >= 0 && def$calibration$target_r2 <= 1,
    "targets$ESS_param must be positive" =
      is.numeric(def$targets$ESS_param) && def$targets$ESS_param > 0,
    "targets$ESS_param_prop must be in [0, 1]" =
      is.numeric(def$targets$ESS_param_prop) &&
      def$targets$ESS_param_prop >= 0 && def$targets$ESS_param_prop <= 1,
    "targets$A_best must be in [0, 1]" =
      is.numeric(def$targets$A_best) &&
      def$targets$A_best >= 0 && def$targets$A_best <= 1,
    "targets$CVw_best must be positive" =
      is.numeric(def$targets$CVw_best) && def$targets$CVw_best > 0,
    "targets$percentile_max must be in (0, 100]" =
      is.numeric(def$targets$percentile_max) &&
      def$targets$percentile_max > 0 && def$targets$percentile_max <= 100
  )

  # SEMANTIC VALIDATION (reasonable bounds and logical consistency)

  # Check n_iterations is reasonable
  if (!is.null(def$calibration$n_iterations)) {
    if (def$calibration$n_iterations > .MOSAIC_MAX_ITERATIONS) {
      stop("calibration$n_iterations (", def$calibration$n_iterations,
           ") exceeds maximum (", .MOSAIC_MAX_ITERATIONS, ")",
           call. = FALSE)
    }
  }

  # Check batch_size is reasonable
  if (def$calibration$batch_size < .MOSAIC_MIN_BATCH_SIZE ||
      def$calibration$batch_size > .MOSAIC_MAX_BATCH_SIZE) {
    stop("calibration$batch_size (", def$calibration$batch_size,
         ") must be between ", .MOSAIC_MIN_BATCH_SIZE, " and ",
         .MOSAIC_MAX_BATCH_SIZE, call. = FALSE)
  }

  # Check max_simulations is reasonable
  if (def$calibration$max_simulations < .MOSAIC_MIN_SIMULATIONS ||
      def$calibration$max_simulations > .MOSAIC_MAX_SIMULATIONS) {
    stop("calibration$max_simulations (", def$calibration$max_simulations,
         ") must be between ", .MOSAIC_MIN_SIMULATIONS, " and ",
         .MOSAIC_MAX_SIMULATIONS, call. = FALSE)
  }

  # Check batch_size < max_simulations
  if (def$calibration$batch_size >= def$calibration$max_simulations) {
    stop("calibration$batch_size (", def$calibration$batch_size,
         ") must be less than calibration$max_simulations (",
         def$calibration$max_simulations, ")", call. = FALSE)
  }

  # LOGICAL CONSISTENCY
  if (def$calibration$min_batches > def$calibration$max_batches) {
    stop("calibration$min_batches (", def$calibration$min_batches,
         ") must be <= calibration$max_batches (", def$calibration$max_batches, ")",
         call. = FALSE)
  }

  # Validate fine-tuning batch sizes
  required_tiers <- c("massive", "large", "standard", "precision", "final")
  missing_tiers <- setdiff(required_tiers, names(def$fine_tuning$batch_sizes))
  if (length(missing_tiers) > 0) {
    stop("fine_tuning$batch_sizes missing tiers: ",
         paste(missing_tiers, collapse = ", "), call. = FALSE)
  }

  # IO VALIDATION
  .mosaic_validate_io(def$io)

  def
}

#' Validate I/O Settings
#' @noRd
.mosaic_validate_io <- function(io) {
  # Format check
  if (!io$format %in% c("csv", "parquet")) {
    stop("io$format must be 'csv' or 'parquet', got: ", io$format, call. = FALSE)
  }

  # Compression check
  valid_compressions <- c("none", "uncompressed", "snappy", "gzip", "lz4", "zstd")
  if (!io$compression %in% valid_compressions) {
    stop("io$compression must be one of: ",
         paste(valid_compressions, collapse = ", "),
         "\nGot: ", io$compression, call. = FALSE)
  }

  # Level check for zstd
  if (io$compression == "zstd" && !is.null(io$compression_level)) {
    if (!is.numeric(io$compression_level) ||
        io$compression_level < 1 ||
        io$compression_level > 22) {
      stop("io$compression_level for zstd must be integer 1-22, got: ",
           io$compression_level, call. = FALSE)
    }
  }

  # Warning for suboptimal choices
  if (io$format == "csv" && io$compression == "none") {
    warning("CSV without compression is 2-3x larger and 6-30x slower than parquet.\n",
            "Consider using mosaic_io_presets('default') for production runs.",
            call. = FALSE, immediate. = TRUE)
  }

  invisible(TRUE)
}

#' Validate Sampling Arguments
#' @noRd
.mosaic_validate_sampling_args <- function(sampling_args) {
  if (!is.list(sampling_args)) {
    stop("sampling_args must be a list", call. = FALSE)
  }

  # Check that all elements are logical
  non_logical <- names(sampling_args)[!sapply(sampling_args, is.logical)]
  if (length(non_logical) > 0) {
    warning("Non-logical values in sampling_args: ",
            paste(non_logical, collapse = ", "),
            call. = FALSE)
  }

  sampling_args
}

#' Validate Config
#' @noRd
.mosaic_validate_config <- function(config, iso_code) {
  if (!is.list(config) || length(config) == 0) {
    stop("Config must be a non-empty list", call. = FALSE)
  }

  # Check required fields
  required_fields <- c("location_name", "reported_cases", "reported_deaths")
  missing <- setdiff(required_fields, names(config))
  if (length(missing) > 0) {
    stop("Config missing required fields: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # Optionally warn if config locations don't match iso_code
  if (!is.null(config$location_name)) {
    config_locations <- config$location_name
    if (!all(iso_code %in% config_locations)) {
      warning("iso_code contains locations not in config: ",
              paste(setdiff(iso_code, config_locations), collapse = ", "),
              "\nThis may be intentional if using custom config.",
              call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Validate Priors
#' @noRd
.mosaic_validate_priors <- function(priors, config) {
  if (!is.list(priors) || length(priors) == 0) {
    stop("Priors must be a non-empty list", call. = FALSE)
  }

  # Skip known metadata/structural fields that don't need 'distribution'
  metadata_fields <- c("metadata", "parameters_global", "parameters_location",
                       "simulation", "reporting", "climate", "vaccination")

  # Check that each prior has distribution field
  for (param_name in names(priors)) {
    # Skip metadata fields and non-list items
    if (!is.list(priors[[param_name]]) || param_name %in% metadata_fields) next

    if (!"distribution" %in% names(priors[[param_name]])) {
      warning("Prior for '", param_name, "' missing 'distribution' field",
              call. = FALSE)
    }
  }

  invisible(TRUE)
}

# =============================================================================
# I/O FUNCTIONS
# =============================================================================

#' Write Parquet with Compression (NFS-Safe)
#'
#' Uses atomic write with sync for network filesystem safety.
#'
#' @noRd
.mosaic_write_parquet <- function(df, path, io) {
  # Check disk space (estimate ~1KB per row)
  estimated_mb <- (nrow(df) * 1024) / (1024^2)
  if (!.mosaic_check_disk_space(dirname(path), required_mb = estimated_mb * 2)) {
    stop("Insufficient disk space for parquet write: ", path, call. = FALSE)
  }

  # Define write function
  write_func <- function(data, file) {
    if (io$compression %in% c("none", "uncompressed")) {
      arrow::write_parquet(data, file, compression = "uncompressed")
    } else if (is.null(io$compression_level) || io$compression != "zstd") {
      arrow::write_parquet(data, file, compression = io$compression)
    } else {
      arrow::write_parquet(data, file,
        compression = io$compression,
        compression_level = io$compression_level)
    }
  }

  # Use NFS-safe atomic write
  .mosaic_atomic_write(df, path, write_func)
}

#' Write JSON with Atomic Rename (NFS-Safe)
#' @noRd
.mosaic_write_json <- function(obj, path, io) {
  # Check disk space (JSON is usually small, 10MB buffer)
  if (!.mosaic_check_disk_space(dirname(path), required_mb = 10)) {
    stop("Insufficient disk space for JSON write: ", path, call. = FALSE)
  }

  # Define write function
  write_func <- function(data, file) {
    jsonlite::write_json(data, file, pretty = TRUE, auto_unbox = TRUE)
  }

  # Use NFS-safe atomic write
  .mosaic_atomic_write(obj, path, write_func)
}

# =============================================================================
# RESULT LOADING AND COMBINING
# =============================================================================

#' Load and Combine Simulation Results
#'
#' Loads individual sim_*.parquet files and combines into single data frame.
#' Two methods available: streaming (default, memory-safe) or rbind (legacy).
#'
#' @param dir_params Directory containing sim_*.parquet files
#' @param method Character. "streaming" (default) uses arrow::open_dataset,
#'   "rbind" loads all files into memory. Streaming is recommended for large runs.
#' @param verbose Logical. Print progress messages
#' @return Data frame with combined simulation results
#' @noRd
.mosaic_load_and_combine_results <- function(dir_params,
                                             method = c("streaming", "rbind"),
                                             verbose = TRUE) {

  method <- match.arg(method)

  # Find simulation files
  files <- list.files(dir_params, pattern = "^sim_.*\\.parquet$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No simulation results to process in: ", dir_params, call. = FALSE)
  }

  if (verbose) {
    total_size_mb <- sum(file.size(files)) / 1024^2
    log_msg("Loading %d simulation files (%.1f MB on disk)", length(files), total_size_mb)
    log_msg("Method: %s", method)
  }

  load_start <- Sys.time()

  # Choose loading strategy
  results <- switch(method,
    streaming = {
      # Arrow streaming: memory-safe, works for any dataset size
      arrow::open_dataset(dir_params, format = "parquet") %>%
        dplyr::collect() %>%
        as.data.frame()
    },

    rbind = {
      # Legacy approach: load all into memory then combine
      # Faster for small datasets, but can OOM for large runs
      if (verbose && length(files) > 10000) {
        warning("Using rbind method with ", length(files),
                " files may cause memory issues. Consider method='streaming'",
                call. = FALSE, immediate. = TRUE)
      }

      df_list <- lapply(files, arrow::read_parquet)

      # Use data.table if available (much faster than do.call(rbind, ...))
      if (requireNamespace("data.table", quietly = TRUE)) {
        results <- data.table::rbindlist(df_list, fill = TRUE)
        as.data.frame(results)
      } else {
        do.call(rbind, df_list)
      }
    }
  )

  if (verbose) {
    load_time <- difftime(Sys.time(), load_start, units = "secs")
    log_msg("Loaded %d rows × %d columns in %.1f seconds",
            nrow(results), ncol(results), as.numeric(load_time))

    # Memory usage info
    size_mb <- as.numeric(object.size(results)) / 1024^2
    log_msg("Results in memory: %.1f MB", size_mb)
  }

  results
}

# =============================================================================
# SIMULATION FUNCTIONS
# =============================================================================

#' Normalize n_sims Argument
#'
#' @note "algo" mode is deprecated alias for "auto"
#' @noRd
.mosaic_normalize_n_sims <- function(n_sims) {
  # Handle NULL as auto mode (for backward compatibility)
  if (is.null(n_sims)) {
    return(list(mode = "auto", fixed_target = NA_integer_))
  }

  if (is.character(n_sims)) {
    v <- tolower(n_sims)
    if (!v %in% c("auto", "algo")) {
      stop("n_sims must be NULL, 'auto', or a positive integer, got: ", n_sims,
           call. = FALSE)
    }
    # Warn about deprecated alias
    if (v == "algo") {
      warning("n_sims='algo' is deprecated, use 'auto' instead", call. = FALSE)
    }
    list(mode = "auto", fixed_target = NA_integer_)
  } else if (is.numeric(n_sims) && length(n_sims) == 1L && is.finite(n_sims) && n_sims > 0) {
    list(mode = "fixed", fixed_target = as.integer(n_sims))
  } else {
    stop("n_sims must be NULL, 'auto', or a positive integer", call. = FALSE)
  }
}

#' Ensure Directory Tree Exists
#' @noRd
.mosaic_ensure_dir_tree <- function(dir_output, run_npe, clean_output) {
  d <- list(
    root = dir_output,
    setup = file.path(dir_output, "0_setup"),
    bfrs = file.path(dir_output, "1_bfrs"),
    bfrs_cfg = file.path(dir_output, "1_bfrs/config"),
    bfrs_diag = file.path(dir_output, "1_bfrs/diagnostics"),
    bfrs_out = file.path(dir_output, "1_bfrs/outputs"),
    bfrs_params = file.path(dir_output, "1_bfrs/outputs/parameters"),
    bfrs_post = file.path(dir_output, "1_bfrs/posterior"),
    bfrs_plots = file.path(dir_output, "1_bfrs/plots"),
    bfrs_plots_diag = file.path(dir_output, "1_bfrs/plots/diagnostics"),
    bfrs_plots_post = file.path(dir_output, "1_bfrs/plots/posterior"),
    bfrs_plots_pred = file.path(dir_output, "1_bfrs/plots/predictions"),
    results = file.path(dir_output, "3_results")
  )

  # Only create NPE-specific directories when NPE is enabled
  if (run_npe) {
    d$bfrs_times = file.path(dir_output, "1_bfrs/outputs/timeseries")
    d$npe <- file.path(dir_output, "2_npe")
    d$npe_plots <- file.path(dir_output, "2_npe/plots")
  }

  if (clean_output && dir.exists(d$root)) {
    message("Cleaning output directory: ", d$root)
    unlink(d$root, recursive = TRUE, force = TRUE)
  }

  invisible(lapply(Filter(Negate(is.null), d), dir.create, recursive = TRUE, showWarnings = FALSE))
  d
}

# =============================================================================
# STATE MANAGEMENT
# =============================================================================

#' Initialize Calibration State
#' @noRd
.mosaic_init_state <- function(control, param_names_est, nspec) {
  list(
    total_sims_run = 0L,
    total_sims_successful = 0L,
    batch_number = 0L,
    batch_success_rates = numeric(),
    batch_sizes_used = integer(),
    phase = if (identical(nspec$mode, "fixed")) "fixed" else "calibration",
    calib_batches = 0L,
    calib_r2 = NA_real_,
    calibration_done = FALSE,
    ess_history = list(),
    ess_tracking = list(),
    param_names_est = param_names_est,
    converged = FALSE,
    predictive_done = FALSE,
    mode = nspec$mode,
    fixed_target = nspec$fixed_target,
    # BUG FIX #1: Track phase iterations to prevent premature phase transitions
    phase_batch_count = 0L,
    phase_last = NULL
  )
}

#' Save State to RDS (Deprecated - Use Safe Version)
#'
#' @description
#' **Deprecated**: Use `.mosaic_save_state_safe()` instead for file locking.
#' This function now redirects to the safe version for backward compatibility.
#'
#' @noRd
.mosaic_save_state <- function(state, path) {
  # Redirect to safe version with locking
  .mosaic_save_state_safe(state, path)
}

#' Decide Next Batch Size
#' @noRd
.mosaic_decide_next_batch <- function(state, control, ess_tracking) {

  # Phase tracking is now managed in main loop
  # This function just decides what to do next

  # Calibration phase
  if (identical(state$phase, "calibration") && !isTRUE(state$calibration_done)) {
    return(list(
      phase = "calibration",
      batch_size = control$calibration$batch_size
    ))
  }

  # Predictive batch
  # BUG FIX #3: Require calibration_done to be TRUE before running predictive
  if (isTRUE(state$calibration_done) && !isTRUE(state$predictive_done)) {
    res <- tryCatch({
      calc_bookend_batch_size(
        ess_history = ess_tracking,
        target_ess = control$targets$ESS_param,
        reserved_sims = 250L,
        max_total_sims = control$calibration$max_simulations,
        target_r_squared = control$calibration$target_r2
      )
    }, error = function(e) NULL)

    # Log predictive batch calculation details
    if (!is.null(res) && !is.null(res$model) && res$batch_size > 0) {
      log_msg("Predictive batch calculation:")
      log_msg("  Model: %s (R² = %.4f)", res$model, res$r_squared)
      log_msg("  Current ESS: %.1f → Target: %d", res$current_ess, res$target_ess)
      log_msg("  Predicted batch size: %d sims (safety factor: %.2f)",
              res$batch_size, res$safety_factor)
      log_msg("  Expected total after batch: %d sims", res$total_predicted)
    }

    size <- if (is.null(res) || res$batch_size <= 0) {
      if (!is.null(res) && !is.null(res$message)) {
        log_msg("Predictive batch: %s", res$message)
      }
      500L
    } else {
      as.integer(res$batch_size)
    }

    return(list(
      phase = "predictive",
      batch_size = size
    ))
  }

  # Fine-tuning phase (5-tier adaptive)
  # BUG FIX #3: Use threshold_ess (percentile-based) instead of min_ess for consistency
  gap <- NA_real_
  if (length(ess_tracking)) {
    cur_threshold <- tail(vapply(ess_tracking, `[[`, numeric(1), "threshold_ess"), 1)
    gap <- control$targets$ESS_param - cur_threshold
  }

  # BUG FIX #5: If gap is negative or zero, we're done (or very close)
  if (!is.na(gap) && gap <= 0) {
    return(list(
      phase = "fine_tuning",
      batch_size = 0L,
      message = "Target ESS achieved or exceeded"
    ))
  }

  bs <- control$fine_tuning$batch_sizes
  size <- if (is.na(gap)) {
    bs$standard
  } else if (gap > 400) {
    bs$massive
  } else if (gap > 200) {
    bs$large
  } else if (gap > 100) {
    bs$standard
  } else if (gap > 50) {
    bs$precision
  } else {
    bs$final
  }

  list(
    phase = "fine_tuning",
    batch_size = as.integer(size)
  )
}

#' ESS Check and Update State
#' @noRd
.mosaic_ess_check_update_state <- function(state, dirs, param_names_est, control) {

  # Load all simulation files fresh from disk
  files <- list.files(dirs$bfrs_params, pattern = "^sim_.*\\.parquet$", full.names = TRUE)
  if (!length(files)) return(state)

  log_msg("Checking ESS convergence...")
  load_start <- Sys.time()

  # Use efficient streaming method (same as final results loading)
  # This is much faster and more memory-efficient than rbindlist for large datasets
  ess_check_results <- tryCatch({
    .mosaic_load_and_combine_results(
      dir_params = dirs$bfrs_params,
      method = "streaming",
      verbose = FALSE
    )
  }, error = function(e) NULL)

  load_time <- difftime(Sys.time(), load_start, units = "secs")

  if (is.null(ess_check_results) || !nrow(ess_check_results)) {
    return(state)
  }

  # Skip ESS calculation if insufficient samples
  # calc_model_ess_parameter requires at least 50 samples for KDE
  if (nrow(ess_check_results) < 50) {
    log_msg("  Skipping ESS check: %d simulations (need at least 50)", nrow(ess_check_results))
    return(state)
  }

  # Calculate ESS using specified method from control
  ess_current <- tryCatch({
    calc_model_ess_parameter(
      results = ess_check_results,
      param_names = param_names_est,
      likelihood_col = "likelihood",
      method = control$targets$ESS_method,
      verbose = FALSE
    )
  }, error = function(e) {
    log_msg("  ESS calculation failed: %s", e$message)
    NULL
  })

  if (is.null(ess_current) || !"ess_marginal" %in% names(ess_current)) {
    return(state)
  }

  # Store ESS history
  state$ess_history[[length(state$ess_history) + 1L]] <- list(
    batch = state$batch_number,
    total_sims = nrow(ess_check_results),
    ess_values = ess_current
  )

  # Calculate percentile-based threshold
  percentile_cutoff <- 1 - control$targets$ESS_param_prop
  threshold_ess <- as.numeric(stats::quantile(
    ess_current$ess_marginal,
    probs = percentile_cutoff,
    na.rm = TRUE
  ))

  state$ess_tracking[[length(state$ess_tracking) + 1L]] <- list(
    batch = state$batch_number,
    total_sims = nrow(ess_check_results),
    threshold_ess = threshold_ess,
    min_ess = min(ess_current$ess_marginal, na.rm = TRUE),
    median_ess = stats::median(ess_current$ess_marginal, na.rm = TRUE),
    max_ess = max(ess_current$ess_marginal, na.rm = TRUE)
  )

  # Report current ESS status
  log_msg("  ESS at %.0f%% percentile: %.1f (target: %d, min: %.1f)",
          percentile_cutoff * 100,
          threshold_ess,
          control$targets$ESS_param,
          min(ess_current$ess_marginal, na.rm = TRUE))

  # Calibration R² check
  # ESS is calculated for ALL batches (data accumulates from batch 1)
  # But calibration model fitting only starts when we have min_batches data points
  # This ensures the model uses data from ALL batches (1, 2, 3, ..., N)
  if (identical(state$phase, "calibration") &&
      !isTRUE(state$calibration_done) &&
      state$batch_number >= control$calibration$min_batches &&
      length(state$ess_tracking) >= control$calibration$min_batches) {

    ess_df <- data.frame(
      sims = vapply(state$ess_tracking, `[[`, numeric(1), "total_sims"),
      threshold_ess = vapply(state$ess_tracking, `[[`, numeric(1), "threshold_ess")
    )

    # Fit sqrt-linear model: ESS ~ sqrt(n) is typical scaling for importance sampling
    ess_df$sqrt_sims <- sqrt(ess_df$sims)
    ess_lm <- stats::lm(threshold_ess ~ sqrt_sims, data = ess_df)
    lm_summary <- summary(ess_lm)
    r2 <- lm_summary$r.squared
    coef <- stats::coef(ess_lm)
    intercept <- coef[1]
    slope <- coef[2]

    state$calib_batches <- state$calib_batches + 1L
    state$calib_r2 <- r2

    # Calculate estimated simulations to reach target ESS
    # Model: ESS = intercept + slope × sqrt(n)
    # Solving for n: n = ((target_ess - intercept) / slope)^2
    target_ess <- control$targets$ESS_param
    est_sims <- if (slope > 0 && (target_ess - intercept) > 0) {
      ((target_ess - intercept) / slope)^2
    } else {
      NA_real_
    }

    # Print model fit diagnostics
    log_msg("Calibration convergence check (batch %d):", state$batch_number)
    if (!is.na(est_sims)) {
      log_msg("  Model: ESS = %.2f + %.4f × sqrt(n)  |  R² = %.4f (target %.2f) | Est. Sims: %d",
              intercept, slope, r2, control$calibration$target_r2, round(est_sims))
    } else {
      log_msg("  Model: ESS = %.2f + %.4f × sqrt(n)  |  R² = %.4f (target %.2f) | Est. Sims: N/A",
              intercept, slope, r2, control$calibration$target_r2)
    }
    log_msg("  Data points: %d measurements (batches 1-%d) | Simulations: %d-%d",
            nrow(ess_df), state$batch_number, min(ess_df$sims), max(ess_df$sims))

    # Check if calibration should end
    if (r2 >= control$calibration$target_r2 ||
        state$batch_number >= control$calibration$max_batches) {

      # Calculate remaining gap
      current_n <- nrow(ess_check_results)
      remaining_sims <- if (!is.na(est_sims) && est_sims > current_n) {
        est_sims - current_n
      } else {
        0
      }

      # Decide whether to end calibration or continue
      # Check max_batches FIRST to ensure hard limit is enforced
      if (state$batch_number >= control$calibration$max_batches) {
        # Hit max batches limit - always exit regardless of R² or gap
        state$calibration_done <- TRUE
        if (r2 < control$calibration$target_r2) {
          log_msg("  → Calibration complete: reached max_batches (%d) before R² converged (%.4f < %.2f)",
                  control$calibration$max_batches, r2, control$calibration$target_r2)
        } else {
          log_msg("  → Calibration complete: reached max_batches (%d)",
                  control$calibration$max_batches)
        }
        if (remaining_sims > 0) {
          log_msg("    Estimated gap: %d sims → proceeding to predictive phase", ceiling(remaining_sims))
        }

      } else if (threshold_ess >= target_ess) {
        # Already at or above target ESS
        # Don't log here - convergence check will announce it
        state$calibration_done <- TRUE

      } else if (remaining_sims > 0 && remaining_sims < control$calibration$batch_size) {
        # Small gap remaining - continue calibration instead of transitioning
        log_msg("  → Calibration R² achieved, but gap is small")
        log_msg("    Current: %d sims | Estimated need: %d sims | Gap: %d sims",
                current_n, round(est_sims), ceiling(remaining_sims))
        log_msg("    Continuing calibration (gap < batch_size)")
        # Don't set calibration_done, continue with one more batch

      } else {
        # R² converged and gap is large enough for predictive phase
        state$calibration_done <- TRUE
        log_msg("  → Calibration complete: R² converged")
        if (remaining_sims > 0) {
          log_msg("    Estimated gap: %d sims → proceeding to predictive batch", ceiling(remaining_sims))
        }
      }
    }

  # BUG FIX #1: Only mark predictive as done after at least one batch has run
  } else if (identical(state$phase, "predictive") &&
             !is.null(state$phase_batch_count) &&
             state$phase_batch_count >= 1) {
    state$predictive_done <- TRUE
    log_msg("  → Predictive batch complete, proceeding to fine-tuning")
  }

  # Check convergence
  n_converged <- sum(ess_current$ess_marginal >= control$targets$ESS_param, na.rm = TRUE)
  prop_converged <- n_converged / length(param_names_est)

  if (prop_converged >= control$targets$ESS_param_prop) {
    state$converged <- TRUE
    log_msg("  → CONVERGENCE ACHIEVED: %.1f%% of parameters at ESS >= %d",
            prop_converged * 100, control$targets$ESS_param)
  }

  # Clean up and return
  rm(ess_check_results)
  gc(verbose = FALSE)

  state
}

# =============================================================================
# WEIGHT CALCULATIONS
# =============================================================================

#' Calculate Adaptive Gibbs Weights
#'
#' Unified adaptive weight calculation using Gibbs tempering with automatic
#' effective range tuning to prevent numerical underflow.
#'
#' @param likelihood Numeric vector of log-likelihood values
#' @param weight_floor Minimum weight for any model (default: 1e-15)
#'   Prevents numerical underflow by ensuring worst model gets at least this weight.
#'   Research-backed value: 10× above machine epsilon, safe up to ΔAIC = 69.
#' @param verbose Logical, print diagnostics
#'
#' @return List containing:
#'   - weights: Normalized weight vector (sum = 1)
#'   - temperature: Gibbs temperature used
#'   - effective_range: Effective AIC range used for temperature calculation
#'   - metrics: List with ESS_kish, ESS_perplexity, actual_range, n_valid
#'
#' @details
#' Algorithm:
#' 1. Convert likelihood to AIC: aic = -2 * likelihood
#' 2. Calculate delta_aic from best model
#' 3. Calculate adaptive effective range to ensure w_min >= weight_floor
#' 4. Calculate inverse temperature: eta = actual_range / effective_range
#' 5. Compute Gibbs weights via calc_model_weights_gibbs
#'
#' The adaptive method automatically adjusts to prevent numerical underflow while
#' maintaining discrimination among models. No manual tuning required.
#'
#' Edge cases:
#' - All likelihoods identical: Returns uniform weights
#' - Actual range < 1e-6: Returns uniform weights
#' - Too few valid samples (< 2): Returns uniform weights
#'
#' @noRd
.mosaic_calc_adaptive_gibbs_weights <- function(
  likelihood,
  weight_floor = 1e-15,
  verbose = FALSE
) {

  # ===========================================================================
  # Input validation
  # ===========================================================================

  if (!is.numeric(likelihood) || length(likelihood) == 0) {
    stop(".mosaic_calc_adaptive_gibbs_weights: likelihood must be non-empty numeric vector",
         call. = FALSE)
  }

  if (weight_floor <= 0 || weight_floor >= 1) {
    stop(".mosaic_calc_adaptive_gibbs_weights: weight_floor must be in (0, 1)",
         call. = FALSE)
  }

  n_total <- length(likelihood)

  # ===========================================================================
  # Filter to valid likelihoods
  # ===========================================================================

  valid_idx <- is.finite(likelihood) & !is.na(likelihood)
  n_valid <- sum(valid_idx)

  if (n_valid == 0) {
    warning(".mosaic_calc_adaptive_gibbs_weights: No valid likelihoods, returning uniform weights",
            call. = FALSE)
    return(list(
      weights = rep(1.0 / n_total, n_total),
      temperature = NA_real_,
      effective_range = NA_real_,
      metrics = list(
        ESS_kish = n_total,
        ESS_perplexity = n_total,
        actual_range = NA_real_,
        n_valid = 0
      )
    ))
  }

  if (n_valid < 2) {
    warning(".mosaic_calc_adaptive_gibbs_weights: Only ", n_valid,
            " valid likelihood(s), returning uniform weights", call. = FALSE)
    return(list(
      weights = rep(1.0 / n_total, n_total),
      temperature = NA_real_,
      effective_range = NA_real_,
      metrics = list(
        ESS_kish = n_total,
        ESS_perplexity = n_total,
        actual_range = NA_real_,
        n_valid = n_valid
      )
    ))
  }

  # ===========================================================================
  # Calculate AIC and delta AIC
  # ===========================================================================

  aic <- numeric(n_total)
  aic[valid_idx] <- -2 * likelihood[valid_idx]
  aic[!valid_idx] <- Inf

  best_aic <- min(aic[valid_idx])
  delta_aic <- aic - best_aic

  # ===========================================================================
  # Calculate AIC range
  # ===========================================================================

  actual_range <- diff(range(delta_aic[valid_idx], na.rm = TRUE))

  # ===========================================================================
  # Check for uniform case
  # ===========================================================================

  if (actual_range < 1e-6) {
    # All models essentially identical
    if (verbose) {
      message("  All models have nearly identical likelihood (range < 1e-6)")
      message("  Returning uniform weights")
    }
    return(list(
      weights = rep(1.0 / n_total, n_total),
      temperature = 1.0,  # Arbitrary, not used
      effective_range = 0.0,
      metrics = list(
        ESS_kish = n_total,
        ESS_perplexity = n_total,
        actual_range = actual_range,
        n_valid = n_valid
      )
    ))
  }

  # ===========================================================================
  # Calculate adaptive effective range
  # ===========================================================================

  # Find worst delta_aic across all valid models
  max_delta_aic <- max(delta_aic[valid_idx], na.rm = TRUE)

  # Calculate effective_range needed to keep worst weight >= floor
  # From: exp(-max_delta_aic / (2*temp)) >= floor
  # Solve for effective_range:
  #   temp = 0.5 * (effective_range / actual_range)
  #   exp(-max_delta_aic / (2 * 0.5 * (effective_range / actual_range))) >= floor
  #   exp(-max_delta_aic * actual_range / effective_range) >= floor
  #   -max_delta_aic * actual_range / effective_range >= log(floor)
  #   effective_range >= -max_delta_aic * actual_range / log(floor)
  effective_range <- actual_range * max_delta_aic / (-log(weight_floor))

  # ===========================================================================
  # Calculate inverse temperature (eta) and weights
  # ===========================================================================

  # calc_model_weights_gibbs uses inverse temperature (eta) in: w ∝ exp(-eta * x)
  # We want: exp(-max_delta_aic * eta) >= floor
  # Therefore: eta = -log(floor) / max_delta_aic
  #
  # Alternatively, from temp = 0.5 * (effective_range / actual_range):
  # eta = 1/(2*temp) = actual_range / effective_range
  eta <- actual_range / effective_range

  # For diagnostics, also calculate equivalent "temperature" in textbook sense
  # (not used in weight calculation, just for reporting)
  temperature <- 0.5 * (effective_range / actual_range)

  # Calculate Gibbs weights using inverse temperature
  weights <- calc_model_weights_gibbs(
    x = delta_aic,
    temperature = eta,  # Note: this parameter is actually inverse temperature
    verbose = FALSE
  )

  # ===========================================================================
  # Calculate ESS metrics
  # ===========================================================================

  ESS_kish <- calc_model_ess(weights, method = "kish")
  ESS_perplexity <- calc_model_ess(weights, method = "perplexity")

  # ===========================================================================
  # Verbose diagnostics
  # ===========================================================================

  if (verbose) {
    message("Adaptive Gibbs Weight Calculation:")
    message("  Total models: ", n_total)
    message("  Valid models: ", n_valid)
    message("  Actual ΔAIC range: ", sprintf("%.2f", actual_range))
    message("  Max ΔAIC: ", sprintf("%.2f", max_delta_aic))
    message("  Weight floor: ", sprintf("%.2e", weight_floor))
    message("  Adaptive effective range: ", sprintf("%.2f", effective_range))
    message("  Temperature: ", sprintf("%.4f", temperature))
    message("  ESS (Kish): ", sprintf("%.1f", ESS_kish))
    message("  ESS (Perplexity): ", sprintf("%.1f", ESS_perplexity))
  }

  # ===========================================================================
  # Return results
  # ===========================================================================

  list(
    weights = weights,
    temperature = temperature,
    effective_range = effective_range,
    metrics = list(
      ESS_kish = ESS_kish,
      ESS_perplexity = ESS_perplexity,
      actual_range = actual_range,
      n_valid = n_valid
    )
  )
}

# =============================================================================
# EXECUTION
# =============================================================================

#' Run Simulation Batch (Sequential or Parallel)
#'
#' Abstraction layer that runs simulations either sequentially (lapply) or
#' in parallel (parLapply) depending on whether a cluster object is provided.
#'
#' @param sim_ids Vector of simulation IDs to run
#' @param worker_func Simulation worker function
#' @param cl Cluster object (NULL for sequential, cluster for parallel)
#' @param show_progress Logical, whether to show progress bar
#' @return List of success indicators from worker function
#' @noRd
.mosaic_run_batch <- function(sim_ids, worker_func, cl, show_progress) {
  if (is.null(cl)) {
    # Sequential execution
    if (isTRUE(show_progress)) {
      # Simple progress bar with block character (no color codes)
      # style = 1: Shows elapsed and remaining time with percentage
      pbo <- pbapply::pboptions(type = "timer", char = "█", style = 1)
      on.exit(pbapply::pboptions(pbo), add = TRUE)

      # Wrap worker to suppress unwanted output
      wrapped_func <- function(id) {
        capture.output(result <- worker_func(id), type = "output")
        result
      }

      pbapply::pblapply(sim_ids, wrapped_func)
    } else {
      lapply(sim_ids, worker_func)
    }
  } else {
    # Parallel execution
    if (isTRUE(show_progress)) {
      # Simple progress bar with block character (no color codes)
      # style = 1: Shows elapsed and remaining time with percentage
      pbo <- pbapply::pboptions(type = "timer", char = "█", style = 1)
      on.exit(pbapply::pboptions(pbo), add = TRUE)

      pbapply::pblapply(sim_ids, worker_func, cl = cl)
    } else {
      parallel::parLapply(cl, sim_ids, worker_func)
    }
  }
}
