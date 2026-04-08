# =============================================================================
# MOSAIC: run_mosaic_helpers.R
# Internal helper functions for run_mosaic()
# =============================================================================

# All functions prefixed with . (not exported - internal use only)

# Constants for validation
.MOSAIC_MAX_ITERATIONS <- 1000L
.MOSAIC_MIN_BATCH_SIZE <- 10L
.MOSAIC_MAX_BATCH_SIZE <- 1000000L
.MOSAIC_MIN_SIMULATIONS <- 100L
.MOSAIC_MAX_SIMULATIONS <- 100000000L

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
    "targets$min_best_subset must be positive integer >= 10" =
      is.numeric(def$targets$min_best_subset) &&
      def$targets$min_best_subset >= 10,
    "targets$max_best_subset must be >= min_best_subset" =
      is.numeric(def$targets$max_best_subset) &&
      def$targets$max_best_subset >= def$targets$min_best_subset
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
#' Uses atomic write with rename for filesystem safety.
#' Disk space is checked once at calibration start, not per file.
#'
#' @noRd
.mosaic_write_parquet <- function(df, path, io) {
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
  # Define write function
  write_func <- function(data, file) {
    jsonlite::write_json(data, file, pretty = TRUE, auto_unbox = TRUE, digits = NA)
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
                                             chunk_size = 5000L,
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
      # Chunked loading: process files in batches to avoid OOM with many small parquets.
      # Arrow's open_dataset() %>% collect() materializes everything at once, which causes
      # OOM with 40K+ single-row parquets (10-20KB overhead per file expands to several GB).
      n_files <- length(files)
      if (n_files <= chunk_size) {
        # Small enough to load in one shot
        arrow::open_dataset(dir_params, format = "parquet") %>%
          dplyr::collect() %>%
          as.data.frame()
      } else {
        # Chunked approach: rbindlist on batches to reduce working set
        n_chunks <- ceiling(n_files / chunk_size)
        if (verbose) log_msg("Loading in %d chunks of up to %d files", n_chunks, chunk_size)
        chunk_list <- vector("list", n_chunks)
        for (ci in seq_len(n_chunks)) {
          idx_start <- (ci - 1L) * chunk_size + 1L
          idx_end <- min(ci * chunk_size, n_files)
          chunk_files <- files[idx_start:idx_end]
          chunk_list[[ci]] <- data.table::rbindlist(
            lapply(chunk_files, arrow::read_parquet),
            fill = TRUE
          )
          if (verbose && ci %% 5 == 0) {
            log_msg("  Loaded chunk %d/%d (%d files)", ci, n_chunks, idx_end)
          }
        }
        as.data.frame(data.table::rbindlist(chunk_list, fill = TRUE))
      }
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
.mosaic_ensure_dir_tree <- function(dir_output, clean_output) {
  d <- list(
    root              = dir_output,
    inputs            = file.path(dir_output, "1_inputs"),
    calibration       = file.path(dir_output, "2_calibration"),
    cal_samples       = file.path(dir_output, "2_calibration/samples"),
    cal_best_model    = file.path(dir_output, "2_calibration/best_model"),
    cal_posterior      = file.path(dir_output, "2_calibration/posterior"),
    cal_diag          = file.path(dir_output, "2_calibration/diagnostics"),
    cal_state         = file.path(dir_output, "2_calibration/state"),
    cal_simresults    = NULL,  # Created conditionally when save_simresults = TRUE
    results           = file.path(dir_output, "3_results"),
    res_posterior      = file.path(dir_output, "3_results/posterior"),
    res_predictions    = file.path(dir_output, "3_results/predictions"),
    res_figures        = file.path(dir_output, "3_results/figures"),
    res_fig_diag       = file.path(dir_output, "3_results/figures/diagnostics"),
    res_fig_post       = file.path(dir_output, "3_results/figures/posterior"),
    res_fig_post_detail = file.path(dir_output, "3_results/figures/posterior/detail"),
    res_fig_pred       = file.path(dir_output, "3_results/figures/predictions"),
    res_fig_ppc        = file.path(dir_output, "3_results/figures/ppc")
  )

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
    phase_last = NULL,
    # Internal timestamp for state file provenance
    .created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}

#' Save Run State as JSON
#'
#' Writes a lightweight JSON representation of the workflow state.
#' The persisted state is a slim subset of the full in-memory state,
#' containing only what is needed for monitoring and future resume.
#' Uses atomic write via tempfile + rename.
#'
#' @noRd
.mosaic_save_state <- function(state, path) {
  # Extract only the fields needed for monitoring / resume
  persisted <- list(
    schema_version = 1L,
    status = "running",
    created_at = if (!is.null(state$.created_at)) state$.created_at
                 else format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    phase = state$phase,
    batch = state$batch_number,
    sims_completed = state$total_sims_successful,
    sims_target = if (identical(state$mode, "fixed")) state$fixed_target else NULL,
    converged = isTRUE(state$converged),
    r_squared = if (!is.na(state$calib_r2)) round(state$calib_r2, 4) else NULL,
    ess_min = if (length(state$ess_tracking) > 0) {
      round(tail(vapply(state$ess_tracking, `[[`, numeric(1), "min_ess"), 1), 1)
    } else NULL
  )

  # Atomic write: tempfile + rename
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".json.tmp")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(persisted, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  file.rename(tmp, path)
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
      log_msg("  Current ESS: %.1f → Target: %.0f", res$current_ess, res$target_ess)
      log_msg("  Predicted batch size: %.0f sims (safety factor: %.2f)",
              res$batch_size, res$safety_factor)
      log_msg("  Expected total after batch: %.0f sims", res$total_predicted)
    }

    size <- if (is.null(res) || res$batch_size <= 0) {
      if (!is.null(res) && !is.null(res$message)) {
        log_msg("Predictive batch: %s", res$message)
      }
      500L
    } else {
      as.integer(res$batch_size)
    }

    # Cap predictive batch to avoid multi-hour single batches with no checkpointing.
    # If the predicted size exceeds the cap, run cap-sized batches with ESS
    # re-evaluation between each (the main loop handles this naturally since
    # predictive_done isn't set until .mosaic_ess_check_update_state marks it).
    max_pred <- control$calibration$max_predictive_batch
    if (!is.null(max_pred) && size > max_pred) {
      log_msg("  Capping predictive batch: %d → %d (max_predictive_batch)", size, max_pred)
      size <- as.integer(max_pred)
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
  files <- list.files(dirs$cal_samples, pattern = "^sim_.*\\.parquet$", full.names = TRUE)
  if (!length(files)) return(state)

  log_msg("Checking ESS convergence...")
  load_start <- Sys.time()

  # Use efficient streaming method (same as final results loading)
  # This is much faster and more memory-efficient than rbindlist for large datasets
  ess_check_results <- tryCatch({
    .mosaic_load_and_combine_results(
      dir_params = dirs$cal_samples,
      method = "streaming",
      chunk_size = control$io$load_chunk_size %||% 5000L,
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
    # Guard against NA/NaN coefficients (occurs when ess_df has only 1 row,
    # making the 2-parameter lm underdetermined — R sets slope=NA)
    est_sims <- if (isTRUE(slope > 0) && isTRUE((target_ess - intercept) > 0)) {
      ((target_ess - intercept) / slope)^2
    } else {
      NA_real_
    }

    # Print model fit diagnostics
    # slope/r2 may be NA when ESS has plateaued (constant response → rank-deficient lm)
    slope_print <- if (is.finite(slope)) slope else 0
    r2_print    <- if (is.finite(r2))    r2    else 0
    log_msg("Calibration convergence check (batch %d):", state$batch_number)
    if (!is.na(est_sims)) {
      log_msg("  Model: ESS = %.2f + %.4f × sqrt(n)  |  R² = %.4f (target %.2f) | Est. Sims: %.0f",
              intercept, slope_print, r2_print, control$calibration$target_r2, round(est_sims))
    } else {
      log_msg("  Model: ESS = %.2f + %.4f × sqrt(n)  |  R² = %.4f (target %.2f) | Est. Sims: N/A%s",
              intercept, slope_print, r2_print, control$calibration$target_r2,
              if (!is.finite(slope)) " [ESS plateau — slope undefined]" else "")
    }
    log_msg("  Data points: %d measurements (batches 1-%d) | Simulations: %d-%d",
            nrow(ess_df), state$batch_number, min(ess_df$sims), max(ess_df$sims))

    # Check if calibration should end.
    # R² is only used as an exit signal when there are at least 5 data points
    # (3 residual df). A 2-parameter model fit to exactly min_batches=3 points
    # has only 1 residual df, making R² trivially near 1 for any monotone ESS
    # trajectory. The max_batches hard limit is always honoured regardless.
    min_r2_points <- 5L
    r2_converged <- r2 >= control$calibration$target_r2 && nrow(ess_df) >= min_r2_points
    if (r2_converged) {
      log_msg("  R² criterion met with %d data points (min required: %d)",
              nrow(ess_df), min_r2_points)
    } else if (r2 >= control$calibration$target_r2 && nrow(ess_df) < min_r2_points) {
      log_msg("  R² = %.4f >= target, but only %d data point(s) — need >= %d for reliable R²",
              r2, nrow(ess_df), min_r2_points)
    }

    if (r2_converged || state$batch_number >= control$calibration$max_batches) {

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
          log_msg("    Estimated gap: %.0f sims → proceeding to predictive phase", ceiling(remaining_sims))
        }

      } else if (threshold_ess >= target_ess) {
        # Already at or above target ESS
        # Don't log here - convergence check will announce it
        state$calibration_done <- TRUE

      } else if (remaining_sims > 0 && remaining_sims < control$calibration$batch_size) {
        # Small gap remaining - continue calibration instead of transitioning
        log_msg("  → Calibration R² achieved, but gap is small")
        log_msg("    Current: %d sims | Estimated need: %.0f sims | Gap: %.0f sims",
                current_n, round(est_sims), ceiling(remaining_sims))
        log_msg("    Continuing calibration (gap < batch_size)")
        # Don't set calibration_done, continue with one more batch

      } else {
        # R² converged and gap is large enough for predictive phase
        state$calibration_done <- TRUE
        log_msg("  → Calibration complete: R² converged")
        if (remaining_sims > 0) {
          log_msg("    Estimated gap: %.0f sims → proceeding to predictive batch", ceiling(remaining_sims))
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

  # Check convergence.
  # Use only parameters that produced a valid ESS estimate as the denominator.
  # Parameters whose KDE failed return NA in ess_marginal; dividing by
  # length(param_names_est) would count them as "not converged" and make
  # the target proportion artificially harder to reach.
  n_converged    <- sum(ess_current$ess_marginal >= control$targets$ESS_param, na.rm = TRUE)
  n_ess_computed <- sum(!is.na(ess_current$ess_marginal))
  prop_converged <- if (n_ess_computed > 0L) n_converged / n_ess_computed else 0

  if (prop_converged >= control$targets$ESS_param_prop) {
    # Only declare convergence after min_batches to ensure sufficient exploration.
    # Early batches can spuriously meet ESS targets with small samples.
    if (state$batch_number >= control$calibration$min_batches) {
      state$converged <- TRUE
      log_msg("  → CONVERGENCE ACHIEVED: %.1f%% of parameters at ESS >= %.0f (computed on %d/%d params)",
              prop_converged * 100, control$targets$ESS_param,
              n_ess_computed, length(param_names_est))
    } else {
      log_msg("  ESS criterion met (%.1f%% >= %.0f) but batch %d < min_batches %d — continuing",
              prop_converged * 100, control$targets$ESS_param,
              state$batch_number, control$calibration$min_batches)
    }
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

  # Calculate Gibbs weights using inverse temperature — valid models only.
  # delta_aic[!valid_idx] = Inf; passing the full vector to calc_model_weights_gibbs
  # would crash because that function stop()s on any non-finite input. Compute
  # weights on the valid subset, then place them back into a full-length vector
  # (invalid models receive weight 0 and are silently dropped by calc_model_ess).
  weights_valid <- calc_model_weights_gibbs(
    x = delta_aic[valid_idx],
    temperature = eta,
    verbose = FALSE
  )
  weights <- numeric(n_total)
  weights[valid_idx] <- weights_valid

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

# =============================================================================
# DASK BACKEND HELPERS
# =============================================================================

#' Extract Base Config Fields for Dask Broadcasting
#'
#' Returns only the large/fixed fields (matrices, metadata) that are identical
#' across all simulations and should be broadcast once via client$scatter().
#' @noRd
.extract_base_config <- function(config) {
  keep <- c(
    # 2-D matrices (n_locations × n_time_steps) — the bulk of the data
    "b_jt", "d_jt", "mu_jt", "psi_jt", "nu_1_jt", "nu_2_jt",
    "reported_cases", "reported_deaths",
    # Structural / metadata
    "date_start", "date_stop", "location_name",
    "N_j_initial", "longitude", "latitude"
  )
  config[names(config) %in% keep]
}

#' Extract Per-Simulation Sampled Parameters
#'
#' Returns only the scalar/vector parameters that sample_parameters() modifies.
#' Most matrix fields are excluded because they live in the broadcast
#' base_config. However, psi_jt is INCLUDED here because
#' apply_psi_star_calibration() modifies it in-place per simulation — the
#' broadcast base_config has stale (uncalibrated) psi_jt.
#' @noRd
.extract_sampled_params <- function(params_sim) {
  # Exclude fields that are in the broadcast base_config AND are never
  # modified by sample_parameters().  psi_jt is intentionally NOT excluded:
  # apply_psi_star_calibration() recalculates it per-sim using psi_star_*
  # params, so the per-sim version must override the broadcast base_config.
  base_fields <- c(
    "b_jt", "d_jt", "mu_jt", "nu_1_jt", "nu_2_jt",
    "reported_cases", "reported_deaths",
    "date_start", "date_stop", "location_name", "seed",
    "N_j_initial", "longitude", "latitude"
  )
  params_sim[!names(params_sim) %in% base_fields]
}

#' Run One Dask Batch (sample → submit → gather → likelihood → write parquets)
#'
#' Replaces .mosaic_run_batch() for Dask execution. Returns a logical vector
#' of per-simulation success indicators (TRUE = parquet written successfully).
#' @noRd
.mosaic_run_batch_dask <- function(sim_ids, n_iterations, priors, config, PATHS,
                                    sampling_args, dirs, param_names_all, control,
                                    likelihood_settings,
                                    client, base_config_future,
                                    mosaic_worker) {

  n_sims   <- length(sim_ids)
  n_locs   <- length(config$location_name)
  obs_cases  <- config$reported_cases
  obs_deaths <- config$reported_deaths

  # ---------------------------------------------------------------------------
  # 1. Pre-sample ALL parameters in R (serial; fast compared to LASER)
  # ---------------------------------------------------------------------------
  log_msg("  Sampling %d parameter sets in R...", n_sims)
  params_list <- vector("list", n_sims)
  for (idx in seq_len(n_sims)) {
    sim_id <- sim_ids[idx]
    params_list[[idx]] <- tryCatch(
      sample_parameters(
        PATHS      = PATHS,
        priors     = priors,
        config     = config,
        seed       = sim_id,
        sample_args = sampling_args,
        verbose    = FALSE,
        validate   = FALSE
      ),
      error = function(e) {
        warning("Param sampling failed for sim ", sim_id, ": ", e$message,
                call. = FALSE, immediate. = FALSE)
        NULL
      }
    )

    # Guardrails: clamp transmission parameters to prevent laser-cholera ValueError
    if (!is.null(params_list[[idx]])) {
      p <- params_list[[idx]]
      if (!is.null(p$beta_j0_tot)) p$beta_j0_tot <- pmax(p$beta_j0_tot, 1e-10)
      if (!is.null(p$beta_j0_hum)) p$beta_j0_hum <- pmax(p$beta_j0_hum, 0)
      if (!is.null(p$beta_j0_env)) p$beta_j0_env <- pmax(p$beta_j0_env, 0)
      if (!is.null(p$p_beta))      p$p_beta      <- pmin(pmax(p$p_beta, 1e-6), 1 - 1e-6)
      if (!is.null(p$tau_i))       p$tau_i       <- pmin(pmax(p$tau_i, 0), 1)
      params_list[[idx]] <- p
    }
  }

  # ---------------------------------------------------------------------------
  # 2. Serialize per-sim scalar/vector params to JSON
  # ---------------------------------------------------------------------------
  sampled_jsons <- lapply(params_list, function(p) {
    if (is.null(p)) return(NULL)
    jsonlite::toJSON(.extract_sampled_params(p),
                     auto_unbox = TRUE,
                     digits     = NA)  # full floating-point precision
  })

  # ---------------------------------------------------------------------------
  # 3. Submit futures to Dask (skip NULL params)
  # ---------------------------------------------------------------------------
  log_msg("  Submitting %d futures to Dask cluster...", n_sims)
  futures <- vector("list", n_sims)
  for (idx in seq_len(n_sims)) {
    if (is.null(sampled_jsons[[idx]])) {
      futures[[idx]] <- NULL
      next
    }
    futures[[idx]] <- client$submit(
      mosaic_worker$run_laser_sim,
      as.integer(sim_ids[[idx]]),
      as.integer(n_iterations),
      as.character(sampled_jsons[[idx]]),
      base_config_future
    )
  }

  valid_futures <- Filter(Negate(is.null), futures)

  # ---------------------------------------------------------------------------
  # 4. Gather results (blocking)
  # ---------------------------------------------------------------------------
  log_msg("  Waiting for %d Dask futures...", length(valid_futures))
  gather_start <- Sys.time()
  gathered <- tryCatch(
    client$gather(valid_futures),
    error = function(e) {
      log_msg("  ERROR in client$gather(): %s", conditionMessage(e))
      # Try to retrieve first few future errors for diagnostics
      tryCatch({
        first_future <- valid_futures[[1L]]
        st <- first_future$status
        log_msg("  First future status: %s", as.character(st))
        if (identical(st, "error")) {
          exc <- first_future$exception()
          log_msg("  First future exception: %s", as.character(exc))
          tb_str <- first_future$traceback()
          if (!is.null(tb_str)) log_msg("  First future traceback: %s", as.character(tb_str))
        }
      }, error = function(e2) {
        log_msg("  (Could not inspect futures: %s)", conditionMessage(e2))
      })
      stop(e)
    }
  )
  gather_elapsed <- as.numeric(difftime(Sys.time(), gather_start, units = "secs"))
  log_msg("  Gather complete: %d results in %.1fs", length(gathered), gather_elapsed)

  # Build lookup: sim_id → worker result
  result_lookup <- list()
  n_worker_errors <- 0L
  for (res in gathered) {
    key <- as.character(res$sim_id)
    result_lookup[[key]] <- res
    if (!isTRUE(res$success) && n_worker_errors < 3L) {
      n_worker_errors <- n_worker_errors + 1L
      err_msg <- if (!is.null(res$error)) res$error else "unknown"
      log_msg("  Worker error (sim %s): %s", key, err_msg)
    }
  }
  if (n_worker_errors > 0L) {
    total_errors <- sum(!vapply(gathered, function(r) isTRUE(r$success), logical(1)))
    log_msg("  Total worker errors: %d/%d", total_errors, length(gathered))
  }

  # ---------------------------------------------------------------------------
  # 4b. Log per-worker timing summary then free gathered list
  # ---------------------------------------------------------------------------
  worker_times <- vapply(gathered, function(res) {
    if (isTRUE(res$success) && !is.null(res$worker_elapsed_sec))
      as.numeric(res$worker_elapsed_sec)
    else
      NA_real_
  }, numeric(1))
  worker_times <- worker_times[is.finite(worker_times)]

  if (length(worker_times) > 0L) {
    log_msg("  Worker timing (n=%d): mean=%.1fs, median=%.1fs, min=%.1fs, max=%.1fs, total=%.1fs",
            length(worker_times),
            mean(worker_times), median(worker_times),
            min(worker_times), max(worker_times),
            sum(worker_times))
  }

  # Free the raw gathered list — result_lookup holds the same data
  rm(gathered); gc(verbose = FALSE)

  # ---------------------------------------------------------------------------
  # 5. For each sim: compute likelihood in R, write parquet
  # ---------------------------------------------------------------------------
  log_msg("  Computing likelihoods + writing parquet for %d sims...", n_sims)
  likelihood_start <- Sys.time()
  success_indicators <- logical(n_sims)

  for (idx in seq_len(n_sims)) {
    # Periodic progress + Dask client health check
    if (idx %% 500L == 0L) {
      elapsed <- as.numeric(difftime(Sys.time(), likelihood_start, units = "secs"))
      log_msg("    Likelihood progress: %d/%d (%.0fs elapsed)", idx, n_sims, elapsed)
      # Ping scheduler to keep connection alive and detect disconnection early
      tryCatch({
        client$scheduler_info()
      }, error = function(e) {
        log_msg("    WARNING: Dask scheduler ping failed at sim %d: %s", idx, e$message)
      })
    }
    sim_id <- sim_ids[[idx]]
    key    <- as.character(sim_id)

    # Skip if param sampling or future submission failed
    if (is.null(params_list[[idx]]) || is.null(futures[[idx]])) {
      success_indicators[idx] <- FALSE
      next
    }

    res <- result_lookup[[key]]

    if (is.null(res) || !isTRUE(res$success)) {
      err_msg <- if (!is.null(res$error)) res$error else "unknown error"
      warning("sim ", sim_id, " failed on worker: ", err_msg,
              call. = FALSE, immediate. = FALSE)
      success_indicators[idx] <- FALSE
      next
    }

    # ------------------------------------------------------------------
    # Compute per-iteration likelihoods in R, then collapse
    # (mirrors .mosaic_run_simulation_worker behavior in run_MOSAIC.R)
    # ------------------------------------------------------------------
    iter_list   <- res$iterations
    n_iter_got  <- length(iter_list)
    lls         <- numeric(n_iter_got)

    for (ji in seq_len(n_iter_got)) {
      iter_res   <- iter_list[[ji]]
      est_cases  <- matrix(unlist(iter_res$reported_cases),
                           nrow = n_locs, byrow = FALSE)
      est_deaths <- matrix(unlist(iter_res$disease_deaths),
                           nrow = n_locs, byrow = FALSE)

      lls[ji] <- tryCatch(
        calc_model_likelihood(
          config       = config,
          obs_cases    = obs_cases,
          est_cases    = est_cases,
          obs_deaths   = obs_deaths,
          est_deaths   = est_deaths,
          weights_time          = likelihood_settings$.weights_time_resolved,
          weights_location      = likelihood_settings$weights_location,
          nb_k_min_cases        = likelihood_settings$nb_k_min_cases,
          nb_k_min_deaths       = likelihood_settings$nb_k_min_deaths,
          weight_cases          = likelihood_settings$weight_cases,
          weight_deaths         = likelihood_settings$weight_deaths,
          weight_peak_timing    = likelihood_settings$weight_peak_timing,
          weight_peak_magnitude = likelihood_settings$weight_peak_magnitude,
          weight_cumulative_total = likelihood_settings$weight_cumulative_total,
          weight_wis            = likelihood_settings$weight_wis,
          sigma_peak_time       = likelihood_settings$sigma_peak_time,
          sigma_peak_log        = likelihood_settings$sigma_peak_log,
          penalty_unmatched_peak = likelihood_settings$penalty_unmatched_peak
        ),
        error = function(e) {
          warning("Likelihood failed sim ", sim_id, " iter ", ji, ": ",
                  e$message, call. = FALSE, immediate. = FALSE)
          NA_real_
        }
      )
    }

    # Collapse iterations exactly as .mosaic_run_simulation_worker does
    if (n_iter_got > 1L) {
      valid_lls <- lls[is.finite(lls)]
      collapsed_ll <- if (length(valid_lls)) calc_log_mean_exp(valid_lls) else NA_real_
    } else {
      collapsed_ll <- lls[1L]
    }

    # Extract flat param vector for parquet row
    raw_params <- tryCatch({
      pv <- convert_config_to_matrix(params_list[[idx]])
      if ("seed" %in% names(pv)) pv <- pv[names(pv) != "seed"]
      pv[param_names_all]
    }, error = function(e) NULL)

    if (is.null(raw_params)) {
      success_indicators[idx] <- FALSE
      next
    }

    # Seed used for the first iteration (mirrors run_MOSAIC.R line 224)
    seed_iter_1 <- as.integer((sim_id - 1L) * n_iterations + 1L)

    row_df <- data.frame(
      sim      = as.integer(sim_id),
      iter     = 1L,
      seed_sim = as.integer(sim_id),
      seed_iter = as.integer(seed_iter_1),
      likelihood = collapsed_ll
    )
    for (pname in param_names_all) {
      row_df[[pname]] <- as.numeric(raw_params[pname])
    }

    out_file <- file.path(dirs$cal_samples,
                          sprintf("sim_%07d.parquet", sim_id))
    .mosaic_write_parquet(row_df, out_file, control$io)
    success_indicators[idx] <- file.exists(out_file)

    # Write raw per-(iter, j, t) simresults for validation (mirrors run_MOSAIC.R)
    if (!is.null(dirs$cal_simresults)) {
      simresults_iters <- vector("list", n_iter_got)
      for (ji in seq_len(n_iter_got)) {
        iter_res   <- res$iterations[[ji]]
        est_cases  <- matrix(unlist(iter_res$reported_cases),
                             nrow = n_locs, byrow = FALSE)
        est_deaths <- matrix(unlist(iter_res$disease_deaths),
                             nrow = n_locs, byrow = FALSE)
        n_j_raw <- nrow(est_cases)
        n_t_raw <- ncol(est_cases)
        sr_df <- data.frame(
          sim    = sim_id,
          iter   = as.integer(ji),
          j      = rep(seq_len(n_j_raw), times = n_t_raw),
          t      = rep(seq_len(n_t_raw), each  = n_j_raw),
          cases  = as.numeric(est_cases),
          deaths = as.numeric(est_deaths)
        )
        psi_jt <- params_list[[idx]]$psi_jt
        if (!is.null(psi_jt)) {
          if (!is.matrix(psi_jt)) psi_jt <- matrix(psi_jt, nrow = 1)
          sr_df$psi_jt <- psi_jt[cbind(sr_df$j, sr_df$t)]
        }
        simresults_iters[[ji]] <- sr_df
      }
      raw_df <- do.call(rbind, simresults_iters)
      for (pname in param_names_all) {
        raw_df[[pname]] <- as.numeric(raw_params[pname])
      }
      sr_file <- file.path(dirs$cal_simresults,
                           sprintf("simresults_%07d.parquet", sim_id))
      .mosaic_write_parquet(raw_df, sr_file, control$io)
    }

    # Free this sim's data immediately — each result holds large case/death
    # arrays from Python. Without this, 20K results peak at several GB.
    result_lookup[key] <- list(NULL)
    params_list[idx]   <- list(NULL)
  }

  likelihood_elapsed <- as.numeric(difftime(Sys.time(), likelihood_start, units = "secs"))
  log_msg("  Likelihood + parquet writing done: %d sims in %.1fs (%.1f sims/s)",
          n_sims, likelihood_elapsed, n_sims / max(likelihood_elapsed, 0.1))

  rm(result_lookup, params_list, sampled_jsons, futures)
  gc(verbose = FALSE)

  success_indicators
}


# =============================================================================
# Post-calibration Dask dispatch
# =============================================================================

#' Dispatch post-calibration LASER simulations via an existing Dask client
#'
#' Uses a reconnected Dask client to dispatch ensemble and/or stochastic
#' parameter uncertainty simulations. Returns pre-computed results that can be
#' passed to calc_model_ensemble() and plot_model_fit_stochastic_param() via
#' their precomputed_results argument.
#'
#' @param client dask.distributed.Client (already connected).
#' @param mosaic_worker Python module (mosaic_dask_worker, already imported).
#' @param config_best Config list for the best-fit model (for ensemble sims).
#' @param param_configs List of config lists for posterior parameter sets (for stochastic sims).
#'   NULL to skip stochastic dispatch.
#' @param n_ensemble Integer. Number of ensemble sims (different seeds, same config_best).
#' @param n_stochastic_per Integer. Number of stochastic sims per param config.
#' @param log_msg Function. Logging function.
#' @return Named list with $ensemble_results and $stochastic_results (or NULL for each).
#' @keywords internal
.mosaic_postca_dask <- function(client, mosaic_worker,
                                config_best, param_configs = NULL,
                                n_ensemble = 100L, n_stochastic_per = 10L,
                                log_msg = message) {

  # Helper: serialize config to JSON + extract large matrix fields
  .config_to_json_and_matrices <- function(cfg) {
    matrix_fields <- c("b_jt", "d_jt", "mu_jt", "psi_jt", "nu_1_jt", "nu_2_jt",
                        "reported_cases", "reported_deaths")
    matrices <- list()
    cfg_for_json <- cfg
    for (f in matrix_fields) {
      if (!is.null(cfg[[f]])) {
        matrices[[f]] <- cfg[[f]]
        cfg_for_json[[f]] <- NULL
      }
    }
    cfg_json <- jsonlite::toJSON(cfg_for_json, auto_unbox = TRUE, digits = NA)
    list(json = as.character(cfg_json),
         matrices = reticulate::r_to_py(matrices))
  }

  ensemble_results    <- NULL
  stochastic_results  <- NULL

  # --- Ensemble sims (one config_best, many seeds) ---
  if (n_ensemble > 0L && !is.null(config_best)) {
    log_msg("Dispatching %d ensemble sims to Dask...", n_ensemble)
    prep <- .config_to_json_and_matrices(config_best)
    extra_future <- client$scatter(prep$matrices, broadcast = TRUE)

    futures <- vector("list", n_ensemble)
    for (i in seq_len(n_ensemble)) {
      futures[[i]] <- client$submit(
        mosaic_worker$run_laser_postca,
        as.integer(i),          # task_id
        as.integer(i),          # seed = 1..n_ensemble
        prep$json,
        extra_future
      )
    }

    raw <- client$gather(futures)
    log_msg("Gathered %d ensemble results", length(raw))

    # Convert to format expected by calc_model_ensemble
    ensemble_results <- lapply(raw, function(r) {
      if (isTRUE(r$success)) {
        list(
          reported_cases = matrix(unlist(r$reported_cases),
                                  nrow = length(r$reported_cases), byrow = TRUE),
          disease_deaths = matrix(unlist(r$disease_deaths),
                                  nrow = length(r$disease_deaths), byrow = TRUE),
          success = TRUE,
          seed = r$seed
        )
      } else {
        list(success = FALSE, seed = r$seed, error = r$error %||% "unknown")
      }
    })
  }

  # --- Stochastic param sims (many configs × many seeds) ---
  if (!is.null(param_configs) && length(param_configs) > 0 && n_stochastic_per > 0L) {
    n_param_sets <- length(param_configs)
    total <- n_param_sets * n_stochastic_per
    log_msg("Dispatching %d stochastic param sims (%d configs x %d sims)...",
            total, n_param_sets, n_stochastic_per)

    # Serialize all param configs
    preps <- lapply(param_configs, .config_to_json_and_matrices)

    # Scatter matrix fields per config (they differ because of psi_jt calibration)
    matrix_futures <- lapply(preps, function(p) client$scatter(p$matrices))

    futures <- vector("list", total)
    task_meta <- vector("list", total)
    idx <- 0L
    for (p in seq_len(n_param_sets)) {
      for (s in seq_len(n_stochastic_per)) {
        idx <- idx + 1L
        stoch_seed <- as.integer((p * 1000L) + s)
        futures[[idx]] <- client$submit(
          mosaic_worker$run_laser_postca,
          as.integer(idx),
          stoch_seed,
          preps[[p]]$json,
          matrix_futures[[p]]
        )
        task_meta[[idx]] <- list(param_idx = p, stoch_idx = s)
      }
    }

    raw <- client$gather(futures)
    log_msg("Gathered %d stochastic results", length(raw))

    # Convert to format expected by plot_model_fit_stochastic_param
    stochastic_results <- lapply(seq_along(raw), function(i) {
      r <- raw[[i]]
      meta <- task_meta[[i]]
      if (isTRUE(r$success)) {
        list(
          param_idx = meta$param_idx,
          stoch_idx = meta$stoch_idx,
          reported_cases = matrix(unlist(r$reported_cases),
                                  nrow = length(r$reported_cases), byrow = TRUE),
          disease_deaths = matrix(unlist(r$disease_deaths),
                                  nrow = length(r$disease_deaths), byrow = TRUE),
          success = TRUE
        )
      } else {
        list(param_idx = meta$param_idx, stoch_idx = meta$stoch_idx,
             success = FALSE, error = r$error %||% "unknown")
      }
    })
  }

  list(
    ensemble_results   = ensemble_results,
    stochastic_results = stochastic_results
  )
}
