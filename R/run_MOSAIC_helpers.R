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
  def <- mosaic_run_defaults()

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
  if (is.character(n_sims)) {
    v <- tolower(n_sims)
    if (!v %in% c("auto", "algo")) {
      stop("n_sims must be 'auto' or a positive integer, got: ", n_sims,
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
    stop("n_sims must be 'auto' or a positive integer", call. = FALSE)
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
    bfrs_plots_params = file.path(dir_output, "1_bfrs/plots/parameters"),
    bfrs_plots_params_detail = file.path(dir_output, "1_bfrs/plots/parameters/detail"),
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
    fixed_target = nspec$fixed_target
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

  # Calibration phase
  if (identical(state$phase, "calibration") && !isTRUE(state$calibration_done)) {
    return(list(phase = "calibration", batch_size = control$calibration$batch_size))
  }

  # Predictive batch
  if (!isTRUE(state$predictive_done)) {
    res <- tryCatch({
      calc_bookend_batch_size(
        ess_history = ess_tracking,
        target_ess = control$targets$ESS_param,
        reserved_sims = 250L,
        max_total_sims = control$calibration$max_simulations,
        target_r_squared = control$calibration$target_r2
      )
    }, error = function(e) NULL)

    size <- if (is.null(res) || res$batch_size <= 0) {
      500L
    } else {
      as.integer(res$batch_size)
    }

    return(list(phase = "predictive", batch_size = size))
  }

  # Fine-tuning phase (5-tier adaptive)
  gap <- NA_real_
  if (length(ess_tracking)) {
    cur_min <- tail(vapply(ess_tracking, `[[`, numeric(1), "min_ess"), 1)
    gap <- control$targets$ESS_param - cur_min
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

  list(phase = "fine_tuning", batch_size = as.integer(size))
}

#' ESS Check and Update State
#' @noRd
.mosaic_ess_check_update_state <- function(state, dirs, param_names_est, control) {

  # Load all simulation files fresh from disk
  files <- list.files(dirs$bfrs_params, pattern = "^sim_.*\\.parquet$", full.names = TRUE)
  if (!length(files)) return(state)

  log_msg("Checking ESS convergence...")
  load_start <- Sys.time()

  ess_check_results <- tryCatch({
    as.data.frame(data.table::rbindlist(
      lapply(files, function(f) {
        tryCatch({
          data.table::as.data.table(arrow::read_parquet(f))
        }, error = function(e) NULL)
      }),
      use.names = TRUE,
      fill = TRUE
    ))
  }, error = function(e) NULL)

  load_time <- difftime(Sys.time(), load_start, units = "secs")

  if (is.null(ess_check_results) || !nrow(ess_check_results)) {
    return(state)
  }

  log_msg("  Loaded %d simulations in %.1f seconds (%.0f sims/sec)",
          nrow(ess_check_results), as.numeric(load_time),
          nrow(ess_check_results) / max(1, as.numeric(load_time)))

  # Skip ESS calculation if insufficient samples
  # calc_model_ess_parameter requires at least 50 samples for KDE
  if (nrow(ess_check_results) < 50) {
    log_msg("  Skipping ESS check: %d simulations (need at least 50)", nrow(ess_check_results))
    return(state)
  }

  # Calculate ESS
  ess_current <- tryCatch({
    calc_model_ess_parameter(
      results = ess_check_results,
      param_names = param_names_est,
      likelihood_col = "likelihood",
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

  # Calibration R² check
  if (identical(state$phase, "calibration") &&
      !isTRUE(state$calibration_done) &&
      state$calib_batches >= control$calibration$min_batches &&
      length(state$ess_tracking) >= control$calibration$min_batches) {

    ess_df <- data.frame(
      sims = vapply(state$ess_tracking, `[[`, numeric(1), "total_sims"),
      threshold_ess = vapply(state$ess_tracking, `[[`, numeric(1), "threshold_ess")
    )

    ess_lm <- stats::lm(threshold_ess ~ sims, data = ess_df)
    r2 <- summary(ess_lm)$r.squared
    state$calib_batches <- state$calib_batches + 1L
    state$calib_r2 <- r2

    log_msg("Calibration R² = %.4f (target %.2f)", r2, control$calibration$target_r2)

    if (r2 >= control$calibration$target_r2 ||
        state$calib_batches >= control$calibration$max_batches) {
      state$phase <- "predictive"
      state$calibration_done <- TRUE
      log_msg("  → Calibration complete, proceeding to predictive batch")
    }

  } else if (identical(state$phase, "predictive")) {
    state$predictive_done <- TRUE
    state$phase <- "fine_tuning"
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
