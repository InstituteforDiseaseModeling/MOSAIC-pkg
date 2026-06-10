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
# Dask worker count
# =============================================================================

#' Count active Dask workers via Python (reticulate-conversion-safe)
#'
#' \code{length(client$scheduler_info()$workers)} cannot be relied on to count
#' Dask workers — reticulate's auto-conversion of the nested
#' \code{scheduler_info} dict surfaces the workers attribute as an R object
#' whose \code{length()} is the per-worker FIELD count (typically 5 in current
#' Dask versions), not the number of workers. The v0.32.35 fix that branched
#' on \code{inherits(workers, "python.builtin.object")} also fails when the
#' attribute IS auto-converted (the inherits check returns FALSE, the
#' \code{length()} fallback fires, and the wrong count is logged anyway).
#'
#' This helper bypasses the conversion problem entirely: a tiny Python helper
#' calls \code{len()} server-side on the workers dict and returns a plain
#' \code{int}, which \code{py_run_string(..., convert = TRUE)} reliably
#' converts to an R integer.
#'
#' @param client A reticulated \code{dask.distributed.Client} object.
#' @return Integer worker count, or \code{NA_integer_} on error.
#' @keywords internal
.mosaic_count_dask_workers <- function(client) {
  tryCatch({
    py_env <- reticulate::py_run_string(
      paste0(
        "def _mosaic_count_workers(client):\n",
        "    try:\n",
        "        return len(client.scheduler_info().get('workers', {}))\n",
        "    except Exception:\n",
        "        return -1\n"
      ),
      convert = TRUE
    )
    n <- as.integer(py_env$"_mosaic_count_workers"(client))
    if (is.na(n) || n < 0L) NA_integer_ else n
  }, error = function(e) NA_integer_)
}

# =============================================================================
# Dask gather with periodic heartbeat
# =============================================================================

#' Gather Dask futures while emitting periodic progress lines
#'
#' Wraps \code{client$gather(futures)} with a polling loop that wakes up every
#' \code{interval_sec} seconds, counts completed vs pending futures via a
#' single round-trip Python helper, and emits one structured
#' \code{[PROGRESS]} log line per heartbeat. Eliminates the
#' silent-during-gather window that otherwise leads a tailing operator (human
#' or AI) to wrongly conclude the pipeline has hung.
#'
#' Note: an earlier version used \code{dask.distributed.wait(timeout=...)} to
#' wake on completion-or-timeout. That API raises \code{TimeoutError} on
#' timeout (it does NOT return a \code{DoneAndNotDoneFutures} as the docs
#' suggest at first read), which aborted the gather on the very first
#' heartbeat. We now poll \code{future.status} from Python in a single batched
#' call (cheap — the status is mirrored locally from scheduler push updates,
#' no round-trip per future) and sleep between iterations.
#'
#' On gather error the helper does NOT swallow — it lets the caller's tryCatch
#' handle diagnostics (first-future status inspection).
#'
#' @param client A reticulated \code{dask.distributed.Client} object.
#' @param futures List of Dask future objects. Empty list returns empty list.
#' @param log_fn Logging function compatible with \code{log_msg(msg, ...)}.
#' @param phase Short slug for the \code{phase=} field of the progress line
#'   (e.g. \code{"calibration_batch"}, \code{"postca_ensemble"}).
#' @param interval_sec Heartbeat interval. Defaults to 30 s — long enough that
#'   the log isn't spammed during fast gathers, short enough that a hung
#'   gather is detected within a minute.
#' @return The list returned by \code{client$gather(futures)}.
#' @keywords internal
.mosaic_gather_with_heartbeat <- function(client, futures, log_fn = log_msg,
                                          phase = "gather",
                                          interval_sec = 30L) {
  total <- length(futures)
  if (total == 0L) return(list())

  # One-shot Python helper: count futures by terminal status in a single call.
  # `future.status` is mirrored locally from scheduler push updates, so this
  # does not produce N reticulate round-trips.
  #
  # Terminal statuses Dask exposes: 'finished', 'error', 'cancelled', 'lost'
  # (worker died holding the future). All four must count toward "done",
  # otherwise the `if (n_pending == 0L) break` below never fires and the
  # heartbeat loop polls forever while the eventual client$gather() raises.
  # 'cancelled' and 'lost' are also surfaced as errored so the [PROGRESS]
  # line distinguishes "completed successfully" from "completed in failure".
  py_env <- reticulate::py_run_string(
    paste0(
      "def _mosaic_count_status(fs):\n",
      "    done = 0\n",
      "    errored = 0\n",
      "    for f in fs:\n",
      "        s = f.status\n",
      "        if s == 'finished':\n",
      "            done += 1\n",
      "        elif s in ('error', 'cancelled', 'lost'):\n",
      "            done += 1\n",
      "            errored += 1\n",
      "    return (done, errored)\n"
    ),
    convert = TRUE
  )
  count_status <- py_env$"_mosaic_count_status"

  start <- Sys.time()
  log_fn("[PROGRESS] phase=%s event=gather_start sims_total=%d interval_sec=%d",
         phase, total, as.integer(interval_sec))

  repeat {
    counts    <- count_status(futures)
    n_done    <- as.integer(counts[[1L]])
    n_errored <- as.integer(counts[[2L]])
    n_pending <- total - n_done
    elapsed   <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    # Count active workers via a pure-Python helper. See
    # .mosaic_count_dask_workers() for the rationale — the obvious
    # length(client$scheduler_info()$workers) returns the per-worker field
    # count (typically 5), NOT the worker count, regardless of whether the
    # workers attribute is still a Python object or already auto-converted.
    n_workers <- .mosaic_count_dask_workers(client)

    log_fn("[PROGRESS] phase=%s event=gather_wait sims_done=%d sims_pending=%d sims_errored=%d elapsed_sec=%.0f workers=%s",
           phase, n_done, n_pending, n_errored, elapsed,
           if (is.na(n_workers)) "?" else as.character(n_workers))

    if (n_pending == 0L) break
    Sys.sleep(as.integer(interval_sec))
  }

  client$gather(futures)
}

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

  # BACKWARD COMPATIBILITY: Renamed parameters (v0.22.16)
  # batch_size → batch_size_adaptive
  if (!is.null(def$calibration$batch_size) && is.null(def$calibration$batch_size_adaptive)) {
    warning("calibration$batch_size is deprecated; use calibration$batch_size_adaptive instead.", call. = FALSE)
    def$calibration$batch_size_adaptive <- def$calibration$batch_size
    def$calibration$batch_size <- NULL
  } else if (!is.null(def$calibration$batch_size)) {
    def$calibration$batch_size <- NULL
  }

  # min_batches → min_batches_adaptive
  if (!is.null(def$calibration$min_batches) && is.null(def$calibration$min_batches_adaptive)) {
    warning("calibration$min_batches is deprecated; use calibration$min_batches_adaptive instead.", call. = FALSE)
    def$calibration$min_batches_adaptive <- def$calibration$min_batches
    def$calibration$min_batches <- NULL
  } else if (!is.null(def$calibration$min_batches)) {
    def$calibration$min_batches <- NULL
  }

  # max_batches → max_batches_adaptive
  if (!is.null(def$calibration$max_batches) && is.null(def$calibration$max_batches_adaptive)) {
    warning("calibration$max_batches is deprecated; use calibration$max_batches_adaptive instead.", call. = FALSE)
    def$calibration$max_batches_adaptive <- def$calibration$max_batches
    def$calibration$max_batches <- NULL
  } else if (!is.null(def$calibration$max_batches)) {
    def$calibration$max_batches <- NULL
  }

  # target_r2 → target_r2_ess → target_r2_adaptive (two-step deprecation chain)
  if (!is.null(def$calibration$target_r2) && is.null(def$calibration$target_r2_adaptive)) {
    warning("calibration$target_r2 is deprecated; use calibration$target_r2_adaptive instead.", call. = FALSE)
    def$calibration$target_r2_adaptive <- def$calibration$target_r2
    def$calibration$target_r2 <- NULL
  } else if (!is.null(def$calibration$target_r2)) {
    def$calibration$target_r2 <- NULL
  }
  if (!is.null(def$calibration$target_r2_ess) && is.null(def$calibration$target_r2_adaptive)) {
    warning("calibration$target_r2_ess is deprecated; use calibration$target_r2_adaptive instead.", call. = FALSE)
    def$calibration$target_r2_adaptive <- def$calibration$target_r2_ess
    def$calibration$target_r2_ess <- NULL
  } else if (!is.null(def$calibration$target_r2_ess)) {
    def$calibration$target_r2_ess <- NULL
  }

  # max_predictive_batch → max_batch_predictive
  if (!is.null(def$calibration$max_predictive_batch) && is.null(def$calibration$max_batch_predictive)) {
    warning("calibration$max_predictive_batch is deprecated; use calibration$max_batch_predictive instead.", call. = FALSE)
    def$calibration$max_batch_predictive <- def$calibration$max_predictive_batch
    def$calibration$max_predictive_batch <- NULL
  } else if (!is.null(def$calibration$max_predictive_batch)) {
    def$calibration$max_predictive_batch <- NULL
  }

  # max_simulations → max_simulations_total
  if (!is.null(def$calibration$max_simulations) && is.null(def$calibration$max_simulations_total)) {
    warning("calibration$max_simulations is deprecated; use calibration$max_simulations_total instead.", call. = FALSE)
    def$calibration$max_simulations_total <- def$calibration$max_simulations
    def$calibration$max_simulations <- NULL
  } else if (!is.null(def$calibration$max_simulations)) {
    def$calibration$max_simulations <- NULL
  }

  # TYPE VALIDATION
  stopifnot(
    "parallel$enable must be logical" =
      is.logical(def$parallel$enable) && length(def$parallel$enable) == 1,
    "parallel$n_cores must be positive integer" =
      is.numeric(def$parallel$n_cores) && def$parallel$n_cores > 0,
    "parallel$type must be 'PSOCK' or 'FORK'" =
      def$parallel$type %in% c("PSOCK", "FORK"),
    "calibration$max_simulations_total must be positive integer" =
      is.numeric(def$calibration$max_simulations_total) && def$calibration$max_simulations_total > 0,
    "calibration$batch_size_adaptive must be positive integer" =
      is.numeric(def$calibration$batch_size_adaptive) && def$calibration$batch_size_adaptive > 0,
    "calibration$min_batches_adaptive must be positive integer" =
      is.numeric(def$calibration$min_batches_adaptive) && def$calibration$min_batches_adaptive > 0,
    "calibration$max_batches_adaptive must be positive integer" =
      is.numeric(def$calibration$max_batches_adaptive) && def$calibration$max_batches_adaptive > 0,
    "calibration$target_r2_adaptive must be in [0, 1]" =
      is.numeric(def$calibration$target_r2_adaptive) &&
      def$calibration$target_r2_adaptive >= 0 && def$calibration$target_r2_adaptive <= 1,
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

  # Check batch_size_adaptive is reasonable
  if (def$calibration$batch_size_adaptive < .MOSAIC_MIN_BATCH_SIZE ||
      def$calibration$batch_size_adaptive > .MOSAIC_MAX_BATCH_SIZE) {
    stop("calibration$batch_size_adaptive (", def$calibration$batch_size_adaptive,
         ") must be between ", .MOSAIC_MIN_BATCH_SIZE, " and ",
         .MOSAIC_MAX_BATCH_SIZE, call. = FALSE)
  }

  # Check max_simulations_total is reasonable
  if (def$calibration$max_simulations_total < .MOSAIC_MIN_SIMULATIONS ||
      def$calibration$max_simulations_total > .MOSAIC_MAX_SIMULATIONS) {
    stop("calibration$max_simulations_total (", def$calibration$max_simulations_total,
         ") must be between ", .MOSAIC_MIN_SIMULATIONS, " and ",
         .MOSAIC_MAX_SIMULATIONS, call. = FALSE)
  }

  # Check batch_size_adaptive < max_simulations_total
  if (def$calibration$batch_size_adaptive >= def$calibration$max_simulations_total) {
    stop("calibration$batch_size_adaptive (", def$calibration$batch_size_adaptive,
         ") must be less than calibration$max_simulations_total (",
         def$calibration$max_simulations_total, ")", call. = FALSE)
  }

  # LOGICAL CONSISTENCY
  if (def$calibration$min_batches_adaptive > def$calibration$max_batches_adaptive) {
    stop("calibration$min_batches_adaptive (", def$calibration$min_batches_adaptive,
         ") must be <= calibration$max_batches_adaptive (", def$calibration$max_batches_adaptive, ")",
         call. = FALSE)
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

#' Resolve Active Subset/Weight Columns for Posterior Construction
#'
#' When \code{control$predictions$optimize_subset = TRUE} and the run produced
#' \code{is_best_subset_opt} / \code{weight_best_opt} columns, those are the
#' canonical subset for posterior work. Otherwise the tier-selected columns
#' (\code{is_best_subset} / \code{weight_best}) are canonical.
#'
#' The \code{any(results$is_best_subset_opt)} guard handles the edge case where
#' \code{optimize_subset = TRUE} but the optimizer silently failed (e.g.,
#' \code{stability_flag} + \code{optimal_n == 0}) — in which case we fall back
#' to the tier subset.
#'
#' @param results data.frame with at least \code{is_best_subset} and
#'   \code{weight_best} columns, optionally \code{is_best_subset_opt} and
#'   \code{weight_best_opt}.
#' @param control MOSAIC control list.
#' @return Named list with \code{subset_col}, \code{weight_col}, and
#'   \code{source} ("tier" or "optimized").
#' @noRd
.mosaic_active_subset_cols <- function(results, control) {
  use_opt <- isTRUE(control$predictions$optimize_subset) &&
             "is_best_subset_opt" %in% names(results) &&
             "weight_best_opt"    %in% names(results) &&
             isTRUE(any(as.logical(results$is_best_subset_opt), na.rm = TRUE))
  list(
    subset_col = if (use_opt) "is_best_subset_opt" else "is_best_subset",
    weight_col = if (use_opt) "weight_best_opt"    else "weight_best",
    source     = if (use_opt) "optimized"          else "tier"
  )
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
    r2_ess = NA_real_,
    calibration_done = FALSE,
    ess_history = list(),
    ess_tracking = list(),
    param_names_est = param_names_est,
    converged = FALSE,
    predictive_done = FALSE,
    mode = nspec$mode,
    fixed_target = nspec$fixed_target,
    # Track batches within each phase (used for predictive batch limits)
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
#' containing only what is needed for external monitoring.
#' Uses atomic write via tempfile + rename.
#'
#' @noRd
.mosaic_save_state <- function(state, path) {
  # Extract only the fields needed for monitoring
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
    ess_regression_r2 = if (!is.na(state$r2_ess)) round(state$r2_ess, 4) else NULL,
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

# =============================================================================
# RESUME SUPPORT
# =============================================================================
#
# Resuming an interrupted calibration treats the per-sim parquet shards in
# 2_calibration/samples/ as the single source of truth. Because each sim's
# parameters are a deterministic function of seed = sim_id (content-addressed
# seeds; see .mosaic_run_simulation_worker), a resumed run must continue from
# max(sim_id on disk) + 1 — never count + 1 — or it would regenerate existing
# draws and double-count mass in the post-hoc importance weights.
#
# Decision-relevant state that cannot be recovered from shards alone
# (ess_tracking, phase, batch counters, convergence flags) is persisted each
# batch to an internal RDS checkpoint (resume_checkpoint.rds). This is distinct
# from the slim, human-facing run_state.json monitoring file.

#' Scan Calibration Samples Directory for Resume
#'
#' Inventories the per-sim parquet shards to support resume. Sweeps orphaned
#' atomic-write temp files left by a hard crash mid-rename, validates each shard
#' is readable with at least one row, and quarantines unreadable shards. Returns
#' the sorted valid sim IDs and the high-water-mark.
#'
#' @param dir_cal_samples Path to 2_calibration/samples
#' @param quarantine Logical; move unreadable shards to samples/.quarantine/
#' @return list(ids = sorted integer vector, watermark = integer, n = integer)
#' @noRd
.mosaic_resume_scan <- function(dir_cal_samples, quarantine = TRUE) {
  empty <- list(ids = integer(0), watermark = 0L, n = 0L)
  if (is.null(dir_cal_samples) || !dir.exists(dir_cal_samples)) return(empty)

  # Sweep orphaned atomic-write temp files (.mosaic_tmp_*) from an interrupted
  # rename. They never match the sim_*.parquet glob but are dead weight.
  tmp_files <- list.files(dir_cal_samples, pattern = "^\\.mosaic_tmp_",
                          all.files = TRUE, full.names = TRUE)
  if (length(tmp_files)) {
    unlink(tmp_files, force = TRUE)
    log_msg("[RESUME] Swept %d orphaned temp file(s)", length(tmp_files))
  }

  files <- list.files(dir_cal_samples, pattern = "^sim_.*\\.parquet$",
                      full.names = FALSE)
  if (!length(files)) return(empty)

  # Validate each shard by actually READING its data (not just parquet footer
  # metadata): this decodes the data pages, so a torn/byte-corrupt shard throws,
  # and we additionally require the load-bearing columns (sim, likelihood) to be
  # present with a numeric likelihood. A footer-valid but data-corrupt or
  # schema-wrong shard would otherwise survive the scan and abort/poison the
  # downstream combine. Shards are one row, so the read is cheap.
  bad    <- character(0)
  ok_ids <- integer(0)
  for (f in files) {
    parsed <- .mosaic_parse_sim_ids(f)
    n_row <- tryCatch({
      df <- as.data.frame(arrow::read_parquet(file.path(dir_cal_samples, f)))
      if (!all(c("sim", "likelihood") %in% names(df))) NA_integer_
      else if (!is.numeric(df$likelihood)) NA_integer_
      else nrow(df)
    }, error = function(e) NA_integer_)
    if (is.na(n_row) || n_row < 1L || !length(parsed)) {
      bad <- c(bad, f)
    } else {
      ok_ids <- c(ok_ids, parsed)
    }
  }

  if (length(bad)) {
    if (quarantine) {
      qdir <- file.path(dir_cal_samples, ".quarantine")
      dir.create(qdir, recursive = TRUE, showWarnings = FALSE)
      # Clear any prior-quarantine collisions so the rename can't silently fail.
      targets <- file.path(qdir, bad)
      unlink(targets[file.exists(targets)], force = TRUE)
      sources <- file.path(dir_cal_samples, bad)
      moved   <- file.rename(sources, targets)
      # If a rename failed (e.g. cross-device .quarantine), the bad shard would
      # otherwise remain in the live sim_*.parquet glob and abort the combine.
      # Delete the unreadable source so it can never re-enter the pool.
      if (any(!moved)) unlink(sources[!moved], force = TRUE)
    }
    # Name the affected shards and the consequence: in auto mode these draws are
    # NOT regenerated (the frontier moves past the watermark), so the pool shrinks
    # by this many; in fixed mode the missing ids are re-run to hit the target.
    log_msg("[RESUME] Quarantined %d unreadable/invalid shard(s) to %s (corrupt, or missing sim/likelihood column): %s",
            length(bad),
            if (quarantine) file.path(basename(dir_cal_samples), ".quarantine") else "(not moved)",
            paste(utils::head(bad, 10L), collapse = ", "))
  }

  ok_ids <- sort(unique(ok_ids))
  if (!length(ok_ids)) return(empty)
  list(ids = ok_ids, watermark = max(ok_ids), n = length(ok_ids))
}

#' Save Internal Resume Checkpoint
#'
#' Persists the decision-relevant calibration state to RDS so an interrupted run
#' resumes with bit-identical stop/continue/phase decisions. Internal durability
#' state, distinct from the slim run_state.json monitoring file. Atomic write;
#' failures warn but never abort the run in progress.
#'
#' @noRd
.mosaic_save_checkpoint <- function(state, checkpoint_file) {
  ckpt <- list(
    schema_version      = 1L,
    total_sims_run      = state$total_sims_run,
    batch_number        = state$batch_number,
    phase               = state$phase,
    phase_batch_count   = state$phase_batch_count,
    calib_batches       = state$calib_batches,
    r2_ess              = state$r2_ess,
    calibration_done    = state$calibration_done,
    predictive_done     = state$predictive_done,
    converged           = state$converged,
    ess_tracking        = state$ess_tracking,
    batch_success_rates = state$batch_success_rates,
    batch_sizes_used    = state$batch_sizes_used
  )
  tryCatch(
    .mosaic_atomic_write(ckpt, checkpoint_file, saveRDS),
    error = function(e)
      log_warn("[RESUME] failed to write resume checkpoint: %s", conditionMessage(e))
  )
  invisible(checkpoint_file)
}

#' Load Internal Resume Checkpoint
#' @noRd
.mosaic_load_checkpoint <- function(checkpoint_file) {
  if (is.null(checkpoint_file) || !file.exists(checkpoint_file)) return(NULL)
  tryCatch(readRDS(checkpoint_file), error = function(e) {
    log_warn("[RESUME] resume checkpoint unreadable (%s); reconstructing from shards",
            conditionMessage(e))
    NULL
  })
}

#' Reconstruct Calibration State From Disk for Resume
#'
#' Overlays the fresh-init `state` with state recovered from the shards on disk
#' (and the resume checkpoint if present). Always sets total_sims_run from the
#' disk watermark so the next sim_id never collides with an existing draw.
#'
#' @param state Fresh state from .mosaic_init_state()
#' @param dirs Directory list
#' @param control Control list
#' @param param_names_est Estimated-parameter names
#' @return Reconstructed state (unchanged if no shards exist → fresh run)
#' @noRd
.mosaic_reconstruct_state <- function(state, dirs, control, param_names_est) {
  scan <- .mosaic_resume_scan(dirs$cal_samples)

  if (scan$n == 0L) {
    log_msg("[RESUME] No existing shards in %s — starting fresh run", dirs$cal_samples)
    return(state)
  }

  # Disk watermark is authoritative for the sim_id frontier (avoids seed reuse).
  state$total_sims_run        <- scan$watermark
  state$total_sims_successful <- scan$n

  ckpt <- .mosaic_load_checkpoint(file.path(dirs$cal_state, "resume_checkpoint.rds"))

  if (!is.null(ckpt)) {
    # ---- Exact restore from checkpoint -------------------------------------
    state$batch_number        <- ckpt$batch_number        %||% state$batch_number
    state$phase               <- ckpt$phase               %||% state$phase
    state$phase_batch_count   <- ckpt$phase_batch_count   %||% state$phase_batch_count
    state$calib_batches       <- ckpt$calib_batches       %||% state$calib_batches
    state$r2_ess              <- ckpt$r2_ess              %||% state$r2_ess
    state$calibration_done    <- isTRUE(ckpt$calibration_done)
    state$predictive_done     <- isTRUE(ckpt$predictive_done)
    state$converged           <- isTRUE(ckpt$converged)
    state$ess_tracking        <- ckpt$ess_tracking        %||% list()
    state$batch_success_rates <- ckpt$batch_success_rates %||% numeric()
    state$batch_sizes_used    <- ckpt$batch_sizes_used    %||% integer()

    # ess_tracking total_sims is a row COUNT, so compare against the shard
    # count (not the max-id watermark, which would over-fire on failed-sim gaps).
    last_count <- if (length(state$ess_tracking))
      tail(vapply(state$ess_tracking, `[[`, numeric(1), "total_sims"), 1) else 0
    log_msg("[RESUME] Restored checkpoint: %d sims, batch %d, phase '%s', converged=%s",
            state$total_sims_run, state$batch_number, state$phase, isTRUE(state$converged))

    # Refresh ESS/convergence from the ACTUAL on-disk pool whenever it differs
    # from what the checkpoint was certified against:
    #   - more shards than the checkpoint (a partial batch wrote after it), or
    #   - FEWER shards (some were quarantined on this scan) — in which case a
    #     stale converged=TRUE no longer matches the pool, so clear it and let
    #     the ESS check re-derive convergence rather than trusting the flag.
    if (identical(state$mode, "auto") && scan$n != last_count) {
      if (state$converged && scan$n < last_count) state$converged <- FALSE
      state <- .mosaic_ess_check_update_state(state, dirs, param_names_est, control)
    }

  } else if (identical(state$mode, "auto")) {
    # ---- Lightweight bootstrap (no checkpoint; e.g. pre-feature run) --------
    bs <- control$calibration$batch_size_adaptive %||% 1000L
    # Estimate batches from the shard COUNT (not the max-id watermark, which
    # over-counts when failed sims leave gaps).
    state$batch_number <- max(1L, as.integer(ceiling(scan$n / max(bs, 1L))))
    # If the recovered pool already exhausted the adaptive batch budget, mark
    # calibration done so resume proceeds to the predictive phase rather than
    # re-running the full adaptive allotment from the resume point.
    max_adaptive <- control$calibration$max_batches_adaptive %||% 8L
    if (state$batch_number >= max_adaptive) state$calibration_done <- TRUE
    log_msg("[RESUME] No checkpoint; bootstrapping from %d shards (watermark %d, batch %d%s)",
            scan$n, scan$watermark, state$batch_number,
            if (isTRUE(state$calibration_done)) ", calibration budget already met" else "")
    state <- .mosaic_ess_check_update_state(state, dirs, param_names_est, control)

  } else {
    log_msg("[RESUME] Fixed mode: %d shards present (watermark %d)", scan$n, scan$watermark)
  }

  state
}

#' Is a laser-cholera Version Pre-v0.13?
#'
#' Returns TRUE for engine versions before 0.13.0, which used the raw
#' disease_deaths deaths-likelihood scale (v0.13 switched to rho_deaths-adjusted
#' reported_deaths). Tolerant of PEP440 suffixes (e.g. "0.12rc1", "0.13.0.dev1",
#' "0.13.0+local"): each dotted component is reduced to its leading integer.
#' Returns NA when the version cannot be parsed to at least major.minor.
#'
#' @noRd
.mosaic_lc_pre013 <- function(v) {
  if (is.null(v) || length(v) != 1L || is.na(v) || !nzchar(v)) return(NA)
  # Reduce each dotted component to its leading run of digits ("12rc1" -> 12).
  parts <- strsplit(v, "\\.")[[1]]
  nums  <- suppressWarnings(as.integer(sub("^([0-9]+).*$", "\\1", parts)))
  if (length(nums) < 2L || is.na(nums[1]) || is.na(nums[2])) return(NA)
  nums[1] < 1L && nums[2] < 13L
}

#' Deaths-Scale Compatibility of Two laser-cholera Versions
#'
#' The v0.12 -> v0.13 transition flipped the deaths-likelihood scale, so resuming
#' across that boundary mixes incompatible scales in the on-disk shards. Two
#' versions on the SAME side of the boundary are compatible (even if they differ).
#'
#' @return "incompatible" if the versions straddle the v0.13 boundary,
#'   "compatible" if both are on the same side, "unknown" if either cannot be
#'   classified.
#' @noRd
.mosaic_lc_deaths_scale <- function(persisted, current) {
  pp <- .mosaic_lc_pre013(persisted)
  pc <- .mosaic_lc_pre013(current)
  if (is.na(pp) || is.na(pc)) return("unknown")
  if (!identical(pp, pc)) return("incompatible")
  "compatible"
}

#' R-side Likelihood Implementation Version
#'
#' Identifies the numeric behaviour of the R \code{calc_model_likelihood()}
#' pipeline. Bump this string whenever a change alters the likelihood VALUES it
#' produces (e.g. the v0.22.20-21 N_obs shape-term normalization) so that resume
#' refuses to pool shards scored by an incompatible likelihood implementation.
#' @noRd
.mosaic_likelihood_impl_version <- function() "R/v0.22.21"

#' Likelihood-Value Provenance Descriptor
#'
#' Captures WHO computed the likelihood stored in each shard, and the version of
#' that implementation, so resume can refuse to pool incomparable likelihoods.
#' On the local backend the likelihood is computed in R by
#' \code{calc_model_likelihood()} on the orchestrator, so
#' \code{engine = "R"} and the version is the R implementation tag.
#' On the Dask backend (Phase 3 / PR #111) the likelihood is computed
#' on-worker by laser-cholera's ported \code{calc_model_likelihood} module,
#' so \code{engine = "python"} and the version is the laser-cholera engine
#' version passed in via \code{lc_version}.
#'
#' Resume then automatically refuses to pool R-scored and Python-scored
#' shards (or shards from different engine versions) since the shard files
#' themselves do not record provenance.
#'
#' @param use_dask Logical; TRUE if the run uses the Dask backend
#'   (engine = "python"), FALSE for the local backend (engine = "R").
#' @param lc_version Current laser-cholera version. Used as the impl_version
#'   when engine is "python"; ignored when engine is "R".
#' @return list(engine, impl_version)
#' @noRd
.mosaic_likelihood_provenance <- function(use_dask = FALSE, lc_version = NA_character_) {
  engine <- if (isTRUE(use_dask)) "python" else "R"
  list(
    engine       = engine,
    impl_version = if (identical(engine, "python")) as.character(lc_version)
                   else .mosaic_likelihood_impl_version()
  )
}

#' Verify Resume Inputs Match the Interrupted Run
#'
#' On resume, the incoming config/priors must match those persisted in 1_inputs/.
#' Changing the prior or likelihood target makes existing shards incomparable to
#' new draws, so a mismatch is a hard error. Comparison uses the same serializer
#' that wrote the files (byte-exact for identical inputs); if serialization
#' cannot be performed the check downgrades to a warning rather than blocking.
#'
#' Also guards the laser-cholera engine version: resuming across the v0.12 ->
#' v0.13 deaths-likelihood-scale boundary is a hard error (the on-disk shards
#' would mix incompatible scales). When \code{control} is supplied, the
#' likelihood target (\code{control$likelihood}) is also compared, since it is
#' not part of config.json/priors.json but changing it re-scores draws under a
#' different target.
#'
#' @noRd
.mosaic_resume_check_inputs <- function(dirs, config, priors, control = NULL, use_dask = FALSE) {
  serialize_obj <- function(obj) {
    tmp <- tempfile(fileext = ".json")
    on.exit(unlink(tmp), add = TRUE)
    ok <- tryCatch({
      jsonlite::write_json(obj, tmp, pretty = TRUE, auto_unbox = TRUE, digits = NA)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) return(NA_character_)
    paste(readLines(tmp, warn = FALSE), collapse = "\n")
  }
  check_one <- function(obj, file, label) {
    if (!file.exists(file)) return(invisible())
    incoming  <- serialize_obj(obj)
    persisted <- tryCatch(paste(readLines(file, warn = FALSE), collapse = "\n"),
                          error = function(e) NA_character_)
    if (is.na(incoming) || is.na(persisted)) {
      warning(sprintf("resume: could not compare %s against 1_inputs/; skipping integrity check",
                      label), call. = FALSE)
      return(invisible())
    }
    if (!identical(incoming, persisted)) {
      stop(sprintf(paste0("resume: supplied %s differs from 1_inputs/%s.json. Changing %s ",
                          "alters the sampling/likelihood target, making the existing shards ",
                          "incomparable to new draws. Resume with identical %s, or start a ",
                          "fresh run in a new directory."),
                   label, label, label, label), call. = FALSE)
    }
    invisible()
  }
  check_one(priors, file.path(dirs$inputs, "priors.json"), "priors")
  check_one(config, file.path(dirs$inputs, "config.json"), "config")

  # Several control fields beyond config/priors change the draws or the stored
  # likelihood and so must also match on resume, or the pooled shards become
  # incomparable. control.json nests the full control under $control (plus a
  # per-run timestamp), so compare the relevant sub-objects rather than
  # byte-comparing the whole file:
  #   - control$likelihood        : weights/sigmas/k_min -> the scoring target
  #   - control$sampling          : which of the ~301 params are sampled; changing
  #                                 it shifts the RNG stream so sample_parameters(
  #                                 seed = sim_id) yields different draws per id
  #   - calibration$n_iterations  : the stored likelihood is log_mean_exp over
  #                                 n_iterations stochastic replicates (and the
  #                                 per-iteration seed embeds n_iterations), so
  #                                 changing it puts new shards on a different scale
  #   - calibration mode (auto/fixed) : switching discards the adaptive state and
  #                                 mixes bookkeeping
  ctrl_file <- file.path(dirs$inputs, "control.json")
  if (!is.null(control) && file.exists(ctrl_file)) {
    persisted_ctrl <- tryCatch(jsonlite::fromJSON(ctrl_file, simplifyVector = TRUE),
                               error = function(e) NULL)
    pc <- tryCatch(persisted_ctrl$control, error = function(e) NULL)
    if (!is.null(pc)) {
      mismatch <- function(incoming, persisted) {
        a <- serialize_obj(incoming); b <- serialize_obj(persisted)
        !is.na(a) && !is.na(b) && !identical(a, b)
      }
      fail_field <- function(field) {
        stop(sprintf(paste0(
          "resume: supplied %s differs from 1_inputs/control.json. Changing it alters the ",
          "draws or the likelihood scale, making new simulations incomparable to the existing ",
          "shards. Resume with the same %s, or start a fresh run in a new directory."),
          field, field), call. = FALSE)
      }
      if (!is.null(control$likelihood) && !is.null(pc$likelihood) &&
          mismatch(control$likelihood, pc$likelihood)) fail_field("control$likelihood")
      if (!is.null(control$sampling) && !is.null(pc$sampling) &&
          mismatch(control$sampling, pc$sampling)) fail_field("control$sampling")
      in_iter <- control$calibration$n_iterations
      pc_iter <- pc$calibration$n_iterations
      if (!is.null(in_iter) && !is.null(pc_iter) &&
          !isTRUE(all.equal(as.numeric(in_iter), as.numeric(pc_iter))))
        fail_field("control$calibration$n_iterations")
      # Tolerant mode classifier (NULL/empty/character -> auto, positive number
      # -> fixed); robust to how a NULL n_simulations round-trips through JSON,
      # which .mosaic_normalize_n_sims would reject with an error.
      mode_of <- function(n) {
        if (is.null(n) || length(n) == 0L || is.character(n)) return("auto")
        if (is.numeric(n) && length(n) == 1L && is.finite(n) && n > 0) return("fixed")
        "auto"
      }
      in_mode <- mode_of(control$calibration$n_simulations)
      pc_mode <- mode_of(pc$calibration$n_simulations)
      if (!identical(in_mode, pc_mode)) {
        stop(sprintf(paste0(
          "resume: calibration mode changed (persisted '%s' vs supplied '%s'). Switching ",
          "between fixed and adaptive(auto) mode discards the adaptive state and mixes ",
          "incompatible bookkeeping. Resume in the original mode, or start a fresh run."),
          pc_mode, in_mode), call. = FALSE)
      }
    }
  }

  # laser-cholera engine version check: the v0.12 -> v0.13 transition flipped
  # the deaths likelihood scale (raw disease_deaths -> rho_deaths-adjusted
  # reported_deaths). Resuming a pre-v0.13 run on v0.13+ silently mixes
  # incompatible scales in the on-disk shards.
  env_file <- file.path(dirs$inputs, "environment.json")
  persisted_env <- NULL
  persisted_lc <- NA_character_
  if (file.exists(env_file)) {
    persisted_env <- tryCatch(
      jsonlite::fromJSON(env_file, simplifyVector = TRUE),
      error = function(e) NULL
    )
    persisted_lc <- tryCatch(persisted_env$python$pkg_laser_cholera, error = function(e) NA_character_)
    if (is.null(persisted_lc) || length(persisted_lc) != 1L) persisted_lc <- NA_character_
  }
  current_lc <- tryCatch({
    if (reticulate::py_available(initialize = FALSE)) {
      importlib <- reticulate::import("importlib.metadata", delay_load = FALSE)
      as.character(importlib$version("laser-cholera"))
    } else NA_character_
  }, error = function(e) NA_character_)

  have_persisted <- !is.na(persisted_lc) && nzchar(persisted_lc)
  have_current   <- !is.na(current_lc) && nzchar(current_lc)

  if (have_persisted && have_current && persisted_lc != current_lc) {
    # Hard-error only when the two versions straddle the v0.12 -> v0.13 boundary
    # (the deaths-likelihood-scale flip). Two versions on the SAME side (both
    # pre-0.13, or both >= 0.13) keep a compatible deaths scale.
    verdict <- .mosaic_lc_deaths_scale(persisted_lc, current_lc)
    if (identical(verdict, "incompatible")) {
      stop(sprintf(paste0(
        "resume: laser-cholera engine version mismatch (persisted '%s' vs current '%s'). ",
        "The v0.12 -> v0.13 transition flipped the deaths likelihood scale (raw disease_deaths ",
        "-> rho_deaths-adjusted reported_deaths). Resuming would silently mix incompatible ",
        "scales in 2_calibration/samples/. Start a fresh run in a new directory."),
        persisted_lc, current_lc), call. = FALSE)
    } else if (identical(verdict, "unknown")) {
      warning(sprintf(paste0(
        "resume: could not classify laser-cholera versions (persisted '%s' vs current '%s') ",
        "against the v0.13 deaths-scale boundary; proceeding, but verify both are on the same ",
        "side before trusting the resumed posterior."),
        persisted_lc, current_lc), call. = FALSE)
    } else {
      warning(sprintf(paste0(
        "resume: laser-cholera engine version differs (persisted '%s' vs current '%s'), ",
        "but both are on the same side of the v0.13 deaths-scale boundary so the schema is ",
        "compatible. Minor numerical differences may exist. Proceeding."),
        persisted_lc, current_lc), call. = FALSE)
    }
  } else if (!have_persisted || !have_current) {
    # Surface a SKIPPED guard rather than passing silently: this is the exact
    # condition (pre-feature run with no recorded version, or Python not bound)
    # under which an undetected v0.13 boundary crossing could mix deaths scales.
    reason <- if (!have_persisted && !have_current)
      "no version recorded in 1_inputs/environment.json and laser-cholera not available in this session"
    else if (!have_persisted)
      "no laser-cholera version recorded in 1_inputs/environment.json"
    else
      "laser-cholera not available in this session"
    warning(sprintf(paste0(
      "resume: the laser-cholera engine deaths-scale guard was SKIPPED (%s). If the engine ",
      "crossed the v0.13 boundary since this run started, the resumed posterior could mix ",
      "incompatible deaths scales."), reason), call. = FALSE)
  }

  # Likelihood-value provenance: refuse to pool shards scored by a different
  # likelihood engine/implementation than the current session would produce
  # (R vs on-worker Python scoring on the Dask backend, or an R likelihood-code
  # change that altered values). The shard files do not record provenance, so
  # this comparison against the persisted environment.json is the only line
  # of defence. Absent on pre-feature runs (skipped, like above).
  persisted_prov <- tryCatch(persisted_env$likelihood_provenance, error = function(e) NULL)
  if (!is.null(persisted_prov)) {
    cur_prov <- .mosaic_likelihood_provenance(use_dask, current_lc)
    a <- serialize_obj(cur_prov); b <- serialize_obj(persisted_prov)
    if (!is.na(a) && !is.na(b) && !identical(a, b)) {
      stop(sprintf(paste0(
        "resume: likelihood provenance differs (persisted engine '%s' / impl '%s' vs current ",
        "engine '%s' / impl '%s'). The shards on disk were scored by a different likelihood ",
        "engine or implementation, so pooling them with new draws would mix incomparable ",
        "likelihoods. Start a fresh run in a new directory."),
        persisted_prov$engine %||% "?", persisted_prov$impl_version %||% "?",
        cur_prov$engine, cur_prov$impl_version), call. = FALSE)
    }
  }

  invisible(TRUE)
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
      batch_size = control$calibration$batch_size_adaptive
    ))
  }

  # Predictive batch
  # Require calibration_done before running predictive
  if (isTRUE(state$calibration_done) && !isTRUE(state$predictive_done)) {
    res <- tryCatch({
      calc_bookend_batch_size(
        ess_history = ess_tracking,
        target_ess = control$targets$ESS_param,
        max_total_sims = control$calibration$max_simulations_total,
        target_r_squared = control$calibration$target_r2_adaptive
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

    # Determine batch size: use model prediction if available, else fall back
    # to the adaptive batch size (ensures we never run trivially small batches
    # or stall when the model can't predict).
    floor_size <- control$calibration$batch_size_adaptive

    if (!is.null(res) && res$batch_size > 0) {
      size <- as.integer(res$batch_size)
    } else {
      # Model returned 0 or failed — use floor as fallback
      if (!is.null(res) && !is.null(res$message)) {
        log_msg("Predictive model: %s \u2014 using batch_size_adaptive as fallback", res$message)
      } else if (is.null(res)) {
        log_msg("Predictive batch calculation failed \u2014 using batch_size_adaptive as fallback")
      }
      size <- as.integer(floor_size)
    }

    # Apply floor: never run a predictive batch smaller than batch_size_adaptive
    if (size < floor_size) {
      size <- as.integer(floor_size)
    }

    # Cap predictive batch to avoid multi-hour single batches with no
    # checkpointing. Capped batches run with ESS re-evaluation between each;
    # predictive_done is set by .mosaic_ess_check_update_state() when the
    # ESS gap closes or max_batches_predictive is reached.
    max_pred <- control$calibration$max_batch_predictive
    if (!is.null(max_pred) && size > max_pred) {
      log_msg("  Capping predictive batch: %d \u2192 %d (max_batch_predictive)", size, max_pred)
      size <- as.integer(max_pred)
    }

    return(list(
      phase = "predictive",
      batch_size = size
    ))
  }

  # If we reach here, calibration_done and predictive_done are both TRUE
  # but convergence hasn't been declared. This shouldn't happen in normal
  # operation — the predictive phase should run until convergence or its
  # batch limit. Return batch_size=0 to signal the loop to stop.
  list(
    phase = "predictive",
    batch_size = 0L,
    message = "Predictive phase exhausted without convergence"
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
  # calc_model_ess_parameter requires at least 50 samples
  if (nrow(ess_check_results) < 50) {
    log_msg("  Skipping ESS check: %d simulations (need at least 50)", nrow(ess_check_results))
    return(state)
  }

  # Calculate ESS using specified method from control.
  # Use the same adaptive n_grid as the final post-hoc ESS calculation in
  # run_MOSAIC (n_grid = 100 * (1 + log(ESS_param/100))). A coarser grid
  # over-estimates ESS for tight posteriors, which caused the loop to declare
  # convergence before the finer-grid post-hoc calculation could confirm it.
  ess_target_val <- control$targets$ESS_param %||% 100
  n_grid_adaptive <- as.integer(round(100 * (1 + log(max(ess_target_val, 100) / 100))))

  ess_current <- tryCatch({
    calc_model_ess_parameter(
      results = ess_check_results,
      param_names = param_names_est,
      likelihood_col = "likelihood",
      n_grid = n_grid_adaptive,
      method = control$targets$ESS_method,
      marginal_method = control$targets$ESS_marginal_method %||% "kde",
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
      state$batch_number >= control$calibration$min_batches_adaptive &&
      length(state$ess_tracking) >= control$calibration$min_batches_adaptive) {

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
    state$r2_ess <- r2

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
      log_msg("  Model: ESS = %.2f + %.4f × sqrt(n)  |  ESS regression R² = %.4f (target %.2f) | Est. Sims: %.0f",
              intercept, slope_print, r2_print, control$calibration$target_r2_adaptive, round(est_sims))
    } else {
      log_msg("  Model: ESS = %.2f + %.4f × sqrt(n)  |  ESS regression R² = %.4f (target %.2f) | Est. Sims: N/A%s",
              intercept, slope_print, r2_print, control$calibration$target_r2_adaptive,
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
    r2_converged <- r2 >= control$calibration$target_r2_adaptive && nrow(ess_df) >= min_r2_points
    if (r2_converged) {
      log_msg("  ESS regression R² criterion met with %d data points (min required: %d)",
              nrow(ess_df), min_r2_points)
    } else if (r2 >= control$calibration$target_r2_adaptive && nrow(ess_df) < min_r2_points) {
      log_msg("  ESS regression R² = %.4f >= target, but only %d data point(s) — need >= %d for reliable fit",
              r2, nrow(ess_df), min_r2_points)
    }

    if (r2_converged || state$batch_number >= control$calibration$max_batches_adaptive) {

      # Calculate remaining gap
      current_n <- nrow(ess_check_results)
      remaining_sims <- if (!is.na(est_sims) && est_sims > current_n) {
        est_sims - current_n
      } else {
        0
      }

      # Decide whether to end calibration or continue
      # Check max_batches FIRST to ensure hard limit is enforced
      if (state$batch_number >= control$calibration$max_batches_adaptive) {
        # Hit max batches limit - always exit regardless of R² or gap
        state$calibration_done <- TRUE
        if (r2 < control$calibration$target_r2_adaptive) {
          log_msg("  → Calibration complete: reached max_batches (%d) before ESS regression R² converged (%.4f < %.2f)",
                  control$calibration$max_batches_adaptive, r2, control$calibration$target_r2_adaptive)
        } else {
          log_msg("  → Calibration complete: reached max_batches (%d)",
                  control$calibration$max_batches_adaptive)
        }
        if (remaining_sims > 0) {
          log_msg("    Estimated gap: %.0f sims → proceeding to predictive phase", ceiling(remaining_sims))
        }

      } else if (threshold_ess >= target_ess) {
        # Already at or above target ESS
        # Don't log here - convergence check will announce it
        state$calibration_done <- TRUE

      } else if (remaining_sims > 0 && remaining_sims < control$calibration$batch_size_adaptive) {
        # Small gap remaining - continue calibration instead of transitioning
        log_msg("  → Calibration R² achieved, but gap is small")
        log_msg("    Current: %d sims | Estimated need: %.0f sims | Gap: %.0f sims",
                current_n, round(est_sims), ceiling(remaining_sims))
        log_msg("    Continuing calibration (gap < batch_size_adaptive)")
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

  # Predictive phase: continue running capped batches until ESS gap closes
  # or safety batch limit is reached. Each iteration re-evaluates via
  # calc_bookend_batch_size() in .mosaic_decide_next_batch().
  } else if (identical(state$phase, "predictive") &&
             !is.null(state$phase_batch_count) &&
             state$phase_batch_count >= 1) {

    pred_gap <- NA_real_
    if (length(state$ess_tracking)) {
      cur_thresh <- tail(vapply(state$ess_tracking, `[[`, numeric(1), "threshold_ess"), 1)
      pred_gap <- control$targets$ESS_param - cur_thresh
    }

    max_pred_batches <- control$calibration$max_batches_predictive %||% 10L

    if (!is.na(pred_gap) && pred_gap <= 0) {
      state$predictive_done <- TRUE
      log_msg("  \u2192 Predictive phase complete: ESS gap closed (gap = %.1f)", pred_gap)

    } else if (state$phase_batch_count >= max_pred_batches) {
      state$predictive_done <- TRUE
      log_msg("  \u2192 Predictive batch limit reached (%d/%d)",
              state$phase_batch_count, max_pred_batches)

    } else {
      pred_gap_str <- if (is.na(pred_gap)) "N/A" else sprintf("%.1f", pred_gap)
      log_msg("  \u2192 Predictive batch %d/%d complete, ESS gap = %s \u2014 continuing predictive phase",
              state$phase_batch_count, max_pred_batches, pred_gap_str)
    }
  }

  # Check convergence.
  # Denominator is the total number of sampled parameters (not just those with
  # a valid ESS estimate). Parameters whose KDE returned NA are counted as
  # "not converged" — otherwise silent KDE failures would shrink the
  # denominator and falsely inflate prop_converged, causing the loop to exit
  # before the post-hoc ESS calculation can confirm the 0.975 target.
  n_total        <- length(param_names_est)
  n_converged    <- sum(ess_current$ess_marginal >= control$targets$ESS_param, na.rm = TRUE)
  n_ess_computed <- sum(!is.na(ess_current$ess_marginal))
  prop_converged <- if (n_total > 0L) n_converged / n_total else 0

  if (prop_converged >= control$targets$ESS_param_prop) {
    # Only declare convergence after min_batches to ensure sufficient exploration.
    # Early batches can spuriously meet ESS targets with small samples.
    if (state$batch_number >= control$calibration$min_batches_adaptive) {
      state$converged <- TRUE
      log_msg("  → CONVERGENCE ACHIEVED: %.1f%% of parameters at ESS >= %.0f (%d/%d; %d ESS computed)",
              prop_converged * 100, control$targets$ESS_param,
              n_converged, n_total, n_ess_computed)
    } else {
      log_msg("  ESS criterion met (%.1f%% >= %.0f) but batch %d < min_batches %d — continuing",
              prop_converged * 100, control$targets$ESS_param,
              state$batch_number, control$calibration$min_batches_adaptive)
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
    eta = eta,
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
    "N_j_initial", "longitude", "latitude",
    # Likelihood-control + epidemic_peaks + analyzer toggle (issue #100,
    # plan §3.1, §3.4.1). Only present when injected by
    # .mosaic_inject_likelihood_settings() on the Dask path; absent on
    # the local PSOCK/FORK path (no-op).
    "calc_likelihood",
    "weight_cases", "weight_deaths",
    "weights_time", "weights_location",
    "nb_k_min_cases", "nb_k_min_deaths",
    "weight_peak_timing", "weight_peak_magnitude",
    "weight_cumulative_total", "weight_wis",
    "sigma_peak_time", "sigma_peak_log",
    "epidemic_peaks"
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

#' Sample, clamp, and JSON-serialize one sim's parameters for the Dask path.
#'
#' Per-sim worker function extracted from .mosaic_run_batch_dask() so the
#' submission sample-and-serialize loop can run in parallel via
#' parallel::mclapply(). Returns a list:
#'   $params : the full sampled-config list (NULL on failure)
#'   $json   : the per-sim JSON string ready for the Coiled worker
#'             (NULL on failure)
#'   $error  : NULL on success; a character message on failure
#'
#' Reproducibility: sample_parameters() takes `seed = sim_id` and is
#' internally seeded, so each sim's output depends ONLY on (sim_id,
#' priors, config, sampling_args). Parallel execution produces output
#' identical to serial execution regardless of fork ordering.
#'
#' Error handling: the entire body is wrapped in tryCatch so any failure
#' (in sample_parameters, in the guardrail clamps, in toJSON) is
#' captured and returned as $error. Callers MUST NOT emit warnings
#' inside this function — fork-side warnings with `immediate. = FALSE`
#' are swallowed by mclapply. Surface $error in the parent instead.
#'
#' Constraint: do NOT touch the parent's reticulate/Coiled state from
#' inside this function. The parent's `client` and `mosaic_worker`
#' objects are inherited via fork but are not safe to use in children.
#' @noRd
.mosaic_sample_and_serialize <- function(sim_id, PATHS, priors, config,
                                         sampling_args) {
  tryCatch({
    params <- sample_parameters(
      PATHS       = PATHS,
      priors      = priors,
      config      = config,
      seed        = sim_id,
      sample_args = sampling_args,
      verbose     = FALSE,
      validate    = FALSE
    )

    # Guardrails: clamp transmission parameters into valid ranges.
    if (!is.null(params$beta_j0_tot)) params$beta_j0_tot <- pmax(params$beta_j0_tot, 1e-10)
    if (!is.null(params$beta_j0_hum)) params$beta_j0_hum <- pmax(params$beta_j0_hum, 0)
    if (!is.null(params$beta_j0_env)) params$beta_j0_env <- pmax(params$beta_j0_env, 0)
    if (!is.null(params$p_beta))      params$p_beta      <- pmin(pmax(params$p_beta, 1e-6), 1 - 1e-6)
    if (!is.null(params$tau_i))       params$tau_i       <- pmin(pmax(params$tau_i, 0), 1)

    # Serialize the sampled (non-base) fields for shipping to the Coiled worker.
    json <- jsonlite::toJSON(
      .extract_sampled_params(params),
      auto_unbox = TRUE,
      digits     = NA
    )

    list(params = params, json = as.character(json), error = NULL)
  }, error = function(e) {
    list(params = NULL, json = NULL, error = conditionMessage(e))
  })
}


#' Build and write a single Dask-path parquet shard.
#'
#' Per-sim worker function extracted from .mosaic_run_batch_dask() so the
#' write loop can run in parallel via parallel::mclapply(). Returns:
#'   TRUE                   : shard wrote successfully
#'   character message      : a failure reason for the parent to log
#'
#' This dual-type return lets the parent distinguish success from failure
#' AND surface a diagnostic — `warning()` calls inside an mclapply fork
#' with `immediate. = FALSE` are silently swallowed, so we propagate
#' failure reasons through the return value instead.
#'
#' Side-effect: writes one parquet file at dirs$cal_samples/sim_NNNNNNN.parquet
#' on success.
#'
#' Pure compute given (sim_id, res). No shared mutable state, no I/O
#' beyond the single parquet write, no Python calls — safe to invoke in
#' parallel from forked workers. The caller (parent process) is
#' responsible for freeing result_lookup / params_list entries after
#' each chunk completes.
#'
#' Constraint: do NOT touch the parent's reticulate/Coiled state from
#' inside this function. The parent's `client` and `mosaic_worker`
#' objects are inherited via fork but are not safe to use in children.
#' @noRd
.mosaic_write_one_shard_dask <- function(sim_id, res, n_iterations,
                                         param_names_all, param_lookup,
                                         config, dirs, control) {
  tryCatch({
    # Skip if param sampling failed (no future submitted) or worker errored
    if (is.null(res) || !isTRUE(res$success)) {
      err_msg <- if (!is.null(res) && !is.null(res$error)) res$error else "unknown error"
      stop("failed on worker: ", err_msg)
    }

    # Re-inject base_config fields stripped by .extract_sampled_params() that
    # are needed for ISO-suffixed column generation. See
    # test-dask_worker_schema_parity.R for the contract.
    params <- res$params
    if (is.null(params)) {
      stop("worker returned no params dict (engine may be < 0.13)")
    }
    params$location_name <- config$location_name
    params$N_j_initial   <- config$N_j_initial

    # Fast path: .mosaic_extract_param_row() uses the precomputed
    # param_lookup to extract values directly by ISO suffix, avoiding the
    # O(n^2) c()-accumulation inside convert_config_to_matrix(). At 47
    # countries / ~60 sampled fields, this saves ~3-5 ms per sim — adds
    # up to ~50 s wall over a 100K-sim batch on 8 cores.
    raw_params <- .mosaic_extract_param_row(params, param_lookup,
                                            length(param_names_all))
    names(raw_params) <- param_names_all

    # Collapse multi-iter to a single row via log-mean-exp (matches the
    # local path: see run_MOSAIC.R ~285-300).
    iter_list  <- res$iterations
    n_iter_got <- length(iter_list)
    if (n_iter_got == 0L) stop("worker returned no iterations")

    lls <- vapply(iter_list, function(it) {
      v <- suppressWarnings(as.numeric(it$likelihood))
      if (length(v) == 1L) v else NA_real_
    }, numeric(1))

    collapsed_ll <- if (n_iter_got > 1L) {
      valid_lls <- lls[is.finite(lls)]
      if (length(valid_lls)) calc_log_mean_exp(valid_lls) else NA_real_
    } else {
      lls[1L]
    }

    seed_iter_1 <- as.integer((sim_id - 1L) * n_iterations + 1L)

    row_df <- data.frame(
      sim        = as.integer(sim_id),
      iter       = 1L,
      seed_sim   = as.integer(sim_id),
      seed_iter  = seed_iter_1,
      likelihood = collapsed_ll
    )
    for (pname in param_names_all) {
      row_df[[pname]] <- as.numeric(raw_params[pname])
    }

    out_file <- file.path(dirs$cal_samples,
                          sprintf("sim_%07d.parquet", sim_id))
    .mosaic_write_parquet(row_df, out_file, control$io)
    if (!file.exists(out_file)) stop("parquet write succeeded but file not on disk")
    TRUE
  }, error = function(e) conditionMessage(e))
}


#' Run One Dask Batch (sample → submit → gather → write parquets)
#'
#' Replaces .mosaic_run_batch() for Dask execution. Workers compute the
#' likelihood on-worker (issue #101) and return per-iter scalar likelihoods
#' plus a sim-level params dict; this function writes one parquet row per
#' sim (likelihood collapsed across iterations via calc_log_mean_exp when
#' n_iterations > 1, mirroring the local-path semantics in run_MOSAIC.R).
#'
#' Two R-side phases run in parallel via parallel::mclapply():
#'
#'   - Submission (sample_parameters + JSON serialize) — chunked in
#'     batches of `submit_chunk_size` (default 1000) so that one
#'     `client$map()` RPC handles each chunk's submissions to Coiled.
#'   - Post-gather parquet write — chunked in batches of
#'     `write_chunk_size` (default 1000) for fork amortization,
#'     progress logging, parent-side memory frees, and Dask scheduler
#'     health pings.
#'
#' Both phases honor `control$parallel$n_cores` for the parallelism
#' budget; default falls back to serial. Returns a logical vector of
#' per-simulation success indicators (TRUE = parquet written
#' successfully).
#' @noRd
.mosaic_run_batch_dask <- function(sim_ids, n_iterations, priors, config, PATHS,
                                    sampling_args, dirs, param_names_all,
                                    param_lookup, control,
                                    client, base_config_future,
                                    mosaic_worker) {

  n_sims <- length(sim_ids)

  # ---------------------------------------------------------------------------
  # 1-3. Sample parameters, serialize, and submit futures (chunked)
  #      Workers start receiving tasks after the first chunk (~seconds) instead
  #      of waiting for all N samples. Chunked client$map() avoids overwhelming
  #      the Dask scheduler with rapid-fire individual submit() calls.
  # ---------------------------------------------------------------------------
  # submit_chunk_size: number of sims per client$map() RPC to Coiled.
  # Controls Dask scheduler load (one RPC per chunk vs. one RPC per sim),
  # NOT local R-side parallelism — that's the inner mclapply over each
  # chunk_indices block. 1000 sits inside Dask's documented sweet spot
  # for client.map() batch sizing
  # (https://docs.dask.org/en/stable/best-practices.html): individual
  # submit() calls are discouraged due to per-call RPC overhead, and
  # batches in the thousands amortize that overhead while keeping the
  # first chunk's futures dispatchable within ~1 s of submission.
  # Not a function of n_cores_parallel — RPC amortization is a network
  # constant, not a CPU-amortization constant.
  submit_chunk_size <- 1000L
  n_chunks <- ceiling(n_sims / submit_chunk_size)

  # R-side parallelism budget. Shared between the submission sample+
  # serialize phase and the post-gather parquet write phase later in
  # this function. Both phases are pure CPU work on the orchestrator
  # host; the caller's control$parallel$n_cores already encodes the
  # right value for whichever machine the orchestrator runs on.
  #
  # NA-safe coercion: %||% only guards NULL, but `detectCores()` can
  # return NA in restricted sandboxes (Docker without /proc/cpuinfo,
  # certain CI VMs). Without this guard, NA propagates through `max()`
  # and bricks `if (n_cores_parallel > 1L)` later with "missing value
  # where TRUE/FALSE needed".
  .raw_n_cores <- suppressWarnings(as.integer(control$parallel$n_cores %||% 1L))
  if (length(.raw_n_cores) != 1L || is.na(.raw_n_cores)) .raw_n_cores <- 1L
  # Cap to LOCAL hardware: this is the orchestrator fork width, never the remote
  # worker count (that is dask_spec$n_workers).
  .local_cores <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  if (is.na(.local_cores) || .local_cores < 1L) .local_cores <- .raw_n_cores
  n_cores_parallel <- max(1L, min(.raw_n_cores, .local_cores))

  log_msg("  Sampling + submitting %d simulations (%d chunks, parallel x %d cores)...",
          n_sims, n_chunks, n_cores_parallel)
  params_list <- vector("list", n_sims)
  futures     <- vector("list", n_sims)
  submit_start <- Sys.time()

  # Log at ~10% intervals (minimum every chunk for small runs)
  log_interval <- max(1L, floor(n_chunks / 10))

  for (chunk_i in seq_len(n_chunks)) {

    # Determine indices for this chunk
    idx_start <- (chunk_i - 1L) * submit_chunk_size + 1L
    idx_end   <- min(chunk_i * submit_chunk_size, n_sims)
    chunk_indices <- idx_start:idx_end

    # Parallel sample + serialize for the chunk. sample_parameters() is
    # deterministically seeded by sim_id, so parallel execution produces
    # output identical to serial regardless of which fork processes each
    # sim. mclapply is POSIX-only; on Windows it silently falls back to
    # serial (safe default).
    chunk_results <- if (n_cores_parallel > 1L) {
      parallel::mclapply(
        chunk_indices,
        function(idx) {
          .mosaic_sample_and_serialize(
            sim_id        = sim_ids[idx],
            PATHS         = PATHS,
            priors        = priors,
            config        = config,
            sampling_args = sampling_args
          )
        },
        mc.cores = n_cores_parallel
      )
    } else {
      lapply(chunk_indices, function(idx) {
        .mosaic_sample_and_serialize(
          sim_id        = sim_ids[idx],
          PATHS         = PATHS,
          priors        = priors,
          config        = config,
          sampling_args = sampling_args
        )
      })
    }

    # Collect results in deterministic order (mclapply preserves order)
    # and build the client$map() submission arrays.
    #
    # Three failure shapes are possible per result entry:
    #   1. `list(params = NULL, json = NULL, error = "...")`
    #      — helper's tryCatch caught an in-fork exception
    #   2. NULL — mclapply sibling-loss: with mc.preschedule = TRUE
    #      (default), if one task in a fork's block crashes, ALL its
    #      siblings in that block return as bare NULL
    #   3. `try-error` — the fork crashed at the mclapply boundary
    #      (rare; typically only if the fork itself segfaults)
    map_indices  <- integer(0)    # positions in params_list/futures
    map_sim_ids  <- integer(0)
    map_jsons    <- character(0)

    for (k in seq_along(chunk_indices)) {
      idx <- chunk_indices[k]
      r   <- chunk_results[[k]]

      if (is.null(r)) {
        warning("sim ", sim_ids[idx], " fork did not deliver a result ",
                "(possibly sibling-loss from mclapply preschedule)",
                call. = FALSE, immediate. = FALSE)
        params_list[[idx]] <- NULL
        next
      }
      if (inherits(r, "try-error")) {
        warning("sim ", sim_ids[idx], " fork crashed: ",
                as.character(attr(r, "condition")$message),
                call. = FALSE, immediate. = FALSE)
        params_list[[idx]] <- NULL
        next
      }
      if (!is.null(r$error)) {
        warning("Param sampling failed for sim ", sim_ids[idx], ": ",
                r$error, call. = FALSE, immediate. = FALSE)
      }

      params_list[[idx]] <- r$params
      if (is.null(r$params) || is.null(r$json)) next

      map_indices <- c(map_indices, idx)
      map_sim_ids <- c(map_sim_ids, as.integer(sim_ids[idx]))
      map_jsons   <- c(map_jsons, r$json)
    }

    # --- Submit entire chunk via client$map() (single scheduler round-trip).
    # The Coiled client lives in the parent process only; submission MUST
    # stay outside the parallel block.
    if (length(map_sim_ids) > 0L) {
      chunk_futures <- client$map(
        mosaic_worker$run_laser_sim,
        as.list(map_sim_ids),
        as.list(rep(as.integer(n_iterations), length(map_sim_ids))),
        as.list(map_jsons),
        as.list(rep(list(base_config_future), length(map_sim_ids)))
      )
      for (fi in seq_along(map_indices)) {
        futures[[ map_indices[fi] ]] <- chunk_futures[[fi]]
      }
    }

    # Compact progress: log at ~10% intervals and at the end
    if (chunk_i %% log_interval == 0L || chunk_i == n_chunks) {
      elapsed <- as.numeric(difftime(Sys.time(), submit_start, units = "secs"))
      pct <- round(100 * idx_end / n_sims)
      log_msg("    %d/%d submitted (%d%%) | %.0fs | %.0f sims/s",
              idx_end, n_sims, pct, elapsed, idx_end / max(elapsed, 0.1))
    }
  }

  valid_futures <- Filter(Negate(is.null), futures)

  # ---------------------------------------------------------------------------
  # 4. Gather results (blocking)
  # ---------------------------------------------------------------------------
  log_msg("  Waiting for %d Dask futures...", length(valid_futures))
  gather_start <- Sys.time()
  gathered <- tryCatch(
    .mosaic_gather_with_heartbeat(
      client     = client,
      futures    = valid_futures,
      log_fn     = log_msg,
      phase      = "calibration_batch"
    ),
    error = function(e) {
      log_msg("  [ERROR] client$gather() failed: %s", conditionMessage(e))
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
  # 5. Build parquet rows from worker-computed likelihoods (issue #101)
  #
  # Workers return per-iter scalar likelihoods alongside a sim-level params
  # dict (the sampled scalars/vectors echoed back, minus matrix fields and
  # location_name — the latter is stripped by .extract_sampled_params()
  # before JSON serialization). We re-inject location_name here so
  # convert_config_to_matrix() emits ISO-suffixed column names (e.g.
  # beta_j0_tot_ETH) instead of falling back to numeric suffixes.
  # ---------------------------------------------------------------------------
  write_start <- Sys.time()
  success_indicators <- logical(n_sims)

  # Parallel parquet write. Per-sim work is pure (no shared mutable state,
  # one file per sim) so it parallelizes cleanly via parallel::mclapply.
  # Re-uses the n_cores_parallel budget computed at the top of the
  # function (same value used by the sample+serialize submission phase).
  log_msg("  Building parquet rows for %d sims (parallel x %d cores)...",
          n_sims, n_cores_parallel)

  # write_chunk_size: number of sims per inner mclapply() call in the
  # write loop. Gates THREE things simultaneously:
  #   1. mclapply fork batch — workers in one chunk share a single
  #      fork round; smaller chunks mean more fork rounds.
  #   2. Parent-side memory free cadence — result_lookup[key] entries
  #      are nulled out only at chunk boundaries (forked workers
  #      inherit the parent's snapshot via COW, so the parent's
  #      drop-as-you-go pattern only works between mclapply calls).
  #   3. Progress log + Dask scheduler health-check cadence — one
  #      "X/N elapsed" line per chunk, one client$scheduler_info()
  #      ping per chunk.
  # Fixed at 1000 to match `submit_chunk_size` for log-line cadence
  # consistency across the two phases. Unlike submit_chunk_size — which
  # is governed by Dask RPC amortization — this one is a fork+progress
  # constant; if it ever wants to diverge (e.g. scale with
  # n_cores_parallel for tighter fork amortization, or with n_sims for
  # progress-line cadence), the two are semantically independent.
  write_chunk_size <- 1000L
  chunks <- split(seq_len(n_sims),
                  ceiling(seq_len(n_sims) / write_chunk_size))

  for (chunk_idxs in chunks) {
    chunk_out <- if (n_cores_parallel > 1L) {
      parallel::mclapply(
        chunk_idxs,
        function(idx) {
          sim_id <- sim_ids[[idx]]
          key    <- as.character(sim_id)
          .mosaic_write_one_shard_dask(
            sim_id, result_lookup[[key]], n_iterations,
            param_names_all, param_lookup, config, dirs, control
          )
        },
        mc.cores = n_cores_parallel
      )
    } else {
      lapply(chunk_idxs, function(idx) {
        sim_id <- sim_ids[[idx]]
        key    <- as.character(sim_id)
        .mosaic_write_one_shard_dask(
          sim_id, result_lookup[[key]], n_iterations,
          param_names_all, param_lookup, config, dirs, control
        )
      })
    }

    # The helper returns TRUE on success, a character error message on
    # failure, NULL on mclapply sibling-loss, or a try-error on fork
    # crash. isTRUE() correctly maps all non-TRUE shapes to FALSE in
    # success_indicators. After accounting for success/failure, surface
    # the failure diagnostic (which would otherwise be lost — fork-side
    # warnings with `immediate. = FALSE` are swallowed by mclapply).
    success_indicators[chunk_idxs] <- vapply(chunk_out, isTRUE, logical(1))

    for (k in seq_along(chunk_idxs)) {
      out <- chunk_out[[k]]
      if (isTRUE(out)) next
      sid <- sim_ids[[chunk_idxs[k]]]
      msg <- if (is.null(out)) {
        "fork did not deliver a result (possibly sibling-loss from mclapply preschedule)"
      } else if (inherits(out, "try-error")) {
        paste0("fork crashed: ", as.character(attr(out, "condition")$message))
      } else if (is.character(out)) {
        out
      } else {
        sprintf("unknown failure shape (%s)", paste(class(out), collapse = "/"))
      }
      warning("sim ", sid, " parquet write failed: ", msg,
              call. = FALSE, immediate. = FALSE)
    }

    # Free parent-side memory for the sims we just wrote
    for (idx in chunk_idxs) {
      key <- as.character(sim_ids[[idx]])
      result_lookup[key] <- list(NULL)
      params_list[idx]   <- list(NULL)
    }

    # Progress + Dask scheduler health check
    written <- max(chunk_idxs)
    elapsed <- as.numeric(difftime(Sys.time(), write_start, units = "secs"))
    log_msg("    Parquet write progress: %d/%d (%.0fs elapsed)",
            written, n_sims, elapsed)
    tryCatch(client$scheduler_info(), error = function(e) {
      log_warn("Dask scheduler ping failed at sim %d: %s",
              written, e$message)
    })
  }

  rm(result_lookup, params_list, futures)
  write_elapsed <- as.numeric(difftime(Sys.time(), write_start, units = "secs"))
  log_msg("  Parquet writing done: %d sims in %.1fs (%.1f sims/s)",
          n_sims, write_elapsed, n_sims / max(write_elapsed, 0.1))
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
#' passed to calc_model_ensemble() and plot_model_ensemble() via
#' their precomputed_results argument.
#'
#' @param client dask.distributed.Client (already connected).
#' @param mosaic_worker Python module (mosaic_dask_worker, already imported).
#' @param param_configs List of config lists for posterior parameter sets (for stochastic sims).
#'   NULL to skip stochastic dispatch.
#' @param n_stochastic_per Integer. Number of stochastic sims per param config.
#' @param log_msg Function. Logging function.
#' @return Named list with $stochastic_results (or NULL if skipped).
#' @keywords internal
.mosaic_postca_dask <- function(client, mosaic_worker,
                                param_configs = NULL,
                                n_stochastic_per = 10L,
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

  stochastic_results  <- NULL

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

    raw <- .mosaic_gather_with_heartbeat(
      client     = client,
      futures    = futures,
      log_fn     = log_msg,
      phase      = "postca_ensemble"
    )
    log_msg("Gathered %d stochastic results", length(raw))

    # Convert to format expected by calc_model_ensemble
    stochastic_results <- lapply(seq_along(raw), function(i) {
      r <- raw[[i]]
      meta <- task_meta[[i]]
      if (isTRUE(r$success)) {
        list(
          param_idx = meta$param_idx,
          stoch_idx = meta$stoch_idx,
          reported_cases = matrix(unlist(r$reported_cases),
                                  nrow = length(r$reported_cases), byrow = TRUE),
          reported_deaths = matrix(unlist(r$reported_deaths),
                                  nrow = length(r$reported_deaths), byrow = TRUE),
          success = TRUE
        )
      } else {
        list(param_idx = meta$param_idx, stoch_idx = meta$stoch_idx,
             success = FALSE, error = r$error %||% "unknown")
      }
    })
  }

  list(
    stochastic_results = stochastic_results
  )
}
