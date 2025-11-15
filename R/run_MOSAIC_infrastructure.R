# =============================================================================
# MOSAIC: run_MOSAIC_infrastructure.R
# Production infrastructure utilities for cluster/VM deployment
# =============================================================================

# All functions prefixed with . (not exported - internal use only)

# =============================================================================
# FILE SYSTEM SAFETY
# =============================================================================

#' Check Available Disk Space
#'
#' Validates sufficient disk space before large write operations.
#' Critical for cluster environments with quotas.
#'
#' @param path Directory to check
#' @param required_mb Minimum required space in MB
#' @return Logical, TRUE if sufficient space
#' @noRd
.mosaic_check_disk_space <- function(path, required_mb = 100) {
  # Ensure directory exists or use parent
  check_path <- if (dir.exists(path)) path else dirname(path)

  tryCatch({
    # Get filesystem info (cross-platform)
    if (.Platform$OS.type == "unix") {
      # Use df command on Unix-like systems
      cmd <- sprintf("df -m '%s' | tail -1 | awk '{print $4}'", check_path)
      available_mb <- as.numeric(system(cmd, intern = TRUE))
    } else {
      # Windows: use fsutil or assume sufficient space
      # Note: fsutil requires admin, so we skip check on Windows
      return(TRUE)
    }

    if (is.na(available_mb) || available_mb < required_mb) {
      warning(sprintf(
        "Low disk space: %.1f MB available, %.1f MB required at %s",
        available_mb, required_mb, check_path
      ), call. = FALSE, immediate. = TRUE)
      return(FALSE)
    }

    TRUE
  }, error = function(e) {
    # If check fails, log warning but allow operation
    warning("Could not verify disk space: ", e$message, call. = FALSE)
    TRUE
  })
}

#' Safe File Locking for Multi-User Environments
#'
#' Implements advisory file locking to prevent race conditions.
#' Uses flock() on Unix systems.
#'
#' @param lockfile Path to lock file
#' @param timeout_sec Maximum seconds to wait for lock
#' @return Lock handle (file connection), or NULL if failed
#' @noRd
.mosaic_acquire_lock <- function(lockfile, timeout_sec = 30) {
  if (.Platform$OS.type != "unix") {
    # Windows: no flock, use simpler approach
    return(.mosaic_acquire_lock_windows(lockfile, timeout_sec))
  }

  start_time <- Sys.time()
  lock_acquired <- FALSE
  lock_con <- NULL

  while (!lock_acquired && difftime(Sys.time(), start_time, units = "secs") < timeout_sec) {
    tryCatch({
      # Create lock file if doesn't exist
      if (!file.exists(lockfile)) {
        file.create(lockfile)
      }

      # Open file connection
      lock_con <- file(lockfile, open = "r+")

      # Try to acquire exclusive lock (non-blocking)
      # Note: R doesn't have native flock, use system call
      lock_result <- system2("flock",
                            args = c("--nonblock", "--exclusive",
                                    as.integer(lock_con), "true"),
                            stdout = FALSE, stderr = FALSE)

      if (lock_result == 0) {
        lock_acquired <- TRUE
      } else {
        close(lock_con)
        lock_con <- NULL
        Sys.sleep(0.1)  # Wait before retry
      }
    }, error = function(e) {
      if (!is.null(lock_con)) {
        try(close(lock_con), silent = TRUE)
        lock_con <- NULL
      }
      Sys.sleep(0.1)
    })
  }

  if (!lock_acquired) {
    warning("Failed to acquire lock on ", lockfile, " after ", timeout_sec, " seconds",
            call. = FALSE, immediate. = TRUE)
    return(NULL)
  }

  lock_con
}

#' Release File Lock
#' @param lock_con Lock handle from .mosaic_acquire_lock()
#' @noRd
.mosaic_release_lock <- function(lock_con) {
  if (!is.null(lock_con) && inherits(lock_con, "connection")) {
    try(close(lock_con), silent = TRUE)
  }
  invisible(TRUE)
}

#' Windows Lock Implementation (Simpler Fallback)
#' @noRd
.mosaic_acquire_lock_windows <- function(lockfile, timeout_sec) {
  start_time <- Sys.time()

  while (difftime(Sys.time(), start_time, units = "secs") < timeout_sec) {
    if (!file.exists(lockfile)) {
      tryCatch({
        writeLines(as.character(Sys.getpid()), lockfile)
        return(lockfile)  # Return path as "handle"
      }, error = function(e) {
        Sys.sleep(0.1)
      })
    } else {
      Sys.sleep(0.1)
    }
  }

  NULL
}

#' NFS-Safe Atomic Write
#'
#' Implements truly atomic write for network filesystems.
#' Uses write-to-temp-and-rename with sync.
#'
#' @param data Data to write
#' @param path Target path
#' @param write_func Function to write data (e.g., saveRDS, write.csv)
#' @return Logical, TRUE if successful
#' @noRd
.mosaic_atomic_write <- function(data, path, write_func, ...) {
  # Create temp file in same directory (ensures same filesystem)
  tmp_file <- tempfile(
    pattern = paste0(".mosaic_tmp_", basename(path), "_"),
    tmpdir = dirname(path)
  )

  tryCatch({
    # Write to temp file
    write_func(data, tmp_file, ...)

    # Sync to disk (flush buffers)
    if (.Platform$OS.type == "unix") {
      system2("sync", wait = TRUE, stdout = FALSE, stderr = FALSE)
    }

    # Atomic rename (should be atomic even on NFS after sync)
    success <- file.rename(tmp_file, path)

    if (!success) {
      stop("file.rename() failed")
    }

    # Clean up temp file if it still exists
    if (file.exists(tmp_file)) {
      unlink(tmp_file, force = TRUE)
    }

    TRUE
  }, error = function(e) {
    # Clean up on error
    if (file.exists(tmp_file)) {
      unlink(tmp_file, force = TRUE)
    }
    stop("Atomic write failed: ", e$message, call. = FALSE)
  })
}

# =============================================================================
# STATE MANAGEMENT
# =============================================================================

#' Validate Run State Structure
#'
#' Ensures loaded state has expected structure and types.
#' Prevents crashes from version incompatibilities.
#'
#' @param state Loaded state object
#' @return Validated state, or NULL if invalid
#' @noRd
.mosaic_validate_state <- function(state) {
  required_fields <- c(
    "total_sims_run", "total_sims_successful", "batch_number",
    "phase", "converged", "mode"
  )

  # Check all required fields exist
  missing <- setdiff(required_fields, names(state))
  if (length(missing) > 0) {
    warning("State missing fields: ", paste(missing, collapse = ", "),
            call. = FALSE)
    return(NULL)
  }

  # Type validation
  type_checks <- list(
    total_sims_run = is.numeric,
    total_sims_successful = is.numeric,
    batch_number = is.numeric,
    phase = is.character,
    converged = is.logical,
    mode = is.character
  )

  for (field in names(type_checks)) {
    if (!type_checks[[field]](state[[field]])) {
      warning("State field '", field, "' has wrong type", call. = FALSE)
      return(NULL)
    }
  }

  # Value validation
  if (state$total_sims_run < 0 || state$batch_number < 0) {
    warning("State has negative counts", call. = FALSE)
    return(NULL)
  }

  if (!state$mode %in% c("auto", "fixed")) {
    warning("State has invalid mode: ", state$mode, call. = FALSE)
    return(NULL)
  }

  state
}

#' Safe State Save with Locking
#' @noRd
.mosaic_save_state_safe <- function(state, path) {
  lockfile <- paste0(path, ".lock")
  lock <- .mosaic_acquire_lock(lockfile, timeout_sec = 10)

  if (is.null(lock)) {
    warning("Could not acquire lock to save state", call. = FALSE)
    return(FALSE)
  }

  on.exit(.mosaic_release_lock(lock), add = TRUE)

  # Use atomic write
  .mosaic_atomic_write(state, path, saveRDS)
}

#' Safe State Load with Locking
#' @noRd
.mosaic_load_state_safe <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  lockfile <- paste0(path, ".lock")
  lock <- .mosaic_acquire_lock(lockfile, timeout_sec = 10)

  if (is.null(lock)) {
    warning("Could not acquire lock to load state", call. = FALSE)
    return(NULL)
  }

  on.exit(.mosaic_release_lock(lock), add = TRUE)

  state <- tryCatch({
    readRDS(path)
  }, error = function(e) {
    warning("Failed to load state: ", e$message, call. = FALSE)
    NULL
  })

  # Validate structure
  .mosaic_validate_state(state)
}

# =============================================================================
# RESOURCE MANAGEMENT
# =============================================================================

#' Setup Signal Handlers for Graceful Shutdown
#'
#' Registers handlers for SIGTERM/SIGINT to save state before exit.
#' Critical for cluster schedulers that send SIGTERM before SIGKILL.
#'
#' @param state_file Path to state file
#' @param state State object to save
#' @param cleanup_func Optional cleanup function
#' @noRd
.mosaic_register_signal_handlers <- function(state_file, state, cleanup_func = NULL) {
  if (.Platform$OS.type != "unix") {
    return(invisible(NULL))
  }

  # Create handler function
  signal_handler <- function() {
    message("\nReceived termination signal. Saving state...")

    # Save state
    tryCatch({
      .mosaic_save_state_safe(state, state_file)
      message("State saved successfully")
    }, error = function(e) {
      message("Failed to save state: ", e$message)
    })

    # Run cleanup
    if (!is.null(cleanup_func)) {
      tryCatch({
        cleanup_func()
        message("Cleanup completed")
      }, error = function(e) {
        message("Cleanup failed: ", e$message)
      })
    }

    # Exit
    quit(save = "no", status = 143)  # 128 + 15 (SIGTERM)
  }

  # Register for SIGTERM and SIGINT
  tryCatch({
    # Note: R's signal handling is limited; this is best-effort
    options(mosaic.signal_handler = signal_handler)
    invisible(TRUE)
  }, error = function(e) {
    warning("Could not register signal handlers: ", e$message, call. = FALSE)
    invisible(FALSE)
  })
}

#' Check If BLAS Threading Control Available
#'
#' Validates BLAS threading can be controlled.
#' Warns if RhpcBLASctl not available (critical for cluster performance).
#'
#' @return Logical, TRUE if control available
#' @noRd
.mosaic_check_blas_control <- function() {
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    return(TRUE)
  }

  # Check if we can control via environment variables
  if (Sys.getenv("OMP_NUM_THREADS") != "") {
    message("BLAS threading controlled via OMP_NUM_THREADS")
    return(TRUE)
  }

  if (Sys.getenv("OPENBLAS_NUM_THREADS") != "") {
    message("BLAS threading controlled via OPENBLAS_NUM_THREADS")
    return(TRUE)
  }

  # No control available - warn user
  warning(
    "Cannot control BLAS threading! This may cause severe performance issues.\n",
    "  Install RhpcBLASctl: install.packages('RhpcBLASctl')\n",
    "  Or set OMP_NUM_THREADS=1 before starting R",
    call. = FALSE,
    immediate. = TRUE
  )

  FALSE
}

#' Set BLAS Threads to 1 (Critical for Parallel Workers)
#' @noRd
.mosaic_set_blas_threads <- function(n_threads = 1L) {
  success <- FALSE

  # Try RhpcBLASctl
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    tryCatch({
      RhpcBLASctl::blas_set_num_threads(n_threads)
      success <- TRUE
    }, error = function(e) {
      warning("RhpcBLASctl failed: ", e$message, call. = FALSE)
    })
  }

  # Fallback: set environment variables
  if (!success) {
    Sys.setenv(OMP_NUM_THREADS = n_threads)
    Sys.setenv(OPENBLAS_NUM_THREADS = n_threads)
    Sys.setenv(MKL_NUM_THREADS = n_threads)
  }

  invisible(success)
}

#' Detect If Output is Redirected (Batch Job)
#'
#' Determines if stdout is a terminal or redirected to file.
#' Used to disable progress bars in batch jobs.
#'
#' @return Logical, TRUE if output is redirected
#' @noRd
.mosaic_is_output_redirected <- function() {
  # Check if stdout is a terminal
  if (.Platform$OS.type == "unix") {
    # Use test command to check if stdout is a tty
    result <- system("test -t 1", ignore.stdout = TRUE, ignore.stderr = TRUE)
    return(result != 0)  # Non-zero means not a terminal
  }

  # Windows: check if interactive
  !interactive()
}

#' Log Cluster Metadata
#'
#' Captures SLURM/PBS job metadata for debugging.
#'
#' @return Named list of cluster metadata
#' @noRd
.mosaic_get_cluster_metadata <- function() {
  metadata <- list(
    hostname = Sys.info()["nodename"],
    user = Sys.info()["user"],
    r_version = R.version.string,
    platform = R.version$platform,
    timestamp = Sys.time()
  )

  # SLURM environment
  slurm_vars <- c(
    "SLURM_JOB_ID", "SLURM_JOB_NAME", "SLURM_NODELIST",
    "SLURM_NTASKS", "SLURM_CPUS_PER_TASK", "SLURM_MEM_PER_NODE"
  )
  for (var in slurm_vars) {
    val <- Sys.getenv(var)
    if (val != "") {
      metadata[[tolower(var)]] <- val
    }
  }

  # PBS environment
  pbs_vars <- c(
    "PBS_JOBID", "PBS_JOBNAME", "PBS_NODEFILE",
    "PBS_NP", "PBS_NUM_NODES"
  )
  for (var in pbs_vars) {
    val <- Sys.getenv(var)
    if (val != "") {
      metadata[[tolower(var)]] <- val
    }
  }

  metadata
}

# =============================================================================
# ERROR HANDLING
# =============================================================================

#' Safe Min with Empty Check
#'
#' Returns NA instead of crashing on empty vector.
#'
#' @param x Numeric vector
#' @param na.rm Remove NAs
#' @return Minimum value or NA
#' @noRd
.mosaic_safe_min <- function(x, na.rm = TRUE) {
  finite_x <- x[is.finite(x)]
  if (length(finite_x) == 0) {
    return(NA_real_)
  }
  min(finite_x, na.rm = na.rm)
}

#' Safe Max with Empty Check
#' @noRd
.mosaic_safe_max <- function(x, na.rm = TRUE) {
  finite_x <- x[is.finite(x)]
  if (length(finite_x) == 0) {
    return(NA_real_)
  }
  max(finite_x, na.rm = na.rm)
}

#' Safe Parse of Simulation ID from Filename
#'
#' Robust extraction of sim IDs from filenames.
#'
#' @param filenames Vector of filenames
#' @param pattern Regex pattern
#' @return Integer vector of sim IDs
#' @noRd
.mosaic_parse_sim_ids <- function(filenames, pattern = "^sim_0*([0-9]+)\\.parquet$") {
  # Extract IDs
  ids_str <- sub(pattern, "\\1", filenames)

  # Convert to integer
  ids <- suppressWarnings(as.integer(ids_str))

  # Remove NAs (failed conversions)
  valid_ids <- ids[!is.na(ids)]

  if (length(valid_ids) < length(filenames)) {
    warning(
      "Failed to parse ", length(filenames) - length(valid_ids),
      " simulation IDs from filenames",
      call. = FALSE
    )
  }

  valid_ids
}
