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

  # BUG FIX #1: Add missing phase tracking fields for backward compatibility
  if (is.null(state$phase_batch_count)) {
    state$phase_batch_count <- 0L
    message("Added missing phase_batch_count field (legacy state file)")
  }
  if (is.null(state$phase_last)) {
    state$phase_last <- NULL
    message("Added missing phase_last field (legacy state file)")
  }

  state
}

#' Safe State Save with Locking
#' @noRd
.mosaic_save_state_safe <- function(state, path) {
  # For single-process runs, locking is not needed
  # For cluster runs, the flock implementation needs improvement
  # Just use atomic write for now
  .mosaic_atomic_write(state, path, saveRDS)
}

#' Safe State Load with Locking
#' @noRd
.mosaic_load_state_safe <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  # For single-process runs, locking is not needed
  # Just load and validate
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
