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

#' Atomic Write
#'
#' Implements atomic write via write-to-temp-and-rename.
#' POSIX rename is atomic on local filesystems.
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

    # Atomic rename (POSIX guarantees atomicity on local filesystems)
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

#' Mark Run State as Completed
#'
#' Reads the run_state.json file and updates its status to "completed".
#' Called at the very end of a successful run.
#'
#' @param state_file Path to run_state.json
#' @noRd
.mosaic_finalize_state <- function(state_file) {
  if (!file.exists(state_file)) return(invisible(NULL))
  persisted <- tryCatch(
    jsonlite::read_json(state_file),
    error = function(e) NULL
  )
  if (is.null(persisted)) return(invisible(NULL))
  persisted$status <- "completed"
  persisted$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  tmp <- tempfile(tmpdir = dirname(state_file), fileext = ".json.tmp")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(persisted, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  file.rename(tmp, state_file)
  invisible(state_file)
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

#' Capture Full Environment Snapshot
#'
#' Records all version, system, and runtime information needed to reproduce
#' a calibration run. Written to 1_inputs/environment.json.
#'
#' @param config Model configuration (for priors metadata)
#' @param priors Prior distributions (for priors metadata)
#' @param control Control object (for parallel settings)
#' @return Named list with sections: R, python, system, git, data
#' @noRd
.mosaic_capture_environment <- function(config = NULL, priors = NULL, control = NULL) {
  tryCatch({

  # --- R environment ---
  r_env <- list(
    version = R.version.string,
    platform = R.version$platform,
    MOSAIC = as.character(utils::packageVersion("MOSAIC"))
  )
  r_pkgs <- c("reticulate", "arrow", "data.table", "dplyr", "sf", "cli")
  for (pkg in r_pkgs) {
    r_env[[paste0("pkg_", pkg)]] <- tryCatch(
      as.character(utils::packageVersion(pkg)),
      error = function(e) NA_character_
    )
  }

  # --- Python environment ---
  # Only query if reticulate has already bound to avoid side effects on clusters
  py_env <- list()
  if (requireNamespace("reticulate", quietly = TRUE) &&
      reticulate::py_available(initialize = FALSE)) {
    tryCatch({
      sys <- reticulate::import("sys", delay_load = FALSE)
      py_env$version <- strsplit(as.character(sys$version), " ")[[1]][1]

      importlib <- reticulate::import("importlib.metadata", delay_load = FALSE)
      py_pkgs <- c("laser-cholera", "laser-core", "numpy", "torch",
                   "pyarrow", "h5py", "sbi", "zuko", "scikit-learn")
      for (pkg in py_pkgs) {
        key <- gsub("-", "_", pkg)
        py_env[[paste0("pkg_", key)]] <- tryCatch(
          as.character(importlib$version(pkg)),
          error = function(e) NA_character_
        )
      }
    }, error = function(e) {
      py_env$error <<- paste("Python query failed:", e$message)
    })
  }

  # --- System / cluster ---
  sys_info <- Sys.info()
  n_cores <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  if (is.null(n_cores) || is.na(n_cores)) n_cores <- NA_integer_
  system_env <- list(
    hostname = unname(sys_info["nodename"]),
    user = unname(sys_info["user"]),
    os = unname(sys_info["sysname"]),
    n_cores_available = n_cores,
    n_cores_requested = if (!is.null(control)) control$parallel$n_cores else NA_integer_
  )
  cluster_vars <- c(
    "SLURM_JOB_ID", "SLURM_JOB_NAME", "SLURM_NODELIST",
    "SLURM_NTASKS", "SLURM_CPUS_PER_TASK", "SLURM_MEM_PER_NODE",
    "PBS_JOBID", "PBS_JOBNAME", "PBS_NODEFILE", "PBS_NP", "PBS_NUM_NODES"
  )
  for (var in cluster_vars) {
    val <- Sys.getenv(var)
    if (val != "") system_env[[tolower(var)]] <- val
  }

  # --- Git ---
  git_env <- list()
  if (nzchar(Sys.which("git"))) {
    .git_cmd <- function(...) {
      out <- tryCatch(
        system2("git", c(...), stdout = TRUE, stderr = FALSE),
        error = function(e) NULL, warning = function(w) NULL
      )
      if (is.null(out) || !is.character(out) || length(out) == 0) return(NA_character_)
      trimws(out[1])
    }
    pkg_dir <- system.file(package = "MOSAIC")
    git_dir <- if (file.exists(file.path(".", ".git"))) "."
               else if (file.exists(file.path(pkg_dir, ".git"))) pkg_dir
               else NA_character_
    if (!is.na(git_dir)) {
      git_env$sha    <- .git_cmd("-C", git_dir, "rev-parse", "--short", "HEAD")
      git_env$branch <- .git_cmd("-C", git_dir, "rev-parse", "--abbrev-ref", "HEAD")
    }
  }

  # --- Data versions ---
  data_env <- list()
  if (!is.null(config) && !is.null(config$metadata)) {
    data_env$config_version <- config$metadata$version
    data_env$config_date <- config$metadata$date
  }
  if (!is.null(priors) && !is.null(priors$metadata)) {
    data_env$priors_version <- priors$metadata$version
    data_env$priors_date <- as.character(priors$metadata$date)
  }

  list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    R = r_env,
    python = py_env,
    system = system_env,
    git = git_env,
    data = data_env
  )

  }, error = function(e) {
    warning("Environment capture failed: ", e$message, call. = FALSE)
    list(timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
         error = paste("Capture failed:", e$message))
  })
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

# =============================================================================
# OUTPUT GENERATION
# =============================================================================

#' Write Summary JSON at Run Completion
#' @noRd
.mosaic_write_summary_json <- function(dirs, state, start_time, io) {
  # Read convergence diagnostics
  diag_file <- file.path(dirs$cal_diag, "convergence_diagnostics.json")
  diag <- if (file.exists(diag_file)) {
    jsonlite::read_json(diag_file)
  } else {
    list()
  }

  # Read parameter ESS
  ess_file <- file.path(dirs$cal_diag, "parameter_ess.csv")
  ess_min_param <- if (file.exists(ess_file)) {
    ess_df <- utils::read.csv(ess_file, stringsAsFactors = FALSE)
    if ("ess_marginal" %in% names(ess_df)) {
      min(ess_df$ess_marginal, na.rm = TRUE)
    } else NA_real_
  } else NA_real_

  wall_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  summary_obj <- list(
    converged = isTRUE(state$converged),
    wall_time_seconds = round(wall_time, 1),
    n_simulations_total = state$total_sims_run,
    n_retained = if (!is.null(diag$summary$retained_simulations)) diag$summary$retained_simulations else NA_integer_,
    n_best_subset = if (!is.null(diag$summary$best_subset_simulations)) diag$summary$best_subset_simulations else NA_integer_,
    r2_final = if (!is.na(state$calib_r2)) round(state$calib_r2, 4) else NA_real_,
    ess_overall = if (!is.null(diag$metrics$ess_best$value)) diag$metrics$ess_best$value else NA_real_,
    ess_min_param = if (is.finite(ess_min_param)) round(ess_min_param, 1) else NA_real_,
    cvw_best = if (!is.null(diag$metrics$cvw_B$value)) diag$metrics$cvw_B$value else NA_real_
  )

  summary_path <- file.path(dirs$results, "summary.json")
  .mosaic_write_json(summary_obj, summary_path, io)
  summary_obj
}

#' Write Parameter Estimates CSV to Results
#' @noRd
.mosaic_write_parameter_estimates <- function(dirs) {
  pq_file <- file.path(dirs$cal_posterior, "posterior_quantiles.csv")
  if (!file.exists(pq_file)) return(invisible(NULL))

  post_q <- utils::read.csv(pq_file, stringsAsFactors = FALSE)

  # Find the quantile columns (q2.5, q50, q97.5)
  q_cols <- names(post_q)
  q2.5_col <- grep("^q0?\\.?025$|^q2\\.5$", q_cols, value = TRUE)[1]
  q50_col <- grep("^q0?\\.?5$|^q50$", q_cols, value = TRUE)[1]
  q97.5_col <- grep("^q0?\\.?975$|^q97\\.5$", q_cols, value = TRUE)[1]

  if (is.na(q2.5_col) || is.na(q50_col) || is.na(q97.5_col)) {
    warning("Could not find expected quantile columns in posterior_quantiles.csv", call. = FALSE)
    return(invisible(NULL))
  }

  param_est <- data.frame(
    parameter = post_q$parameter,
    median    = post_q[[q50_col]],
    Q2.5      = post_q[[q2.5_col]],
    Q97.5     = post_q[[q97.5_col]],
    stringsAsFactors = FALSE
  )

  out_path <- file.path(dirs$res_posterior, "parameter_estimates.csv")
  utils::write.csv(param_est, out_path, row.names = FALSE)
  invisible(out_path)
}

#' Write _README.md at Run Completion (Completion Signal)
#' @noRd
.mosaic_write_readme <- function(dirs, config, summary_obj) {
  iso <- if (!is.null(config$location_name)) {
    paste(config$location_name, collapse = ", ")
  } else "unknown"

  date_range <- if (!is.null(config$date_start) && !is.null(config$date_stop)) {
    paste(config$date_start, "to", config$date_stop)
  } else "unknown"

  mosaic_ver <- tryCatch(
    as.character(utils::packageVersion("MOSAIC")),
    error = function(e) "unknown"
  )

  # Convergence verdict
  status <- if (isTRUE(summary_obj$converged)) "CONVERGED" else "DID NOT CONVERGE"

  lines <- c(
    paste0("# MOSAIC Calibration: ", iso),
    "",
    "## Run Identity",
    "",
    paste0("- **Locations**: ", iso),
    paste0("- **Period**: ", date_range),
    paste0("- **Run date**: ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste0("- **MOSAIC version**: ", mosaic_ver),
    paste0("- **Output path**: `", dirs$root, "`"),
    "",
    "## Convergence",
    "",
    paste0("**Status: ", status, "**"),
    "",
    paste0("| Metric | Value |"),
    paste0("|--------|-------|"),
    paste0("| Total simulations | ", summary_obj$n_simulations_total, " |"),
    paste0("| Retained | ", ifelse(is.na(summary_obj$n_retained), "N/A", summary_obj$n_retained), " |"),
    paste0("| Best subset | ", ifelse(is.na(summary_obj$n_best_subset), "N/A", summary_obj$n_best_subset), " |"),
    paste0("| R-squared | ", ifelse(is.na(summary_obj$r2_final), "N/A", round(summary_obj$r2_final, 4)), " |"),
    paste0("| ESS (min param) | ", ifelse(is.na(summary_obj$ess_min_param), "N/A", round(summary_obj$ess_min_param, 1)), " |"),
    paste0("| CVw (best) | ", ifelse(is.na(summary_obj$cvw_best), "N/A", round(summary_obj$cvw_best, 2)), " |"),
    paste0("| Wall time | ", round(summary_obj$wall_time_seconds / 60, 1), " min |"),
    "",
    "## Directory Guide",
    "",
    "| I want to... | Go to |",
    "|---|---|",
    "| See run inputs (config, priors) | `1_inputs/` |",
    "| Get posterior parameter estimates | `3_results/posterior/parameter_estimates.csv` |",
    "| See model fit plots | `3_results/figures/predictions/` |",
    "| See convergence diagnostics | `3_results/figures/diagnostics/` |",
    "| See prior vs posterior | `3_results/figures/posterior/` |",
    "| Get raw calibration samples | `2_calibration/samples.parquet` |",
    "| Get full posterior distributions | `2_calibration/posterior/posteriors.json` |",
    "| Get best-fit model config | `2_calibration/best_model/config_best.json` |",
    ""
  )

  readme_path <- file.path(dirs$root, "_README.md")
  writeLines(lines, readme_path)
  invisible(readme_path)
}
