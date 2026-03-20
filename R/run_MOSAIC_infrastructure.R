# =============================================================================
# MOSAIC: run_MOSAIC_infrastructure.R
# Production infrastructure utilities for cluster/VM deployment
# =============================================================================

# All functions prefixed with . (not exported - internal use only)

# =============================================================================
# FILE SYSTEM SAFETY
# =============================================================================

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
#'
#' @param dirs Directory structure
#' @param state Internal calibration state
#' @param start_time POSIXct start time for wall-clock calculation
#' @param config Base LASER config (for provenance fields)
#' @param r2_cases R-squared for cases (best model vs observed)
#' @param r2_deaths R-squared for deaths (best model vs observed)
#' @param io I/O settings for JSON writing
#' @noRd
.mosaic_write_summary_json <- function(dirs, state, start_time, config,
                                       r2_cases = NA_real_, r2_deaths = NA_real_,
                                       io) {
  # Read convergence diagnostics
  diag_file <- file.path(dirs$cal_diag, "convergence_diagnostics.json")
  diag <- if (file.exists(diag_file)) {
    jsonlite::read_json(diag_file)
  } else {
    list()
  }

  # Read parameter ESS
  ess_file <- file.path(dirs$cal_diag, "parameter_ess.csv")
  ess_stats <- list(
    n_params = NA_integer_, n_above_target = NA_integer_,
    pct_above_target = NA_real_, min_ess = NA_real_, median_ess = NA_real_,
    target = NA_real_
  )
  if (file.exists(ess_file)) {
    ess_df <- utils::read.csv(ess_file, stringsAsFactors = FALSE)
    if ("ess_marginal" %in% names(ess_df)) {
      ess_vals <- ess_df$ess_marginal[is.finite(ess_df$ess_marginal)]
      ess_target <- if (!is.null(diag$targets$ess_min$value)) {
        as.numeric(diag$targets$ess_min$value)
      } else 100  # fallback default
      ess_stats$n_params <- length(ess_vals)
      ess_stats$n_above_target <- sum(ess_vals >= ess_target)
      ess_stats$pct_above_target <- if (length(ess_vals) > 0) {
        round(100 * ess_stats$n_above_target / length(ess_vals), 1)
      } else NA_real_
      ess_stats$min_ess <- if (length(ess_vals) > 0) round(min(ess_vals), 1) else NA_real_
      ess_stats$median_ess <- if (length(ess_vals) > 0) round(stats::median(ess_vals), 1) else NA_real_
      ess_stats$target <- ess_target
    }
  }

  wall_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  summary_obj <- list(
    # Provenance: what was run and when
    location      = paste(config$location_name, collapse = ", "),
    date_start    = config$date_start,
    date_stop     = config$date_stop,
    timestamp     = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
    mode          = if (!is.null(state$mode)) state$mode else NA_character_,
    # Run statistics
    wall_time_seconds          = round(wall_time, 1),
    n_batches                  = state$batch_number,
    n_simulations_total        = state$total_sims_run,
    n_simulations_successful   = state$total_sims_successful,
    n_retained                 = if (!is.null(diag$summary$retained_simulations)) diag$summary$retained_simulations else NA_integer_,
    n_best_subset              = if (!is.null(diag$metrics$B_size$value)) as.integer(diag$metrics$B_size$value) else NA_integer_,
    # Convergence and model fit
    converged     = isTRUE(state$converged),
    r2_cases      = if (!is.na(r2_cases)) round(r2_cases, 4) else NA_real_,
    r2_deaths     = if (!is.na(r2_deaths)) round(r2_deaths, 4) else NA_real_,
    # ESS summary
    ess_n_params        = ess_stats$n_params,
    ess_n_above_target  = ess_stats$n_above_target,
    ess_pct_above_target = ess_stats$pct_above_target,
    ess_target          = ess_stats$target,
    ess_min             = ess_stats$min_ess,
    ess_median          = ess_stats$median_ess
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

  # Find the quantile columns
  # calc_model_posterior_quantiles() produces: q0.025, q0.25, q0.5, q0.75, q0.975
  q_cols <- names(post_q)
  q2.5_col <- grep("^q0\\.025$", q_cols, value = TRUE)[1]
  q50_col <- grep("^q0\\.5$", q_cols, value = TRUE)[1]
  q97.5_col <- grep("^q0\\.975$", q_cols, value = TRUE)[1]

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

