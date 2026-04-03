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
  jsonlite::write_json(persisted, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  file.rename(tmp, state_file)

  # Write _README.md completion signal to run root directory
  # state_file path: dir_output/2_calibration/state/run_state.json (3 levels deep)
  dir_output <- dirname(dirname(dirname(state_file)))
  readme_path <- file.path(dir_output, "_README.md")
  readme_lines <- c(
    "# MOSAIC Run Complete",
    "",
    paste0("**Status:** completed"),
    paste0("**Completed:** ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Output Structure",
    "",
    "| Directory | Contents |",
    "|-----------|----------|",
    "| `1_inputs/` | Immutable run inputs (config, priors, control, environment) |",
    "| `2_calibration/` | Calibration outputs (samples, posterior, diagnostics, state) |",
    "| `3_results/` | Curated outputs for downstream use (summary, figures, predictions) |",
    "",
    "See `3_results/summary.json` for key metrics and `2_calibration/samples.parquet` for all parameter draws."
  )
  writeLines(readme_lines, readme_path)

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
#' @param r2_cases_ensemble R-squared for cases (ensemble mean vs observed)
#' @param r2_deaths_ensemble R-squared for deaths (ensemble mean vs observed)
#' @param io I/O settings for JSON writing
#' @noRd
.mosaic_write_summary_json <- function(dirs, state, start_time, config,
                                       r2_cases = NA_real_, r2_deaths = NA_real_,
                                       r2_cases_ensemble = NA_real_,
                                       r2_deaths_ensemble = NA_real_,
                                       bias_ratio_cases = NA_real_,
                                       bias_ratio_deaths = NA_real_,
                                       bias_ratio_cases_ensemble = NA_real_,
                                       bias_ratio_deaths_ensemble = NA_real_,
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
      ess_target <- if (!is.null(diag$targets$ess_param$value)) {
        as.numeric(diag$targets$ess_param$value)
      } else if (!is.null(diag$targets$ess_min$value)) {
        as.numeric(diag$targets$ess_min$value)  # legacy fallback
      } else 100  # default fallback
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
    # Convergence and model fit (best = single best model, ensemble = mean of N stochastic runs)
    converged     = isTRUE(state$converged),
    r2_cases      = if (!is.na(r2_cases)) round(r2_cases, 4) else NA_real_,
    r2_deaths     = if (!is.na(r2_deaths)) round(r2_deaths, 4) else NA_real_,
    r2_cases_ensemble  = if (!is.na(r2_cases_ensemble)) round(r2_cases_ensemble, 4) else NA_real_,
    r2_deaths_ensemble = if (!is.na(r2_deaths_ensemble)) round(r2_deaths_ensemble, 4) else NA_real_,
    bias_ratio_cases   = if (!is.na(bias_ratio_cases)) round(bias_ratio_cases, 4) else NA_real_,
    bias_ratio_deaths  = if (!is.na(bias_ratio_deaths)) round(bias_ratio_deaths, 4) else NA_real_,
    bias_ratio_cases_ensemble  = if (!is.na(bias_ratio_cases_ensemble)) round(bias_ratio_cases_ensemble, 4) else NA_real_,
    bias_ratio_deaths_ensemble = if (!is.na(bias_ratio_deaths_ensemble)) round(bias_ratio_deaths_ensemble, 4) else NA_real_,
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

  # Filter to posterior rows only (file contains both prior and posterior rows)
  if ("type" %in% names(post_q)) {
    post_q <- post_q[post_q$type == "posterior", ]
  }

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


#' Compute R² and Bias Ratio Across Trailing Time Windows
#'
#' @param obs_cases Numeric vector of observed cases.
#' @param est_cases Numeric vector of estimated cases (same length).
#' @param obs_deaths Numeric vector of observed deaths.
#' @param est_deaths Numeric vector of estimated deaths.
#' @param dates Date vector of same length as obs/est vectors.
#' @param windows Integer vector of trailing observation counts (e.g. c(365, 120, 90, 60, 30)).
#' @return data.frame with one row per window plus a "full" row.
#' @noRd
.mosaic_compute_windowed_metrics <- function(obs_cases, est_cases,
                                             obs_deaths, est_deaths,
                                             dates, windows = c(365, 120, 90, 60, 30)) {

  valid_idx_c <- which(!is.na(obs_cases) & is.finite(obs_cases))
  valid_idx_d <- which(!is.na(obs_deaths) & is.finite(obs_deaths))

  compute_row <- function(label, idx_c, idx_d) {
    # Cases
    r2_c <- bias_c <- NA_real_
    if (length(idx_c) > 2) {
      o <- obs_cases[idx_c]; e <- est_cases[idx_c]
      v <- is.finite(o) & is.finite(e)
      if (sum(v) > 2) r2_c <- stats::cor(o[v], e[v])^2
      bias_c <- calc_bias_ratio(o, e)
    }
    # Deaths
    r2_d <- bias_d <- NA_real_
    if (length(idx_d) > 2) {
      o <- obs_deaths[idx_d]; e <- est_deaths[idx_d]
      v <- is.finite(o) & is.finite(e)
      if (sum(v) > 2) r2_d <- stats::cor(o[v], e[v])^2
      bias_d <- calc_bias_ratio(o, e)
    }

    # Date range from cases (primary)
    idx_all <- sort(unique(c(idx_c, idx_d)))
    date_start <- if (length(idx_all) > 0) as.character(dates[min(idx_all)]) else NA_character_
    date_end   <- if (length(idx_all) > 0) as.character(dates[max(idx_all)]) else NA_character_

    data.frame(
      window     = label,
      n_obs      = max(length(idx_c), length(idx_d)),
      date_start = date_start,
      date_end   = date_end,
      r2_cases   = round(r2_c, 4),
      bias_cases = round(bias_c, 4),
      r2_deaths  = round(r2_d, 4),
      bias_deaths = round(bias_d, 4),
      stringsAsFactors = FALSE
    )
  }

  rows <- list()

  # Full series
  rows[[1]] <- compute_row("full", valid_idx_c, valid_idx_d)

  # Trailing windows
  for (w in windows) {
    idx_c <- if (length(valid_idx_c) >= w) tail(valid_idx_c, w) else integer(0)
    idx_d <- if (length(valid_idx_d) >= w) tail(valid_idx_d, w) else integer(0)
    if (length(idx_c) == 0 && length(idx_d) == 0) next
    rows[[length(rows) + 1]] <- compute_row(paste0("last_", w), idx_c, idx_d)
  }

  do.call(rbind, rows)
}


#' Plot Windowed Model Fit Metrics (R² + Bias Ratio)
#'
#' Generates a 2-panel figure (cases/deaths) showing R² as bars and bias ratio
#' as an overlaid line with a reference at 1.0.
#'
#' @param metrics_df data.frame from .mosaic_compute_windowed_metrics().
#' @param output_path File path for the saved PNG.
#' @param location Character string for the title.
#' @noRd
.mosaic_plot_windowed_metrics <- function(metrics_df, output_path, location = "") {

  if (nrow(metrics_df) < 1) return(invisible(NULL))

  # Ordered factor for x-axis
  window_labels <- metrics_df$window
  window_display <- gsub("^last_", "", window_labels)
  window_display[window_display == "full"] <- "Full"
  window_display[window_display != "Full"] <- paste0(window_display[window_display != "Full"], "d")
  # Add observation counts
  window_display <- paste0(window_display, "\n(n=", metrics_df$n_obs, ")")
  metrics_df$window_label <- factor(window_display, levels = window_display)

  col_cases  <- mosaic_colors("cases")
  col_deaths <- mosaic_colors("deaths")

  make_panel <- function(r2_col, bias_col, color, title) {
    r2_vals   <- metrics_df[[r2_col]]
    bias_vals <- metrics_df[[bias_col]]
    r2_vals[is.na(r2_vals)]     <- 0
    bias_vals[is.na(bias_vals)] <- NA

    # Base bar plot
    p <- ggplot2::ggplot(metrics_df, ggplot2::aes(x = window_label)) +
      ggplot2::geom_col(
        ggplot2::aes(y = .data[[r2_col]]),
        fill = color, alpha = 0.7, width = 0.6
      ) +
      # Bias line on secondary axis (scaled: bias mapped to 0-1 range for overlay)
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[bias_col]] / 2, group = 1),
        color = mosaic_colors("data"), linewidth = 1.0, na.rm = TRUE
      ) +
      ggplot2::geom_point(
        ggplot2::aes(y = .data[[bias_col]] / 2),
        color = mosaic_colors("data"), size = 3, na.rm = TRUE
      ) +
      # Reference line at bias = 1.0 (mapped to 0.5 on left axis)
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50", linewidth = 0.5) +
      # R² value labels on bars
      ggplot2::geom_text(
        ggplot2::aes(y = .data[[r2_col]], label = sprintf("%.3f", .data[[r2_col]])),
        vjust = -0.5, size = 3, color = color
      ) +
      # Bias value labels on points
      ggplot2::geom_text(
        ggplot2::aes(y = .data[[bias_col]] / 2,
                     label = ifelse(is.na(.data[[bias_col]]), "", sprintf("%.2f", .data[[bias_col]]))),
        vjust = -1.2, size = 2.8, color = mosaic_colors("data"), na.rm = TRUE
      ) +
      ggplot2::scale_y_continuous(
        name = expression(R^2 ~ "(cor"^2 * ")"),
        limits = c(0, max(1.0, max(r2_vals, na.rm = TRUE) * 1.3,
                       max(bias_vals / 2, na.rm = TRUE) * 1.2)),
        sec.axis = ggplot2::sec_axis(~ . * 2, name = "Bias Ratio")
      ) +
      ggplot2::labs(title = title, x = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold", size = 12),
        axis.title.y.left = ggplot2::element_text(color = color),
        axis.title.y.right = ggplot2::element_text(color = mosaic_colors("data")),
        panel.grid.minor = ggplot2::element_blank()
      )

    p
  }

  p_cases  <- make_panel("r2_cases", "bias_cases", col_cases, "Cases")
  p_deaths <- make_panel("r2_deaths", "bias_deaths", col_deaths, "Deaths")

  title <- if (nchar(location) > 0) {
    paste0("Model Fit by Time Window: ", location)
  } else {
    "Model Fit by Time Window"
  }

  p_combined <- cowplot::plot_grid(p_cases, p_deaths, ncol = 1, rel_heights = c(1, 1))
  p_final <- cowplot::plot_grid(
    cowplot::ggdraw() + cowplot::draw_label(title, fontface = "bold", size = 14),
    p_combined,
    ncol = 1, rel_heights = c(0.06, 0.94)
  )

  ggplot2::ggsave(output_path, p_final, width = 10, height = 8, dpi = 150)
  invisible(output_path)
}

