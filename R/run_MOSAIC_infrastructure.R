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

#' Set all thread-count environment variables for parallel workers
#'
#' Sets the canonical thread-pool variables documented in CLAUDE.md so that
#' BLAS, OpenMP, MKL, NumExpr, Intel TBB, Numba, and Apache Arrow each spawn only
#' `n` worker threads. Use everywhere a worker process is about to do CPU-bound work
#' to prevent oversubscription / deadlock.
#' @noRd
.mosaic_set_all_thread_env <- function(n = 1L) {
  n_chr <- as.character(n)
  Sys.setenv(
    OMP_NUM_THREADS      = n_chr,
    MKL_NUM_THREADS      = n_chr,
    OPENBLAS_NUM_THREADS = n_chr,
    NUMEXPR_NUM_THREADS  = n_chr,
    TBB_NUM_THREADS      = n_chr,
    # Retained deliberately although numba left the Python environment with
    # laser-cholera in v0.69.0: this is a generic oversubscription guard and
    # costs nothing to set for an absent package, so it keeps working if a
    # future dependency pulls numba back in. Distinct from zzz.R's
    # NUMBA_THREADING_LAYER = "workqueue", which WAS removed -- that was a
    # workaround for a specific numba/data.table libiomp5 clash, and a
    # workaround for a bug in an absent package is dead code, not a guard.
    NUMBA_NUM_THREADS    = n_chr,
    ARROW_NUM_THREADS    = n_chr   # Apache Arrow CPU thread pool (parquet writer)
  )
  invisible(NULL)
}

#' Set BLAS Threads to 1 (Critical for Parallel Workers)
#' @noRd
.mosaic_set_blas_threads <- function(n_threads = 1L) {
  # Always set the full canonical thread-env set; RhpcBLASctl only controls
  # BLAS, but NumExpr / TBB / Numba spawn their own pools regardless.
  .mosaic_set_all_thread_env(n_threads)

  success <- FALSE
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    tryCatch({
      RhpcBLASctl::blas_set_num_threads(n_threads)
      success <- TRUE
    }, error = function(e) {
      warning("RhpcBLASctl failed: ", e$message, call. = FALSE)
    })
  }

  invisible(success)
}

#' Move shards left by an earlier run out of a fresh run's samples directory
#'
#' A non-resume run starts its sim ids at 1 and later combines every
#' \code{sim_*.parquet} in \code{2_calibration/samples}, so shards from an
#' earlier interrupted run (higher ids, or other chunk ranges) would be pooled
#' into the new posterior without the resume input checks. They are moved,
#' not deleted, into a timestamped \code{2_calibration/samples_stale_<time>/}
#' directory so the earlier draws are recoverable.
#'
#' @param dirs The directory list from \code{.mosaic_ensure_dir_tree()}.
#' @param log_warn Logging callback for the warning.
#' @return Invisibly, the quarantine directory, or \code{NULL} if there was
#'   nothing to move.
#' @noRd
.mosaic_quarantine_stale_shards <- function(dirs, log_warn = function(...) invisible(NULL)) {
  if (is.null(dirs$cal_samples) || !dir.exists(dirs$cal_samples)) return(invisible(NULL))
  stale <- list.files(dirs$cal_samples, pattern = "^sim_.*\\.parquet$", full.names = TRUE)
  if (!length(stale)) return(invisible(NULL))
  qdir <- file.path(dirs$calibration,
                    paste0("samples_stale_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  base <- qdir
  k <- 1L
  while (dir.exists(qdir)) {
    qdir <- paste0(base, "_", k)
    k <- k + 1L
  }
  dir.create(qdir, recursive = TRUE, showWarnings = FALSE)
  moved <- file.rename(stale, file.path(qdir, basename(stale)))
  if (!all(moved)) {
    stop(sprintf(paste0(
      "resume = FALSE but %d shard(s) from an earlier run in %s could not be moved aside; ",
      "they would be pooled into this run's posterior. Remove them, set resume = TRUE, ",
      "or use a new dir_output."), sum(!moved), dirs$cal_samples), call. = FALSE)
  }
  log_warn(paste0("resume = FALSE: moved %d shard(s) left by an earlier run from ",
                  "2_calibration/samples to %s so they are not pooled into this run. ",
                  "Use resume = TRUE to continue an interrupted run."),
           length(stale), file.path("2_calibration", basename(qdir)))
  invisible(qdir)
}

#' Remove post-calibration artifacts left by an earlier run
#'
#' The posterior/medoid/trajectory/spatial artifacts are rebuilt from the
#' current run's samples, but several writers are conditional (optimizer on,
#' medoid found, arrays present). Deleting them before they are rebuilt means a
#' skipped or failed block leaves no file rather than a stale one from a
#' previous run into the same \code{dir_output}.
#'
#' @param dirs The directory list from \code{.mosaic_ensure_dir_tree()}.
#' @param log_msg Logging callback.
#' @return Invisibly, the paths that were removed.
#' @noRd
.mosaic_clear_posterior_artifacts <- function(dirs, log_msg = function(...) invisible(NULL)) {
  cal_files <- c("ensemble_candidate.rds", "ensemble_optimized.rds", "subset_opt.rds",
                 "medoid_ensemble.rds", "trajectories_ensemble.rds",
                 "spatial_hazard_ensemble.rds", "coupling_ensemble.rds",
                 "pi_ij_ensemble.rds")
  paths <- file.path(dirs$calibration, cal_files)
  if (!is.null(dirs$cal_best_model))
    paths <- c(paths, file.path(dirs$cal_best_model, "config_medoid.json"))
  stale <- paths[file.exists(paths)]
  if (length(stale)) {
    unlink(stale)
    log_msg("Removed %d post-calibration artifact(s) from an earlier run: %s",
            length(stale), paste(basename(stale), collapse = ", "))
  }
  invisible(stale)
}

#' Calibration convergence and end-of-run status
#'
#' The calibration ESS criterion is evaluated only in auto mode, so a
#' fixed-mode run reports \code{converged = NA} and its own status rather
#' than "completed_unconverged". Status tree:
#' \itemize{
#'   \item \code{"completed_fixed"} / \code{"completed_fixed_partial"}: fixed
#'     mode, with / without the posterior-ensemble metrics;
#'   \item \code{"completed_unconverged"}: auto mode, ESS criterion not met;
#'   \item \code{"success_partial"}: converged but the posterior-ensemble block
#'     failed and never populated \code{r2_cases_ensemble};
#'   \item \code{"success"}: converged and \code{r2_cases_ensemble} populated.
#' }
#'
#' @param state The calibration state (\code{mode}, \code{converged}).
#' @param outputs_ok Logical; whether the ensemble metrics were populated.
#' @return \code{.mosaic_run_converged()}: \code{TRUE}/\code{FALSE}, or
#'   \code{NA} in fixed mode. \code{.mosaic_run_status()}: the status string.
#' @noRd
.mosaic_run_converged <- function(state) {
  if (identical(state$mode, "fixed")) NA else isTRUE(state$converged)
}

#' End-of-run status string (see .mosaic_run_converged() for the tree)
#' @noRd
.mosaic_run_status <- function(state, outputs_ok) {
  if (identical(state$mode, "fixed")) {
    if (isTRUE(outputs_ok)) "completed_fixed" else "completed_fixed_partial"
  } else if (!isTRUE(state$converged)) {
    "completed_unconverged"
  } else if (!isTRUE(outputs_ok)) {
    "success_partial"
  } else {
    "success"
  }
}

#' Write the per-location tau_i credible-interval artifact
#'
#' Copies the 95\% interval of the upstream departure-probability fit
#' (\code{fit_prob_travel()}, written to
#' \code{MODEL_INPUT/mobility_travel_prob_params.csv}) into
#' \code{1_inputs/mobility_tau_ci.csv}, in config location order, so
#' \code{plot_departure_tau()} can draw interval bars. Written atomically.
#'
#' @param params_file Path to \code{mobility_travel_prob_params.csv}.
#' @param locations Character vector of config location ISO codes.
#' @param dir_inputs The run's \code{1_inputs} directory.
#' @return The written path, or \code{NULL} (invisibly) when the source file is
#'   absent or lacks the \code{iso3} and interval columns.
#' @noRd
.mosaic_write_tau_ci <- function(params_file, locations, dir_inputs) {
  if (length(params_file) != 1L || !file.exists(params_file)) return(invisible(NULL))
  tp <- utils::read.csv(params_file, stringsAsFactors = FALSE)
  lo_col <- intersect(c("Q2.5", "q2.5", "lower", "ci_lower"), names(tp))[1]
  hi_col <- intersect(c("Q97.5", "q97.5", "upper", "ci_upper"), names(tp))[1]
  if (!("iso3" %in% names(tp)) || is.na(lo_col) || is.na(hi_col)) return(invisible(NULL))
  locs <- as.character(locations)
  m    <- match(locs, tp$iso3)
  ci_df <- data.frame(location = locs, lower = tp[[lo_col]][m],
                      upper = tp[[hi_col]][m], stringsAsFactors = FALSE)
  out <- file.path(dir_inputs, "mobility_tau_ci.csv")
  tmp <- paste0(out, ".tmp")
  utils::write.csv(ci_df, tmp, row.names = FALSE)
  if (!file.rename(tmp, out)) {
    unlink(tmp)
    stop("could not move ", tmp, " into place")
  }
  out
}

#' Git provenance of the MOSAIC code and of the working directory
#'
#' \code{sha}/\code{branch} describe the MOSAIC package that is running:
#' the \code{RemoteSha} (or \code{GithubSHA1}) recorded by a remotes/pak
#' install when present (\code{source = "remote"}), otherwise the git checkout
#' the package was loaded from (\code{devtools::load_all()}; the checkout's
#' DESCRIPTION must name MOSAIC; \code{source = "checkout"}), otherwise NA (\code{source = "unknown"}, e.g.
#' a plain \code{R CMD INSTALL}, whose version is in \code{R$MOSAIC}). The
#' repository of the working directory, which is often a country repo rather
#' than MOSAIC, is recorded separately as \code{cwd_path}/\code{cwd_sha}/
#' \code{cwd_branch}.
#'
#' @param pkg_dir Directory the MOSAIC package was loaded from.
#' @param cwd Working directory.
#' @param desc \code{utils::packageDescription("MOSAIC")} (a list), or NULL.
#' @return Named list (empty when git is unavailable).
#' @noRd
.mosaic_git_provenance <- function(pkg_dir = system.file(package = "MOSAIC"),
                                   cwd = getwd(),
                                   desc = tryCatch(utils::packageDescription("MOSAIC"),
                                                   error = function(e) NULL)) {
  out <- list(sha = NA_character_, branch = NA_character_, source = "unknown")
  remote_sha <- NULL
  if (is.list(desc)) {
    remote_sha <- desc$RemoteSha %||% desc$GithubSHA1
    if (!is.null(remote_sha) && nzchar(remote_sha)) {
      out$sha    <- substr(remote_sha, 1L, 9L)
      out$branch <- desc$RemoteRef %||% desc$GithubRef %||% NA_character_
      out$source <- "remote"
    }
  }
  if (!nzchar(Sys.which("git"))) return(if (identical(out$source, "remote")) out else list())

  git_cmd <- function(dir, ...) {
    res <- tryCatch(
      system2("git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = FALSE),
      error = function(e) NULL, warning = function(w) NULL
    )
    if (is.null(res) || !is.character(res) || length(res) == 0) return(NA_character_)
    trimws(res[1])
  }
  in_repo <- function(dir) {
    length(dir) == 1L && nzchar(dir) && dir.exists(dir) &&
      identical(git_cmd(dir, "rev-parse", "--is-inside-work-tree"), "true")
  }

  # A MOSAIC source checkout: pkg_dir (under load_all, <src>/inst) sits in a
  # git work tree whose top level is the MOSAIC package itself -- not, say, a
  # project repo that happens to hold an renv library.
  is_mosaic_checkout <- function(dir) {
    if (!in_repo(dir)) return(FALSE)
    top <- git_cmd(dir, "rev-parse", "--show-toplevel")
    d <- file.path(top, "DESCRIPTION")
    !is.na(top) && file.exists(d) &&
      identical(tryCatch(unname(read.dcf(d, fields = "Package")[1, 1]),
                         error = function(e) NA_character_), "MOSAIC")
  }
  if (!identical(out$source, "remote") && is_mosaic_checkout(pkg_dir)) {
    out$sha    <- git_cmd(pkg_dir, "rev-parse", "--short", "HEAD")
    out$branch <- git_cmd(pkg_dir, "rev-parse", "--abbrev-ref", "HEAD")
    out$source <- "checkout"
  }
  if (in_repo(cwd)) {
    out$cwd_path   <- git_cmd(cwd, "rev-parse", "--show-toplevel")
    out$cwd_sha    <- git_cmd(cwd, "rev-parse", "--short", "HEAD")
    out$cwd_branch <- git_cmd(cwd, "rev-parse", "--abbrev-ref", "HEAD")
  }
  out
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
  py_env <- if (requireNamespace("reticulate", quietly = TRUE) &&
                reticulate::py_available(initialize = FALSE)) {
    tryCatch({
      sys      <- reticulate::import("sys", delay_load = FALSE)
      py_ver   <- strsplit(as.character(sys$version), " ")[[1]][1]
      importlib <- reticulate::import("importlib.metadata", delay_load = FALSE)
      # laser-cholera / laser-core left this list in v0.69.0 with the Python
      # engine. The Python environment now serves the suitability model only,
      # so what is worth snapshotting is the TensorFlow stack. The transmission
      # engine's version is the MOSAIC version, recorded under `R` above.
      py_pkgs  <- c("numpy", "tensorflow", "keras", "torch",
                    "sbi", "zuko", "scikit-learn")
      pkg_versions <- lapply(py_pkgs, function(pkg) {
        tryCatch(as.character(importlib$version(pkg)), error = function(e) NA_character_)
      })
      names(pkg_versions) <- paste0("pkg_", gsub("-", "_", py_pkgs))
      c(list(version = py_ver), pkg_versions)
    }, error = function(e) {
      list(error = paste("Python query failed:", e$message))
    })
  } else {
    list()
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
  git_env <- .mosaic_git_provenance()

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

# =============================================================================
# OUTPUT GENERATION
# =============================================================================

#' Coerce a possibly-absent JSON scalar to a rounded numeric
#' @param x Value read back from convergence_diagnostics.json.
#' @return Numeric scalar, or NA_real_ when absent/non-finite.
#' @keywords internal
#' @noRd
.mosaic_diag_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || !is.finite(x)) return(NA_real_)
  round(x, 6)
}

#' Write Summary JSON at Run Completion
#'
#' @param dirs Directory structure
#' @param state Internal calibration state
#' @param start_time POSIXct start time for wall-clock calculation
#' @param config Base simulation config (for provenance fields)
#' @param r2_cases_ensemble R-squared for cases (central tendency -- mean by
#'   default, per central_method -- of the canonical posterior ensemble;
#'   tier-selected when optimize_subset = FALSE, optimizer-refined when
#'   optimize_subset = TRUE)
#' @param r2_deaths_ensemble R-squared for deaths (canonical ensemble, see above)
#' @param n_ensemble_params Number of parameter sets in the canonical ensemble
#' @param n_ensemble_stochastic_per Stochastic reruns per parameter set
#' @param r2_cases_ensemble_tier Tier-subset R-squared for cases, preserved for
#'   comparison when optimize_subset = TRUE. NA when the flag is FALSE (in which
#'   case r2_cases_ensemble already holds the tier metric).
#' @param r2_deaths_ensemble_tier Tier-subset R-squared for deaths (see above).
#' @param bias_ratio_cases_ensemble_tier Tier-subset bias ratio for cases.
#' @param bias_ratio_deaths_ensemble_tier Tier-subset bias ratio for deaths.
#' @param n_ensemble_params_tier Tier-subset param count (= n_ensemble_params when
#'   optimize_subset = FALSE; typically larger than n_ensemble_params when TRUE).
#' @param central_method Resolved per-channel central tendency
#'   (\code{c(cases=, deaths=)}) that the canonical r2_*_ensemble fields and the
#'   prediction plots use. Recorded as provenance in summary.json.
#' @param r2_cases_ensemble_mean,r2_deaths_ensemble_mean,r2_cases_ensemble_median,r2_deaths_ensemble_median
#'   Ensemble R-squared computed against BOTH the weighted mean and weighted
#'   median, independent of \code{central_method}, for transition cross-walk.
#' @param bias_ratio_cases_ensemble_mean,bias_ratio_deaths_ensemble_mean,bias_ratio_cases_ensemble_median,bias_ratio_deaths_ensemble_median
#'   Ensemble bias ratios against both tendencies (see above).
#' @param posthoc_criteria_met Whether a post-hoc best-subset tier met all its targets.
#' @param io I/O settings for JSON writing
#' @noRd
.mosaic_write_summary_json <- function(dirs, state, start_time, config,
                                       nb_dispersion = NULL,
                                       r2_cases_ensemble = NA_real_,
                                       r2_deaths_ensemble = NA_real_,
                                       bias_ratio_cases_ensemble = NA_real_,
                                       bias_ratio_deaths_ensemble = NA_real_,
                                       central_method = NULL,
                                       r2_cases_ensemble_mean = NA_real_,
                                       r2_deaths_ensemble_mean = NA_real_,
                                       r2_cases_ensemble_median = NA_real_,
                                       r2_deaths_ensemble_median = NA_real_,
                                       bias_ratio_cases_ensemble_mean = NA_real_,
                                       bias_ratio_deaths_ensemble_mean = NA_real_,
                                       bias_ratio_cases_ensemble_median = NA_real_,
                                       bias_ratio_deaths_ensemble_median = NA_real_,
                                       n_ensemble_params = NA_integer_,
                                       n_ensemble_stochastic_per = NA_integer_,
                                       r2_cases_ensemble_tier = NA_real_,
                                       r2_deaths_ensemble_tier = NA_real_,
                                       bias_ratio_cases_ensemble_tier = NA_real_,
                                       bias_ratio_deaths_ensemble_tier = NA_real_,
                                       n_ensemble_params_tier = NA_integer_,
                                       cfr_implied = NULL,
                                       posthoc_criteria_met = NA,
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
    resumed       = isTRUE(state$resumed),
    n_simulations_reused = if (!is.null(state$n_sims_reused)) as.integer(state$n_sims_reused) else 0L,
    # Run statistics
    wall_time_seconds          = round(wall_time, 1),
    n_batches                  = state$batch_number,
    # n_simulations_total reports the count of usable shards in samples.parquet
    # (= nrow(results)), matching the denominator used by n_retained and the
    # implied success rate. n_sim_id_frontier preserves the prior
    # state$total_sims_run semantic (max sim_id ever attempted, includes
    # failed-and-skipped sim ids); the two differ exactly by the failed-sim
    # count after resume-with-gaps or on any partial worker failure.
    n_simulations_total        = if (!is.null(diag$summary$total_simulations_original)) {
                                   as.integer(diag$summary$total_simulations_original)
                                 } else as.integer(state$total_sims_run),
    n_sim_id_frontier          = as.integer(state$total_sims_run),
    n_simulations_successful   = state$total_sims_successful,
    n_retained                 = if (!is.null(diag$summary$retained_simulations)) diag$summary$retained_simulations else NA_integer_,
    # n_best_subset reflects the subset that r2_*_ensemble was actually
    # computed against — the optimizer-refined one when optimize_subset = TRUE
    # and the optimizer landed a refined subset, otherwise the tier subset.
    # n_best_subset_tier preserves the pre-opt size for provenance.
    n_best_subset              = if (!is.null(diag$summary$n_best_subset_optimized)) {
                                   as.integer(diag$summary$n_best_subset_optimized)
                                 } else if (!is.null(diag$metrics$B_size$value)) {
                                   as.integer(diag$metrics$B_size$value)
                                 } else NA_integer_,
    n_best_subset_tier         = if (!is.null(diag$metrics$B_size$value)) as.integer(diag$metrics$B_size$value) else NA_integer_,
    # Convergence and model fit. The pipeline produces the posterior ENSEMBLE
    # and the MEDOID member; the single best-likelihood model is not produced,
    # so summary.json reports ensemble (+ tier) fit metrics only.
    # `converged` is the calibration ESS stopping criterion, evaluated only in
    # auto mode; a fixed-mode run never evaluates it, so it is NA (null), not
    # FALSE. `posthoc_criteria_met` is whether a post-hoc best-subset tier met
    # all its targets (FALSE = fallback subset), in either mode.
    converged     = .mosaic_run_converged(state),
    posthoc_criteria_met = as.logical(posthoc_criteria_met)[1],
    r2_cases_ensemble  = if (!is.na(r2_cases_ensemble)) round(r2_cases_ensemble, 4) else NA_real_,
    r2_deaths_ensemble = if (!is.na(r2_deaths_ensemble)) round(r2_deaths_ensemble, 4) else NA_real_,
    bias_ratio_cases_ensemble  = if (!is.na(bias_ratio_cases_ensemble)) round(bias_ratio_cases_ensemble, 4) else NA_real_,
    bias_ratio_deaths_ensemble = if (!is.na(bias_ratio_deaths_ensemble)) round(bias_ratio_deaths_ensemble, 4) else NA_real_,
    # Resolved central tendency the canonical r2_*_ensemble fields + plots use.
    central_method_cases  = if (!is.null(central_method)) central_method[["cases"]]  else NA_character_,
    central_method_deaths = if (!is.null(central_method)) central_method[["deaths"]] else NA_character_,
    # Dual ensemble metrics (BOTH tendencies, central_method-independent) so
    # median runs stay comparable after the default flipped to mean.
    r2_cases_ensemble_mean     = if (!is.na(r2_cases_ensemble_mean))     round(r2_cases_ensemble_mean, 4)     else NA_real_,
    r2_deaths_ensemble_mean    = if (!is.na(r2_deaths_ensemble_mean))    round(r2_deaths_ensemble_mean, 4)    else NA_real_,
    r2_cases_ensemble_median   = if (!is.na(r2_cases_ensemble_median))   round(r2_cases_ensemble_median, 4)   else NA_real_,
    r2_deaths_ensemble_median  = if (!is.na(r2_deaths_ensemble_median))  round(r2_deaths_ensemble_median, 4)  else NA_real_,
    bias_ratio_cases_ensemble_mean    = if (!is.na(bias_ratio_cases_ensemble_mean))    round(bias_ratio_cases_ensemble_mean, 4)    else NA_real_,
    bias_ratio_deaths_ensemble_mean   = if (!is.na(bias_ratio_deaths_ensemble_mean))   round(bias_ratio_deaths_ensemble_mean, 4)   else NA_real_,
    bias_ratio_cases_ensemble_median  = if (!is.na(bias_ratio_cases_ensemble_median))  round(bias_ratio_cases_ensemble_median, 4)  else NA_real_,
    bias_ratio_deaths_ensemble_median = if (!is.na(bias_ratio_deaths_ensemble_median)) round(bias_ratio_deaths_ensemble_median, 4) else NA_real_,
    n_ensemble_params          = if (!is.na(n_ensemble_params)) as.integer(n_ensemble_params) else NA_integer_,
    n_ensemble_stochastic_per  = if (!is.na(n_ensemble_stochastic_per)) as.integer(n_ensemble_stochastic_per) else NA_integer_,
    # Tier-subset ensemble metrics — populated ONLY when optimize_subset = TRUE,
    # preserved for comparison. When the flag is off, the canonical
    # r2_cases_ensemble fields above ARE the tier metrics and these fields are NA.
    r2_cases_ensemble_tier  = if (!is.na(r2_cases_ensemble_tier)) round(r2_cases_ensemble_tier, 4) else NA_real_,
    r2_deaths_ensemble_tier = if (!is.na(r2_deaths_ensemble_tier)) round(r2_deaths_ensemble_tier, 4) else NA_real_,
    bias_ratio_cases_ensemble_tier  = if (!is.na(bias_ratio_cases_ensemble_tier)) round(bias_ratio_cases_ensemble_tier, 4) else NA_real_,
    bias_ratio_deaths_ensemble_tier = if (!is.na(bias_ratio_deaths_ensemble_tier)) round(bias_ratio_deaths_ensemble_tier, 4) else NA_real_,
    n_ensemble_params_tier = if (!is.na(n_ensemble_params_tier)) as.integer(n_ensemble_params_tier) else NA_integer_,
    # ESS summary
    ess_n_params        = ess_stats$n_params,
    ess_n_above_target  = ess_stats$n_above_target,
    ess_pct_above_target = ess_stats$pct_above_target,
    ess_target          = ess_stats$target,
    ess_min             = ess_stats$min_ess,
    ess_median          = ess_stats$median_ess,
    # Exact (untruncated) importance-sampling diagnostics. REPORTED, NOT GATED.
    # `ess_best` above is computed on delta-AIC-truncated weights, which bound
    # every weight into a narrow band and therefore keep ESS_B high regardless
    # of fit. These fields are the honest IS numbers: ess_is_all is the
    # effective sample size of the raw likelihood weights over all draws, and
    # khat_all >= 0.7 means IS estimates are unreliable. A large gap between
    # ess_best and ess_is_all is expected and is the point of reporting both.
    # Metrics on the subset the posterior ACTUALLY uses. The gated ess_best /
    # A / CVw above are scored on the TIER subset; when subset optimization
    # succeeds, the canonical posterior is built from the smaller optimized
    # subset instead. A gap between these two groups means the convergence
    # verdict describes draws the run does not use. Reported, never gated.
    ess_best_optimized  = .mosaic_diag_num(diag$metrics$ess_best_optimized$value),
    A_best_optimized    = .mosaic_diag_num(diag$metrics$A_B_optimized$value),
    cvw_best_optimized  = .mosaic_diag_num(diag$metrics$cvw_B_optimized$value),
    ess_is_optimized    = .mosaic_diag_num(diag$metrics$ess_is_optimized$value),
    ess_is_best         = .mosaic_diag_num(diag$importance_sampling$best_subset$ess_is),
    ess_is_all          = .mosaic_diag_num(diag$importance_sampling$all_draws$ess_is),
    ess_is_all_prop     = .mosaic_diag_num(diag$importance_sampling$all_draws$ess_is_prop),
    khat_all            = .mosaic_diag_num(diag$importance_sampling$all_draws$khat),
    khat_all_status     = if (!is.null(diag$importance_sampling$all_draws$khat_status)) {
                              as.character(diag$importance_sampling$all_draws$khat_status)
                          } else NA_character_,
    n_positive_ratios_all = if (!is.null(diag$importance_sampling$all_draws$n_positive_ratios)) {
                              as.integer(diag$importance_sampling$all_draws$n_positive_ratios)
                          } else NA_integer_,
    # NB dispersion actually used, estimated once from the weekly observations
    # by est_nb_dispersion(). bound_binds is a standing diagnostic: in a
    # well-specified fit the hard bounds should rarely bind.
    nb_dispersion       = if (!is.null(nb_dispersion)) {
                              .k <- nb_dispersion$k
                              .ch <- nb_dispersion$channel
                              f <- function(ch) {
                                   v <- .k[.ch == ch]
                                   list(median_k = if (any(is.finite(v))) round(stats::median(v[is.finite(v)]), 4) else NA_real_,
                                        n_estimated = sum(is.finite(v)),
                                        n_poisson   = sum(is.infinite(v)))
                              }
                              # report EVERY status, so a new fit path cannot be
                              # invisible in the diagnostics
                              .st <- as.list(table(nb_dispersion$status))
                              c(list(cases = f("cases"), deaths = f("deaths"),
                                     bound_binds = sum(nb_dispersion$status == "clamped_lower_bound", na.rm = TRUE)),
                                list(status_counts = .st))
                          } else NULL,
    # Implied CFR per location (period-weighted from posterior ensemble
    # predictions: sum simulated reported_deaths / sum simulated reported_cases
    # over the calibration window, per ensemble member). Reports median +
    # 95% CI across members, plus the observed period CFR for direct comparison.
    # NA when ensemble was not computed (e.g., very small calibrations).
    cfr_implied         = if (!is.null(cfr_implied)) {
                              lapply(cfr_implied, function(x) lapply(x, function(v) {
                                   if (is.numeric(v) && is.finite(v)) round(v, 6) else v
                              }))
                          } else NULL
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
#' Series are \code{[n_loc x n_time]} matrices (a vector is one location).
#' A cell is scored when both its observation and its estimate are finite
#' (the estimate is NA where the scoring mask removed it). A trailing window
#' \code{last_<w>obs} is the last \code{w} time steps that carry at least one
#' scored cell for that channel, pooling every location's scored cells in
#' those steps; for one location this is the last \code{w} scored
#' observations. Dates are indexed by time step.
#'
#' @param obs_cases Observed cases, \code{[n_loc x n_time]} matrix or vector.
#' @param est_cases Estimated cases, same shape.
#' @param obs_deaths Observed deaths, same shape.
#' @param est_deaths Estimated deaths, same shape.
#' @param dates Date vector of length \code{n_time}.
#' @param windows Integer vector of trailing observation counts (e.g. c(365, 120, 90, 60, 30)).
#' @return data.frame with one row per window plus a "full" row; \code{n_obs}
#'   is the number of scored cells.
#' @noRd
.mosaic_compute_windowed_metrics <- function(obs_cases, est_cases,
                                             obs_deaths, est_deaths,
                                             dates, windows = c(365, 120, 90, 60, 30)) {

  as_mat <- function(x) if (is.null(dim(x))) matrix(x, nrow = 1L) else as.matrix(x)
  obs_cases  <- as_mat(obs_cases);  est_cases  <- as_mat(est_cases)
  obs_deaths <- as_mat(obs_deaths); est_deaths <- as_mat(est_deaths)
  if (!identical(dim(obs_cases), dim(est_cases)) ||
      !identical(dim(obs_deaths), dim(est_deaths))) {
    stop("observed and estimated series must have the same dimensions")
  }
  n_time <- ncol(obs_cases)

  ok_c <- is.finite(obs_cases)  & is.finite(est_cases)
  ok_d <- is.finite(obs_deaths) & is.finite(est_deaths)
  # Time steps carrying at least one scored cell, per channel.
  valid_t_c <- which(colSums(ok_c) > 0L)
  valid_t_d <- which(colSums(ok_d) > 0L)

  date_at <- function(t) {
    if (length(dates) == n_time) as.character(dates[t]) else NA_character_
  }

  compute_row <- function(label, t_c, t_d) {
    sel_c <- ok_c; sel_c[, -t_c] <- FALSE
    sel_d <- ok_d; sel_d[, -t_d] <- FALSE
    if (!length(t_c)) sel_c[] <- FALSE
    if (!length(t_d)) sel_d[] <- FALSE
    n_c <- sum(sel_c); n_d <- sum(sel_d)

    r2_c <- bias_c <- NA_real_
    if (n_c > 2) {
      o <- obs_cases[sel_c]; e <- est_cases[sel_c]
      r2_c <- calc_model_R2(o, e)
      bias_c <- calc_bias_ratio(o, e)
    }
    r2_d <- bias_d <- NA_real_
    if (n_d > 2) {
      o <- obs_deaths[sel_d]; e <- est_deaths[sel_d]
      r2_d <- calc_model_R2(o, e)
      bias_d <- calc_bias_ratio(o, e)
    }

    t_all <- sort(unique(c(t_c, t_d)))
    data.frame(
      window      = label,
      n_obs       = max(n_c, n_d),
      date_start  = if (length(t_all)) date_at(min(t_all)) else NA_character_,
      date_end    = if (length(t_all)) date_at(max(t_all)) else NA_character_,
      r2_cases    = round(r2_c, 4),
      bias_cases  = round(bias_c, 4),
      r2_deaths   = round(r2_d, 4),
      bias_deaths = round(bias_d, 4),
      stringsAsFactors = FALSE
    )
  }

  rows <- list()

  # Full series
  rows[[1]] <- compute_row("full", valid_t_c, valid_t_d)

  # Trailing windows
  for (w in windows) {
    t_c <- if (length(valid_t_c) >= w) utils::tail(valid_t_c, w) else integer(0)
    t_d <- if (length(valid_t_d) >= w) utils::tail(valid_t_d, w) else integer(0)
    if (length(t_c) == 0 && length(t_d) == 0) next
    # The window measures "last w time steps with a scored observation", not
    # last w calendar days. For weekly-reporting countries the temporal span
    # can be ~7x the day count. Label as "last_<w>obs" so an operator (or AI
    # tail) doesn't mistake it for a day window.
    rows[[length(rows) + 1]] <- compute_row(paste0("last_", w, "obs"), t_c, t_d)
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
  # Strip the "last_" prefix AND the trailing "obs" so the plot axis still
  # reads as 30/60/90/etc. (the "obs" unit is implicit on a plot — it is
  # made explicit in the CSV column and log line).
  window_display <- gsub("obs$", "", gsub("^last_", "", window_labels))
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

