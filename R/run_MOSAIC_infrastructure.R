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
#' @param summary The summary.json list (\code{.mosaic_write_summary_json()}),
#'   whose convergence status run_state.json and _README.md then report.
#' @noRd
.mosaic_finalize_state <- function(state_file, summary = NULL) {
  if (!file.exists(state_file)) return(invisible(NULL))
  persisted <- tryCatch(
    jsonlite::read_json(state_file),
    error = function(e) NULL
  )
  if (is.null(persisted)) return(invisible(NULL))
  persisted$status <- "completed"
  persisted$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  # The final status is summary.json's (it describes the members the posterior
  # uses; fixed mode never applies the stopping rule, so stopping_rule_met is
  # null there), so the two files agree (P13).
  one <- function(x) if (length(x) == 1L && !is.na(x)) x else NULL
  if (!is.null(summary)) {
    persisted["stopping_rule_met"] <- list(one(summary$stopping_rule_met))
    persisted["convergence_status"] <- list(one(summary$convergence_status))
  }
  tmp <- tempfile(tmpdir = dirname(state_file), fileext = ".json.tmp")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(persisted, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  file.rename(tmp, state_file)

  # Write _README.md completion signal to run root directory
  # state_file path: dir_output/2_calibration/state/run_state.json (3 levels deep)
  dir_output <- dirname(dirname(dirname(state_file)))
  readme_path <- file.path(dir_output, "_README.md")
  conv <- one(summary$convergence_status)
  readme_lines <- c(
    "# MOSAIC Run Complete",
    "",
    paste0("**Status:** completed"),
    paste0("**Simulation convergence:** ",
           if (is.null(conv)) "not assessed"
           else sprintf("%s -- %s%s", toupper(conv), one(summary$convergence_label) %||% "",
                        if (identical(summary$mode, "fixed")) " (fixed budget: assessed, not used to stop)" else "")),
    if (!is.null(conv) && conv != "green" && nzchar(one(summary$convergence_reasons_text) %||% ""))
      paste0("**Reasons:** ", summary$convergence_reasons_text),
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
#' The posterior/medoid/trajectory/spatial artifacts, the per-location
#' prediction and trajectory CSVs, the fit/optimizer/CFR/HSIC tables and the
#' post-hoc reproductive-number tables are rebuilt from the
#' current run's samples, but several writers are conditional (optimizer on,
#' medoid found, arrays present). Deleting them before they are rebuilt means a
#' skipped or failed block leaves no file rather than a stale one from a
#' previous run into the same \code{dir_output}. \code{3_results/summary.json}
#' goes too: it marks a completed run, it is written only after the in-run
#' render, and the renderer reads its \code{central_method_*} first, so an
#' earlier run's copy would set this run's figures' central line.
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
  # Conditional 3_results tables. The per-location prediction CSVs matter most:
  # the combine step globs predictions_<type>_*.csv, so a re-run with fewer
  # locations would otherwise fold the earlier run's files into *_all.csv.
  # trajectories_<LOC>.csv is written only when capture_trajectories is on and
  # the reduction succeeds, and it is what the MOSAIC-results promoter archives.
  if (!is.null(dirs$res_predictions) && dir.exists(dirs$res_predictions))
    paths <- c(paths, list.files(dirs$res_predictions,
                                 pattern = "^(predictions_(ensemble|medoid)|trajectories)_.*\\.csv$",
                                 full.names = TRUE))
  # parameter_sensitivity.csv is written inside a non-fatal block (and skipped
  # with < 10 finite sims or < 2 sampled parameters); render_MOSAIC_figures()
  # plots any existing copy instead of recomputing, so a stale one would be
  # rendered as this run's HSIC table.
  if (!is.null(dirs$res_fig_diag))
    paths <- c(paths, file.path(dirs$res_fig_diag,
                                c("model_fit_windows.csv", "optimization_diagnostics.csv",
                                  "parameter_sensitivity.csv", "calibration_report.pdf",
                                  "convergence_status.csv", "convergence_status.pdf")))
  # reproductive_numbers.{csv,rds} come from add_reproductive_numbers() on
  # trajectories_ensemble.rds, which is rebuilt above; with overwrite = FALSE
  # it would otherwise return the earlier run's R_eff as "skipped_exists".
  if (!is.null(dirs$res_posterior))
    paths <- c(paths, file.path(dirs$res_posterior,
                                c("cfr_posterior.csv", "reproductive_numbers.csv",
                                  "reproductive_numbers.rds")))
  # calibration_diagnostics.json is what the report is drawn from; the
  # convergence_status.* files are the pre-1.4.0 table, no longer written.
  if (!is.null(dirs$results))
    paths <- c(paths, file.path(dirs$results, c("summary.json", "calibration_diagnostics.json")))
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
#' The calibration ESS criterion is evaluated only in auto mode. A fixed-mode
#' run keeps the documented logical \code{converged = FALSE} (the criterion
#' was not met because it was never tested) but records
#' \code{convergence_evaluated = FALSE} and its own status rather than
#' "completed_unconverged". Status tree:
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
#' @return \code{.mosaic_run_converged()}: \code{TRUE}/\code{FALSE} (always
#'   \code{FALSE} in fixed mode). \code{.mosaic_convergence_evaluated()}:
#'   \code{FALSE} in fixed mode. \code{.mosaic_run_status()}: the status string.
#' @noRd
.mosaic_run_converged <- function(state) {
  !identical(state$mode, "fixed") && isTRUE(state$converged)
}

#' Whether the calibration ESS criterion was evaluated (FALSE in fixed mode)
#' @noRd
.mosaic_convergence_evaluated <- function(state) {
  !identical(state$mode, "fixed")
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
#' Copies the 95 percent interval of the upstream departure-probability fit
#' (\code{fit_prob_travel()}, written to
#' \code{MODEL_INPUT/mobility_travel_prob_params.csv}) into
#' \code{1_inputs/mobility_tau_ci.csv}, in config location order, so
#' \code{plot_departure_tau()} can draw interval bars. Written atomically.
#'
#' @param params_file Path to \code{mobility_travel_prob_params.csv}.
#' @param locations Character vector of config location ISO codes.
#' @param dir_inputs The run's \code{1_inputs} directory.
#' @return The written path, or \code{NULL} (invisibly) when the source file is
#'   absent or lacks the \code{iso3} and interval columns; in that case any
#'   \code{mobility_tau_ci.csv} left by an earlier run is removed.
#' @noRd
.mosaic_write_tau_ci <- function(params_file, locations, dir_inputs) {
  out <- file.path(dir_inputs, "mobility_tau_ci.csv")
  no_ci <- function() {
    if (file.exists(out)) unlink(out)
    invisible(NULL)
  }
  if (length(params_file) != 1L || !file.exists(params_file)) return(no_ci())
  tp <- utils::read.csv(params_file, stringsAsFactors = FALSE)
  lo_col <- intersect(c("Q2.5", "q2.5", "lower", "ci_lower"), names(tp))[1]
  hi_col <- intersect(c("Q97.5", "q97.5", "upper", "ci_upper"), names(tp))[1]
  if (!("iso3" %in% names(tp)) || is.na(lo_col) || is.na(hi_col)) return(no_ci())
  locs <- as.character(locations)
  m    <- match(locs, tp$iso3)
  ci_df <- data.frame(location = locs, lower = tp[[lo_col]][m],
                      upper = tp[[hi_col]][m], stringsAsFactors = FALSE)
  tmp <- paste0(out, ".tmp")
  utils::write.csv(ci_df, tmp, row.names = FALSE)
  if (!file.rename(tmp, out)) {
    unlink(tmp)
    stop("could not move ", tmp, " into place")
  }
  out
}

#' Git provenance of the working directory and of the MOSAIC code
#'
#' \code{sha}/\code{branch}/\code{path} describe the git repository of the
#' working directory -- usually the country repo the run was launched from --
#' and are omitted when the working directory is not in a git repository. This
#' is the long-standing meaning of these fields, which downstream tools read
#' as the country-repo commit (MOSAIC-OCV \code{promote_model.R}).
#'
#' \code{mosaic_sha}/\code{mosaic_branch}/\code{mosaic_source} describe the
#' MOSAIC package that is running: the \code{RemoteSha} (or
#' \code{GithubSHA1}) recorded by a remotes/pak install when present
#' (\code{mosaic_source = "remote"}), otherwise the git checkout the package
#' was loaded from (\code{devtools::load_all()}; the checkout's DESCRIPTION
#' must name MOSAIC; \code{"checkout"}), otherwise NA (\code{"unknown"}, e.g.
#' a plain \code{R CMD INSTALL}, whose version is in \code{R$MOSAIC}).
#'
#' @param pkg_dir Directory the MOSAIC package was loaded from.
#' @param cwd Working directory.
#' @param desc \code{utils::packageDescription("MOSAIC")} (a list), or NULL.
#' @return Named list.
#' @noRd
.mosaic_git_provenance <- function(pkg_dir = system.file(package = "MOSAIC"),
                                   cwd = getwd(),
                                   desc = tryCatch(utils::packageDescription("MOSAIC"),
                                                   error = function(e) NULL)) {
  out <- list()
  mosaic <- list(mosaic_sha = NA_character_, mosaic_branch = NA_character_,
                 mosaic_source = "unknown")
  if (is.list(desc)) {
    remote_sha <- desc$RemoteSha %||% desc$GithubSHA1
    if (!is.null(remote_sha) && nzchar(remote_sha)) {
      mosaic$mosaic_sha    <- substr(remote_sha, 1L, 9L)
      mosaic$mosaic_branch <- desc$RemoteRef %||% desc$GithubRef %||% NA_character_
      mosaic$mosaic_source <- "remote"
    }
  }
  if (!nzchar(Sys.which("git"))) return(c(out, mosaic))

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

  if (in_repo(cwd)) {
    out$sha    <- git_cmd(cwd, "rev-parse", "--short", "HEAD")
    out$branch <- git_cmd(cwd, "rev-parse", "--abbrev-ref", "HEAD")
    out$path   <- git_cmd(cwd, "rev-parse", "--show-toplevel")
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
  if (!identical(mosaic$mosaic_source, "remote") && is_mosaic_checkout(pkg_dir)) {
    mosaic$mosaic_sha    <- git_cmd(pkg_dir, "rev-parse", "--short", "HEAD")
    mosaic$mosaic_branch <- git_cmd(pkg_dir, "rev-parse", "--abbrev-ref", "HEAD")
    mosaic$mosaic_source <- "checkout"
  }
  c(out, mosaic)
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

#' Write Summary JSON at Run Completion (summary.json schema 2)
#'
#' The machine-readable run summary, \code{3_results/summary.json}, in the
#' MOSAIC 1.4.0 schema 2 (claude/plan_convergence/SCHEMA_v2.md section 1):
#' provenance and counts, the simulation-convergence status
#' (\code{convergence_status}, reasons, flags, the four checks) with every
#' quantity behind it, the fit metrics (unchanged), and the deprecated aliases
#' kept for one release (\code{converged}, \code{convergence_evaluated},
#' \code{posthoc_criteria_met}, \code{weight_health_status},
#' \code{ess_coarsened}, \code{n_best_subset}, \code{ess_min},
#' \code{ess_median}, \code{ess_target}). Missing numbers are JSON null.
#'
#' @param dirs Directory structure
#' @param state Internal calibration state
#' @param start_time POSIXct start time for wall-clock calculation
#' @param config Base simulation config (for provenance fields)
#' @param nb_dispersion The per-location NB dispersion table.
#' @param r2_cases_ensemble,r2_deaths_ensemble,bias_ratio_cases_ensemble,bias_ratio_deaths_ensemble
#'   Fit of the canonical ensemble's central line (central_method).
#' @param central_method Resolved per-channel central tendency.
#' @param r2_cases_ensemble_mean,r2_deaths_ensemble_mean,r2_cases_ensemble_median,r2_deaths_ensemble_median
#'   Ensemble R-squared against both tendencies, for cross-walk.
#' @param bias_ratio_cases_ensemble_mean,bias_ratio_deaths_ensemble_mean,bias_ratio_cases_ensemble_median,bias_ratio_deaths_ensemble_median
#'   Ensemble bias ratios against both tendencies.
#' @param n_ensemble_params Parameter sets in the canonical ensemble.
#' @param n_ensemble_stochastic_per Stochastic reruns per parameter set.
#' @param r2_cases_ensemble_tier,r2_deaths_ensemble_tier,bias_ratio_cases_ensemble_tier,bias_ratio_deaths_ensemble_tier,n_ensemble_params_tier
#'   Fit of the members before the subset optimiser (NA when it did not run).
#' @param cfr_implied Implied CFR per location.
#' @param conv_health The \code{calc_weight_health()} result describing the
#'   members the posterior uses (NULL when no posterior was built).
#' @param weighting \code{control$targets$posterior_weighting}.
#' @param param_ess The parameter_ess table (\code{ess} column), or NULL.
#' @param n_simulations_total Draws in samples.parquet.
#' @param n_retained Draws passing the Tukey fence (legacy schemes; NA otherwise).
#'   Outside "saturated" / "tempered" the fields that carry nothing there
#'   (\code{n_retained} when NA, the \code{*_tier} fit when the optimiser did not
#'   run, \code{A_best_optimized}, \code{ess_n_above_target},
#'   \code{ess_pct_above_target}) are not written.
#' @param n_members_candidate Members before the subset optimiser.
#' @param opt_metrics Legacy ESS_B / A / CVw of the optimised subset, or NULL.
#' @param io I/O settings
#' @return The summary list (invisibly written to \code{3_results/summary.json}).
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
                                       conv_health = NULL,
                                       weighting = NA_character_,
                                       param_ess = NULL,
                                       n_simulations_total = NA_integer_,
                                       n_retained = NA_integer_,
                                       n_members_candidate = NA_integer_,
                                       opt_metrics = NULL,
                                       io) {
  r4 <- function(x) if (length(x) == 1L && is.finite(x)) round(x, 4) else NA_real_
  int1 <- function(x) if (length(x) == 1L && !is.na(x)) as.integer(x) else NA_integer_

  # Per-parameter ESS of the members' weights: a diagnostic.
  pe <- if (!is.null(param_ess) && "ess" %in% names(param_ess)) param_ess$ess else numeric(0)
  pe <- pe[is.finite(pe)]
  fields <- .mosaic_weight_health_fields(conv_health)
  fields <- .mosaic_schema2_round1_fields(fields, conv_health, length(config$location_name),
                                          conv_health$coarsened$alpha %||% NA_real_)
  fields$posterior_weighting <- as.character(weighting)[1]
  legacy <- isTRUE(fields$posterior_weighting %in% c("saturated", "tempered"))
  ess_target <- fields$ess_best_target
  auto <- !identical(state$mode, "fixed")
  stopping_rule_met <- if (auto) isTRUE(state$converged) else NA

  wall_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  summary_obj <- list(
    schema_version = "2",
    # Provenance: what was run and when
    location      = paste(config$location_name, collapse = ", "),
    date_start    = config$date_start,
    date_stop     = config$date_stop,
    timestamp     = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
    mode          = if (!is.null(state$mode)) state$mode else NA_character_,
    resumed       = isTRUE(state$resumed),
    n_simulations_reused = if (!is.null(state$n_sims_reused)) as.integer(state$n_sims_reused) else 0L,
    # Run statistics. n_simulations_total counts the draws in samples.parquet;
    # n_sim_id_frontier is the largest sim id attempted (they differ by the
    # failed-sim count).
    wall_time_seconds          = round(wall_time, 1),
    n_batches                  = state$batch_number,
    n_simulations_total        = int1(if (is.na(n_simulations_total)) state$total_sims_run else n_simulations_total),
    n_sim_id_frontier          = as.integer(state$total_sims_run),
    n_simulations_successful   = state$total_sims_successful,
    n_retained                 = int1(n_retained),
    # Simulation convergence (calc_convergence_status()): adaptive runs stop on
    # it; fixed-budget runs assess it.
    stopping_rule_met          = stopping_rule_met
  )
  summary_obj <- c(summary_obj, fields[setdiff(names(fields), c("ess_coarsened", "weight_health_status"))])
  summary_obj$param_ess_min    <- if (length(pe)) round(min(pe), 1) else NA_real_
  summary_obj$param_ess_median <- if (length(pe)) round(stats::median(pe), 1) else NA_real_

  summary_obj <- c(summary_obj, list(
    # Fit of the canonical ensemble (the optimised one when optimize_subset =
    # TRUE produced a subset): central line per central_method.
    r2_cases_ensemble  = r4(r2_cases_ensemble),
    r2_deaths_ensemble = r4(r2_deaths_ensemble),
    bias_ratio_cases_ensemble  = r4(bias_ratio_cases_ensemble),
    bias_ratio_deaths_ensemble = r4(bias_ratio_deaths_ensemble),
    central_method_cases  = if (!is.null(central_method)) central_method[["cases"]]  else NA_character_,
    central_method_deaths = if (!is.null(central_method)) central_method[["deaths"]] else NA_character_,
    r2_cases_ensemble_mean     = r4(r2_cases_ensemble_mean),
    r2_deaths_ensemble_mean    = r4(r2_deaths_ensemble_mean),
    r2_cases_ensemble_median   = r4(r2_cases_ensemble_median),
    r2_deaths_ensemble_median  = r4(r2_deaths_ensemble_median),
    bias_ratio_cases_ensemble_mean    = r4(bias_ratio_cases_ensemble_mean),
    bias_ratio_deaths_ensemble_mean   = r4(bias_ratio_deaths_ensemble_mean),
    bias_ratio_cases_ensemble_median  = r4(bias_ratio_cases_ensemble_median),
    bias_ratio_deaths_ensemble_median = r4(bias_ratio_deaths_ensemble_median),
    n_ensemble_params          = int1(n_ensemble_params),
    n_ensemble_stochastic_per  = int1(n_ensemble_stochastic_per),
    # Members before the subset optimiser and their fit: populated only when
    # optimize_subset = TRUE produced a subset.
    n_best_subset_tier = int1(n_members_candidate),
    r2_cases_ensemble_tier  = r4(r2_cases_ensemble_tier),
    r2_deaths_ensemble_tier = r4(r2_deaths_ensemble_tier),
    bias_ratio_cases_ensemble_tier  = r4(bias_ratio_cases_ensemble_tier),
    bias_ratio_deaths_ensemble_tier = r4(bias_ratio_deaths_ensemble_tier),
    n_ensemble_params_tier = int1(n_ensemble_params_tier),
    # Legacy ESS_B / A / CVw of the optimised subset (opt-in optimiser only).
    ess_best_optimized  = r4(opt_metrics$ess %||% NA_real_),
    A_best_optimized    = r4(opt_metrics$A %||% NA_real_),
    cvw_best_optimized  = r4(opt_metrics$cvw %||% NA_real_),
    # Per-parameter ESS against the ensemble ESS target (diagnostic).
    ess_n_params         = if (length(pe)) length(pe) else NA_integer_,
    ess_n_above_target   = if (length(pe) && is.finite(ess_target)) sum(pe >= ess_target) else NA_integer_,
    ess_pct_above_target = if (length(pe) && is.finite(ess_target)) round(100 * mean(pe >= ess_target), 1) else NA_real_,
    # Deprecated aliases (schema 2; removed in the next release). converged
    # keeps its legacy meaning (FALSE in fixed mode); ess_target is now the
    # ensemble ESS target (it was the per-parameter ESS_param in 1.3.x).
    converged             = .mosaic_run_converged(state),
    convergence_evaluated = .mosaic_convergence_evaluated(state),
    posthoc_criteria_met  = if (is.na(fields$convergence_status)) NA else identical(fields$convergence_status, "green"),
    weight_health_status  = fields$weight_health_status,
    ess_coarsened         = fields$ess_coarsened,
    n_best_subset         = fields$ensemble_members,
    ess_min               = summary_obj$param_ess_min,
    ess_median            = summary_obj$param_ess_median,
    ess_target            = ess_target
  ))
  summary_obj <- c(summary_obj, list(
    # NB dispersion actually used, estimated once from the weekly observations
    # by est_nb_dispersion(). bound_binds is a standing diagnostic: in a
    # well-specified fit the hard bounds should rarely bind.
    nb_dispersion       = if (!is.null(nb_dispersion)) {
                              .k <- nb_dispersion$k
                              .ch <- nb_dispersion$channel
                              .pt <- if (is.null(nb_dispersion$panel_trend)) rep(FALSE, length(.k)) else
                                        nb_dispersion$panel_trend %in% TRUE
                              f <- function(ch) {
                                   v <- .k[.ch == ch]
                                   list(median_k = if (any(is.finite(v))) round(stats::median(v[is.finite(v)]), 4) else NA_real_,
                                        n_estimated = sum(is.finite(v)),
                                        n_poisson   = sum(is.infinite(v)),
                                        n_panel_trend = sum(.pt[.ch == ch]))
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
    cfr_implied         = if (!is.null(cfr_implied)) {
                              lapply(cfr_implied, function(x) lapply(x, function(v) {
                                   if (is.numeric(v) && is.finite(v)) round(v, 6) else v
                              }))
                          } else NULL
  ))

  # Fields that carry no information outside the legacy schemes (R10): the
  # pre-optimiser *_tier fit when the optimiser did not run, the legacy
  # agreement index, the Tukey-fence count of "bestfit", and the share of
  # per-parameter ESS above the ensemble ESS target (two different ESS).
  if (!legacy) {
    tier_f <- c("n_best_subset_tier", "r2_cases_ensemble_tier", "r2_deaths_ensemble_tier",
                "bias_ratio_cases_ensemble_tier", "bias_ratio_deaths_ensemble_tier", "n_ensemble_params_tier")
    if (all(vapply(summary_obj[tier_f], function(v) length(v) == 1L && is.na(v), logical(1))))
      summary_obj[tier_f] <- NULL
    summary_obj[c("ess_best_optimized", "A_best_optimized", "cvw_best_optimized",
                  "ess_n_above_target", "ess_pct_above_target")] <- NULL
    if (length(summary_obj$n_retained) == 1L && is.na(summary_obj$n_retained)) summary_obj$n_retained <- NULL
  }

  summary_path <- file.path(dirs$results, "summary.json")
  .mosaic_atomic_write(summary_obj, summary_path, function(data, file)
    jsonlite::write_json(data, file, pretty = TRUE, auto_unbox = TRUE,
                         digits = .MOSAIC_JSON_DIGITS, na = "null"))
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


# =============================================================================
# CALIBRATION DIAGNOSTICS (3_results/calibration_diagnostics.json, schema 2)
# =============================================================================

#' Fit tables of the canonical ensemble on the scored days
#'
#' From the ensemble prediction table (\code{.mosaic_assemble_prediction_table()},
#' whose unscored head is already \code{NA}), on the days with a finite
#' observation and central line: per channel, the R-squared and bias of the
#' central line, the 50 and 95 percent interval coverage and the weighted
#' interval score (Bracher et al. 2021, median and the two intervals; in
#' reported counts per location-day); per location and channel, R-squared,
#' bias, 95 percent coverage and the observed total; and the weekly series of
#' \code{.mosaic_calibration_weekly()}.
#'
#' The primary table rows (\code{subset = "scored"}) use every scored day, the
#' definition of summary.json's \code{r2_*_ensemble} / \code{bias_ratio_*} fields,
#' so the report and the summary agree (red team round 1, D8). When the config
#' carries a surveillance tier (\code{config$reported_tier}: 1 observed, 2
#' reconstructed, 3 imputed), a second row per channel (\code{subset =
#' "tier12"}) restricts them to tier-1/2 days.
#'
#' @param pt Prediction table (\code{location}, \code{date}, \code{metric},
#'   \code{observed}, \code{predicted_central}, \code{predicted_median},
#'   \code{ci_<k>_lower/upper}), or NULL.
#' @param config Model config (\code{reported_tier}, \code{location_name},
#'   \code{date_start}).
#' @param ensemble The \code{mosaic_ensemble} the table was assembled from, or
#'   NULL (see \code{.mosaic_calibration_weekly()}).
#' @param central_method Per-channel central method, or NULL to read it from
#'   the table.
#' @return \code{list(table, per_location, weekly, weekly_meta)} (data frames
#'   with zero rows when there is nothing to score).
#' @noRd
.mosaic_calibration_fit <- function(pt, config, ensemble = NULL, central_method = NULL) {
  wk <- .mosaic_calibration_weekly(pt, ensemble = ensemble, central_method = central_method)
  empty <- list(
    table = data.frame(metric = character(0), subset = character(0), r2 = numeric(0), bias = numeric(0),
                       cov50 = numeric(0), cov95 = numeric(0), wis = numeric(0), n_scored = integer(0)),
    per_location = data.frame(location = character(0), metric = character(0), r2 = numeric(0),
                              bias = numeric(0), cov95 = numeric(0), observed_total = numeric(0)),
    weekly = wk$weekly, weekly_meta = wk$meta)
  if (is.null(pt) || !nrow(pt)) return(empty)
  pt <- as.data.frame(pt)
  pt$date <- as.Date(pt$date)
  pt$metric <- ifelse(grepl("death", as.character(pt$metric), ignore.case = TRUE), "deaths", "cases")

  locs <- as.character(config$location_name)
  tier <- config$reported_tier
  has_tier <- !is.null(tier)
  pt$tier <- NA_integer_
  if (has_tier) {
    n_t <- if (is.matrix(tier)) ncol(tier) else length(tier) / max(1L, length(locs))
    tier <- if (is.matrix(tier)) tier else matrix(as.numeric(tier), nrow = length(locs), ncol = n_t,
                                                  byrow = length(locs) == 1L)
    t_idx <- as.integer(pt$date - as.Date(config$date_start)) + 1L
    l_idx <- match(pt$location, locs)
    ok <- !is.na(l_idx) & t_idx >= 1L & t_idx <= ncol(tier)
    pt$tier[ok] <- as.integer(tier[cbind(l_idx[ok], t_idx[ok])])
  }

  bands <- .mosaic_prediction_bands(pt)
  pt[names(bands)] <- bands

  sc <- pt[is.finite(pt$observed) & is.finite(pt$predicted_central), , drop = FALSE]
  if (!nrow(sc)) return(empty)
  med <- if ("predicted_median" %in% names(sc)) sc$predicted_median else sc$predicted_central
  sc$med <- ifelse(is.finite(med), med, sc$predicted_central)
  is_int <- function(y, l, u, a) (u - l) + (2 / a) * (l - y) * (y < l) + (2 / a) * (y - u) * (y > u)
  r2 <- function(y, x) suppressWarnings(tryCatch(calc_model_R2(y, x), error = function(e) NA_real_))
  bias <- function(y, x) suppressWarnings(tryCatch(calc_bias_ratio(y, x), error = function(e) NA_real_))
  cover <- function(y, l, u) { ok <- is.finite(l) & is.finite(u); if (any(ok)) mean(y[ok] >= l[ok] & y[ok] <= u[ok]) else NA_real_ }

  row_of <- function(x, m, subset) {
    if (!nrow(x)) return(NULL)
    okw <- is.finite(x$lo50) & is.finite(x$hi50) & is.finite(x$lo95) & is.finite(x$hi95)
    wis <- if (any(okw)) with(x[okw, ], mean((0.5 * abs(observed - med) + 0.25 * is_int(observed, lo50, hi50, 0.5) +
                                               0.025 * is_int(observed, lo95, hi95, 0.05)) / 2.5)) else NA_real_
    data.frame(metric = m, subset = subset, r2 = r2(x$observed, x$predicted_central),
               bias = bias(x$observed, x$predicted_central),
               cov50 = cover(x$observed, x$lo50, x$hi50), cov95 = cover(x$observed, x$lo95, x$hi95),
               wis = wis, n_scored = nrow(x), stringsAsFactors = FALSE)
  }
  tab <- do.call(rbind, lapply(c("cases", "deaths"), function(m) {
    x <- sc[sc$metric == m, , drop = FALSE]
    rbind(row_of(x, m, "scored"),
          if (has_tier) row_of(x[x$tier %in% c(1L, 2L), , drop = FALSE], m, "tier12"))
  }))
  per_loc <- do.call(rbind, lapply(split(sc, list(sc$location, sc$metric), drop = TRUE), function(x)
    data.frame(location = x$location[1], metric = x$metric[1], r2 = r2(x$observed, x$predicted_central),
               bias = bias(x$observed, x$predicted_central), cov95 = cover(x$observed, x$lo95, x$hi95),
               observed_total = sum(x$observed))))
  rownames(tab) <- rownames(per_loc) <- NULL
  list(table = tab %||% empty$table, per_location = per_loc[order(per_loc$location, per_loc$metric), , drop = FALSE],
       weekly = wk$weekly, weekly_meta = wk$meta)
}

#' The 95 and 50 percent interval bounds of a prediction table
#'
#' The widest \code{ci_<k>} pair is the 95 percent interval and the next the 50
#' percent interval (the run's envelope \code{c(0.025, 0.25, 0.75, 0.975)}).
#' @param pt Prediction table.
#' @return \code{list(lo95, hi95, lo50, hi50)}, \code{NA} where absent.
#' @noRd
.mosaic_prediction_bands <- function(pt) {
  ci <- grep("^ci_[0-9]+_lower$", names(pt), value = TRUE)
  k <- sub("^ci_([0-9]+)_lower$", "\\1", ci)
  width <- vapply(k, function(i) {
    w <- suppressWarnings(mean(pt[[paste0("ci_", i, "_upper")]] - pt[[paste0("ci_", i, "_lower")]], na.rm = TRUE))
    if (is.finite(w)) w else -Inf
  }, numeric(1))
  ord <- k[order(-width)]
  pick <- function(pos, side) if (length(ord) >= pos) as.numeric(pt[[paste0("ci_", ord[pos], "_", side)]])
                              else rep(NA_real_, nrow(pt))
  list(lo95 = pick(1L, "lower"), hi95 = pick(1L, "upper"), lo50 = pick(2L, "lower"), hi50 = pick(2L, "upper"))
}

#' Weekly series of the ensemble: every complete week, from weekly-summed trajectories
#'
#' One row per complete reporting week of each location-channel series of the
#' prediction table and, in a multi-location run, of the all-locations total
#' (\code{location} = \code{meta$all_locations_id}). A reporting week is the
#' seven days from Monday plus the location's \code{week_offset} (the blocks the
#' observation model and the weekly dispersion use, \code{.mosaic_week_blocks()};
#' 0 for every production location, so Monday to Sunday); it is complete when
#' all seven of its days are in the table, so the partial weeks at either end of
#' the window are left out. Per week:
#' \itemize{
#'   \item \code{scored}: all seven days are scored (a finite observation and a
#'     finite central line, the days the fit table scores), and
#'     \code{observed} is their total; otherwise \code{observed} is null, so an
#'     observed total is never compared over a short week. For the
#'     all-locations row a day counts as scored only when every location is
#'     scored on it.
#'   \item \code{predicted}, \code{lo50}, \code{hi50}, \code{lo95},
#'     \code{hi95}: null when any of the seven days is in the channel's
#'     unscored head (the burn-in, where the table's central line is
#'     \code{NA}). Otherwise, from the member trajectories: each member x
#'     stochastic run's daily series \eqn{x_{r,t}} (for the all-locations row,
#'     its sum over the locations, so the locations of one trajectory stay
#'     together) is summed to the week, \eqn{X_{r,w} = \sum_{t \in w} x_{r,t}},
#'     and the trajectories are weighted \eqn{v_r = w_p / S} (member \eqn{p}'s
#'     normalised weight shared by its \eqn{S} runs; a trajectory with a missing
#'     day that week is left out and the rest renormalised, as for the daily
#'     summaries). The central line is the weighted median
#'     (\code{central_method = "median"}) or weighted mean (\code{"mean"}) of
#'     the ENGINE-level weekly totals, the daily central-line convention; the
#'     bounds are the weighted 2.5, 25, 75 and 97.5 percent quantiles
#'     (\code{weighted_quantiles()}) of the OBSERVATION-level weekly totals
#'     (\code{cases_array} / \code{deaths_array}), the draws the daily intervals
#'     come from.
#' }
#' Summing daily medians or daily quantile bounds instead makes the line hug
#' zero (the daily median is 0 on most days of a sparse series) and the bands
#' blocky; the trajectory sums do neither. The all-locations row is built the
#' same way, never by summing per-location medians. When the locations'
#' reporting weeks differ, the all-locations row uses Monday weeks
#' (\code{meta$all_locations_week_offset}).
#'
#' When \code{ensemble} lacks its arrays (a run that did not persist them,
#' re-rendered later), the model values are the weekly sums of the table's
#' daily central line and bounds (the all-locations row: the central line
#' only, no bounds), and \code{meta$source} says so (\code{"daily_sums"}).
#' @param pt Prediction table, or NULL.
#' @param ensemble \code{mosaic_ensemble} with \code{cases_array},
#'   \code{deaths_array} and the engine arrays, or NULL.
#' @param central_method Per-channel central method; NULL reads the table's
#'   \code{central_method} column (else the package default).
#' @return \code{list(weekly, meta)}: \code{weekly} with columns
#'   \code{location, metric, week} (ISO date of the first day of the reporting
#'   week), \code{scored, observed, predicted, lo50, hi50, lo95, hi95};
#'   \code{meta} with \code{source} (\code{"trajectories"},
#'   \code{"daily_sums"} or \code{"none"}), \code{central_method},
#'   \code{intervals} per channel (\code{"observation"} or \code{"engine"}),
#'   \code{quantiles}, \code{week_start}, \code{week_offset} (per location),
#'   \code{all_locations_id} and \code{all_locations_week_offset} (NULL in a
#'   one-location run).
#' @noRd
.mosaic_calibration_weekly <- function(pt, ensemble = NULL, central_method = NULL) {
  probs <- c(0.025, 0.25, 0.75, 0.975)
  empty <- data.frame(location = character(0), metric = character(0), week = character(0), scored = logical(0),
                      observed = numeric(0), predicted = numeric(0), lo50 = numeric(0), hi50 = numeric(0),
                      lo95 = numeric(0), hi95 = numeric(0), stringsAsFactors = FALSE)
  meta <- function(source, cm = .mosaic_resolve_central_method(NULL), intervals = c(cases = NA, deaths = NA),
                   offsets = list(), all_id = NULL, all_off = NULL)
    list(source = source, central_method = as.list(cm), intervals = as.list(intervals),
         quantiles = probs, week_start = "Monday + week_offset", week_offset = offsets,
         all_locations_id = all_id, all_locations_week_offset = all_off)
  if (is.null(pt) || !nrow(pt)) return(list(weekly = empty, meta = meta("none")))
  pt <- as.data.frame(pt)
  pt$location <- as.character(pt$location)
  pt$date <- as.Date(pt$date)
  pt$metric <- ifelse(grepl("death", as.character(pt$metric), ignore.case = TRUE), "deaths", "cases")
  if (is.null(central_method) && "central_method" %in% names(pt)) {
    v <- vapply(c("cases", "deaths"), function(m) {
      x <- unique(as.character(pt$central_method[pt$metric == m]))
      x <- x[!is.na(x) & x %in% c("mean", "median")]
      if (length(x)) x[1] else NA_character_
    }, character(1))
    if (any(!is.na(v))) central_method <- v[!is.na(v)]
  }
  cm <- .mosaic_resolve_central_method(central_method)
  bands <- .mosaic_prediction_bands(pt)
  pt[names(bands)] <- bands
  pt <- pt[order(pt$location, pt$metric, pt$date), , drop = FALSE]
  locs <- unique(pt$location)

  # reporting-week offset of each location (the observation model's blocks)
  off <- stats::setNames(rep(0L, length(locs)), locs)
  wo <- suppressWarnings(as.integer(unlist(ensemble$observation_model$week_offset)))
  en <- as.character(ensemble$location_names %||% character(0))
  if (length(wo) && length(en)) {
    wo <- rep_len(wo, length(en))
    hit <- match(locs, en)
    ok <- !is.na(hit) & !is.na(wo[hit]) & wo[hit] >= 0L & wo[hit] <= 6L
    off[ok] <- wo[hit[ok]]
  }
  week_of <- function(d, o) d - ((as.integer(format(d, "%u")) - 1L - o) %% 7L)

  # trajectories: engine and observation arrays of both channels, weights
  arrays <- !is.null(ensemble) && all(vapply(c("cases", "deaths"), function(ch) {
    e <- .mosaic_engine_array(ensemble, ch); o <- ensemble[[paste0(ch, "_array")]]
    !is.null(e) && !is.null(o) && length(dim(e)) == 4L && identical(dim(e), dim(o))
  }, logical(1)))
  if (arrays) {
    dims <- dim(ensemble$cases_array)
    w <- as.numeric(ensemble$parameter_weights)
    arrays <- length(w) == dims[3] && any(is.finite(w) & w > 0)
  }
  if (arrays) {
    n_t <- dims[2]; S <- dims[4]
    w <- w / sum(w[is.finite(w)])
    sw <- rep(w, times = S) / S        # param-fastest, as as.vector(arr[l, t, , ])
    ens_dates <- seq(as.Date(ensemble$date_start), as.Date(ensemble$date_stop), length.out = ensemble$n_time_points)
    l_index <- match(locs, en)
    flat <- function(arr, l) { m <- arr[l, , , , drop = FALSE]; dim(m) <- c(n_t, dims[3] * S); m }
  }
  series <- function(days, week, eng = NULL, obs = NULL, method = "median", bands = TRUE) {
    g <- match(week, unique(week))
    n_days <- tabulate(g)
    sumg <- function(x) as.vector(rowsum(as.numeric(x), g, reorder = FALSE))
    n_model <- sumg(days$has_model); n_sc <- sumg(days$scored)
    first <- !duplicated(g)
    out <- data.frame(week = week[first], scored = n_sc == 7L,
                      observed = ifelse(n_sc == 7L, sumg(ifelse(days$scored, days$observed, 0)), NA_real_),
                      predicted = NA_real_, lo50 = NA_real_, hi50 = NA_real_, lo95 = NA_real_, hi95 = NA_real_)
    keep <- n_days == 7L & n_model == 7L
    if (!is.null(eng)) {
      kk <- which(keep)
      if (length(kk)) {
        rows <- which(g %in% kk)
        grp <- match(g[rows], kk)
        te <- rowsum(eng[rows, , drop = FALSE], grp, reorder = FALSE)
        to <- rowsum(obs[rows, , drop = FALSE], grp, reorder = FALSE)
        for (k in seq_along(kk)) {
          ve <- te[k, ]
          out$predicted[kk[k]] <- if (method == "mean") {
            okv <- is.finite(ve) & sw > 0
            if (any(okv)) sum(ve[okv] * sw[okv]) / sum(sw[okv]) else NA_real_
          } else weighted_quantiles(ve, sw, 0.5)
          out[kk[k], c("lo95", "lo50", "hi50", "hi95")] <- as.list(weighted_quantiles(to[k, ], sw, probs))
        }
      }
    } else {
      fin <- function(x) ifelse(is.finite(x), x, NA_real_)
      out$predicted <- ifelse(keep, sumg(fin(days$central)), NA_real_)
      for (cl in c("lo50", "hi50", "lo95", "hi95"))
        out[[cl]] <- if (bands) ifelse(keep, sumg(fin(days[[cl]])), NA_real_) else NA_real_
    }
    out[n_days == 7L, , drop = FALSE]
  }
  day_frame <- function(x) data.frame(date = x$date, has_model = is.finite(x$predicted_central),
                                      scored = is.finite(x$predicted_central) & is.finite(x$observed),
                                      observed = x$observed, central = x$predicted_central,
                                      lo50 = x$lo50, hi50 = x$hi50, lo95 = x$lo95, hi95 = x$hi95)
  t_rows <- function(d) match(d, ens_dates)

  rows <- list()
  for (loc in locs) for (ch in c("cases", "deaths")) {
    x <- pt[pt$location == loc & pt$metric == ch, , drop = FALSE]
    if (!nrow(x)) next
    days <- day_frame(x)
    eng <- obs <- NULL
    if (arrays && !is.na(l_index[match(loc, locs)])) {
      tr <- t_rows(days$date)
      if (!anyNA(tr)) {
        l <- l_index[match(loc, locs)]
        eng <- flat(.mosaic_engine_array(ensemble, ch), l)[tr, , drop = FALSE]
        obs <- flat(ensemble[[paste0(ch, "_array")]], l)[tr, , drop = FALSE]
      }
    }
    s <- series(days, week_of(days$date, off[[loc]]), eng, obs, cm[[ch]])
    if (nrow(s)) rows[[length(rows) + 1L]] <- cbind(location = loc, metric = ch, s, stringsAsFactors = FALSE)
  }

  all_id <- all_off <- NULL
  if (length(locs) > 1L) {
    all_id <- "ALL"
    while (all_id %in% locs) all_id <- paste0(all_id, "_")
    all_off <- if (length(unique(off)) == 1L) unname(off[1]) else 0L
    for (ch in c("cases", "deaths")) {
      x <- pt[pt$metric == ch, , drop = FALSE]
      dates <- sort(unique(x$date))
      # a day enters only when every location has it
      n_loc_day <- tabulate(match(x$date, dates), nbins = length(dates))
      dates <- dates[n_loc_day == length(locs)]
      x <- x[x$date %in% dates, , drop = FALSE]
      if (!length(dates)) next
      di <- match(x$date, dates)
      agg_all <- function(v) as.vector(tapply(v, factor(di, levels = seq_along(dates)), all))
      agg_sum <- function(v) as.vector(rowsum(as.numeric(v), di, reorder = TRUE))
      has_model <- agg_all(is.finite(x$predicted_central))
      sc <- agg_all(is.finite(x$predicted_central) & is.finite(x$observed))
      days <- data.frame(date = dates, has_model = has_model, scored = sc,
                         observed = agg_sum(ifelse(is.finite(x$observed), x$observed, 0)),
                         central = ifelse(has_model, agg_sum(ifelse(is.finite(x$predicted_central),
                                                                    x$predicted_central, 0)), NA_real_),
                         lo50 = NA_real_, hi50 = NA_real_, lo95 = NA_real_, hi95 = NA_real_)
      eng <- obs <- NULL
      if (arrays && !anyNA(l_index)) {
        tr <- t_rows(dates)
        if (!anyNA(tr)) {
          for (l in l_index) {
            fe <- flat(.mosaic_engine_array(ensemble, ch), l)[tr, , drop = FALSE]
            fo <- flat(ensemble[[paste0(ch, "_array")]], l)[tr, , drop = FALSE]
            eng <- if (is.null(eng)) fe else eng + fe
            obs <- if (is.null(obs)) fo else obs + fo
          }
        }
      }
      s <- series(days, week_of(dates, all_off), eng, obs, cm[[ch]], bands = FALSE)
      if (nrow(s)) rows[[length(rows) + 1L]] <- cbind(location = all_id, metric = ch, s, stringsAsFactors = FALSE)
    }
  }
  wk <- if (length(rows)) do.call(rbind, rows) else empty
  wk <- wk[order(wk$location, wk$metric, wk$week), names(empty), drop = FALSE]
  wk$week <- format(as.Date(wk$week), "%Y-%m-%d")
  rownames(wk) <- NULL
  offsets <- as.list(off)
  if (arrays) {
    intervals <- vapply(c("cases", "deaths"), function(ch)
      if (isTRUE(ensemble$observation_model[[ch]])) "observation" else "engine", character(1))
    list(weekly = wk, meta = meta("trajectories", cm, intervals, offsets, all_id, all_off))
  } else list(weekly = wk, meta = meta("daily_sums", cm, c(cases = NA, deaths = NA), offsets, all_id, all_off))
}

#' Round the non-integer numbers of a JSON-bound object to significant digits
#'
#' Recurses through lists and data frames; whole numbers (counts, draw
#' indices) and non-numeric values are left exactly as they are.
#' @param x A list, data frame or vector.
#' @param digits Significant digits. Default 4.
#' @return \code{x}, rounded.
#' @noRd
.mosaic_signif_json <- function(x, digits = 4L) {
  if (is.data.frame(x)) {
    x[] <- lapply(x, .mosaic_signif_json, digits = digits)
    return(x)
  }
  if (is.list(x)) {
    cls <- class(x)
    out <- lapply(x, .mosaic_signif_json, digits = digits)
    attributes(out) <- attributes(x)
    class(out) <- cls
    return(out)
  }
  if (is.double(x) && !inherits(x, c("Date", "POSIXt"))) {
    whole <- !is.finite(x) | x == round(x)
    x[!whole] <- signif(x[!whole], digits)
  }
  x
}

#' The summary.json / calibration_diagnostics.json fields added in red-team round 1
#'
#' \code{n_locations} and \code{alpha_per_location} (\code{coarsening_alpha /
#' n_locations}: eta pools the locations, D2) are run-level; the convergence
#' fields \code{ess_untempered_frac}, \code{sims_projected_for_green} and
#' \code{projection_reliable} come from \code{.mosaic_weight_health_fields()}
#' and are filled with \code{NA} (JSON null) when the status does not carry
#' them, so the schema is complete for every run.
#' @param f \code{.mosaic_weight_health_fields()} result.
#' @param wh The \code{calc_weight_health()} result (may be \code{NULL}).
#' @param n_locations Locations in the run.
#' @param alpha \code{coarsening_alpha}.
#' @return \code{f} with the round-1 fields.
#' @noRd
.mosaic_schema2_round1_fields <- function(f, wh, n_locations, alpha) {
  f$n_locations <- as.integer(n_locations)
  a <- suppressWarnings(as.numeric(f$coarsening_alpha %||% alpha))
  if (length(a) != 1L || !is.finite(a)) a <- suppressWarnings(as.numeric(alpha))
  f$alpha_per_location <- if (length(a) == 1L && is.finite(a) && n_locations > 0) a / n_locations else NA_real_
  for (nm in c("ess_untempered_frac", "sims_projected_for_green"))
    if (is.null(f[[nm]])) f[[nm]] <- NA_real_
  if (is.null(f$projection_reliable)) f$projection_reliable <- NA
  f
}

#' Each convergence flag with its own text
#' @param conv The \code{calc_convergence_status()} result (or NULL).
#' @return Data frame \code{id, text}; zero rows when there are no flags or
#'   their texts are not parallel to them (the 1.5.1 completion then
#'   regenerates them, \code{.mosaic_calrep_complete()}).
#' @noRd
.mosaic_flags_detail <- function(conv) {
  ids <- as.character(unlist(conv$flags) %||% character(0))
  txt <- as.character(unlist(conv$flag_text) %||% character(0))
  if (!length(ids) || length(txt) != length(ids))
    return(data.frame(id = character(0), text = character(0), stringsAsFactors = FALSE))
  data.frame(id = ids, text = txt, stringsAsFactors = FALSE)
}

#' Assemble calibration_diagnostics.json (schema 2)
#'
#' Every number the calibration-diagnostics report and the dashboard show,
#' computed once (claude/plan_convergence/SCHEMA_v2.md section 2): run
#' description, the convergence status and its four checks, the sampling
#' quantities with the ESS and best-fit paths (at most 60 points), per-parameter
#' Monte Carlo error and prior-to-posterior KL with the prior family, the fit
#' tables on scored tier-1/2 days, and the eta = 1 likelihood concentration.
#' Written by \code{run_MOSAIC()} whatever \code{plots} is.
#'
#' @param conv_health The \code{calc_weight_health()} result describing the
#'   members the posterior uses (NULL when no posterior was built).
#' @param state Calibration state (\code{mode}).
#' @param config,control The run's config and merged control.
#' @param n_obs_scored Scored observation cells.
#' @param ess_replay \code{.mosaic_ess_replay()} result.
#' @param quantiles_file posterior_quantiles.csv of the run.
#' @param prediction_table The ensemble prediction table, or NULL.
#' @param is_diag_all eta = 1 diagnostics over all draws (\code{calc_is_diagnostics()}).
#' @param ensemble The canonical \code{mosaic_ensemble} (its member arrays give
#'   \code{fit.weekly}, \code{.mosaic_calibration_weekly()}), or NULL.
#' @param central_method The run's per-channel central method, or NULL.
#' @param glossary \code{.mosaic_glossary()} result (the display fields of
#'   \code{.mosaic_calrep_complete()}).
#' @return A list ready for JSON.
#' @noRd
.mosaic_calibration_diagnostics <- function(conv_health, state, config, control, n_obs_scored,
                                            ess_replay, quantiles_file, prediction_table = NULL,
                                            is_diag_all = NULL, ensemble = NULL, central_method = NULL,
                                            glossary = .mosaic_glossary()) {
  f <- .mosaic_weight_health_fields(conv_health)
  num <- function(x) { x <- suppressWarnings(as.numeric(unlist(x))); if (length(x) == 1L) x else NA_real_ }
  thin <- function(df, n = 60L) if (nrow(df) > n) df[unique(round(seq(1, nrow(df), length.out = n))), , drop = FALSE] else df
  conv <- conv_health$convergence
  locs <- as.character(config$location_name)

  run <- list(
    mosaic_version = as.character(utils::packageVersion("MOSAIC")),
    locations = I(locs),
    n_locations = length(locs),
    mode = state$mode %||% NA_character_,
    posterior_weighting = control$targets$posterior_weighting,
    coarsening_alpha = num(control$targets$coarsening_alpha),
    eta = num(control$targets$coarsening_alpha / n_obs_scored),
    n_obs_scored = as.integer(n_obs_scored),
    # simulations attempted (valid or not): the report header reads "1 valid of 40 simulated"
    n_simulations = as.integer(state$total_sims_run %||% NA_integer_),
    burn_in_days = as.integer(control$likelihood$burn_in_days %||% 0L),
    n_iterations = as.integer(control$calibration$n_iterations),
    n_iter_ensemble = as.integer(control$predictions$n_iter_ensemble %||% NA_integer_),
    date_start = as.character(config$date_start),
    date_stop = as.character(config$date_stop),
    # 1.5.1: the budget and member bounds the report's info rows compare against
    n_simulations_requested = num(control$calibration$n_simulations),
    max_simulations_total = num(control$calibration$max_simulations_total),
    min_ensemble_members = num(control$targets$min_ensemble_members),
    max_ensemble_members = num(control$targets$max_ensemble_members),
    central_method = as.list(.mosaic_resolve_central_method(central_method %||% control$predictions$central_method)))

  checks <- if (!is.null(conv$checks)) {
    ck <- conv$checks
    data.frame(check = ck$check, label = ck$label, value = ck$value, threshold_green = ck$threshold_green,
               threshold_yellow = ck$threshold_yellow, colour = ck$colour, reason = ck$reason,
               stringsAsFactors = FALSE)
  } else data.frame(check = character(0), label = character(0), value = numeric(0), threshold_green = numeric(0),
                    threshold_yellow = numeric(0), colour = character(0), reason = character(0))
  status <- list(convergence_status = f$convergence_status, convergence_label = f$convergence_label,
                 reasons = I(as.character(unlist(conv$reasons) %||% character(0))),
                 flags = I(as.character(unlist(conv$flags) %||% character(0))),
                 flags_text = f$convergence_flags_text,
                 # 1.5.1: each flag with its own text (flags_text joins them)
                 flags_detail = .mosaic_flags_detail(conv),
                 stopping_rule_met = if (identical(state$mode, "fixed")) NA else isTRUE(state$converged),
                 checks = checks,
                 thresholds = .mosaic_convergence_thresholds(control$targets$status_thresholds),
                 thresholds_source = "run control")

  rp <- if (is.null(ess_replay)) data.frame(n = integer(0), ess_all = numeric(0), best_gap = numeric(0),
                                             top100_gap = numeric(0)) else thin(ess_replay)
  f <- .mosaic_schema2_round1_fields(f, conv_health, length(locs), control$targets$coarsening_alpha)
  sampling <- c(f[c("n_locations", "alpha_per_location", "ess_untempered_frac", "sims_projected_for_green",
                    "projection_reliable", "ess_all", "ess_all_target", "ess_best", "ess_best_target",
                    "best_subset_ratio", "best_subset_mass", "ensemble_members", "ensemble_members_unique",
                    "ensemble_binding", "khat_tempered", "khat_status", "eta_star", "eta_star_n_eff",
                    "flat_likelihood", "ess_growth_slope", "sims_used", "sims_projected_for_target",
                    "budget_exhausted")],
                list(ess_path = rp[, c("n", "ess_all"), drop = FALSE],
                     loglik_path = rp[, c("n", "best_gap", "top100_gap"), drop = FALSE]))

  # Per parameter: the sampled parameters with a Monte Carlo error, their
  # prior family and their prior-to-posterior KL (posterior_quantiles.csv).
  mce <- conv_health$mce$per_parameter
  mce <- if (is.null(mce)) numeric(0) else unlist(mce)
  q <- if (!is.null(quantiles_file) && file.exists(quantiles_file))
    utils::read.csv(quantiles_file, stringsAsFactors = FALSE) else NULL
  q <- if (!is.null(q) && "type" %in% names(q)) q[q$type == "posterior", , drop = FALSE] else q
  pnames <- names(mce) %||% character(0)
  qi <- if (!is.null(q)) match(pnames, q$parameter) else rep(NA_integer_, length(pnames))
  loc <- if (!is.null(q) && "location" %in% names(q)) as.character(q$location[qi]) else rep(NA_character_, length(pnames))
  loc[is.na(loc) | !nzchar(loc)] <- NA_character_
  base <- ifelse(is.na(loc), pnames, sub(paste0("_", "(", paste(locs, collapse = "|"), ")$"), "", pnames))
  kl <- if (!is.null(q) && "kl" %in% names(q)) suppressWarnings(as.numeric(q$kl[qi])) else rep(NA_real_, length(pnames))
  band_edges <- c(0.05, 0.5)
  # Bands are suppressed below an ensemble ESS of about 100, where the null
  # PIT-KL bias would put prior-only parameters in "partly informed" (T12).
  band <- .mosaic_kl_band(kl, ess = f$ess_best, edges = band_edges)
  params <- data.frame(parameter = pnames, base = base, location = loc,
                       prior_family = if (!is.null(q) && "prior_distribution" %in% names(q))
                                        as.character(q$prior_distribution[qi]) else rep(NA_character_, length(pnames)),
                       mce = as.numeric(mce), kl = kl, kl_band = band, stringsAsFactors = FALSE)
  o <- order(-ifelse(is.na(params$mce), -Inf, params$mce))
  worst <- params[o[seq_len(min(5L, sum(!is.na(params$mce))))], c("parameter", "mce"), drop = FALSE]
  rownames(params) <- rownames(worst) <- NULL

  fit <- tryCatch(.mosaic_calibration_fit(prediction_table, config, ensemble = ensemble,
                                          central_method = run$central_method),
                  error = function(e) .mosaic_calibration_fit(NULL, config))

  out <- list(
    schema_version = "2",
    run = run,
    status = status,
    sampling = sampling,
    parameters = params,
    mce_summary = list(median = f$mce_median, p90 = f$mce_p90, max = f$mce_max, worst = worst),
    kl_summary = list(largely_prior = sum(band %in% "largely prior"),
                      partly_informed = sum(band %in% "partly informed"),
                      data_informed = sum(band %in% "data-informed"),
                      # KL computed but its band withheld (ensemble ESS < 100, T12 / RN3)
                      withheld = sum(is.finite(kl) & is.na(band)),
                      band_edges = band_edges),
    fit = fit,
    likelihood_concentration = list(ess_is_all = num(is_diag_all$ess_is),
                                    note = .mosaic_likelihood_concentration_note()))
  .mosaic_calrep_complete(out, glossary)
}

#' Write calibration_diagnostics.json atomically (NA and Inf as JSON null)
#'
#' The bulky arrays (\code{fit.weekly}, \code{fit.per_location},
#' \code{sampling.ess_path}, \code{sampling.loglik_path}) are rounded to four
#' significant digits (\code{.mosaic_signif_json()}); every scalar in
#' \code{status}, \code{sampling}, \code{mce_summary}, \code{parameters} and
#' \code{fit.table} keeps full precision, so the report and summary.json show
#' the same number (RN2). Size: claude/redteam/swe_r1/p14_size_40loc.R.
#' @param obj \code{.mosaic_calibration_diagnostics()} result.
#' @param path Destination.
#' @return \code{path}, invisibly.
#' @noRd
.mosaic_write_calibration_diagnostics <- function(obj, path) {
  # Only the bulky arrays are rounded; every scalar the report, the log and
  # summary.json quote keeps full precision, so one number is shown everywhere
  # (red team round 2, RN2 / CN3).
  for (k in c("weekly", "per_location"))
    if (!is.null(obj$fit[[k]])) obj$fit[[k]] <- .mosaic_signif_json(obj$fit[[k]], 4L)
  for (k in c("ess_path", "loglik_path"))
    if (!is.null(obj$sampling[[k]])) obj$sampling[[k]] <- .mosaic_signif_json(obj$sampling[[k]], 4L)
  .mosaic_atomic_write(obj, path, function(data, file)
    jsonlite::write_json(data, file, pretty = TRUE, auto_unbox = TRUE, digits = NA,
                         na = "null", null = "null", dataframe = "rows"))
  invisible(path)
}
