# =============================================================================
# MOSAIC: run_MOSAIC_dask.R
# Simulation-level Dask parallelism for MOSAIC BFRS calibration
# =============================================================================
# Mirrors run_MOSAIC() exactly except the parallel cluster section (R's
# parallel::makeCluster) is replaced by a Dask cluster (Coiled.io or a
# user-supplied scheduler). LASER simulations run as pure-Python Dask tasks
# via inst/python/mosaic_dask_worker.py — no R subprocess on workers.
#
# Architecture:
#   R orchestrator                        Dask workers (Python)
#   ─────────────────────                 ──────────────────────
#   sample_parameters() × batch_size  →  run_laser_sim(sim_id, params_json,
#   serialize sampled params to JSON         base_config)
#   client$map() / client$submit()     ←  returns {iterations: [{cases,deaths}]}
#   calc_model_likelihood() in R
#   write sim_XXXXXXX.parquet
#   [all post-processing identical to run_MOSAIC()]
# =============================================================================


# =============================================================================
# PRIVATE HELPERS
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
      est_cases  <- matrix(unlist(iter_res$expected_cases),
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
          add_max_terms         = likelihood_settings$add_max_terms,
          add_peak_timing       = likelihood_settings$add_peak_timing,
          add_peak_magnitude    = likelihood_settings$add_peak_magnitude,
          add_cumulative_total  = likelihood_settings$add_cumulative_total,
          add_wis               = likelihood_settings$add_wis,
          weight_cases          = likelihood_settings$weight_cases,
          weight_deaths         = likelihood_settings$weight_deaths,
          weight_max_terms      = likelihood_settings$weight_max_terms,
          weight_peak_timing    = likelihood_settings$weight_peak_timing,
          weight_peak_magnitude = likelihood_settings$weight_peak_magnitude,
          weight_cumulative_total = likelihood_settings$weight_cumulative_total,
          weight_wis            = likelihood_settings$weight_wis,
          sigma_peak_time       = likelihood_settings$sigma_peak_time,
          sigma_peak_log        = likelihood_settings$sigma_peak_log,
          penalty_unmatched_peak = likelihood_settings$penalty_unmatched_peak,
          enable_guardrails     = likelihood_settings$enable_guardrails,
          floor_likelihood      = likelihood_settings$floor_likelihood,
          guardrail_verbose     = likelihood_settings$guardrail_verbose
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
        est_cases  <- matrix(unlist(iter_res$expected_cases),
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
# PUBLIC API
# =============================================================================

#' Run MOSAIC Calibration Workflow with Dask Cluster
#'
#' @description
#' **Simulation-level distributed calibration using Dask (Coiled.io or local scheduler).**
#'
#' Mirrors \code{\link{run_MOSAIC}} exactly except the R \code{parallel} cluster
#' is replaced by a Dask cluster. Each LASER simulation runs as a pure-Python
#' Dask task — no R subprocess on workers. Workers call
#' \code{laser.cholera.metapop.model.run_model()} directly, return case/death
#' arrays, and likelihood computation happens in R after gathering.
#'
#' This enables scaling across Azure/cloud nodes (via Coiled.io) without the
#' country-level coupling constraint that invalidates parallelism in multi-country
#' runs: coupling between countries happens \emph{inside} each LASER simulation,
#' so parallelism must be at the simulation level.
#'
#' @param config Named list of LASER model configuration (REQUIRED). Same as
#'   \code{\link{run_MOSAIC}}.
#' @param priors Named list of prior distributions (REQUIRED). Same as
#'   \code{\link{run_MOSAIC}}.
#' @param dir_output Character. Output directory for this calibration run (REQUIRED).
#' @param control Control list from \code{\link{mosaic_control_defaults}}. If
#'   \code{NULL}, uses defaults.
#' @param dask_spec Named list specifying the Dask cluster. Fields:
#'   \describe{
#'     \item{type}{Character. \code{"coiled"} (default) or \code{"scheduler"}.}
#'     \item{n_workers}{Integer. Number of Dask workers. Defaults to
#'       \code{control$parallel$n_cores}.}
#'     \item{software}{Character. Coiled software environment name.
#'       Default \code{"mosaic-acr-workers"} (pulls from Azure Container Registry).}
#'     \item{vm_types}{Character vector. Azure VM type(s) for workers.
#'       Default \code{"Standard_D8s_v6"}.}
#'     \item{scheduler_vm_types}{Character vector. Azure VM type(s) for the
#'       scheduler. Should have enough RAM to coordinate all workers and hold
#'       gathered results. Default \code{"Standard_D4s_v6"} (4 vCPU, 16 GB).}
#'     \item{region}{Character. Azure region. Default \code{"westus2"}.}
#'     \item{idle_timeout}{Character. Coiled idle shutdown timeout.
#'       Default \code{"2 hours"}.}
#'     \item{address}{Character. Scheduler address for \code{type="scheduler"},
#'       e.g. \code{"tcp://localhost:8786"}.}
#'   }
#'   If \code{NULL}, defaults to \code{list(type="coiled")} with all sub-fields
#'   at their defaults.
#' @return Invisibly returns same list as \code{\link{run_MOSAIC}}:
#'   \code{dirs}, \code{files}, \code{summary}.
#'
#' @section Output storage (TODO):
#' Currently uses Option A: worker returns \code{(expected_cases, disease_deaths)}
#' arrays in-memory via \code{client$gather()}. R computes likelihoods and writes
#' parquets. Appropriate for ≤1000 sims per batch.
#'
#' TODO (Option B — scale-up for >1000 sims per batch):
#' Have workers write directly to Azure Blob Storage using the
#' azure-storage-blob Python SDK:
#'   \code{blob_client.upload_blob(f"sims/sim_{sim_id:07d}.parquet", data)}
#' R orchestrator then reads back via \code{AzureStor::storage_download_blob()}.
#' This avoids scheduler memory bottleneck and is recommended for production runs
#' with thousands of simulations per batch. Requires \code{AZURE_STORAGE_CONNECTION_STRING}
#' to be set on workers (pass via Coiled \code{environ} or worker plugin).
#'
#' @seealso \code{\link{run_MOSAIC}} for the single-node parallel version.
#'   \code{\link{mosaic_control_defaults}} for building the control structure.
#' @export
run_MOSAIC_dask <- function(config,
                             priors,
                             dir_output,
                             control   = NULL,
                             dask_spec = NULL) {

  # ===========================================================================
  # ARGUMENT VALIDATION
  # ===========================================================================

  stopifnot(
    "config is required and must be a list" =
      !missing(config) && is.list(config) && length(config) > 0,
    "priors is required and must be a list" =
      !missing(priors) && is.list(priors) && length(priors) > 0,
    "dir_output is required and must be character string" =
      !missing(dir_output) && is.character(dir_output) && length(dir_output) == 1L
  )

  if (!"location_name" %in% names(config)) {
    stop("config must contain 'location_name' field", call. = FALSE)
  }

  # Normalise dask_spec defaults
  if (is.null(dask_spec)) dask_spec <- list()
  dask_spec$type         <- dask_spec$type         %||% "coiled"
  dask_spec$software     <- dask_spec$software     %||% "mosaic-acr-workers"
  dask_spec$vm_types              <- dask_spec$vm_types              %||% c("Standard_D8s_v6")
  dask_spec$scheduler_vm_types    <- dask_spec$scheduler_vm_types    %||% c("Standard_D4s_v6")
  dask_spec$region                <- dask_spec$region                %||% "westus2"
  dask_spec$idle_timeout          <- dask_spec$idle_timeout          %||% "2 hours"

  if (!dask_spec$type %in% c("coiled", "scheduler")) {
    stop("dask_spec$type must be 'coiled' or 'scheduler'", call. = FALSE)
  }
  if (dask_spec$type == "scheduler" &&
      (is.null(dask_spec$address) || !nzchar(dask_spec$address))) {
    stop("dask_spec$address must be set when type='scheduler'", call. = FALSE)
  }

  iso_code <- config$location_name

  # ===========================================================================
  # ROOT DIRECTORY
  # ===========================================================================

  root_dir <- getOption("root_directory")
  if (is.null(root_dir)) {
    stop(
      "MOSAIC root directory not set. Please set once per session:\n",
      "  set_root_directory('/path/to/MOSAIC')\n",
      call. = FALSE
    )
  }
  if (!dir.exists(root_dir)) {
    stop("MOSAIC root directory does not exist: ", root_dir, call. = FALSE)
  }
  for (d in c("MOSAIC-pkg", "MOSAIC-data")) {
    if (!dir.exists(file.path(root_dir, d))) {
      warning("Expected directory not found: ", file.path(root_dir, d),
              call. = FALSE)
    }
  }

  # ===========================================================================
  # CONTROL DEFAULTS
  # ===========================================================================

  log_msg("Using user-provided config for: %s", paste(iso_code, collapse = ", "))
  .mosaic_validate_config(config, iso_code)
  log_msg("Using user-provided priors")
  .mosaic_validate_priors(priors, config)

  if (is.null(control)) control <- mosaic_control_defaults()
  control <- .mosaic_validate_and_merge_control(control)

  n_iterations <- control$calibration$n_iterations
  n_simulations <- control$calibration$n_simulations
  sampling_args <- control$sampling

  if (!is.numeric(n_iterations) || n_iterations < 1) {
    stop("control$calibration$n_iterations must be a positive integer", call. = FALSE)
  }
  sampling_args <- .mosaic_validate_sampling_args(sampling_args)

  # ===========================================================================
  # PYTHON ENV CHECK
  # ===========================================================================

  check_python_env()
  Sys.setenv(PYTHONWARNINGS = "ignore::UserWarning")
  .mosaic_check_blas_control()

  # ===========================================================================
  # PATHS AND DIRECTORIES
  # ===========================================================================

  log_msg("Starting MOSAIC Dask calibration")
  set_root_directory(root_dir)
  PATHS <- get_paths()

  dirs <- .mosaic_ensure_dir_tree(
    dir_output   = dir_output,
    clean_output = isTRUE(control$paths$clean_output)
  )

  # Conditionally create simresults directory for validation output
  save_simresults <- isTRUE(control$io$save_simresults)
  if (save_simresults) {
    dirs$cal_simresults <- file.path(dir_output, "2_calibration/simulation_results")
    dir.create(dirs$cal_simresults, recursive = TRUE, showWarnings = FALSE)
  }

  # ===========================================================================
  # SETUP FILES
  # ===========================================================================

  # Capture full environment snapshot (versions, system, git, data)
  env_snapshot <- .mosaic_capture_environment(
    config = config, priors = priors, control = control
  )
  .mosaic_write_json(env_snapshot, file.path(dirs$inputs, "environment.json"), control$io)
  log_msg("  Saved %s", "1_inputs/environment.json")

  log_msg("Writing setup files...")
  .mosaic_write_json(control, file.path(dirs$inputs, "control.json"), control$io)
  .mosaic_write_json(priors,  file.path(dirs$inputs, "priors.json"),  control$io)
  .mosaic_write_json(config,  file.path(dirs$inputs, "config.json"),  control$io)
  log_msg("  Saved control.json, priors.json, config.json")

  if (isTRUE(control$paths$plots)) {
    log_msg("Plotting prior distributions")
    plot_model_distributions(
      json_files   = file.path(dirs$inputs, "priors.json"),
      method_names = "Prior",
      output_dir   = dirs$inputs,
      verbose      = control$logging$verbose
    )
  }

  # ===========================================================================
  # PARAMETER NAME DETECTION
  # ===========================================================================

  tmp <- convert_config_to_matrix(config)
  if ("seed" %in% names(tmp)) tmp <- tmp[names(tmp) != "seed"]
  param_names_all <- names(tmp)
  rm(tmp)

  est_params_df <- get("estimated_parameters", envir = asNamespace("MOSAIC"))
  if (!is.data.frame(est_params_df) || nrow(est_params_df) == 0) {
    stop("Failed to load MOSAIC::estimated_parameters")
  }
  base_params <- unique(gsub("_[A-Z]{3}$", "", param_names_all))
  valid_base_params <- base_params[base_params %in% est_params_df$parameter_name]
  param_names_estimated <- param_names_all[
    gsub("_[A-Z]{3}$", "", param_names_all) %in% valid_base_params
  ]
  param_names_estimated <- param_names_estimated[
    !grepl("^[NSEIRV][12]?_j_initial", param_names_estimated)
  ]
  if (!length(param_names_estimated)) stop("No estimated parameters found for ESS tracking")

  # Derive disabled parameter names from sampling_args flags so the log
  # message reflects the actual number being sampled in this run.
  # Flags use "sample_<name>" convention; strip the prefix to get the config
  # key. Handle the special-case name mappings used in sample_parameters.R.
  disabled_base_params <- character(0)
  if (!is.null(sampling_args)) {
    disabled_flags <- names(sampling_args)[vapply(sampling_args, isFALSE, logical(1))]
    disabled_flags <- disabled_flags[startsWith(disabled_flags, "sample_")]
    disabled_base <- gsub("^sample_", "", disabled_flags)
    special_map <- list(
      beta_j0_tot = c("beta_j0_tot", "beta_j0_hum", "beta_j0_env"),
      initial_conditions = c("prop_S_initial", "prop_E_initial", "prop_I_initial",
                             "prop_R_initial", "prop_V1_initial", "prop_V2_initial")
    )
    resolved <- unlist(lapply(disabled_base, function(nm) {
      if (nm %in% names(special_map)) special_map[[nm]] else nm
    }))
    disabled_base_params <- unique(resolved)
  }

  param_names_sampled <- param_names_estimated[
    !(gsub("_[A-Z]{3}$", "", param_names_estimated) %in% disabled_base_params)
  ]

  log_msg("Parameters: %d sampled (of %d estimable, %d total) | Locations: %s",
          length(param_names_sampled), length(param_names_estimated),
          length(param_names_all),
          paste(config$location_name, collapse = ", "))

  # Resolve likelihood settings once (avoids closure over control in batch func)
  likelihood_settings <- control$likelihood
  if (!is.null(likelihood_settings$weights_time)) {
    n_t <- ncol(config$reported_cases)
    wt_raw <- likelihood_settings$weights_time
    if (length(wt_raw) == 1L) wt_raw <- rep(wt_raw, n_t)
    likelihood_settings$.weights_time_resolved <- wt_raw / sum(wt_raw) * n_t
  } else {
    likelihood_settings$.weights_time_resolved <- NULL
  }

  # ===========================================================================
  # DASK CLUSTER SETUP
  # ===========================================================================

  dask_dist <- reticulate::import("dask.distributed")
  cluster   <- NULL

  if (dask_spec$type == "coiled") {
    coiled_mod <- reticulate::import("coiled")
    n_workers_req <- dask_spec$n_workers %||% control$parallel$n_cores

    log_msg("Creating Coiled cluster: %d workers (%s, %s)",
            n_workers_req, dask_spec$vm_types[1], dask_spec$region)

    cluster_name <- paste0("mosaic-dask-",
                           format(Sys.time(), "%Y%m%d-%H%M%S"))

    # Build Coiled cluster args.
    # Tested options for large images (>10 GB) that may hit pull timeouts:
    #   timeout              = 1500L        # client-side wait (seconds)
    #   environ              = list(COILED_STARTUP_TIMEOUT = "900")
    #   scheduler_disk_size  = "120 GiB"    # more disk for image extraction
    #   worker_disk_size     = "120 GiB"
    #   scheduler_options    = list(idle_timeout = "30 minutes")
    #   spot_policy          = "spot_with_fallback"
    cluster_args <- list(
      name                = cluster_name,
      n_workers           = as.integer(n_workers_req),
      worker_vm_types     = as.list(dask_spec$vm_types),
      scheduler_vm_types  = as.list(dask_spec$scheduler_vm_types),
      region              = dask_spec$region,
      software            = dask_spec$software,
      workspace           = dask_spec$workspace,
      shutdown_on_close   = TRUE,
      idle_timeout        = dask_spec$idle_timeout
    )

    # Pass through optional Coiled args from dask_spec
    for (opt in c("timeout", "environ", "scheduler_disk_size",
                  "worker_disk_size", "scheduler_options", "worker_options",
                  "spot_policy", "host_setup_script")) {
      if (!is.null(dask_spec[[opt]])) {
        cluster_args[[opt]] <- dask_spec[[opt]]
        if (opt == "host_setup_script") log_msg("Using host_setup_script for worker VM init")
      }
    }

    cluster <- do.call(coiled_mod$Cluster, cluster_args)
    client <- dask_dist$Client(cluster)
  } else {
    log_msg("Connecting to Dask scheduler: %s", dask_spec$address)
    client <- dask_dist$Client(dask_spec$address)
  }

  log_msg("Dask dashboard: %s", client$dashboard_link)

  # Register cleanup (runs on normal exit OR error).
  # client/cluster are set to NULL after graceful close in post-processing,
  # so this only fires if an error occurs during the batch loop.
  on.exit({
    if (!is.null(client)) {
      try({ client$close(); log_msg("Dask client closed (on.exit)") }, silent = TRUE)
    }
    if (!is.null(cluster)) {
      try({ cluster$close(); log_msg("Dask cluster closed (on.exit)") }, silent = TRUE)
    }
  }, add = TRUE)

  # Upload Python worker module to all workers
  worker_py_path <- system.file("python/mosaic_dask_worker.py", package = "MOSAIC")
  if (!nzchar(worker_py_path) || !file.exists(worker_py_path)) {
    stop("Cannot find inst/python/mosaic_dask_worker.py — was the package reinstalled?",
         call. = FALSE)
  }
  client$upload_file(worker_py_path)
  log_msg("Uploaded mosaic_dask_worker.py to all workers")

  # client$upload_file() adds the module to each worker's sys.path, but the
  # local Python process doesn't know about it yet. Add the module's directory
  # to local sys.path so reticulate::import() can find it here too.
  # Cloudpickle then serializes the function as "mosaic_dask_worker.run_laser_sim"
  # (module name + function name), which workers can deserialize via their own import.
  reticulate::py_run_string(paste0(
    "import sys\n",
    "_mw_dir = '", dirname(worker_py_path), "'\n",
    "if _mw_dir not in sys.path:\n",
    "    sys.path.insert(0, _mw_dir)"
  ))
  mosaic_worker <- reticulate::import("mosaic_dask_worker")

  # Scatter base config (matrices + metadata) to all workers ONCE
  log_msg("Broadcasting base config to workers...")
  base_config_py     <- reticulate::r_to_py(.extract_base_config(config))
  base_config_future <- client$scatter(base_config_py, broadcast = TRUE)
  log_msg("  Base config broadcast complete")

  # ===========================================================================
  # RUN STATE (same as run_MOSAIC)
  # ===========================================================================

  nspec      <- .mosaic_normalize_n_sims(n_simulations)
  state_file <- file.path(dirs$cal_state, "run_state.json")

  state <- .mosaic_init_state(control, param_names_sampled, nspec)

  log_msg("Starting simulation (mode: %s)", state$mode)
  start_time <- Sys.time()

  # ===========================================================================
  # FIXED MODE
  # ===========================================================================

  if (identical(state$mode, "fixed")) {
    target <- state$fixed_target
    log_msg("[FIXED MODE] Running exactly %d simulations", target)

    all_ids <- seq_len(target)

    if (length(all_ids) == 0L) {
      log_msg("Nothing to do")
    } else {
      # Sub-batch to avoid OOM: R holds params + futures + results per batch
      bsize <- control$calibration$batch_size %||% 10000L
      batch_chunks <- split(all_ids, ceiling(seq_along(all_ids) / bsize))
      n_chunks <- length(batch_chunks)
      log_msg("Running %d simulations in %d sub-batches of up to %d",
              length(all_ids), n_chunks, bsize)

      total_success <- 0L
      fixed_start_time <- Sys.time()

      for (chunk_i in seq_len(n_chunks)) {
        sim_ids <- batch_chunks[[chunk_i]]
        log_msg("Sub-batch %d/%d: sims %d-%d (%d sims)",
                chunk_i, n_chunks, min(sim_ids), max(sim_ids), length(sim_ids))
        batch_start_time <- Sys.time()

        success_indicators <- .mosaic_run_batch_dask(
          sim_ids            = sim_ids,
          n_iterations       = n_iterations,
          priors             = priors,
          config             = config,
          PATHS              = PATHS,
          sampling_args      = sampling_args,
          dirs               = dirs,
          param_names_all    = param_names_all,
          control            = control,
          likelihood_settings = likelihood_settings,
          client             = client,
          base_config_future = base_config_future,
          mosaic_worker      = mosaic_worker
        )

        n_success <- sum(unlist(success_indicators))
        total_success <- total_success + n_success
        state$total_sims_successful <- state$total_sims_successful + n_success
        state$batch_success_rates   <- c(state$batch_success_rates,
                                          (n_success / length(sim_ids)) * 100)
        state$batch_sizes_used  <- c(state$batch_sizes_used, length(sim_ids))
        state$batch_number      <- state$batch_number + 1L

        batch_runtime <- difftime(Sys.time(), batch_start_time, units = "mins")
        log_msg("Sub-batch %d/%d complete: %d/%d successful (%.1f%%) in %.1f minutes",
                chunk_i, n_chunks, n_success, length(sim_ids),
                tail(state$batch_success_rates, 1),
                as.numeric(batch_runtime))

        .mosaic_save_state(state, state_file)
        gc(verbose = FALSE)
      }

      state$total_sims_run <- length(all_ids)

      fixed_runtime <- difftime(Sys.time(), fixed_start_time, units = "mins")
      log_msg("Fixed mode complete: %d/%d successful (%.1f%%) in %.1f minutes",
              total_success, length(all_ids),
              (total_success / length(all_ids)) * 100,
              as.numeric(fixed_runtime))
    }

  # ===========================================================================
  # AUTO MODE (adaptive batching — identical phase logic to run_MOSAIC)
  # ===========================================================================

  } else {

    log_msg("Phase 1: Adaptive Calibration")
    log_msg("  - Run %d-%d batches × %d sims (R² target: %.2f)",
            control$calibration$min_batches, control$calibration$max_batches,
            control$calibration$batch_size,  control$calibration$target_r2)
    log_msg("Phase 2: Single Predictive Batch")
    log_msg("Phase 3: Adaptive Fine-tuning (5-tier)")
    log_msg("Target ESS: %d per parameter | Max simulations: %d",
            control$targets$ESS_param, control$calibration$max_simulations)

    repeat {
      if (state$converged ||
          state$total_sims_run >= control$calibration$max_simulations) break

      decision       <- .mosaic_decide_next_batch(state, control, state$ess_tracking)
      current_phase  <- decision$phase
      current_bsize  <- decision$batch_size

      if (!identical(state$phase, current_phase)) {
        old_phase <- state$phase
        state$phase <- current_phase
        state$phase_batch_count <- 0L
        state$phase_last <- current_phase
        log_msg("  Phase transition: %s -> %s", toupper(old_phase),
                toupper(state$phase))
      }

      if (current_bsize <= 0) {
        log_msg("No additional simulations needed (batch_size = 0)")
        break
      }

      state$phase_batch_count <- state$phase_batch_count + 1L

      batch_start <- state$total_sims_run + 1L
      batch_end   <- state$total_sims_run + current_bsize

      # Reserve sims for fine-tuning (mirrors run_MOSAIC.R lines 869-878)
      if (identical(current_phase, "predictive")) {
        reserved <- 250L
        max_pred  <- control$calibration$max_simulations - reserved
        if (batch_end > max_pred) {
          batch_end <- max_pred
          log_msg("Capping predictive batch to leave %d reserved", reserved)
        }
      }

      batch_end <- min(batch_end, control$calibration$max_simulations)
      sim_ids   <- batch_start:batch_end

      log_msg(paste(rep("-", 60), collapse = ""))
      log_msg("[%s] Batch %d: Running %d simulations (%d-%d)",
              toupper(current_phase), state$batch_number + 1L,
              length(sim_ids), min(sim_ids), max(sim_ids))

      batch_start_time <- Sys.time()

      success_indicators <- .mosaic_run_batch_dask(
        sim_ids            = sim_ids,
        n_iterations       = n_iterations,
        priors             = priors,
        config             = config,
        PATHS              = PATHS,
        sampling_args      = sampling_args,
        dirs               = dirs,
        param_names_all    = param_names_all,
        control            = control,
        likelihood_settings = likelihood_settings,
        client             = client,
        base_config_future = base_config_future,
        mosaic_worker      = mosaic_worker
      )

      n_success_batch  <- sum(unlist(success_indicators))
      batch_success_rate <- (n_success_batch / length(sim_ids)) * 100

      state$total_sims_successful <- state$total_sims_successful + n_success_batch
      state$batch_success_rates   <- c(state$batch_success_rates, batch_success_rate)
      state$batch_sizes_used  <- c(state$batch_sizes_used, length(sim_ids))
      state$batch_number      <- state$batch_number + 1L
      state$total_sims_run    <- batch_end

      batch_runtime <- difftime(Sys.time(), batch_start_time, units = "mins")
      log_msg("Batch %d complete: %d/%d successful (%.1f%%) in %.1f minutes",
              state$batch_number, n_success_batch, length(sim_ids),
              batch_success_rate, as.numeric(batch_runtime))

      # ESS convergence check — always run so state$converged is accurate.
      # The old guard skipped this when total_sims_run >= max_simulations,
      # which meant the final batch never updated the converged flag and
      # always emitted a spurious "no convergence" warning even when the
      # target was met on that last batch.
      state <- .mosaic_ess_check_update_state(state, dirs,
                                              param_names_sampled, control)
      .mosaic_save_state(state, state_file)

      if (state$total_sims_run >= control$calibration$max_simulations &&
          !state$converged) {
        log_msg("WARNING: Reached max simulations (%d) without convergence",
                control$calibration$max_simulations)
        break
      }
    }
  }

  # ===========================================================================
  # POST-PROCESSING — identical to run_MOSAIC() from here onward
  # ===========================================================================

  # Close Dask client/cluster — no longer needed, all results are gathered.
  # Closing now prevents TLS heartbeat timeout errors during heavy R-side
  # post-processing (parquet combining, ESS, weights) which blocks the
  # Python event loop and starves the Dask heartbeat.
  log_msg("Closing Dask cluster (all results gathered, no longer needed)")
  tryCatch({
    client$close()
    log_msg("  Dask client closed")
  }, silent = TRUE, error = function(e) {
    log_msg("  Dask client close failed (may already be disconnected): %s", e$message)
  })
  if (!is.null(cluster)) {
    tryCatch({
      cluster$close()
      log_msg("  Dask cluster closed")
    }, silent = TRUE, error = function(e) {
      log_msg("  Dask cluster close failed: %s", e$message)
    })
  }
  # Null out so the on.exit handler doesn't double-close
  client  <- NULL
  cluster <- NULL

  # Force GC before loading parquets — batch intermediates can hold several GB
  gc(verbose = FALSE)
  mem_mb <- sum(gc(verbose = FALSE)[, 2])
  log_msg("Memory after GC: %.0f MB (R heap)", mem_mb)

  post_start <- Sys.time()
  log_msg("Combining simulation files")

  parquet_files <- list.files(dirs$cal_samples,
                               pattern = "^sim_.*\\.parquet$",
                               full.names = TRUE)

  load_method <- if (!is.null(control$io$load_method)) {
    control$io$load_method
  } else {
    "streaming"
  }

  load_chunk_size <- control$io$load_chunk_size %||% 5000L

  results <- .mosaic_load_and_combine_results(
    dir_params = dirs$cal_samples,
    method     = load_method,
    chunk_size = load_chunk_size,
    verbose    = TRUE
  )

  log_msg("Adding index columns...")
  results$is_finite  <- is.finite(results$likelihood) & !is.na(results$likelihood)
  results$is_valid   <- results$is_finite & results$likelihood != -999999999
  results$is_outlier <- FALSE

  if (sum(results$is_valid) > 0) {
    valid_ll  <- results$likelihood[results$is_valid]
    q1 <- stats::quantile(valid_ll, 0.25, na.rm = TRUE)
    q3 <- stats::quantile(valid_ll, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    iqr_mult <- control$weights$iqr_multiplier
    lower_threshold <- q1 - iqr_mult * iqr

    # Only apply the lower fence. Log-likelihoods have a long left tail (many
    # poor fits) but are bounded above near 0. Applying an upper fence via
    # Q3 + k*IQR would incorrectly discard the highest-likelihood simulations
    # — exactly the models we want to keep for the posterior. The lower fence
    # removes pathologically bad simulations that survived the guardrails.
    results$is_outlier[results$is_valid] <- valid_ll < lower_threshold

    log_msg("  Outlier detection (Tukey lower fence only, multiplier = %.1f):", iqr_mult)
    log_msg("    - Lower threshold: %.1f | Outliers: %d (%.1f%%)",
            lower_threshold,
            sum(results$is_outlier),
            100 * sum(results$is_outlier) / sum(results$is_valid))
  }

  results$is_retained    <- results$is_valid & !results$is_outlier
  results$is_best_subset <- FALSE
  results$is_best_model  <- FALSE
  if (any(results$is_valid)) {
    results$is_best_model[which.max(results$likelihood)] <- TRUE
  }

  simulations_file <- file.path(dirs$calibration, "samples.parquet")
  .mosaic_write_parquet(results, simulations_file, control$io)

  if (length(parquet_files) > 0) {
    unlink(parquet_files)
    log_msg("  Cleaned up %d individual simulation files", length(parquet_files))
  }
  log_msg("  Combined simulations saved: %s", basename(simulations_file))
  log_msg("  Combine + write elapsed: %.1fs",
          as.numeric(difftime(Sys.time(), post_start, units = "secs")))

  # --- Parameter ESS ---
  log_msg("Calculating parameter ESS")
  ess_results <- calc_model_ess_parameter(
    results        = results,
    param_names    = param_names_sampled,
    likelihood_col = "likelihood",
    n_grid         = 100,
    method         = control$targets$ESS_method,
    verbose        = control$logging$verbose
  )
  ess_file <- file.path(dirs$cal_diag, "parameter_ess.csv")
  write.csv(ess_results, ess_file, row.names = FALSE)
  log_msg("Saved %s", ess_file)

  # --- Subset optimisation ---
  log_msg("Optimizing subset selection...")
  subset_tiers <- get_default_subset_tiers(
    target_ESS_best = control$targets$ESS_best,
    target_A        = control$targets$A_best,
    target_CVw      = control$targets$CVw_best
  )

  optimal_subset_result <- NULL
  tier_used <- NULL

  for (tier_name in names(subset_tiers)) {
    tier <- subset_tiers[[tier_name]]
    log_msg("  Testing tier '%s'...", tier$name)

    tier_result <- grid_search_best_subset(
      results    = results[results$is_retained, ],
      target_ESS = tier$ESS_B,
      target_A   = tier$A,
      target_CVw = tier$CVw,
      min_size   = control$targets$min_best_subset,
      max_size   = control$targets$max_best_subset,
      ess_method = control$targets$ESS_method,
      verbose    = control$logging$verbose
    )

    if (tier_result$converged) {
      tier_pct <- (tier_result$n / nrow(results)) * 100
      log_msg("    Tier '%s' converged at n=%d (%.1f%%)",
              tier$name, tier_result$n, tier_pct)
      optimal_subset_result <- tier_result
      tier_used <- tier$name
      rm(tier)
      break
    } else {
      rm(tier_result, tier)
    }
  }
  if (exists("subset_tiers")) rm(subset_tiers)
  gc(verbose = FALSE)

  if (!is.null(optimal_subset_result)) {
    top_subset_final  <- optimal_subset_result$subset
    n_top_final       <- optimal_subset_result$n
    percentile_used   <- (n_top_final / nrow(results)) * 100
    convergence_tier  <- tier_used
  } else {
    retained_for_fallback <- results[results$is_retained, ]
    n_top_final       <- min(control$targets$max_best_subset, nrow(retained_for_fallback))
    results_ranked    <- retained_for_fallback[order(retained_for_fallback$likelihood, decreasing = TRUE), ]
    top_subset_final  <- results_ranked[1:n_top_final, ]
    rm(results_ranked, retained_for_fallback)
    percentile_used   <- (n_top_final / nrow(results)) * 100
    convergence_tier  <- "fallback"
    log_msg("  All tiers failed — using fallback (top %d sims)", n_top_final)
    optimal_subset_result <- list(subset = top_subset_final, n = n_top_final,
                                  converged = FALSE,
                                  metrics = list(ESS = NA_real_, A = NA_real_,
                                                 CVw = NA_real_))
  }

  n_valid_final <- sum(is.finite(top_subset_final$likelihood))

  if (n_valid_final < 2) {
    log_msg("WARNING: Insufficient valid sims (%d) — setting metrics to NA",
            n_valid_final)
    ESS_B_final <- A_final <- CVw_final <- NA_real_
    gibbs_temperature_final <- 1
  } else {
    aic_final       <- -2 * top_subset_final$likelihood
    best_aic_final  <- min(aic_final[is.finite(aic_final)])

    if (!is.finite(best_aic_final)) {
      ESS_B_final <- A_final <- CVw_final <- NA_real_
      gibbs_temperature_final <- 1
    } else {
      delta_aic_trunc      <- pmin(aic_final - best_aic_final, 4.0)
      gibbs_temperature_final <- 0.5
      weights_final        <- calc_model_weights_gibbs(
        x           = delta_aic_trunc,
        temperature = gibbs_temperature_final,
        verbose     = FALSE
      )
      w_tilde_final  <- weights_final
      w_final        <- weights_final * length(weights_final)
      ESS_B_final    <- calc_model_ess(w_tilde_final,
                                        method = control$targets$ESS_method)
      ag_final       <- calc_model_agreement_index(w_final)
      A_final        <- ag_final$A
      CVw_final      <- calc_model_cvw(w_final)
    }
  }

  log_msg("Final subset (%s): %.1f%% (n=%d) | ESS_B=%s, A=%s, CVw=%s",
          convergence_tier, percentile_used, n_top_final,
          if (is.finite(ESS_B_final)) sprintf("%.1f", ESS_B_final) else "NA",
          if (is.finite(A_final))     sprintf("%.3f", A_final)     else "NA",
          if (is.finite(CVw_final))   sprintf("%.3f", CVw_final)   else "NA")

  final_converged <- convergence_tier != "fallback"

  results$is_best_subset <- FALSE
  results$is_best_subset[results$sim %in% top_subset_final$sim] <- TRUE

  # Weight calculation
  log_msg("Calculating weights for %d simulations...", nrow(results))
  results$weight_all      <- 0
  results$weight_retained <- 0
  results$weight_best     <- 0

  if (sum(results$is_valid) > 0) {
    all_result <- .mosaic_calc_adaptive_gibbs_weights(
      likelihood    = results$likelihood[results$is_valid],
      weight_floor  = control$weights$floor,
      verbose       = control$logging$verbose
    )
    results$weight_all[results$is_valid] <- all_result$weights
    log_msg("  ESS (all): %.1f", all_result$metrics$ESS_perplexity)
    rm(all_result)
  }

  if (sum(results$is_retained) > 0) {
    aic_ret    <- -2 * results$likelihood[results$is_retained]
    best_ret   <- min(aic_ret[is.finite(aic_ret)])
    delta_ret  <- pmin(aic_ret - best_ret, 25.0)
    wt_ret     <- calc_model_weights_gibbs(x = delta_ret, temperature = 0.5,
                                            verbose = control$logging$verbose)
    results$weight_retained[results$is_retained] <- wt_ret
    log_msg("  ESS (retained): %.1f",
            calc_model_ess(wt_ret, method = control$targets$ESS_method))
  }

  if (sum(results$is_best_subset) > 0) {
    aic_best  <- -2 * results$likelihood[results$is_best_subset]
    best_best <- min(aic_best[is.finite(aic_best)])
    delta_best <- pmin(aic_best - best_best, 4.0)
    wt_best   <- calc_model_weights_gibbs(x = delta_best, temperature = 0.5,
                                           verbose = control$logging$verbose)
    results$weight_best[results$is_best_subset] <- wt_best
    log_msg("  ESS (best): %.1f",
            calc_model_ess(wt_best, method = control$targets$ESS_method))
  }

  subset_summary <- data.frame(
    min_search_size   = control$targets$min_best_subset,
    max_search_size   = control$targets$max_best_subset,
    optimal_size      = n_top_final,
    optimal_percentile = percentile_used,
    optimization_tier  = convergence_tier,
    optimization_method = "grid_search",
    n_selected         = n_top_final,
    ESS_B              = ESS_B_final,
    A                  = A_final,
    CVw                = CVw_final,
    gibbs_temperature  = gibbs_temperature_final,
    meets_all_criteria = final_converged,
    timestamp          = Sys.time()
  )
  summary_file <- file.path(dirs$cal_diag, "subset_selection_summary.csv")
  write.csv(subset_summary, summary_file, row.names = FALSE)
  log_msg("Saved %s", summary_file)

  .mosaic_write_parquet(results, simulations_file, control$io)

  # --- Convergence diagnostics ---
  log_msg("Calculating convergence diagnostics")
  retained_results <- results[results$is_retained, ]
  best_results     <- results[results$is_best_subset, ]

  diagnostics <- calc_convergence_diagnostics(
    n_total            = nrow(results),
    n_successful       = sum(is.finite(results$likelihood)),
    n_retained         = nrow(retained_results),
    n_best_subset      = nrow(best_results),
    ess_best           = ESS_B_final,
    A_best             = A_final,
    cvw_best           = CVw_final,
    percentile_used    = percentile_used,
    convergence_tier   = convergence_tier,
    param_ess_results  = ess_results,
    target_ess_best    = control$targets$ESS_best,
    target_A_best      = control$targets$A_best,
    target_cvw_best    = control$targets$CVw_best,
    target_max_best_subset = control$targets$max_best_subset,
    target_ess_param   = control$targets$ESS_param,
    target_ess_param_prop = control$targets$ESS_param_prop,
    ess_method         = control$targets$ESS_method,
    temperature        = gibbs_temperature_final,
    verbose            = TRUE
  )

  best_aic_val <- .mosaic_safe_min(-2 * results$likelihood[is.finite(results$likelihood)])
  convergence_results_df <- data.frame(
    sim        = results$sim,
    seed       = results$seed_sim,
    likelihood = results$likelihood,
    aic        = -2 * results$likelihood,
    delta_aic  = if (is.finite(best_aic_val)) {
      -2 * results$likelihood - best_aic_val
    } else rep(NA_real_, nrow(results)),
    w          = results$weight_best,
    w_tilde    = results$weight_best,
    retained   = results$is_best_subset,
    w_retained     = results$weight_retained,
    is_retained    = results$is_retained,
    is_best_subset = results$is_best_subset
  )

  convergence_file <- file.path(dirs$cal_diag, "convergence_results.parquet")
  .mosaic_write_parquet(convergence_results_df, convergence_file, control$io)

  diagnostics_file <- file.path(dirs$cal_diag, "convergence_diagnostics.json")
  jsonlite::write_json(diagnostics, diagnostics_file,
                       pretty = TRUE, auto_unbox = TRUE, digits = NA)
  log_msg("Saved convergence_results.parquet and convergence_diagnostics.json")

  if (control$paths$plots) {
    log_msg("Generating convergence diagnostic plots...")
    plot_model_convergence(
      results_dir = dirs$cal_diag,
      plots_dir   = dirs$res_fig_diag,
      verbose     = control$logging$verbose
    )
    plot_model_convergence_status(
      results_dir = dirs$cal_diag,
      plots_dir   = dirs$res_fig_diag,
      verbose     = control$logging$verbose
    )
  }

  # --- Posterior quantiles & distributions ---
  log_msg("Calculating posterior quantiles")
  posterior_quantiles <- calc_model_posterior_quantiles(
    results    = results,
    probs      = c(0.025, 0.25, 0.5, 0.75, 0.975),
    output_dir = dirs$cal_posterior,
    verbose    = control$logging$verbose
  )
  log_msg("Saved posterior_quantiles.csv")

  if (control$paths$plots) {
    plot_model_posterior_quantiles(
      csv_files  = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
      output_dir = dirs$res_fig_post,
      verbose    = control$logging$verbose
    )
  }

  log_msg("Calculating posterior distributions")
  calc_model_posterior_distributions(
    quantiles_file = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
    priors_file    = file.path(dirs$inputs, "priors.json"),
    output_dir     = dirs$cal_posterior,
    control        = control,
    verbose        = control$logging$verbose
  )
  log_msg("Saved posteriors.json")

  if (control$paths$plots) {
    plot_model_distributions(
      json_files   = c(file.path(dirs$inputs, "priors.json"),
                       file.path(dirs$cal_posterior, "posteriors.json")),
      method_names = c("Prior", "Posterior"),
      output_dir   = dirs$res_fig_post
    )
    plot_model_posteriors_detail(
      quantiles_file  = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
      results_file    = file.path(dirs$calibration, "samples.parquet"),
      priors_file     = file.path(dirs$inputs, "priors.json"),
      posteriors_file = file.path(dirs$cal_posterior, "posteriors.json"),
      output_dir      = file.path(dirs$res_fig_post, "detail"),
      verbose         = control$logging$verbose
    )
  }

  # --- Posterior predictive checks ---
  log_msg("Running posterior predictive checks")
  best_idx      <- which.max(results$likelihood)
  best_seed_sim <- results$seed_sim[best_idx]

  config_best <- sample_parameters(
    PATHS       = PATHS,
    priors      = priors,
    config      = config,
    seed        = best_seed_sim,
    sample_args = sampling_args,
    verbose     = FALSE
  )

  config_best_file <- file.path(dirs$cal_best_model, "config_best.json")
  jsonlite::write_json(config_best, config_best_file,
                       pretty = TRUE, auto_unbox = TRUE, digits = NA)
  log_msg("Saved config_best.json")

  # Run best model locally (single sim, not on Dask)
  lc <- reticulate::import("laser.cholera.metapop.model")
  best_model <- lc$run_model(paramfile = .mosaic_prepare_config_for_python(config_best), quiet = TRUE)

  # ===========================================================================
  # MODEL FIT METRICS (BEST MODEL)
  # ===========================================================================

  # Single-model R² and bias ratio
  r2_cases <- tryCatch({
    obs   <- as.numeric(unlist(config_best$reported_cases))
    est   <- as.numeric(unlist(best_model$results$reported_cases))
    valid <- is.finite(obs) & is.finite(est)
    if (sum(valid) > 2) stats::cor(obs[valid], est[valid])^2 else NA_real_
  }, error = function(e) NA_real_)

  r2_deaths <- tryCatch({
    obs   <- as.numeric(unlist(config_best$reported_deaths))
    est   <- as.numeric(unlist(best_model$results$disease_deaths))
    valid <- is.finite(obs) & is.finite(est)
    if (sum(valid) > 2) stats::cor(obs[valid], est[valid])^2 else NA_real_
  }, error = function(e) NA_real_)

  bias_ratio_cases <- tryCatch({
    calc_bias_ratio(config_best$reported_cases, best_model$results$reported_cases)
  }, error = function(e) NA_real_)

  bias_ratio_deaths <- tryCatch({
    calc_bias_ratio(config_best$reported_deaths, best_model$results$disease_deaths)
  }, error = function(e) NA_real_)

  log_msg("Best model R\u00b2: cases = %.4f (bias=%.2f), deaths = %.4f (bias=%.2f)",
          ifelse(is.na(r2_cases),        0, r2_cases),
          ifelse(is.na(bias_ratio_cases), 0, bias_ratio_cases),
          ifelse(is.na(r2_deaths),        0, r2_deaths),
          ifelse(is.na(bias_ratio_deaths), 0, bias_ratio_deaths))

  # ===========================================================================
  # ENSEMBLE R² AND WINDOWED FIT METRICS
  # ===========================================================================

  n_ensemble_r2          <- control$predictions$best_model_n_sims
  r2_cases_ensemble      <- NA_real_
  r2_deaths_ensemble     <- NA_real_
  bias_ratio_cases_ensemble  <- NA_real_
  bias_ratio_deaths_ensemble <- NA_real_

  if (n_ensemble_r2 > 1) {
    log_msg("Computing ensemble R\u00b2 (%d stochastic runs)...", n_ensemble_r2)

    ensemble_results <- tryCatch({
      obs_cases_vec  <- as.numeric(unlist(config_best$reported_cases))
      obs_deaths_vec <- as.numeric(unlist(config_best$reported_deaths))
      n_ts           <- length(obs_cases_vec)

      cases_matrix  <- matrix(NA_real_, nrow = n_ensemble_r2, ncol = n_ts)
      deaths_matrix <- matrix(NA_real_, nrow = n_ensemble_r2, ncol = n_ts)

      for (i in seq_len(n_ensemble_r2)) {
        config_best$seed <- as.integer(best_seed_sim * 1000L + i)
        m <- tryCatch(
          lc$run_model(paramfile = MOSAIC:::.mosaic_prepare_config_for_python(config_best),
                       quiet = TRUE),
          error = function(e) NULL
        )
        if (!is.null(m)) {
          cases_matrix[i, ]  <- as.numeric(unlist(m$results$reported_cases))
          deaths_matrix[i, ] <- as.numeric(unlist(m$results$disease_deaths))
        }
      }

      mean_cases  <- colMeans(cases_matrix,  na.rm = TRUE)
      mean_deaths <- colMeans(deaths_matrix, na.rm = TRUE)

      valid_c <- is.finite(obs_cases_vec)  & is.finite(mean_cases)
      valid_d <- is.finite(obs_deaths_vec) & is.finite(mean_deaths)

      list(
        r2_cases    = if (sum(valid_c) > 2) stats::cor(obs_cases_vec[valid_c],  mean_cases[valid_c])^2  else NA_real_,
        r2_deaths   = if (sum(valid_d) > 2) stats::cor(obs_deaths_vec[valid_d], mean_deaths[valid_d])^2 else NA_real_,
        bias_cases  = calc_bias_ratio(obs_cases_vec,  mean_cases),
        bias_deaths = calc_bias_ratio(obs_deaths_vec, mean_deaths),
        mean_cases  = mean_cases,
        mean_deaths = mean_deaths,
        obs_cases   = obs_cases_vec,
        obs_deaths  = obs_deaths_vec
      )
    }, error = function(e) {
      log_msg("Warning: ensemble R\u00b2 computation failed: %s", e$message)
      list(r2_cases = NA_real_, r2_deaths = NA_real_,
           bias_cases = NA_real_, bias_deaths = NA_real_,
           mean_cases = NULL, mean_deaths = NULL,
           obs_cases = NULL, obs_deaths = NULL)
    })

    r2_cases_ensemble          <- ensemble_results$r2_cases
    r2_deaths_ensemble         <- ensemble_results$r2_deaths
    bias_ratio_cases_ensemble  <- ensemble_results$bias_cases
    bias_ratio_deaths_ensemble <- ensemble_results$bias_deaths

    log_msg("Ensemble R\u00b2 (%d runs): cases = %.4f (bias=%.2f), deaths = %.4f (bias=%.2f)",
            n_ensemble_r2,
            ifelse(is.na(r2_cases_ensemble),          0, r2_cases_ensemble),
            ifelse(is.na(bias_ratio_cases_ensemble),  0, bias_ratio_cases_ensemble),
            ifelse(is.na(r2_deaths_ensemble),         0, r2_deaths_ensemble),
            ifelse(is.na(bias_ratio_deaths_ensemble), 0, bias_ratio_deaths_ensemble))

    # Windowed model fit metrics (trailing windows of observed data)
    if (!is.null(ensemble_results$mean_cases)) {
      n_ts      <- length(ensemble_results$obs_cases)
      dates_vec <- seq.Date(as.Date(config_best$date_start), by = "day", length.out = n_ts)

      fit_windows <- if (!is.null(control$predictions$fit_windows)) {
        control$predictions$fit_windows
      } else {
        c(365, 120, 90, 60, 30)
      }

      windowed_metrics <- .mosaic_compute_windowed_metrics(
        obs_cases  = ensemble_results$obs_cases,
        est_cases  = ensemble_results$mean_cases,
        obs_deaths = ensemble_results$obs_deaths,
        est_deaths = ensemble_results$mean_deaths,
        dates      = dates_vec,
        windows    = fit_windows
      )

      wm_path <- file.path(dirs$res_fig_diag, "model_fit_windows.csv")
      utils::write.csv(windowed_metrics, wm_path, row.names = FALSE)
      log_msg("Saved 3_results/figures/diagnostics/model_fit_windows.csv")

      full_row      <- windowed_metrics[windowed_metrics$window == "full", ]
      short_windows <- windowed_metrics[windowed_metrics$window != "full", ]
      if (nrow(short_windows) > 0) {
        last_row <- short_windows[nrow(short_windows), ]
        log_msg("Windowed fit: R2_cases [full=%.3f, %s=%.3f] | Bias [full=%.2f, %s=%.2f]",
                full_row$r2_cases, last_row$window, last_row$r2_cases,
                full_row$bias_cases, last_row$window, last_row$bias_cases)
      }

      if (control$paths$plots) {
        wm_plot_path <- file.path(dirs$res_fig_diag, "model_fit_windows.png")
        tryCatch({
          .mosaic_plot_windowed_metrics(windowed_metrics, wm_plot_path,
                                        location = paste(config_best$location_name, collapse = ", "))
          log_msg("Saved 3_results/figures/diagnostics/model_fit_windows.png")
        }, error = function(e) {
          log_msg("Warning: windowed metrics plot failed: %s", e$message)
        })
      }
    }
  }

  if (control$paths$plots) {
    log_msg("Generating posterior predictive plots (best model)...")
    plot_model_fit_stochastic(
      config             = config_best,
      n_simulations      = control$predictions$best_model_n_sims,
      output_dir         = dirs$res_fig_pred,
      envelope_quantiles = c(0.025, 0.975),
      save_predictions   = TRUE,
      parallel           = control$parallel$enable,
      n_cores            = control$parallel$n_cores,
      root_dir           = root_dir,
      verbose            = control$logging$verbose
    )
  }

  if (control$paths$plots && sum(results$is_best_subset) > 0) {
    log_msg("Generating ensemble predictions...")
    best_subset_results <- results[results$is_best_subset == TRUE, ]
    param_seeds  <- best_subset_results$seed_sim
    param_weights <- best_subset_results$weight_best[
      best_subset_results$weight_best > 0
    ]
    if (length(param_weights) > 0) {
      param_weights <- param_weights / sum(param_weights)
    } else {
      param_weights <- NULL
    }

    plot_model_fit_stochastic_param(
      config                      = config,
      parameter_seeds             = param_seeds,
      parameter_weights           = param_weights,
      n_simulations_per_config    = control$predictions$ensemble_n_sims_per_param,
      envelope_quantiles          = c(0.025, 0.25, 0.75, 0.975),
      PATHS                       = PATHS,
      priors                      = priors,
      sampling_args               = sampling_args,
      output_dir                  = dirs$res_fig_pred,
      save_predictions            = TRUE,
      parallel                    = control$parallel$enable,
      n_cores                     = control$parallel$n_cores,
      root_dir                    = root_dir,
      verbose                     = control$logging$verbose
    )
  }

  if (control$paths$plots) {
    ppc_result <- tryCatch(
      plot_model_ppc(
        predictions_dir = dirs$res_fig_pred,
        output_dir      = dirs$res_fig_ppc,
        verbose         = control$logging$verbose
      ),
      error = function(e) {
        if (grepl("unused argument", e$message)) {
          log_msg("Warning: plot_model_ppc using legacy signature")
          NULL
        } else stop(e)
      }
    )
  }

  # ===========================================================================
  # COMBINE PREDICTION CSVs
  # ===========================================================================

  # Combine per-location prediction CSVs by type (ensemble and stochastic have
  # different column schemas and cannot be rbind'd together).
  # Only runs when plots = TRUE (prediction CSVs are a side-effect of plot fns).
  for (pred_type in c("ensemble", "stochastic")) {
    pred_csvs <- list.files(dirs$res_fig_pred,
                            pattern = sprintf("^predictions_%s_.*\\.csv$", pred_type),
                            full.names = TRUE)
    if (length(pred_csvs) > 1) {
      combined <- tryCatch({
        do.call(rbind, lapply(pred_csvs, utils::read.csv, stringsAsFactors = FALSE))
      }, error = function(e) {
        log_msg("Warning: Could not combine %s prediction CSVs: %s", pred_type, e$message)
        NULL
      })
      if (!is.null(combined)) {
        out_file <- file.path(dirs$res_predictions, sprintf("all_predictions_%s.csv", pred_type))
        utils::write.csv(combined, out_file, row.names = FALSE)
        log_msg("Combined %d %s prediction files into %s",
                length(pred_csvs), pred_type, basename(out_file))
      }
    } else if (length(pred_csvs) == 1) {
      out_file <- file.path(dirs$res_predictions, sprintf("all_predictions_%s.csv", pred_type))
      file.copy(pred_csvs, out_file, overwrite = TRUE)
    }
  }

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  log_msg("Dask calibration complete: %d batches, %d simulations, %.2f min",
          state$batch_number, state$total_sims_run, as.numeric(runtime))

  invisible(list(
    dirs  = dirs,
    files = list(
      simulations       = simulations_file,
      ess_csv           = file.path(dirs$cal_diag, "parameter_ess.csv"),
      posterior_quantiles = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
      posteriors_json   = file.path(dirs$cal_posterior, "posteriors.json")
    ),
    summary = list(
      batches      = state$batch_number,
      sims_total   = state$total_sims_run,
      sims_success = state$total_sims_successful,
      converged    = isTRUE(state$converged),
      runtime_min  = as.numeric(runtime)
    )
  ))
}

#' @rdname run_MOSAIC_dask
#' @export
run_mosaic_dask <- run_MOSAIC_dask
