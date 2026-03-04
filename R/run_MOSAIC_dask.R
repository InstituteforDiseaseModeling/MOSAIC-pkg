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
#' Matrix fields are excluded because they live in the broadcast base_config.
#' @noRd
.extract_sampled_params <- function(params_sim) {
  # Exclude fields that are in the broadcast base_config
  base_fields <- c(
    "b_jt", "d_jt", "mu_jt", "psi_jt", "nu_1_jt", "nu_2_jt",
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
        verbose    = FALSE
      ),
      error = function(e) {
        warning("Param sampling failed for sim ", sim_id, ": ", e$message,
                call. = FALSE, immediate. = FALSE)
        NULL
      }
    )
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
  gathered <- client$gather(valid_futures)

  # Build lookup: sim_id → worker result
  result_lookup <- list()
  for (res in gathered) {
    key <- as.character(res$sim_id)
    result_lookup[[key]] <- res
  }

  # ---------------------------------------------------------------------------
  # 5. For each sim: compute likelihood in R, write parquet
  # ---------------------------------------------------------------------------
  success_indicators <- logical(n_sims)

  for (idx in seq_len(n_sims)) {
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
          add_max_terms         = control$likelihood$add_max_terms,
          add_peak_timing       = control$likelihood$add_peak_timing,
          add_peak_magnitude    = control$likelihood$add_peak_magnitude,
          add_cumulative_total  = control$likelihood$add_cumulative_total,
          add_wis               = control$likelihood$add_wis,
          weight_cases          = control$likelihood$weight_cases,
          weight_deaths         = control$likelihood$weight_deaths,
          weight_max_terms      = control$likelihood$weight_max_terms,
          weight_peak_timing    = control$likelihood$weight_peak_timing,
          weight_peak_magnitude = control$likelihood$weight_peak_magnitude,
          weight_cumulative_total = control$likelihood$weight_cumulative_total,
          weight_wis            = control$likelihood$weight_wis,
          sigma_peak_time       = control$likelihood$sigma_peak_time,
          sigma_peak_log        = control$likelihood$sigma_peak_log,
          penalty_unmatched_peak = control$likelihood$penalty_unmatched_peak,
          enable_guardrails     = control$likelihood$enable_guardrails,
          floor_likelihood      = control$likelihood$floor_likelihood,
          guardrail_verbose     = control$likelihood$guardrail_verbose
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
    seed_iter_1 <- (sim_id - 1L) * n_iterations + 1L

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

    out_file <- file.path(dirs$bfrs_params,
                          sprintf("sim_%07d.parquet", sim_id))
    .mosaic_write_parquet(row_df, out_file, control$io)
    success_indicators[idx] <- file.exists(out_file)
  }

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
#' \code{laser_cholera.metapop.model.run_model()} directly, return case/death
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
#'       Default \code{"mosaic-docker-workers"}.}
#'     \item{vm_types}{Character vector. Azure VM type(s).
#'       Default \code{"Standard_D8s_v6"}.}
#'     \item{region}{Character. Azure region. Default \code{"westus2"}.}
#'     \item{idle_timeout}{Character. Coiled idle shutdown timeout.
#'       Default \code{"2 hours"}.}
#'     \item{address}{Character. Scheduler address for \code{type="scheduler"},
#'       e.g. \code{"tcp://localhost:8786"}.}
#'   }
#'   If \code{NULL}, defaults to \code{list(type="coiled")} with all sub-fields
#'   at their defaults.
#' @param resume Logical. If \code{TRUE}, continues from existing checkpoint.
#'   Default \code{FALSE}.
#'
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
                             dask_spec = NULL,
                             resume    = FALSE) {

  # ===========================================================================
  # ARGUMENT VALIDATION
  # ===========================================================================

  stopifnot(
    "config is required and must be a list" =
      !missing(config) && is.list(config) && length(config) > 0,
    "priors is required and must be a list" =
      !missing(priors) && is.list(priors) && length(priors) > 0,
    "dir_output is required and must be character string" =
      !missing(dir_output) && is.character(dir_output) && length(dir_output) == 1L,
    "resume must be logical" =
      is.logical(resume) && length(resume) == 1L
  )

  if (!"location_name" %in% names(config)) {
    stop("config must contain 'location_name' field", call. = FALSE)
  }

  # Normalise dask_spec defaults
  if (is.null(dask_spec)) dask_spec <- list()
  dask_spec$type         <- dask_spec$type         %||% "coiled"
  dask_spec$software     <- dask_spec$software     %||% "mosaic-docker-workers"
  dask_spec$vm_types     <- dask_spec$vm_types     %||% c("Standard_D8s_v6")
  dask_spec$region       <- dask_spec$region       %||% "westus2"
  dask_spec$idle_timeout <- dask_spec$idle_timeout %||% "2 hours"

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
    run_npe      = isTRUE(control$npe$enable),
    clean_output = isTRUE(control$paths$clean_output)
  )

  # ===========================================================================
  # SETUP FILES
  # ===========================================================================

  cluster_metadata <- .mosaic_get_cluster_metadata()
  sim_params <- list(
    control          = control,
    n_iterations     = n_iterations,
    iso_code         = iso_code,
    timestamp        = Sys.time(),
    R_version        = R.version.string,
    MOSAIC_version   = as.character(utils::packageVersion("MOSAIC")),
    cluster_metadata = cluster_metadata,
    dask_spec        = dask_spec,
    paths = list(
      dir_output = dirs$root,
      dir_setup  = dirs$setup,
      dir_bfrs   = dirs$bfrs,
      dir_npe    = if (isTRUE(control$npe$enable)) dirs$npe else NULL,
      dir_results = dirs$results
    )
  )

  log_msg("Writing setup files...")
  .mosaic_write_json(sim_params, file.path(dirs$setup, "simulation_params.json"), control$io)
  .mosaic_write_json(priors,     file.path(dirs$setup, "priors.json"),            control$io)
  .mosaic_write_json(config,     file.path(dirs$setup, "config_base.json"),       control$io)
  log_msg("  Saved simulation_params.json, priors.json, config_base.json")

  log_msg("Plotting prior distributions")
  plot_model_distributions(
    json_files   = file.path(dirs$setup, "priors.json"),
    method_names = "Prior",
    output_dir   = dirs$setup,
    verbose      = control$logging$verbose
  )

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

  log_msg("Parameters: %d estimated (of %d total) | Locations: %s",
          length(param_names_estimated), length(param_names_all),
          paste(config$location_name, collapse = ", "))

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

    cluster <- coiled_mod$Cluster(
      name                = cluster_name,
      n_workers           = as.integer(n_workers_req),
      worker_vm_types     = as.list(dask_spec$vm_types),
      scheduler_vm_types  = as.list(dask_spec$vm_types),
      region              = dask_spec$region,
      software            = dask_spec$software,
      shutdown_on_close   = TRUE,
      idle_timeout        = dask_spec$idle_timeout
    )
    client <- dask_dist$Client(cluster)
  } else {
    log_msg("Connecting to Dask scheduler: %s", dask_spec$address)
    client <- dask_dist$Client(dask_spec$address)
  }

  log_msg("Dask dashboard: %s", client$dashboard_link)

  # Register cleanup (runs on normal exit OR error)
  on.exit({
    try({
      client$close()
      log_msg("Dask client closed")
    }, silent = TRUE)
    if (!is.null(cluster)) {
      try({
        cluster$close()
        log_msg("Dask cluster closed")
      }, silent = TRUE)
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
  state_file <- file.path(dirs$bfrs_diag, "run_state.rds")

  state <- if (resume && file.exists(state_file)) {
    log_msg("Attempting to resume from: %s", state_file)
    loaded <- .mosaic_load_state_safe(state_file)
    if (is.null(loaded)) {
      log_msg("WARNING: Failed to load state file — starting fresh")
      .mosaic_init_state(control, param_names_estimated, nspec)
    } else {
      log_msg("Resumed state (batch %d, %d sims completed)",
              loaded$batch_number, loaded$total_sims_run)
      loaded
    }
  } else {
    .mosaic_init_state(control, param_names_estimated, nspec)
  }

  log_msg("Starting simulation (mode: %s)", state$mode)
  start_time <- Sys.time()

  # ===========================================================================
  # FIXED MODE
  # ===========================================================================

  if (identical(state$mode, "fixed")) {
    target <- state$fixed_target
    log_msg("[FIXED MODE] Running exactly %d simulations", target)

    done_ids <- integer()
    if (resume) {
      existing <- list.files(dirs$bfrs_params,
                             pattern = "^sim_[0-9]{7}\\.parquet$",
                             full.names = FALSE)
      if (length(existing)) {
        done_ids <- .mosaic_parse_sim_ids(existing,
                                          pattern = "^sim_0*([0-9]+)\\.parquet$")
        log_msg("Found %d existing simulations to skip", length(done_ids))
      }
    }

    all_ids <- seq_len(target)
    sim_ids <- setdiff(all_ids, done_ids)

    if (length(sim_ids) == 0L) {
      log_msg("Nothing to do: %d simulations already complete", length(done_ids))
    } else {
      log_msg("Running %d simulations (%d-%d)",
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
        client             = client,
        base_config_future = base_config_future,
        mosaic_worker      = mosaic_worker
      )

      n_success <- sum(unlist(success_indicators))
      state$total_sims_successful <- state$total_sims_successful + n_success
      state$batch_success_rates   <- c(state$batch_success_rates,
                                        (n_success / length(sim_ids)) * 100)
      state$batch_sizes_used  <- c(state$batch_sizes_used, length(sim_ids))
      state$batch_number      <- state$batch_number + 1L
      state$total_sims_run    <- length(all_ids)

      batch_runtime <- difftime(Sys.time(), batch_start_time, units = "mins")
      log_msg("Fixed batch complete: %d/%d successful (%.1f%%) in %.1f minutes",
              n_success, length(sim_ids),
              tail(state$batch_success_rates, 1),
              as.numeric(batch_runtime))

      .mosaic_save_state(state, state_file)
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

      if (state$total_sims_run < control$calibration$max_simulations) {
        state <- .mosaic_ess_check_update_state(state, dirs,
                                                param_names_estimated, control)
        .mosaic_save_state(state, state_file)
      } else {
        log_msg("Skipping ESS check (final batch)")
      }

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

  log_msg("Combining simulation files")

  parquet_files <- list.files(dirs$bfrs_params,
                               pattern = "^sim_.*\\.parquet$",
                               full.names = TRUE)

  load_method <- if (!is.null(control$io$load_method)) {
    control$io$load_method
  } else {
    "streaming"
  }

  results <- .mosaic_load_and_combine_results(
    dir_params = dirs$bfrs_params,
    method     = load_method,
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
    results$is_outlier[results$is_valid] <-
      valid_ll < (q1 - iqr_mult * iqr) | valid_ll > (q3 + iqr_mult * iqr)
    log_msg("  Outliers: %d (%.1f%%)",
            sum(results$is_outlier),
            100 * sum(results$is_outlier) / sum(results$is_valid))
  }

  results$is_retained    <- results$is_finite & !results$is_outlier
  results$is_best_subset <- FALSE
  results$is_best_model  <- FALSE
  if (any(results$is_valid)) {
    results$is_best_model[which.max(results$likelihood)] <- TRUE
  }

  simulations_file <- file.path(dirs$bfrs_out, "simulations.parquet")
  .mosaic_write_parquet(results, simulations_file, control$io)

  if (length(parquet_files) > 0) {
    unlink(parquet_files)
    log_msg("  Cleaned up %d individual simulation files", length(parquet_files))
  }
  log_msg("  Combined simulations saved: %s", basename(simulations_file))

  # --- Parameter ESS ---
  log_msg("Calculating parameter ESS")
  ess_results <- calc_model_ess_parameter(
    results        = results,
    param_names    = param_names_estimated,
    likelihood_col = "likelihood",
    n_grid         = 100,
    method         = control$targets$ESS_method,
    verbose        = control$logging$verbose
  )
  ess_file <- file.path(dirs$bfrs_diag, "parameter_ess.csv")
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
      results    = results,
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
    n_top_final       <- min(control$targets$max_best_subset, nrow(results))
    results_ranked    <- results[order(results$likelihood, decreasing = TRUE), ]
    top_subset_final  <- results_ranked[1:n_top_final, ]
    rm(results_ranked)
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
  summary_file <- file.path(dirs$bfrs_diag, "subset_selection_summary.csv")
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
    seed       = results$sim,
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

  convergence_file <- file.path(dirs$bfrs_diag, "convergence_results.parquet")
  .mosaic_write_parquet(convergence_results_df, convergence_file, control$io)

  diagnostics_file <- file.path(dirs$bfrs_diag, "convergence_diagnostics.json")
  jsonlite::write_json(diagnostics, diagnostics_file,
                       pretty = TRUE, auto_unbox = TRUE)
  log_msg("Saved convergence_results.parquet and convergence_diagnostics.json")

  if (control$paths$plots) {
    log_msg("Generating convergence diagnostic plots...")
    plot_model_convergence(
      results_dir = dirs$bfrs_diag,
      plots_dir   = dirs$bfrs_plots_diag,
      verbose     = control$logging$verbose
    )
    plot_model_convergence_status(
      results_dir = dirs$bfrs_diag,
      plots_dir   = dirs$bfrs_plots_diag,
      verbose     = control$logging$verbose
    )
  }

  # --- Posterior quantiles & distributions ---
  log_msg("Calculating posterior quantiles")
  posterior_quantiles <- calc_model_posterior_quantiles(
    results    = results,
    probs      = c(0.025, 0.25, 0.5, 0.75, 0.975),
    output_dir = dirs$bfrs_post,
    verbose    = control$logging$verbose
  )
  log_msg("Saved posterior_quantiles.csv")

  if (control$paths$plots) {
    plot_model_posterior_quantiles(
      csv_files  = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
      output_dir = dirs$bfrs_plots_post,
      verbose    = control$logging$verbose
    )
  }

  log_msg("Calculating posterior distributions")
  calc_model_posterior_distributions(
    quantiles_file = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
    priors_file    = file.path(dirs$setup, "priors.json"),
    output_dir     = dirs$bfrs_post,
    verbose        = control$logging$verbose
  )
  log_msg("Saved posteriors.json")

  if (control$paths$plots) {
    plot_model_distributions(
      json_files   = c(file.path(dirs$setup, "priors.json"),
                       file.path(dirs$bfrs_post, "posteriors.json")),
      method_names = c("Prior", "Posterior"),
      output_dir   = dirs$bfrs_plots_post
    )
    plot_model_posteriors_detail(
      quantiles_file  = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
      results_file    = file.path(dirs$bfrs_out, "simulations.parquet"),
      priors_file     = file.path(dirs$setup, "priors.json"),
      posteriors_file = file.path(dirs$bfrs_post, "posteriors.json"),
      output_dir      = file.path(dirs$bfrs_plots_post, "detail"),
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

  config_best_file <- file.path(dirs$bfrs_cfg, "config_best.json")
  jsonlite::write_json(config_best, config_best_file,
                       pretty = TRUE, auto_unbox = TRUE)
  log_msg("Saved config_best.json")

  # Run best model locally (single sim, not on Dask)
  lc <- reticulate::import("laser_cholera.metapop.model")
  best_model <- lc$run_model(paramfile = config_best, quiet = TRUE)

  if (control$paths$plots) {
    log_msg("Generating posterior predictive plots (best model)...")
    plot_model_fit_stochastic(
      config             = config_best,
      n_simulations      = control$predictions$best_model_n_sims,
      output_dir         = dirs$bfrs_plots_pred,
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
      output_dir                  = dirs$bfrs_plots_pred,
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
        predictions_dir = dirs$bfrs_plots_pred,
        output_dir      = dirs$bfrs_plots,
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

  # --- NPE stage ---
  if (control$npe$enable) {
    log_msg("Starting NPE Stage")
    npe_result <- run_NPE(
      results     = results,
      priors      = priors,
      config      = config,
      control     = control,
      param_names = param_names_estimated,
      dirs        = dirs,
      PATHS       = PATHS,
      verbose     = control$logging$verbose
    )
    log_msg("NPE Stage complete")
  } else {
    log_msg("NPE skipped")
  }

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  log_msg("Dask calibration complete: %d batches, %d simulations, %.2f min",
          state$batch_number, state$total_sims_run, as.numeric(runtime))

  invisible(list(
    dirs  = dirs,
    files = list(
      simulations       = simulations_file,
      ess_csv           = file.path(dirs$bfrs_diag, "parameter_ess.csv"),
      posterior_quantiles = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
      posteriors_json   = file.path(dirs$bfrs_post, "posteriors.json")
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
