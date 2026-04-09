# =============================================================================
# MOSAIC: run_mosaic.R
# Main calibration workflow function
# =============================================================================

# =============================================================================
# PYTHON COMPATIBILITY HELPER
# =============================================================================

# Wrap location-specific scalar params as R lists so reticulate passes them as
# Python lists instead of Python scalars.  Python's dict_to_propertysetex uses
# np.array() (not as_ndarray()) for these params, so a Python scalar produces a
# 0-d array (shape ()) rather than the expected 1-d array (shape (npatches,)).
# This only affects single-location runs where length == 1.
#
# @param config A MOSAIC config list
# @return config with affected length-1 params wrapped as R lists
# @noRd
.mosaic_prepare_config_for_python <- function(config) {
  # Multi-location configs never need wrapping (all location params have length > 1)
  if (length(config$location_name) > 1L) return(config)

  array_params <- c(
    "psi_star_a", "psi_star_b", "psi_star_z", "psi_star_k",
    "beta_j0_tot", "p_beta",
    "prop_S_initial", "prop_E_initial", "prop_I_initial",
    "prop_R_initial", "prop_V1_initial", "prop_V2_initial",
    "mu_j_baseline", "mu_j_slope", "mu_j_epidemic_factor"
  )
  for (p in array_params) {
    if (!is.null(config[[p]]) && length(config[[p]]) == 1 && !is.list(config[[p]])) {
      config[[p]] <- as.list(config[[p]])
    }
  }
  config
}

# =============================================================================
# FAST PARAMETER EXTRACTION (replaces convert_config_to_matrix in hot loop)
# =============================================================================

#' Build Parameter Lookup Table
#'
#' Precomputes a mapping from param_names_all to config keys and location
#' indices. Built once at setup, passed to each worker to avoid per-sim
#' overhead of convert_config_to_matrix().
#'
#' @param param_names_all Character vector of parameter names (from setup)
#' @param location_names Character vector of location ISO codes
#' @return A list of lists, each with \code{key} (config key) and \code{idx}
#'   (integer location index, or NULL for global params)
#' @noRd
.mosaic_build_param_lookup <- function(param_names_all, location_names) {
  n <- length(param_names_all)
  lookup <- vector("list", n)
  for (i in seq_len(n)) {
    nm <- param_names_all[i]
    # Check if name ends with _ISO (3-letter uppercase suffix matching a location)
    suffix <- sub("^.*_([A-Z]{3})$", "\\1", nm)
    if (nchar(suffix) == 3L && suffix %in% location_names) {
      base_key <- sub("_[A-Z]{3}$", "", nm)
      loc_idx <- match(suffix, location_names)
      lookup[[i]] <- list(key = base_key, idx = loc_idx)
    } else {
      lookup[[i]] <- list(key = nm, idx = NULL)
    }
  }
  lookup
}

#' Extract Parameter Row from Config Using Precomputed Lookup
#'
#' Lightweight replacement for convert_config_to_matrix() in the worker hot
#' loop. Uses direct indexed access instead of iterating all config keys.
#'
#' @param config A sampled config list
#' @param param_lookup Precomputed lookup from .mosaic_build_param_lookup()
#' @param n_params Length of param_names_all
#' @return Numeric vector of length n_params
#' @noRd
.mosaic_extract_param_row <- function(config, param_lookup, n_params) {
  row <- numeric(n_params)
  for (i in seq_len(n_params)) {
    entry <- param_lookup[[i]]
    val <- config[[entry$key]]
    if (is.null(val)) {
      row[i] <- NA_real_
    } else if (!is.null(entry$idx)) {
      row[i] <- as.numeric(val[entry$idx])
    } else {
      row[i] <- as.numeric(val[1L])
    }
  }
  row
}

# =============================================================================
# SIMULATION WORKER FUNCTION
# =============================================================================

#' Simulation Worker Function
#'
#' Runs n_iterations iterations per simulation, samples parameters once per sim_id,
#' collapses likelihoods via log-mean-exp, and writes parquet files.
#'
#' @section Seed Scheme:
#' \itemize{
#'   \item \code{sim_id}: Unique simulation ID (1-based integer)
#'   \item \code{seed_sim}: Parameter sampling seed (equals sim_id)
#'   \item \code{seed_iter}: LASER model seed for iteration j
#'                           = (sim_id - 1) * n_iterations + j
#' }
#'
#' This ensures:
#' - Same sim_id always gets same parameters
#' - Different iterations have different stochastic realizations
#' - Seeds don't overlap between simulations
#'
#' @noRd
.mosaic_run_simulation_worker <- function(sim_id, n_iterations, priors, config, PATHS,
                                          dir_cal_samples,
                                          dir_cal_simresults = NULL,
                                          param_names_all, param_lookup, sampling_args, io,
                                          likelihood_settings) {

  # Pre-allocate result matrix (FIXED: proper pre-allocation)
  n_params <- length(param_names_all)
  result_matrix <- matrix(NA_real_, nrow = n_iterations, ncol = 5 + n_params)
  colnames(result_matrix) <- c('sim', 'iter', 'seed_sim', 'seed_iter', 'likelihood', param_names_all)

  # Sample parameters ONCE per simulation
  params_sim <- tryCatch({
    sample_parameters(
      PATHS = PATHS,
      priors = priors,
      config = config,
      seed = sim_id,
      sample_args = sampling_args,
      verbose = FALSE,
      validate = FALSE  # Skip per-sim validation (priors guarantee valid ranges)
    )
  }, error = function(e) {
    # Log error details for debugging
    warning("Simulation ", sim_id, " failed during parameter sampling: ",
            e$message, call. = FALSE, immediate. = FALSE)
    NULL
  })

  if (is.null(params_sim)) return(FALSE)

  # Guardrails: clamp transmission parameters to prevent laser-cholera ValueError
  # (GitHub #24: p = -np.expm1(-rate) produces p > 1 when rate < 0)
  if (!is.null(params_sim$beta_j0_tot)) {
    params_sim$beta_j0_tot <- pmax(params_sim$beta_j0_tot, 1e-10)
  }
  if (!is.null(params_sim$beta_j0_hum)) {
    params_sim$beta_j0_hum <- pmax(params_sim$beta_j0_hum, 0)
  }
  if (!is.null(params_sim$beta_j0_env)) {
    params_sim$beta_j0_env <- pmax(params_sim$beta_j0_env, 0)
  }
  if (!is.null(params_sim$p_beta)) {
    params_sim$p_beta <- pmin(pmax(params_sim$p_beta, 1e-6), 1 - 1e-6)
  }
  if (!is.null(params_sim$tau_i)) {
    params_sim$tau_i <- pmin(pmax(params_sim$tau_i, 0), 1)
  }

  # Pre-allocate simresults collector (validation mode only)
  simresults_raw <- if (!is.null(dir_cal_simresults)) vector("list", n_iterations) else NULL

  # Import laser-cholera (explicit check, no inherits)
  # Parallel mode: lc exists in worker global environment
  # Sequential mode: import here
  if (!exists("lc", where = .GlobalEnv, inherits = FALSE)) {
    lc <- reticulate::import("laser.cholera.metapop.model")
  } else {
    lc <- get("lc", envir = .GlobalEnv)
  }

  # Run iterations
  for (j in 1:n_iterations) {
    # Use iteration-specific seed (see seed scheme documentation above)
    seed_ij <- as.integer((sim_id - 1L) * n_iterations + j)
    params_sim$seed <- seed_ij

    # Initialize row directly in pre-allocated matrix (FIXED: no rbind!)
    result_matrix[j, 1:4] <- c(sim_id, j, sim_id, seed_ij)

    # Extract parameters directly using precomputed lookup (fast path)
    param_row <- tryCatch({
      .mosaic_extract_param_row(params_sim, param_lookup, n_params)
    }, error = function(e) NULL)

    if (!is.null(param_row)) {
      result_matrix[j, 6:(5 + n_params)] <- param_row
    }

    # Prepare config for Python (wrap length-1 array params as lists)
    params_py <- .mosaic_prepare_config_for_python(params_sim)

    # Run model
    model <- tryCatch({
      lc$run_model(paramfile = params_py, quiet = TRUE)
    }, error = function(e) {
      # Log model run failure (but don't fail entire simulation)
      warning("Simulation ", sim_id, " iteration ", j, " model run failed: ",
              e$message, call. = FALSE, immediate. = FALSE)
      NULL
    })

    # Calculate likelihood
    if (!is.null(model)) {
      likelihood <- tryCatch({
        obs_cases <- params_sim$reported_cases
        est_cases <- model$results$reported_cases  # v0.11.1: reported_cases = Isym*rho/chi (comparable to surveillance data)
        obs_deaths <- params_sim$reported_deaths
        est_deaths <- model$results$disease_deaths

        if (!is.null(obs_cases) && !is.null(est_cases) &&
            !is.null(obs_deaths) && !is.null(est_deaths)) {

          calc_model_likelihood(
            config = config,
            obs_cases = obs_cases,
            est_cases = est_cases,
            obs_deaths = obs_deaths,
            est_deaths = est_deaths,
            weights_time = likelihood_settings$.weights_time_resolved,
            weights_location = likelihood_settings$weights_location,
            nb_k_min_cases = likelihood_settings$nb_k_min_cases,
            nb_k_min_deaths = likelihood_settings$nb_k_min_deaths,
            weight_cases = likelihood_settings$weight_cases,
            weight_deaths = likelihood_settings$weight_deaths,
            weight_peak_timing = likelihood_settings$weight_peak_timing,
            weight_peak_magnitude = likelihood_settings$weight_peak_magnitude,
            weight_cumulative_total = likelihood_settings$weight_cumulative_total,
            weight_wis = likelihood_settings$weight_wis,
            sigma_peak_time = likelihood_settings$sigma_peak_time,
            sigma_peak_log = likelihood_settings$sigma_peak_log
          )
        } else {
          NA_real_
        }
      }, error = function(e) {
        warning("Simulation ", sim_id, " iteration ", j, " likelihood calculation failed: ",
                e$message, call. = FALSE, immediate. = FALSE)
        NA_real_
      })

      result_matrix[j, 5] <- likelihood

      # Capture raw per-(j, t) output for validation simresults
      if (!is.null(simresults_raw)) {
        raw_cases  <- model$results$reported_cases
        raw_deaths <- model$results$disease_deaths
        if (!is.null(raw_cases) && !is.null(raw_deaths)) {
          if (!is.matrix(raw_cases))  raw_cases  <- matrix(raw_cases,  nrow = 1)
          if (!is.matrix(raw_deaths)) raw_deaths <- matrix(raw_deaths, nrow = 1)
          n_j_raw <- nrow(raw_cases)
          n_t_raw <- ncol(raw_cases)
          simresults_raw[[j]] <- data.frame(
            sim    = sim_id,
            iter   = as.integer(j),
            j      = rep(seq_len(n_j_raw), times = n_t_raw),
            t      = rep(seq_len(n_t_raw), each  = n_j_raw),
            cases  = as.numeric(raw_cases),
            deaths = as.numeric(raw_deaths)
          )
          if (!is.null(params_sim$psi_jt)) {
            simresults_raw[[j]]$psi_jt <- params_sim$psi_jt[cbind(simresults_raw[[j]]$j, simresults_raw[[j]]$t)]
          }
        }
      }
    }

    # Explicit garbage collection on last iteration to prevent Python object buildup
    if (j == n_iterations) {
      gc(verbose = FALSE)
    }
  }

  # Collapse iterations if n_iterations > 1
  if (n_iterations > 1 && nrow(result_matrix) > 0) {
    likelihoods <- result_matrix[, "likelihood"]
    valid_ll <- is.finite(likelihoods)

    if (any(valid_ll)) {
      collapsed_ll <- calc_log_mean_exp(likelihoods[valid_ll])
      collapsed_params <- colMeans(result_matrix[valid_ll, param_names_all, drop = FALSE], na.rm = TRUE)
      collapsed_row <- c(sim_id, 1, sim_id, result_matrix[1, "seed_iter"], collapsed_ll, collapsed_params)
      result_matrix <- matrix(collapsed_row, nrow = 1)
      colnames(result_matrix) <- c('sim', 'iter', 'seed_sim', 'seed_iter', 'likelihood', param_names_all)
    } else {
      result_matrix <- result_matrix[1, , drop = FALSE]
    }
  }

  # Write parameter file
  output_file <- file.path(dir_cal_samples, sprintf("sim_%07d.parquet", sim_id))
  .mosaic_write_parquet(as.data.frame(result_matrix), output_file, io)

  # Write raw simulation results for validation (when save_simresults = TRUE)
  if (!is.null(simresults_raw) && !is.null(dir_cal_simresults)) {
    simresults_raw <- Filter(Negate(is.null), simresults_raw)
    if (length(simresults_raw) > 0) {
      raw_df <- do.call(rbind, simresults_raw)
      param_vals <- as.numeric(result_matrix[1, param_names_all])
      for (pi in seq_along(param_names_all)) {
        raw_df[[param_names_all[pi]]] <- param_vals[pi]
      }
      simresults_file <- file.path(dir_cal_simresults,
                                   sprintf("simresults_%07d.parquet", sim_id))
      .mosaic_write_parquet(raw_df, simresults_file, io)
    }
  }

  # R GC every sim to process reticulate finalizer queue;
  # Python full GC every 100 sims (frequent full sweeps are counterproductive)
  gc(verbose = FALSE)
  if (sim_id %% 100L == 0L) {
    reticulate::import("gc")$collect()
  }

  return(file.exists(output_file))
}

# =============================================================================
# PUBLIC API FUNCTIONS
# =============================================================================

#' Run MOSAIC Calibration Workflow
#'
#' @description
#' **Complete Bayesian calibration workflow with full control over model specification.**
#'
#' This function accepts pre-configured config and priors objects, allowing:
#' \itemize{
#'   \item Completely custom configs (non-standard locations, custom data)
#'   \item Fine-grained control over all model parameters
#'   \item Testing with synthetic configurations
#' }
#'
#' Executes the full MOSAIC calibration workflow:
#' \enumerate{
#'   \item Adaptive calibration with R² convergence detection
#'   \item Single predictive batch (calculated from calibration phase)
#'   \item Adaptive fine-tuning with 5-tier batch sizing
#'   \item Post-hoc subset optimization
#'   \item Posterior quantile and distribution estimation
#'   \item Posterior predictive checks and uncertainty quantification
#' }
#'
#' @param config Named list of LASER model configuration (REQUIRED). Contains location_name,
#'   reported_cases, reported_deaths, and all model parameters. Create with custom data
#'   or obtain via \code{get_location_config()}.
#' @param priors Named list of prior distributions (REQUIRED). Contains distribution
#'   specifications for all parameters. Create custom or obtain via \code{get_location_priors()}.
#' @param dir_output Character. Output directory for this calibration run (REQUIRED).
#'   All results will be saved here. Must be unique per run.
#' @param control Control list created with [mosaic_control_defaults()]. If \code{NULL}, uses defaults.
#'   Controls calibration strategy, parameter sampling, parallelization, and output settings.
#'   Key settings:
#'   \itemize{
#'     \item \code{calibration$n_simulations}: NULL for auto mode, integer for fixed mode
#'     \item \code{calibration$n_iterations}: LASER iterations per simulation (default: 3)
#'     \item \code{calibration$max_simulations}: Maximum total simulations (default: 100000)
#'     \item \code{sampling}: Which parameters to sample vs hold fixed
#'     \item \code{parallel}: Cluster settings for parallel execution
#'   }
#' @param cluster Optional pre-built R parallel cluster from
#'   \code{\link{make_mosaic_cluster}}. Only used when \code{dask_spec = NULL}.
#'   When provided, skips cluster creation and teardown, reusing existing workers.
#'   Useful for staged estimation where multiple \code{run_MOSAIC} calls share a
#'   cluster. When \code{dask_spec} is provided this argument is not used; the
#'   caller retains ownership of any cluster passed and is responsible for stopping
#'   it after \code{run_MOSAIC} returns.
#' @param dask_spec Optional named list specifying a Dask/Coiled cluster. When
#'   provided, simulations are dispatched to Dask workers instead of a local R
#'   parallel cluster. Required fields: \code{type} ("coiled" or "scheduler").
#'   Coiled fields: \code{n_workers}, \code{software}, \code{workspace} (required
#'   for multi-workspace Coiled accounts), \code{vm_types}, \code{scheduler_vm_types},
#'   \code{region}, \code{idle_timeout}. Scheduler fields: \code{address}.
#'   Optional Coiled pass-through fields: \code{timeout}, \code{environ},
#'   \code{scheduler_disk_size}, \code{worker_disk_size}, \code{scheduler_options},
#'   \code{worker_options}, \code{spot_policy}, \code{host_setup_script}.
#'   When \code{NULL} (default) the local R parallel backend is used.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{dirs}{Named list of output directories}
#'   \item{files}{Named list of key output files}
#'   \item{summary}{Named list with run statistics (batches, sims, converged, runtime)}
#' }
#'
#' @section Control Structure:
#' See [mosaic_control_defaults()] for complete documentation. The control structure contains:
#' \describe{
#'   \item{calibration}{n_simulations, n_iterations, max_simulations, batch_size, etc.}
#'   \item{sampling}{sample_tau_i, sample_mobility_gamma, sample_mu_j, etc.}
#'   \item{parallel}{enable, n_cores, type, progress}
#'   \item{paths}{clean_output, plots}
#'   \item{targets}{ESS_param, ESS_best, A_best, CVw_best, etc.}
#'   \item{io}{format, compression, compression_level}
#' }
#'
#' @section Output Files:
#' Results are organized in a structured directory tree:
#' \itemize{
#'   \item \code{1_inputs/}: Configuration files (JSON format)
#'   \item \code{2_calibration/samples/}: Simulation results (Parquet format)
#'   \item \code{2_calibration/diagnostics/}: ESS metrics, convergence results
#'   \item \code{2_calibration/posterior/}: Posterior quantiles and distributions
#'   \item \code{3_results/figures/}: Diagnostic, parameter, and prediction plots
#'   \item \code{3_results/}: Final combined results
#' }
#'
#' @examples
#' \dontrun{
#' # === BASIC CUSTOM CONFIG ===
#'
#' # Load and modify default config
#' config <- get_location_config(iso = "ETH")
#' config$population_size <- 1000000
#'
#' priors <- get_location_priors(iso = "ETH")
#'
#' run_MOSAIC(
#'   config = config,
#'   priors = priors,
#'   dir_output = "./output"
#' )
#'
#' # === MULTI-LOCATION WITH CUSTOM CONTROL ===
#'
#' config <- get_location_config(iso = c("ETH", "KEN", "TZA"))
#' priors <- get_location_priors(iso = c("ETH", "KEN", "TZA"))
#'
#' ctrl <- mosaic_control_defaults(
#'   calibration = list(
#'     n_simulations = 5000,  # Fixed mode
#'     n_iterations = 5
#'   ),
#'   parallel = list(enable = TRUE, n_cores = 16)
#' )
#'
#' run_MOSAIC(config, priors, "./output", ctrl)
#'
#' # === CUSTOM PRIORS FOR SENSITIVITY ===
#'
#' config <- get_location_config(iso = "ETH")
#' priors <- get_location_priors(iso = "ETH")
#'
#' # Tighten transmission rate prior
#' priors$tau_i$shape <- 20
#' priors$tau_i$rate <- 4
#'
#' run_MOSAIC(config, priors, "./output")
#'
#' # === COMPLETELY CUSTOM CONFIG ===
#'
#' # Non-standard location names
#' custom_config <- list(
#'   location_name = c("Region1", "Region2"),
#'   reported_cases = my_cases_data,
#'   reported_deaths = my_deaths_data,
#'   # ... all other LASER parameters
#' )
#'
#' custom_priors <- list(
#'   # ... custom prior specifications
#' )
#'
#' run_MOSAIC(custom_config, custom_priors, "./output")
#' }
#'
#' @seealso [mosaic_control_defaults()] for building control structures
#' @export
run_MOSAIC <- function(config,
                       priors,
                       dir_output,
                       control   = NULL,
                       cluster   = NULL,
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

  # Validate config structure
  if (!"location_name" %in% names(config)) {
    stop("config must contain 'location_name' field", call. = FALSE)
  }

  # Extract iso_code from config for logging
  iso_code <- config$location_name

  # Determine backend: Dask if dask_spec provided, R parallel otherwise
  use_dask <- !is.null(dask_spec)

  # Normalise and validate dask_spec defaults
  if (use_dask) {
    if (!is.list(dask_spec)) {
      stop("dask_spec must be a named list or NULL", call. = FALSE)
    }
    dask_spec$type               <- dask_spec$type               %||% "coiled"
    dask_spec$software           <- dask_spec$software           %||% "mosaic-acr-workers"
    dask_spec$vm_types           <- dask_spec$vm_types           %||% c("Standard_D8s_v6")
    dask_spec$scheduler_vm_types <- dask_spec$scheduler_vm_types %||% c("Standard_D4s_v6")
    dask_spec$region             <- dask_spec$region             %||% "westus2"
    dask_spec$idle_timeout       <- dask_spec$idle_timeout       %||% "2 hours"
    # workspace: no default — NULL is valid for single-workspace Coiled accounts;
    # required for multi-workspace orgs. Validate below if type = "coiled".
    if (dask_spec$type == "coiled" && is.null(dask_spec$workspace)) {
      message("Note: dask_spec$workspace not set. If you have multiple Coiled workspaces, ",
              "set workspace = 'your-workspace-name' to avoid a Coiled runtime error.")
    }

    if (!dask_spec$type %in% c("coiled", "scheduler")) {
      stop("dask_spec$type must be 'coiled' or 'scheduler'", call. = FALSE)
    }
    if (dask_spec$type == "scheduler" &&
        (is.null(dask_spec$address) || !nzchar(dask_spec$address))) {
      stop("dask_spec$address must be set when type='scheduler'", call. = FALSE)
    }
  }

  # ===========================================================================
  # SETUP MOSAIC ROOT DIRECTORY
  # ===========================================================================

  # Check if root_directory is already set (from previous set_root_directory() call)
  root_dir <- getOption("root_directory")

  if (is.null(root_dir)) {
    stop(
      "MOSAIC root directory not set. Please set once per session:\n",
      "  set_root_directory('/path/to/MOSAIC')\n",
      "Or set the option directly:\n",
      "  options(root_directory = '/path/to/MOSAIC')\n",
      "The root should contain MOSAIC-pkg/, MOSAIC-data/, etc.",
      call. = FALSE
    )
  }

  if (!dir.exists(root_dir)) {
    stop("MOSAIC root directory does not exist: ", root_dir, call. = FALSE)
  }

  # Validate critical subdirectories exist
  required_dirs <- c("MOSAIC-pkg", "MOSAIC-data")
  for (d in required_dirs) {
    if (!dir.exists(file.path(root_dir, d))) {
      warning("Expected directory not found: ", file.path(root_dir, d), call. = FALSE)
    }
  }

  # ===========================================================================
  # LOAD CONTROL DEFAULTS AND EXTRACT PARAMETERS
  # ===========================================================================

  log_msg("Using user-provided config for: %s", paste(iso_code, collapse = ", "))
  .mosaic_validate_config(config, iso_code)

  log_msg("Using user-provided priors")
  .mosaic_validate_priors(priors, config)

  # Load control defaults if not provided
  if (is.null(control)) {
    control <- mosaic_control_defaults()
  }

  # Merge and validate control
  control <- .mosaic_validate_and_merge_control(control)

  # Extract parameters from control structure
  n_iterations <- control$calibration$n_iterations
  n_simulations <- control$calibration$n_simulations
  sampling_args <- control$sampling

  # Validate extracted parameters
  if (!is.numeric(n_iterations) || n_iterations < 1) {
    stop("control$calibration$n_iterations must be a positive integer", call. = FALSE)
  }
  if (!is.null(n_simulations) && !is.character(n_simulations) && (!is.numeric(n_simulations) || n_simulations < 1)) {
    stop("control$calibration$n_simulations must be NULL, 'auto', or a positive integer", call. = FALSE)
  }

  # Validate sampling_args
  sampling_args <- .mosaic_validate_sampling_args(sampling_args)

  # ===========================================================================
  # PYTHON ENVIRONMENT CHECK
  # ===========================================================================

  check_python_env()
  Sys.setenv(PYTHONWARNINGS = "ignore::UserWarning")

  # ===========================================================================
  # BLAS THREAD CONTROL CHECK (Critical for cluster performance)
  # ===========================================================================

  .mosaic_check_blas_control()

  # ===========================================================================
  # SETUP PATHS
  log_msg("Starting MOSAIC calibration")

  # Set root directory and get paths
  set_root_directory(root_dir)
  PATHS <- get_paths()

  # Create directory structure
  dirs <- .mosaic_ensure_dir_tree(
    dir_output = dir_output,
    clean_output = isTRUE(control$paths$clean_output)
  )

  # Conditionally create simresults directory for validation output
  save_simresults <- isTRUE(control$io$save_simresults)
  if (save_simresults) {
    dirs$cal_simresults <- file.path(dir_output, "2_calibration/simulation_results")
    dir.create(dirs$cal_simresults, recursive = TRUE, showWarnings = FALSE)
  }

  # ===========================================================================
  # WRITE SETUP FILES (with cluster metadata)
  # ===========================================================================

  # Capture full environment snapshot (versions, system, git, data)
  env_snapshot <- .mosaic_capture_environment(
    config = config, priors = priors, control = control
  )
  .mosaic_write_json(env_snapshot, file.path(dirs$inputs, "environment.json"), control$io)
  log_msg("  Saved %s", "1_inputs/environment.json")

  sim_params <- list(
    control = control,
    n_iterations = n_iterations,
    iso_code = iso_code,
    timestamp = Sys.time(),
    paths = list(
      dir_output = dirs$root,
      dir_inputs = dirs$inputs,
      dir_calibration = dirs$calibration,
      dir_results = dirs$results
    )
  )

  log_msg("Writing setup files...")
  .mosaic_write_json(sim_params, file.path(dirs$inputs, "control.json"), control$io)
  log_msg("  Saved %s", "control.json")

  .mosaic_write_json(priors, file.path(dirs$inputs, "priors.json"), control$io)
  log_msg("  Saved %s", "priors.json")

  .mosaic_write_json(config, file.path(dirs$inputs, "config.json"), control$io)
  log_msg("  Saved %s", "config.json")

  # ===========================================================================
  # PARAMETER NAME DETECTION
  # ===========================================================================

  tmp <- convert_config_to_matrix(config)
  if ("seed" %in% names(tmp)) tmp <- tmp[names(tmp) != "seed"]
  param_names_all <- names(tmp)
  rm(tmp)

  # Precompute lookup for fast parameter extraction in worker hot loop
  param_lookup <- .mosaic_build_param_lookup(param_names_all, config$location_name)

  # Filter to estimated parameters for ESS tracking using metadata table.
  # Uses the `scale` column ("global" vs "location") from estimated_parameters
  # to determine which params have ISO suffixes, instead of fragile regex.
  est_params_df <- get("estimated_parameters", envir = asNamespace("MOSAIC"))
  if (!is.data.frame(est_params_df) || nrow(est_params_df) == 0) {
    stop("Failed to load MOSAIC::estimated_parameters")
  }

  estimable_global   <- est_params_df$parameter_name[est_params_df$scale == "global"]
  estimable_location <- est_params_df$parameter_name[est_params_df$scale == "location"]

  # Derive base name for each param: direct match for globals,
  # strip known location suffixes (_ISO) for location-specific params
  location_names <- config$location_name
  location_suffixes <- paste0("_", location_names)
  param_base <- param_names_all
  for (sfx in location_suffixes) {
    hits <- endsWith(param_names_all, sfx)
    param_base[hits] <- sub(paste0(sfx, "$"), "", param_names_all[hits])
  }

  is_estimable <- param_base %in% c(estimable_global, estimable_location)
  param_names_estimated <- param_names_all[is_estimable]
  # Exclude initial condition counts (proportions are estimable, counts are derived)
  param_names_estimated <- param_names_estimated[!grepl("^[NSEIRV][12]?_j_initial", param_names_estimated)]

  if (!length(param_names_estimated)) {
    stop("No estimated parameters found for ESS tracking")
  }

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
          paste(config$location_name, collapse = ', '))

  # ===========================================================================
  # CLUSTER INITIALISATION
  # ===========================================================================

  # Resolve and normalize weights_time into the private slot used by all workers.
  # Normalize to sum to n_t (number of time points) so the scale is independent
  # of user-supplied magnitude. Applied once here for both R and Dask paths;
  # the Dask path also has its own copy of this logic for the likelihood_settings
  # local variable it builds — that redundant normalization is harmless.
  if (!is.null(control$likelihood$weights_time)) {
    n_t_cfg <- if (is.matrix(config$reported_cases)) ncol(config$reported_cases) else
                 length(config$reported_cases)
    wt_raw <- control$likelihood$weights_time
    if (length(wt_raw) == 1L) wt_raw <- rep(wt_raw, n_t_cfg)
    control$likelihood$.weights_time_resolved <- wt_raw / sum(wt_raw) * n_t_cfg
  } else {
    control$likelihood$.weights_time_resolved <- NULL
  }

  # Initialise Dask-related variables to NULL so the on.exit handler is always safe.
  # Never alias the caller-provided R cluster here — if dask_spec is provided the
  # caller retains full ownership of any R cluster they passed; it is not used and
  # not touched by the Dask code path.
  client      <- NULL
  dask_cluster <- NULL  # Dask/Coiled cluster object (distinct from R cluster arg)

  if (use_dask) {

    # --- Dask / Coiled cluster ---

    # Force sequential if parallel is disabled (n_cores controls n_workers default)
    if (!isTRUE(control$parallel$enable)) {
      control$parallel$n_cores <- 1L
    }

    dask_dist <- reticulate::import("dask.distributed")

    if (dask_spec$type == "coiled") {
      coiled_mod <- reticulate::import("coiled")
      n_workers_req <- dask_spec$n_workers %||% control$parallel$n_cores

      log_msg("Creating Coiled cluster: %d workers (%s, %s)",
              n_workers_req, dask_spec$vm_types[1], dask_spec$region)

      cluster_name <- paste0("mosaic-dask-",
                             format(Sys.time(), "%Y%m%d-%H%M%S"))

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

      # Cluster creation with retry — transient Azure provisioning failures
      # (e.g. "Timed out waiting for process to phone home") are common and
      # resolve on retry without any code change needed.
      max_cluster_attempts <- 3L
      cluster_attempt      <- 0L
      repeat {
        cluster_attempt <- cluster_attempt + 1L
        tryCatch({
          dask_cluster <- do.call(coiled_mod$Cluster, cluster_args)
          client       <- dask_dist$Client(dask_cluster)
          break   # success
        }, error = function(e) {
          try({ if (!is.null(dask_cluster)) dask_cluster$close() }, silent = TRUE)
          dask_cluster <<- NULL
          if (cluster_attempt >= max_cluster_attempts)
            stop("Coiled cluster creation failed after ", max_cluster_attempts,
                 " attempts: ", e$message, call. = FALSE)
          wait_secs <- 30L * cluster_attempt
          log_msg("Cluster creation attempt %d/%d failed: %s",
                  cluster_attempt, max_cluster_attempts, e$message)
          log_msg("  Retrying in %ds...", wait_secs)
          Sys.sleep(wait_secs)
        })
      }
    } else {
      log_msg("Connecting to Dask scheduler: %s", dask_spec$address)
      client <- dask_dist$Client(dask_spec$address)
    }

    log_msg("Dask dashboard: %s", client$dashboard_link)

    # Register cleanup (runs on normal exit OR error).
    # client/dask_cluster are set to NULL after graceful close before post-processing,
    # so this only fires if an error occurs during the batch loop.
    on.exit({
      if (!is.null(client)) {
        try({ client$close(); log_msg("Dask client closed (on.exit)") }, silent = TRUE)
      }
      if (!is.null(dask_cluster)) {
        try({ dask_cluster$close(); log_msg("Dask cluster closed (on.exit)") }, silent = TRUE)
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
    reticulate::py_run_string(paste0(
      "import sys\n",
      "_mw_dir = '", dirname(worker_py_path), "'\n",
      "if _mw_dir not in sys.path:\n",
      "    sys.path.insert(0, _mw_dir)"
    ))
    mosaic_worker <- reticulate::import("mosaic_dask_worker")

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

    # Scatter base config (matrices + metadata) to all workers ONCE
    log_msg("Broadcasting base config to workers...")
    base_config_py     <- reticulate::r_to_py(.extract_base_config(config))
    base_config_future <- client$scatter(base_config_py, broadcast = TRUE)
    log_msg("  Base config broadcast complete")

    # cl is not used in Dask mode
    cl <- NULL

  } else {

    # --- Local R parallel cluster ---

    # Force sequential if parallel is disabled
    if (!isTRUE(control$parallel$enable)) {
      control$parallel$n_cores <- 1L
    }

    # Determine cluster ownership:
    #   owns_cluster = TRUE  → we created it, we tear it down
    #   owns_cluster = FALSE → caller provided it, caller tears it down
    owns_cluster <- FALSE

    if (!is.null(cluster)) {
      # User provided a pre-built cluster (e.g. from make_mosaic_cluster)
      cl <- cluster
      owns_cluster <- FALSE
      log_msg("Using pre-built cluster (%d workers)", length(cl))
    } else if (control$parallel$n_cores > 1L) {
      # Create cluster internally via make_mosaic_cluster
      cl <- make_mosaic_cluster(
        n_cores = control$parallel$n_cores,
        type = control$parallel$type
      )
      owns_cluster <- TRUE
      log_msg("Created %s cluster with %d cores", control$parallel$type, control$parallel$n_cores)

      # Register cleanup handler (will be called on normal exit or error)
      on.exit({
        if (owns_cluster && !is.null(cl)) {
          try({
            parallel::stopCluster(cl)
            log_msg("Cluster stopped successfully")
          }, silent = TRUE)
        }
      }, add = TRUE)
    } else {
      log_msg("Running sequentially (n_cores = 1)")
      cl <- NULL
    }

    # Export per-run data to workers (needed every run, even with reused cluster)
    if (!is.null(cl)) {
      likelihood_settings <- control$likelihood
      io_settings <- control$io

      parallel::clusterExport(cl,
        c("n_iterations", "priors", "config", "PATHS", "param_names_all", "param_lookup",
          "sampling_args", "dirs",
          "likelihood_settings", "io_settings"),
        envir = environment())

      # Install worker function using the exported per-run variables
      parallel::clusterCall(cl, function() {
        assign(".run_sim_worker", function(sim_id) {
          MOSAIC:::.mosaic_run_simulation_worker(
            sim_id = sim_id,
            n_iterations = n_iterations,
            priors = priors,
            config = config,
            PATHS = PATHS,
            dir_cal_samples = dirs$cal_samples,
            dir_cal_simresults = dirs$cal_simresults,
            param_names_all = param_names_all,
            param_lookup = param_lookup,
            sampling_args = sampling_args,
            io = io_settings,
            likelihood_settings = likelihood_settings
          )
        }, envir = .GlobalEnv)
        NULL
      })
    }

    # Dask variables not used in R parallel mode
    mosaic_worker      <- NULL
    base_config_future <- NULL

  }

  # ===========================================================================
  # DETERMINE RUN MODE: AUTO vs FIXED (with safe state loading)
  # ===========================================================================

  nspec <- .mosaic_normalize_n_sims(n_simulations)
  state_file <- file.path(dirs$cal_state, "run_state.json")

  state <- .mosaic_init_state(control, param_names_sampled, nspec)

  log_msg("Starting simulation (mode: %s)", state$mode)
  start_time <- Sys.time()

  # Disk space check removed in v0.17.10 — fragile across platforms (macOS df
  # output parsing) and low value on local machines. Cluster deployments should
  # check quotas at the job scheduler level, not inside R.

  # ===========================================================================
  # SIMULATION: FIXED MODE
  # ===========================================================================

  if (identical(state$mode, "fixed")) {

    target <- state$fixed_target

    log_msg("[FIXED MODE] Running exactly %d simulations", target)

    all_ids <- seq_len(target)
    done_ids <- integer()
    sim_ids <- setdiff(all_ids, done_ids)

    if (length(sim_ids) == 0L) {
      log_msg("Nothing to do: %d simulations already complete", length(done_ids))
    } else {
      log_msg("Running %d simulations (%d-%d)", length(sim_ids), min(sim_ids), max(sim_ids))

      batch_start_time <- Sys.time()

      # Dispatch batch: Dask or R parallel/sequential
      success_indicators <- if (use_dask) {
        .mosaic_run_batch_dask(
          sim_ids             = sim_ids,
          n_iterations        = n_iterations,
          priors              = priors,
          config              = config,
          PATHS               = PATHS,
          sampling_args       = sampling_args,
          dirs                = dirs,
          param_names_all     = param_names_all,
          control             = control,
          likelihood_settings = likelihood_settings,
          client              = client,
          base_config_future  = base_config_future,
          mosaic_worker       = mosaic_worker
        )
      } else if (!is.null(cl)) {
        # Parallel: use worker function defined on cluster
        .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .run_sim_worker(sim_id),
          cl = cl,
          show_progress = control$parallel$progress
        )
      } else {
        # Sequential: define inline (use named args to avoid positional shift)
        .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .mosaic_run_simulation_worker(
            sim_id             = sim_id,
            n_iterations       = n_iterations,
            priors             = priors,
            config             = config,
            PATHS              = PATHS,
            dir_cal_samples    = dirs$cal_samples,
            dir_cal_simresults = dirs$cal_simresults,
            param_names_all    = param_names_all,
            param_lookup       = param_lookup,
            sampling_args      = sampling_args,
            io                 = control$io,
            likelihood_settings = control$likelihood
          ),
          cl = cl,
          show_progress = control$parallel$progress
        )
      }

      n_success_batch <- sum(unlist(success_indicators))
      state$total_sims_successful <- state$total_sims_successful + n_success_batch
      state$batch_success_rates <- c(state$batch_success_rates, (n_success_batch / length(sim_ids)) * 100)
      state$batch_sizes_used <- c(state$batch_sizes_used, length(sim_ids))
      state$batch_number <- state$batch_number + 1L
      state$total_sims_run <- length(all_ids)

      batch_runtime <- difftime(Sys.time(), batch_start_time, units = "mins")
      log_msg("Fixed batch complete: %d/%d successful (%.1f%%) in %.1f minutes",
              n_success_batch, length(sim_ids), tail(state$batch_success_rates, 1),
              as.numeric(batch_runtime))

      .mosaic_save_state(state, state_file)
    }

  # ===========================================================================
  # SIMULATION: AUTO MODE (ADAPTIVE BATCHING)
  # ===========================================================================

  } else {

    log_msg("Phase 1: Adaptive Calibration")
    log_msg("  - Run %d-%d batches × %d sims (R² target: %.2f)",
            control$calibration$min_batches, control$calibration$max_batches,
            control$calibration$batch_size, control$calibration$target_r2)
    log_msg("Phase 2: Single Predictive Batch")
    log_msg("Phase 3: Adaptive Fine-tuning (5-tier)")
    log_msg("Target ESS: %d per parameter | Max simulations: %d",
            control$targets$ESS_param, control$calibration$max_simulations)

    repeat {
      if (state$converged || state$total_sims_run >= control$calibration$max_simulations) break

      # Decide next batch
      decision <- .mosaic_decide_next_batch(state, control, state$ess_tracking)
      current_phase <- decision$phase
      current_batch_size <- decision$batch_size

      # Update phase from decision
      if (!identical(state$phase, current_phase)) {
        old_phase <- state$phase
        state$phase <- current_phase
        # Reset batch counter for new phase
        state$phase_batch_count <- 0L
        state$phase_last <- current_phase
        log_msg("  ✓ Phase transition: %s → %s", toupper(old_phase), toupper(state$phase))
      }

      # If batch size is 0, ESS target already met — save state before breaking
      if (current_batch_size <= 0) {
        log_msg("No additional simulations needed (batch_size = 0)")
        .mosaic_save_state(state, state_file)
        break
      }

      # Increment phase batch counter (tracks batches within current phase)
      state$phase_batch_count <- state$phase_batch_count + 1L

      batch_start <- state$total_sims_run + 1L
      batch_end <- state$total_sims_run + current_batch_size

      # BUG FIX #4: Enforce reserved simulations for fine-tuning phase
      if (identical(current_phase, "predictive")) {
        # Leave 250 simulations reserved for fine-tuning
        reserved_sims <- 250L
        max_for_predictive <- control$calibration$max_simulations - reserved_sims
        if (batch_end > max_for_predictive) {
          batch_end <- max_for_predictive
          log_msg("Capping predictive batch to leave %d reserved for fine-tuning", reserved_sims)
        }
      }

      # Final cap at max_simulations
      batch_end <- min(batch_end, control$calibration$max_simulations)
      sim_ids <- batch_start:batch_end

      log_msg(paste(rep("-", 60), collapse = ""))
      log_msg("[%s] Batch %d: Running %d simulations (%d-%d)",
              toupper(current_phase), state$batch_number + 1L,
              length(sim_ids), min(sim_ids), max(sim_ids))

      # Run batch
      batch_start_time <- Sys.time()

      # Dispatch batch: Dask or R parallel/sequential
      success_indicators <- if (use_dask) {
        .mosaic_run_batch_dask(
          sim_ids             = sim_ids,
          n_iterations        = n_iterations,
          priors              = priors,
          config              = config,
          PATHS               = PATHS,
          sampling_args       = sampling_args,
          dirs                = dirs,
          param_names_all     = param_names_all,
          control             = control,
          likelihood_settings = likelihood_settings,
          client              = client,
          base_config_future  = base_config_future,
          mosaic_worker       = mosaic_worker
        )
      } else if (!is.null(cl)) {
        # Parallel: use worker function defined on cluster
        .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .run_sim_worker(sim_id),
          cl = cl,
          show_progress = control$parallel$progress
        )
      } else {
        # Sequential: define inline (use named args to avoid positional shift)
        .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .mosaic_run_simulation_worker(
            sim_id             = sim_id,
            n_iterations       = n_iterations,
            priors             = priors,
            config             = config,
            PATHS              = PATHS,
            dir_cal_samples    = dirs$cal_samples,
            dir_cal_simresults = dirs$cal_simresults,
            param_names_all    = param_names_all,
            param_lookup       = param_lookup,
            sampling_args      = sampling_args,
            io                 = control$io,
            likelihood_settings = control$likelihood
          ),
          cl = cl,
          show_progress = control$parallel$progress
        )
      }

      n_success_batch <- sum(unlist(success_indicators))
      batch_success_rate <- (n_success_batch / length(sim_ids)) * 100

      state$total_sims_successful <- state$total_sims_successful + n_success_batch
      state$batch_success_rates <- c(state$batch_success_rates, batch_success_rate)
      state$batch_sizes_used <- c(state$batch_sizes_used, length(sim_ids))
      state$batch_number <- state$batch_number + 1L
      state$total_sims_run <- batch_end

      batch_runtime <- difftime(Sys.time(), batch_start_time, units = "mins")
      log_msg("Batch %d complete: %d/%d successful (%.1f%%) in %.1f minutes",
              state$batch_number, n_success_batch, length(sim_ids),
              batch_success_rate, as.numeric(batch_runtime))

      # ESS convergence check — always run so state$converged is accurate.
      # The old guard skipped this when total_sims_run >= max_simulations,
      # which meant the final batch never updated the converged flag and
      # always emitted a spurious "no convergence" warning even when the
      # target was met on that last batch.
      state <- .mosaic_ess_check_update_state(state, dirs, param_names_sampled, control)
      .mosaic_save_state(state, state_file)

      if (state$total_sims_run >= control$calibration$max_simulations && !state$converged) {
        log_msg("WARNING: Reached maximum simulations (%d) without convergence",
                control$calibration$max_simulations)
        break
      }
    }
  }

  # Stop R cluster only if we created it (not if caller provided it)
  if (!use_dask && exists("owns_cluster") && owns_cluster && !is.null(cl)) {
    parallel::stopCluster(cl)
    cl <- NULL
  }

  # Disconnect Dask client before R-heavy post-processing to prevent Python
  # event loop starvation / TLS heartbeat timeouts. The cluster object stays
  # alive (holds TLS context) so we can reconnect for post-cal ensemble sims.
  if (use_dask && !is.null(client)) {
    log_msg("Disconnecting Dask client (cluster stays alive for post-cal reconnect)")
    tryCatch({
      client$close()
      log_msg("  Client disconnected")
    }, error = function(e) {
      log_msg("  Dask client disconnect failed: %s", e$message)
    })
    client <- NULL
    gc(verbose = FALSE)
  }

  # ===========================================================================
  # COMBINE RESULTS AND ADD FLAGS
  log_msg("Combining simulation files")

  # Get list of simulation files
  parquet_files <- list.files(dirs$cal_samples, pattern = "^sim_.*\\.parquet$", full.names = TRUE)

  # Load and combine all simulation files
  # Default: streaming (memory-safe for large runs)
  # Override via control$io$load_method if needed
  load_method <- if (!is.null(control$io$load_method)) {
    control$io$load_method
  } else {
    "streaming"  # Safe default
  }

  load_chunk_size <- control$io$load_chunk_size %||% 5000L

  results <- .mosaic_load_and_combine_results(
    dir_params = dirs$cal_samples,
    method = load_method,
    chunk_size = load_chunk_size,
    verbose = TRUE
  )

  # Add index columns
  log_msg("Adding index columns to results matrix...")
  results$is_finite <- is.finite(results$likelihood) & !is.na(results$likelihood)
  results$is_valid <- results$is_finite & results$likelihood != -999999999
  results$is_outlier <- FALSE

  if (sum(results$is_valid) > 0) {
    valid_ll <- results$likelihood[results$is_valid]
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

  results$is_retained <- results$is_valid & !results$is_outlier
  results$is_best_subset <- FALSE
  results$is_best_model <- FALSE

  if (any(results$is_valid)) {
    results$is_best_model[which.max(results$likelihood)] <- TRUE
  }

  # Write combined simulations and clean up shards
  simulations_file <- file.path(dirs$calibration, "samples.parquet")
  .mosaic_write_parquet(results, simulations_file, control$io)

  # Delete individual simulation files after combining
  if (length(parquet_files) > 0) {
    unlink(parquet_files)
    log_msg("  Cleaned up %d individual simulation files", length(parquet_files))
  }

  log_msg("  Combined simulations saved: %s", basename(simulations_file))

  # ===========================================================================
  # PARAMETER-SPECIFIC ESS
  # ===========================================================================

  log_msg("Calculating parameter ESS")
  ess_results <- calc_model_ess_parameter(
    results = results,
    param_names = param_names_sampled,
    likelihood_col = "likelihood",
    n_grid = 100,
    method = control$targets$ESS_method,
    marginal_method = control$targets$ESS_marginal_method %||% "kde",
    verbose = control$logging$verbose
  )

  ess_file <- file.path(dirs$cal_diag, "parameter_ess.csv")
  write.csv(ess_results, ess_file, row.names = FALSE)
  log_msg("Saved %s", ess_file)

  log_msg("Optimizing subset selection (testing up to 30 tiers with grid search)...")

  subset_tiers <- get_default_subset_tiers(
    target_ESS_best = control$targets$ESS_best,
    target_A = control$targets$A_best,
    target_CVw = control$targets$CVw_best
  )

  optimal_subset_result <- NULL
  tier_used <- NULL

  for (tier_name in names(subset_tiers)) {
    tier <- subset_tiers[[tier_name]]

    log_msg("  Testing tier '%s' (ESS_B=%.0f, A=%.2f, CVw=%.2f)...",
            tier$name, tier$ESS_B, tier$A, tier$CVw)

    tier_result <- grid_search_best_subset(
      results = results[results$is_retained, ],
      target_ESS = tier$ESS_B,
      target_A = tier$A,
      target_CVw = tier$CVw,
      min_size = control$targets$min_best_subset,
      max_size = control$targets$max_best_subset,
      ess_method = control$targets$ESS_method,
      verbose = control$logging$verbose
    )

    if (tier_result$converged) {
      # CRITICAL: Calculate percentile from absolute count
      tier_percentile <- (tier_result$n / nrow(results)) * 100
      log_msg("    ✓ Tier '%s' converged at n=%d (%.1f%% of retained)",
              tier$name, tier_result$n, tier_percentile)
      optimal_subset_result <- tier_result
      tier_used <- tier$name
      rm(tier)
      break
    } else {
      log_msg("    ✗ Tier '%s' failed to converge", tier$name)
      rm(tier_result)
      rm(tier)
    }
  }

  if (exists("subset_tiers")) rm(subset_tiers)
  gc(verbose = FALSE)

  if (!is.null(optimal_subset_result)) {
    # CRITICAL: Use $n (not $n_selected) and calculate percentile from count
    top_subset_final <- optimal_subset_result$subset
    n_top_final <- optimal_subset_result$n
    percentile_used <- (n_top_final / nrow(results)) * 100
    convergence_tier <- tier_used

  } else {
    # Fallback: use max_best_subset directly (from retained only)
    retained_for_fallback <- results[results$is_retained, ]
    n_top_final <- min(control$targets$max_best_subset, nrow(retained_for_fallback))
    results_ranked_final <- retained_for_fallback[order(retained_for_fallback$likelihood, decreasing = TRUE), ]
    top_subset_final <- results_ranked_final[1:n_top_final, ]
    rm(results_ranked_final, retained_for_fallback)

    percentile_used <- (n_top_final / nrow(results)) * 100
    convergence_tier <- "fallback"

    log_msg("  All tiers failed - using fallback (top %d simulations, %.1f%%)",
            n_top_final, percentile_used)

    # Create minimal optimal_subset_result for consistency
    optimal_subset_result <- list(
      subset = top_subset_final,
      n = n_top_final,
      converged = FALSE,
      metrics = list(ESS = NA_real_, A = NA_real_, CVw = NA_real_)
    )
  }

  # Check if we have sufficient valid data for metric calculation
  n_valid_final <- sum(is.finite(top_subset_final$likelihood))

  if (n_valid_final < 2) {
    # Insufficient data for metrics - set to NA
    log_msg("\n⚠ WARNING: Insufficient valid simulations (%d) in final subset", n_valid_final)
    log_msg("  Cannot calculate convergence metrics. Setting to NA.")

    ESS_B_final <- NA_real_
    A_final <- NA_real_
    CVw_final <- NA_real_
    gibbs_temperature_final <- 1

  } else {
    # Calculate final metrics for best subset
    aic_final <- -2 * top_subset_final$likelihood
    best_aic_final <- min(aic_final[is.finite(aic_final)])

    if (!is.finite(best_aic_final)) {
      # All AIC values are non-finite
      log_msg("\n⚠ WARNING: No finite AIC values in final subset")
      ESS_B_final <- NA_real_
      A_final <- NA_real_
      CVw_final <- NA_real_
      gibbs_temperature_final <- 1
    } else {
      # Use truncated Akaike weights with fixed effective range for best subset
      # Effective AIC = 4 for best subset (5% threshold)
      aic_final <- -2 * top_subset_final$likelihood
      best_aic_final <- min(aic_final[is.finite(aic_final)])
      delta_aic_final <- aic_final - best_aic_final

      # Truncate to effective range
      effective_range_best <- 4.0
      delta_aic_truncated <- pmin(delta_aic_final, effective_range_best)

      # Calculate standard Akaike weights: w ∝ exp(-0.5 * delta_aic)
      gibbs_temperature_final <- 0.5  # Standard for Akaike weights
      weights_final <- calc_model_weights_gibbs(
        x = delta_aic_truncated,
        temperature = gibbs_temperature_final,
        verbose = FALSE
      )

      w_tilde_final <- weights_final
      w_final <- weights_final * length(weights_final)

      # Calculate metrics (use control ESS_method)
      ESS_B_final <- calc_model_ess(w_tilde_final, method = control$targets$ESS_method)
      ag_final <- calc_model_agreement_index(w_final)
      A_final <- ag_final$A
      CVw_final <- calc_model_cvw(w_final)

      actual_range_final <- diff(range(delta_aic_final[is.finite(delta_aic_final)]))
      log_msg("  Subset selection weights (truncated Akaike, effective range = %.1f):", effective_range_best)
      log_msg("    Actual delta AIC range: %.1f", actual_range_final)
      log_msg("    Temperature: %.4f (standard Akaike)", gibbs_temperature_final)
    }
  }

  log_msg("\nFinal subset (%s): %.1f%% (n=%d) | ESS_B=%s, A=%s, CVw=%s, T=%.4f",
          convergence_tier, percentile_used, n_top_final,
          if(is.finite(ESS_B_final)) sprintf("%.1f", ESS_B_final) else "NA",
          if(is.finite(A_final)) sprintf("%.3f", A_final) else "NA",
          if(is.finite(CVw_final)) sprintf("%.3f", CVw_final) else "NA",
          gibbs_temperature_final)

  # Check convergence based on tier used
  if (convergence_tier != "fallback") {
    log_msg("\n✓ Post-hoc optimization succeeded with %s criteria", convergence_tier)
    log_msg("  → Using %.1f%% of simulations (%d total)",
            percentile_used, n_top_final)
    final_converged <- TRUE
  } else {
    log_msg("\n⚠ Post-hoc optimization: No tier converged, using fallback")
    log_msg("  Consider running more simulations or adjusting tier criteria")
    final_converged <- FALSE
  }

  # Update best subset column
  results$is_best_subset <- FALSE
  results$is_best_subset[results$sim %in% top_subset_final$sim] <- TRUE

  log_msg("Calculating weights for %d simulations...", nrow(results))

  results$weight_all <- 0
  results$weight_retained <- 0
  results$weight_best <- 0

  if (sum(results$is_valid) > 0) {
    log_msg("  Computing adaptive Gibbs weights (all valid: n=%d)...", sum(results$is_valid))
    all_result <- .mosaic_calc_adaptive_gibbs_weights(
      likelihood = results$likelihood[results$is_valid],
      weight_floor = control$weights$floor,
      verbose = control$logging$verbose
    )
    results$weight_all[results$is_valid] <- all_result$weights
    log_msg("    ESS (all): %.1f", all_result$metrics$ESS_perplexity)
    rm(all_result)
  }

  if (sum(results$is_retained) > 0) {
    log_msg("  Computing Akaike weights (retained subset: n=%d)...", sum(results$is_retained))
    # Truncated Akaike weights for retained subset (effective AIC = 25)
    aic_retained <- -2 * results$likelihood[results$is_retained]
    best_aic_retained <- min(aic_retained[is.finite(aic_retained)])
    delta_aic_retained <- aic_retained - best_aic_retained
    delta_aic_retained_trunc <- pmin(delta_aic_retained, 25.0)

    retained_weights <- calc_model_weights_gibbs(
      x = delta_aic_retained_trunc,
      temperature = 0.5,
      verbose = control$logging$verbose
    )
    results$weight_retained[results$is_retained] <- retained_weights
    ESS_retained <- calc_model_ess(retained_weights, method = control$targets$ESS_method)
    log_msg("    ESS (retained): %.1f", ESS_retained)
  }

  if (sum(results$is_best_subset) > 0) {
    log_msg("  Computing Akaike weights (best subset: n=%d)...", sum(results$is_best_subset))
    # Truncated Akaike weights for best subset (effective AIC = 4)
    aic_best <- -2 * results$likelihood[results$is_best_subset]
    best_aic_best <- min(aic_best[is.finite(aic_best)])
    delta_aic_best <- aic_best - best_aic_best
    delta_aic_best_trunc <- pmin(delta_aic_best, 4.0)

    best_weights <- calc_model_weights_gibbs(
      x = delta_aic_best_trunc,
      temperature = 0.5,
      verbose = control$logging$verbose
    )
    results$weight_best[results$is_best_subset] <- best_weights

    # Calculate ESS for reference (using control method)
    ESS_best <- calc_model_ess(best_weights, method = control$targets$ESS_method)
    log_msg("    ESS (best): %.1f", ESS_best)
  }

  log_msg("Weight calculation complete")

  gc(verbose = FALSE)

  subset_summary <- data.frame(
    # Absolute count-based fields (new)
    min_search_size = control$targets$min_best_subset,
    max_search_size = control$targets$max_best_subset,
    optimal_size = n_top_final,

    # Percentile-based fields (backward compatibility)
    optimal_percentile = percentile_used,

    # Standard fields
    optimization_tier = convergence_tier,
    optimization_method = "grid_search",
    n_selected = n_top_final,
    ESS_B = ESS_B_final,
    A = A_final,
    CVw = CVw_final,
    gibbs_temperature = gibbs_temperature_final,
    meets_all_criteria = final_converged,
    timestamp = Sys.time()
  )
  summary_file <- file.path(dirs$cal_diag, "subset_selection_summary.csv")
  write.csv(subset_summary, summary_file, row.names = FALSE)
  log_msg("Saved %s", summary_file)

  .mosaic_write_parquet(results, simulations_file, control$io)
  log_msg("Saved %s", simulations_file)

  log_msg("Calculating convergence diagnostics")

  # Calculate metrics for retained models
  retained_results <- results[results$is_retained, ]
  n_retained <- nrow(retained_results)

  # Best subset metrics already calculated above
  best_results <- results[results$is_best_subset, ]
  n_best <- nrow(best_results)

  # Calculate convergence diagnostics
  # (Function has its own validation - will fail with clear error if inputs invalid)
  diagnostics <- calc_convergence_diagnostics(
    # Metrics
    n_total = nrow(results),
    n_successful = sum(is.finite(results$likelihood)),
    n_retained = n_retained,
    n_best_subset = n_best,
    ess_best = ESS_B_final,
    A_best = A_final,
    cvw_best = CVw_final,
    percentile_used = percentile_used,
    convergence_tier = convergence_tier,
    param_ess_results = ess_results,

    # Targets
    target_ess_best = control$targets$ESS_best,
    target_A_best = control$targets$A_best,
    target_cvw_best = control$targets$CVw_best,
    target_max_best_subset = control$targets$max_best_subset,
    target_ess_param = control$targets$ESS_param,
    target_ess_param_prop = control$targets$ESS_param_prop,

    # Settings
    ess_method = control$targets$ESS_method,
    temperature = gibbs_temperature_final,
    verbose = TRUE
  )

  # Save convergence results (parquet) - FIXED: Issue 1.2 (safe min)
  best_aic_val <- .mosaic_safe_min(-2 * results$likelihood[is.finite(results$likelihood)])

  convergence_results_df <- data.frame(
    sim = results$sim,
    seed = results$seed_sim,
    likelihood = results$likelihood,
    aic = -2 * results$likelihood,
    delta_aic = if (is.finite(best_aic_val)) {
      -2 * results$likelihood - best_aic_val
    } else {
      rep(NA_real_, nrow(results))
    },
    # w_tilde: normalized importance weights (sum to 1 over best subset)
    # w: unnormalized weights scaled to n (w = w_tilde * n, for ESS calcs)
    w_tilde = results$weight_best,
    w = results$weight_best * nrow(results),
    retained = results$is_best_subset,
    # Additional columns for two-tier structure
    w_retained = results$weight_retained,
    is_retained = results$is_retained,
    is_best_subset = results$is_best_subset
  )

  convergence_file <- file.path(dirs$cal_diag, "convergence_results.parquet")
  .mosaic_write_parquet(convergence_results_df, convergence_file, control$io)
  log_msg("Saved %s", convergence_file)

  diagnostics_file <- file.path(dirs$cal_diag, "convergence_diagnostics.json")
  jsonlite::write_json(diagnostics, diagnostics_file, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  log_msg("Saved %s", diagnostics_file)

  if (control$paths$plots) {
    log_msg("Generating convergence diagnostic plots...")
    plot_model_convergence(
      results_dir = dirs$cal_diag,
      plots_dir = dirs$res_fig_diag,
      verbose = control$logging$verbose
    )
    plot_model_convergence_status(
      results_dir = dirs$cal_diag,
      plots_dir = dirs$res_fig_diag,
      verbose = control$logging$verbose
    )
  }

  # ===========================================================================
  # POSTERIOR QUANTILES AND DISTRIBUTIONS
  # ===========================================================================

  log_msg("Calculating posterior quantiles")
  posterior_quantiles <- calc_model_posterior_quantiles(
    results = results,
    probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
    output_dir = dirs$cal_posterior,
    priors = priors,
    verbose = control$logging$verbose
  )
  log_msg("Saved %s", file.path(dirs$cal_posterior, "posterior_quantiles.csv"))

  if (control$paths$plots) {
    plot_model_posterior_quantiles(
      csv_files = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
      output_dir = dirs$res_fig_post,
      verbose = control$logging$verbose
    )
  }

  # Calculate posterior distributions
  log_msg("Calculating posterior distributions")
  calc_model_posterior_distributions(
    quantiles_file = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
    priors_file = file.path(dirs$inputs, "priors.json"),
    output_dir = dirs$cal_posterior,
    control = control,
    verbose = control$logging$verbose
  )
  log_msg("Saved %s", file.path(dirs$cal_posterior, "posteriors.json"))

  if (control$paths$plots) {
    plot_model_distributions(
      json_files = c(file.path(dirs$inputs, "priors.json"),
                    file.path(dirs$cal_posterior, "posteriors.json")),
      method_names = c("Prior", "Posterior"),
      output_dir = dirs$res_fig_post
    )
    plot_model_posteriors_detail(
      quantiles_file = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
      results_file = file.path(dirs$calibration, "samples.parquet"),
      priors_file = file.path(dirs$inputs, "priors.json"),
      posteriors_file = file.path(dirs$cal_posterior, "posteriors.json"),
      output_dir = dirs$res_fig_post_detail,
      verbose = control$logging$verbose
    )
  }

  # ===========================================================================
  # PARAMETER SENSITIVITY AND CORRELATION
  # ===========================================================================

  if (control$paths$plots) {
    log_msg("Generating parameter sensitivity and correlation plots...")

    tryCatch(
      plot_model_parameter_sensitivity(
        results_file = file.path(dirs$calibration, "samples.parquet"),
        priors_file = file.path(dirs$inputs, "priors.json"),
        output_dir = dirs$res_fig_diag,
        verbose = control$logging$verbose
      ),
      error = function(e) log_msg("Warning: parameter sensitivity plot failed: %s", e$message)
    )

    tryCatch(
      plot_model_parameter_correlation(
        results_file = file.path(dirs$calibration, "samples.parquet"),
        priors_file = file.path(dirs$inputs, "priors.json"),
        output_dir = dirs$res_fig_diag,
        verbose = control$logging$verbose
      ),
      error = function(e) log_msg("Warning: parameter correlation plot failed: %s", e$message)
    )
  }

  # ===========================================================================
  # POSTERIOR PREDICTIVE CHECKS
  # ===========================================================================

  log_msg("Running posterior predictive checks")
  best_idx <- which.max(results$likelihood)
  best_seed_sim <- results$seed_sim[best_idx]

  config_best <- tryCatch(
    sample_parameters(
      PATHS       = PATHS,
      priors      = priors,
      config      = config,
      seed        = best_seed_sim,
      sample_args = sampling_args,
      verbose     = FALSE
    ),
    error = function(e) {
      log_msg("ERROR: sample_parameters failed for best seed %d: %s", best_seed_sim, e$message)
      log_msg("  Post-processing (PPC, ensemble, summary) will be skipped.")
      NULL
    }
  )

  if (is.null(config_best)) {
    # Finalize state so the run is not left perpetually "running" in monitoring
    .mosaic_finalize_state(state_file)
    return(invisible(list(
      dirs    = dirs,
      files   = list(),
      summary = list(converged = isTRUE(state$converged),
                     error     = "sample_parameters failed for best seed")
    )))
  }

  config_best_file <- file.path(dirs$cal_best_model, "config_best.json")
  jsonlite::write_json(config_best, config_best_file, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  log_msg("Saved %s", config_best_file)

  lc <- reticulate::import("laser.cholera.metapop.model")
  best_model <- lc$run_model(paramfile = MOSAIC:::.mosaic_prepare_config_for_python(config_best), quiet = TRUE)

  # Compute overall model-fit R² and bias ratio (best model vs observed data)
  r2_cases <- tryCatch({
    calc_model_R2(config_best$reported_cases, best_model$results$reported_cases)
  }, error = function(e) NA_real_)

  r2_deaths <- tryCatch({
    calc_model_R2(config_best$reported_deaths, best_model$results$disease_deaths)
  }, error = function(e) NA_real_)

  bias_ratio_cases <- tryCatch({
    calc_bias_ratio(config_best$reported_cases, best_model$results$reported_cases)
  }, error = function(e) NA_real_)

  bias_ratio_deaths <- tryCatch({
    calc_bias_ratio(config_best$reported_deaths, best_model$results$disease_deaths)
  }, error = function(e) NA_real_)

  log_msg("Best model R²: cases = %.4f (bias=%.2f), deaths = %.4f (bias=%.2f)",
          ifelse(is.na(r2_cases), 0, r2_cases),
          ifelse(is.na(bias_ratio_cases), 0, bias_ratio_cases),
          ifelse(is.na(r2_deaths), 0, r2_deaths),
          ifelse(is.na(bias_ratio_deaths), 0, bias_ratio_deaths))

  # ===========================================================================
  # RECONNECT DASK FOR POST-CAL SIMS (if applicable)
  # ===========================================================================
  # The client was disconnected before R-heavy post-processing. Now reconnect
  # to the same (still-alive) cluster to dispatch ensemble + stochastic sims.

  n_ensemble_r2   <- control$predictions$best_model_n_sims
  postca_dask     <- NULL

  if (use_dask && !is.null(dask_cluster)) {
    postca_dask <- tryCatch({
      log_msg("Reconnecting to Dask cluster for post-cal sims...")
      client <- dask_cluster$get_client()
      log_msg("  Reconnected (%d workers)", length(client$scheduler_info()$workers))

      # Re-upload worker module (workers may have restarted during idle)
      worker_py_path <- system.file("python/mosaic_dask_worker.py", package = "MOSAIC")
      client$upload_file(worker_py_path)
      mosaic_worker <- reticulate::import("mosaic_dask_worker")

      # Build stochastic param configs (always, not just when plots = TRUE)
      stoch_param_configs <- NULL
      n_stochastic_per <- control$predictions$ensemble_n_sims_per_param %||% 10L

      if (sum(results$is_best_subset) > 0) {
        best_subset_results <- results[results$is_best_subset == TRUE, ]
        stoch_param_seeds   <- best_subset_results$seed_sim
        stoch_param_weights <- best_subset_results$weight_best[best_subset_results$weight_best > 0]

        if (length(stoch_param_weights) > 0) {
          stoch_param_configs <- lapply(stoch_param_seeds[stoch_param_weights > 0], function(seed_i) {
            tryCatch(
              sample_parameters(PATHS = PATHS, priors = priors, config = config,
                                seed = seed_i, sample_args = sampling_args, verbose = FALSE),
              error = function(e) NULL
            )
          })
          stoch_param_configs <- Filter(Negate(is.null), stoch_param_configs)
        }
      }

      # Dispatch all post-cal sims
      result <- .mosaic_postca_dask(
        client           = client,
        mosaic_worker    = mosaic_worker,
        config_best      = config_best,
        param_configs    = stoch_param_configs,
        n_ensemble       = n_ensemble_r2,
        n_stochastic_per = n_stochastic_per,
        log_msg          = log_msg
      )

      # Close for real now
      log_msg("Closing Dask cluster (post-cal sims complete)")
      tryCatch(client$close(), error = function(e) NULL)
      client <- NULL
      if (!is.null(dask_cluster)) {
        tryCatch(dask_cluster$close(), error = function(e) NULL)
        dask_cluster <- NULL
      }

      result
    }, error = function(e) {
      log_msg("WARNING: Post-cal Dask reconnect/dispatch failed: %s", e$message)
      log_msg("  Falling back to local execution for ensemble/stochastic sims")
      # Clean up on failure
      tryCatch({ if (!is.null(client)) client$close() }, error = function(e2) NULL)
      client <<- NULL
      if (!is.null(dask_cluster)) {
        tryCatch(dask_cluster$close(), error = function(e2) NULL)
        dask_cluster <<- NULL
      }
      NULL
    })
  }

  # ===========================================================================
  # WEIGHTED POSTERIOR ENSEMBLE: R², WINDOWED METRICS, AND PREDICTIVE PLOTS
  # ===========================================================================

  # Compute the true posterior-weighted ensemble ALWAYS (outside plots flag).
  # The same mosaic_ensemble object is used for:
  #   (a) R²/bias ratios (summary.json)
  #   (b) windowed fit metrics
  #   (c) predictive plots (when plots = TRUE)
  r2_cases_ensemble          <- NA_real_
  r2_deaths_ensemble         <- NA_real_
  bias_ratio_cases_ensemble  <- NA_real_
  bias_ratio_deaths_ensemble <- NA_real_
  n_ensemble_params          <- 0L
  n_ensemble_stochastic_per  <- as.integer(control$predictions$ensemble_n_sims_per_param %||% 10L)

  if (sum(results$is_best_subset) > 0) {
    best_subset_results <- results[results$is_best_subset == TRUE, ]
    param_seeds   <- best_subset_results$seed_sim
    param_weights <- best_subset_results$weight_best[best_subset_results$weight_best > 0]

    if (length(param_weights) > 0) {
      param_weights <- param_weights / sum(param_weights)
    } else {
      param_weights <- NULL
    }

    log_msg("Computing weighted posterior ensemble (%d param sets x %d stochastic)...",
            length(param_seeds), n_ensemble_stochastic_per)

    ensemble <- tryCatch(
      calc_model_ensemble(
        config                   = config,
        parameter_seeds          = param_seeds,
        parameter_weights        = param_weights,
        n_simulations_per_config = n_ensemble_stochastic_per,
        envelope_quantiles       = c(0.025, 0.25, 0.75, 0.975),
        PATHS                    = PATHS,
        priors                   = priors,
        sampling_args            = sampling_args,
        parallel                 = control$parallel$enable,
        n_cores                  = control$parallel$n_cores,
        root_dir                 = root_dir,
        precomputed_results      = postca_dask$stochastic_results,
        verbose                  = control$logging$verbose
      ),
      error = function(e) {
        log_msg("Warning: calc_model_ensemble failed: %s", e$message)
        NULL
      }
    )

    if (!is.null(ensemble)) {

      n_ensemble_params <- ensemble$n_param_sets

      # R² and bias from weighted median predictions
      median_c_flat <- as.numeric(ensemble$cases_median)
      median_d_flat <- as.numeric(ensemble$deaths_median)
      obs_c_flat    <- as.numeric(ensemble$obs_cases)
      obs_d_flat    <- as.numeric(ensemble$obs_deaths)

      r2_cases_ensemble  <- calc_model_R2(obs_c_flat, median_c_flat)
      r2_deaths_ensemble <- calc_model_R2(obs_d_flat, median_d_flat)

      bias_ratio_cases_ensemble  <- tryCatch(
        calc_bias_ratio(obs_c_flat, median_c_flat), error = function(e) NA_real_)
      bias_ratio_deaths_ensemble <- tryCatch(
        calc_bias_ratio(obs_d_flat, median_d_flat), error = function(e) NA_real_)

      log_msg("Ensemble R\u00b2 (%d params x %d stoch): cases = %.4f (bias=%.2f), deaths = %.4f (bias=%.2f)",
              n_ensemble_params, n_ensemble_stochastic_per,
              ifelse(is.na(r2_cases_ensemble),          0, r2_cases_ensemble),
              ifelse(is.na(bias_ratio_cases_ensemble),  0, bias_ratio_cases_ensemble),
              ifelse(is.na(r2_deaths_ensemble),         0, r2_deaths_ensemble),
              ifelse(is.na(bias_ratio_deaths_ensemble), 0, bias_ratio_deaths_ensemble))

      # Windowed model fit metrics
      n_ts      <- ensemble$n_time_points
      dates_vec <- seq.Date(as.Date(config$date_start), by = "day", length.out = n_ts)

      fit_windows <- control$predictions$fit_windows %||% c(365L, 120L, 90L, 60L, 30L)

      windowed_metrics <- .mosaic_compute_windowed_metrics(
        obs_cases  = obs_c_flat,
        est_cases  = median_c_flat,
        obs_deaths = obs_d_flat,
        est_deaths = median_d_flat,
        dates      = dates_vec,
        windows    = fit_windows
      )

      wm_path <- file.path(dirs$res_fig_diag, "model_fit_windows.csv")
      utils::write.csv(windowed_metrics, wm_path, row.names = FALSE)
      log_msg("Saved 3_results/figures/diagnostics/model_fit_windows.csv")

      full_row      <- windowed_metrics[windowed_metrics$window == "full", ]
      short_windows <- windowed_metrics[windowed_metrics$window != "full", ]
      if (nrow(short_windows) > 0L) {
        last_row <- short_windows[nrow(short_windows), ]
        log_msg("Windowed fit: R2_cases [full=%.3f, %s=%.3f] | Bias [full=%.2f, %s=%.2f]",
                full_row$r2_cases, last_row$window, last_row$r2_cases,
                full_row$bias_cases, last_row$window, last_row$bias_cases)
      }

      if (control$paths$plots) {
        wm_plot_path <- file.path(dirs$res_fig_diag, "model_fit_windows.png")
        tryCatch({
          .mosaic_plot_windowed_metrics(windowed_metrics, wm_plot_path,
                                        location = paste(config$location_name, collapse = ", "))
          log_msg("Saved 3_results/figures/diagnostics/model_fit_windows.png")
        }, error = function(e) {
          log_msg("Warning: windowed metrics plot failed: %s", e$message)
        })
      }

      # Predictive plots — reuse ensemble, no second set of simulations
      if (control$paths$plots) {
        log_msg("Generating posterior ensemble plots...")
        plot_model_ensemble(
          ensemble         = ensemble,
          output_dir       = dirs$res_fig_pred,
          save_predictions = TRUE,
          verbose          = control$logging$verbose
        )
      }

    } else {
      log_msg("Warning: calc_model_ensemble failed — skipping ensemble R\u00b2 and predictive plots")
    }

  } else {
    log_msg("Warning: no best subset — skipping posterior ensemble")
  }

  # ===========================================================================
  # COMBINE PREDICTION CSVs
  # ===========================================================================

  # Combine per-location prediction CSVs by type (ensemble and stochastic have
  # different column schemas and cannot be rbind'd together)
  for (pred_type in c("ensemble", "stochastic")) {
    pred_csvs <- list.files(dirs$res_fig_pred,
                            pattern = sprintf("^predictions_%s_.*\\.csv$", pred_type),
                            full.names = TRUE)
    if (length(pred_csvs) > 1) {
      combined <- tryCatch({
        pred_list <- lapply(pred_csvs, utils::read.csv, stringsAsFactors = FALSE)
        do.call(rbind, pred_list)
      }, error = function(e) {
        log_msg("Warning: Could not combine %s prediction CSVs: %s", pred_type, e$message)
        NULL
      })
      if (!is.null(combined)) {
        out_file <- file.path(dirs$res_predictions, sprintf("all_predictions_%s.csv", pred_type))
        utils::write.csv(combined, out_file, row.names = FALSE)
        log_msg("Combined %d %s prediction files into %s", length(pred_csvs), pred_type, basename(out_file))
      }
    } else if (length(pred_csvs) == 1) {
      # Single location — copy directly rather than combining
      out_file <- file.path(dirs$res_predictions, sprintf("all_predictions_%s.csv", pred_type))
      file.copy(pred_csvs, out_file, overwrite = TRUE)
    }
  }

  # ===========================================================================
  # POSTERIOR PREDICTIVE CHECKS (PPC)
  # ===========================================================================

  if (control$paths$plots) {
    # Robust call handling both old and new plot_model_ppc signatures
    # Old signature: plot_model_ppc(model, output_dir, verbose)
    # New signature: plot_model_ppc(predictions_dir, predictions_files, locations, model, output_dir, verbose)
    ppc_result <- tryCatch(
      {
        # Try new signature first (always creates both aggregate and per-location plots)
        plot_model_ppc(
          predictions_dir = dirs$res_fig_pred,
          output_dir = dirs$res_figures,
          verbose = control$logging$verbose
        )
      },
      error = function(e) {
        # If new signature fails, check if it's an argument mismatch
        if (grepl("unused argument", e$message)) {
          log_msg("Warning: plot_model_ppc using legacy signature (package may need reinstallation)")
          # Fall back to reading predictions and calling with model object
          # This is a compatibility shim for clusters with old package versions
          NULL  # Skip PPC plots with old signature
        } else {
          # Re-throw other errors
          stop(e)
        }
      }
    )
  }

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  log_msg("Calibration complete: %d batches, %d simulations, %.2f min",
          state$batch_number, state$total_sims_run, as.numeric(runtime))

  # ===========================================================================
  # WRITE RESULTS SUMMARY
  # ===========================================================================

  # Parameter estimates (tidy CSV for downstream use)
  log_msg("Writing parameter estimates...")
  .mosaic_write_parameter_estimates(dirs)
  log_msg("  Saved 3_results/posterior/parameter_estimates.csv")

  # Summary JSON (machine-readable run summary)
  log_msg("Writing summary...")
  summary_obj <- .mosaic_write_summary_json(dirs, state, start_time, config,
                                             r2_cases = r2_cases, r2_deaths = r2_deaths,
                                             r2_cases_ensemble = r2_cases_ensemble,
                                             r2_deaths_ensemble = r2_deaths_ensemble,
                                             bias_ratio_cases = bias_ratio_cases,
                                             bias_ratio_deaths = bias_ratio_deaths,
                                             bias_ratio_cases_ensemble = bias_ratio_cases_ensemble,
                                             bias_ratio_deaths_ensemble = bias_ratio_deaths_ensemble,
                                             n_ensemble_params = n_ensemble_params,
                                             n_ensemble_stochastic_per = n_ensemble_stochastic_per,
                                             io = control$io)
  log_msg("  Saved 3_results/summary.json")

  # Print human-readable summary to log
  fmt_r2b <- function(r2, bias) {
    r2s <- if (is.na(r2)) "NA" else sprintf("%.4f", r2)
    bs  <- if (is.na(bias)) "" else sprintf(" (bias=%.2f)", bias)
    paste0(r2s, bs)
  }
  r2c_str  <- fmt_r2b(summary_obj$r2_cases, summary_obj$bias_ratio_cases)
  r2d_str  <- fmt_r2b(summary_obj$r2_deaths, summary_obj$bias_ratio_deaths)
  r2ce_str <- fmt_r2b(summary_obj$r2_cases_ensemble, summary_obj$bias_ratio_cases_ensemble)
  r2de_str <- fmt_r2b(summary_obj$r2_deaths_ensemble, summary_obj$bias_ratio_deaths_ensemble)
  ret_str  <- if (is.na(summary_obj$n_retained))    "NA" else format(as.integer(summary_obj$n_retained),  big.mark = ",")
  best_str <- if (is.na(summary_obj$n_best_subset)) "NA" else format(as.integer(summary_obj$n_best_subset), big.mark = ",")
  log_msg("=== Run Summary ===")
  log_msg("  Location: %s (%s to %s)", summary_obj$location, summary_obj$date_start, summary_obj$date_stop)
  log_msg("  Converged: %s", if (isTRUE(summary_obj$converged)) "YES" else "NO")
  log_msg("  R2 best model: cases = %s | deaths = %s", r2c_str, r2d_str)
  log_msg("  R2 ensemble:   cases = %s | deaths = %s", r2ce_str, r2de_str)
  if (!is.na(summary_obj$ess_n_params)) {
    log_msg("  ESS: %d/%d params (%.0f%%) above target %g (min: %.1f, median: %.1f)",
            summary_obj$ess_n_above_target, summary_obj$ess_n_params,
            summary_obj$ess_pct_above_target, summary_obj$ess_target,
            summary_obj$ess_min, summary_obj$ess_median)
  }
  log_msg("  Sims: %s total, %s retained, %s best subset",
          format(summary_obj$n_simulations_total, big.mark = ","), ret_str, best_str)
  log_msg("===================")

  # Finalize run state (mark as completed)
  .mosaic_finalize_state(state_file)

  # Return invisibly
  invisible(list(
    dirs = dirs,
    files = list(
      simulations = file.path(dirs$calibration, "samples.parquet"),
      ess_csv = file.path(dirs$cal_diag, "parameter_ess.csv"),
      posterior_quantiles = file.path(dirs$cal_posterior, "posterior_quantiles.csv"),
      posteriors_json = file.path(dirs$cal_posterior, "posteriors.json")
    ),
    summary = list(
      batches = state$batch_number,
      sims_total = state$total_sims_run,
      sims_success = state$total_sims_successful,
      converged = isTRUE(state$converged),
      runtime_min = as.numeric(runtime)
    )
  ))
}

#' @rdname run_MOSAIC
#' @export
run_mosaic <- run_MOSAIC

#' Build Complete MOSAIC Control Structure
#'
#' @description
#' Creates a complete control structure for \code{run_mosaic()}.
#' This is the primary interface for configuring MOSAIC execution settings, consolidating
#' calibration strategy, parameter sampling, parallelization, and output options.
#'
#' **Parameters are organized in workflow order:**
#' \enumerate{
#'   \item \code{calibration}: How to run (simulations, iterations, batch sizes)
#'   \item \code{sampling}: What to sample (which parameters to vary)
#'   \item \code{likelihood}: How to score model fit (likelihood components and weights)
#'   \item \code{targets}: When to stop (ESS convergence thresholds)
#'   \item \code{fine_tuning}: Advanced calibration (adaptive batch sizing)
#'   \item \code{parallel}: Infrastructure (cores, cluster type)
#'   \item \code{io}: Output format (file format, compression)
#'   \item \code{paths}: File management (output directories, plots)
#' }
#'
#' @param calibration List of calibration settings. Default is:
#'   \itemize{
#'     \item \code{n_simulations}: NULL for auto mode, or integer for fixed mode
#'     \item \code{n_iterations}: Number of LASER iterations per simulation (default: 3L)
#'     \item \code{max_simulations}: Maximum total simulations in auto mode (default: 100000L)
#'     \item \code{batch_size}: Simulations per batch in calibration phase (default: 500L)
#'     \item \code{min_batches}: Minimum calibration batches (default: 5L)
#'     \item \code{max_batches}: Maximum calibration batches (default: 8L)
#'     \item \code{target_r2}: R² target for calibration convergence (default: 0.90)
#'   }
#'
#' @param sampling List of parameter sampling flags (what to sample). Default is:
#'   \itemize{
#'     \item \code{sample_tau_i}: Sample transmission rate (default: TRUE)
#'     \item \code{sample_mobility_gamma}: Sample mobility gamma (default: TRUE)
#'     \item \code{sample_mobility_omega}: Sample mobility omega (default: TRUE)
#'     \item \code{sample_mu_j}: Sample recovery rate (default: TRUE)
#'     \item \code{sample_iota}: Sample importation rate (default: TRUE)
#'     \item \code{sample_gamma_2}: Sample second dose efficacy (default: TRUE)
#'     \item \code{sample_alpha_1}: Sample first dose efficacy (default: TRUE)
#'     \item ... (see \code{mosaic_control_defaults()} for complete list of 38 parameters)
#'   }
#'
#' @param likelihood List of likelihood calculation settings (how to score model fit). Default is:
#'   \itemize{
#'     \item \code{weight_cases}: Weight for cases vs deaths (default: 1.0)
#'     \item \code{weight_deaths}: Weight for deaths vs cases (default: 1.0)
#'     \item \code{weight_wis}: WIS regularizer weight (default: 0, try 0.10)
#'     \item ... (see \code{mosaic_control_defaults()} for complete list)
#'   }
#'
#' @param targets List of convergence targets (when to stop). Default is:
#'   \itemize{
#'     \item \code{ESS_param}: Target ESS per parameter (default: 500)
#'     \item \code{ESS_param_prop}: Proportion of parameters meeting ESS (default: 0.95)
#'     \item \code{ESS_best}: Target for both subset size and ESS (default: 100). Both B_size and ESS_B must be >= ESS_best.
#'     \item \code{A_best}: Target agreement index (default: 0.95)
#'     \item \code{CVw_best}: Target CV of weights (default: 0.5)
#'     \item \code{percentile_min}: Minimum percentile for best subset search (default: 0.001)
#'     \item \code{percentile_max}: Maximum percentile for best subset (default: 5.0)
#'     \item \code{ESS_method}: ESS calculation method, "kish" or "perplexity" (default: "kish")
#'   }
#'
#' @param fine_tuning List of fine-tuning batch sizes (advanced calibration). Default is:
#'   \itemize{
#'     \item \code{batch_sizes}: Named list with massive, large, standard, precision, final
#'   }
#'
#' @param predictions List of prediction generation settings. Default is:
#'   \itemize{
#'     \item \code{best_model_n_sims}: Stochastic runs for best model (default: 10L)
#'     \item \code{ensemble_n_param_sets}: Number of parameter sets in ensemble (default: 50L)
#'     \item \code{ensemble_n_sims_per_param}: Stochastic runs per parameter set (default: 10L)
#'   }
#'   Total ensemble simulations = ensemble_n_param_sets × ensemble_n_sims_per_param (e.g., 50 × 10 = 500)
#'
#' @param parallel List of parallelization settings (infrastructure). Default is:
#'   \itemize{
#'     \item \code{enable}: Enable parallel execution (default: FALSE)
#'     \item \code{n_cores}: Number of cores to use (default: 1L)
#'     \item \code{type}: Cluster type, "PSOCK" or "FORK" (default: "PSOCK")
#'     \item \code{progress}: Show progress bar (default: TRUE)
#'   }
#'
#' @param io List of I/O settings (output format). Default is:
#'   \itemize{
#'     \item \code{format}: Output format, "parquet" or "csv" (default: "parquet")
#'     \item \code{compression}: Compression algorithm (default: "zstd")
#'     \item \code{compression_level}: Compression level (default: 3L)
#'   }
#'
#' @param paths List of path and output settings (file management). Default is:
#'   \itemize{
#'     \item \code{clean_output}: Remove output directory if exists (default: FALSE)
#'     \item \code{plots}: Generate diagnostic plots (default: TRUE)
#'   }
#'
#' @return A complete control list suitable for passing to \code{run_mosaic()}.
#'
#' @examples
#' # Default control settings
#' ctrl <- mosaic_control_defaults()
#'
#' # Quick parallel run with 8 cores
#' ctrl <- mosaic_control_defaults(
#'   parallel = list(enable = TRUE, n_cores = 8)
#' )
#'
#' # Fixed mode with 5000 simulations, 5 iterations each
#' ctrl <- mosaic_control_defaults(
#'   calibration = list(
#'     n_simulations = 5000,
#'     n_iterations = 5
#'   )
#' )
#'
#' # Auto mode with custom settings
#' ctrl <- mosaic_control_defaults(
#'   calibration = list(
#'     n_simulations = NULL,  # NULL = auto mode
#'     n_iterations = 3,
#'     max_simulations = 50000,
#'     batch_size = 1000
#'   ),
#'   parallel = list(enable = TRUE, n_cores = 16)
#' )
#'
#' # Only sample specific parameters
#' ctrl <- mosaic_control_defaults(
#'   sampling = list(
#'     sample_tau_i = TRUE,
#'     sample_mobility_gamma = FALSE,
#'     sample_mobility_omega = FALSE,
#'     sample_mu_j = TRUE,
#'     sample_iota = FALSE,
#'     sample_gamma_2 = FALSE,
#'     sample_alpha_1 = FALSE
#'   )
#' )
#'
#' # Enable WIS regularizer and peak timing
#' ctrl <- mosaic_control_defaults(
#'   likelihood = list(
#'     weight_wis = 0.10,
#'     weight_peak_timing = 0.25
#'   )
#' )
#'
#' # Use perplexity method for ESS calculations
#' ctrl <- mosaic_control_defaults(
#'   targets = list(ESS_method = "perplexity")
#' )
#'
#' # Full workflow configuration (demonstrates logical order)
#' ctrl <- mosaic_control_defaults(
#'   calibration = list(n_simulations = NULL, n_iterations = 3),      # How to run
#'   sampling = list(sample_tau_i = TRUE, sample_mu_j = TRUE),        # What to sample
#'   likelihood = list(weight_wis = 0.10, weight_cases = 1.0),        # How to score
#'   targets = list(ESS_param = 500, ESS_param_prop = 0.95),          # When to stop
#'   fine_tuning = list(batch_sizes = list(final = 200)),             # Advanced calibration
#'   parallel = list(enable = TRUE, n_cores = 16),                    # Infrastructure
#'   io = mosaic_io_presets("default"),                               # Output format
#'   paths = list(clean_output = FALSE, plots = TRUE)                 # File management
#' )
#'
#' @export
#' @rdname mosaic_control_defaults
mosaic_control_defaults <- function(calibration = NULL,
                           sampling = NULL,
                           likelihood = NULL,
                           targets = NULL,
                           fine_tuning = NULL,
                           predictions = NULL,
                           weights = NULL,
                           parallel = NULL,
                           io = NULL,
                           paths = NULL,
                           logging = NULL) {

  # Default calibration settings
  default_calibration <- list(
    n_simulations = NULL,      # NULL = auto mode, integer = fixed mode
    n_iterations = 3L,          # iterations per simulation
    max_simulations = 100000L,  # max total simulations in auto mode
    batch_size = 500L,
    max_predictive_batch = 10000L, # Cap on single predictive batch (prevents multi-hour unchecked runs)
    min_batches = 5L,
    max_batches = 8L,
    target_r2 = 0.90
  )

  # Default sampling settings
  # All parameters enabled by default - users can selectively disable
  default_sampling <- list(
    # === GLOBAL PARAMETERS (21) ===
    # Transmission dynamics
    sample_iota = TRUE,              # Environmental contamination rate
    sample_epsilon = TRUE,           # Latent period rate
    sample_gamma_1 = TRUE,           # Recovery rate (symptomatic)
    sample_gamma_2 = TRUE,           # Recovery rate (asymptomatic)
    sample_rho = TRUE,               # Proportion symptomatic

    # Mobility
    sample_mobility_gamma = TRUE,    # Gravity model exponent
    sample_mobility_omega = TRUE,    # Mobility rate

    # Vaccine efficacy
    sample_alpha_1 = TRUE,           # Vaccine efficacy (1 dose)
    sample_alpha_2 = TRUE,           # Vaccine efficacy (2 doses)
    sample_omega_1 = TRUE,           # Waning rate (1 dose)
    sample_omega_2 = TRUE,           # Waning rate (2 doses)
    sample_phi_1 = TRUE,             # Vaccine coverage (1 dose)
    sample_phi_2 = TRUE,             # Vaccine coverage (2 doses)

    # Reporting/observation
    sample_sigma = TRUE,             # Proportion symptomatic
    sample_kappa = TRUE,             # Overdispersion parameter
    sample_chi_endemic = TRUE,       # PPV among suspected cases (endemic)
    sample_chi_epidemic = TRUE,      # PPV among suspected cases (epidemic)
    sample_delta_reporting_cases = TRUE,  # Infection-to-case reporting delay
    sample_delta_reporting_deaths = TRUE, # Infection-to-death reporting delay

    # Environmental decay
    sample_decay_days_long = TRUE,   # Long-term environmental decay
    sample_decay_days_short = TRUE,  # Short-term environmental decay
    sample_decay_shape_1 = TRUE,     # Decay shape parameter 1
    sample_decay_shape_2 = TRUE,     # Decay shape parameter 2

    # Advanced parameters
    sample_zeta_1 = TRUE,            # Advanced parameter 1
    sample_zeta_2 = TRUE,            # Advanced parameter 2

    # === LOCATION-SPECIFIC PARAMETERS ===
    # Transmission and seasonality
    sample_beta_j0_tot = TRUE,       # Baseline transmission rate by location
    sample_p_beta = TRUE,            # Proportion human-to-human transmission
    sample_tau_i = TRUE,             # Travel/diffusion probability
    sample_theta_j = TRUE,           # WASH coverage
    sample_mu_j_baseline = TRUE,     # Baseline location-specific IFR
    sample_mu_j_slope = TRUE,        # Temporal IFR trend
    sample_mu_j_epidemic_factor = TRUE, # Epidemic IFR multiplier
    sample_epidemic_threshold = TRUE, # Epidemic activation threshold

    # Climate relationship
    sample_a_1_j = TRUE,             # Temperature coefficient 1
    sample_a_2_j = TRUE,             # Temperature coefficient 2
    sample_b_1_j = TRUE,             # Rainfall coefficient 1
    sample_b_2_j = TRUE,             # Rainfall coefficient 2

    # Psi-star calibration
    sample_psi_star_a = TRUE,        # Psi-star parameter a
    sample_psi_star_b = TRUE,        # Psi-star parameter b
    sample_psi_star_z = TRUE,        # Psi-star parameter z
    sample_psi_star_k = TRUE,        # Psi-star parameter k

    # === INITIAL CONDITIONS ===
    sample_initial_conditions = TRUE,  # Initial compartment proportions
    ic_moment_match = FALSE            # Derive E/I from observed week-1 cases + reporting chain
  )

  # Default likelihood calculation settings
  default_likelihood <- list(
    # === Component weights (0 = OFF; set > 0 to enable) ===
    weight_cases = 1.0,              # Weight for cases vs deaths
    weight_deaths = 1.0,             # Weight for deaths vs cases
    weight_peak_timing = 0,          # T-normalized (0.25 = 25% of NB core); default OFF
    weight_peak_magnitude = 0,       # T-normalized; default OFF
    weight_cumulative_total = 0,     # T-normalized (/end_idx in helper); default OFF
    weight_wis = 0,                  # T-normalized; default OFF (try 0.10 for regularization)

    # === Peak controls ===
    sigma_peak_time = 1,             # Std dev for peak timing Gaussian (in time steps)
    sigma_peak_log = 0.5,            # Std dev for log peak magnitude

    # === Time/location weighting ===
    weights_time = NULL,             # Numeric vector of per-timestep weights (NULL = uniform)
    weights_location = NULL,         # Numeric vector of per-location weights (NULL = uniform)
    nb_k_min_cases = 3,              # Minimum NB dispersion floor (cases)
    nb_k_min_deaths = 3              # Minimum NB dispersion floor (deaths)
  )

  # Default parallel settings
  default_parallel <- list(
    enable = FALSE,
    n_cores = 1L,
    type = "PSOCK",
    progress = TRUE
  )

  # Default path settings
  default_paths <- list(
    clean_output = FALSE,
    plots = TRUE
  )

  # Default fine-tuning settings
  default_fine_tuning <- list(
    batch_sizes = list(
      massive = 1000L,
      large = 750L,
      standard = 500L,
      precision = 350L,
      final = 250L
    )
  )

  # Default target settings
  default_targets <- list(
    # Parameter-level targets
    ESS_param = 500,
    ESS_param_prop = 0.95,

    # Best subset targets
    ESS_best = 500,              # Updated from 100 for better statistical power
    A_best = 0.95,
    CVw_best = 0.7,              # Updated from 0.5 for realistic subset quality

    # Subset search bounds (absolute counts, replacing percentile-based)
    min_best_subset = 30,        # Minimum subset size for stable metrics
    max_best_subset = 1000,      # Maximum subset size (~1.5% of typical retained)

    # ESS calculation method
    ESS_method = "perplexity",   # "kish" or "perplexity" (ESS formula)
    ESS_marginal_method = "kde"  # "kde", "owen", or "binned" (per-parameter marginal method)
  )

  # Default prediction settings
  default_predictions <- list(
    best_model_n_sims = 10L,            # Stochastic runs for best model
    ensemble_n_param_sets = 50L,        # Number of parameter sets in ensemble
    ensemble_n_sims_per_param = 10L     # Stochastic runs per parameter set
  )

  # Default weight calculation settings
  default_weights <- list(
    floor = 1e-15,        # Minimum weight to prevent underflow (research-backed optimal value)
    iqr_multiplier = 1.5  # Tukey IQR outlier detection multiplier (1.5 = standard, 3.0 = extreme outliers only)
  )

  # Default I/O settings
  default_io <- list(
    format = "parquet",
    compression = "zstd",
    compression_level = 3L,
    load_method = "streaming",         # "streaming" (memory-safe) or "rbind" (legacy)
    load_chunk_size = 5000L,           # Files per chunk when loading many small parquets
    save_simresults = FALSE,           # Save raw per-(sim,iter,j,t) output for validation
    verbose_weights = FALSE            # Print detailed weight calculation diagnostics
  )

  # Default logging settings
  default_logging <- list(
    verbose = FALSE                    # Enable detailed progress messages in sub-functions
  )

  # Merge user-provided settings with defaults
  # Order follows workflow: calibration → sampling → likelihood → targets → fine_tuning → predictions → weights → parallel → io → paths → logging
  list(
    calibration = if (is.null(calibration)) default_calibration else modifyList(default_calibration, calibration),
    sampling = if (is.null(sampling)) default_sampling else modifyList(default_sampling, sampling),
    likelihood = if (is.null(likelihood)) default_likelihood else modifyList(default_likelihood, likelihood),
    targets = if (is.null(targets)) default_targets else modifyList(default_targets, targets),
    fine_tuning = if (is.null(fine_tuning)) default_fine_tuning else modifyList(default_fine_tuning, fine_tuning),
    predictions = if (is.null(predictions)) default_predictions else modifyList(default_predictions, predictions),
    weights = if (is.null(weights)) default_weights else modifyList(default_weights, weights),
    parallel = if (is.null(parallel)) default_parallel else modifyList(default_parallel, parallel),
    io = if (is.null(io)) default_io else modifyList(default_io, io),
    paths = if (is.null(paths)) default_paths else modifyList(default_paths, paths),
    logging = if (is.null(logging)) default_logging else modifyList(default_logging, logging)
  )
}

#' Get Pre-configured I/O Settings
#'
#' @description
#' Returns pre-configured I/O settings for common use cases. Choose from
#' debug, fast, default, or archive presets to optimize for your workflow.
#'
#' @param preset Character. One of "default", "debug", "fast", or "archive"
#'
#' @return Named list with I/O settings (format, compression, compression_level)
#'
#' @details
#' Presets:
#' \itemize{
#'   \item \code{debug}: CSV format, no compression (easy inspection)
#'   \item \code{fast}: Parquet with low compression (fastest)
#'   \item \code{default}: Parquet with medium compression (balanced)
#'   \item \code{archive}: Parquet with high compression (smallest files)
#' }
#'
#' @export
mosaic_io_presets <- function(preset = c("default", "debug", "fast", "archive")) {
  preset <- match.arg(preset)

  switch(preset,
    debug = list(
      format = "csv",
      compression = "none",
      compression_level = NULL
    ),
    fast = list(
      format = "parquet",
      compression = "snappy",
      compression_level = NULL
    ),
    default = list(
      format = "parquet",
      compression = "zstd",
      compression_level = 3L
    ),
    archive = list(
      format = "parquet",
      compression = "zstd",
      compression_level = 9L
    )
  )
}
