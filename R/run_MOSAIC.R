# =============================================================================
# MOSAIC: run_mosaic.R
# Main calibration workflow function
# =============================================================================

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
                                          dir_bfrs_parameters, dir_bfrs_timeseries,
                                          param_names_all, sampling_args, io,
                                          save_timeseries = TRUE) {

  # Pre-allocate result matrix (FIXED: proper pre-allocation)
  n_params <- length(param_names_all)
  result_matrix <- matrix(NA_real_, nrow = n_iterations, ncol = 5 + n_params)
  colnames(result_matrix) <- c('sim', 'iter', 'seed_sim', 'seed_iter', 'likelihood', param_names_all)

  # Pre-allocate output matrix (estimated size, will grow if needed)
  # Estimate: assume ~10 locations × 100 time points = 1000 rows per iteration
  estimated_rows <- n_iterations * 1000
  output_matrix <- matrix(NA_character_, nrow = estimated_rows, ncol = 6)
  colnames(output_matrix) <- c('sim', 'iter', 'j', 't', 'cases', 'deaths')
  output_row_idx <- 1L

  # Sample parameters ONCE per simulation
  params_sim <- tryCatch({
    sample_parameters(
      PATHS = PATHS,
      priors = priors,
      config = config,
      seed = sim_id,
      sample_args = sampling_args,
      verbose = FALSE  # Suppress verbose output (progress bar shows overall progress)
    )
  }, error = function(e) {
    # Log error details for debugging
    warning("Simulation ", sim_id, " failed during parameter sampling: ",
            e$message, call. = FALSE, immediate. = FALSE)
    NULL
  })

  if (is.null(params_sim)) return(FALSE)

  # Import laser-cholera (explicit check, no inherits)
  # Parallel mode: lc exists in worker global environment
  # Sequential mode: import here
  if (!exists("lc", where = .GlobalEnv, inherits = FALSE)) {
    lc <- reticulate::import("laser_cholera.metapop.model")
  } else {
    lc <- get("lc", envir = .GlobalEnv)
  }

  # Run iterations
  for (j in 1:n_iterations) {
    # Use iteration-specific seed (see seed scheme documentation above)
    seed_ij <- (sim_id - 1L) * n_iterations + j
    params <- params_sim
    params$seed <- seed_ij

    # Initialize row directly in pre-allocated matrix (FIXED: no rbind!)
    result_matrix[j, 1:4] <- c(sim_id, j, sim_id, seed_ij)

    # Convert parameters to vector
    param_vec <- tryCatch({
      convert_config_to_matrix(params)
    }, error = function(e) NULL)

    if (!is.null(param_vec) && length(param_vec) > 0) {
      if ("seed" %in% names(param_vec)) {
        param_vec <- param_vec[names(param_vec) != "seed"]
      }
      param_vec <- param_vec[param_names_all]

      for (i in seq_along(param_names_all)) {
        val <- suppressWarnings(as.numeric(param_vec[i]))
        if (!is.na(val)) {
          result_matrix[j, 5 + i] <- val
        }
      }
    }

    # Run model
    model <- tryCatch({
      lc$run_model(paramfile = params, quiet = TRUE)
    }, error = function(e) {
      # Log model run failure (but don't fail entire simulation)
      warning("Simulation ", sim_id, " iteration ", j, " model run failed: ",
              e$message, call. = FALSE, immediate. = FALSE)
      NULL
    })

    # Calculate likelihood
    if (!is.null(model)) {
      likelihood <- tryCatch({
        obs_cases <- params$reported_cases
        est_cases <- model$results$expected_cases
        obs_deaths <- params$reported_deaths
        est_deaths <- model$results$disease_deaths

        if (!is.null(obs_cases) && !is.null(est_cases) &&
            !is.null(obs_deaths) && !is.null(est_deaths)) {

          calc_model_likelihood(
            config = config,
            obs_cases = obs_cases,
            est_cases = est_cases,
            obs_deaths = obs_deaths,
            est_deaths = est_deaths,
            add_max_terms = control$likelihood$add_max_terms,
            add_peak_timing = control$likelihood$add_peak_timing,
            add_peak_magnitude = control$likelihood$add_peak_magnitude,
            add_cumulative_total = control$likelihood$add_cumulative_total,
            add_wis = control$likelihood$add_wis,
            weight_cases = control$likelihood$weight_cases,
            weight_deaths = control$likelihood$weight_deaths,
            weight_max_terms = control$likelihood$weight_max_terms,
            weight_peak_timing = control$likelihood$weight_peak_timing,
            weight_peak_magnitude = control$likelihood$weight_peak_magnitude,
            weight_cumulative_total = control$likelihood$weight_cumulative_total,
            weight_wis = control$likelihood$weight_wis,
            sigma_peak_time = control$likelihood$sigma_peak_time,
            sigma_peak_log = control$likelihood$sigma_peak_log,
            penalty_unmatched_peak = control$likelihood$penalty_unmatched_peak,
            enable_guardrails = control$likelihood$enable_guardrails,
            floor_likelihood = control$likelihood$floor_likelihood,
            guardrail_verbose = control$likelihood$guardrail_verbose
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

      # Store time series outputs
      est_cases_array <- model$results$expected_cases
      est_deaths_array <- model$results$disease_deaths

      if (!is.null(est_cases_array) && !is.null(est_deaths_array)) {
        # Ensure matrix format
        if (!is.matrix(est_cases_array)) {
          est_cases_array <- matrix(est_cases_array, nrow = 1)
          est_deaths_array <- matrix(est_deaths_array, nrow = 1)
        }

        n_j <- nrow(est_cases_array)
        n_t <- ncol(est_cases_array)
        n_rows_needed <- n_j * n_t

        # Check if we need to grow the output matrix
        if (output_row_idx + n_rows_needed - 1 > nrow(output_matrix)) {
          # Grow to exact size needed (not just double, which may be insufficient)
          rows_to_add <- (output_row_idx + n_rows_needed - 1) - nrow(output_matrix)
          new_rows <- matrix(NA_character_, nrow = rows_to_add, ncol = 6)
          colnames(new_rows) <- colnames(output_matrix)
          output_matrix <- rbind(output_matrix, new_rows)
        }

        # Write directly to pre-allocated matrix (FIXED: no rbind in loop!)
        for (loc_idx in 1:n_j) {
          for (t_idx in 1:n_t) {
            output_matrix[output_row_idx, ] <- c(
              as.character(sim_id),
              as.character(j),
              as.character(loc_idx),
              as.character(t_idx),
              as.character(est_cases_array[loc_idx, t_idx]),
              as.character(est_deaths_array[loc_idx, t_idx])
            )
            output_row_idx <- output_row_idx + 1L
          }
        }
      }
    }

    # Explicit garbage collection to prevent Python object buildup
    # Run every 10 iterations to balance cleanup vs overhead
    if (j %% 10 == 0) {
      gc(verbose = FALSE)
      reticulate::import("gc")$collect()
    }
  }

  # Trim output matrix to actual size used
  if (output_row_idx > 1) {
    output_matrix <- output_matrix[1:(output_row_idx - 1), , drop = FALSE]
  } else {
    output_matrix <- output_matrix[0, , drop = FALSE]  # Empty matrix
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
  output_file <- file.path(dir_bfrs_parameters, sprintf("sim_%07d.parquet", sim_id))
  .mosaic_write_parquet(as.data.frame(result_matrix), output_file, io)

  # Collapse and write time series
  if (nrow(output_matrix) > 0) {
    unique_combos <- unique(output_matrix[, c(3, 4)])
    n_combos <- nrow(unique_combos)

    collapsed_matrix <- matrix(NA_real_, nrow = n_combos, ncol = 6)
    colnames(collapsed_matrix) <- c('sim', 'iter', 'j', 't', 'cases', 'deaths')

    for (i in 1:n_combos) {
      j_val <- as.integer(unique_combos[i, 1])
      t_val <- as.integer(unique_combos[i, 2])

      match_rows <- output_matrix[, 3] == as.character(j_val) &
                    output_matrix[, 4] == as.character(t_val)
      matching_data <- output_matrix[match_rows, , drop = FALSE]

      collapsed_matrix[i, ] <- c(
        sim_id, 1, j_val, t_val,
        mean(as.numeric(matching_data[, 5]), na.rm = TRUE),
        mean(as.numeric(matching_data[, 6]), na.rm = TRUE)
      )
    }

    collapsed_df <- data.frame(
      sim = as.integer(collapsed_matrix[, 1]),
      iter = as.integer(collapsed_matrix[, 2]),
      j = as.integer(collapsed_matrix[, 3]),
      t = as.integer(collapsed_matrix[, 4]),
      cases = as.numeric(collapsed_matrix[, 5]),
      deaths = as.numeric(collapsed_matrix[, 6])
    )

    # Only write timeseries files if NPE is enabled
    if (save_timeseries && !is.null(dir_bfrs_timeseries)) {
      out_file <- file.path(dir_bfrs_timeseries, sprintf("timeseries_%07d.parquet", sim_id))
      .mosaic_write_parquet(collapsed_df, out_file, io)
    }
  }

  # CRITICAL: Cleanup Python objects after EACH simulation completes
  # This prevents accumulation across thousands of simulations in large batches
  # In predictive phase with 70K+ sims, this prevents finalizer queue saturation
  gc(verbose = FALSE)
  reticulate::import("gc")$collect()

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
#'   \item Post-hoc subset optimization for NPE priors
#'   \item Posterior quantile and distribution estimation
#'   \item Posterior predictive checks and uncertainty quantification
#'   \item Optional: Neural Posterior Estimation (NPE) stage
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
#' @param resume Logical. If \code{TRUE}, continues from existing checkpoint. Default: FALSE.
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
#'   \item{npe}{enable, weight_strategy}
#'   \item{io}{format, compression, compression_level}
#' }
#'
#' @section Output Files:
#' Results are organized in a structured directory tree:
#' \itemize{
#'   \item \code{0_setup/}: Configuration files (JSON format)
#'   \item \code{1_bfrs/outputs/}: Simulation results (Parquet format)
#'   \item \code{1_bfrs/diagnostics/}: ESS metrics, convergence results
#'   \item \code{1_bfrs/posterior/}: Posterior quantiles and distributions
#'   \item \code{1_bfrs/plots/}: Diagnostic, parameter, and prediction plots
#'   \item \code{2_npe/}: Neural Posterior Estimation results (if enabled)
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
                       control = NULL,
                       resume = FALSE) {

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

  # Validate config structure
  if (!"location_name" %in% names(config)) {
    stop("config must contain 'location_name' field", call. = FALSE)
  }

  # Extract iso_code from config for logging
  iso_code <- config$location_name

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
    run_npe = isTRUE(control$npe$enable),
    clean_output = isTRUE(control$paths$clean_output)
  )

  # ===========================================================================
  # WRITE SETUP FILES (with cluster metadata)
  # ===========================================================================

  # Capture cluster metadata for debugging
  cluster_metadata <- .mosaic_get_cluster_metadata()

  sim_params <- list(
    control = control,  # Complete control object for reproducibility
    n_iterations = n_iterations,
    iso_code = iso_code,
    timestamp = Sys.time(),
    R_version = R.version.string,
    MOSAIC_version = as.character(utils::packageVersion("MOSAIC")),
    cluster_metadata = cluster_metadata,
    paths = list(
      dir_output = dirs$root,
      dir_setup = dirs$setup,
      dir_bfrs = dirs$bfrs,
      dir_npe = if (isTRUE(control$npe$enable)) dirs$npe else NULL,
      dir_results = dirs$results
    )
  )

  log_msg("Writing setup files...")
  .mosaic_write_json(sim_params, file.path(dirs$setup, "simulation_params.json"), control$io)
  log_msg("  Saved %s", basename(file.path(dirs$setup, "simulation_params.json")))

  .mosaic_write_json(priors, file.path(dirs$setup, "priors.json"), control$io)
  log_msg("  Saved %s", basename(file.path(dirs$setup, "priors.json")))

  .mosaic_write_json(config, file.path(dirs$setup, "config_base.json"), control$io)
  log_msg("  Saved %s", basename(file.path(dirs$setup, "config_base.json")))

  # ===========================================================================
  # PARAMETER NAME DETECTION
  # ===========================================================================

  tmp <- convert_config_to_matrix(config)
  if ("seed" %in% names(tmp)) tmp <- tmp[names(tmp) != "seed"]
  param_names_all <- names(tmp)
  rm(tmp)

  # Filter to estimated parameters for ESS tracking
  est_params_df <- get("estimated_parameters", envir = asNamespace("MOSAIC"))
  if (!is.data.frame(est_params_df) || nrow(est_params_df) == 0) {
    stop("Failed to load MOSAIC::estimated_parameters")
  }

  base_params <- unique(gsub("_[A-Z]{3}$", "", param_names_all))
  valid_base_params <- base_params[base_params %in% est_params_df$parameter_name]
  param_names_estimated <- param_names_all[gsub("_[A-Z]{3}$", "", param_names_all) %in% valid_base_params]
  param_names_estimated <- param_names_estimated[!grepl("^[NSEIRV][12]?_j_initial", param_names_estimated)]

  if (!length(param_names_estimated)) {
    stop("No estimated parameters found for ESS tracking")
  }

  log_msg("Parameters: %d estimated (of %d total) | Locations: %s",
          length(param_names_estimated), length(param_names_all),
          paste(config$location_name, collapse = ', '))

  # ===========================================================================
  # SETUP PARALLEL CLUSTER (OR SEQUENTIAL EXECUTION)
  # ===========================================================================

  # Force sequential if parallel is disabled
  if (!isTRUE(control$parallel$enable)) {
    control$parallel$n_cores <- 1L
  }

  # Progress bar controlled by user via control$parallel$progress
  # (No automatic detection - user can disable if needed)

  # Only create cluster if n_cores > 1
  if (control$parallel$n_cores > 1L) {
    log_msg("Setting up %s cluster with %d cores", control$parallel$type, control$parallel$n_cores)

    cl <- parallel::makeCluster(control$parallel$n_cores, type = control$parallel$type)

    # Register cleanup handler (will be called on normal exit or error)
    on.exit({
      if (!is.null(cl)) {
        try({
          parallel::stopCluster(cl)
          log_msg("Cluster stopped successfully")
        }, silent = TRUE)
      }
    }, add = TRUE)

    .root_dir_val <- root_dir
    parallel::clusterExport(cl, varlist = c(".root_dir_val"), envir = environment())

    parallel::clusterEvalQ(cl, {
      # Set library path for VM user installation
      .libPaths(c('~/R/library', .libPaths()))

      library(MOSAIC)
      library(reticulate)
      library(arrow)

      # CRITICAL: Limit each worker to single-threaded BLAS operations
      # Without this, each worker spawns multiple BLAS threads → severe oversubscription
      # Example: 16 workers × 8 BLAS threads = 128 total threads (worse than single-threaded!)
      # This ensures: 16 workers × 1 BLAS thread = 16 threads (optimal)
      # Use ::: to access internal function from MOSAIC namespace in PSOCK workers
      MOSAIC:::.mosaic_set_blas_threads(1L)

      set_root_directory(.root_dir_val)
      PATHS <- get_paths()

      # Import laser-cholera ONCE per worker (not per simulation)
      # This avoids repeated import overhead (~5ms per simulation)
      lc <- reticulate::import("laser_cholera.metapop.model")
      assign("lc", lc, envir = .GlobalEnv)  # Store in global for worker function

      # Suppress NumPy warnings
      warnings_py <- reticulate::import("warnings")
      warnings_py$filterwarnings("ignore", message = "invalid value encountered in divide")
      NULL
    })

    parallel::clusterExport(cl,
      c("n_iterations", "priors", "config", "PATHS", "param_names_all", "sampling_args", "dirs", "control"),
      envir = environment())

    # Create worker function on each worker using exported variables
    # This avoids anonymous function closure serialization overhead
    parallel::clusterCall(cl, function() {
      assign(".run_sim_worker", function(sim_id) {
        # Use ::: to access internal function from MOSAIC namespace in PSOCK workers
        MOSAIC:::.mosaic_run_simulation_worker(
          sim_id = sim_id,
          n_iterations = n_iterations,
          priors = priors,
          config = config,
          PATHS = PATHS,
          dir_bfrs_parameters = dirs$bfrs_params,
          dir_bfrs_timeseries = if (control$npe$enable) dirs$bfrs_times else NULL,
          param_names_all = param_names_all,
          sampling_args = sampling_args,
          io = control$io,
          save_timeseries = control$npe$enable
        )
      }, envir = .GlobalEnv)
      NULL
    })
  } else {
    log_msg("Running sequentially (n_cores = 1)")
    cl <- NULL
  }

  # ===========================================================================
  # DETERMINE RUN MODE: AUTO vs FIXED (with safe state loading)
  # ===========================================================================

  nspec <- .mosaic_normalize_n_sims(n_simulations)
  state_file <- file.path(dirs$bfrs_diag, "run_state.rds")

  # Load state with validation (FIXED: Issue 1.4)
  state <- if (resume && file.exists(state_file)) {
    log_msg("Attempting to resume from: %s", state_file)
    loaded_state <- .mosaic_load_state_safe(state_file)

    if (is.null(loaded_state)) {
      log_msg("WARNING: Failed to load or validate state file")
      log_msg("Starting fresh calibration")
      .mosaic_init_state(control, param_names_estimated, nspec)
    } else {
      log_msg("Successfully loaded state (batch %d, %d simulations completed)",
              loaded_state$batch_number, loaded_state$total_sims_run)
      loaded_state
    }
  } else {
    .mosaic_init_state(control, param_names_estimated, nspec)
  }

  log_msg("Starting simulation (mode: %s)", state$mode)
  start_time <- Sys.time()

  # ===========================================================================
  # SIMULATION: FIXED MODE
  # ===========================================================================

  if (identical(state$mode, "fixed")) {

    target <- state$fixed_target

    log_msg("[FIXED MODE] Running exactly %d simulations", target)

    # Find existing files if resuming (FIXED: Issue 1.5 - safe sim ID parsing)
    done_ids <- integer()
    if (resume) {
      existing <- list.files(dirs$bfrs_params, pattern = "^sim_[0-9]{7}\\.parquet$", full.names = FALSE)
      if (length(existing)) {
        done_ids <- .mosaic_parse_sim_ids(existing, pattern = "^sim_0*([0-9]+)\\.parquet$")
        log_msg("Found %d existing simulations to skip", length(done_ids))
      }
    }

    all_ids <- seq_len(target)
    sim_ids <- setdiff(all_ids, done_ids)

    if (length(sim_ids) == 0L) {
      log_msg("Nothing to do: %d simulations already complete", length(done_ids))
    } else {
      log_msg("Running %d simulations (%d-%d)", length(sim_ids), min(sim_ids), max(sim_ids))

      batch_start_time <- Sys.time()

      # Use worker function (parallel mode uses pre-defined .run_sim_worker on workers)
      if (!is.null(cl)) {
        # Parallel: use worker function defined on cluster
        success_indicators <- .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .run_sim_worker(sim_id),
          cl = cl,
          show_progress = control$parallel$progress
        )
      } else {
        # Sequential: define inline
        success_indicators <- .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .mosaic_run_simulation_worker(
            sim_id, n_iterations, priors, config, PATHS,
            dirs$bfrs_params,
            if (control$npe$enable) dirs$bfrs_times else NULL,
            param_names_all, sampling_args,
            io = control$io,
            save_timeseries = control$npe$enable
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

      # BUG FIX #5: If batch size is 0, we're done
      if (current_batch_size <= 0) {
        log_msg("No additional simulations needed (batch_size = 0)")
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

      # Use worker function (parallel mode uses pre-defined .run_sim_worker on workers)
      if (!is.null(cl)) {
        # Parallel: use worker function defined on cluster
        success_indicators <- .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .run_sim_worker(sim_id),
          cl = cl,
          show_progress = control$parallel$progress
        )
      } else {
        # Sequential: define inline
        success_indicators <- .mosaic_run_batch(
          sim_ids = sim_ids,
          worker_func = function(sim_id) .mosaic_run_simulation_worker(
            sim_id, n_iterations, priors, config, PATHS,
            dirs$bfrs_params,
            if (control$npe$enable) dirs$bfrs_times else NULL,
            param_names_all, sampling_args,
            io = control$io,
            save_timeseries = control$npe$enable
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

      # ESS convergence check (skips automatically if insufficient samples)
      # Skip if final batch - we're about to load data anyway for final processing
      if (state$total_sims_run < control$calibration$max_simulations) {
        state <- .mosaic_ess_check_update_state(state, dirs, param_names_estimated, control)
        .mosaic_save_state(state, state_file)
      } else {
        log_msg("Skipping ESS check (final batch)")
      }

      if (state$total_sims_run >= control$calibration$max_simulations && !state$converged) {
        log_msg("WARNING: Reached maximum simulations (%d) without convergence",
                control$calibration$max_simulations)
        break
      }
    }
  }

  # Stop cluster if it was created
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }

  # ===========================================================================
  # COMBINE RESULTS AND ADD FLAGS
  log_msg("Combining simulation files")

  # Get list of simulation files
  parquet_files <- list.files(dirs$bfrs_params, pattern = "^sim_.*\\.parquet$", full.names = TRUE)

  # Load and combine all simulation files
  # Default: streaming (memory-safe for large runs)
  # Override via control$io$load_method if needed
  load_method <- if (!is.null(control$io$load_method)) {
    control$io$load_method
  } else {
    "streaming"  # Safe default
  }

  results <- .mosaic_load_and_combine_results(
    dir_params = dirs$bfrs_params,
    method = load_method,
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
    upper_threshold <- q3 + iqr_mult * iqr
    results$is_outlier[results$is_valid] <- valid_ll < lower_threshold | valid_ll > upper_threshold

    log_msg("  Outlier detection (Tukey IQR, multiplier = %.1f):", iqr_mult)
    log_msg("    - Outliers: %d (%.1f%%)", sum(results$is_outlier),
            100 * sum(results$is_outlier) / sum(results$is_valid))
  }

  results$is_retained <- results$is_finite & !results$is_outlier
  results$is_best_subset <- FALSE
  results$is_best_model <- FALSE

  if (any(results$is_valid)) {
    results$is_best_model[which.max(results$likelihood)] <- TRUE
  }

  # Write combined simulations and clean up shards
  simulations_file <- file.path(dirs$bfrs_out, "simulations.parquet")
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
    param_names = param_names_estimated,
    likelihood_col = "likelihood",
    n_grid = 100,
    method = control$targets$ESS_method,
    verbose = control$logging$verbose
  )

  ess_file <- file.path(dirs$bfrs_diag, "parameter_ess.csv")
  write.csv(ess_results, ess_file, row.names = FALSE)
  log_msg("Saved %s", ess_file)

  log_msg("Optimizing subset selection (testing up to 15 tiers)...")

  subset_tiers <- get_default_subset_tiers(
    target_ESS_best = control$targets$ESS_best,
    target_A = control$targets$A_best,
    target_CVw = control$targets$CVw_best,
    n_tiers = 15
  )

  optimal_subset_result <- NULL
  tier_used <- NULL

  for (tier_name in names(subset_tiers)) {
    tier <- subset_tiers[[tier_name]]

    log_msg("  Testing tier '%s' (ESS_B=%.0f, A=%.2f, CVw=%.2f)...",
            tier$name, tier$ESS_B, tier$A, tier$CVw)

    tier_result <- identify_best_subset(
      results = results,
      min_B = tier$ESS_B,  # Use tier-specific ESS target as minimum count
      target_ESS_B = tier$ESS_B,
      target_A = tier$A,
      target_CVw = tier$CVw,
      min_percentile = control$targets$percentile_min,
      max_percentile = control$targets$percentile_max,
      precision = 0.0001,
      ess_method = 'kish', # Leave as kish, this is intentional because it is more conservative when forming the best subset, but using perplexity elsewhere for reporting
      verbose = control$logging$verbose
    )

    if (tier_result$converged) {
      log_msg("    ✓ Tier '%s' converged at %.1f%% percentile",
              tier$name, tier_result$percentile_used)
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
    top_subset_final <- optimal_subset_result$subset
    percentile_used <- optimal_subset_result$percentile_used
    n_top_final <- optimal_subset_result$n_selected
    convergence_tier <- tier_used

  } else {
    fallback_percentile <- control$targets$percentile_max
    n_top_final <- ceiling(nrow(results) * fallback_percentile / 100)
    results_ranked_final <- results[order(results$likelihood, decreasing = TRUE), ]
    top_subset_final <- results_ranked_final[1:n_top_final, ]
    rm(results_ranked_final)

    percentile_used <- fallback_percentile
    convergence_tier <- "fallback"
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
    min_search_percentile = control$targets$percentile_min,
    max_search_percentile = control$targets$percentile_max,
    optimal_percentile = percentile_used,
    optimization_tier = convergence_tier,
    n_selected = n_top_final,
    ESS_B = ESS_B_final,
    A = A_final,
    CVw = CVw_final,
    gibbs_temperature = gibbs_temperature_final,
    meets_all_criteria = final_converged,
    timestamp = Sys.time()
  )
  summary_file <- file.path(dirs$bfrs_diag, "subset_selection_summary.csv")
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
    target_percentile_max = control$targets$percentile_max,
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
    seed = results$sim,
    likelihood = results$likelihood,
    aic = -2 * results$likelihood,
    delta_aic = if (is.finite(best_aic_val)) {
      -2 * results$likelihood - best_aic_val
    } else {
      rep(NA_real_, nrow(results))
    },
    w = results$weight_best,
    w_tilde = results$weight_best,
    retained = results$is_best_subset,
    # Additional columns for two-tier structure
    w_retained = results$weight_retained,
    is_retained = results$is_retained,
    is_best_subset = results$is_best_subset
  )

  convergence_file <- file.path(dirs$bfrs_diag, "convergence_results.parquet")
  .mosaic_write_parquet(convergence_results_df, convergence_file, control$io)
  log_msg("Saved %s", convergence_file)

  diagnostics_file <- file.path(dirs$bfrs_diag, "convergence_diagnostics.json")
  jsonlite::write_json(diagnostics, diagnostics_file, pretty = TRUE, auto_unbox = TRUE)
  log_msg("Saved %s", diagnostics_file)

  if (control$paths$plots) {
    log_msg("Generating convergence diagnostic plots...")
    plot_model_convergence(
      results_dir = dirs$bfrs_diag,
      plots_dir = dirs$bfrs_plots_diag,
      verbose = control$logging$verbose
    )
    plot_model_convergence_status(
      results_dir = dirs$bfrs_diag,
      plots_dir = dirs$bfrs_plots_diag,
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
    output_dir = dirs$bfrs_post,
    verbose = control$logging$verbose
  )
  log_msg("Saved %s", file.path(dirs$bfrs_post, "posterior_quantiles.csv"))

  if (control$paths$plots) {
    plot_model_posterior_quantiles(
      csv_files = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
      output_dir = dirs$bfrs_plots_post,
      verbose = control$logging$verbose
    )
  }

  # Calculate posterior distributions
  log_msg("Calculating posterior distributions")
  calc_model_posterior_distributions(
    quantiles_file = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
    priors_file = file.path(dirs$setup, "priors.json"),
    output_dir = dirs$bfrs_post,
    verbose = control$logging$verbose
  )
  log_msg("Saved %s", file.path(dirs$bfrs_post, "posteriors.json"))

  if (control$paths$plots) {
    plot_model_distributions(
      json_files = c(file.path(dirs$setup, "priors.json"),
                    file.path(dirs$bfrs_post, "posteriors.json")),
      method_names = c("Prior", "Posterior"),
      output_dir = dirs$bfrs_plots_post
    )
    plot_model_posteriors_detail(
      quantiles_file = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
      results_file = file.path(dirs$bfrs_out, "simulations.parquet"),
      priors_file = file.path(dirs$setup, "priors.json"),
      posteriors_file = file.path(dirs$bfrs_post, "posteriors.json"),
      output_dir = file.path(dirs$bfrs_plots_post, "detail"),
      verbose = control$logging$verbose
    )
  }

  # ===========================================================================
  # POSTERIOR PREDICTIVE CHECKS
  # ===========================================================================

  log_msg("Running posterior predictive checks")
  best_idx <- which.max(results$likelihood)
  best_seed_sim <- results$seed_sim[best_idx]

  config_best <- sample_parameters(
    PATHS = PATHS,
    priors = priors,
    config = config,
    seed = best_seed_sim,
    sample_args = sampling_args,
    verbose = FALSE
  )

  config_best_file <- file.path(dirs$bfrs_cfg, "config_best.json")
  jsonlite::write_json(config_best, config_best_file, pretty = TRUE, auto_unbox = TRUE)
  log_msg("Saved %s", config_best_file)

  lc <- reticulate::import("laser_cholera.metapop.model")
  best_model <- lc$run_model(paramfile = config_best, quiet = TRUE)

  if (control$paths$plots) {
    log_msg("Generating posterior predictive plots (best model)...")
    plot_model_fit_stochastic(
      config = config_best,
      n_simulations = control$predictions$best_model_n_sims,
      output_dir = dirs$bfrs_plots_pred,
      envelope_quantiles = c(0.025, 0.975),
      save_predictions = TRUE,
      parallel = TRUE,
      n_cores = control$parallel$n_cores,
      root_dir = root_dir,
      verbose = control$logging$verbose
    )
  }

  # ===========================================================================
  # PARAMETER + STOCHASTIC UNCERTAINTY PLOTS
  # ===========================================================================

  if (control$paths$plots && sum(results$is_best_subset) > 0) {
    log_msg("Generating ensemble predictions (parameter + stochastic uncertainty)...")
    best_subset_results <- results[results$is_best_subset == TRUE, ]
    param_seeds <- best_subset_results$seed_sim
    param_weights <- best_subset_results$weight_best[best_subset_results$weight_best > 0]

    if (length(param_weights) > 0) {
      param_weights <- param_weights / sum(param_weights)
    } else {
      param_weights <- NULL
    }

    plot_model_fit_stochastic_param(
      config = config,
      parameter_seeds = param_seeds,
      parameter_weights = param_weights,
      n_simulations_per_config = control$predictions$ensemble_n_sims_per_param,
      envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
      PATHS = PATHS,
      priors = priors,
      sampling_args = sampling_args,
      output_dir = dirs$bfrs_plots_pred,
      save_predictions = TRUE,
      parallel = TRUE,
      n_cores = control$parallel$n_cores,
      root_dir = root_dir,
      verbose = control$logging$verbose
    )
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
          predictions_dir = dirs$bfrs_plots_pred,
          output_dir = dirs$bfrs_plots,
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

  # ===========================================================================
  # STAGE 2: NEURAL POSTERIOR ESTIMATION (NPE)
  # ===========================================================================

  if (control$npe$enable) {
    log_msg("Starting NPE")

    npe_weights <- get_npe_weights(
      bfrs_results = results,
      strategy = control$npe$weight_strategy,
      verbose = FALSE
    )

    npe_weights_file <- file.path(dirs$npe, "npe_weights.parquet")
    weight_df <- data.frame(
      sim = results$sim,
      weight_npe = npe_weights
    )
    .mosaic_write_parquet(weight_df, npe_weights_file, control$io)
    log_msg("Saved %s", npe_weights_file)

    observed_data <- get_npe_observed_data(config, verbose = FALSE)
    observed_file <- file.path(dirs$npe, "observed_data.csv")
    write.csv(observed_data, observed_file, row.names = FALSE)
    log_msg("Saved %s", observed_file)

    npe_dirs <- list(
      root = dirs$npe,
      model = file.path(dirs$npe, "model"),
      posterior = file.path(dirs$npe, "posterior"),
      diagnostics = file.path(dirs$npe, "diagnostics"),
      plots = file.path(dirs$npe, "plots")
    )
    lapply(npe_dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))

    log_msg("Preparing NPE data")
    npe_data <- prepare_npe_data(
      bfrs_dir = dirs$bfrs,
      results = results,
      param_names = param_names_estimated,
      weights = npe_weights,
      verbose = FALSE
    )

    arch_spec <- calc_npe_architecture(
      n_sims = npe_data$n_samples,
      n_params = npe_data$n_params,
      n_timesteps = npe_data$n_timesteps,
      n_locations = npe_data$n_locations,
      tier = "auto",
      verbose = FALSE
    )

    log_msg("Training NPE model")
    npe_model <- train_npe(
      X = npe_data$parameters,
      y = npe_data$observations,
      weights = npe_data$weights,
      bounds = npe_data$bounds,
      architecture = arch_spec,
      output_dir = npe_dirs$model,
      use_gpu = FALSE,
      seed = 42,
      verbose = FALSE
    )

    param_names_npe <- npe_data$param_names
    n_params_npe <- npe_data$n_params
    n_samples_npe <- npe_data$n_samples
    rm(npe_data)
    gc(verbose = FALSE)

    log_msg("Running NPE diagnostics")
    diagnostics <- calc_npe_diagnostics(
      n_sbc_sims = 10,
      model = npe_model,
      npe_model_dir = npe_model$output_dir,
      PATHS = PATHS,
      priors = priors,
      config_base = config,
      param_names = param_names_npe,
      n_npe_samples = 100,
      probs = c(0.025, 0.25, 0.75, 0.975),
      output_dir = npe_dirs$diagnostics,
      verbose = FALSE,
      parallel = FALSE,
      n_cores = 6
    )

    log_msg("Estimating posteriors")
    posterior_result <- estimate_npe_posterior(
      model = npe_model,
      observed_data = observed_data,
      n_samples = 1000,
      return_log_probs = TRUE,
      output_dir = npe_dirs$posterior,
      verbose = FALSE,
      rejection_sampling = TRUE,
      max_rejection_rate = 0.25,
      max_attempts = 10
    )

    posterior_samples <- posterior_result$samples
    posterior_log_probs <- posterior_result$log_probs
    posterior_quantiles <- posterior_result$quantiles

    if (is.null(colnames(posterior_samples)) && !is.null(param_names_npe)) {
      colnames(posterior_samples) <- param_names_npe
    }

    npe_result <- list(
      posterior_samples = posterior_samples,
      posterior_log_probs = posterior_log_probs,
      posterior_quantiles = posterior_quantiles,
      param_names = param_names_npe,
      architecture = arch_spec,
      diagnostics = diagnostics,
      model = npe_model
    )

    config_npe <- create_config_npe(
      posterior_result = npe_result,
      config_base = config,
      output_file = file.path(dirs$npe, "config_npe.json"),
      use_median = TRUE,
      verbose = FALSE
    )

    posteriors_npe <- fit_posterior_distributions(
      posterior_samples = posterior_samples,
      priors_file = file.path(dirs$setup, "priors.json"),
      output_file = file.path(npe_dirs$posterior, "posteriors.json"),
      verbose = FALSE
    )

    npe_summary <- list(
      n_posterior_samples = nrow(posterior_samples),
      param_names = param_names_npe,
      architecture = arch_spec,
      diagnostics = diagnostics,
      has_log_probs = !is.null(posterior_log_probs),
      rejection_info = posterior_result$rejection_info,
      timestamp = Sys.time()
    )
    npe_summary_file <- file.path(dirs$npe, "npe_summary.rds")
    saveRDS(npe_summary, npe_summary_file)
    log_msg("Saved %s", npe_summary_file)

    if (control$paths$plots && !is.null(diagnostics)) {
      plot_npe_convergence_status(
        npe_dir = dirs$npe,
        output_dir = npe_dirs$plots,
        coverage_targets = list("50" = 0.50, "95" = 0.95),
        ks_threshold = 0.05,
        show_param_details = 15,
        verbose = FALSE
      )

      plot_npe_diagnostics_coverage(
        diagnostics = diagnostics,
        plots_dir = npe_dirs$plots,
        max_params_per_page = 40,
        verbose = FALSE
      )

      plot_npe_diagnostics_coverage_bars(
        diagnostics = diagnostics,
        plots_dir = npe_dirs$plots,
        verbose = FALSE
      )

      plot_npe_diagnostics_sbc(
        diagnostics = diagnostics,
        plots_dir = npe_dirs$plots,
        max_params_per_page = 40,
        verbose = FALSE
      )

      plot_npe_training_loss(
        npe_dirs = npe_dirs,
        output_file = file.path(npe_dirs$plots, "npe_training_loss.png"),
        plot_height = 8,
        show_best_epoch = TRUE,
        smooth_curves = FALSE,
        log_scale = FALSE,
        verbose = FALSE
      )

      quantile_files <- c()
      bfrs_quantiles <- file.path(dirs$bfrs_post, "posterior_quantiles.csv")
      if (file.exists(bfrs_quantiles)) quantile_files <- c(quantile_files, bfrs_quantiles)

      npe_quantiles <- file.path(npe_dirs$posterior, "posterior_quantiles.csv")
      if (file.exists(npe_quantiles)) quantile_files <- c(quantile_files, npe_quantiles)

      if (length(quantile_files) > 0) {
        plot_model_posterior_quantiles(
          csv_files = quantile_files,
          output_dir = npe_dirs$plots,
          plot_types = "both",
          verbose = FALSE
        )
      }

      priors_json <- file.path(dirs$setup, "priors.json")
      posteriors_bfrs_json <- file.path(dirs$bfrs_post, "posteriors.json")
      posteriors_npe_json <- file.path(npe_dirs$posterior, "posteriors.json")

      json_files <- c()
      method_names <- c()

      if (file.exists(priors_json)) {
        json_files <- c(json_files, priors_json)
        method_names <- c(method_names, "Prior")
      }

      if (file.exists(posteriors_bfrs_json)) {
        json_files <- c(json_files, posteriors_bfrs_json)
        method_names <- c(method_names, "BFRS")
      }

      if (file.exists(posteriors_npe_json)) {
        json_files <- c(json_files, posteriors_npe_json)
        method_names <- c(method_names, "NPE")
      }

      if (length(json_files) >= 2) {
        plot_model_distributions(
          json_files = json_files,
          method_names = method_names,
          output_dir = npe_dirs$plots
        )
      }
    }

    if (control$paths$plots && !is.null(config_npe)) {
      plot_model_fit_stochastic(
        config = config_npe,
        n_simulations = control$predictions$best_model_n_sims,
        output_dir = npe_dirs$plots,
        envelope_quantiles = c(0.025, 0.975),
        save_predictions = TRUE,
        parallel = TRUE,
        n_cores = control$parallel$n_cores,
        root_dir = root_dir,
        verbose = FALSE
      )

      n_param_sets <- control$predictions$ensemble_n_param_sets
      param_configs <- list()

      if (nrow(posterior_samples) >= n_param_sets) {
        set.seed(123)
        sample_indices <- sample(1:nrow(posterior_samples), n_param_sets, replace = FALSE)

        for (i in 1:n_param_sets) {
          idx <- sample_indices[i]

          # Create mini posterior_result with single sample
          # CRITICAL: Use create_config_npe() to ensure location-aware parameter assignment
          # The old naive loop created new fields (e.g., tau_i_ETH) instead of updating
          # vector elements, causing ensemble to use prior values instead of posterior samples
          sample_result <- list(
            samples = matrix(posterior_samples[idx, ], nrow = 1),
            quantiles = NULL
          )
          colnames(sample_result$samples) <- colnames(posterior_samples)

          # Use create_config_npe with use_median=FALSE (mean of 1 sample = the sample itself)
          param_configs[[i]] <- create_config_npe(
            posterior_result = sample_result,
            config_base = config,
            output_file = NULL,
            use_median = FALSE,
            verbose = FALSE
          )
        }

        # CRITICAL: Use uniform weights for posterior predictive sampling
        # When samples are drawn directly from p(θ|x_obs) (as NPE does),
        # the sampling distribution already encodes the posterior.
        # Using density as weights would cause double-weighting: [p(θ|x)]²
        # This over-emphasizes high-density regions and under-represents uncertainty.
        # For posterior predictive: p(y|x) ≈ (1/N) Σ p(y|θ_i) where θ_i ~ p(θ|x)
        # Density weights are ONLY correct for importance sampling from q(θ) ≠ p(θ|x)
        param_weights <- NULL  # NULL → uniform weights in plot_model_fit_stochastic_param

        plot_model_fit_stochastic_param(
          configs = param_configs,
          parameter_weights = param_weights,
          n_simulations_per_config = control$predictions$ensemble_n_sims_per_param,
          output_dir = npe_dirs$plots,
          envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
          save_predictions = TRUE,
          parallel = TRUE,
          n_cores = control$parallel$n_cores,
          root_dir = root_dir,
          verbose = FALSE,
          plot_decomposed = FALSE
        )

        # NPE Posterior Predictive Checks
        # Create PPC diagnostics for NPE ensemble predictions (same as BFRS)
        log_msg("Creating NPE posterior predictive check plots")
        ppc_npe_result <- tryCatch(
          {
            plot_model_ppc(
              predictions_dir = npe_dirs$plots,
              output_dir = npe_dirs$plots,
              verbose = control$logging$verbose
            )
          },
          error = function(e) {
            log_msg("Warning: NPE PPC plots failed: %s", e$message)
            NULL
          }
        )
      }

      gc(verbose = FALSE)
    }

  } else {
    log_msg("NPE skipped")
  }

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  log_msg("Calibration complete: %d batches, %d simulations, %.2f min",
          state$batch_number, state$total_sims_run, as.numeric(runtime))

  invisible(list(
    dirs = dirs,
    files = list(
      simulations = simulations_file,
      ess_csv = file.path(dirs$bfrs_diag, "parameter_ess.csv"),
      posterior_quantiles = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
      posteriors_json = file.path(dirs$bfrs_post, "posteriors.json")
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
#' Creates a complete control structure for \code{run_mosaic()} and \code{run_mosaic_iso()}.
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
#'   \item \code{npe}: Post-calibration stage (neural posterior estimation)
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
#'     \item \code{add_max_terms}: Add negative binomial time-series likelihood (default: FALSE)
#'     \item \code{add_peak_timing}: Add Gaussian peak timing penalty (default: FALSE)
#'     \item \code{add_peak_magnitude}: Add log-normal peak magnitude penalty (default: FALSE)
#'     \item \code{add_cumulative_total}: Add cumulative total penalty (default: FALSE)
#'     \item \code{add_wis}: Add Weighted Interval Score (default: FALSE)
#'     \item \code{weight_cases}: Weight for cases vs deaths (default: 1.0)
#'     \item \code{weight_deaths}: Weight for deaths vs cases (default: 1.0)
#'     \item \code{enable_guardrails}: Enable sanity checks (default: FALSE)
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
#' @param npe List of NPE settings (post-calibration stage). Default is:
#'   \itemize{
#'     \item \code{enable}: Enable NPE training (default: FALSE)
#'     \item \code{weight_strategy}: Weight strategy: "continuous_best", "continuous_retained", "continuous_all", "binary_best", "binary_retained", "binary_all" (default: "continuous_best")
#'     \item \code{architecture_tier}: Architecture size: "auto", "minimal", "small", "medium", "large", "xlarge" (default: "auto")
#'     \item \code{n_epochs}: Maximum training epochs (default: 1000)
#'     \item \code{batch_size}: Training batch size (default: 512)
#'     \item \code{learning_rate}: Initial learning rate (default: 1e-3)
#'     \item \code{validation_split}: Proportion for validation (default: 0.15)
#'     \item \code{early_stopping}: Enable early stopping (default: TRUE)
#'     \item \code{patience}: Early stopping patience in epochs (default: 20)
#'     \item \code{use_gpu}: Use GPU if available (default: TRUE)
#'     \item \code{seed}: Random seed for reproducibility (default: 42)
#'     \item \code{n_posterior_samples}: Number of posterior samples to draw (default: 10000)
#'   }
#'
#' @param predictions List of prediction generation settings. Default is:
#'   \itemize{
#'     \item \code{best_model_n_sims}: Stochastic runs for best model (default: 100L)
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
#' @return A complete control list suitable for passing to \code{run_mosaic()} or \code{run_mosaic_iso()}.
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
#' # Enable peak timing and magnitude penalties in likelihood
#' ctrl <- mosaic_control_defaults(
#'   likelihood = list(
#'     add_peak_timing = TRUE,
#'     add_peak_magnitude = TRUE,
#'     weight_peak_timing = 0.5,
#'     weight_peak_magnitude = 0.5
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
#'   likelihood = list(add_peak_timing = TRUE, weight_cases = 1.0),   # How to score
#'   targets = list(ESS_param = 500, ESS_param_prop = 0.95),          # When to stop
#'   fine_tuning = list(batch_sizes = list(final = 200)),             # Advanced calibration
#'   npe = list(enable = TRUE, weight_strategy = "best_subset"),      # Post-calibration
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
                           npe = NULL,
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
    sample_sigma = TRUE,             # Reporting rate
    sample_kappa = TRUE,             # Overdispersion parameter

    # Environmental decay
    sample_decay_days_long = TRUE,   # Long-term environmental decay
    sample_decay_days_short = TRUE,  # Short-term environmental decay
    sample_decay_shape_1 = TRUE,     # Decay shape parameter 1
    sample_decay_shape_2 = TRUE,     # Decay shape parameter 2

    # Advanced parameters
    sample_zeta_1 = TRUE,            # Advanced parameter 1
    sample_zeta_2 = TRUE,            # Advanced parameter 2

    # === LOCATION-SPECIFIC PARAMETERS (13) ===
    # Transmission and seasonality
    sample_beta_j0_tot = TRUE,       # Baseline transmission rate by location
    sample_p_beta = TRUE,            # Proportion of seasonality
    sample_tau_i = TRUE,             # Rainfall effect timing
    sample_theta_j = TRUE,           # Temperature seasonal effect
    sample_mu_j = TRUE,              # Baseline rate by location

    # Climate relationship
    sample_a1 = TRUE,                # Temperature coefficient 1
    sample_a2 = TRUE,                # Temperature coefficient 2
    sample_b1 = TRUE,                # Rainfall coefficient 1
    sample_b2 = TRUE,                # Rainfall coefficient 2

    # Psi-star calibration
    sample_psi_star_a = TRUE,        # Psi-star parameter a
    sample_psi_star_b = TRUE,        # Psi-star parameter b
    sample_psi_star_z = TRUE,        # Psi-star parameter z
    sample_psi_star_k = TRUE,        # Psi-star parameter k

    # === INITIAL CONDITIONS (1) ===
    sample_initial_conditions = TRUE  # Initial compartment proportions
  )

  # Default likelihood calculation settings
  default_likelihood <- list(
    # === Toggle components ===
    add_max_terms = FALSE,           # Negative binomial time-series likelihood (baseline always included)
    add_peak_timing = FALSE,         # Gaussian penalty on peak timing mismatch
    add_peak_magnitude = FALSE,      # Log-normal penalty on peak magnitude mismatch
    add_cumulative_total = FALSE,    # Penalty on cumulative case/death mismatch
    add_wis = FALSE,                 # Weighted Interval Score (probabilistic scoring)

    # === Component weights ===
    weight_cases = 1.0,              # Weight for cases vs deaths
    weight_deaths = 1.0,             # Weight for deaths vs cases
    weight_max_terms = 0.5,          # Weight for time-series likelihood
    weight_peak_timing = 0.5,        # Weight for peak timing penalty
    weight_peak_magnitude = 0.5,     # Weight for peak magnitude penalty
    weight_cumulative_total = 0.3,   # Weight for cumulative mismatch
    weight_wis = 0.8,                # Weight for WIS score

    # === Peak controls ===
    sigma_peak_time = 1,             # Std dev for peak timing Gaussian (in time steps)
    sigma_peak_log = 0.5,            # Std dev for log peak magnitude
    penalty_unmatched_peak = -3,     # Penalty when peak not detected

    # === Guardrails ===
    enable_guardrails = FALSE,       # Enable sanity checks on model output
    floor_likelihood = -999999999,   # Floor value for invalid likelihoods
    guardrail_verbose = FALSE        # Print guardrail diagnostics
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
    ESS_param = 500,
    ESS_param_prop = 0.95,
    ESS_best = 100,
    A_best = 0.95,
    CVw_best = 0.5,
    percentile_min = 0.001,
    percentile_max = 5.0,
    ESS_method = "perplexity"     # ESS calculation method: "kish" or "perplexity"
  )

  # Default NPE settings
  default_npe <- list(
    enable = FALSE,
    weight_strategy = "continuous_best",  # Weight strategy for NPE training
    architecture_tier = "auto",           # Architecture size: "auto", "minimal", "small", "medium", "large", "xlarge"
    n_epochs = 1000,                      # Maximum training epochs
    batch_size = 512,                     # Training batch size
    learning_rate = 1e-3,                 # Initial learning rate
    validation_split = 0.15,              # Proportion of data for validation
    early_stopping = TRUE,                # Enable early stopping
    patience = 20,                        # Early stopping patience (epochs)
    use_gpu = TRUE,                       # Use GPU if available
    seed = 42,                            # Random seed for reproducibility
    n_posterior_samples = 10000           # Number of posterior samples to draw after training
  )

  # Default prediction settings
  default_predictions <- list(
    best_model_n_sims = 100L,           # Stochastic runs for best model
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
    verbose_weights = FALSE            # Print detailed weight calculation diagnostics
  )

  # Default logging settings
  default_logging <- list(
    verbose = FALSE                    # Enable detailed progress messages in sub-functions
  )

  # Merge user-provided settings with defaults
  # Order follows workflow: calibration → sampling → likelihood → targets → fine_tuning → npe → predictions → weights → parallel → io → paths → logging
  list(
    calibration = if (is.null(calibration)) default_calibration else modifyList(default_calibration, calibration),
    sampling = if (is.null(sampling)) default_sampling else modifyList(default_sampling, sampling),
    likelihood = if (is.null(likelihood)) default_likelihood else modifyList(default_likelihood, likelihood),
    targets = if (is.null(targets)) default_targets else modifyList(default_targets, targets),
    fine_tuning = if (is.null(fine_tuning)) default_fine_tuning else modifyList(default_fine_tuning, fine_tuning),
    npe = if (is.null(npe)) default_npe else modifyList(default_npe, npe),
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
