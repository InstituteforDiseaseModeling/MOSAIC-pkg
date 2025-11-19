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
          # Double the size
          new_rows <- matrix(NA_character_, nrow = nrow(output_matrix), ncol = 6)
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
      reticulate::py_gc()
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
  reticulate::py_gc()

  return(file.exists(output_file))
}

# =============================================================================
# PUBLIC API FUNCTIONS
# =============================================================================

#' Run MOSAIC Calibration by ISO Code (Simple Interface)
#'
#' @description
#' **This is the recommended interface for most users.** Simplified wrapper that loads
#' default config and priors for specified ISO codes and runs the full MOSAIC calibration.
#'
#' For advanced customization (custom configs, non-standard locations), use [run_mosaic()] directly.
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
#' @param iso_code Character vector of ISO3 codes (e.g., \code{"ETH"} or \code{c("ETH", "KEN", "TZA")}).
#'   Determines which locations to model. Config and priors are automatically loaded for these locations.
#' @param dir_output Character. Output directory for this calibration run (REQUIRED).
#'   All results will be saved here. Must be unique per run.
#' @param control Control list created with [mosaic_control_defaults()]. If \code{NULL}, uses defaults.
#'   Common settings:
#'   \itemize{
#'     \item \code{calibration$n_simulations}: NULL for auto mode, integer for fixed
#'     \item \code{calibration$n_iterations}: Iterations per simulation (default: 3)
#'     \item \code{parallel$enable}: Enable parallel execution (default: FALSE)
#'     \item \code{parallel$n_cores}: Number of cores (default: 1)
#'   }
#' @param resume Logical. If \code{TRUE}, continues from existing checkpoint. Default: FALSE.
#'
#' @return Invisibly returns a list with:
#' \describe{
#'   \item{dirs}{Named list of output directories}
#'   \item{files}{Named list of key output files}
#'   \item{summary}{Named list with run statistics}
#' }
#'
#' @examples
#' \dontrun{
#' # === SIMPLE USAGE ===
#'
#' # Single location with defaults
#' run_mosaic_iso("ETH", "./output")
#'
#' # Multiple locations
#' run_mosaic_iso(c("ETH", "KEN", "TZA"), "./output")
#'
#' # === WITH CONTROL SETTINGS ===
#'
#' # Parallel execution with 8 cores
#' run_mosaic_iso(
#'   iso_code = "ETH",
#'   dir_output = "./output",
#'   control = mosaic_control_defaults(
#'     parallel = list(enable = TRUE, n_cores = 8)
#'   )
#' )
#'
#' # Fixed mode: exactly 5000 simulations, 5 iterations each
#' run_mosaic_iso(
#'   iso_code = "ETH",
#'   dir_output = "./output",
#'   control = mosaic_control_defaults(
#'     calibration = list(
#'       n_simulations = 5000,
#'       n_iterations = 5
#'     )
#'   )
#' )
#'
#' # Auto mode with custom maximum
#' run_mosaic_iso(
#'   iso_code = "ETH",
#'   dir_output = "./output",
#'   control = mosaic_control_defaults(
#'     calibration = list(
#'       n_simulations = NULL,  # NULL = auto mode
#'       max_simulations = 50000,
#'       batch_size = 1000
#'     ),
#'     parallel = list(enable = TRUE, n_cores = 16)
#'   )
#' )
#'
#' # === SAMPLE SPECIFIC PARAMETERS ===
#'
#' # Only sample transmission and recovery, hold others fixed
#' run_mosaic_iso(
#'   iso_code = "ETH",
#'   dir_output = "./output",
#'   control = mosaic_control_defaults(
#'     sampling = list(
#'       sample_tau_i = TRUE,
#'       sample_mobility_gamma = FALSE,
#'       sample_mobility_omega = FALSE,
#'       sample_mu_j = TRUE,
#'       sample_iota = FALSE,
#'       sample_gamma_2 = FALSE,
#'       sample_alpha_1 = FALSE
#'     )
#'   )
#' )
#' }
#'
#' @seealso [run_mosaic()] for advanced usage with custom configs
#' @seealso [mosaic_control_defaults()] for building control structures
#' @export
run_mosaic_iso <- function(iso_code,
                           dir_output,
                           control = NULL,
                           resume = FALSE) {

  # Input validation
  if (missing(iso_code) || is.null(iso_code)) {
    stop("iso_code is required", call. = FALSE)
  }
  if (!is.character(iso_code) || length(iso_code) == 0) {
    stop("iso_code must be a character vector with at least one ISO3 code", call. = FALSE)
  }
  if (missing(dir_output) || is.null(dir_output)) {
    stop("dir_output is required", call. = FALSE)
  }

  # Load default config and priors for specified locations
  log_msg("Loading default config for: %s", paste(iso_code, collapse = ", "))
  config <- get_location_config(iso = iso_code)

  log_msg("Loading default priors for: %s", paste(iso_code, collapse = ", "))
  priors <- get_location_priors(iso = iso_code)

  # Use mosaic_control_defaults() defaults if control not provided
  if (is.null(control)) {
    control <- mosaic_control_defaults()
  }

  # Call main run_MOSAIC() function
  run_MOSAIC(
    config = config,
    priors = priors,
    dir_output = dir_output,
    control = control,
    resume = resume
  )
}

#' @rdname run_mosaic_iso
#' @export
run_MOSAIC_iso <- run_mosaic_iso

#' Run MOSAIC Calibration Workflow (Advanced Interface)
#'
#' @description
#' **Advanced interface with full control over model specification.**
#' For most users, [run_mosaic_iso()] provides a simpler interface.
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
#' @seealso [run_mosaic_iso()] for simple interface with ISO codes
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
  # ===========================================================================

  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("MOSAIC-LASER Calibration — run_mosaic()")
  log_msg(paste(rep("=", 80), collapse = ""))

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

  .mosaic_write_json(sim_params, file.path(dirs$setup, "simulation_params.json"), control$io)
  .mosaic_write_json(priors, file.path(dirs$setup, "priors.json"), control$io)
  .mosaic_write_json(config, file.path(dirs$setup, "config_base.json"), control$io)

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

  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("ADAPTIVE SIMULATION — Mode: %s", toupper(state$mode))
  log_msg(paste(rep("=", 80), collapse = ""))

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
  # ===========================================================================

  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("COMBINING SIMULATION OUTPUT FILES")
  log_msg(paste(rep("=", 80), collapse = ""))

  # Get list of simulation files before loading
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

  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("CALCULATING PARAMETER-SPECIFIC ESS")
  log_msg(paste(rep("=", 80), collapse = ""))

  # Check if we have enough samples
  n_valid <- sum(!is.na(results$likelihood) & is.finite(results$likelihood))

  if (n_valid < 50) {
    log_msg("Skipping parameter-specific ESS: %d valid samples (need at least 50)", n_valid)
    ess_results <- NULL
  } else {
    log_msg("Calculating marginal ESS for each parameter...")
    ess_results <- calc_model_ess_parameter(
      results = results,
      param_names = param_names_estimated,
      likelihood_col = "likelihood",
      n_grid = 100,
      method = control$targets$ESS_method,
      verbose = TRUE
    )
  }

  # Save ESS results to CSV (if calculated)
  if (!is.null(ess_results)) {
    ess_output_file <- file.path(dirs$bfrs_diag, "parameter_ess.csv")
    write.csv(ess_results, ess_output_file, row.names = FALSE)
    log_msg("Parameter ESS summary (%d params): Mean=%.1f, Median=%.1f, Min=%.1f, Max=%.1f",
            nrow(ess_results),
            mean(ess_results$ess_marginal, na.rm = TRUE),
            median(ess_results$ess_marginal, na.rm = TRUE),
            min(ess_results$ess_marginal, na.rm = TRUE),
            max(ess_results$ess_marginal, na.rm = TRUE))

    # Identify parameters needing more samples
    low_ess <- ess_results[ess_results$ess_marginal < control$targets$ESS_param, ]
    if (nrow(low_ess) > 0) {
      log_msg("\nParameters with ESS < %d (%d):", control$targets$ESS_param, nrow(low_ess))
      for (i in 1:min(10, nrow(low_ess))) {
      log_msg("  %s: ESS = %.1f",
              low_ess$parameter[i],
              low_ess$ess_marginal[i])
    }
      if (nrow(low_ess) > 10) {
        log_msg("  ... and %d more", nrow(low_ess) - 10)
      }
    } else {
      log_msg("\nAll parameters have ESS >= %d (well converged)", control$targets$ESS_param)
    }
  }

  log_msg(paste(rep("=", 80), collapse = ""))

  # ===========================================================================
  # POST-HOC SUBSET OPTIMIZATION
  # ===========================================================================

  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("POST-HOC OPTIMIZATION: FINDING OPTIMAL SUBSET FOR NPE PRIORS")
  log_msg(paste(rep("=", 80), collapse = ""))

  log_msg("\nTrying tiered criteria for optimal subset selection...")
  log_msg("  Max search percentile: %.1f%%", control$targets$percentile_max)

  # Define tiered criteria for post-hoc optimization
  subset_tiers <- get_default_subset_tiers(
    target_ESS_best = control$targets$ESS_best,
    target_A = control$targets$A_best,
    target_CVw = control$targets$CVw_best,
    n_tiers = 15
  )

  # Try each tier in order until convergence
  optimal_subset_result <- NULL
  tier_used <- NULL

  for (tier_name in names(subset_tiers)) {
    tier <- subset_tiers[[tier_name]]

    log_msg("\n--- Trying %s tier ---", tier$name)
    log_msg("  Criteria: A≥%.2f, CVw≤%.2f, ESS_B≥%.0f",
            tier$A, tier$CVw, tier$ESS_B)

    tier_result <- identify_best_subset(
      results = results,
      min_B = control$targets$B_min,
      target_ESS_B = tier$ESS_B,
      target_A = tier$A,
      target_CVw = tier$CVw,
      min_percentile = 0.001,
      max_percentile = control$targets$percentile_max,
      precision = 0.001,
      verbose = FALSE
    )

    if (tier_result$converged) {
      log_msg("  ✓ CONVERGED: %.1f%% (%d simulations)",
              tier_result$percentile_used,
              tier_result$n_selected)
      log_msg("    Metrics: ESS_B=%.1f, A=%.3f, CVw=%.3f",
              tier_result$metrics$ESS_B,
              tier_result$metrics$A,
              tier_result$metrics$CVw)
      optimal_subset_result <- tier_result
      tier_used <- tier$name
      break
    } else {
      log_msg("  ✗ Not converged")
    }
  }

  # Decide which subset to use
  if (!is.null(optimal_subset_result)) {
    log_msg("\n✓ OPTIMIZATION SUCCESS: Converged with %s criteria", tier_used)
    log_msg("  Using %.1f%% (%d simulations) for NPE priors",
            optimal_subset_result$percentile_used,
            optimal_subset_result$n_selected)

    top_subset_final <- optimal_subset_result$subset
    percentile_used <- optimal_subset_result$percentile_used
    n_top_final <- optimal_subset_result$n_selected
    convergence_tier <- tier_used

  } else {
    log_msg("\n⚠ No tier achieved convergence")
    log_msg("  Falling back to maximum allowed: top %.1f%%", control$targets$percentile_max)

    fallback_percentile <- control$targets$percentile_max
    n_top_final <- ceiling(nrow(results) * fallback_percentile / 100)
    results_ranked_final <- results[order(results$likelihood, decreasing = TRUE), ]
    top_subset_final <- results_ranked_final[1:n_top_final, ]
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
      # Use unified adaptive weight method for consistency with weight_best column
      # Note: top_subset_final is the data frame of selected simulations at this point
      weight_result_final <- .mosaic_calc_adaptive_gibbs_weights(
        likelihood = top_subset_final$likelihood,
        weight_floor = control$weights$floor,
        verbose = FALSE
      )

      weights_final <- weight_result_final$weights
      w_tilde_final <- weights_final
      w_final <- weights_final * length(weights_final)

      # Calculate metrics (use control ESS_method)
      ESS_B_final <- calc_model_ess(w_tilde_final, method = control$targets$ESS_method)
      ag_final <- calc_model_agreement_index(w_final)
      A_final <- ag_final$A
      CVw_final <- calc_model_cvw(w_final)
      gibbs_temperature_final <- weight_result_final$temperature

      log_msg("  Subset selection weights (unified adaptive method):")
      log_msg("    Adaptive effective range: %.1f (actual range: %.1f)",
              weight_result_final$effective_range, weight_result_final$metrics$actual_range)
      log_msg("    Temperature: %.4f", weight_result_final$temperature)
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
  log_msg("\nMarked %d simulations as best subset", sum(results$is_best_subset))

  # ===========================================================================
  # CALCULATE ANALYSIS WEIGHTS (UNIFIED ADAPTIVE METHOD)
  # ===========================================================================

  log_msg("\nCalculating analysis weights using adaptive Gibbs method...")

  # Initialize weight columns
  results$weight_all <- 0
  results$weight_retained <- 0
  results$weight_best <- 0

  # ---------------------------------------------------------------------------
  # Weight ALL valid simulations (for NPE continuous_all strategy)
  # ---------------------------------------------------------------------------

  if (sum(results$is_valid) > 0) {
    log_msg("\n--- ALL Simulations (Valid) ---")

    all_result <- .mosaic_calc_adaptive_gibbs_weights(
      likelihood = results$likelihood[results$is_valid],
      weight_floor = control$weights$floor,
      verbose = control$io$verbose_weights
    )

    results$weight_all[results$is_valid] <- all_result$weights

    log_msg("  n=%d | Range: %.1f (actual: %.1f) | T=%.4f | ESS: %.1f (Kish), %.1f (Perp)",
            sum(results$is_valid), all_result$effective_range,
            all_result$metrics$actual_range, all_result$temperature,
            all_result$metrics$ESS_kish, all_result$metrics$ESS_perplexity)
  }

  # ---------------------------------------------------------------------------
  # Weight RETAINED models (for plotting, ensemble predictions)
  # ---------------------------------------------------------------------------

  if (sum(results$is_retained) > 0) {
    log_msg("\n--- RETAINED Subset ---")

    retained_result <- .mosaic_calc_adaptive_gibbs_weights(
      likelihood = results$likelihood[results$is_retained],
      weight_floor = control$weights$floor,
      verbose = control$io$verbose_weights
    )

    results$weight_retained[results$is_retained] <- retained_result$weights

    log_msg("  n=%d | Range: %.1f (actual: %.1f) | T=%.4f | ESS: %.1f (Kish), %.1f (Perp)",
            sum(results$is_retained), retained_result$effective_range,
            retained_result$metrics$actual_range, retained_result$temperature,
            retained_result$metrics$ESS_kish, retained_result$metrics$ESS_perplexity)
  }

  # ---------------------------------------------------------------------------
  # Weight BEST subset (for NPE priors, posterior inference)
  # ---------------------------------------------------------------------------

  if (sum(results$is_best_subset) > 0) {
    log_msg("\n--- BEST Subset ---")

    best_result <- .mosaic_calc_adaptive_gibbs_weights(
      likelihood = results$likelihood[results$is_best_subset],
      weight_floor = control$weights$floor,
      verbose = control$io$verbose_weights
    )

    results$weight_best[results$is_best_subset] <- best_result$weights
    ESS_best <- best_result$metrics$ESS_kish

    log_msg("  n=%d | Range: %.1f (actual: %.1f) | T=%.4f | ESS: %.1f (Kish), %.1f (Perp)",
            sum(results$is_best_subset), best_result$effective_range,
            best_result$metrics$actual_range, best_result$temperature,
            best_result$metrics$ESS_kish, best_result$metrics$ESS_perplexity)
  }

  # Save subset selection summary
  subset_summary <- data.frame(
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
  log_msg("Subset selection summary saved to: %s", basename(summary_file))

  # Re-save simulations.parquet with weight columns
  .mosaic_write_parquet(results, simulations_file, control$io)
  log_msg("Updated simulations.parquet with weight columns")

  # ===========================================================================
  # CONVERGENCE ANALYSIS
  # ===========================================================================

  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("CONVERGENCE ANALYSIS")
  log_msg(paste(rep("=", 80), collapse = ""))

  # Calculate metrics for retained models
  retained_results <- results[results$is_retained, ]
  n_retained <- nrow(retained_results)

  if (n_retained > 0) {
    retained_weights_norm <- results$weight_retained[results$is_retained]
    ess_retained <- calc_model_ess(retained_weights_norm, method = control$targets$ESS_method)
  } else {
    ess_retained <- NA_real_
  }

  # Best subset metrics already calculated above
  best_results <- results[results$is_best_subset, ]
  n_best <- nrow(best_results)

  # Validate convergence diagnostic inputs before calling function
  # This ensures graceful handling when convergence not reached
  diagnostics_valid <- TRUE
  diagnostics_issues <- character()

  # Check if n_best is valid
  if (!is.numeric(n_best) || length(n_best) != 1 || n_best < 0) {
    diagnostics_valid <- FALSE
    diagnostics_issues <- c(diagnostics_issues, sprintf("n_best invalid: %s", n_best))
  }

  # Check if n_best <= n_retained
  if (is.numeric(n_best) && is.numeric(n_retained) && n_best > n_retained) {
    diagnostics_valid <- FALSE
    diagnostics_issues <- c(diagnostics_issues,
                           sprintf("n_best (%d) > n_retained (%d)", n_best, n_retained))
  }

  # Check if metrics are finite
  if (!is.finite(ESS_B_final)) {
    diagnostics_valid <- FALSE
    diagnostics_issues <- c(diagnostics_issues, "ESS_B_final is not finite")
  }

  if (!is.finite(A_final)) {
    diagnostics_valid <- FALSE
    diagnostics_issues <- c(diagnostics_issues, "A_final is not finite")
  }

  if (!is.finite(CVw_final)) {
    diagnostics_valid <- FALSE
    diagnostics_issues <- c(diagnostics_issues, "CVw_final is not finite")
  }

  # Calculate convergence diagnostics if inputs are valid
  if (diagnostics_valid) {
    diagnostics <- calc_convergence_diagnostics(
      # Metrics
      n_total = nrow(results),
      n_successful = sum(is.finite(results$likelihood)),
      n_retained = n_retained,
      n_best_subset = n_best,
      ess_retained = ess_retained,
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
      target_B_min = control$targets$B_min,
      target_percentile_max = control$targets$percentile_max,
      target_ess_param = control$targets$ESS_param,
      target_ess_param_prop = control$targets$ESS_param_prop,

      # Settings
      ess_method = control$targets$ESS_method,
      temperature = gibbs_temperature_final,
      verbose = TRUE
    )
  } else {
    # Create minimal diagnostics structure with NA values
    log_msg("\n⚠ WARNING: Cannot calculate convergence diagnostics")
    log_msg("  Issues detected:")
    for (issue in diagnostics_issues) {
      log_msg("    - %s", issue)
    }
    log_msg("  Proceeding with NA diagnostics (convergence not reached)")

    diagnostics <- list(
      converged = FALSE,
      status = "FAIL",
      issues = diagnostics_issues,
      n_total = nrow(results),
      n_successful = sum(is.finite(results$likelihood)),
      n_retained = n_retained,
      n_best_subset = if(is.numeric(n_best)) n_best else NA_integer_,
      ess_retained = ess_retained,
      ess_best = ESS_B_final,
      A_best = A_final,
      cvw_best = CVw_final,
      percentile_used = percentile_used,
      convergence_tier = convergence_tier,
      message = "Convergence diagnostics could not be calculated due to insufficient data"
    )
  }

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
  log_msg("Saved convergence results to: convergence_results.parquet")

  # Save diagnostics JSON
  diagnostics_file <- file.path(dirs$bfrs_diag, "convergence_diagnostics.json")
  jsonlite::write_json(diagnostics, diagnostics_file, pretty = TRUE, auto_unbox = TRUE)
  log_msg("Saved convergence diagnostics to: convergence_diagnostics.json")

  # Generate convergence plots
  if (control$paths$plots) {
    tryCatch({
      plot_model_convergence(
        results_dir = dirs$bfrs_diag,
        plots_dir = dirs$bfrs_plots_diag,
        verbose = TRUE
      )
      log_msg("Convergence plots created successfully")
    }, error = function(e) {
      log_msg("ERROR creating convergence plots: %s", e$message)
    })

    tryCatch({
      plot_model_convergence_status(
        results_dir = dirs$bfrs_diag,
        plots_dir = dirs$bfrs_plots_diag,
        verbose = TRUE
      )
      log_msg("Convergence status table created successfully")
    }, error = function(e) {
      log_msg("ERROR creating convergence status table: %s", e$message)
    })
  }

  # ===========================================================================
  # POSTERIOR QUANTILES AND DISTRIBUTIONS
  # ===========================================================================

  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("CALCULATING POSTERIOR QUANTILES")
  log_msg(paste(rep("=", 80), collapse = ""))

  # Calculate posterior quantiles
  posterior_quantiles <- tryCatch({
    calc_model_posterior_quantiles(
      results = results,
      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
      output_dir = dirs$bfrs_post,
      verbose = TRUE
    )
  }, error = function(e) {
    log_msg("ERROR calculating posterior quantiles: %s", e$message)
    NULL
  })

  if (!is.null(posterior_quantiles)) {
    log_msg("Posterior quantiles: %d parameters → posterior_quantiles.csv",
            nrow(posterior_quantiles))

    # Generate posterior quantiles plots
    if (control$paths$plots) {
      log_msg(paste(rep("=", 80), collapse = ""))
      log_msg("GENERATING POSTERIOR QUANTILES PLOTS")
      log_msg(paste(rep("=", 80), collapse = ""))

      tryCatch({
        plot_model_posterior_quantiles(
          csv_files = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
          output_dir = dirs$bfrs_plots_post,
          verbose = TRUE
        )
        log_msg("Posterior quantiles plots created successfully")
      }, error = function(e) {
        log_msg("ERROR creating posterior quantiles plots: %s", e$message)
      })
    }
  }

  # Calculate posterior distributions
  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("CALCULATING POSTERIOR DISTRIBUTIONS")
  log_msg(paste(rep("=", 80), collapse = ""))

  posterior_analysis <- tryCatch({
    calc_model_posterior_distributions(
      quantiles_file = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
      priors_file = file.path(dirs$setup, "priors.json"),
      output_dir = dirs$bfrs_post,
      verbose = TRUE
    )
  }, error = function(e) {
    log_msg("ERROR calculating posterior distributions: %s", e$message)
    NULL
  })

  if (!is.null(posterior_analysis)) {
    log_msg("Posterior distributions → posteriors.json")

    # Generate posterior distribution plots
    if (control$paths$plots) {
      tryCatch({
        plot_model_distributions(
          json_files = c(file.path(dirs$setup, "priors.json"),
                        file.path(dirs$bfrs_post, "posteriors.json")),
          method_names = c("Prior", "Posterior"),
          output_dir = dirs$bfrs_plots_post
        )
        log_msg("Posterior distribution plots created successfully")
      }, error = function(e) {
        log_msg("ERROR creating posterior distribution plots: %s", e$message)
      })

      tryCatch({
        plot_model_posteriors_detail(
          quantiles_file = file.path(dirs$bfrs_post, "posterior_quantiles.csv"),
          results_file = file.path(dirs$bfrs_out, "simulations.parquet"),
          priors_file = file.path(dirs$setup, "priors.json"),
          posteriors_file = file.path(dirs$bfrs_post, "posteriors.json"),
          output_dir = file.path(dirs$bfrs_plots_post, "detail"),
          verbose = TRUE
        )
        log_msg("Detailed posterior plots created successfully")
      }, error = function(e) {
        log_msg("ERROR creating detailed posterior plots: %s", e$message)
      })
    }
  }

  # ===========================================================================
  # POSTERIOR PREDICTIVE CHECKS
  # ===========================================================================

  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("POSTERIOR PREDICTIVE CHECKS")
  log_msg(paste(rep("=", 80), collapse = ""))

  # Find best model
  best_idx <- which.max(results$likelihood)
  if (length(best_idx) > 0) {
    best_seed_sim <- results$seed_sim[best_idx]
    best_likelihood <- results$likelihood[best_idx]
    log_msg("Best model identified: sim %d (seed %d) with likelihood %.2f",
            results$sim[best_idx], best_seed_sim, best_likelihood)

    # Re-run best model for detailed analysis
    log_msg("Re-running best model for PPC and detailed plots")

    config_best <- tryCatch({
      sample_parameters(
        PATHS = PATHS,
        priors = priors,
        config = config,
        seed = best_seed_sim,
        sample_args = sampling_args,
        verbose = FALSE
      )
    }, error = function(e) {
      log_msg("ERROR sampling best model parameters: %s", e$message)
      NULL
    })

    if (!is.null(config_best)) {
      # Save best configuration
      jsonlite::write_json(config_best, file.path(dirs$bfrs_cfg, "config_best.json"),
                          pretty = TRUE, auto_unbox = TRUE)

      # Import laser-cholera
      lc <- reticulate::import("laser_cholera.metapop.model")

      # Run best model
      best_model <- tryCatch({
        lc$run_model(paramfile = config_best, quiet = TRUE)
      }, error = function(e) {
        log_msg("ERROR running best model: %s", e$message)
        NULL
      })

      if (!is.null(best_model) && control$paths$plots) {
        # Generate PPC plots
        tryCatch({
          plot_model_ppc(
            model = best_model,
            output_dir = dirs$bfrs_plots_diag,
            verbose = TRUE
          )
          log_msg("PPC plots created successfully")
        }, error = function(e) {
          log_msg("ERROR creating PPC plots: %s", e$message)
        })

        # Generate stochastic fit plot for best model
        log_msg("Generating best model fit with stochastic uncertainty...")
        log_msg("  Using %d stochastic simulations", control$predictions$best_model_n_sims)
        tryCatch({
          plot_model_fit_stochastic(
            config = config_best,
            n_simulations = control$predictions$best_model_n_sims,
            output_dir = dirs$bfrs_plots_pred,
            envelope_quantiles = c(0.025, 0.975),
            save_predictions = TRUE,
            parallel = TRUE,
            n_cores = control$parallel$n_cores,
            root_dir = root_dir,
            verbose = TRUE
          )
          log_msg("Best model stochastic fit plot generated successfully")
        }, error = function(e) {
          log_msg("ERROR generating best model stochastic fit plot: %s", e$message)
        })
      }
    }
  } else {
    log_msg("WARNING: No best model identified")
  }

  # ===========================================================================
  # PARAMETER + STOCHASTIC UNCERTAINTY PLOTS
  # ===========================================================================

  if (control$paths$plots && sum(results$is_best_subset) > 0) {
    log_msg(paste(rep("=", 80), collapse = ""))
    log_msg("GENERATING PARAMETER + STOCHASTIC UNCERTAINTY PLOTS")
    log_msg(paste(rep("=", 80), collapse = ""))

    # Use best subset for parameter uncertainty
    best_subset_results <- results[results$is_best_subset == TRUE, ]

    if (nrow(best_subset_results) > 0) {
      # Get parameter seeds and weights
      param_seeds <- best_subset_results$seed_sim
      param_weights <- best_subset_results$weight_best[best_subset_results$weight_best > 0]

      # Ensure weights sum to 1
      if (length(param_weights) > 0) {
        param_weights <- param_weights / sum(param_weights)
      } else {
        param_weights <- NULL
      }

      if (length(param_weights) > 0) {
        log_msg("Ensemble uncertainty: %d param sets (weights: %.4f-%.4f) × %d sims = %d total",
                length(param_seeds), min(param_weights), max(param_weights),
                control$predictions$ensemble_n_sims_per_param,
                length(param_seeds) * control$predictions$ensemble_n_sims_per_param)
      } else {
        log_msg("Ensemble uncertainty: %d param sets × %d sims = %d total",
                length(param_seeds), control$predictions$ensemble_n_sims_per_param,
                length(param_seeds) * control$predictions$ensemble_n_sims_per_param)
      }

      tryCatch({
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
          verbose = TRUE
        )
        log_msg("Parameter + stochastic uncertainty plots generated successfully")
      }, error = function(e) {
        log_msg("ERROR generating parameter + stochastic plots: %s", e$message)
      })
    } else {
      log_msg("WARNING: No models in best subset for parameter uncertainty analysis")
    }
  }

  # ===========================================================================
  # STAGE 2: NEURAL POSTERIOR ESTIMATION (NPE)
  # ===========================================================================

  if (control$npe$enable) {
    log_msg(paste(rep("=", 80), collapse = ""))
    log_msg("STAGE 2: NEURAL POSTERIOR ESTIMATION")
    log_msg(paste(rep("=", 80), collapse = ""))

    # Select NPE weights from BFRS results
    log_msg("\nSelecting NPE weights from BFRS results...")
    log_msg("Using strategy: %s", control$npe$weight_strategy)

    npe_weights <- get_npe_weights(
      bfrs_results = results,
      strategy = control$npe$weight_strategy,
      verbose = TRUE
    )

    # Add weights to results for NPE
    results$weight_npe <- npe_weights

    # Save updated results with NPE weights
    .mosaic_write_parquet(results, simulations_file, control$io)
    log_msg("NPE weights added to simulations.parquet")

    # Prepare observed data
    log_msg("\nPreparing observed outbreak data...")
    observed_data <- get_npe_observed_data(config, verbose = TRUE)

    # Save observed data
    write.csv(observed_data, file.path(dirs$npe, "observed_data.csv"), row.names = FALSE)
    log_msg("  Observed data saved: %d locations, %d time points",
            length(unique(observed_data$j)), max(observed_data$t))

    # Run complete NPE workflow
    log_msg("\n=== NPE WORKFLOW STARTING ===\n")

    # Create NPE output directories
    npe_dirs <- list(
      root = dirs$npe,
      model = file.path(dirs$npe, "model"),
      posterior = file.path(dirs$npe, "posterior"),
      diagnostics = file.path(dirs$npe, "diagnostics"),
      plots = file.path(dirs$npe, "plots")
    )
    lapply(npe_dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))

    # Step 1: Load and prepare NPE data from BFRS results
    log_msg("Loading BFRS results for NPE...")
    npe_data <- prepare_npe_data(
      bfrs_dir = dirs$bfrs,
      results = results,
      param_names = param_names_estimated,
      verbose = TRUE
    )

    # Step 2: Calculate NPE architecture
    log_msg("Calculating NPE architecture...")
    arch_spec <- calc_npe_architecture(
      n_sims = npe_data$n_samples,
      n_params = npe_data$n_params,
      n_timesteps = npe_data$n_timesteps,
      n_locations = npe_data$n_locations,
      tier = "large",
      verbose = TRUE
    )

    # Step 3: Train NPE model
    log_msg("Training NPE model...")
    npe_model <- train_npe(
      X = npe_data$parameters,
      y = npe_data$observations,
      weights = npe_data$weights,
      bounds = npe_data$bounds,
      architecture = arch_spec,
      output_dir = npe_dirs$model,
      use_gpu = FALSE,
      seed = 42,
      verbose = TRUE
    )

    # Step 4: Run post-training diagnostics using comprehensive SBC
    log_msg("Running post-training diagnostics (SBC)...")
    diagnostics <- calc_npe_diagnostics(
      n_sbc_sims = 10,
      model = npe_model,
      npe_model_dir = npe_model$output_dir,
      PATHS = PATHS,
      priors = priors,
      config_base = config,
      param_names = npe_data$param_names,
      n_npe_samples = 100,
      probs = c(0.025, 0.25, 0.75, 0.975),
      output_dir = npe_dirs$diagnostics,
      verbose = TRUE,
      parallel = FALSE,
      n_cores = 6
    )

    # Check if model needs retraining based on diagnostics
    needs_retraining <- (diagnostics$diagnostics$overall_coverage < 0.85 ||
                        diagnostics$diagnostics$overall_coverage > 1.05 ||
                        diagnostics$diagnostics$overall_sbc_ks_pvalue < 0.01)

    if (needs_retraining) {
      log_msg("  Warning: Diagnostics suggest model may need retraining")
      log_msg("    - Overall coverage: %.1f%% (target: 95%%)",
              diagnostics$diagnostics$overall_coverage * 100)
      log_msg("    - SBC KS p-value: %.3f", diagnostics$diagnostics$overall_sbc_ks_pvalue)
    }

    # Step 5: Estimate posteriors with observed data
    log_msg("Estimating posteriors...")
    posterior_result <- estimate_npe_posterior(
      model = npe_model,
      observed_data = observed_data,
      n_samples = 10000,
      return_log_probs = TRUE,
      output_dir = npe_dirs$posterior,
      verbose = TRUE,
      rejection_sampling = TRUE,
      max_rejection_rate = 0.20,
      max_attempts = 10
    )

    # Extract results
    posterior_samples <- posterior_result$samples
    posterior_log_probs <- posterior_result$log_probs
    posterior_quantiles <- posterior_result$quantiles

    # Ensure column names are set
    if (is.null(colnames(posterior_samples)) && !is.null(npe_data$param_names)) {
      colnames(posterior_samples) <- npe_data$param_names
    }

    # Check if NPE succeeded
    if (!is.null(posterior_samples)) {
      log_msg("\nNPE complete: %d samples × %d params | %s tier (%d transforms)",
              nrow(posterior_samples), length(npe_data$param_names),
              arch_spec$tier, arch_spec$n_transforms)

      # Check diagnostics
      if (!is.null(diagnostics)) {
        if (!is.null(diagnostics$sbc_ranks)) {
          coverage_50 <- mean(diagnostics$sbc_ranks >= 0.25 & diagnostics$sbc_ranks <= 0.75, na.rm = TRUE)
          log_msg("  - Coverage at 50%% CI: %.1f%%", coverage_50 * 100)
        }
        log_msg("  - Coverage at 95%% CI: %.1f%%", diagnostics$diagnostics$overall_coverage * 100)
        log_msg("  - SBC KS p-value: %.3f", diagnostics$diagnostics$overall_sbc_ks_pvalue)
        log_msg("  - Model calibration: %s",
                ifelse(needs_retraining, "Needs improvement", "Good"))
      }

      # Check that log_probs were saved
      if (!is.null(posterior_log_probs)) {
        log_msg("  - Log probabilities saved for SMC: YES")
      } else {
        log_msg("  - WARNING: Log probabilities NOT saved - SMC will not be possible")
      }

      # Log rejection sampling diagnostics
      if (!is.null(posterior_result$rejection_info)) {
        info <- posterior_result$rejection_info
        log_msg("")
        log_msg("=== Rejection Sampling Summary ===")
        log_msg("  - Samples requested: %d", info$requested)
        log_msg("  - Samples achieved: %d", info$achieved)
        log_msg("  - Total drawn: %d", info$total_drawn)
        log_msg("  - Rejection rate: %.2f%%", info$rejection_rate * 100)
        log_msg("  - Attempts: %d", info$n_attempts)

        if (info$rejection_rate > 0.10) {
          log_msg("  - ⚠ Warning: High rejection rate suggests model calibration issues")
          log_msg("    Consider retraining with wider bounds or different architecture")
        } else if (info$rejection_rate > 0) {
          log_msg("  - ✓ Rejection rate acceptable (<10%%)")
        } else {
          log_msg("  - ✓ All samples within bounds!")
        }
      }

      # Create NPE result object
      npe_result <- list(
        posterior_samples = posterior_samples,
        posterior_log_probs = posterior_log_probs,
        posterior_quantiles = posterior_quantiles,
        param_names = npe_data$param_names,
        architecture = arch_spec,
        diagnostics = diagnostics,
        model = npe_model
      )

      # Create NPE configuration with posterior medians
      log_msg("\nCreating NPE configuration...")
      config_npe <- create_config_npe(
        posterior_result = npe_result,
        config_base = config,
        output_file = file.path(dirs$npe, "config_npe.json"),
        use_median = TRUE,
        verbose = TRUE
      )

      # Fit posterior distributions
      log_msg("\nFitting posterior distributions...")
      posteriors_npe <- fit_posterior_distributions(
        posterior_samples = posterior_samples,
        priors_file = file.path(dirs$setup, "priors.json"),
        output_file = file.path(npe_dirs$posterior, "posteriors.json"),
        verbose = TRUE
      )

      # Save NPE summary
      npe_summary <- list(
        n_posterior_samples = nrow(posterior_samples),
        param_names = npe_data$param_names,
        architecture = arch_spec,
        diagnostics = diagnostics,
        has_log_probs = !is.null(posterior_log_probs),
        rejection_info = posterior_result$rejection_info,
        timestamp = Sys.time()
      )

      saveRDS(npe_summary, file.path(dirs$npe, "npe_summary.rds"))

      log_msg("\n=== NPE STAGE COMPLETE ===")
      log_msg("Results saved in: %s/", basename(dirs$npe))

    } else {
      log_msg("\nERROR: NPE failed to generate posterior samples")
    }

    # =========================================================================
    # PLOT NPE DIAGNOSTICS
    # =========================================================================

    if (control$paths$plots && !is.null(diagnostics)) {
      # Plot NPE convergence status table
      plot_npe_convergence_status(
        npe_dir = dirs$npe,
        output_dir = npe_dirs$plots,
        coverage_targets = list("50" = 0.50, "95" = 0.95),
        ks_threshold = 0.05,
        show_param_details = 15,
        verbose = TRUE
      )

      # Plot coverage histograms
      plot_npe_diagnostics_coverage(
        diagnostics = diagnostics,
        plots_dir = npe_dirs$plots,
        max_params_per_page = 40,
        verbose = TRUE
      )

      # Plot coverage bar plots
      plot_npe_diagnostics_coverage_bars(
        diagnostics = diagnostics,
        plots_dir = npe_dirs$plots,
        verbose = TRUE
      )

      # Plot SBC test results
      plot_npe_diagnostics_sbc(
        diagnostics = diagnostics,
        plots_dir = npe_dirs$plots,
        max_params_per_page = 40,
        verbose = TRUE
      )

      # Plot NPE training loss curves
      log_msg("\nGenerating NPE training loss visualization...")
      training_plot <- plot_npe_training_loss(
        npe_dirs = npe_dirs,
        output_file = file.path(npe_dirs$plots, "npe_training_loss.png"),
        plot_height = 8,
        show_best_epoch = TRUE,
        smooth_curves = FALSE,
        log_scale = FALSE,
        verbose = TRUE
      )
      log_msg("NPE training loss plot saved")

      # =========================================================================
      # Plot posterior distributions (caterpillar and detailed)
      # =========================================================================

      log_msg("\nGenerating posterior quantile plots...")

      # Collect quantile files for comparison
      quantile_files <- c()

      bfrs_quantiles <- file.path(dirs$bfrs_post, "posterior_quantiles.csv")
      if (file.exists(bfrs_quantiles)) {
        quantile_files <- c(quantile_files, bfrs_quantiles)
      }

      npe_quantiles <- file.path(npe_dirs$posterior, "posterior_quantiles.csv")
      if (file.exists(npe_quantiles)) {
        quantile_files <- c(quantile_files, npe_quantiles)
      }

      if (length(quantile_files) > 0) {
        tryCatch({
          plot_model_posterior_quantiles(
            csv_files = quantile_files,
            output_dir = npe_dirs$plots,
            plot_types = "both",
            verbose = TRUE
          )
          log_msg("Posterior quantile plots created successfully")
          log_msg("  - Comparing: Prior (from BFRS), BFRS Posterior, and NPE")
        }, error = function(e) {
          log_msg("ERROR creating posterior quantile plots: %s", e$message)
        })
      }

      # Plot overlaid posterior distributions
      log_msg("\nGenerating overlaid posterior distribution plots...")

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
        tryCatch({
          plot_model_distributions(
            json_files = json_files,
            method_names = method_names,
            output_dir = npe_dirs$plots
          )
          log_msg("Overlaid posterior distribution plots created successfully")
          log_msg("  - Comparing: %s", paste(method_names, collapse = ", "))
        }, error = function(e) {
          log_msg("ERROR creating overlaid distribution plots: %s", e$message)
        })
      }
    }

    # =========================================================================
    # PLOT MODEL FIT WITH NPE POSTERIOR ESTIMATES
    # =========================================================================

    if (control$paths$plots && !is.null(config_npe)) {
      log_msg("\n=== GENERATING STOCHASTIC MODEL PLOTS WITH NPE ESTIMATES ===")

      log_msg("Plotting model fit using NPE posterior median values...")
      log_msg("  Using %d stochastic simulations", control$predictions$best_model_n_sims)

      stochastic_plots_median <- plot_model_fit_stochastic(
        config = config_npe,
        n_simulations = control$predictions$best_model_n_sims,
        output_dir = npe_dirs$plots,
        envelope_quantiles = c(0.025, 0.975),
        save_predictions = TRUE,
        parallel = TRUE,
        n_cores = control$parallel$n_cores,
        root_dir = root_dir,
        verbose = TRUE
      )

      log_msg("Stochastic plots saved to: %s/", basename(npe_dirs$plots))

      # Generate parameter uncertainty plots using multiple NPE posterior samples
      log_msg("\nGenerating stochastic plots with parameter uncertainty...")

      n_param_sets <- control$predictions$ensemble_n_param_sets
      log_msg("  Using %d parameter sets from NPE posterior", n_param_sets)
      param_configs <- list()

      if (nrow(posterior_samples) >= n_param_sets) {
        set.seed(123)
        sample_indices <- sample(1:nrow(posterior_samples), n_param_sets, replace = FALSE)

        for (i in 1:n_param_sets) {
          idx <- sample_indices[i]
          config_sample <- config

          for (param_name in param_names_estimated) {
            if (param_name %in% colnames(posterior_samples)) {
              config_sample[[param_name]] <- posterior_samples[idx, param_name]
            }
          }

          param_configs[[i]] <- config_sample
        }

        log_msg("  Using %d parameter sets from NPE posterior", n_param_sets)

        # Use posterior weights based on log probabilities
        param_weights <- NULL
        if (!is.null(posterior_log_probs)) {
          selected_log_probs <- posterior_log_probs[sample_indices]
          param_weights <- exp(selected_log_probs - max(selected_log_probs))
          param_weights <- param_weights / sum(param_weights)
        }

        if (!is.null(param_weights)) {
          log_msg("  NPE ensemble: %d param sets (weighted) × %d sims = %d total",
                  n_param_sets, control$predictions$ensemble_n_sims_per_param,
                  n_param_sets * control$predictions$ensemble_n_sims_per_param)
        } else {
          log_msg("  NPE ensemble: %d param sets × %d sims = %d total",
                  n_param_sets, control$predictions$ensemble_n_sims_per_param,
                  n_param_sets * control$predictions$ensemble_n_sims_per_param)
        }

        stochastic_param_plots <- plot_model_fit_stochastic_param(
          configs = param_configs,
          parameter_weights = param_weights,
          n_simulations_per_config = control$predictions$ensemble_n_sims_per_param,
          output_dir = npe_dirs$plots,
          envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
          save_predictions = TRUE,
          parallel = TRUE,
          n_cores = control$parallel$n_cores,
          root_dir = root_dir,
          verbose = TRUE,
          plot_decomposed = FALSE
        )

        log_msg("Stochastic parameter plots saved to: %s/", basename(npe_dirs$plots))
      } else {
        log_msg("  Insufficient posterior samples for parameter uncertainty analysis")
      }

      # =========================================================================
      # CLEANUP OUTPUT FILES
      # =========================================================================

      log_msg(paste0("\n", paste(rep("=", 80), collapse = "")))
      log_msg("CLEANING UP TIME SERIES FILES")
      log_msg(paste(rep("=", 80), collapse = ""))

      outputs_dir <- file.path(dirs$bfrs_out, "outputs")
      if (dir.exists(outputs_dir)) {
        output_files <- list.files(outputs_dir, pattern = "^out_.*\\.parquet$",
                                   full.names = TRUE)
        file_sizes_mb <- sum(file.size(output_files)) / 1024^2

        log_msg("Removing %d individual output files (%.1f MB total)",
                length(output_files), file_sizes_mb)
        log_msg("Removing directory: %s", basename(outputs_dir))

        unlink(outputs_dir, recursive = TRUE)

        log_msg("Cleanup completed. Disk space freed: %.1f MB", file_sizes_mb)
        log_msg("Note: BFRS metadata (simulations.parquet) preserved for future analysis")
      } else {
        log_msg("No outputs directory found to clean up")
      }
    }

  } else {
    log_msg(paste0("\n", paste(rep("=", 80), collapse = "")))
    log_msg("STAGE 2: NEURAL POSTERIOR ESTIMATION SKIPPED (control$npe$enable = FALSE)")
    log_msg(paste(rep("=", 80), collapse = ""))
  }

  # ===========================================================================
  # RETURN SUMMARY
  # ===========================================================================

  runtime <- difftime(Sys.time(), start_time, units = "mins")

  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("CALIBRATION COMPLETE")
  log_msg(paste(rep("=", 80), collapse = ""))
  log_msg("%d batches | %d simulations | Converged: %s | Runtime: %.2f min",
          state$batch_number, state$total_sims_run,
          ifelse(isTRUE(state$converged), "YES", "NO"), as.numeric(runtime))

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
#'     \item \code{ESS_best}: Target ESS for best subset (default: 100)
#'     \item \code{A_best}: Target agreement index (default: 0.95)
#'     \item \code{CVw_best}: Target CV of weights (default: 0.5)
#'     \item \code{B_min}: Minimum best subset size (default: 30)
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
#'     \item \code{weight_strategy}: NPE weight strategy (default: "continuous_all")
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
                           paths = NULL) {

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
    B_min = 30,
    percentile_max = 5.0,
    ESS_method = "perplexity"     # ESS calculation method: "kish" or "perplexity"
  )

  # Default NPE settings
  default_npe <- list(
    enable = FALSE,
    weight_strategy = "continuous_best"  # Use BFRS best weights (consistent with prior definition)
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

  # Merge user-provided settings with defaults
  # Order follows workflow: calibration → sampling → likelihood → targets → fine_tuning → npe → predictions → weights → parallel → io → paths
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
    paths = if (is.null(paths)) default_paths else modifyList(default_paths, paths)
  )
}

#' Get Default MOSAIC Run Settings (Internal)
#'
#' @description
#' Internal function that returns default control settings.
#' Used by `.mosaic_validate_and_merge_control()`.
#'
#' @return Named list with default settings for paths, parallel, calibration, fine_tuning, targets, npe, and io
#' @noRd
mosaic_run_defaults <- function() {
  list(
    paths = list(
      clean_output = FALSE,
      plots = TRUE
    ),
    parallel = list(
      enable = FALSE,
      n_cores = 1L,
      type = "PSOCK",
      progress = TRUE
    ),
    calibration = list(
      batch_size = 500L,
      min_batches = 5L,
      max_batches = 8L,
      target_r2 = 0.90,
      max_simulations = 100000L
    ),
    fine_tuning = list(
      batch_sizes = list(
        massive = 1000L,
        large = 750L,
        standard = 500L,
        precision = 350L,
        final = 250L
      )
    ),
    targets = list(
      ESS_param = 500,
      ESS_param_prop = 0.95,
      ESS_best = 100,
      A_best = 0.95,
      CVw_best = 0.5,
      B_min = 30,
      percentile_max = 5.0
    ),
    npe = list(
      enable = FALSE,
      weight_strategy = "continuous_best"
    ),
    predictions = list(
      best_model_n_sims = 100L,
      ensemble_n_param_sets = 50L,
      ensemble_n_sims_per_param = 10L
    ),
    weights = list(
      floor = 1e-15,
      iqr_multiplier = 1.5
    ),
    io = list(
      format = "parquet",
      compression = "zstd",
      compression_level = 3L,
      verbose_weights = FALSE
    )
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
