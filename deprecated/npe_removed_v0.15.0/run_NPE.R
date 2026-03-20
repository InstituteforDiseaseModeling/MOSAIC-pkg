# ==============================================================================
# run_NPE: Neural Posterior Estimation Workflow
# ==============================================================================
# Dual-mode function for running NPE either:
#   1. Standalone: After run_MOSAIC completes (loads from disk)
#   2. Embedded: Inside run_MOSAIC (uses in-memory objects)
# ==============================================================================

#' Run Neural Posterior Estimation (NPE)
#'
#' @description
#' Trains a normalizing flow model for neural posterior estimation using
#' results from Bayesian Filtering with Resampling (BFRS). Can run in two modes:
#' \enumerate{
#'   \item \strong{Standalone mode}: Load from completed run_MOSAIC output directory
#'   \item \strong{Embedded mode}: Use in-memory objects from within run_MOSAIC
#' }
#'
#' Mode is automatically detected based on which arguments are provided.
#'
#' @section Standalone Mode:
#' Provide \code{input_dir} to load data from a completed BFRS run:
#' \itemize{
#'   \item Loads results from \code{1_bfrs/outputs/simulations.parquet}
#'   \item Loads priors from \code{0_setup/priors.json}
#'   \item Loads config from \code{0_setup/config_base.json}
#'   \item Loads control from \code{0_setup/simulation_params.json} (or uses provided)
#'   \item Auto-detects parameter names from results
#'   \item Creates PATHS from root_dir
#'   \item Constructs dirs from input_dir structure
#' }
#'
#' Ideal for post-hoc experimentation with different NPE settings without
#' re-running expensive BFRS calibration.
#'
#' @section Embedded Mode:
#' Provide \code{results} and other in-memory objects from run_MOSAIC:
#' \itemize{
#'   \item Uses all objects already in memory (no disk I/O)
#'   \item Requires: results, priors, config, control, dirs
#'   \item Optional: param_names (auto-detects if NULL), PATHS (creates if NULL)
#' }
#'
#' This is how run_MOSAIC calls NPE when \code{control$npe$enable = TRUE}.
#'
#' @param input_dir Character. Directory containing completed BFRS run
#'   (with \code{0_setup/} and \code{1_bfrs/} subdirectories).
#'   Required for standalone mode. Ignored in embedded mode.
#' @param output_dir Character. Where to save NPE results.
#'   In standalone mode: defaults to \code{input_dir/2_npe/}.
#'   Specify custom path to run multiple NPE configurations.
#'   In embedded mode: ignored (uses \code{dirs$npe}).
#' @param root_dir Character. MOSAIC root directory. If NULL, uses:
#'   \enumerate{
#'     \item Existing \code{getOption('root_directory')} if set
#'     \item Current working directory as fallback
#'   }
#'   Set globally via \code{set_root_directory("~/MOSAIC")} to avoid
#'   specifying on each call. Ignored in embedded mode if PATHS provided.
#' @param results Data frame. BFRS results with simulations and weights.
#'   If provided, runs in embedded mode. If NULL, loads from \code{input_dir}.
#' @param priors List. Prior distributions for parameters.
#'   Required in embedded mode. If NULL in standalone, loads from disk.
#' @param config List. LASER model configuration.
#'   Required in embedded mode. If NULL in standalone, loads from disk.
#' @param param_names Character vector. Parameters to include in NPE.
#'   If NULL, auto-detects from results column names.
#' @param dirs List. Directory structure from run_MOSAIC containing:
#'   \code{$bfrs}, \code{$setup}, \code{$npe}, \code{$npe_model}, etc.
#'   Required in embedded mode. Constructed in standalone mode.
#' @param PATHS List. Global MOSAIC paths from \code{get_paths()}.
#'   Optional in both modes. If NULL, created from \code{root_dir} or
#'   \code{getOption('root_directory')}.
#' @param control List. Control settings from \code{mosaic_control_defaults()}.
#'   Required in embedded mode. In standalone mode: if NULL, loads from
#'   \code{simulation_params.json}; if provided, overrides (for experimentation).
#' @param verbose Logical. Print progress messages (default: TRUE).
#'
#' @return Invisibly returns a list containing:
#' \describe{
#'   \item{posterior_samples}{Matrix of posterior samples (n_samples × n_params)}
#'   \item{posterior_log_probs}{Vector of log q(theta|x) for SMC}
#'   \item{posterior_quantiles}{Data frame of parameter quantiles}
#'   \item{param_names}{Character vector of parameter names}
#'   \item{architecture}{List with network architecture specification}
#'   \item{diagnostics}{List with SBC, coverage, and validation metrics}
#'   \item{model}{Trained NPE model object}
#'   \item{config_npe}{Config with NPE posterior medians}
#'   \item{posteriors_npe}{Fitted parametric distributions}
#'   \item{npe_summary}{List with metadata and diagnostics}
#'   \item{dirs}{List with output directory paths}
#'   \item{mode}{Character: "standalone" or "embedded"}
#' }
#'
#' @examples
#' \dontrun{
#' # === STANDALONE MODE ===
#'
#' # Minimal call (uses defaults):
#' library(MOSAIC)
#' set_root_directory("~/MOSAIC")  # Set once per session
#'
#' npe_result <- run_NPE(
#'   input_dir = "./output/ethiopia_2024",
#'   verbose = TRUE
#' )
#'
#' # Experiment with different weight strategies:
#' npe_best <- run_NPE(
#'   input_dir = "./output/ethiopia_2024",
#'   output_dir = "./output/ethiopia_2024/2_npe_strategy_best",
#'   control = mosaic_control_defaults(
#'     npe = list(weight_strategy = "continuous_best")
#'   )
#' )
#'
#' npe_retained <- run_NPE(
#'   input_dir = "./output/ethiopia_2024",
#'   output_dir = "./output/ethiopia_2024/2_npe_strategy_retained",
#'   control = mosaic_control_defaults(
#'     npe = list(weight_strategy = "continuous_retained")
#'   )
#' )
#'
#' # === EMBEDDED MODE ===
#'
#' # Called automatically by run_MOSAIC when npe$enable = TRUE:
#' run_mosaic_iso(
#'   iso_code = "ETH",
#'   dir_output = "./output/ethiopia_2024",
#'   control = mosaic_control_defaults(
#'     calibration = list(n_simulations = 5000),
#'     npe = list(enable = TRUE)
#'   )
#' )
#' }
#'
#' @export
run_NPE <- function(
  # === STANDALONE MODE ===
  input_dir = NULL,
  output_dir = NULL,
  root_dir = NULL,

  # === EMBEDDED MODE ===
  results = NULL,
  priors = NULL,
  config = NULL,
  param_names = NULL,
  dirs = NULL,
  PATHS = NULL,

  # === CONTROL (BOTH MODES) ===
  control = NULL,

  # === COMMON ===
  verbose = TRUE
) {

  # ===========================================================================
  # MODE DETECTION
  # ===========================================================================

  standalone_mode <- !is.null(input_dir)
  embedded_mode <- !is.null(results)

  if (!standalone_mode && !embedded_mode) {
    stop(
      "Must provide either:\n",
      "  - 'input_dir' (standalone mode: load from disk)\n",
      "  - 'results' (embedded mode: use in-memory objects)\n",
      "See ?run_NPE for examples."
    )
  }

  if (standalone_mode && embedded_mode) {
    warning(
      "Both 'input_dir' and 'results' provided. ",
      "Using embedded mode (results)."
    )
    standalone_mode <- FALSE
  }

  if (verbose) {
    message("\n=== Run NPE ===")
    message("Mode: ", if (embedded_mode) "embedded (in-memory)" else "standalone (load from disk)")
  }

  # ===========================================================================
  # STANDALONE MODE: LOAD FROM DISK
  # ===========================================================================

  if (standalone_mode) {

    if (verbose) message("\n--- Loading BFRS Results from Disk ---")

    # =========================================================================
    # Validate input_dir structure
    # =========================================================================

    if (!dir.exists(input_dir)) {
      stop("input_dir does not exist: ", input_dir)
    }

    setup_dir <- file.path(input_dir, "0_setup")
    bfrs_dir <- file.path(input_dir, "1_bfrs")

    if (!dir.exists(setup_dir)) {
      stop("Setup directory not found: ", setup_dir, "\n",
           "Expected structure: input_dir/0_setup/")
    }

    if (!dir.exists(bfrs_dir)) {
      stop("BFRS directory not found: ", bfrs_dir, "\n",
           "Expected structure: input_dir/1_bfrs/")
    }

    # =========================================================================
    # Load results
    # =========================================================================

    simulations_file <- file.path(bfrs_dir, "outputs", "simulations.parquet")
    if (!file.exists(simulations_file)) {
      stop("Simulations file not found: ", simulations_file, "\n",
           "Has BFRS stage completed successfully?")
    }

    results <- arrow::read_parquet(simulations_file)
    if (verbose) {
      message("  Loaded results: ", nrow(results), " simulations, ",
              ncol(results), " columns")
    }

    # =========================================================================
    # Load priors (if not provided)
    # =========================================================================

    if (is.null(priors)) {
      priors_file <- file.path(setup_dir, "priors.json")
      if (!file.exists(priors_file)) {
        stop("Priors file not found: ", priors_file)
      }

      priors <- read_json_to_list(priors_file)
      if (verbose) {
        message("  Loaded priors from ", basename(priors_file))
      }
    } else {
      if (verbose) {
        message("  Using provided priors object")
      }
    }

    # =========================================================================
    # Load config (if not provided)
    # =========================================================================

    if (is.null(config)) {
      config_file <- file.path(setup_dir, "config_base.json")
      if (!file.exists(config_file)) {
        stop("Config file not found: ", config_file)
      }

      config <- read_json_to_list(config_file)
      if (verbose) {
        message("  Loaded config from ", basename(config_file))
      }
    } else {
      if (verbose) {
        message("  Using provided config object")
      }
    }

    # =========================================================================
    # Load control (if not provided)
    # =========================================================================

    if (is.null(control)) {
      sim_params_file <- file.path(setup_dir, "simulation_params.json")
      if (!file.exists(sim_params_file)) {
        stop("Simulation params file not found: ", sim_params_file)
      }

      sim_params <- read_json_to_list(sim_params_file)
      control <- sim_params$control

      # Convert from JSON list structure to proper control structure
      # (JSON serialization may lose some R-specific structures)
      if (!is.null(control) && is.list(control)) {
        if (verbose) {
          message("  Loaded control from ", basename(sim_params_file))
        }
      } else {
        stop("Could not extract control object from simulation_params.json")
      }
    } else {
      if (verbose) {
        message("  Using provided control object (overriding saved settings)")
      }
    }

    # =========================================================================
    # Handle root_dir → PATHS
    # =========================================================================

    if (is.null(root_dir)) {
      # Check for existing root in options
      if ('root_directory' %in% names(options())) {
        root_dir <- getOption('root_directory')
        if (verbose) {
          message("  Using existing root_directory from options: ", root_dir)
        }
      } else {
        # Fallback to current directory
        root_dir <- getwd()
        if (verbose) {
          message("  No root_directory set, using current directory: ", root_dir)
          message("  Tip: Call set_root_directory('~/MOSAIC') at session start")
        }
      }
    } else {
      # User provided explicit root_dir
      if (verbose && 'root_directory' %in% names(options())) {
        existing_root <- getOption('root_directory')
        if (root_dir != existing_root) {
          message("  Overriding existing root (", existing_root, ") with: ", root_dir)
        }
      }
    }

    # Set root directory (updates global option)
    set_root_directory(root_dir)

    # Get global paths
    PATHS <- get_paths()

    # =========================================================================
    # Construct dirs object
    # =========================================================================

    # Determine NPE output directory
    if (is.null(output_dir)) {
      npe_dir <- file.path(input_dir, "2_npe")
      if (verbose) {
        message("  Using default NPE output directory: ", npe_dir)
      }
    } else {
      npe_dir <- output_dir
      if (verbose) {
        message("  Using custom NPE output directory: ", npe_dir)
      }
    }

    # Construct dirs list
    dirs <- list(
      root = input_dir,
      setup = setup_dir,
      bfrs = bfrs_dir,
      npe = npe_dir,
      npe_model = file.path(npe_dir, "model"),
      npe_posterior = file.path(npe_dir, "posterior"),
      npe_diagnostics = file.path(npe_dir, "diagnostics"),
      npe_plots = file.path(npe_dir, "plots")
    )

    # Create NPE directories
    npe_subdirs <- dirs[grep("^npe", names(dirs))]
    lapply(npe_subdirs, function(d) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    })

    if (verbose) {
      message("  Created NPE directory structure")
    }

  } else {

    # =========================================================================
    # EMBEDDED MODE: USE IN-MEMORY OBJECTS
    # =========================================================================

    if (verbose) message("\n--- Using In-Memory Objects ---")

    # =========================================================================
    # Validate required objects
    # =========================================================================

    if (is.null(priors)) {
      stop("'priors' required in embedded mode")
    }

    if (is.null(config)) {
      stop("'config' required in embedded mode")
    }

    if (is.null(control)) {
      stop("'control' required in embedded mode")
    }

    if (is.null(dirs)) {
      stop("'dirs' required in embedded mode")
    }

    # =========================================================================
    # Warn if standalone-specific args provided
    # =========================================================================

    if (!is.null(output_dir)) {
      warning("'output_dir' ignored in embedded mode - using dirs$npe from run_MOSAIC")
    }

    if (!is.null(input_dir)) {
      warning("'input_dir' ignored in embedded mode")
    }

    # =========================================================================
    # Handle PATHS
    # =========================================================================

    if (is.null(PATHS)) {
      # Try to create from existing root
      if ('root_directory' %in% names(options())) {
        PATHS <- get_paths()  # Uses getOption('root_directory')
        if (verbose) {
          message("  Created PATHS from existing root_directory: ",
                  getOption('root_directory'))
        }
      } else if (!is.null(root_dir)) {
        set_root_directory(root_dir)
        PATHS <- get_paths()
        if (verbose) {
          message("  Created PATHS from provided root_dir: ", root_dir)
        }
      } else {
        stop(
          "'PATHS' required in embedded mode.\n",
          "Solutions:\n",
          "  1. Pass PATHS argument: run_NPE(..., PATHS = PATHS)\n",
          "  2. Set root first: set_root_directory('~/MOSAIC')\n",
          "  3. Pass root_dir: run_NPE(..., root_dir = '~/MOSAIC')"
        )
      }
    } else {
      if (verbose) {
        message("  Using provided PATHS object")
      }
    }

    if (verbose) {
      message("  Validated all required objects")
    }
  }

  # ===========================================================================
  # EXTRACT PARAMETER NAMES (COMMON TO BOTH MODES)
  # ===========================================================================

  if (is.null(param_names)) {
    # Auto-detect from results columns
    # Exclude known non-parameter columns
    exclude_cols <- c(
      "sim", "iter", "seed_sim", "seed_iter", "likelihood",
      "is_valid", "is_retained", "is_best_subset",
      "weight_all", "weight_retained", "weight_best", "weight_npe"
    )
    param_names <- setdiff(names(results), exclude_cols)

    if (verbose) {
      message("\n--- Parameter Detection ---")
      message("  Auto-detected ", length(param_names), " parameters from results")
    }
  } else {
    if (verbose) {
      message("\n--- Parameter Detection ---")
      message("  Using provided param_names (", length(param_names), " parameters)")
    }
  }

  # ===========================================================================
  # NPE WORKFLOW (SAME FOR BOTH MODES)
  # ===========================================================================

  if (verbose) {
    message("\n=== NPE Workflow ===")
  }

  # ===========================================================================
  # STEP 1: Calculate NPE Weights
  # ===========================================================================

  if (verbose) {
    message("\nStep 1/9: Calculating NPE weights...")
  }

  npe_weights <- get_npe_weights(
    bfrs_results = results,
    strategy = control$npe$weight_strategy,
    verbose = verbose
  )

  # Add weight column to results
  results$weight_npe <- npe_weights

  # Save weights
  weight_df <- data.frame(
    sim = results$sim,
    weight_npe = npe_weights
  )

  weights_file <- file.path(dirs$npe, "npe_weights.parquet")
  .mosaic_write_parquet(weight_df, weights_file, control$io)

  if (verbose) {
    message("  Saved ", basename(weights_file))
  }

  # ===========================================================================
  # STEP 2: Prepare Observed Data
  # ===========================================================================

  if (verbose) {
    message("\nStep 2/9: Preparing observed data...")
  }

  observed_data <- get_npe_observed_data(config, verbose = verbose)

  observed_file <- file.path(dirs$npe, "observed_data.csv")
  write.csv(observed_data, observed_file, row.names = FALSE)

  if (verbose) {
    message("  Saved ", basename(observed_file))
  }

  # ===========================================================================
  # STEP 3: Prepare NPE Training Data (PERFORMANCE CRITICAL)
  # ===========================================================================

  if (verbose) {
    message("\nStep 3/9: Preparing NPE training data...")
    message("  (Loading individual timeseries files for weighted simulations)")
  }

  npe_data <- prepare_npe_data(
    bfrs_dir = dirs$bfrs,
    results = results,
    param_names = param_names,
    weights = NULL,  # Extracted from results$weight_npe
    verbose = verbose
  )

  if (verbose) {
    message("  Prepared data: ", npe_data$n_samples, " samples × ",
            npe_data$n_params, " parameters")
    message("  Observation matrix: ", nrow(npe_data$observations), " × ",
            ncol(npe_data$observations))
  }

  # ===========================================================================
  # STEP 4: Calculate NPE Architecture
  # ===========================================================================

  if (verbose) {
    message("\nStep 4/9: Calculating NPE architecture...")
  }

  arch_spec <- calc_npe_architecture(
    n_sims = npe_data$n_samples,
    n_params = npe_data$n_params,
    n_timesteps = npe_data$n_timesteps,
    n_locations = npe_data$n_locations,
    tier = "auto",
    verbose = verbose
  )

  if (verbose) {
    message("  Architecture tier: ", arch_spec$tier)
    message("  Embedding dim: ", arch_spec$embedding_dim)
    message("  Flow transforms: ", arch_spec$n_transforms)
  }

  # ===========================================================================
  # STEP 5: Train NPE Model
  # ===========================================================================

  if (verbose) {
    message("\nStep 5/9: Training NPE model...")
  }

  npe_model <- train_npe(
    X = npe_data$parameters,
    y = npe_data$observations,
    weights = npe_data$weights,
    bounds = npe_data$bounds,
    architecture = arch_spec,
    output_dir = dirs$npe_model,
    use_gpu = FALSE,
    seed = 42,
    verbose = verbose
  )

  # Store parameter names and clean up large objects
  param_names_npe <- npe_data$param_names
  n_params_npe <- npe_data$n_params
  n_samples_npe <- npe_data$n_samples

  rm(npe_data)
  gc(verbose = FALSE)

  if (verbose) {
    message("  Model training complete")
  }

  # ===========================================================================
  # STEP 6: Run NPE Diagnostics
  # ===========================================================================

  if (verbose) {
    message("\nStep 6/9: Running NPE diagnostics...")
  }

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
    output_dir = dirs$npe_diagnostics,
    verbose = verbose,
    parallel = FALSE,
    n_cores = 6
  )

  if (verbose) {
    message("  Diagnostics complete")
  }

  # ===========================================================================
  # STEP 7: Estimate NPE Posterior
  # ===========================================================================

  if (verbose) {
    message("\nStep 7/9: Estimating NPE posterior...")
  }

  posterior_result <- estimate_npe_posterior(
    model = npe_model,
    observed_data = observed_data,
    n_samples = 1000,
    return_log_probs = TRUE,
    output_dir = dirs$npe_posterior,
    verbose = verbose,
    rejection_sampling = TRUE,
    max_rejection_rate = 0.25,
    max_attempts = 10
  )

  # Extract results
  posterior_samples <- posterior_result$samples
  posterior_log_probs <- posterior_result$log_probs
  posterior_quantiles <- posterior_result$quantiles

  # Ensure column names are set
  if (is.null(colnames(posterior_samples)) && !is.null(param_names_npe)) {
    colnames(posterior_samples) <- param_names_npe
  }

  if (verbose) {
    message("  Generated ", nrow(posterior_samples), " posterior samples")
    if (!is.null(posterior_result$rejection_info)) {
      rej_info <- posterior_result$rejection_info
      if (rej_info$enabled) {
        message("  Rejection sampling: ", round(rej_info$rejection_rate * 100, 1),
                "% rejected (", rej_info$n_attempts, " attempts)")
      }
    }
  }

  # ===========================================================================
  # STEP 8: Post-processing
  # ===========================================================================

  if (verbose) {
    message("\nStep 8/9: Post-processing results...")
  }

  # Create NPE config with posterior medians
  config_npe <- create_config_npe(
    posterior_result = list(
      posterior_samples = posterior_samples,
      posterior_log_probs = posterior_log_probs,
      posterior_quantiles = posterior_quantiles,
      param_names = param_names_npe,
      architecture = arch_spec,
      diagnostics = diagnostics,
      model = npe_model
    ),
    config_base = config,
    output_file = file.path(dirs$npe, "config_npe.json"),
    use_median = TRUE,
    verbose = verbose
  )

  # Fit parametric distributions to posteriors
  posteriors_npe <- fit_posterior_distributions(
    posterior_samples = posterior_samples,
    priors_file = file.path(dirs$setup, "priors.json"),
    output_file = file.path(dirs$npe_posterior, "posteriors.json"),
    verbose = verbose
  )

  # Create summary
  npe_summary <- list(
    mode = if (embedded_mode) "embedded" else "standalone",
    n_posterior_samples = nrow(posterior_samples),
    n_params = n_params_npe,
    n_training_samples = n_samples_npe,
    param_names = param_names_npe,
    architecture = arch_spec,
    diagnostics = diagnostics,
    has_log_probs = !is.null(posterior_log_probs),
    rejection_info = posterior_result$rejection_info,
    timestamp = Sys.time(),
    weight_strategy = control$npe$weight_strategy
  )

  npe_summary_file <- file.path(dirs$npe, "npe_summary.rds")
  saveRDS(npe_summary, npe_summary_file)

  if (verbose) {
    message("  Saved ", basename(npe_summary_file))
  }

  # ===========================================================================
  # STEP 9: Generate Diagnostic Plots
  # ===========================================================================

  if (control$paths$plots && !is.null(diagnostics)) {
    if (verbose) {
      message("\nStep 9/9: Generating diagnostic plots...")
    }

    # Convergence status
    tryCatch({
      plot_npe_convergence_status(
        npe_dir = dirs$npe,
        output_dir = dirs$npe_plots,
        coverage_targets = list("50" = 0.50, "95" = 0.95),
        ks_threshold = 0.05,
        show_param_details = 15,
        verbose = FALSE
      )
    }, error = function(e) {
      if (verbose) warning("Could not generate convergence status plot: ", e$message)
    })

    # Coverage diagnostics
    tryCatch({
      plot_npe_diagnostics_coverage(
        diagnostics = diagnostics,
        plots_dir = dirs$npe_plots,
        max_params_per_page = 40,
        verbose = FALSE
      )
    }, error = function(e) {
      if (verbose) warning("Could not generate coverage plots: ", e$message)
    })

    # Coverage bars
    tryCatch({
      plot_npe_diagnostics_coverage_bars(
        diagnostics = diagnostics,
        plots_dir = dirs$npe_plots,
        verbose = FALSE
      )
    }, error = function(e) {
      if (verbose) warning("Could not generate coverage bar plots: ", e$message)
    })

    # SBC diagnostics
    tryCatch({
      plot_npe_diagnostics_sbc(
        diagnostics = diagnostics,
        plots_dir = dirs$npe_plots,
        max_params_per_page = 40,
        verbose = FALSE
      )
    }, error = function(e) {
      if (verbose) warning("Could not generate SBC plots: ", e$message)
    })

    # Training loss
    tryCatch({
      plot_npe_training_loss(
        npe_dirs = dirs,
        output_file = file.path(dirs$npe_plots, "npe_training_loss.png"),
        plot_height = 8,
        show_best_epoch = TRUE,
        smooth_curves = FALSE,
        log_scale = FALSE,
        verbose = FALSE
      )
    }, error = function(e) {
      if (verbose) warning("Could not generate training loss plot: ", e$message)
    })

    # Posterior quantiles comparison (if BFRS quantiles available)
    bfrs_quantiles_file <- file.path(dirs$bfrs, "posterior", "posterior_quantiles.csv")
    npe_quantiles_file <- file.path(dirs$npe_posterior, "posterior_quantiles.csv")

    if (file.exists(bfrs_quantiles_file) && file.exists(npe_quantiles_file)) {
      tryCatch({
        plot_model_posterior_quantiles(
          quantile_files = c(bfrs_quantiles_file, npe_quantiles_file),
          output_file = file.path(dirs$npe_plots, "posterior_comparison.png"),
          labels = c("BFRS", "NPE"),
          verbose = FALSE
        )
      }, error = function(e) {
        if (verbose) warning("Could not generate posterior comparison plot: ", e$message)
      })
    }

    if (verbose) {
      message("  Saved diagnostic plots to ", basename(dirs$npe_plots))
    }
  } else {
    if (verbose) {
      message("\nStep 9/9: Skipping plots (control$paths$plots = FALSE or diagnostics = NULL)")
    }
  }

  # ===========================================================================
  # RETURN RESULTS
  # ===========================================================================

  if (verbose) {
    message("\n=== NPE Complete ===")
    message("Results saved to: ", dirs$npe)
    message("Mode: ", if (embedded_mode) "embedded" else "standalone")
  }

  result <- list(
    posterior_samples = posterior_samples,
    posterior_log_probs = posterior_log_probs,
    posterior_quantiles = posterior_quantiles,
    param_names = param_names_npe,
    architecture = arch_spec,
    diagnostics = diagnostics,
    model = npe_model,
    config_npe = config_npe,
    posteriors_npe = posteriors_npe,
    npe_summary = npe_summary,
    dirs = dirs,
    mode = if (embedded_mode) "embedded" else "standalone"
  )

  invisible(result)
}
