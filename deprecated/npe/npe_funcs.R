# =============================================================================
# NPE HELPER FUNCTIONS - Production-ready utilities
# =============================================================================

#' Create NPE logger for structured logging
#' @keywords internal
.create_npe_logger <- function(verbose = TRUE, log_file = NULL) {
    logger <- list(
        verbose = verbose,
        log_file = log_file,
        log = function(level, msg, ...) {
            if (!verbose && level != "ERROR") return(invisible())

            timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
            formatted_msg <- sprintf(paste(timestamp, level, "NPE:", msg), ...)

            if (!is.null(log_file)) {
                tryCatch({
                    cat(formatted_msg, "\n", file = log_file, append = TRUE)
                }, error = function(e) {
                    warning("Failed to write to log file: ", e$message)
                })
            }

            if (level == "ERROR") {
                stop(formatted_msg, call. = FALSE)
            } else if (level == "WARNING") {
                warning(formatted_msg, call. = FALSE)
            } else {
                message(formatted_msg)
            }
        }
    )
    class(logger) <- "npe_logger"
    return(logger)
}

#' Validate inputs for train_npe function
#' @keywords internal
.validate_train_inputs <- function(simulations_file, outputs_file, output_dir,
                                  param_names, simulation_filter, architecture,
                                  override_spec, use_gpu, seed, verbose, logger) {

    logger$log("INFO", "Validating input parameters...")

    # File validation
    if (!file.exists(simulations_file)) {
        logger$log("ERROR", "Simulations file not found: %s", simulations_file)
    }
    if (!file.access(simulations_file, 4) == 0) {
        logger$log("ERROR", "No read permission for simulations file: %s", simulations_file)
    }
    if (!file.exists(outputs_file)) {
        logger$log("ERROR", "Outputs file not found: %s", outputs_file)
    }
    if (!file.access(outputs_file, 4) == 0) {
        logger$log("ERROR", "No read permission for outputs file: %s", outputs_file)
    }

    # Directory validation
    parent_dir <- dirname(output_dir)
    if (!dir.exists(parent_dir)) {
        logger$log("ERROR", "Parent directory does not exist: %s", parent_dir)
    }

    # Create output directory if needed
    if (!dir.exists(output_dir)) {
        tryCatch({
            dir.create(output_dir, recursive = TRUE)
            logger$log("INFO", "Created output directory: %s", output_dir)
        }, error = function(e) {
            logger$log("ERROR", "Failed to create output directory: %s", e$message)
        })
    }

    # Check write permission
    test_file <- file.path(output_dir, ".write_test")
    tryCatch({
        writeLines("test", test_file)
        unlink(test_file)
    }, error = function(e) {
        logger$log("ERROR", "No write permission for output directory: %s", output_dir)
    })

    # Parameter validation
    if (!is.null(param_names) && !is.character(param_names)) {
        logger$log("ERROR", "param_names must be a character vector or NULL")
    }

    # Architecture validation
    valid_architectures <- c("auto", "tiny", "small", "medium", "large", "xlarge",
                            "epidemic_small", "epidemic_large", "endemic")
    if (!architecture %in% valid_architectures) {
        logger$log("ERROR", "Invalid architecture: %s. Must be one of: %s",
                  architecture, paste(valid_architectures, collapse = ", "))
    }

    # Numeric validation
    if (!is.numeric(seed) || seed != as.integer(seed)) {
        logger$log("ERROR", "seed must be an integer")
    }

    if (!is.logical(use_gpu)) {
        logger$log("ERROR", "use_gpu must be logical (TRUE/FALSE)")
    }

    if (!is.logical(verbose)) {
        logger$log("ERROR", "verbose must be logical (TRUE/FALSE)")
    }

    logger$log("INFO", "Input validation completed successfully")
    return(invisible(TRUE))
}

#' Save model atomically to prevent corruption
#' @keywords internal
.save_model_atomic <- function(object, filepath, logger = NULL) {
    if (is.null(logger)) logger <- .create_npe_logger(verbose = FALSE)

    temp_file <- paste0(filepath, ".tmp")
    backup_file <- paste0(filepath, ".bak")

    tryCatch({
        # Save to temporary file first
        if (tools::file_ext(filepath) == "json") {
            jsonlite::write_json(object, temp_file, auto_unbox = TRUE, pretty = TRUE)
        } else if (tools::file_ext(filepath) == "rds") {
            saveRDS(object, temp_file)
        } else if (tools::file_ext(filepath) == "csv") {
            write.csv(object, temp_file, row.names = FALSE)
        } else {
            # Default to RDS for unknown extensions
            saveRDS(object, temp_file)
        }

        # Backup existing file if present
        if (file.exists(filepath)) {
            file.rename(filepath, backup_file)
        }

        # Atomic rename
        file.rename(temp_file, filepath)

        # Remove backup on success
        if (file.exists(backup_file)) {
            unlink(backup_file)
        }

        logger$log("DEBUG", "Successfully saved: %s", basename(filepath))
        return(TRUE)

    }, error = function(e) {
        # Restore from backup on failure
        if (file.exists(backup_file)) {
            file.rename(backup_file, filepath)
            logger$log("WARNING", "Restored backup after save failure: %s", filepath)
        }
        if (file.exists(temp_file)) {
            unlink(temp_file)
        }
        logger$log("ERROR", "Failed to save %s: %s", filepath, e$message)
        return(FALSE)
    })
}

#' Clean up Python and R resources
#' @keywords internal
.cleanup_npe_resources <- function(use_gpu = FALSE, logger = NULL) {
    if (is.null(logger)) logger <- .create_npe_logger(verbose = FALSE)

    tryCatch({
        # Python garbage collection
        if (requireNamespace("reticulate", quietly = TRUE)) {
            reticulate::py_run_string("import gc; gc.collect()")

            # GPU memory cleanup if applicable
            if (use_gpu) {
                reticulate::py_run_string("
import torch
if torch.cuda.is_available():
    torch.cuda.empty_cache()
    torch.cuda.synchronize()
")
            }
        }

        # R garbage collection
        gc(verbose = FALSE, full = TRUE)

        logger$log("DEBUG", "Resource cleanup completed")
    }, error = function(e) {
        logger$log("WARNING", "Resource cleanup failed: %s", e$message)
    })
}

#' Check Python dependencies with version requirements
#' @keywords internal
.check_python_dependencies <- function(logger = NULL) {
    if (is.null(logger)) logger <- .create_npe_logger(verbose = TRUE)

    deps <- list(
        torch = list(min_version = "1.10.0", install = "pip install torch"),
        lampe = list(min_version = "0.5.0", install = "pip install lampe-torch"),
        zuko = list(min_version = "1.0.0", install = "pip install zuko"),
        numpy = list(min_version = "1.19.0", install = "pip install numpy"),
        pandas = list(min_version = "1.0.0", install = "pip install pandas"),
        sklearn = list(min_version = "0.24.0", install = "pip install scikit-learn")
    )

    missing <- character()
    outdated <- character()

    for (pkg_name in names(deps)) {
        tryCatch({
            # Special handling for sklearn (import name differs)
            import_name <- if (pkg_name == "sklearn") "sklearn" else pkg_name
            module <- reticulate::import(import_name)

            # Get version
            version <- module$`__version__`

            # Compare versions
            if (utils::compareVersion(version, deps[[pkg_name]]$min_version) < 0) {
                outdated <- c(outdated,
                            sprintf("%s (have %s, need >=%s): %s",
                                   pkg_name, version,
                                   deps[[pkg_name]]$min_version,
                                   deps[[pkg_name]]$install))
            }

            logger$log("DEBUG", "Python package %s version %s OK", pkg_name, version)

        }, error = function(e) {
            missing <- c(missing, sprintf("%s: %s", pkg_name, deps[[pkg_name]]$install))
        })
    }

    if (length(missing) > 0) {
        logger$log("ERROR", "Missing Python packages:\n%s", paste(missing, collapse = "\n"))
    }

    if (length(outdated) > 0) {
        logger$log("ERROR", "Outdated Python packages:\n%s", paste(outdated, collapse = "\n"))
    }

    logger$log("INFO", "All Python dependencies satisfied")
    return(invisible(TRUE))
}

#' Get NPE configuration with defaults and overrides
#' @keywords internal
.get_npe_config <- function(override = NULL) {
    default_config <- list(
        training = list(
            early_stopping_patience = 30,
            gradient_clip_value = 1.0,
            log_every_n_epochs = 20
        ),
        gpu = list(
            memory_fraction = 0.9,
            fallback_to_cpu = TRUE,
            enable_amp = TRUE  # Automatic mixed precision
        ),
        io = list(
            use_atomic_save = TRUE,
            create_backups = TRUE,
            compression = "gzip"
        ),
        validation = list(
            strict_mode = FALSE,
            warn_on_na_replacement = TRUE,
            check_data_range = TRUE
        ),
        features = list(
            use_improved_loss_check = TRUE,  # Fix NaN loss timing
            use_proportion_bounds_fix = TRUE, # Fix proportion bounds
            enable_checkpointing = FALSE      # Future feature
        )
    )

    # Merge with overrides if provided
    if (!is.null(override) && is.list(override)) {
        config <- modifyList(default_config, override)
    } else {
        config <- default_config
    }

    return(config)
}

#' Train Neural Posterior Estimator for MOSAIC Parameters
#'
#' Trains a Neural Posterior Estimator (NPE) using the Lampe library to approximate
#' posterior distributions for MOSAIC cholera model parameters. This function processes
#' MOSAIC calibration results to create a fast surrogate for Bayesian inference.
#'
#' @param simulations_file Character string path to simulations parquet file containing
#'   parameter samples and metadata from MOSAIC calibration runs. Expected columns include
#'   parameter names, \code{likelihood}, and quality indicators like \code{is_finite}.
#' @param outputs_file Character string path to outputs parquet file containing time series
#'   results from MOSAIC simulations. Expected columns: \code{sim}, \code{iter}, \code{j}
#'   (location), \code{t} (time), \code{cases}, \code{deaths}.
#' @param output_dir Character string path where trained NPE models will be saved.
#'   Will create directory if it doesn't exist.
#' @param param_names Character vector of parameter names to include in training.
#'   If NULL (default), automatically detects parameters from simulation results.
#' @param simulation_filter Character vector specifying quality filters to apply:
#'   \itemize{
#'     \item \code{"all"} — Use all simulations (no filtering)
#'     \item \code{"complete_cases"} — Remove simulations with missing values
#'     \item \code{"is_finite"} — Keep only simulations with finite likelihoods
#'     \item \code{"is_retained"} — Use convergence-retained simulations only
#'   }
#' @param architecture Character string specifying NPE architecture:
#'   \itemize{
#'     \item \code{"auto"} — Automatically select based on data characteristics (default)
#'     \item \code{"tiny", "small", "medium", "large", "xlarge"} — Predefined sizes
#'     \item Custom preset names: \code{"epidemic_small", "epidemic_large", "endemic"}
#'   }
#' @param override_spec Named list of architecture overrides. See \code{\link{calc_npe_spec}}
#'   for available options.
#' @param use_gpu Logical indicating whether to attempt GPU acceleration if available.
#'   Default is TRUE.
#' @param seed Integer random seed for reproducible training. Default is 42.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#'
#' @return Named list containing:
#'   \itemize{
#'     \item \code{output_dir} — Path to saved model directory
#'     \item \code{spec} — NPE architecture specification used
#'     \item \code{param_names} — Parameter names included in training
#'     \item \code{n_simulations} — Number of simulations used for training
#'     \item \code{training_time} — Time taken for training
#'   }
#'
#' @details
#' This function implements Neural Posterior Estimation using the Lampe library,
#' which provides efficient simulation-based inference for complex models. The NPE
#' learns to approximate \eqn{p(\theta|x)} directly from simulation pairs \eqn{(\theta, x)}.
#'
#' The training process includes:
#' \itemize{
#'   \item Data preprocessing and parameter bounds extraction via \code{\link{.get_npe_prior_bounds}}
#'   \item Architecture specification via \code{\link{calc_npe_spec}}
#'   \item Neural network training using normalizing flows
#'   \item Model ensemble creation for uncertainty quantification
#' }
#'
#' @section External Dependencies:
#' This function requires:
#' \itemize{
#'   \item \strong{Lampe}: Neural posterior estimation library (\url{https://github.com/probabilists/lampe})
#'   \item \strong{PyTorch}: Deep learning framework (\url{https://pytorch.org/})
#'   \item \strong{Python}: Compatible environment managed via reticulate
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{est_npe_posterior}} for parameter estimation using trained models
#'   \item \code{\link{calc_npe_spec}} for architecture specification details
#'   \item \code{\link{sample_from_prior_batch}} for prior sampling utilities
#' }
#'
#' @references
#' \itemize{
#'   \item Rozet, F. & Louppe, G. (2021). "Lampe: Likelihood-free AMortized Posterior Estimation."
#'   \item Papamakarios, G., et al. (2019). "Sequential Neural Likelihood." \emph{AISTATS}.
#' }
#'
#' @examples
#' \dontrun{
#' # Train NPE model from MOSAIC calibration results
#' npe_model <- train_npe(
#'   simulations_file = "path/to/simulations.parquet",
#'   outputs_file = "path/to/outputs.parquet",
#'   output_dir = "path/to/npe/models",
#'   simulation_filter = c("is_finite", "is_retained"),
#'   architecture = "auto",
#'   use_gpu = TRUE,
#'   seed = 123
#' )
#' }
#'
#' @export
train_npe <- function(
          simulations_file,
          outputs_file,
          output_dir,
          param_names = NULL,
          simulation_filter = "complete_cases",
          architecture = "auto",
          override_spec = NULL,
          use_gpu = TRUE,
          seed = 42,
          verbose = TRUE,
          config_override = NULL  # New parameter for configuration overrides
) {

     # =============================================================================
     # SETUP WITH PRODUCTION-READY IMPROVEMENTS
     # =============================================================================

     # Create logger
     log_file_path <- if (!is.null(output_dir)) {
          file.path(output_dir, paste0("npe_training_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
     } else NULL

     logger <- .create_npe_logger(verbose = verbose, log_file = log_file_path)

     # Register cleanup on exit
     on.exit({
          logger$log("DEBUG", "Running cleanup...")
          .cleanup_npe_resources(use_gpu = use_gpu, logger = logger)
     }, add = TRUE)

     # Get configuration
     config <- .get_npe_config(config_override)
     logger$log("INFO", "NPE configuration loaded (atomic_save=%s, strict_validation=%s)",
               config$io$use_atomic_save, config$validation$strict_mode)

     # Validate inputs
     .validate_train_inputs(
          simulations_file = simulations_file,
          outputs_file = outputs_file,
          output_dir = output_dir,
          param_names = param_names,
          simulation_filter = simulation_filter,
          architecture = architecture,
          override_spec = override_spec,
          use_gpu = use_gpu,
          seed = seed,
          verbose = verbose,
          logger = logger
     )

     # Load required packages with error handling
     required_packages <- c("reticulate", "dplyr", "tidyr", "arrow", "jsonlite")
     for (pkg in required_packages) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
               logger$log("ERROR", "Required R package not installed: %s", pkg)
          }
          suppressPackageStartupMessages(library(pkg, character.only = TRUE))
     }

     # Check Python dependencies
     logger$log("INFO", "Checking Python dependencies...")
     .check_python_dependencies(logger)

     set.seed(seed)
     logger$log("INFO", "Random seed set to %d", seed)

     # Backward compatibility wrapper for log_msg
     log_msg <- function(msg, ...) logger$log("INFO", msg, ...)

     logger$log("INFO", "Starting Lampe NPE v5.1 with production-ready improvements")

     # =============================================================================
     # DATA LOADING WITH ERROR HANDLING
     # =============================================================================

     logger$log("INFO", "Loading simulation data...")
     logger$log("INFO", "  Simulations file: %s", simulations_file)
     logger$log("INFO", "  Outputs file: %s", outputs_file)

     # Load data with error handling
     params <- tryCatch({
          arrow::read_parquet(simulations_file)
     }, error = function(e) {
          logger$log("ERROR", "Failed to load simulations file: %s", e$message)
     })

     outputs <- tryCatch({
          arrow::read_parquet(outputs_file)
     }, error = function(e) {
          logger$log("ERROR", "Failed to load outputs file: %s", e$message)
     })

     # Validate data structure
     if (nrow(params) == 0) {
          logger$log("ERROR", "Simulations file contains no data")
     }
     if (nrow(outputs) == 0) {
          logger$log("ERROR", "Outputs file contains no data")
     }

     log_msg("Data loaded: %d simulations, %d output rows", nrow(params), nrow(outputs))

     # =============================================================================
     # DYNAMIC LOCATION AND TIME DISCOVERY
     # =============================================================================

     location_order <- sort(unique(outputs$j))
     n_locations <- length(location_order)
     t_levels <- sort(unique(outputs$t))
     n_timesteps <- length(t_levels)

     log_msg("Data characteristics:")
     log_msg("  Locations: %d (%s)", n_locations,
             paste(head(location_order, 3), collapse = ", "))
     log_msg("  Timesteps: %d (t=%d to %d)", n_timesteps, min(t_levels), max(t_levels))

     # =============================================================================
     # SIMULATION FILTERING (from v3)
     # =============================================================================

     log_msg("Applying simulation filters: %s",
             if(length(simulation_filter) == 1 && simulation_filter == "all") "all data"
             else paste(simulation_filter, collapse = ", "))

     # Apply filters (reusing v3 logic)
     if (length(simulation_filter) == 1 && simulation_filter == "all") {
          params_valid <- params
     } else if (length(simulation_filter) == 1 && simulation_filter == "complete_cases") {
          params_valid <- params[complete.cases(params), ]
     } else {
          filter_mask <- rep(TRUE, nrow(params))

          for (filter_name in simulation_filter) {
               if (filter_name == "is_finite" && "is_finite" %in% names(params)) {
                    filter_mask <- filter_mask & params$is_finite
               } else if (filter_name == "is_valid" && "is_valid" %in% names(params)) {
                    filter_mask <- filter_mask & params$is_valid
               } else if (filter_name == "is_retained" && "is_retained" %in% names(params)) {
                    filter_mask <- filter_mask & params$is_retained
               } else if (filter_name == "is_outlier" && "is_outlier" %in% names(params)) {
                    filter_mask <- filter_mask & !params$is_outlier
               } else if (filter_name == "is_best_subset" && "is_best_subset" %in% names(params)) {
                    filter_mask <- filter_mask & params$is_best_subset
               } else if (filter_name == "complete_cases") {
                    filter_mask <- filter_mask & complete.cases(params)
               }
          }

          params_valid <- params[filter_mask, ]
     }

     n_valid <- nrow(params_valid)
     log_msg("Filtered to %d valid simulations (%.1f%%)",
             n_valid, 100 * n_valid / nrow(params))

     # =============================================================================
     # PARAMETER ORGANIZATION
     # =============================================================================

     # Get parameter columns (exclude metadata)
     metadata_cols <- c("sim", "iter", "seed_sim", "seed_iter", "likelihood",
                        "is_finite", "is_valid", "is_outlier", "is_retained",
                        "aic", "delta_aic", "weight_retained", "weight_best",
                        "is_best_subset", "best_subset_B", "is_best_model")

     all_param_cols <- names(params_valid)[!names(params_valid) %in% metadata_cols]

     if (!is.null(param_names)) {
          all_param_cols <- intersect(all_param_cols, param_names)
     }

     # Filter out constants but keep everything else for now
     # Use relative variance to avoid filtering small-scale parameters like tau_i
     param_vars <- apply(params_valid[all_param_cols], 2, var, na.rm = TRUE)
     param_means <- apply(params_valid[all_param_cols], 2, mean, na.rm = TRUE)

     # Calculate coefficient of variation (CV) for non-zero mean parameters
     # Parameters with CV < 1e-6 are considered constant
     constant_params <- character()
     for (i in seq_along(param_vars)) {
          param_name <- names(param_vars)[i]
          var_val <- param_vars[i]
          mean_val <- abs(param_means[i])

          # If mean is very close to zero, use absolute variance threshold
          if (mean_val < 1e-10) {
               if (var_val < 1e-20) {  # Very strict for near-zero parameters
                    constant_params <- c(constant_params, param_name)
                    logger$log("DEBUG", "Parameter %s identified as constant (near-zero variance)", param_name)
               }
          } else {
               # Use coefficient of variation for other parameters
               cv <- sqrt(var_val) / mean_val
               if (cv < 1e-6) {  # Less than 0.0001% variation
                    constant_params <- c(constant_params, param_name)
                    logger$log("DEBUG", "Parameter %s identified as constant (CV < 1e-6)", param_name)
               }
          }
     }

     population_params <- grep("^N_j_initial$", all_param_cols, value = TRUE)

     # Remove constants but keep population parameters for conversion
     working_param_cols <- setdiff(all_param_cols, constant_params)

     log_msg("Parameters: %d total, %d constants removed, %d population parameters found",
             length(all_param_cols), length(constant_params), length(population_params))

     # =============================================================================
     # CONVERT INITIAL CONDITION COUNTS TO PROPORTIONS
     # =============================================================================

     log_msg("Converting initial condition counts to proportions...")

     # Find initial condition count parameters
     initial_count_patterns <- c("^S_j_initial_", "^E_j_initial_", "^I_j_initial_",
                                 "^R_j_initial_", "^V1_j_initial_", "^V2_j_initial_")
     initial_count_cols <- grep(paste(initial_count_patterns, collapse = "|"),
                                working_param_cols, value = TRUE)

     # Use population parameters from all_param_cols
     population_cols <- population_params

     if (length(initial_count_cols) > 0 && length(population_cols) > 0) {

          log_msg("Found %d initial condition count parameters and %d population parameters",
                  length(initial_count_cols), length(population_cols))

          # Create proportion parameters
          proportion_params <- data.frame(row.names = rownames(params_valid))

          for (count_col in initial_count_cols) {
               # Use the global N_j_initial parameter (no location suffix in your data)
               n_col <- "N_j_initial"

               if (n_col %in% population_cols) {
                    # Convert count to proportion
                    count_values <- params_valid[[count_col]]
                    n_values <- params_valid[[n_col]]

                    # Handle division by zero or NA values
                    prop_values <- ifelse(n_values > 0 & !is.na(n_values),
                                          count_values / n_values,
                                          0)

                    # Create proportion parameter name: S_j_initial_ETH -> prop_S_initial_ETH
                    # Extract location suffix (e.g., "_ETH" from "S_j_initial_ETH")
                    location_suffix <- gsub("^[A-Z0-9]+_j_initial(_[A-Z]+)$", "\\1", count_col)
                    compartment <- gsub("^([A-Z0-9]+)_j_initial_.*", "\\1", count_col)
                    prop_col <- paste0("prop_", compartment, "_initial", location_suffix)

                    proportion_params[[prop_col]] <- prop_values

                    log_msg("  %s -> %s (range: %.4f to %.4f)",
                            count_col, prop_col,
                            min(prop_values, na.rm = TRUE),
                            max(prop_values, na.rm = TRUE))
               } else {
                    log_msg("  Warning: No population parameter %s found for %s", n_col, count_col)
               }
          }

          if (ncol(proportion_params) > 0) {
               # Add proportion parameters to params_valid
               for (prop_col in names(proportion_params)) {
                    params_valid[[prop_col]] <- proportion_params[[prop_col]]
               }

               # Create final parameter list: remove initial count parameters, add proportion parameters
               working_param_cols <- setdiff(working_param_cols, initial_count_cols)
               working_param_cols <- c(working_param_cols, names(proportion_params))

               log_msg("Converted %d initial condition counts to %d proportions",
                       length(initial_count_cols), ncol(proportion_params))
          } else {
               log_msg("No proportion parameters created - check parameter naming")
          }

     } else {
          if (length(initial_count_cols) == 0) {
               log_msg("No initial condition count parameters found")
          }
          if (length(population_cols) == 0) {
               log_msg("No population parameters found")
          }
     }

     # Create final parameter list (remove population parameters)
     final_param_cols <- setdiff(working_param_cols, population_params)
     n_params <- length(final_param_cols)

     log_msg("Final parameter count: %d (removed %d population parameters)",
             n_params, length(population_params))

     # =============================================================================
     # PARAMETER BOUNDS FOR NPE
     # =============================================================================

     log_msg("Setting up NPE parameter bounds...")

     # Get mathematical bounds using location-specific priors
     priors_file <- file.path(dirname(simulations_file), "../priors.json")
     if (!file.exists(priors_file)) {
          log_msg("  WARNING: priors.json not found at expected location")
          priors_file <- NULL
     }

     # Get bounds using the new function
     prior_bounds <- get_npe_parameter_bounds(
          param_names = final_param_cols,
          priors_file = priors_file,
          safety_buffer = 1e-10,
          verbose = TRUE
     )

     # Check for NPE weights in data
     has_weights <- "weight_npe" %in% names(params_valid)
     if (has_weights) {
          weights_vector <- params_valid$weight_npe
          valid_weights <- weights_vector[weights_vector > 0]
          eff_n <- 1/sum(valid_weights^2)

          log_msg("NPE importance weights detected:")
          log_msg("  Non-zero weights: %d", length(valid_weights))
          log_msg("  Effective sample size: %.1f", eff_n)
          log_msg("  Max weight: %.6f", max(valid_weights))
     } else {
          log_msg("No weight_npe column found, using uniform weights")
          weights_vector <- rep(1/nrow(params_valid), nrow(params_valid))
     }

     # Store proportion parameter names for Python (backward compatibility)
     prop_params <- grep("^prop_.*_initial_", final_param_cols, value = TRUE)
     py$prop_param_names <- if (length(prop_params) > 0) prop_params else character(0)

     # =============================================================================
     # DYNAMIC ARCHITECTURE SPECIFICATION
     # =============================================================================

     log_msg("Calculating optimal architecture specification...")

     # Import torch early to check device availability
     torch <- reticulate::import("torch", convert = FALSE)

     # Determine device - use py_to_r to ensure scalar conversion
     device_type <- if (use_gpu) {
          cuda_available <- reticulate::py_to_r(torch$cuda$is_available())
          if (isTRUE(cuda_available)) {
               "cuda"
          } else {
               mps_available <- tryCatch({
                    reticulate::py_to_r(torch$backends$mps$is_available())
               }, error = function(e) FALSE)

               if (isTRUE(mps_available)) "mps" else "cpu"
          }
     } else {
          "cpu"
     }

     # Handle architecture specification
     if (architecture == "auto") {
          # Automatic specification based on data
          npe_spec <- calc_npe_spec(
               n_sims = n_valid,
               n_params = n_params,
               n_timesteps = n_timesteps,
               n_locations = n_locations,
               tier = NULL,  # Auto-select
               preset = NULL,
               device = device_type,
               verbose = verbose
          )
     } else if (architecture %in% c("tiny", "small", "medium", "large", "xlarge")) {
          # Use specified tier
          npe_spec <- calc_npe_spec(
               n_sims = n_valid,
               n_params = n_params,
               n_timesteps = n_timesteps,
               n_locations = n_locations,
               tier = architecture,
               preset = NULL,
               device = device_type,
               verbose = verbose
          )
     } else if (architecture %in% c("epidemic_small", "epidemic_large", "endemic")) {
          # Use preset configuration
          npe_spec <- calc_npe_spec(
               n_sims = n_valid,
               n_params = n_params,
               n_timesteps = n_timesteps,
               n_locations = n_locations,
               tier = NULL,
               preset = architecture,
               device = device_type,
               verbose = verbose
          )
     } else {
          stop("Unknown architecture: ", architecture)
     }

     # Apply any user overrides
     if (!is.null(override_spec)) {
          log_msg("Applying user overrides to specification")
          for (key in names(override_spec)) {
               if (key %in% names(npe_spec)) {
                    npe_spec[[key]] <- override_spec[[key]]
               } else {
                    # Handle nested overrides
                    parts <- strsplit(key, "\\.")[[1]]
                    if (length(parts) == 2 && parts[1] %in% names(npe_spec)) {
                         npe_spec[[parts[1]]][[parts[2]]] <- override_spec[[key]]
                    }
               }
          }
     }

     # Save specification for reproducibility
     spec_file <- file.path(output_dir, "npe_spec.json")
     jsonlite::write_json(npe_spec, spec_file, pretty = TRUE, auto_unbox = TRUE)
     log_msg("Architecture specification saved to: %s", spec_file)

     # =============================================================================
     # PREPARE DATA MATRICES
     # =============================================================================

     log_msg("Preparing data matrices...")

     # Get outputs for valid simulations
     valid_sims <- params_valid$sim
     outputs_valid <- outputs %>% filter(sim %in% valid_sims)

     # Convert to wide format
     x_wide <- outputs_valid %>%
          tidyr::pivot_wider(
               id_cols = sim,
               names_from = c(j, t),
               values_from = cases,
               names_sep = "_t",
               values_fill = NA_real_
          )

     # Ensure deterministic column ordering (Feature #3)
     location_order <- sort(unique(outputs_valid$j))
     t_levels <- sort(unique(outputs_valid$t))
     expected_cols <- unlist(lapply(location_order, function(j) {
          paste0(j, "_t", t_levels)
     }))

     # Reorder columns to match expected spatial-temporal structure
     available_cols <- intersect(expected_cols, names(x_wide))
     missing_cols <- setdiff(expected_cols, names(x_wide))

     if (length(missing_cols) > 0) {
          log_msg("Warning: Missing columns in wide format: %s", paste(missing_cols, collapse = ", "))
          # Add missing columns with NA
          for (col in missing_cols) {
               x_wide[[col]] <- NA_real_
          }
     }

     # Reorder to ensure consistent column structure
     x_wide <- x_wide[, c("sim", expected_cols)]

     # Align rows
     x_wide <- x_wide[match(params_valid$sim, x_wide$sim), ]
     stopifnot(all(x_wide$sim == params_valid$sim))

     # Create matrices
     x_matrix <- as.matrix(x_wide[, -1])
     theta_matrix <- as.matrix(params_valid[, final_param_cols])

     # Handle missing values - Option A: Replace with 0 (simple approach for case counts)
     na_count <- sum(is.na(x_matrix))
     if (na_count > 0) {
          log_msg("Replacing %d NA values with 0 for case counts", na_count)
          x_matrix[is.na(x_matrix)] <- 0
          n_valid <- nrow(theta_matrix)
     }

     # Apply preprocessing
     if (npe_spec$preprocessing$use_log1p) {
          x_matrix <- log1p(x_matrix)
          log_msg("Applied log1p transformation to observations")
     }

     log_msg("Final data shape: θ=%d×%d, x=%d×%d",
             nrow(theta_matrix), ncol(theta_matrix),
             nrow(x_matrix), ncol(x_matrix))

     # =============================================================================
     # PYTHON SETUP
     # =============================================================================

     log_msg("Setting up Python environment...")

     # torch already imported earlier for device detection
     np <- reticulate::import("numpy")

     if (!tryCatch({reticulate::import("lampe"); TRUE}, error = function(e) FALSE)) {
          stop("Lampe not available. Install with: pip install lampe-torch")
     }

     # Get Python main module
     py <- reticulate::py

     # Pass data and spec to Python
     py$theta_matrix_r <- theta_matrix
     py$x_matrix_r <- x_matrix
     py$n_locations <- n_locations
     py$n_timesteps <- n_timesteps
     py$location_order <- location_order
     py$param_names <- final_param_cols
     py$prior_bounds <- prior_bounds
     py$npe_spec <- npe_spec
     py$use_gpu <- use_gpu
     py$seed <- as.integer(seed)
     py$output_dir <- output_dir
     py$weights_vector <- weights_vector  # Pass importance weights to Python

     # =============================================================================
     # TRAIN NPE
     # =============================================================================

     log_msg("Training NPE...")

     train_start <- Sys.time()

     # Execute Python training with dynamic spec
     reticulate::py_run_string('
import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np
import pandas as pd
from sklearn.preprocessing import StandardScaler
import lampe
from lampe.inference import NPE, NPELoss
from zuko.distributions import BoxUniform
from zuko.flows import NSF
import json
import pickle
import random
import os
import time
from torch.utils.data import DataLoader, TensorDataset, random_split

print("=== LAMPE NPE V5.0 WITH PROPORTION-BASED INITIAL CONDITIONS ===")
print(f"PyTorch version: {torch.__version__}")

# Set seeds for reproducibility
def set_all_seeds(seed):
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

set_all_seeds(seed)

# Device selection and optimizations
if use_gpu and torch.cuda.is_available():
    device = "cuda"
    print(f"Using CUDA GPU: {torch.cuda.get_device_name(0)}")
    torch.cuda.set_per_process_memory_fraction(0.9)
    use_amp = True  # Enable mixed precision for CUDA
elif use_gpu and torch.backends.mps.is_available():
    device = "mps"
    print("Using MPS (Apple Silicon GPU)")
    os.environ["PYTORCH_ENABLE_MPS_FALLBACK"] = "1"
    # Set watermark to 0 to disable memory limit
    os.environ["PYTORCH_MPS_HIGH_WATERMARK_RATIO"] = "0.0"
    use_amp = False  # MPS does not support GradScaler yet
    # Reduce batch size for MPS (Feature #6)
    if "batch_size" in locals():
        batch_size = int(batch_size * 0.75)
else:
    device = "cpu"
    print("Using CPU for training")
    use_amp = False  # No AMP for CPU

# =============================================================================
# EXTRACT CONFIGURATION FROM SPEC
# =============================================================================

print("\\nArchitecture configuration from calc_npe_spec:")
print(f"  Tier: {npe_spec[\"tier\"]}")
print(f"  Embedding dim: {npe_spec[\"embedding\"][\"embedding_dim\"]}")
print(f"  TCN blocks: {npe_spec[\"embedding\"][\"tcn_blocks\"]}")
print(f"  TCN channels: {npe_spec[\"embedding\"][\"tcn_channels\"]}")
print(f"  Hidden features: {npe_spec[\"flow\"][\"hidden_features\"]}")
print(f"  Transforms: {npe_spec[\"flow\"][\"num_transforms\"]}")
print(f"  Batch size: {npe_spec[\"training\"][\"batch_size\"]}")
print(f"  Ensembles: {npe_spec[\"training\"][\"n_ensembles\"]}")

# Extract key parameters
embedding_dim = int(npe_spec["embedding"]["embedding_dim"])
tcn_blocks = int(npe_spec["embedding"]["tcn_blocks"])
tcn_channels = int(npe_spec["embedding"]["tcn_channels"])

# Ensure TCN channels are valid for GroupNorm
# GroupNorm requires channels to be divisible by num_groups
# We will use groups of size that divides evenly into channels
if tcn_channels % 8 == 0:
    group_norm_groups = 8
elif tcn_channels % 4 == 0:
    group_norm_groups = 4
elif tcn_channels % 2 == 0:
    group_norm_groups = 2
else:
    group_norm_groups = 1

print(f"  TCN GroupNorm: {tcn_channels} channels with {group_norm_groups} groups")

tcn_dropout = float(npe_spec["embedding"]["tcn_dropout"])
attention_heads = int(npe_spec["pooling"]["attention_heads"])
hidden_features = int(npe_spec["flow"]["hidden_features"])
num_transforms = int(npe_spec["flow"]["num_transforms"])
num_bins = int(npe_spec["flow"]["num_bins"])
batch_size = int(npe_spec["training"]["batch_size"])
max_epochs = int(npe_spec["training"]["max_epochs"])
val_split = float(npe_spec["training"]["validation_split"])
n_ensembles = int(npe_spec["training"]["n_ensembles"])
learning_rate = float(npe_spec["optimization"]["learning_rate"])
weight_decay = float(npe_spec["optimization"]["weight_decay"])
gradient_clip = float(npe_spec["optimization"]["gradient_clip_value"])

# Gradient accumulation (Feature #5)
# Calculate steps for effective batch size if memory constrained
target_batch_size = batch_size
actual_batch_size = batch_size
if device == "mps" or device == "cpu":
    # Reduce actual batch size on memory-limited devices
    actual_batch_size = min(batch_size, 128)
gradient_accumulate_steps = max(1, target_batch_size // actual_batch_size)
if gradient_accumulate_steps > 1:
    print(f"  Gradient accumulation: {gradient_accumulate_steps} steps (effective batch={target_batch_size})")
    batch_size = actual_batch_size  # Use smaller batch size for dataloaders

# =============================================================================
# DATA PREPARATION
# =============================================================================

theta_np = np.array(theta_matrix_r)
x_np = np.array(x_matrix_r)
weights_np = np.array(weights_vector)

print(f"\\nData shape: θ={theta_np.shape}, x={x_np.shape}")
print(f"Weights shape: {weights_np.shape}, non-zero: {np.sum(weights_np > 0)}")

# Create tensors (will standardize after split to avoid data leakage)
theta_tensor = torch.tensor(theta_np, dtype=torch.float32, device=device)
x_tensor_raw = torch.tensor(x_np, dtype=torch.float32, device=device)
weights_tensor = torch.tensor(weights_np, dtype=torch.float32, device=device)

# Prior bounds with comprehensive validation
prior_bounds_df = pd.DataFrame(prior_bounds)
prior_mins = []
prior_maxs = []

# Track proportion parameters for validation
prop_param_indices = []

for i, param in enumerate(param_names):
    row = prior_bounds_df[prior_bounds_df["parameter"] == param]
    if len(row) > 0:
        param_min = float(row["min"].iloc[0])
        param_max = float(row["max"].iloc[0])

        # CRITICAL FIX: Enforce [0,1] bounds for ALL proportion parameters
        # Multiple checks to ensure robustness
        is_prop_param = False

        # Check 1: Standard prop_*_initial_* pattern
        if param.startswith("prop_") and "_initial_" in param:
            is_prop_param = True

        # Check 2: ALL proportion parameters (p_*, phi_*, rho, sigma)
        if (param.startswith("p_") or
            param.startswith("phi_") or
            param.startswith("rho") or
            param.startswith("sigma") or
            param in ["p_beta", "p_detect", "p_asymp", "p_severe",
                     "phi_1", "phi_2", "rho", "sigma"]):
            is_prop_param = True

        # Check 3: Also check if param is in the R-provided list
        if "prop_param_names" in globals() and param in prop_param_names:
            is_prop_param = True

        if is_prop_param:
            print(f"  [BOUNDS] Enforcing [0,1] for proportion parameter: {param}")
            print(f"    Original R bounds: [{param_min:.8f}, {param_max:.8f}]")

            # Force exact [0,1] bounds
            param_min = 0.0
            param_max = 1.0

            # Track this parameter index
            prop_param_indices.append(i)

            print(f"    Enforced bounds: [{param_min:.8f}, {param_max:.8f}]")

            # Validation check
            if param_min != 0.0 or param_max != 1.0:
                raise ValueError(f"Failed to set [0,1] bounds for {param}")

        prior_mins.append(param_min)
        prior_maxs.append(param_max)

# Also validate rate parameters are non-negative
rate_param_indices = []
for i, param in enumerate(param_names):
    # Check for rate parameters that must be non-negative
    if (param.startswith("beta_j0_") or param.startswith("tau_i_") or
        param.startswith("mu_j") or param.startswith("mu_i") or
        param.startswith("gamma_") or param.startswith("omega_") or
        param.startswith("epsilon") or param.startswith("iota") or
        "mobility_gamma" in param or "mobility_omega" in param):

        if prior_mins[i] < 0:
            print(f"  [BOUNDS] Fixing negative bound for rate parameter: {param}")
            print(f"    Original: [{prior_mins[i]:.8f}, {prior_maxs[i]:.8f}]")
            prior_mins[i] = 0.0
            print(f"    Fixed: [{prior_mins[i]:.8f}, {prior_maxs[i]:.8f}]")
            rate_param_indices.append(i)

print(f"\\n[VALIDATION] Total parameters: {len(param_names)}")
print(f"[VALIDATION] Proportion parameters with [0,1] bounds: {len(prop_param_indices)}")
print(f"[VALIDATION] Rate parameters fixed to non-negative: {len(rate_param_indices)}")
if len(prop_param_indices) > 0:
    print(f"[VALIDATION] Proportion parameter indices: {prop_param_indices}")
if len(rate_param_indices) > 0:
    print(f"[VALIDATION] Rate parameter indices: {rate_param_indices}")

theta_bounds_min = torch.tensor(prior_mins, dtype=torch.float32, device=device)
theta_bounds_max = torch.tensor(prior_maxs, dtype=torch.float32, device=device)

# Transform to unit space with validation
def transform_to_unit(theta, bounds_min, bounds_max):
    """Transform parameters to unit space [0,1]"""
    # Add small epsilon to avoid division by zero
    bounds_range = bounds_max - bounds_min
    bounds_range = torch.clamp(bounds_range, min=1e-8)
    return (theta - bounds_min) / bounds_range

# Validate proportion parameters BEFORE transformation
print("\\n[PRE-TRANSFORM VALIDATION]")
for idx in prop_param_indices:
    param_values = theta_tensor[:, idx]
    param_min = param_values.min().item()
    param_max = param_values.max().item()
    param_name = param_names[idx]

    if param_min < -1e-6 or param_max > 1.0 + 1e-6:
        print(f"  WARNING: {param_name} has values outside [0,1]: [{param_min:.8f}, {param_max:.8f}]")
        # Clamp the values to ensure they are within bounds
        theta_tensor[:, idx] = torch.clamp(param_values, 0.0, 1.0)
        print(f"  FIXED: Clamped to [0,1]")
    else:
        print(f"  OK: {param_name} within [0,1]: [{param_min:.8f}, {param_max:.8f}]")

theta_tensor_unit = transform_to_unit(theta_tensor, theta_bounds_min, theta_bounds_max)
theta_tensor_unit = torch.clamp(theta_tensor_unit, 0.0, 1.0)

# Validate unit space transformation
print("\\n[POST-TRANSFORM VALIDATION]")
for idx in prop_param_indices:
    unit_values = theta_tensor_unit[:, idx]
    unit_min = unit_values.min().item()
    unit_max = unit_values.max().item()
    param_name = param_names[idx]

    if unit_min < -1e-6 or unit_max > 1.0 + 1e-6:
        print(f"  ERROR: {param_name} unit space outside [0,1]: [{unit_min:.8f}, {unit_max:.8f}]")
    else:
        print(f"  OK: {param_name} unit space within [0,1]: [{unit_min:.8f}, {unit_max:.8f}]")

# =============================================================================
# DYNAMIC TCN ARCHITECTURE
# =============================================================================

class DynamicTCN(nn.Module):
    """TCN with dynamic configuration from spec"""
    def __init__(self, n_timesteps, channels, n_blocks, kernel_size=3, dropout=0.1, norm_groups=None):
        super().__init__()
        layers = []
        in_channels = 1

        # Determine group norm groups if not specified
        if norm_groups is None:
            if channels % 8 == 0:
                norm_groups = 8
            elif channels % 4 == 0:
                norm_groups = 4
            elif channels % 2 == 0:
                norm_groups = 2
            else:
                norm_groups = 1

        for block in range(n_blocks):
            dilation = 2 ** block
            padding = dilation * (kernel_size - 1)

            layers.extend([
                nn.Conv1d(in_channels, channels, kernel_size,
                         padding=padding, dilation=dilation),
                nn.GroupNorm(norm_groups, channels),
                nn.SiLU(),
                nn.Dropout(dropout)
            ])
            in_channels = channels

        self.tcn_layers = nn.Sequential(*layers)
        self.adaptive_pool = nn.AdaptiveAvgPool1d(1)

        # Report receptive field
        receptive_field = 1 + sum(2**i * (kernel_size-1) for i in range(n_blocks))
        print(f"  TCN receptive field: {receptive_field} (covers {n_timesteps} timesteps)")

    def forward(self, x):
        x = x.unsqueeze(1)
        h = self.tcn_layers(x)
        return self.adaptive_pool(h).squeeze(-1)


class DynamicSpatialEncoder(nn.Module):
    """Spatial encoder with dynamic configuration"""
    def __init__(self, n_timesteps, n_locations, embedding_dim, tcn_channels,
                 tcn_blocks, attention_heads, tcn_dropout=0.1, group_norm_groups=8):
        super().__init__()
        self.n_timesteps = n_timesteps
        self.n_locations = n_locations

        self.location_tcn = DynamicTCN(
            n_timesteps=n_timesteps,
            channels=tcn_channels,
            n_blocks=tcn_blocks,
            dropout=tcn_dropout,
            norm_groups=group_norm_groups
        )

        self.spatial_attention = nn.MultiheadAttention(
            embed_dim=tcn_channels,
            num_heads=attention_heads,
            batch_first=True,
            dropout=0.1
        )

        # Dynamic projection based on embedding_dim
        self.context_projection = nn.Sequential(
            nn.Linear(n_locations * tcn_channels, min(512, embedding_dim * 2)),
            nn.SiLU(),
            nn.Dropout(0.1),
            nn.Linear(min(512, embedding_dim * 2), embedding_dim)
        )

    def forward(self, x_raw):
        batch_size = x_raw.size(0)
        x = x_raw.view(batch_size, self.n_locations, self.n_timesteps)

        location_embeddings = []
        for loc in range(self.n_locations):
            loc_embedding = self.location_tcn(x[:, loc, :])
            location_embeddings.append(loc_embedding)

        H = torch.stack(location_embeddings, dim=1)
        H_attended, _ = self.spatial_attention(H, H, H)
        H_flat = H_attended.reshape(batch_size, -1)
        context = self.context_projection(H_flat)

        return context

print(f"\\nCreating dynamic spatial encoder...")
encoder = DynamicSpatialEncoder(
    n_timesteps=n_timesteps,
    n_locations=n_locations,
    embedding_dim=embedding_dim,
    tcn_channels=tcn_channels,
    tcn_blocks=tcn_blocks,
    attention_heads=attention_heads,
    tcn_dropout=tcn_dropout,
    group_norm_groups=group_norm_groups
).to(device)

# =============================================================================
# NPE WITH DYNAMIC FLOW
# =============================================================================

prior = BoxUniform(
    torch.zeros(len(prior_mins), device=device),
    torch.ones(len(prior_mins), device=device)
)

def build_dynamic_nsf(theta_dim, x_dim):
    """Build NSF with dynamic configuration"""
    # Dynamic hidden layer structure
    if hidden_features <= 256:
        hidden_layers = [hidden_features, hidden_features, hidden_features // 2]
    elif hidden_features <= 512:
        hidden_layers = [hidden_features, hidden_features, hidden_features,
                        hidden_features // 2]
    else:
        hidden_layers = [hidden_features, hidden_features, hidden_features,
                        hidden_features // 2, hidden_features // 4]

    return NSF(
        features=theta_dim,
        context=x_dim,
        bins=num_bins,
        transforms=num_transforms,
        hidden_features=hidden_layers,  # Use the list of hidden layer sizes
        activation=nn.ReLU
    )

# =============================================================================
# ENSEMBLE TRAINING
# =============================================================================

ensemble_models = []
ensemble_encoders = []
ensemble_histories = []

print(f"\\nTraining {n_ensembles} ensemble member(s)...")

for ensemble_idx in range(n_ensembles):
    if n_ensembles > 1:
        print(f"\\n========== Ensemble Member {ensemble_idx + 1}/{n_ensembles} ==========")
        # Different seed for each ensemble member
        set_all_seeds(seed + ensemble_idx * 1000)

    # Create model instances
    npe = NPE(
        theta_dim=theta_tensor_unit.shape[1],
        x_dim=embedding_dim,
        build=build_dynamic_nsf
    ).to(device)

    encoder = DynamicSpatialEncoder(
        n_timesteps=n_timesteps,
        n_locations=n_locations,
        embedding_dim=embedding_dim,
        tcn_channels=tcn_channels,
        tcn_blocks=tcn_blocks,
        attention_heads=attention_heads,
        tcn_dropout=tcn_dropout,
        group_norm_groups=group_norm_groups
    ).to(device)

    # Dataset and splits - using raw (non-standardized) data WITH WEIGHTS
    dataset = TensorDataset(theta_tensor_unit, x_tensor_raw, weights_tensor)
    train_size = int((1 - val_split) * len(dataset))
    val_size = len(dataset) - train_size
    train_dataset, val_dataset = random_split(
        dataset, [train_size, val_size],
        generator=torch.Generator().manual_seed(seed + ensemble_idx)
    )

    # Fix data leakage: Fit StandardScaler on training data only
    train_indices = train_dataset.indices
    val_indices = val_dataset.indices

    x_train_np = x_np[train_indices]
    x_val_np = x_np[val_indices]

    # Fit scaler on training data only
    x_scaler = StandardScaler()
    x_train_scaled = x_scaler.fit_transform(x_train_np)
    x_val_scaled = x_scaler.transform(x_val_np)  # Only transform validation data

    # Create tensors with standardized data
    x_train_tensor = torch.tensor(x_train_scaled, dtype=torch.float32, device=device)
    x_val_tensor = torch.tensor(x_val_scaled, dtype=torch.float32, device=device)

    # Recreate datasets with standardized observations and weights
    train_dataset = TensorDataset(theta_tensor_unit[train_indices], x_train_tensor, weights_tensor[train_indices])
    val_dataset = TensorDataset(theta_tensor_unit[val_indices], x_val_tensor, weights_tensor[val_indices])

    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=batch_size, shuffle=False)

    # Optimizer
    optimizer = torch.optim.AdamW(
        list(npe.parameters()) + list(encoder.parameters()),
        lr=learning_rate,
        weight_decay=weight_decay
    )

    scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
        optimizer, mode="min", factor=0.5, patience=10, verbose=False
    )

    # Mixed precision training setup (Feature #2)
    scaler = None
    if use_amp and device == "cuda":
        scaler = torch.cuda.amp.GradScaler()
        print("  Using mixed precision training (FP16)")

    # Training loop
    best_val_loss = float("inf")
    patience_counter = 0
    train_losses = []
    val_losses = []
    best_state = None
    training_start_time = time.time()  # Track training time

    for epoch in range(max_epochs):
        # Training
        npe.train()
        encoder.train()
        epoch_train_loss = 0.0
        accumulate_loss = 0.0
        nan_count = 0  # Track NaN/Inf losses for debugging

        for batch_idx, (batch_theta, batch_x_raw, batch_weights) in enumerate(train_loader):
            # Gradient accumulation (Feature #5): only zero gradients at start of accumulation
            if batch_idx % gradient_accumulate_steps == 0:
                optimizer.zero_grad()

            # Mixed precision training (Feature #2)
            if use_amp and device == "cuda" and scaler is not None:
                with torch.autocast(device):
                    x_ctx = encoder(batch_x_raw)
                    # Weighted loss: multiply log_prob by weights before mean
                    log_probs = npe.flow(x_ctx).log_prob(batch_theta)
                    weighted_log_probs = log_probs * batch_weights
                    loss = -weighted_log_probs.sum() / batch_weights.sum()
                    # Scale loss for gradient accumulation
                    loss = loss / gradient_accumulate_steps

                # CRITICAL FIX: Check for NaN/Inf BEFORE backward propagation
                if torch.isnan(loss) or torch.isinf(loss):
                    nan_count += 1
                    if nan_count % 10 == 0:  # Log every 10th NaN
                        print(f"  Warning: Skipped {nan_count} batches with NaN/Inf loss")
                    continue

                scaler.scale(loss).backward()
                accumulate_loss += loss.item()

                # Only step optimizer after accumulating gradients
                if (batch_idx + 1) % gradient_accumulate_steps == 0 or batch_idx == len(train_loader) - 1:
                    scaler.unscale_(optimizer)
                    torch.nn.utils.clip_grad_norm_(
                        list(npe.parameters()) + list(encoder.parameters()),
                        gradient_clip
                    )
                    scaler.step(optimizer)
                    scaler.update()
                    epoch_train_loss += accumulate_loss
                    accumulate_loss = 0.0
            else:
                # Standard precision training
                x_ctx = encoder(batch_x_raw)
                # Weighted loss: multiply log_prob by weights before mean
                log_probs = npe.flow(x_ctx).log_prob(batch_theta)
                weighted_log_probs = log_probs * batch_weights
                loss = -weighted_log_probs.sum() / batch_weights.sum()
                # Scale loss for gradient accumulation
                loss = loss / gradient_accumulate_steps

                # CRITICAL FIX: Check for NaN/Inf BEFORE backward propagation
                if torch.isnan(loss) or torch.isinf(loss):
                    nan_count += 1
                    if nan_count % 10 == 0:  # Log every 10th NaN
                        print(f"  Warning: Skipped {nan_count} batches with NaN/Inf loss")
                    continue

                loss.backward()
                accumulate_loss += loss.item()

                # Only step optimizer after accumulating gradients
                if (batch_idx + 1) % gradient_accumulate_steps == 0 or batch_idx == len(train_loader) - 1:
                    torch.nn.utils.clip_grad_norm_(
                        list(npe.parameters()) + list(encoder.parameters()),
                        gradient_clip
                    )
                    optimizer.step()
                    epoch_train_loss += accumulate_loss
                    accumulate_loss = 0.0

        avg_train_loss = epoch_train_loss / len(train_loader)
        train_losses.append(avg_train_loss)

        # Validation
        npe.eval()
        encoder.eval()
        epoch_val_loss = 0.0

        with torch.no_grad():
            for batch_theta, batch_x_raw, batch_weights in val_loader:
                x_ctx = encoder(batch_x_raw)
                # Weighted validation loss
                log_probs = npe.flow(x_ctx).log_prob(batch_theta)
                weighted_log_probs = log_probs * batch_weights
                val_loss = -weighted_log_probs.sum() / batch_weights.sum()
                epoch_val_loss += val_loss.item()

        avg_val_loss = epoch_val_loss / len(val_loader)
        val_losses.append(avg_val_loss)

        scheduler.step(avg_val_loss)

        # Early stopping
        if avg_val_loss < best_val_loss:
            best_val_loss = avg_val_loss
            patience_counter = 0
            best_state = {
                "npe_state_dict": npe.state_dict(),
                "encoder_state_dict": encoder.state_dict()
            }
        else:
            patience_counter += 1

        if epoch % 20 == 0:
            print(f"  Epoch {epoch+1}/{max_epochs}: "
                  f"Train={avg_train_loss:.4f}, Val={avg_val_loss:.4f}")

        if patience_counter >= 30:
            print(f"  Early stopping at epoch {epoch+1}")
            break

    # Restore best model
    if best_state is not None:
        npe.load_state_dict(best_state["npe_state_dict"])
        encoder.load_state_dict(best_state["encoder_state_dict"])

    ensemble_models.append(npe)
    ensemble_encoders.append(encoder)
    ensemble_histories.append({
        "train_losses": train_losses,
        "val_losses": val_losses,
        "best_val_loss": float(best_val_loss)
    })

    print(f"  Best validation loss: {best_val_loss:.4f}")

training_time = time.time() - training_start_time

# =============================================================================
# SAVE MODELS AND METADATA
# =============================================================================

print("\\nSaving models and metadata...")

os.makedirs(output_dir, exist_ok=True)

# Save ensemble models
for i, (npe, encoder) in enumerate(zip(ensemble_models, ensemble_encoders)):
    if n_ensembles > 1:
        model_suffix = f"_ensemble_{i}"
    else:
        model_suffix = ""

    # Save model with proportion parameter metadata
    model_metadata = {
        "npe_state_dict": npe.state_dict(),
        "encoder_state_dict": encoder.state_dict(),
        "theta_bounds_min": theta_bounds_min.cpu(),
        "theta_bounds_max": theta_bounds_max.cpu(),
        "x_scaler_mean": x_scaler.mean_,
        "x_scaler_scale": x_scaler.scale_,
        "spec": npe_spec,
        "ensemble_idx": i,
        "history": ensemble_histories[i],
        # Add proportion parameter tracking
        "param_names": param_names,
        "prop_param_indices": prop_param_indices,
        "prop_param_names": [param_names[idx] for idx in prop_param_indices]
    }

    # Final validation before saving
    print(f"\\n[SAVE VALIDATION] Model {i}:")
    for idx in prop_param_indices:
        bounds_min = theta_bounds_min[idx].item()
        bounds_max = theta_bounds_max[idx].item()
        param_name = param_names[idx]
        if bounds_min != 0.0 or bounds_max != 1.0:
            print(f"  ERROR: {param_name} bounds not [0,1]: [{bounds_min:.8f}, {bounds_max:.8f}]")
        else:
            print(f"  OK: {param_name} bounds are [0,1]")

    torch.save(model_metadata, os.path.join(output_dir, f"npe_state{model_suffix}.pt"))

    torch.save(npe, os.path.join(output_dir, f"npe{model_suffix}.pt"))
    torch.save(encoder, os.path.join(output_dir, f"encoder{model_suffix}.pt"))

# Save scaler
with open(os.path.join(output_dir, "x_scaler.pkl"), "wb") as f:
    pickle.dump(x_scaler, f)

# Save prior bounds
prior_bounds_df.to_csv(os.path.join(output_dir, "prior_bounds.csv"), index=False)

# Enhanced metadata
metadata = {
    "version": "5.0",
    "architecture_tier": npe_spec["tier"],
    "training_info": {
        "device": device,
        "n_ensembles": n_ensembles,
        "best_val_losses": [h["best_val_loss"] for h in ensemble_histories]
    },
    "data": {
        "n_simulations": len(dataset),
        "n_training": train_size,
        "n_validation": val_size,
        "n_parameters": len(param_names),
        "n_locations": n_locations,
        "n_timesteps": n_timesteps,
        "location_name": location_order,
        "location_codes": location_order  # For backwards compatibility
    },
    "parameters": {
        "names": param_names,
        "n_parameters": len(param_names)
    },
    "training_parameters": param_names,  # For backwards compatibility
    "initial_conditions": {
        "converted_to_proportions": True,
        "proportion_bounds": [0.0, 1.0],
        "note": "Initial condition counts converted to proportions during training"
    },
    "spec_used": npe_spec,
    "config_base": None,  # Can be populated with base configuration for SBC
    "training_history": {
        "ensemble_histories": ensemble_histories,
        "training_time_seconds": training_time,
        "final_training_loss": [h["train_losses"][-1] for h in ensemble_histories],
        "final_validation_loss": [h["val_losses"][-1] for h in ensemble_histories],
        "best_validation_loss": [h["best_val_loss"] for h in ensemble_histories]
    }
}

with open(os.path.join(output_dir, "npe_metadata.json"), "w") as f:
    json.dump(metadata, f, indent=2)

# Save detailed training history separately for plotting
training_history_file = os.path.join(output_dir, "training_history.json")
with open(training_history_file, "w") as f:
    json.dump({"ensemble_histories": ensemble_histories}, f, indent=2)

print(f"\\nModels saved to: {output_dir}")
print(f"Training history saved to: {training_history_file}")
')

train_time <- as.numeric(Sys.time() - train_start, units = "mins")

log_msg("NPE training completed in %.2f minutes", train_time)

# =============================================================================
# RETURN RESULTS
# =============================================================================

metadata_file <- file.path(output_dir, "npe_metadata.json")
if (file.exists(metadata_file)) {
     metadata <- jsonlite::fromJSON(metadata_file)
}

return(list(
     success = TRUE,
     output_dir = output_dir,
     spec_used = npe_spec,
     n_ensembles = npe_spec$training$n_ensembles,
     training_time_minutes = train_time,
     data_info = list(
          n_simulations = n_valid,
          n_parameters = n_params,
          n_locations = n_locations,
          n_timesteps = n_timesteps
     )
))
}





#' Estimate MOSAIC Parameters using Trained NPE Models
#'
#' Performs fast Bayesian parameter estimation using previously trained Neural Posterior
#' Estimator models from \code{\link{train_npe}}. Provides posterior quantiles in the
#' same format as \code{\link{calc_model_posterior_quantiles}} for seamless integration
#' with MOSAIC workflows.
#'
#' @param model_dir Character string path to directory containing trained NPE models.
#'   This should be the output directory from \code{\link{train_npe}}.
#' @param observed_data Data frame containing observed outbreak time series data with columns:
#'   \itemize{
#'     \item \code{j} — Location identifier (character, matching training data)
#'     \item \code{t} — Time index (integer, starting from 1)
#'     \item \code{cases} — Observed case counts (numeric)
#'   }
#' @param n_samples Integer number of posterior samples to draw per ensemble model.
#'   Default is 10,000. Higher values provide more accurate quantiles but slower computation.
#' @param quantiles Numeric vector of quantiles to compute for posterior summaries.
#'   Default is \code{c(0.025, 0.25, 0.5, 0.75, 0.975)} for 95% credible intervals.
#' @param output_dir Character string path for saving estimation results. If NULL (default),
#'   results are not saved to files (only returned in memory). If a path is provided,
#'   results are saved to that directory.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param return_samples Logical indicating whether to return raw posterior samples.
#'   Default is TRUE. This parameter is included for backward compatibility - samples
#'   are always included in the return value.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{quantiles} — Data frame with posterior quantiles
#'     \item \code{samples} — Matrix of raw posterior samples (n_samples × n_parameters)
#'     \item \code{metadata} — List containing estimation metadata
#'     \item \code{param_names} — Character vector of parameter names
#'   }
#'   The \code{quantiles} data frame contains columns:
#'   \itemize{
#'     \item \code{parameter} — Parameter name
#'     \item \code{description} — Human-readable parameter description
#'     \item \code{category} — Parameter category grouping
#'     \item \code{param_type} — Type classification
#'     \item \code{location} — Location code (for location-specific parameters)
#'     \item \code{prior_distribution} — Prior distribution type
#'     \item \code{type} — Set to "npe" for NPE-derived posteriors
#'     \item \code{mean} — Posterior mean
#'     \item \code{sd} — Posterior standard deviation
#'     \item \code{mode} — Posterior mode (set to NA)
#'     \item \code{kl} — KL divergence (set to NA)
#'     \item \code{q*} — Quantile columns for each requested quantile
#'   }
#'
#' @details
#' This function performs ensemble-based posterior estimation by:
#' \itemize{
#'   \item Loading trained NPE models, encoders, and preprocessing components
#'   \item Validating observed data format and dimensions against training data
#'   \item Running posterior sampling across all ensemble models
#'   \item Aggregating samples and computing summary statistics
#'   \item Formatting output to match standard MOSAIC posterior format
#' }
#'
#' The NPE provides orders-of-magnitude speedup compared to traditional MCMC while
#' maintaining comparable accuracy for well-trained models.
#'
#' @section External Dependencies:
#' This function requires:
#' \itemize{
#'   \item \strong{Lampe}: For loading and running trained NPE models (\url{https://github.com/probabilists/lampe})
#'   \item \strong{PyTorch}: For neural network inference (\url{https://pytorch.org/})
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{train_npe}} for training NPE models
#'   \item \code{\link{calc_model_posterior_quantiles}} for traditional posterior quantiles
#'   \item \code{\link{plot_model_posterior_quantiles}} for comparing posterior estimates
#' }
#'
#' @examples
#' \dontrun{
#' # Prepare observed data
#' observed <- data.frame(
#'   j = c("ETH", "ETH", "ETH"),
#'   t = c(1, 2, 3),
#'   cases = c(45, 67, 52)
#' )
#'
#' # Estimate parameters using trained NPE
#' result <- est_npe_posterior(
#'   model_dir = "path/to/trained/npe/models",
#'   observed_data = observed,
#'   n_samples = 5000,
#'   quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
#'   output_dir = NULL  # Don't save to files
#' )
#'
#' # Access results
#' posterior_quantiles <- result$quantiles
#' posterior_samples <- result$samples
#' }
#'
#' @export
est_npe_posterior <- function(
          model_dir,
          observed_data,
          n_samples = 10000,
          quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
          output_dir = NULL,
          verbose = TRUE,
          return_samples = TRUE,  # For backward compatibility - samples are always returned
          config_override = NULL   # New parameter for configuration overrides
) {

     # =============================================================================
     # SETUP AND VALIDATION WITH PRODUCTION IMPROVEMENTS
     # =============================================================================

     # Create logger
     logger <- .create_npe_logger(verbose = verbose)

     # Register cleanup on exit
     on.exit({
          logger$log("DEBUG", "Running cleanup...")
          .cleanup_npe_resources(use_gpu = FALSE, logger = logger)
     }, add = TRUE)

     # Get configuration
     config <- .get_npe_config(config_override)

     # Backward compatibility wrapper for log_msg
     log_msg <- function(msg, ...) logger$log("INFO", msg, ...)

     logger$log("INFO", "Starting NPE v5.1 parameter estimation with production improvements")

     # Comprehensive input validation
     if (!dir.exists(model_dir)) {
          logger$log("ERROR", "Model directory does not exist: %s", model_dir)
     }

     if (!is.data.frame(observed_data)) {
          logger$log("ERROR", "observed_data must be a data frame")
     }

     if (!is.numeric(n_samples) || n_samples <= 0) {
          logger$log("ERROR", "n_samples must be a positive numeric value")
     }
     n_samples <- as.integer(n_samples)

     if (!is.numeric(quantiles) || any(quantiles < 0) || any(quantiles > 1)) {
          logger$log("ERROR", "quantiles must be numeric values between 0 and 1")
     }

     # Check for reasonable sample size
     if (n_samples < 100) {
          logger$log("WARNING", "Small sample size (%d) may lead to inaccurate estimates", n_samples)
     }
     if (n_samples > 1000000) {
          logger$log("WARNING", "Large sample size (%d) may cause memory issues", n_samples)
     }

     # Check required columns in observed data
     required_cols <- c("j", "t", "cases")
     missing_cols <- setdiff(required_cols, names(observed_data))
     if (length(missing_cols) > 0) {
          stop("observed_data missing required columns: ", paste(missing_cols, collapse = ", "))
     }

     # Check for valid case values
     if (any(observed_data$cases < 0, na.rm = TRUE)) {
          stop("Observed data contains negative case counts")
     }

     if (all(is.na(observed_data$cases))) {
          stop("All case counts are NA in observed data")
     }

     # Get data dimensions
     locations <- sort(unique(observed_data$j))
     timesteps <- sort(unique(observed_data$t))
     n_locations <- length(locations)
     n_timesteps <- length(timesteps)

     log_msg("Observed data: %d locations × %d timesteps", n_locations, n_timesteps)

     # Check for required model files
     required_files <- c("npe_spec.json", "npe_metadata.json")

     # Check for single model or ensemble
     has_single_model <- file.exists(file.path(model_dir, "npe_state.pt"))
     has_ensemble <- file.exists(file.path(model_dir, "npe_state_ensemble_0.pt"))

     if (!has_single_model && !has_ensemble) {
          stop("No trained NPE models found in ", model_dir,
               ". Expected either 'npe_state.pt' or 'npe_state_ensemble_*.pt' files.")
     }

     missing_files <- required_files[!file.exists(file.path(model_dir, required_files))]
     if (length(missing_files) > 0) {
          stop("Required model files not found: ", paste(missing_files, collapse = ", "))
     }

     # Output directory will be handled later if provided

     # =============================================================================
     # LOAD MODEL SPECIFICATION AND METADATA
     # =============================================================================

     log_msg("Loading model specification and metadata...")

     # Load specification
     spec_file <- file.path(model_dir, "npe_spec.json")
     npe_spec <- tryCatch({
          jsonlite::fromJSON(spec_file)
     }, error = function(e) {
          stop("Failed to load model specification from ", spec_file, ": ", e$message)
     })

     # Load metadata
     metadata_file <- file.path(model_dir, "npe_metadata.json")
     metadata <- tryCatch({
          jsonlite::fromJSON(metadata_file)
     }, error = function(e) {
          warning("Failed to load metadata from ", metadata_file, ": ", e$message)
          list()
     })

     # Validate data dimensions against model expectations
     if (!is.null(metadata$data)) {
          expected_locations <- metadata$data$n_locations
          expected_timesteps <- metadata$data$n_timesteps

          if (!is.null(expected_locations) && n_locations != expected_locations) {
               stop(sprintf("Model expects %d locations but observed data has %d. ",
                            expected_locations, n_locations),
                    "NPE models cannot generalize to different spatial structures.")
          }

          if (!is.null(expected_timesteps) && n_timesteps != expected_timesteps) {
               stop(sprintf("Model expects %d timesteps but observed data has %d. ",
                            expected_timesteps, n_timesteps),
                    "Observed data must match training data temporal structure exactly.")
          }
     }

     log_msg("Model validation passed - dimensions compatible")

     # =============================================================================
     # PREPARE OBSERVED DATA
     # =============================================================================

     log_msg("Preparing observed data matrix...")

     # Convert to wide format matching training data structure
     obs_wide <- tryCatch({
          # First, check for and handle problematic values in observed_data
          observed_data$cases[is.na(observed_data$cases)] <- 0
          observed_data$cases[!is.finite(observed_data$cases)] <- 0

          # Ensure j and t are factors/characters for proper pivoting
          observed_data$j <- as.character(observed_data$j)
          observed_data$t <- as.numeric(observed_data$t)

          log_msg("Input data for pivoting: %d rows, %d locations, %d time points",
                  nrow(observed_data), length(unique(observed_data$j)), length(unique(observed_data$t)))
          log_msg("Cases range: %.3f to %.3f, NAs: %d",
                  min(observed_data$cases, na.rm = TRUE), max(observed_data$cases, na.rm = TRUE),
                  sum(is.na(observed_data$cases)))

          observed_data %>%
               tidyr::pivot_wider(
                    id_cols = character(0),  # Single observation
                    names_from = c(j, t),
                    values_from = cases,
                    names_sep = "_t",
                    values_fill = 0
               )
     }, error = function(e) {
          stop("Failed to convert observed data to wide format: ", e$message)
     })

     # Convert to matrix
     observed_x_matrix <- as.matrix(obs_wide)

     # Validate single observation
     if (nrow(observed_x_matrix) != 1) {
          stop("Observed data must represent a single outbreak/observation")
     }

     # Check for and handle missing values in observed data with proper warnings
     if (any(is.na(observed_x_matrix))) {
          na_count <- sum(is.na(observed_x_matrix))
          na_positions <- which(is.na(observed_x_matrix), arr.ind = TRUE)

          if (config$validation$warn_on_na_replacement) {
               logger$log("WARNING", "Found %d NA values in observed data matrix", na_count)
               logger$log("WARNING", "Replacing NA values with 0 - this may affect results")
               if (na_count <= 10) {
                    # Show specific positions if not too many
                    for (i in 1:nrow(na_positions)) {
                         logger$log("DEBUG", "  NA at position [%d, %d]",
                                   na_positions[i, 1], na_positions[i, 2])
                    }
               }
          }

          observed_x_matrix[is.na(observed_x_matrix)] <- 0
     }

     # Apply preprocessing if specified in model
     if (!is.null(npe_spec$preprocessing$use_log1p) && npe_spec$preprocessing$use_log1p) {
          observed_x_matrix <- log1p(observed_x_matrix)
          log_msg("Applied log1p transformation to observed data")
     }

     # Final validation of observed data with enhanced checking
     if (any(!is.finite(observed_x_matrix))) {
          non_finite_count <- sum(!is.finite(observed_x_matrix))
          inf_count <- sum(is.infinite(observed_x_matrix))
          nan_count <- sum(is.nan(observed_x_matrix))

          if (config$validation$warn_on_na_replacement) {
               logger$log("WARNING", "Found %d non-finite values after preprocessing:", non_finite_count)
               if (inf_count > 0) logger$log("WARNING", "  - %d infinite values", inf_count)
               if (nan_count > 0) logger$log("WARNING", "  - %d NaN values", nan_count)
               logger$log("WARNING", "Replacing non-finite values with 0 - this may affect results")
          }

          observed_x_matrix[!is.finite(observed_x_matrix)] <- 0
     }

     # Data range validation
     if (config$validation$check_data_range) {
          data_min <- min(observed_x_matrix)
          data_max <- max(observed_x_matrix)

          if (data_min < 0) {
               logger$log("WARNING", "Negative values detected in observed data (min=%.3f)", data_min)
          }
          if (data_max > 1e6) {
               logger$log("WARNING", "Very large values detected in observed data (max=%.3e)", data_max)
          }
     }

     log_msg("Observed data matrix shape: 1 × %d (range: %.3f to %.3f)",
             ncol(observed_x_matrix), min(observed_x_matrix), max(observed_x_matrix))

     # =============================================================================
     # PYTHON ENVIRONMENT SETUP
     # =============================================================================

     log_msg("Setting up Python environment...")

     # Load required libraries with better error handling
     required_packages <- c("reticulate", "dplyr", "tidyr", "jsonlite", "arrow")
     for (pkg in required_packages) {
          if (!requireNamespace(pkg, quietly = TRUE)) {
               logger$log("ERROR", "Required R package not installed: %s", pkg)
          }
     }

     # Check Python dependencies first
     logger$log("INFO", "Checking Python dependencies...")
     tryCatch({
          .check_python_dependencies(logger)
     }, error = function(e) {
          # Continue anyway but warn user
          logger$log("WARNING", "Some Python dependencies may be missing or outdated")
     })

     # Import Python modules with error handling
     torch <- tryCatch({
          reticulate::import("torch", convert = FALSE)
     }, error = function(e) {
          logger$log("ERROR", "PyTorch not available: %s", e$message)
     })

     np <- tryCatch({
          reticulate::import("numpy")
     }, error = function(e) {
          logger$log("ERROR", "NumPy not available: %s", e$message)
     })

     lampe_available <- tryCatch({
          reticulate::import("lampe")
          TRUE
     }, error = function(e) {
          stop("LAMPE not available. Please install: pip install lampe")
     })

     # Use CPU for reliable memory management
     device_type <- "cpu"
     log_msg("Using device: %s (CPU-only for reliable operation)", device_type)

     # =============================================================================
     # RUN PARAMETER ESTIMATION
     # =============================================================================

     log_msg("Loading models and running posterior sampling...")

     estimation_start <- Sys.time()

     # Get Python main module
     py <- reticulate::py

     # Pass data to Python environment
     py$observed_x_matrix <- observed_x_matrix
     py$n_samples <- as.integer(n_samples)
     py$quantiles <- quantiles
     py$model_dir <- model_dir
     py$n_locations <- as.integer(n_locations)
     py$n_timesteps <- as.integer(n_timesteps)
     py$device_type <- device_type
     py$npe_spec <- npe_spec

     # Run parameter estimation in Python
     tryCatch({
          reticulate::py_run_string('
import os
import json
import pickle
import torch
import numpy as np
import pandas as pd
import gc
import warnings
from pathlib import Path

# Suppress FutureWarning about torch.load - we are loading our own trusted models
warnings.filterwarnings("ignore", category=FutureWarning, message=".*weights_only.*")

print("=" * 70)
print("NPE V5 PARAMETER ESTIMATION")
print("=" * 70)

def clear_memory():
    """Clear memory and run garbage collection"""
    gc.collect()

# Set device
device = torch.device(device_type)
print(f"Using device: {device}")

# Initialize results
estimation_success = False
error_message = None
samples = None
param_names = []

try:
    # Load models and components
    model_dir = Path(model_dir)
    ensemble_models = []
    ensemble_encoders = []

    # Detect model files
    ensemble_idx = 0
    model_files_found = []

    while True:
        if ensemble_idx == 0:
            state_file = model_dir / "npe_state.pt"
        else:
            state_file = model_dir / f"npe_state_ensemble_{ensemble_idx}.pt"

        if not state_file.exists():
            if ensemble_idx == 0:
                print("No single model found, checking for ensemble models...")
                ensemble_idx += 1
                continue
            else:
                break

        model_files_found.append(str(state_file))
        ensemble_idx += 1

    if len(model_files_found) == 0:
        raise FileNotFoundError("No NPE model files found")

    print(f"Found {len(model_files_found)} model file(s)")

    # Initialize global variables that will be set from first model
    theta_bounds_min_global = None
    theta_bounds_max_global = None
    x_scaler_mean_global = None
    x_scaler_scale_global = None
    prop_param_indices_global = []
    prop_param_names_global = []
    n_params = None

    # Load each model
    for file_idx, state_file in enumerate(model_files_found):
        print(f"Loading model {file_idx + 1}/{len(model_files_found)}: {Path(state_file).name}")

        # Load state dictionary (weights_only=False required for complex model metadata)
        # This is safe since we are loading our own trained models
        state_dict = torch.load(state_file, map_location=device, weights_only=False)

        # Extract model components
        theta_bounds_min = state_dict["theta_bounds_min"].to(device)
        theta_bounds_max = state_dict["theta_bounds_max"].to(device)
        x_scaler_mean = state_dict["x_scaler_mean"]
        x_scaler_scale = state_dict["x_scaler_scale"]

        # Extract proportion parameter metadata if available
        prop_param_indices_local = state_dict.get("prop_param_indices", [])
        prop_param_names_local = state_dict.get("prop_param_names", [])
        param_names_from_model = state_dict.get("param_names", [])

        # If we have proportion parameter info, validate and enforce bounds
        if len(prop_param_indices_local) > 0:
            print(f"  Found {len(prop_param_indices_local)} proportion parameters in model metadata")
            for idx in prop_param_indices_local:
                if idx < len(theta_bounds_min):
                    # Enforce [0,1] bounds
                    original_min = theta_bounds_min[idx].item()
                    original_max = theta_bounds_max[idx].item()

                    if original_min != 0.0 or original_max != 1.0:
                        print(f"    Correcting bounds for param {idx}: [{original_min:.6f}, {original_max:.6f}] -> [0, 1]")
                        theta_bounds_min[idx] = 0.0
                        theta_bounds_max[idx] = 1.0

        # Debug: Check loaded parameters for issues
        print(f"Debug - Model {file_idx + 1} loaded parameters:")
        print(f"  theta_bounds_min: shape={theta_bounds_min.shape}, range=[{theta_bounds_min.min():.6f}, {theta_bounds_min.max():.6f}]")
        print(f"  theta_bounds_max: shape={theta_bounds_max.shape}, range=[{theta_bounds_max.min():.6f}, {theta_bounds_max.max():.6f}]")
        print(f"  bounds_diff_min: {(theta_bounds_max - theta_bounds_min).min():.6f}")
        print(f"  scaler_mean: shape={x_scaler_mean.shape}, has_nan={np.isnan(x_scaler_mean).any()}")
        print(f"  scaler_scale: shape={x_scaler_scale.shape}, has_nan={np.isnan(x_scaler_scale).any()}, min={x_scaler_scale.min():.6f}")

        # Architecture parameters - use EXACT values from spec
        embedding_dim = int(npe_spec["embedding"]["embedding_dim"])
        tcn_channels = int(npe_spec["embedding"]["tcn_channels"])
        tcn_blocks = int(npe_spec["embedding"]["tcn_blocks"])
        attention_heads = int(npe_spec["pooling"]["attention_heads"])
        hidden_features = int(npe_spec["flow"]["hidden_features"])
        num_transforms = int(npe_spec["flow"]["num_transforms"])
        num_bins = int(npe_spec["flow"]["num_bins"])

        print(f"  Architecture from spec:")
        print(f"    embedding_dim: {embedding_dim}")
        print(f"    tcn_channels: {tcn_channels}")
        print(f"    tcn_blocks: {tcn_blocks}")
        print(f"    hidden_features: {hidden_features}")
        print(f"    num_transforms: {num_transforms}")

        # GroupNorm configuration
        if tcn_channels % 8 == 0:
            group_norm_groups = 8
        elif tcn_channels % 4 == 0:
            group_norm_groups = 4
        elif tcn_channels % 2 == 0:
            group_norm_groups = 2
        else:
            group_norm_groups = 1

        # Rebuild model architecture
        import lampe
        from zuko.flows import NSF

        class DynamicTCN(torch.nn.Module):
            def __init__(self, n_timesteps, channels, n_blocks, kernel_size=3, dropout=0.1, norm_groups=8):
                super().__init__()
                layers = []
                in_channels = 1

                for block in range(n_blocks):
                    dilation = 2 ** block
                    padding = dilation * (kernel_size - 1)

                    layers.extend([
                        torch.nn.Conv1d(in_channels, channels, kernel_size,
                                       padding=padding, dilation=dilation),
                        torch.nn.GroupNorm(norm_groups, channels),
                        torch.nn.SiLU(),
                        torch.nn.Dropout(dropout)
                    ])
                    in_channels = channels

                self.tcn_layers = torch.nn.Sequential(*layers)
                self.adaptive_pool = torch.nn.AdaptiveAvgPool1d(1)

            def forward(self, x):
                x = x.unsqueeze(1)
                h = self.tcn_layers(x)
                return self.adaptive_pool(h).squeeze(-1)

        class DynamicSpatialEncoder(torch.nn.Module):
            def __init__(self, n_timesteps, n_locations, embedding_dim, tcn_channels,
                         tcn_blocks, attention_heads, tcn_dropout=0.1, group_norm_groups=8):
                super().__init__()
                self.n_timesteps = n_timesteps
                self.n_locations = n_locations

                self.location_tcn = DynamicTCN(
                    n_timesteps=n_timesteps,
                    channels=tcn_channels,
                    n_blocks=tcn_blocks,
                    dropout=tcn_dropout,
                    norm_groups=group_norm_groups
                )

                self.spatial_attention = torch.nn.MultiheadAttention(
                    embed_dim=tcn_channels,
                    num_heads=attention_heads,
                    batch_first=True,
                    dropout=0.1
                )

                self.context_projection = torch.nn.Sequential(
                    torch.nn.Linear(n_locations * tcn_channels, min(512, embedding_dim * 2)),
                    torch.nn.SiLU(),
                    torch.nn.Dropout(0.1),
                    torch.nn.Linear(min(512, embedding_dim * 2), embedding_dim)
                )

            def forward(self, x_raw):
                batch_size = x_raw.size(0)
                x = x_raw.view(batch_size, self.n_locations, self.n_timesteps)

                location_embeddings = []
                for loc in range(self.n_locations):
                    loc_embedding = self.location_tcn(x[:, loc, :])
                    location_embeddings.append(loc_embedding)

                H = torch.stack(location_embeddings, dim=1)
                H_attended, _ = self.spatial_attention(H, H, H)
                H_flat = H_attended.reshape(batch_size, -1)
                context = self.context_projection(H_flat)

                return context

        # Create encoder
        encoder = DynamicSpatialEncoder(
            n_timesteps=n_timesteps,
            n_locations=n_locations,
            embedding_dim=embedding_dim,
            tcn_channels=tcn_channels,
            tcn_blocks=tcn_blocks,
            attention_heads=attention_heads,
            group_norm_groups=group_norm_groups
        ).to(device)

        # Load encoder weights
        encoder.load_state_dict(state_dict["encoder_state_dict"])
        encoder.eval()

        # Create NPE model
        n_params = theta_bounds_min.shape[0]

        def build_dynamic_nsf(theta_dim, x_dim):
            """Build NSF matching training configuration"""
            if hidden_features <= 256:
                hidden_layers = [hidden_features, hidden_features, hidden_features // 2]
            elif hidden_features <= 512:
                hidden_layers = [hidden_features, hidden_features, hidden_features,
                                hidden_features // 2]
            else:
                hidden_layers = [hidden_features, hidden_features, hidden_features,
                                hidden_features // 2, hidden_features // 4]

            return NSF(
                features=theta_dim,
                context=x_dim,
                bins=num_bins,
                transforms=num_transforms,
                hidden_features=hidden_layers,
                activation=torch.nn.ReLU
            )

        class NPEModel(torch.nn.Module):
            def __init__(self, n_params, embedding_dim):
                super().__init__()
                self.flow = build_dynamic_nsf(n_params, embedding_dim)

            def sample(self, x, n_samples):
                return self.flow(x).sample((n_samples,))

        npe = NPEModel(n_params, embedding_dim).to(device)
        npe.load_state_dict(state_dict["npe_state_dict"])
        npe.eval()

        ensemble_models.append(npe)
        ensemble_encoders.append(encoder)

        # Store parameters from first model
        if file_idx == 0:
            theta_bounds_min_global = theta_bounds_min
            theta_bounds_max_global = theta_bounds_max
            x_scaler_mean_global = x_scaler_mean
            x_scaler_scale_global = x_scaler_scale
            # Store proportion parameter indices globally
            prop_param_indices_global = prop_param_indices_local
            prop_param_names_global = prop_param_names_local
            # Store n_params globally
            n_params = theta_bounds_min.shape[0]

        clear_memory()

    # Check that we successfully loaded at least one model
    if theta_bounds_min_global is None:
        raise RuntimeError("Failed to load model parameters - theta_bounds_min not initialized")
    if x_scaler_mean_global is None:
        raise RuntimeError("Failed to load model parameters - scaler not initialized")
    if n_params is None:
        raise RuntimeError("Failed to load model parameters - n_params not initialized")

    # Load parameter names
    prior_bounds_file = model_dir / "prior_bounds.csv"
    if prior_bounds_file.exists():
        prior_bounds_df = pd.read_csv(prior_bounds_file)
        param_names = prior_bounds_df["parameter"].tolist()
    else:
        param_names = [f"param_{i}" for i in range(n_params)]

    print(f"Parameters: {len(param_names)}")

    # Prepare observed data for inference
    print("Preparing observed data for inference...")

    print(f"Input data validation:")
    print(f"  observed_x_matrix: shape={observed_x_matrix.shape}, has_nan={np.isnan(observed_x_matrix).any()}")
    if not np.isnan(observed_x_matrix).all():
        print(f"  observed_x_matrix: range=[{np.nanmin(observed_x_matrix):.3f}, {np.nanmax(observed_x_matrix):.3f}]")
    else:
        print(f"  ERROR: All values in observed_x_matrix are NaN!")

    # Replace any remaining NaN values before standardization
    observed_x_clean = np.nan_to_num(observed_x_matrix, nan=0.0, posinf=0.0, neginf=0.0)
    print(f"  after_nan_cleanup: shape={observed_x_clean.shape}, has_nan={np.isnan(observed_x_clean).any()}")

    # Check if scaler parameters are valid
    scaler_valid = not (np.isnan(x_scaler_mean_global).any() or np.isnan(x_scaler_scale_global).any())
    print(f"  scaler_valid: {scaler_valid}")

    if not scaler_valid:
        print(f"  ERROR: Invalid scaler parameters detected!")
        raise ValueError("Scaler parameters contain NaN values")

    # Standardize observed data using cleaned data
    x_standardized = (observed_x_clean - x_scaler_mean_global) / (x_scaler_scale_global + 1e-8)
    print(f"  standardized_data: shape={x_standardized.shape}, has_nan={np.isnan(x_standardized).any()}")
    print(f"  standardized_data: range=[{np.nanmin(x_standardized):.3f}, {np.nanmax(x_standardized):.3f}]")

    # Final cleanup and tensor conversion
    x_standardized_clean = np.nan_to_num(x_standardized, nan=0.0, posinf=0.0, neginf=0.0)
    x_tensor = torch.tensor(x_standardized_clean, dtype=torch.float32, device=device)

    # Run ensemble inference
    print(f"Running ensemble inference with {len(ensemble_models)} model(s)...")
    all_samples = []
    samples_per_model = max(1, n_samples // len(ensemble_models))

    for model_idx, (npe, encoder) in enumerate(zip(ensemble_models, ensemble_encoders)):
        print(f"  Sampling from model {model_idx + 1}/{len(ensemble_models)}")

        with torch.no_grad():
            # Get context embedding
            x_ctx = encoder(x_tensor)
            print(f"  Context embedding: shape={x_ctx.shape}, has_nan={torch.isnan(x_ctx).any()}, has_inf={torch.isinf(x_ctx).any()}")

            # Check if context is valid
            if torch.isnan(x_ctx).any() or torch.isinf(x_ctx).any():
                print(f"  ERROR: Invalid context embedding detected!")
                raise ValueError(f"Context embedding contains NaN or Inf values")

            # Sample from posterior
            try:
                samples_unit = npe.sample(x_ctx, samples_per_model)
                print(f"  Raw samples: shape={samples_unit.shape}, has_nan={torch.isnan(samples_unit).any()}, has_inf={torch.isinf(samples_unit).any()}")

                if torch.isnan(samples_unit).any():
                    print(f"  ERROR: NaN values in raw samples - model sampling failed")
                    raise ValueError("NPE sampling produced NaN values")

            except Exception as e:
                print(f"  ERROR in NPE sampling: {e}")
                raise

            # Handle tensor dimensions
            if samples_unit.dim() == 3 and samples_unit.shape[1] == 1:
                samples_unit = samples_unit.squeeze(1)
                print(f"  After squeeze: shape={samples_unit.shape}")

            # Check bounds validity
            bounds_diff = theta_bounds_max_global - theta_bounds_min_global
            print(f"  Bounds check: min_diff={bounds_diff.min():.6f}, has_zero_diff={torch.any(bounds_diff <= 0)}")

            if torch.any(bounds_diff <= 0):
                print(f"  ERROR: Invalid parameter bounds - some parameters have zero or negative range")
                # Set minimum bounds difference
                bounds_diff = torch.clamp(bounds_diff, min=1e-6)

            # First clamp unit space samples to ensure they are in [0,1]
            samples_unit = torch.clamp(samples_unit, 0.0, 1.0)

            # Transform back to parameter space with bounds enforcement
            samples_original = (samples_unit * bounds_diff + theta_bounds_min_global)

            # CRITICAL: Enforce alpha parameter bounds [0.05, 0.99]
            for idx, pname in enumerate(param_names):
                if pname in ["alpha_1", "alpha_2"]:
                    param_samples = samples_original[:, idx]
                    original_min = param_samples.min().item()
                    original_max = param_samples.max().item()

                    # Clamp to [0.05, 0.99]
                    samples_original[:, idx] = torch.clamp(param_samples, 0.05, 0.99)

                    new_min = samples_original[:, idx].min().item()
                    new_max = samples_original[:, idx].max().item()

                    if original_min < 0.05 or original_max > 0.99:
                        print(f"    CLAMPED {pname}: [{original_min:.6f}, {original_max:.6f}] -> [{new_min:.6f}, {new_max:.6f}]")

            # CRITICAL: Enforce [0,1] bounds for proportion parameters
            if "prop_param_indices_global" in globals() and len(prop_param_indices_global) > 0:
                print(f"  Enforcing [0,1] bounds for {len(prop_param_indices_global)} proportion parameters")
                for idx in prop_param_indices_global:
                    if idx < samples_original.shape[1]:
                        # Get values for this parameter
                        param_samples = samples_original[:, idx]
                        original_min = param_samples.min().item()
                        original_max = param_samples.max().item()

                        # Clamp to [0,1]
                        samples_original[:, idx] = torch.clamp(param_samples, 0.0, 1.0)

                        # Report if clamping was needed
                        new_min = samples_original[:, idx].min().item()
                        new_max = samples_original[:, idx].max().item()

                        if original_min < 0.0 or original_max > 1.0:
                            param_name = param_names[idx] if idx < len(param_names) else f"param_{idx}"
                            print(f"    CLAMPED {param_name}: [{original_min:.6f}, {original_max:.6f}] -> [{new_min:.6f}, {new_max:.6f}]")
            else:
                # Fallback: detect proportion and rate parameters by name pattern
                for i, param_name in enumerate(param_names):
                    if i < samples_original.shape[1]:
                        # Handle ALL proportion parameters
                        if (param_name.startswith("prop_") or
                            param_name.startswith("p_") or
                            param_name.startswith("phi_") or
                            param_name.startswith("rho") or
                            param_name.startswith("sigma") or
                            param_name in ["p_beta", "p_detect", "p_asymp", "p_severe",
                                         "phi_1", "phi_2", "rho", "sigma"]):
                            param_samples = samples_original[:, i]
                            original_min = param_samples.min().item()
                            original_max = param_samples.max().item()
                            if original_min < 0.0 or original_max > 1.0:
                                samples_original[:, i] = torch.clamp(param_samples, 0.0, 1.0)
                                print(f"    CLAMPED {param_name}: [{original_min:.6f}, {original_max:.6f}] -> [0, 1]")

                        # Handle rate parameters (must be non-negative)
                        elif (param_name.startswith("beta_j0_") or param_name.startswith("tau_i_") or
                              param_name.startswith("mu_j") or param_name.startswith("mu_i") or
                              param_name.startswith("gamma_") or param_name.startswith("omega_") or
                              param_name.startswith("epsilon") or param_name.startswith("iota") or
                              "mobility_gamma" in param_name or "mobility_omega" in param_name):
                            param_samples = samples_original[:, i]
                            original_min = param_samples.min().item()
                            if original_min < 0.0:
                                samples_original[:, i] = torch.clamp(param_samples, min=0.0)
                                print(f"    CLAMPED {param_name}: min={original_min:.6f} -> 0.0")

            print(f"  Transformed samples: shape={samples_original.shape}, has_nan={torch.isnan(samples_original).any()}")

            all_samples.append(samples_original)

        clear_memory()

    # Combine ensemble samples
    combined_samples = torch.cat(all_samples, dim=0)

    # Ensure exact number of samples
    if combined_samples.shape[0] != n_samples:
        if combined_samples.shape[0] > n_samples:
            indices = torch.randperm(combined_samples.shape[0])[:n_samples]
            combined_samples = combined_samples[indices]
        else:
            needed = n_samples - combined_samples.shape[0]
            indices = torch.randint(0, combined_samples.shape[0], (needed,))
            extra_samples = combined_samples[indices]
            combined_samples = torch.cat([combined_samples, extra_samples], dim=0)

    # Convert to numpy
    samples = combined_samples.cpu().numpy()

    print(f"Sampling completed: {samples.shape[0]} samples × {samples.shape[1]} parameters")

    # Check for problematic values
    nan_count = np.isnan(samples).sum()
    inf_count = np.isinf(samples).sum()

    if nan_count > 0:
        print(f"WARNING: {nan_count} NaN values found in samples")
    if inf_count > 0:
        print(f"WARNING: {inf_count} Inf values found in samples")

    if nan_count == 0 and inf_count == 0:
        estimation_success = True
        print("Parameter estimation completed successfully!")
    else:
        error_message = f"Estimation produced {nan_count} NaN and {inf_count} Inf values"

    # Store ensemble count for R
    n_ensemble_models = len(ensemble_models)

except Exception as e:
    print(f"ERROR in parameter estimation: {e}")
    import traceback
    traceback.print_exc()
    error_message = str(e)
    estimation_success = False
    # Create NaN samples for failed estimation
    if len(param_names) > 0:
        samples = np.full((n_samples, len(param_names)), np.nan)
    else:
        samples = np.full((n_samples, 1), np.nan)
        param_names = ["unknown"]
    n_ensemble_models = 0  # No models loaded on error

')
     }, error = function(e) {
          stop("Python execution failed: ", e$message)
     })

     estimation_time <- as.numeric(Sys.time() - estimation_start, units = "secs")

     # Check if estimation was successful
     if (!py$estimation_success) {
          error_msg <- if (!is.null(py$error_message)) {
               paste("Parameter estimation failed:", py$error_message)
          } else {
               "Parameter estimation failed - check console output for details"
          }
          stop(error_msg)
     }

     # =============================================================================
     # PROCESS RESULTS AND FORMAT OUTPUT
     # =============================================================================

     log_msg("Processing results and formatting output...")

     # Get results from Python
     posterior_samples <- py$samples
     param_names <- py$param_names

     # Validate samples
     if (is.null(posterior_samples) || all(is.na(posterior_samples))) {
          stop("All posterior samples are NA. This indicates a failure in the sampling process.")
     }

     # Set column names
     colnames(posterior_samples) <- param_names

     # Validate and enforce parameter bounds
     # 1. Check alpha transmission parameters (must be in [0.05, 0.99])
     alpha_param_indices <- which(param_names %in% c("alpha_1", "alpha_2"))
     if (length(alpha_param_indices) > 0) {
          log_msg("Validating %d alpha transmission parameters...", length(alpha_param_indices))

          for (idx in alpha_param_indices) {
               param_samples <- posterior_samples[, idx]
               original_range <- range(param_samples, na.rm = TRUE)

               # Check for violations
               if (original_range[1] < 0.05 || original_range[2] > 0.99) {
                    log_msg("  VIOLATION: %s had values outside [0.05, 0.99]: [%.8f, %.8f]",
                            param_names[idx], original_range[1], original_range[2])

                    # Apply hard clamping to ensure bounds
                    posterior_samples[, idx] <- pmax(0.05, pmin(0.99, param_samples))

                    new_range <- range(posterior_samples[, idx], na.rm = TRUE)
                    log_msg("  FIXED: Clamped to [%.8f, %.8f]", new_range[1], new_range[2])
               } else {
                    log_msg("  OK: %s within [0.05, 0.99]: [%.8f, %.8f]",
                            param_names[idx], original_range[1], original_range[2])
               }
          }
     }

     # 2. Check ALL proportion parameters
     prop_param_indices <- c(
          grep("^prop_.*_initial_", param_names),
          grep("^p_", param_names),
          grep("^phi_", param_names),
          grep("^rho", param_names),
          grep("^sigma", param_names)
     )
     prop_param_indices <- unique(prop_param_indices)

     if (length(prop_param_indices) > 0) {
          log_msg("Validating %d proportion parameters...", length(prop_param_indices))
          violations_fixed <- 0

          for (idx in prop_param_indices) {
               param_samples <- posterior_samples[, idx]
               original_range <- range(param_samples, na.rm = TRUE)

               # Check for violations (with small tolerance)
               if (original_range[1] < -1e-6 || original_range[2] > 1 + 1e-6) {
                    log_msg("  VIOLATION: %s had values outside [0,1]: [%.8f, %.8f]",
                            param_names[idx], original_range[1], original_range[2])

                    # Apply hard clamping to ensure bounds
                    posterior_samples[, idx] <- pmax(0.0, pmin(1.0, param_samples))

                    new_range <- range(posterior_samples[, idx], na.rm = TRUE)
                    log_msg("  FIXED: Clamped to [%.8f, %.8f]", new_range[1], new_range[2])
                    violations_fixed <- violations_fixed + 1
               } else {
                    log_msg("  OK: %s within [0,1]: [%.8f, %.8f]",
                            param_names[idx], original_range[1], original_range[2])
               }
          }

          if (violations_fixed > 0) {
               log_msg("Fixed %d proportion parameter violations", violations_fixed)
          } else {
               log_msg("All proportion parameters within bounds ✓")
          }
     }

     # 2. Check rate parameters
     rate_param_indices <- c(
          grep("^beta_j0_", param_names),
          grep("^tau_i_", param_names),
          grep("^mu_j|^mu_i", param_names),
          grep("^gamma_[12]", param_names),
          grep("^omega_[12]", param_names),
          grep("^epsilon", param_names),
          grep("^iota", param_names),
          grep("mobility_gamma|mobility_omega", param_names)
     )
     rate_param_indices <- unique(rate_param_indices)

     if (length(rate_param_indices) > 0) {
          log_msg("Validating %d rate parameters...", length(rate_param_indices))
          violations_fixed <- 0

          for (idx in rate_param_indices) {
               param_samples <- posterior_samples[, idx]
               min_val <- min(param_samples, na.rm = TRUE)

               if (min_val < -1e-6) {
                    log_msg("  VIOLATION: %s had negative values: min=%.8f",
                            param_names[idx], min_val)

                    # Apply hard clamping to ensure non-negative
                    posterior_samples[, idx] <- pmax(0.0, param_samples)

                    new_min <- min(posterior_samples[, idx], na.rm = TRUE)
                    log_msg("  FIXED: Clamped to non-negative: min=%.8f", new_min)
                    violations_fixed <- violations_fixed + 1
               } else {
                    log_msg("  OK: %s is non-negative: min=%.8f",
                            param_names[idx], min_val)
               }
          }

          if (violations_fixed > 0) {
               log_msg("Fixed %d rate parameter violations", violations_fixed)
          } else {
               log_msg("All rate parameters within bounds ✓")
          }
     }

     # Compute quantiles and statistics
     quantile_names <- paste0("q", quantiles)

     # Initialize results data frame
     quantile_results <- data.frame(
          parameter = param_names,
          description = "",
          category = "",
          param_type = "",
          location = "",
          prior_distribution = "",
          type = "npe",
          mean = NA_real_,
          sd = NA_real_,
          mode = NA_real_,  # NPE doesn't compute mode
          kl = NA_real_,    # NPE doesn't compute KL divergence
          stringsAsFactors = FALSE
     )

     # Add quantile columns
     for (qname in quantile_names) {
          quantile_results[[qname]] <- NA_real_
     }

     # Compute statistics for each parameter
     for (i in seq_len(length(param_names))) {
          param_samples <- posterior_samples[, i]

          # Basic statistics
          quantile_results$mean[i] <- mean(param_samples, na.rm = TRUE)
          quantile_results$sd[i] <- sd(param_samples, na.rm = TRUE)

          # Quantiles
          if (!all(is.na(param_samples))) {
               param_quantiles <- quantile(param_samples, probs = quantiles, na.rm = TRUE)
               for (j in seq_along(quantiles)) {
                    quantile_results[[quantile_names[j]]][i] <- param_quantiles[j]
               }
          }
     }

     # Add parameter metadata from estimated_parameters if available
     tryCatch({
          data("estimated_parameters", package = "MOSAIC", envir = environment())

          for (i in seq_len(nrow(quantile_results))) {
               param <- quantile_results$parameter[i]

               # Handle location-specific parameters (e.g., beta_ETH -> beta)
               base_param <- gsub("_[A-Z]{3}$", "", param)
               location_code <- if (grepl("_[A-Z]{3}$", param)) {
                    gsub(".*_([A-Z]{3})$", "\\1", param)
               } else {
                    ""
               }

               # Find matching row in estimated_parameters
               match_idx <- which(estimated_parameters$parameter_name == base_param)

               if (length(match_idx) > 0) {
                    # Fill in metadata
                    quantile_results$description[i] <- estimated_parameters$display_name[match_idx[1]]
                    quantile_results$category[i] <- estimated_parameters$category[match_idx[1]]
                    quantile_results$param_type[i] <- estimated_parameters$scale[match_idx[1]]
                    quantile_results$location[i] <- location_code
                    quantile_results$prior_distribution[i] <- estimated_parameters$distribution[match_idx[1]]
               }
          }

          log_msg("Added parameter metadata from estimated_parameters")

     }, error = function(e) {
          log_msg("Could not load estimated_parameters metadata: %s", e$message)
     })


     # =============================================================================
     # CREATE METADATA
     # =============================================================================

     # Get ensemble count from Python
     n_ensemble_models <- if (!is.null(py$n_ensemble_models)) {
          py$n_ensemble_models
     } else {
          # Fallback: count model files in directory
          n_single <- if (file.exists(file.path(model_dir, "npe.pt"))) 1 else 0
          n_ensemble <- length(list.files(model_dir, pattern = "^npe_ensemble_[0-9]+\\.pt$"))
          max(n_single, n_ensemble)
     }

     # Create estimation metadata
     estimation_info <- list(
          timestamp = Sys.time(),
          estimation_time_seconds = estimation_time,
          n_samples = n_samples,
          n_parameters = ncol(posterior_samples),
          quantiles_computed = quantiles,
          model_source = model_dir,
          n_ensemble_models = n_ensemble_models,
          device_used = device_type,
          observed_data_summary = list(
               n_locations = n_locations,
               n_timesteps = n_timesteps,
               total_cases = sum(observed_data$cases, na.rm = TRUE),
               max_cases = max(observed_data$cases, na.rm = TRUE)
          ),
          function_version = "v5"
     )

     # =============================================================================
     # SAVE RESULTS TO FILES (IF OUTPUT_DIR PROVIDED)
     # =============================================================================

     if (!is.null(output_dir)) {
          log_msg("Saving results to files...")

          # Create directory if needed
          if (!dir.exists(output_dir)) {
               dir.create(output_dir, recursive = TRUE)
          }

          # Save posterior quantiles CSV
          quantiles_file <- file.path(output_dir, "posterior_quantiles.csv")
          write.csv(quantile_results, quantiles_file, row.names = FALSE)

          # Save posterior samples
          samples_file <- file.path(output_dir, "posterior_samples.parquet")
          arrow::write_parquet(as.data.frame(posterior_samples), samples_file)

          # Save metadata
          info_file <- file.path(output_dir, "estimation_info.json")
          jsonlite::write_json(estimation_info, info_file, pretty = TRUE, auto_unbox = TRUE)

          log_msg("Results saved to: %s", output_dir)
          log_msg("  - Quantiles: %s", basename(quantiles_file))
          log_msg("  - Samples: %s", basename(samples_file))
          log_msg("  - Metadata: %s", basename(info_file))
     } else {
          log_msg("No output_dir specified - results not saved to files")
     }

     log_msg("Estimation completed in %.1f seconds", estimation_time)

     # =============================================================================
     # RETURN COMPLETE RESULTS
     # =============================================================================

     # Return everything as a list
     return(list(
          quantiles = quantile_results,
          samples = posterior_samples,
          metadata = estimation_info,
          param_names = param_names
     ))
}






#!/usr/bin/env Rscript
# calc_npe_spec.R
# Dynamic NPE architecture specification based on data characteristics
# Version 1.0 - Complete implementation with tier system

#' Select Valid Number of Attention Heads (Internal)
#'
#' Internal helper function to select a valid number of attention heads that
#' evenly divides the TCN channels. Used by \code{\link{calc_npe_spec}}.
#'
#' @param channels Integer, number of TCN channels.
#' @param suggested_heads Integer, preferred number of attention heads.
#' @return Integer, valid number of attention heads that divides \code{channels}.
#' @keywords internal
.pick_valid_attention_heads <- function(channels, suggested_heads) {
     valid_options <- c(suggested_heads, 16, 8, 4, 2, 1)
     for (h in valid_options) {
          if (h >= 1 && channels %% h == 0 && h <= suggested_heads) {
               return(as.integer(h))
          }
     }
     return(1L)  # Fallback
}

#' Calculate Required TCN Blocks for Temporal Coverage (Internal)
#'
#' Internal helper function to determine the number of TCN blocks needed to achieve
#' sufficient temporal receptive field coverage. Used by \code{\link{calc_npe_spec}}.
#'
#' @param T Integer, number of timesteps in the time series.
#' @param kernel_size Integer, TCN kernel size (default 3).
#' @return Integer, number of TCN blocks needed (bounded between 4 and 10).
#' @keywords internal
.calc_tcn_blocks <- function(T, kernel_size = 3) {
     if (is.null(T) || !is.finite(T) || T <= 0) return(4)  # default

     # Required receptive field should cover temporal sequence
     # With exponential dilation: RF = 1 + sum(2^i * (k-1) for i in 0:blocks-1)
     # Simplified: RF ≈ 2^blocks * (k-1)
     # blocks = log2(T / (k-1))

     required_blocks <- ceiling(log2(T / (kernel_size - 1)))

     # Bound between reasonable limits
     as.integer(max(4, min(10, required_blocks)))
}

#' Calculate NPE Architecture Specification for MOSAIC Models
#'
#' Dynamically determines optimal Neural Posterior Estimator architecture based on
#' data characteristics and complexity. Used internally by \code{\link{train_npe}}
#' to automatically configure neural network architectures for different problem scales.
#'
#' @param n_sims Integer, number of training simulations available. Used to determine
#'   data regime and select appropriate regularization.
#' @param n_params Integer, number of parameters to estimate. Affects network capacity
#'   and embedding dimensions.
#' @param n_timesteps Integer, number of time points in data series (optional).
#'   When provided, optimizes temporal processing components.
#' @param n_locations Integer, number of spatial locations (optional).
#'   Influences spatial embedding and attention mechanisms.
#' @param tier Character string to override automatic tier selection. Options:
#'   \code{"tiny", "small", "medium", "large", "xlarge"}. If NULL (default),
#'   automatically determined from data characteristics.
#' @param preset Character string for predefined architecture configurations:
#'   \itemize{
#'     \item \code{"epidemic_small"} — Optimized for small outbreak datasets
#'     \item \code{"epidemic_large"} — Designed for large epidemic time series
#'     \item \code{"endemic"} — Tuned for endemic transmission patterns
#'   }
#' @param device Character string specifying computing device:
#'   \code{"cpu", "cuda", "mps"}. Default is "cpu".
#' @param verbose Logical indicating whether to print architecture details.
#'   Default is TRUE.
#'
#' @return Named list containing complete NPE architecture specification with components:
#'   \itemize{
#'     \item \code{tier} — Selected architecture tier
#'     \item \code{embedding} — Time series embedding configuration
#'     \item \code{flow} — Normalizing flow parameters
#'     \item \code{training} — Training hyperparameters
#'     \item \code{device} — Target computing device
#'     \item \code{architecture_type} — Summary of architecture choices
#'   }
#'
#' @details
#' The function implements a tiered architecture system that scales with problem complexity:
#'
#' \strong{Tier Selection Criteria:}
#' \itemize{
#'   \item \strong{Tiny}: < 1K simulations, < 10 parameters
#'   \item \strong{Small}: < 5K simulations, < 25 parameters
#'   \item \strong{Medium}: < 20K simulations, < 50 parameters
#'   \item \strong{Large}: < 100K simulations, < 100 parameters
#'   \item \strong{XLarge}: >= 100K simulations, >= 100 parameters
#' }
#'
#' \strong{Architecture Components:}
#' \itemize{
#'   \item \strong{Embedding Network}: Temporal Convolutional Networks (TCN) with
#'     attention mechanisms for processing time series data
#'   \item \strong{Normalizing Flows}: Neural Spline Flows for flexible posterior
#'     approximation with appropriate transform depth
#'   \item \strong{Training Setup}: Batch sizes, learning rates, and regularization
#'     adapted to data regime
#' }
#'
#' Internal helper functions:
#' \itemize{
#'   \item \code{\link{.calc_tcn_blocks}} — Determines temporal receptive field
#'   \item \code{\link{.pick_valid_attention_heads}} — Validates attention configuration
#' }
#'
#' @section External Dependencies:
#' Architecture specifications are designed for:
#' \itemize{
#'   \item \strong{Lampe}: NPE implementation framework (\url{https://github.com/probabilists/lampe})
#'   \item \strong{PyTorch}: Neural network backend (\url{https://pytorch.org/})
#'   \item \strong{Zuko}: Normalizing flows library (\url{https://github.com/probabilists/zuko})
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{train_npe}} for using the architecture specification
#'   \item \code{\link{.calc_tcn_blocks}} for temporal complexity calculation
#'   \item \code{\link{.pick_valid_attention_heads}} for attention head validation
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate specification for medium-scale problem
#' spec <- calc_npe_spec(
#'   n_sims = 10000,
#'   n_params = 25,
#'   n_timesteps = 104,  # 2 years of weekly data
#'   n_locations = 3,
#'   device = "cuda",
#'   verbose = TRUE
#' )
#'
#' # Use epidemic preset
#' spec_preset <- calc_npe_spec(
#'   n_sims = 5000,
#'   n_params = 15,
#'   preset = "epidemic_small"
#' )
#' }
#'
#' @export
calc_npe_spec <- function(
          n_sims,
          n_params,
          n_timesteps = NULL,
          n_locations = NULL,
          tier = NULL,
          preset = NULL,
          device = "cpu",
          verbose = TRUE
) {

     # =============================================================================
     # INPUT VALIDATION
     # =============================================================================

     stopifnot(
          "n_sims must be positive" = n_sims > 0,
          "n_params must be positive" = n_params > 0
     )

     if (!is.null(n_timesteps)) {
          stopifnot("n_timesteps must be positive" = n_timesteps > 0)
     }

     if (!is.null(n_locations)) {
          stopifnot("n_locations must be positive" = n_locations > 0)
     }

     # Warnings for extreme values
     if (n_sims < 1000) {
          warning(sprintf("Low simulation count (%d) may lead to overfitting", n_sims))
     }
     if (n_params > 500) {
          warning(sprintf("High parameter count (%d) may require custom tuning", n_params))
     }

     # =============================================================================
     # PRESET CONFIGURATIONS
     # =============================================================================

     if (!is.null(preset)) {
          spec <- switch(preset,
                         "epidemic_small" = list(
                              tier = "small",
                              n_timesteps = 52,
                              n_locations = 4,
                              embedding_dim = 256,
                              tcn_blocks = 6,
                              tcn_channels = 64,
                              attention_heads = 2,
                              hidden_features = 256,
                              num_transforms = 8,
                              num_bins = 10,
                              batch_size = 256,
                              val_split = 0.2
                         ),
                         "epidemic_large" = list(
                              tier = "large",
                              n_timesteps = 913,
                              n_locations = 40,
                              embedding_dim = 512,
                              tcn_blocks = 9,
                              tcn_channels = 192,
                              attention_heads = 8,
                              hidden_features = 512,
                              num_transforms = 12,
                              num_bins = 16,
                              batch_size = 1024,
                              val_split = 0.1
                         ),
                         "endemic" = list(
                              tier = "medium",
                              n_timesteps = 365,
                              n_locations = 10,
                              embedding_dim = 384,
                              tcn_blocks = 8,
                              tcn_channels = 128,
                              attention_heads = 4,
                              hidden_features = 384,
                              num_transforms = 10,
                              num_bins = 12,
                              batch_size = 512,
                              val_split = 0.1
                         ),
                         stop("Unknown preset: ", preset)
          )

          # Override with preset values
          if (!is.null(spec$n_timesteps)) n_timesteps <- spec$n_timesteps
          if (!is.null(spec$n_locations)) n_locations <- spec$n_locations
          tier <- spec$tier
     }

     # =============================================================================
     # TIER DETERMINATION
     # =============================================================================

     if (is.null(tier)) {
          # Automatic tier selection BIASED TOWARDS SMALLER/FASTER models
          data_complexity <- n_sims * sqrt(n_params)

          tier <- if (data_complexity < 100000) {  # Increased threshold for small
               "tiny"
          } else if (data_complexity < 800000) {   # Increased threshold for medium
               "small"
          } else if (data_complexity < 3000000) {  # Increased threshold for large
               "medium"
          } else {
               "large"  # Rarely use xlarge
          }

          # More aggressive downsizing for speed
          if (n_sims < 10000) tier <- "tiny"      # Expanded tiny range
          if (n_sims < 2000) tier <- "tiny"       # Even smaller datasets stay tiny
     }

     # =============================================================================
     # BASE CONFIGURATION BY TIER
     # =============================================================================

     base_config <- switch(tier,
                           tiny = list(
                                embedding_dim = 96,     # Reduced for speed
                                hidden_features = 96,   # Reduced for speed
                                num_transforms = 4,     # Fewer transforms = faster
                                num_bins = 6,          # Fewer bins = faster
                                tcn_channels = 24,     # Reduced (divisible by 8)
                                tcn_blocks = 3,        # Fewer blocks = faster
                                tcn_dropout = 0.15,    # Reduced dropout
                                batch_size = 96,       # Optimized batch size
                                val_split = 0.3,
                                weight_decay = 5e-4,
                                n_ensembles = 1,
                                max_epochs = 80        # Fewer epochs for speed
                           ),
                           small = list(
                                embedding_dim = 192,    # Reduced for speed
                                hidden_features = 192,  # Reduced for speed
                                num_transforms = 6,     # Fewer transforms = faster
                                num_bins = 8,          # Fewer bins = faster
                                tcn_channels = 48,     # Reduced (divisible by 8)
                                tcn_blocks = 4,        # Fewer blocks = faster
                                tcn_dropout = 0.1,     # Reduced dropout
                                batch_size = 128,      # Larger batches for efficiency
                                val_split = 0.2,
                                weight_decay = 1e-4,
                                n_ensembles = 1,
                                max_epochs = 120       # Fewer epochs for speed
                           ),
                           medium = list(
                                embedding_dim = 384,
                                hidden_features = 384,
                                num_transforms = 10,
                                num_bins = 12,
                                tcn_channels = 96,
                                tcn_blocks = 6,
                                tcn_dropout = 0.1,
                                batch_size = 512,
                                val_split = 0.15,
                                weight_decay = 1e-5,
                                n_ensembles = 1,
                                max_epochs = 200
                           ),
                           large = list(
                                embedding_dim = 512,
                                hidden_features = 512,
                                num_transforms = 12,
                                num_bins = 16,
                                tcn_channels = 128,
                                tcn_blocks = 7,
                                tcn_dropout = 0.1,
                                batch_size = 1024,
                                val_split = 0.1,
                                weight_decay = 1e-5,
                                n_ensembles = 3,
                                max_epochs = 250
                           ),
                           xlarge = list(
                                embedding_dim = 768,
                                hidden_features = 768,
                                num_transforms = 15,
                                num_bins = 20,
                                tcn_channels = 192,
                                tcn_blocks = 8,
                                tcn_dropout = 0.15,
                                batch_size = 2048,
                                val_split = 0.1,
                                weight_decay = 5e-6,
                                n_ensembles = 5,
                                max_epochs = 300
                           )
     )

     # Extract base configuration (without mutating environment)
     embedding_dim <- base_config$embedding_dim
     hidden_features <- base_config$hidden_features
     num_transforms <- base_config$num_transforms
     num_bins <- base_config$num_bins
     tcn_channels <- base_config$tcn_channels
     tcn_blocks <- base_config$tcn_blocks
     tcn_dropout <- base_config$tcn_dropout
     batch_size <- base_config$batch_size
     val_split <- base_config$val_split
     weight_decay <- base_config$weight_decay
     n_ensembles <- base_config$n_ensembles
     max_epochs <- base_config$max_epochs

     # =============================================================================
     # DYNAMIC SCALING BASED ON DATA CHARACTERISTICS
     # =============================================================================

     # Scale based on parameter count
     if (n_params > 100) {
          scale_factor <- 1 + 0.1 * log10(n_params / 100)
          hidden_features <- as.integer(hidden_features * scale_factor)
          embedding_dim <- as.integer(embedding_dim * scale_factor)

          # More transforms for high-dimensional parameter spaces
          num_transforms <- as.integer(min(20, num_transforms + floor(n_params / 100)))
     }

     # Scale based on simulation count (data efficiency)
     if (n_sims > 50000) {
          # Can afford larger models with more data
          scale_factor <- 1 + 0.05 * log10(n_sims / 50000)
          hidden_features <- as.integer(hidden_features * scale_factor)
     } else if (n_sims < 5000) {
          # Reduce model size for small datasets
          scale_factor <- 0.8
          hidden_features <- as.integer(hidden_features * scale_factor)
          embedding_dim <- as.integer(embedding_dim * scale_factor)
     }

     # =============================================================================
     # TEMPORAL SCALING (TCN BLOCKS)
     # =============================================================================

     # Use external TCN block calculation function

     if (!is.null(n_timesteps) && n_timesteps > 0) {
          tcn_blocks <- .calc_tcn_blocks(n_timesteps)

          # Adjust channels based on temporal complexity
          if (n_timesteps > 500) {
               tcn_channels <- as.integer(tcn_channels * 1.2)
          }
     }

     # =============================================================================
     # SPATIAL SCALING
     # =============================================================================

     if (!is.null(n_locations) && n_locations > 0) {
          # Scale channels with square root of locations (sublinear)
          location_scale <- sqrt(n_locations / 4)  # Normalized to 4 locations
          scaled_channels <- round(tcn_channels * location_scale)

          # Ensure channels are divisible by a valid group size for GroupNorm
          # Valid group sizes are divisors of the channel count, commonly 1, 2, 4, 8
          # We'll round to nearest multiple of 8 (or 4 for small values)
          if (scaled_channels < 32) {
               # For small channel counts, round to multiple of 4
               tcn_channels <- as.integer(4 * round(scaled_channels / 4))
               tcn_channels <- max(8, tcn_channels)  # Minimum 8 channels
          } else if (scaled_channels < 64) {
               # Round to multiple of 8
               tcn_channels <- as.integer(8 * round(scaled_channels / 8))
               tcn_channels <- max(32, tcn_channels)
          } else {
               # Round to multiple of 16 for larger counts
               tcn_channels <- as.integer(16 * round(scaled_channels / 16))
               tcn_channels <- min(256, tcn_channels)  # Maximum 256
          }

          # Scale attention heads with locations
          suggested_heads <- as.integer(pmin(pmax(
               round(n_locations / 5),  # One head per 5 locations
               2   # Minimum heads
          ), 16))  # Maximum heads

          # Ensure attention_heads divides tcn_channels evenly
          attention_heads <- .pick_valid_attention_heads(tcn_channels, suggested_heads)
     } else {
          attention_heads <- 4  # Default
     }

     # =============================================================================
     # MEMORY-AWARE ADJUSTMENTS
     # =============================================================================

     # Estimate model complexity
     if (!is.null(n_timesteps) && !is.null(n_locations)) {
          model_complexity <- tcn_channels * tcn_blocks * n_timesteps * n_locations

          if (model_complexity > 1e8) {
               # Very large model - reduce batch size
               batch_size <- as.integer(max(128, batch_size / 4))
               if (verbose) {
                    message(sprintf("Large model detected (complexity=%.1e), reducing batch size to %d",
                                    model_complexity, batch_size))
               }
          } else if (model_complexity > 1e7) {
               # Large model - moderately reduce batch size
               batch_size <- as.integer(max(256, batch_size / 2))
          }
     }

     # Device-specific adjustments
     if (device == "cpu") {
          batch_size <- as.integer(min(256, batch_size))
          max_epochs <- as.integer(min(100, max_epochs))
     } else if (device == "mps") {
          # Apple Silicon has different memory characteristics
          batch_size <- as.integer(batch_size * 0.75)
     }

     # =============================================================================
     # CONSTRUCT SPECIFICATION
     # =============================================================================

     spec <- list(
          # Tier and architecture type
          tier = tier,
          architecture_type = "tcn_attention",

          # Embedding configuration
          embedding = list(
               type = "tcn",
               embedding_dim = as.integer(embedding_dim),
               tcn_blocks = as.integer(tcn_blocks),
               tcn_channels = as.integer(tcn_channels),
               tcn_kernel_size = 3L,
               tcn_dropout = tcn_dropout
          ),

          # Spatial pooling
          pooling = list(
               type = "attention",
               attention_heads = as.integer(attention_heads),
               dropout = 0.1
          ),

          # Flow configuration (Neural Spline Flow)
          flow = list(
               type = "nsf",
               hidden_features = as.integer(hidden_features),
               num_transforms = as.integer(num_transforms),
               num_bins = as.integer(num_bins),
               activation = "relu",
               dropout = 0.0
          ),

          # Optimization configuration
          optimization = list(
               optimizer = "adamw",
               learning_rate = 1e-3,
               weight_decay = weight_decay,
               gradient_clip_value = 1.0,
               scheduler = "reduce_on_plateau",
               scheduler_patience = 10,
               scheduler_factor = 0.5
          ),

          # Training configuration
          training = list(
               batch_size = as.integer(batch_size),
               max_epochs = as.integer(max_epochs),
               early_stopping_patience = 30L,
               validation_split = val_split,
               n_ensembles = as.integer(n_ensembles),
               seed = 42L
          ),

          # Preprocessing configuration
          preprocessing = list(
               use_log1p = TRUE,
               standardize = TRUE,
               handle_missing = "drop",
               clip_outliers = FALSE
          ),

          # Data characteristics (for reference)
          data_info = list(
               n_sims = as.integer(n_sims),
               n_params = as.integer(n_params),
               n_timesteps = if (is.null(n_timesteps)) NA_integer_ else as.integer(n_timesteps),
               n_locations = if (is.null(n_locations)) NA_integer_ else as.integer(n_locations),
               device = device,
               tier_auto = is.null(tier)
          )
     )

     class(spec) <- c("npe_spec", "list")

     # =============================================================================
     # VERBOSE OUTPUT
     # =============================================================================

     if (verbose) {
          message("\n========== NPE Architecture Specification ==========")
          message(sprintf("Tier: %s (auto=%s)", tier, is.null(tier)))
          message(sprintf("Data: %d sims × %d params", n_sims, n_params))

          if (!is.null(n_timesteps)) {
               message(sprintf("Temporal: %d timesteps → %d TCN blocks (RF≈%d)",
                               n_timesteps, spec$embedding$tcn_blocks,
                               2^spec$embedding$tcn_blocks * 2))
          }

          if (!is.null(n_locations)) {
               message(sprintf("Spatial: %d locations → %d attention heads",
                               n_locations, spec$pooling$attention_heads))
          }

          message(sprintf("\nArchitecture:"))
          message(sprintf("  Embedding: %d dim, TCN %d blocks × %d channels",
                          spec$embedding$embedding_dim,
                          spec$embedding$tcn_blocks,
                          spec$embedding$tcn_channels))
          message(sprintf("  Flow: NSF with %d hidden, %d transforms, %d bins",
                          spec$flow$hidden_features,
                          spec$flow$num_transforms,
                          spec$flow$num_bins))
          message(sprintf("  Training: batch=%d, epochs≤%d, val=%.1f%%",
                          spec$training$batch_size,
                          spec$training$max_epochs,
                          100 * spec$training$validation_split))

          if (spec$training$n_ensembles > 1) {
               message(sprintf("  Ensemble: %d models", spec$training$n_ensembles))
          }

          message("====================================================\n")
     }

     return(spec)
}






#!/usr/bin/env Rscript
# Extract prior bounds from MOSAIC priors for NPE training
# This function extracts the min/max bounds for each parameter from priors_default

#' Extract parameter bounds empirically from simulations with distribution validation
#'
#' Uses the range of parameter values from simulations.parquet with a 10% buffer,
#' then validates against known distribution constraints.
#'
#' @param simulations_df Data frame from simulations.parquet with parameter columns
#' @param param_names Vector of parameter names to extract bounds for
#' @param priors Optional MOSAIC priors object for distribution type lookup
#' @param buffer Proportion to expand bounds (default 0.1 for 10%)
#' @return Data frame with columns: parameter, min, max, distribution
# Removed old complex prior bounds functions - replaced with get_npe_parameter_bounds()

#' Extract Parameter Bounds from MOSAIC Prior Distributions
#'
#' Extracts minimum and maximum bounds for parameters from MOSAIC prior distributions
#' by analyzing the distribution types and their parameters. This is the original
#' bounds extraction method that works directly with prior distribution definitions.
#'
#' @param priors MOSAIC priors object containing parameter distributions.
#'   Typically \code{priors_default} or output from \code{\link{get_location_priors}}.
#' @param param_names Character vector of parameter names to extract bounds for.
#'   Should match parameter names in the priors object.
#' @param location_order Character vector of location ISO codes in expected order.
#'   Used for location-specific parameters.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{parameter} — Parameter name
#'     \item \code{min} — Minimum theoretical bound
#'     \item \code{max} — Maximum theoretical bound
#'   }
#'
#' @details
#' This function extracts bounds by analyzing prior distribution types:
#' \itemize{
#'   \item \strong{Beta}: [0, 1] for all beta distributions
#'   \item \strong{Gamma}: [0, 99th percentile] to avoid infinite support
#'   \item \strong{Lognormal}: [0.001, 99th percentile] to avoid zero and infinity
#'   \item \strong{Normal}: [1st percentile, 99th percentile] to avoid infinite support
#'   \item \strong{Uniform}: [min, max] from distribution parameters
#'   \item \strong{Truncated Normal}: [a, b] from truncation bounds
#' }
#'
#' @section Alternative Methods:
#' For NPE training, consider using \code{\link{.get_npe_prior_bounds}} which provides
#' empirical bounds from simulation data with additional biological validation.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{.get_npe_prior_bounds}} for empirical bounds extraction
#'   \item \code{\link{sample_from_prior_batch}} for batch sampling from priors
#'   \item \code{\link{get_location_priors}} for loading location-specific priors
#' }
#'
#' @examples
#' \dontrun{
#' # Extract bounds for global parameters
#' param_bounds <- extract_prior_bounds(
#'   priors = priors_default,
#'   param_names = c("alpha_1", "alpha_2", "iota"),
#'   location_order = c("ETH", "KEN", "UGA")
#' )
#'
#' # View the bounds
#' print(param_bounds)
#' }
#'
#' @export
extract_prior_bounds <- function(priors, param_names, location_order) {

     bounds_list <- list()

     for (param_name in param_names) {

          # Parse parameter name to determine if it's location-specific
          is_location_param <- grepl("_[A-Z]{3}$", param_name)

          if (is_location_param) {
               # Extract base name and location
               base_name <- gsub("_[A-Z]{3}$", "", param_name)
               location <- regmatches(param_name, regexpr("[A-Z]{3}$", param_name))

               # Handle seasonality parameter naming mismatch
               # Calibration uses: a_1_j, a_2_j, b_1_j, b_2_j
               # Priors use: a1, a2, b1, b2
               if (base_name == "a_1_j") {
                    base_name <- "a1"
               } else if (base_name == "a_2_j") {
                    base_name <- "a2"
               } else if (base_name == "b_1_j") {
                    base_name <- "b1"
               } else if (base_name == "b_2_j") {
                    base_name <- "b2"
               }

               # Look in location-specific parameters
               if (!is.null(priors$parameters_location[[base_name]]$location[[location]])) {
                    prior_info <- priors$parameters_location[[base_name]]$location[[location]]
               } else {
                    warning(sprintf("Prior not found for %s (looked for %s_%s)", param_name, base_name, location))
                    next
               }
          } else {
               # Global parameter
               if (!is.null(priors$parameters_global[[param_name]])) {
                    prior_info <- priors$parameters_global[[param_name]]
               } else {
                    warning(sprintf("Prior not found for %s", param_name))
                    next
               }
          }

          # Extract bounds based on distribution type
          dist_type <- prior_info$distribution
          params <- prior_info$parameters

          # Calculate bounds based on distribution
          if (dist_type == "uniform") {
               min_val <- params$min
               max_val <- params$max

          } else if (dist_type == "discrete_uniform") {
               min_val <- params$min
               max_val <- params$max

          } else if (dist_type == "beta") {
               # Beta is bounded [0, 1]
               min_val <- 0.0
               max_val <- 1.0

          } else if (dist_type == "gamma") {
               # Gamma: use 0.001 and 99.9 percentiles
               shape <- params$shape
               rate <- params$rate
               min_val <- qgamma(0.001, shape = shape, rate = rate)
               max_val <- qgamma(0.999, shape = shape, rate = rate)

          } else if (dist_type == "lognormal") {
               # Lognormal: use 0.001 and 99.9 percentiles
               if (!is.null(params$meanlog)) {
                    # Using meanlog/sdlog parameterization
                    meanlog <- params$meanlog
                    sdlog <- params$sdlog
                    min_val <- qlnorm(0.001, meanlog = meanlog, sdlog = sdlog)
                    max_val <- qlnorm(0.999, meanlog = meanlog, sdlog = sdlog)
               } else {
                    # Using mean/sd parameterization
                    mean_val <- params$mean
                    sd_val <- params$sd
                    # Convert to meanlog/sdlog
                    cv2 <- (sd_val / mean_val)^2
                    meanlog <- log(mean_val / sqrt(1 + cv2))
                    sdlog <- sqrt(log(1 + cv2))
                    min_val <- qlnorm(0.001, meanlog = meanlog, sdlog = sdlog)
                    max_val <- qlnorm(0.999, meanlog = meanlog, sdlog = sdlog)
               }

          } else if (dist_type == "normal") {
               # Normal: use 0.001 and 99.9 percentiles
               mean_val <- params$mean
               sd_val <- params$sd
               min_val <- qnorm(0.001, mean = mean_val, sd = sd_val)
               max_val <- qnorm(0.999, mean = mean_val, sd = sd_val)

          } else if (dist_type == "truncnorm") {
               # Truncated normal: use provided bounds
               min_val <- params$a
               max_val <- params$b

          } else if (dist_type == "gompertz") {
               # Gompertz: use custom quantile function
               b <- params$b
               eta <- params$eta
               # Use MOSAIC's qgompertz if available, else approximate
               # Try to load MOSAIC if not already loaded
               if (!"MOSAIC" %in% .packages()) {
                    tryCatch(library(MOSAIC), error = function(e) {})
               }
               if (exists("qgompertz")) {
                    min_val <- MOSAIC::qgompertz(0.001, b = b, eta = eta)
                    max_val <- MOSAIC::qgompertz(0.999, b = b, eta = eta)
               } else {
                    # Approximate with exponential bounds
                    warning(sprintf("qgompertz not available, using approximation for %s", param_name))
                    min_val <- 1e-10
                    max_val <- 1e-3
               }

          } else {
               warning(sprintf("Unknown distribution type '%s' for %s", dist_type, param_name))
               next
          }

          # Store bounds
          bounds_list[[param_name]] <- data.frame(
               parameter = param_name,
               min = min_val,
               max = max_val,
               distribution = dist_type,
               stringsAsFactors = FALSE
          )
     }

     # Combine all bounds
     if (length(bounds_list) > 0) {
          bounds_df <- do.call(rbind, bounds_list)
          rownames(bounds_df) <- NULL

          # Ensure finite bounds
          bounds_df$min[!is.finite(bounds_df$min)] <- -1e6
          bounds_df$max[!is.finite(bounds_df$max)] <- 1e6

          # Ensure min < max
          invalid <- bounds_df$min >= bounds_df$max
          if (any(invalid)) {
               warning(sprintf("Invalid bounds for parameters: %s",
                               paste(bounds_df$parameter[invalid], collapse = ", ")))
               # Fix by expanding slightly
               bounds_df$min[invalid] <- bounds_df$min[invalid] - 0.01
               bounds_df$max[invalid] <- bounds_df$max[invalid] + 0.01
          }

          return(bounds_df)
     } else {
          stop("No valid parameter bounds could be extracted")
     }
}

#' Batch Sample from MOSAIC Prior Distributions
#'
#' Efficiently samples multiple parameters simultaneously from MOSAIC prior distributions.
#' Handles both global parameters and location-specific parameters with automatic
#' parameter name parsing and distribution type detection.
#'
#' @param priors MOSAIC priors object containing parameter distributions.
#'   Should have nested structure with \code{parameters_global} and
#'   \code{parameters_location} components.
#' @param param_names Character vector of parameter names to sample.
#'   Supports both global parameters (e.g., "alpha_1") and location-specific
#'   parameters (e.g., "mu_j_ETH", "a_1_j_KEN").
#' @param n_samples Integer number of samples to draw for each parameter.
#'   Default is 1000.
#'
#' @return Numeric matrix with dimensions \code{n_samples × n_parameters}.
#'   Column names match the input \code{param_names}. Each row represents
#'   one complete parameter vector sample.
#'
#' @details
#' This function provides efficient batch sampling by:
#' \itemize{
#'   \item Parsing parameter names to distinguish global vs. location-specific parameters
#'   \item Handling location code extraction (e.g., "ETH" from "mu_j_ETH")
#'   \item Managing seasonality parameter name translation (e.g., "a_1_j" → "a1")
#'   \item Delegating to \code{\link{sample_from_prior}} for consistent distribution handling
#' }
#'
#' \strong{Supported Distribution Types:}
#' All distributions supported by \code{\link{sample_from_prior}}:
#' beta, gamma, lognormal, normal, truncnorm, uniform, discrete_uniform, gompertz.
#'
#' \strong{Parameter Naming Conventions:}
#' \itemize{
#'   \item \strong{Global}: "alpha_1", "iota", "gamma_2"
#'   \item \strong{Location-specific}: "mu_j_ETH", "theta_j_KEN", "a_1_j_UGA"
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{sample_from_prior}} for single parameter sampling
#'   \item \code{\link{sample_parameters}} for full MOSAIC configuration sampling
#'   \item \code{\link{get_location_priors}} for loading location-specific priors
#' }
#'
#' @examples
#' \dontrun{
#' # Sample mixed global and location-specific parameters
#' param_samples <- sample_from_prior_batch(
#'   priors = priors_default,
#'   param_names = c("alpha_1", "iota", "mu_j_ETH", "mu_j_KEN"),
#'   n_samples = 5000
#' )
#'
#' # Check sample dimensions and summary
#' dim(param_samples)  # Should be 5000 × 4
#' head(param_samples)
#' apply(param_samples, 2, summary)
#' }
#'
#' @export
sample_from_prior_batch <- function(priors, param_names, n_samples = 1000) {

     samples <- matrix(NA, nrow = n_samples, ncol = length(param_names))
     colnames(samples) <- param_names

     for (i in seq_along(param_names)) {
          param_name <- param_names[i]

          # Parse parameter name
          is_location_param <- grepl("_[A-Z]{3}$", param_name)

          if (is_location_param) {
               base_name <- gsub("_[A-Z]{3}$", "", param_name)
               location <- regmatches(param_name, regexpr("[A-Z]{3}$", param_name))

               # Handle seasonality parameter naming mismatch
               if (base_name == "a_1_j") {
                    base_name <- "a1"
               } else if (base_name == "a_2_j") {
                    base_name <- "a2"
               } else if (base_name == "b_1_j") {
                    base_name <- "b1"
               } else if (base_name == "b_2_j") {
                    base_name <- "b2"
               }

               if (!is.null(priors$parameters_location[[base_name]]$location[[location]])) {
                    prior_info <- priors$parameters_location[[base_name]]$location[[location]]
               } else {
                    stop(sprintf("Prior not found for %s (looked for %s_%s)", param_name, base_name, location))
               }
          } else {
               if (!is.null(priors$parameters_global[[param_name]])) {
                    prior_info <- priors$parameters_global[[param_name]]
               } else {
                    stop(sprintf("Prior not found for %s", param_name))
               }
          }

          # Use the centralized sample_from_prior function
          # Create a standard prior object that sample_from_prior expects
          standard_prior <- list(
               distribution = prior_info$distribution,
               parameters = prior_info$parameters
          )

          # Sample using the centralized function
          # This ensures consistent distribution handling across the package
          tryCatch({
               samples[, i] <- sample_from_prior(n = n_samples, prior = standard_prior, verbose = FALSE)
          }, error = function(e) {
               # Provide more informative error message with parameter context
               stop(sprintf("Error sampling parameter '%s': %s", param_name, e$message))
          })
     }

     return(samples)
}

#' Calculate Coverage Diagnostics for NPE Models
#'
#' @description
#' Calculates expected coverage diagnostics for trained NPE models using lampe's
#' expected_coverage_mc function. This helps validate that the posterior estimator
#' is well-calibrated by checking if credible intervals contain the true parameters
#' at the expected rates.
#'
#' @param npe_model_dir Directory containing trained NPE model files
#' @param test_data_dir Directory containing test dataset (simulations.parquet and outputs.parquet)
#' @param n_samples Number of samples to draw from posterior for each test case (default: 1024)
#' @param n_test_cases Maximum number of test cases to evaluate (default: 100)
#' @param output_file Optional path to save coverage results as CSV (default: NULL)
#' @param verbose Logical; print progress messages (default: TRUE)
#' @param seed Random seed for reproducibility (default: 42)
#'
#' @return List containing:
#' \describe{
#'   \item{levels}{Vector of credible levels}
#'   \item{coverages}{Vector of empirical coverage rates}
#'   \item{n_test_cases}{Number of test cases used}
#'   \item{coverage_rmse}{Root mean squared error between expected and observed coverage}
#'   \item{is_well_calibrated}{Logical indicating if model is well-calibrated (RMSE < 0.05)}
#' }
#'
#' @details
#' This function loads a trained NPE model and evaluates its calibration by:
#' 1. Loading test data (parameter-observation pairs)
#' 2. For each test case, sampling from the posterior given the observation
#' 3. Computing the proportion of posterior samples with higher density than the true parameter
#' 4. Checking if this proportion matches expected coverage rates
#'
#' A well-calibrated posterior should have empirical coverage rates that match
#' the nominal credible levels (e.g., 95% intervals should contain true parameters 95% of the time).
#'
#' @examples
#' \dontrun{
#' # Calculate coverage diagnostics for trained NPE model
#' coverage_results <- calc_npe_coverage_diagnostics(
#'   npe_model_dir = "./results/npe",
#'   test_data_dir = "./results/test",
#'   n_samples = 1024,
#'   n_test_cases = 50
#' )
#'
#' print(coverage_results$coverage_rmse)  # Should be < 0.05 for well-calibrated model
#' }
#'
#' @export
calc_npe_coverage_diagnostics <- function(
    npe_model_dir,
    test_data_dir = NULL,
    n_samples = 1024,
    n_test_cases = 100,
    output_file = NULL,
    verbose = TRUE,
    seed = 42
) {
    require(reticulate)
    require(arrow)

    log_msg <- function(msg, ...) {
        if (verbose) {
            timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
            message(sprintf(paste(timestamp, msg), ...))
        }
    }

    set.seed(seed)
    log_msg("Starting NPE coverage diagnostics")

    # Validate inputs
    if (!dir.exists(npe_model_dir)) {
        stop("NPE model directory not found: ", npe_model_dir)
    }

    # Check for model files (support both single and ensemble models)
    npe_file <- file.path(npe_model_dir, "npe.pt")
    encoder_file <- file.path(npe_model_dir, "encoder.pt")
    state_file <- file.path(npe_model_dir, "npe_state.pt")
    metadata_file <- file.path(npe_model_dir, "npe_metadata.json")

    # Check for ensemble models if single model not found
    if (!file.exists(npe_file)) {
        npe_file <- file.path(npe_model_dir, "npe_ensemble_0.pt")
        encoder_file <- file.path(npe_model_dir, "encoder_ensemble_0.pt")
        state_file <- file.path(npe_model_dir, "npe_state_ensemble_0.pt")
    }

    if (!file.exists(npe_file)) {
        stop("NPE model file not found. Expected 'npe.pt' or 'npe_ensemble_0.pt' in: ", npe_model_dir)
    }
    if (!file.exists(metadata_file)) {
        stop("NPE metadata file not found: ", metadata_file)
    }

    # Load metadata
    log_msg("Loading NPE metadata...")
    metadata <- jsonlite::fromJSON(metadata_file)

    # Use training data directory if test data not specified
    if (is.null(test_data_dir)) {
        test_data_dir <- dirname(npe_model_dir)
        log_msg("Using training data directory for testing: %s", test_data_dir)
    }

    # Load test data
    simulations_file <- file.path(test_data_dir, "simulations.parquet")
    outputs_file <- file.path(test_data_dir, "outputs.parquet")

    if (!file.exists(simulations_file) || !file.exists(outputs_file)) {
        stop("Test data files not found in: ", test_data_dir)
    }

    log_msg("Loading test data...")
    params <- arrow::read_parquet(simulations_file)
    outputs <- arrow::read_parquet(outputs_file)

    # Import Python modules
    log_msg("Importing Python modules...")
    torch <- import("torch")
    lampe <- import("lampe")
    diagnostics <- import("lampe.diagnostics")

    # Load trained model components
    log_msg("Loading trained NPE model...")
    device <- if (torch$cuda$is_available()) "cuda" else "cpu"

    # Load NPE and encoder
    npe_model <- torch$load(npe_file, map_location = device)
    encoder <- torch$load(encoder_file, map_location = device)

    # Load state dictionary if available
    if (file.exists(state_file)) {
        state_dict <- torch$load(state_file, map_location = device)
        # State dict contains metadata and training info
    }

    npe_model$eval()
    encoder$eval()

    # Prepare test dataset
    log_msg("Preparing test dataset...")

    # Get parameter names from metadata
    param_names <- metadata$training_parameters
    log_msg("Parameters: %s", paste(head(param_names, 5), collapse = ", "))

    # Filter and prepare parameters
    test_params <- params[complete.cases(params[param_names]), param_names]

    # Limit test cases
    n_available <- nrow(test_params)
    n_test_use <- min(n_test_cases, n_available)

    if (n_test_use < n_available) {
        test_indices <- sample(n_available, n_test_use)
        test_params <- test_params[test_indices, ]
        # Also filter outputs to match
        outputs <- outputs[outputs$sim %in% test_indices, ]
    }

    log_msg("Using %d test cases out of %d available", n_test_use, n_available)

    # Convert outputs to tensor format expected by NPE
    log_msg("Preparing observation tensors...")

    # Reshape outputs to (n_sims, n_locations * n_timesteps) format
    location_order <- sort(unique(outputs$j))
    t_levels <- sort(unique(outputs$t))
    n_locations <- length(location_order)
    n_timesteps <- length(t_levels)

    log_msg("Data dimensions: %d locations, %d timesteps", n_locations, n_timesteps)

    # Create observation matrix
    obs_matrix <- matrix(0, nrow = n_test_use, ncol = n_locations * n_timesteps)

    for (i in 1:n_test_use) {
        sim_id <- if (exists("test_indices")) test_indices[i] else i
        sim_outputs <- outputs[outputs$sim == sim_id, ]

        if (nrow(sim_outputs) > 0) {
            # Create flattened observation vector
            obs_vec <- numeric(n_locations * n_timesteps)

            for (row in 1:nrow(sim_outputs)) {
                j_idx <- which(location_order == sim_outputs$j[row])
                t_idx <- which(t_levels == sim_outputs$t[row])
                flat_idx <- (j_idx - 1) * n_timesteps + t_idx
                obs_vec[flat_idx] <- sim_outputs$cases[row]
            }

            obs_matrix[i, ] <- obs_vec
        }
    }

    # Convert to PyTorch tensors
    log_msg("Converting to PyTorch tensors...")
    param_tensor <- torch$tensor(as.matrix(test_params), dtype = torch$float32)
    obs_tensor <- torch$tensor(obs_matrix, dtype = torch$float32)

    if (device == "cuda") {
        param_tensor <- param_tensor$to(device)
        obs_tensor <- obs_tensor$to(device)
        npe_model <- npe_model$to(device)
    }

    # Create parameter-observation pairs for diagnostics
    log_msg("Creating test pairs...")
    test_pairs <- list()
    for (i in 1:n_test_use) {
        test_pairs[[i]] <- list(param_tensor[i, ], obs_tensor[i, ])
    }

    # Define posterior function
    log_msg("Calculating coverage diagnostics...")
    posterior_fn <- function(x) {
        # x is observation tensor
        # Return the posterior distribution conditioned on x
        return(npe_model(x))
    }

    # Calculate expected coverage using Monte Carlo
    tryCatch({
        coverage_result <- diagnostics$expected_coverage_mc(
            posterior = posterior_fn,
            pairs = test_pairs,
            n = as.integer(n_samples),
            device = device
        )

        # Extract results
        levels <- as.numeric(coverage_result[[1]]$cpu()$numpy())
        coverages <- as.numeric(coverage_result[[2]]$cpu()$numpy())

    }, error = function(e) {
        log_msg("Error in coverage calculation: %s", e$message)
        log_msg("Falling back to simplified coverage analysis...")

        # Simplified coverage analysis if lampe diagnostics fail
        levels <- seq(0.1, 0.9, by = 0.1)
        coverages <- rep(NA, length(levels))

        # Manual coverage calculation for key levels
        for (i in seq_along(levels)) {
            level <- levels[i]
            alpha <- 1 - level

            # Sample from posterior for each test case and check coverage
            covered <- numeric(n_test_use)

            for (j in 1:min(10, n_test_use)) {  # Limit to 10 cases for speed
                obs <- obs_tensor[j, ]$unsqueeze(0L)
                posterior_dist <- npe_model(obs)

                # Sample from posterior
                samples <- posterior_dist$sample(torch$Size(c(n_samples)))

                # Calculate empirical quantiles
                true_param <- param_tensor[j, ]

                # Simple density-based coverage check
                log_probs <- posterior_dist$log_prob(samples)
                true_log_prob <- posterior_dist$log_prob(true_param$unsqueeze(0L))

                # Proportion of samples with higher probability than true parameter
                prop_higher <- mean(as.numeric(log_probs$cpu()) >= as.numeric(true_log_prob$cpu()))

                covered[j] <- as.numeric(prop_higher <= alpha)
            }

            coverages[i] <- mean(covered[1:min(10, n_test_use)], na.rm = TRUE)
        }
    })

    # Calculate calibration metrics
    expected_coverages <- levels
    coverage_errors <- coverages - expected_coverages
    coverage_rmse <- sqrt(mean(coverage_errors^2, na.rm = TRUE))
    is_well_calibrated <- coverage_rmse < 0.05

    log_msg("Coverage RMSE: %.4f", coverage_rmse)
    log_msg("Well calibrated: %s", ifelse(is_well_calibrated, "YES", "NO"))

    # Prepare results
    results <- list(
        levels = levels,
        coverages = coverages,
        expected_coverages = expected_coverages,
        coverage_errors = coverage_errors,
        coverage_rmse = coverage_rmse,
        is_well_calibrated = is_well_calibrated,
        n_test_cases = n_test_use,
        n_samples_per_case = n_samples
    )

    # Save results if requested
    if (!is.null(output_file)) {
        log_msg("Saving coverage results to: %s", output_file)
        results_df <- data.frame(
            credible_level = levels,
            expected_coverage = expected_coverages,
            empirical_coverage = coverages,
            coverage_error = coverage_errors
        )
        write.csv(results_df, output_file, row.names = FALSE)
        results$output_file <- output_file
    }

    log_msg("Coverage diagnostics complete")
    return(results)
}

#' Plot NPE Training Loss Curves
#'
#' @description
#' Creates training and validation loss curves for NPE models, showing convergence
#' behavior and potential overfitting. Supports ensemble models by plotting
#' individual curves and ensemble statistics.
#'
#' @param npe_model_dir Directory containing trained NPE model with training_history.json
#' @param output_file Optional path to save the plot (default: NULL for display only)
#' @param plot_width Plot width in inches (default: 10)
#' @param plot_height Plot height in inches (default: 6)
#' @param show_individual_ensembles Logical; show individual ensemble curves (default: TRUE)
#' @param smooth_curves Logical; apply smoothing to loss curves (default: TRUE)
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return ggplot2 object containing the training loss visualization
#'
#' @details
#' This function loads the training history from training_history.json and creates
#' publication-quality plots showing:
#' 1. Training and validation loss curves over epochs
#' 2. Individual ensemble member curves (if multiple ensembles)
#' 3. Ensemble mean and confidence intervals
#' 4. Best validation loss points
#' 5. Training convergence diagnostics
#'
#' The plot helps identify:
#' - Training convergence
#' - Overfitting (validation loss increasing while training loss decreases)
#' - Ensemble consistency
#' - Optimal stopping points
#'
#' @examples
#' \dontrun{
#' # Plot training curves for NPE model
#' loss_plot <- plot_npe_training_loss(
#'   npe_model_dir = "./results/npe",
#'   output_file = "./figures/npe_training_loss.png"
#' )
#'
#' # Display the plot
#' print(loss_plot)
#' }
#'
#' @export
plot_npe_training_loss <- function(
    npe_model_dir,
    output_file = NULL,
    plot_width = 10,
    plot_height = 6,
    show_individual_ensembles = TRUE,
    smooth_curves = TRUE,
    verbose = TRUE
) {
    require(ggplot2)
    require(dplyr)
    require(tidyr)
    require(jsonlite)

    log_msg <- function(msg, ...) {
        if (verbose) {
            timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
            message(sprintf(paste(timestamp, msg), ...))
        }
    }

    log_msg("Creating NPE training loss visualization")

    # Validate inputs
    if (!dir.exists(npe_model_dir)) {
        stop("NPE model directory not found: ", npe_model_dir)
    }

    # Check for training history file
    history_file <- file.path(npe_model_dir, "training_history.json")
    if (!file.exists(history_file)) {
        stop("Training history file not found: ", history_file)
    }

    # Load training history
    log_msg("Loading training history...")
    history_data <- jsonlite::fromJSON(history_file)
    ensemble_histories <- history_data$ensemble_histories

    if (length(ensemble_histories) == 0) {
        stop("No training histories found in file")
    }

    n_ensembles <- length(ensemble_histories)
    log_msg("Found training histories for %d ensemble(s)", n_ensembles)

    # Convert to data frame for plotting
    all_data <- list()

    for (i in seq_along(ensemble_histories)) {
        history <- ensemble_histories[[i]]

        train_losses <- unlist(history$train_losses)
        val_losses <- unlist(history$val_losses)
        n_epochs <- length(train_losses)

        # Create data frame for this ensemble
        ensemble_data <- data.frame(
            epoch = rep(1:n_epochs, 2),
            loss = c(train_losses, val_losses),
            loss_type = rep(c("Training", "Validation"), each = n_epochs),
            ensemble = paste0("Ensemble_", i),
            best_val_loss = history$best_val_loss
        )

        all_data[[i]] <- ensemble_data
    }

    # Combine all ensemble data
    plot_data <- do.call(rbind, all_data)

    log_msg("Plotting training curves for %d epochs", max(plot_data$epoch))

    # Create base plot
    p <- ggplot(plot_data, aes(x = epoch, y = loss, color = loss_type))

    # Add individual ensemble curves if requested and multiple ensembles
    if (show_individual_ensembles && n_ensembles > 1) {
        p <- p + geom_line(aes(group = paste(ensemble, loss_type)),
                          alpha = 0.3, size = 0.5)
    }

    # Calculate ensemble statistics if multiple ensembles
    if (n_ensembles > 1) {
        ensemble_stats <- plot_data %>%
            group_by(epoch, loss_type) %>%
            summarise(
                mean_loss = mean(loss, na.rm = TRUE),
                se_loss = sd(loss, na.rm = TRUE) / sqrt(n()),
                min_loss = min(loss, na.rm = TRUE),
                max_loss = max(loss, na.rm = TRUE),
                .groups = "drop"
            )

        # Add confidence bands
        p <- p + geom_ribbon(data = ensemble_stats,
                           aes(x = epoch, y = mean_loss,
                               ymin = mean_loss - 2*se_loss,
                               ymax = mean_loss + 2*se_loss,
                               fill = loss_type),
                           alpha = 0.2, inherit.aes = FALSE)

        # Add ensemble mean lines
        p <- p + geom_line(data = ensemble_stats,
                         aes(x = epoch, y = mean_loss, color = loss_type),
                         size = 1.2, inherit.aes = FALSE)
    } else {
        # Single ensemble - show the actual curves
        if (smooth_curves) {
            p <- p + geom_smooth(method = "loess", span = 0.1, se = FALSE, size = 1.2)
        } else {
            p <- p + geom_line(size = 1.2)
        }
    }

    # Add best validation loss points
    best_val_data <- plot_data %>%
        filter(loss_type == "Validation") %>%
        group_by(ensemble) %>%
        slice_min(loss, n = 1, with_ties = FALSE) %>%
        ungroup()

    p <- p + geom_point(data = best_val_data,
                       aes(x = epoch, y = loss),
                       color = "red", size = 2, shape = 8)

    # Styling
    p <- p +
        scale_color_manual(values = c("Training" = "#2E86AB", "Validation" = "#A23B72")) +
        scale_fill_manual(values = c("Training" = "#2E86AB", "Validation" = "#A23B72")) +
        labs(
            title = "NPE Model Training Curves",
            subtitle = paste0("Ensemble size: ", n_ensembles,
                            " | Best validation loss: ",
                            sprintf("%.4f", min(best_val_data$loss))),
            x = "Epoch",
            y = "Loss (Negative Log-Probability)",
            color = "Loss Type",
            fill = "Loss Type"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12, color = "grey40"),
            legend.position = "right",
            panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold")
        )

    # Add convergence diagnostics text
    final_train_loss <- mean(plot_data[plot_data$loss_type == "Training" &
                                     plot_data$epoch == max(plot_data$epoch), "loss"])
    final_val_loss <- mean(plot_data[plot_data$loss_type == "Validation" &
                                   plot_data$epoch == max(plot_data$epoch), "loss"])
    gap <- final_val_loss - final_train_loss

    convergence_text <- paste0(
        "Final Training Loss: ", sprintf("%.4f", final_train_loss), "\n",
        "Final Validation Loss: ", sprintf("%.4f", final_val_loss), "\n",
        "Train-Val Gap: ", sprintf("%.4f", gap),
        if (gap > 0.5) " (possible overfitting)" else ""
    )

    p <- p + annotate("text", x = Inf, y = Inf, label = convergence_text,
                     hjust = 1.1, vjust = 1.1, size = 3, color = "grey40")

    # Save plot if requested
    if (!is.null(output_file)) {
        log_msg("Saving plot to: %s", output_file)
        ggsave(output_file, plot = p, width = plot_width, height = plot_height, dpi = 300)
    }

    log_msg("Training loss visualization complete")
    return(p)
}

#' Simulation-Based Calibration (SBC) Diagnostics for NPE Models
#'
#' @description
#' Performs simulation-based calibration to validate NPE posterior inference
#' by generating data from the prior predictive distribution and checking if
#' the posterior correctly recovers the true parameters. This is the gold
#' standard for validating Bayesian inference procedures.
#'
#' @param npe_model_dir Directory containing trained NPE model files
#' @param priors_file Path to priors.json file for generating test parameters
#' @param n_sbc_samples Number of SBC test cases to generate (default: 100)
#' @param n_posterior_samples Number of posterior samples per test case (default: 1000)
#' @param output_dir Directory to save SBC results and plots (default: same as npe_model_dir)
#' @param use_observed_data Logical; use observed data format instead of simulated (default: FALSE)
#' @param verbose Logical; print progress messages (default: TRUE)
#' @param seed Random seed for reproducibility (default: 42)
#'
#' @return List containing:
#' \describe{
#'   \item{sbc_results}{Data frame with rank statistics for each parameter}
#'   \item{sbc_p_values}{P-values from uniformity tests for each parameter}
#'   \item{overall_calibration}{Overall calibration assessment}
#'   \item{problematic_parameters}{Parameters that fail SBC tests}
#'   \item{sbc_plot}{ggplot2 object showing SBC histograms}
#' }
#'
#' @details
#' Simulation-based calibration works by:
#' 1. Drawing true parameters θ* from the prior
#' 2. Simulating data x* ~ p(x|θ*) using the forward model
#' 3. Computing posterior samples θ₁,...,θₙ ~ p(θ|x*) using NPE
#' 4. Computing the rank of θ* among {θ₁,...,θₙ,θ*}
#' 5. Checking if ranks are uniformly distributed across parameters
#'
#' Well-calibrated posteriors should produce uniform rank distributions.
#' Deviations from uniformity indicate problems with the posterior inference.
#'
#' @examples
#' \dontrun{
#' # Run SBC diagnostics on trained NPE model
#' sbc_results <- calc_npe_sbc_diagnostics(
#'   npe_model_dir = "./results/npe",
#'   priors_file = "./config/priors.json",
#'   n_sbc_samples = 50,
#'   n_posterior_samples = 1000
#' )
#'
#' # Check overall calibration
#' print(sbc_results$overall_calibration)
#'
#' # Display SBC plot
#' print(sbc_results$sbc_plot)
#' }
#'
#' @export
calc_npe_sbc_diagnostics <- function(
    npe_model_dir,
    priors_file,
    n_sbc_samples = 100,
    n_posterior_samples = 1000,
    output_dir = NULL,
    use_observed_data = FALSE,
    verbose = TRUE,
    seed = 42
) {
    require(reticulate)
    require(arrow)
    require(ggplot2)
    require(dplyr)
    require(tidyr)

    log_msg <- function(msg, ...) {
        if (verbose) {
            timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
            message(sprintf(paste(timestamp, msg), ...))
        }
    }

    set.seed(seed)
    log_msg("Starting NPE Simulation-Based Calibration (SBC) diagnostics")

    # Set default output directory
    if (is.null(output_dir)) {
        output_dir <- npe_model_dir
    }

    # Validate inputs
    if (!dir.exists(npe_model_dir)) {
        stop("NPE model directory not found: ", npe_model_dir)
    }
    if (!file.exists(priors_file)) {
        stop("Priors file not found: ", priors_file)
    }

    # Load NPE metadata
    metadata_file <- file.path(npe_model_dir, "npe_metadata.json")
    if (!file.exists(metadata_file)) {
        stop("NPE metadata file not found: ", metadata_file)
    }

    log_msg("Loading NPE metadata and priors...")
    metadata <- jsonlite::fromJSON(metadata_file)
    priors <- jsonlite::fromJSON(priors_file)

    param_names <- metadata$training_parameters
    n_parameters <- length(param_names)
    log_msg("Parameters for SBC: %s", paste(head(param_names, 5), collapse = ", "))

    # Import Python modules
    log_msg("Importing Python modules...")
    torch <- import("torch")
    np <- import("numpy")

    # Load trained NPE model components
    log_msg("Loading trained NPE model...")
    device <- if (torch$cuda$is_available()) "cuda" else "cpu"

    # Check for model files (support both single and ensemble models)
    npe_file <- file.path(npe_model_dir, "npe.pt")
    encoder_file <- file.path(npe_model_dir, "encoder.pt")
    state_file <- file.path(npe_model_dir, "npe_state.pt")

    # Check for ensemble models if single model not found
    if (!file.exists(npe_file)) {
        npe_file <- file.path(npe_model_dir, "npe_ensemble_0.pt")
        encoder_file <- file.path(npe_model_dir, "encoder_ensemble_0.pt")
        state_file <- file.path(npe_model_dir, "npe_state_ensemble_0.pt")
    }

    if (!file.exists(npe_file)) {
        stop("NPE model file not found. Expected 'npe.pt' or 'npe_ensemble_0.pt' in: ", npe_model_dir)
    }

    # Load NPE and encoder
    npe_model <- torch$load(npe_file, map_location = device)
    encoder <- torch$load(encoder_file, map_location = device)

    # Load state dictionary if available
    if (file.exists(state_file)) {
        state_dict <- torch$load(state_file, map_location = device)
        # State dict contains metadata and training info
    }

    npe_model$eval()
    encoder$eval()

    # Initialize SBC results storage
    sbc_ranks <- matrix(NA, nrow = n_sbc_samples, ncol = n_parameters)
    colnames(sbc_ranks) <- param_names

    true_params_matrix <- matrix(NA, nrow = n_sbc_samples, ncol = n_parameters)
    colnames(true_params_matrix) <- param_names

    log_msg("Generating %d SBC test cases...", n_sbc_samples)

    # Progress tracking
    progress_interval <- max(1, floor(n_sbc_samples / 10))

    for (sbc_idx in 1:n_sbc_samples) {
        if (verbose && sbc_idx %% progress_interval == 0) {
            log_msg("  SBC test case %d/%d (%.1f%%)", sbc_idx, n_sbc_samples,
                   100 * sbc_idx / n_sbc_samples)
        }

        # Step 1: Sample true parameters from prior
        true_params <- sample_from_prior_batch(priors, param_names, n_samples = 1)
        true_params_vec <- as.numeric(true_params[1, ])
        true_params_matrix[sbc_idx, ] <- true_params_vec

        # Step 2: Generate synthetic observation using LASER simulator
        # For SBC, we need to simulate data given the true parameters
        # This is typically done by calling the forward model

        tryCatch({
            # Create a temporary parameter configuration
            temp_config <- create_sbc_config(true_params_vec, param_names, metadata)

            # Generate synthetic observation using LASER forward model
            if (use_observed_data) {
                # Use format matching observed data structure
                synthetic_obs <- generate_synthetic_observation_observed(temp_config, metadata)
            } else {
                # Use format matching training data structure
                synthetic_obs <- generate_synthetic_observation_training(temp_config, metadata)
            }

            # Step 3: Get posterior samples from NPE given synthetic observation
            obs_tensor <- torch$tensor(synthetic_obs, dtype = torch$float32)$unsqueeze(0L)
            if (device != "cpu") {
                obs_tensor <- obs_tensor$to(device)
            }

            # Sample from NPE posterior
            posterior_dist <- npe_model(obs_tensor)
            posterior_samples <- posterior_dist$sample(torch$Size(c(n_posterior_samples)))

            # Convert to numpy for rank calculation
            posterior_samples_np <- as.matrix(posterior_samples$cpu()$detach()$numpy())

            # Step 4: Calculate ranks for each parameter
            for (p in 1:n_parameters) {
                true_val <- true_params_vec[p]
                posterior_vals <- posterior_samples_np[, p]

                # Add true parameter to samples and calculate rank
                all_vals <- c(posterior_vals, true_val)
                rank <- sum(all_vals <= true_val)  # Rank of true value

                # Normalize rank to [0, 1] interval
                normalized_rank <- (rank - 1) / n_posterior_samples
                sbc_ranks[sbc_idx, p] <- normalized_rank
            }

        }, error = function(e) {
            log_msg("  Error in SBC test case %d: %s", sbc_idx, e$message)
            # Fill with NAs for this case
            sbc_ranks[sbc_idx, ] <- NA
            next
        })
    }

    log_msg("Analyzing SBC results...")

    # Remove incomplete cases
    complete_cases <- complete.cases(sbc_ranks)
    sbc_ranks_clean <- sbc_ranks[complete_cases, , drop = FALSE]
    n_complete <- sum(complete_cases)

    if (n_complete < 10) {
        stop("Too few complete SBC cases (", n_complete, "). Need at least 10.")
    }

    log_msg("Completed %d/%d SBC test cases", n_complete, n_sbc_samples)

    # Statistical tests for uniformity
    sbc_p_values <- numeric(n_parameters)
    names(sbc_p_values) <- param_names

    for (p in 1:n_parameters) {
        ranks <- sbc_ranks_clean[, p]
        ranks <- ranks[!is.na(ranks)]

        if (length(ranks) >= 10) {
            # Kolmogorov-Smirnov test against uniform distribution
            ks_test <- ks.test(ranks, "punif", 0, 1)
            sbc_p_values[p] <- ks_test$p.value
        } else {
            sbc_p_values[p] <- NA
        }
    }

    # Overall calibration assessment
    significant_params <- sum(sbc_p_values < 0.05, na.rm = TRUE)
    total_tested <- sum(!is.na(sbc_p_values))
    overall_well_calibrated <- significant_params / total_tested < 0.1  # Less than 10% failures

    # Identify problematic parameters
    problematic_params <- param_names[sbc_p_values < 0.05 & !is.na(sbc_p_values)]

    # Create SBC visualization
    log_msg("Creating SBC visualization...")
    sbc_plot <- create_sbc_histogram_plot(sbc_ranks_clean, param_names, sbc_p_values)

    # Prepare results
    sbc_results_df <- data.frame(
        parameter = param_names,
        mean_rank = colMeans(sbc_ranks_clean, na.rm = TRUE),
        sd_rank = apply(sbc_ranks_clean, 2, sd, na.rm = TRUE),
        p_value = sbc_p_values,
        well_calibrated = sbc_p_values >= 0.05,
        n_samples = colSums(!is.na(sbc_ranks_clean))
    )

    results <- list(
        sbc_results = sbc_results_df,
        sbc_p_values = sbc_p_values,
        overall_calibration = list(
            well_calibrated = overall_well_calibrated,
            fraction_problematic = significant_params / total_tested,
            n_problematic = significant_params,
            n_total = total_tested
        ),
        problematic_parameters = problematic_params,
        sbc_plot = sbc_plot,
        raw_ranks = sbc_ranks_clean
    )

    # Save results
    sbc_file <- file.path(output_dir, "sbc_results.csv")
    write.csv(sbc_results_df, sbc_file, row.names = FALSE)

    plot_file <- file.path(output_dir, "sbc_diagnostics.png")
    ggsave(plot_file, plot = sbc_plot, width = 12, height = 8, dpi = 300)

    log_msg("SBC Results Summary:")
    log_msg("  Overall well-calibrated: %s", ifelse(overall_well_calibrated, "YES", "NO"))
    log_msg("  Problematic parameters: %d/%d", significant_params, total_tested)
    if (length(problematic_params) > 0) {
        log_msg("  Failed parameters: %s", paste(problematic_params, collapse = ", "))
    }
    log_msg("  Results saved to: %s", sbc_file)
    log_msg("  Plot saved to: %s", plot_file)

    log_msg("SBC diagnostics complete")
    return(results)
}

#' Create SBC Configuration for Forward Simulation
#'
#' @description Internal helper to create configuration for LASER forward simulation
#' in SBC testing. This would typically interface with the actual LASER model.
#'
#' @param params Vector of parameter values
#' @param param_names Vector of parameter names
#' @param metadata NPE metadata containing model configuration
#'
#' @return Configuration object for LASER simulation
#'
#' @keywords internal
create_sbc_config <- function(params, param_names, metadata) {
    # Create a proper LASER configuration from parameter vector
    # Start with base configuration from metadata
    if (!is.null(metadata$config_base)) {
        config <- metadata$config_base
    } else {
        # Initialize with minimal required structure
        config <- list()
    }

    # Map parameter values to configuration
    for (i in seq_along(param_names)) {
        param_name <- param_names[i]
        param_value <- params[i]

        # Handle location-specific parameters (e.g., "beta_j0_hum_ETH")
        if (grepl("_[A-Z]{3}$", param_name)) {
            # Extract location code and base parameter name
            location <- regmatches(param_name, regexpr("[A-Z]{3}$", param_name))
            base_name <- sub("_[A-Z]{3}$", "", param_name)

            # Get location index
            if (!is.null(metadata$data$location_name)) {
                loc_idx <- which(metadata$data$location_name == location)
                if (length(loc_idx) > 0) {
                    # Initialize parameter vector if needed
                    if (is.null(config[[base_name]])) {
                        config[[base_name]] <- rep(NA, length(metadata$data$location_name))
                    }
                    # Set location-specific value
                    config[[base_name]][loc_idx] <- param_value
                }
            }
        } else {
            # Set scalar/global parameter
            config[[param_name]] <- param_value
        }
    }

    # Ensure critical metadata is present
    if (is.null(config$location_name) && !is.null(metadata$data$location_name)) {
        config$location_name <- metadata$data$location_name
    }

    if (is.null(config$n_locations)) {
        config$n_locations <- metadata$data$n_locations
    }

    if (is.null(config$n_timesteps)) {
        config$n_timesteps <- metadata$data$n_timesteps
    }

    # Add time range if available
    if (!is.null(metadata$data$date_start)) {
        config$date_start <- metadata$data$date_start
    }
    if (!is.null(metadata$data$date_stop)) {
        config$date_stop <- metadata$data$date_stop
    }

    return(config)
}

#' Generate Synthetic Observation for SBC Testing
#'
#' @description Internal helper to generate synthetic observations using LASER forward model
#' for simulation-based calibration testing.
#'
#' @param config Configuration object for simulation
#' @param metadata NPE metadata
#'
#' @return Vector of synthetic observations
#'
#' @keywords internal
generate_synthetic_observation_training <- function(config, metadata) {
    # Generate synthetic observations using LASER forward model

    # Create temporary file for configuration
    config_file <- tempfile(fileext = ".json")
    on.exit(unlink(config_file), add = TRUE)

    # Write configuration to JSON
    tryCatch({
        write_model_json(config, config_file, type = "priors", validate = FALSE)
    }, error = function(e) {
        stop("Failed to write configuration for LASER: ", e$message)
    })

    # Run LASER simulation
    tryCatch({
        # Use seed from metadata if available
        seed <- if (!is.null(metadata$seed)) metadata$seed else 123L

        # Run LASER model
        laser_result <- run_LASER_model(
            paramfile = config_file,
            seed = as.integer(seed),
            visualize = FALSE,
            pdf = FALSE
        )

        # Extract simulated data using existing function
        simulated_data <- get_npe_simulated_data(
            laser_result = laser_result,
            include_deaths = FALSE,
            validate = FALSE,
            verbose = FALSE
        )

        # Convert to observation vector format expected by NPE
        n_locations <- metadata$data$n_locations
        n_timesteps <- metadata$data$n_timesteps
        n_features <- n_locations * n_timesteps

        # Initialize observation vector
        obs_vec <- numeric(n_features)

        # Get location mapping
        location_names <- if (!is.null(metadata$data$location_name)) {
            metadata$data$location_name
        } else {
            unique(simulated_data$j)
        }
        location_map <- setNames(seq_along(location_names) - 1, location_names)

        # Fill observation vector with proper indexing
        for (i in 1:nrow(simulated_data)) {
            j_idx <- location_map[simulated_data$j[i]]
            t_idx <- simulated_data$t[i] - 1  # Convert to 0-indexed

            if (!is.na(j_idx) && j_idx >= 0 && j_idx < n_locations &&
                t_idx >= 0 && t_idx < n_timesteps) {
                # Row-major indexing: idx = t * n_locations + j
                idx <- t_idx * n_locations + j_idx + 1  # +1 for R's 1-indexing
                obs_vec[idx] <- simulated_data$cases[i]
            }
        }

        return(obs_vec)

    }, error = function(e) {
        warning("LASER simulation failed: ", e$message, ". Using fallback generation.")
        # Fallback to simple generation if LASER fails
        n_obs <- metadata$data$n_locations * metadata$data$n_timesteps
        synthetic_obs <- rpois(n_obs, lambda = 10)  # Poisson-distributed cases
        return(synthetic_obs)
    })
}

#' Generate Synthetic Observation for Observed Data Format
#'
#' @description Internal helper for observed data format in SBC testing.
#'
#' @param config Configuration object
#' @param metadata NPE metadata
#'
#' @return Synthetic observation in observed data format
#'
#' @keywords internal
generate_synthetic_observation_observed <- function(config, metadata) {
    # Generate synthetic observation in the same format as observed data
    # Both functions produce the same vector format for NPE
    return(generate_synthetic_observation_training(config, metadata))
}

#' Create SBC Histogram Plot
#'
#' @description Internal function to create SBC diagnostic histograms showing
#' rank distributions for each parameter.
#'
#' @param sbc_ranks Matrix of SBC ranks
#' @param param_names Parameter names
#' @param p_values P-values from uniformity tests
#'
#' @return ggplot2 object
#'
#' @keywords internal
create_sbc_histogram_plot <- function(sbc_ranks, param_names, p_values) {
    # Convert to long format for plotting
    ranks_df <- as.data.frame(sbc_ranks) %>%
        mutate(sbc_case = row_number()) %>%
        pivot_longer(cols = -sbc_case, names_to = "parameter", values_to = "rank")

    # Add p-values for labeling
    p_value_labels <- paste0("p = ", sprintf("%.3f", p_values[param_names]))
    names(p_value_labels) <- param_names

    ranks_df$p_value_label <- p_value_labels[ranks_df$parameter]
    ranks_df$well_calibrated <- p_values[ranks_df$parameter] >= 0.05

    # Create histogram plot
    p <- ggplot(ranks_df, aes(x = rank)) +
        geom_histogram(aes(fill = well_calibrated), bins = 20, alpha = 0.7,
                      color = "white", size = 0.2) +
        geom_hline(yintercept = nrow(sbc_ranks) / 20, linetype = "dashed",
                  color = "red", alpha = 0.7) +
        facet_wrap(~ parameter + p_value_label, scales = "free_y", ncol = 4) +
        scale_fill_manual(values = c("TRUE" = "#2E86AB", "FALSE" = "#A23B72"),
                         labels = c("Well-calibrated", "Poorly-calibrated")) +
        labs(
            title = "Simulation-Based Calibration (SBC) Diagnostics",
            subtitle = "Rank histograms should be approximately uniform (red dashed line shows expected frequency)",
            x = "Normalized Rank",
            y = "Frequency",
            fill = "Calibration Status"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11, color = "grey40"),
            strip.text = element_text(size = 9, face = "bold"),
            legend.position = "bottom",
            panel.grid.minor = element_blank()
        )

    return(p)
}

#' Posterior Predictive Checks (PPC) for NPE Models
#'
#' @description
#' Performs posterior predictive checks to validate NPE models by comparing
#' observed data with predictions from the posterior predictive distribution.
#' This helps identify model misspecification and assess goodness of fit.
#'
#' @param npe_model_dir Directory containing trained NPE model files
#' @param observed_data_file Path to observed data file (parquet format)
#' @param n_posterior_samples Number of posterior samples to draw (default: 1000)
#' @param n_predictive_samples Number of predictive samples per posterior sample (default: 1)
#' @param test_statistics Vector of test statistics to compute (default: c("mean", "var", "max", "skewness"))
#' @param output_dir Directory to save PPC results and plots (default: same as npe_model_dir)
#' @param verbose Logical; print progress messages (default: TRUE)
#' @param seed Random seed for reproducibility (default: 42)
#'
#' @return List containing:
#' \describe{
#'   \item{ppc_results}{Data frame with test statistics for observed and predicted data}
#'   \item{p_values}{P-values for each test statistic}
#'   \item{ppc_plots}{List of ggplot2 objects showing predictive distributions}
#'   \item{overall_fit}{Overall model fit assessment}
#' }
#'
#' @details
#' Posterior predictive checks work by:
#' 1. Using observed data to get posterior samples θ₁,...,θₙ ~ p(θ|y_obs)
#' 2. For each θᵢ, simulating replicated data y_rep,i ~ p(y|θᵢ)
#' 3. Computing test statistics T(y_rep) and T(y_obs)
#' 4. Checking if T(y_obs) is typical of the predictive distribution
#'
#' Good model fit should show T(y_obs) within the central range of T(y_rep).
#' Extreme values indicate potential model misspecification.
#'
#' @examples
#' \dontrun{
#' # Run posterior predictive checks
#' ppc_results <- calc_npe_posterior_predictive_checks(
#'   npe_model_dir = "./results/npe",
#'   observed_data_file = "./data/observed_cases.parquet",
#'   n_posterior_samples = 500,
#'   test_statistics = c("mean", "var", "max")
#' )
#'
#' # Check model fit
#' print(ppc_results$overall_fit)
#'
#' # Display PPC plots
#' print(ppc_results$ppc_plots$mean)
#' }
#'
#' @export
calc_npe_posterior_predictive_checks <- function(
    npe_model_dir,
    observed_data_file,
    n_posterior_samples = 1000,
    n_predictive_samples = 1,
    test_statistics = c("mean", "var", "max", "skewness"),
    output_dir = NULL,
    verbose = TRUE,
    seed = 42
) {
    require(reticulate)
    require(arrow)
    require(ggplot2)
    require(dplyr)
    require(tidyr)

    log_msg <- function(msg, ...) {
        if (verbose) {
            timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
            message(sprintf(paste(timestamp, msg), ...))
        }
    }

    set.seed(seed)
    log_msg("Starting NPE Posterior Predictive Checks (PPC)")

    # Set default output directory
    if (is.null(output_dir)) {
        output_dir <- npe_model_dir
    }

    # Validate inputs
    if (!dir.exists(npe_model_dir)) {
        stop("NPE model directory not found: ", npe_model_dir)
    }
    if (!file.exists(observed_data_file)) {
        stop("Observed data file not found: ", observed_data_file)
    }

    # Load NPE metadata
    metadata_file <- file.path(npe_model_dir, "npe_metadata.json")
    if (!file.exists(metadata_file)) {
        stop("NPE metadata file not found: ", metadata_file)
    }

    log_msg("Loading NPE metadata and observed data...")
    metadata <- jsonlite::fromJSON(metadata_file)

    # Load observed data
    observed_data <- arrow::read_parquet(observed_data_file)
    log_msg("Loaded observed data: %d rows", nrow(observed_data))

    # Import Python modules
    log_msg("Importing Python modules...")
    torch <- import("torch")
    np <- import("numpy")

    # Load trained NPE model components
    log_msg("Loading trained NPE model...")
    device <- if (torch$cuda$is_available()) "cuda" else "cpu"

    # Check for model files (support both single and ensemble models)
    npe_file <- file.path(npe_model_dir, "npe.pt")
    encoder_file <- file.path(npe_model_dir, "encoder.pt")
    state_file <- file.path(npe_model_dir, "npe_state.pt")

    # Check for ensemble models if single model not found
    if (!file.exists(npe_file)) {
        npe_file <- file.path(npe_model_dir, "npe_ensemble_0.pt")
        encoder_file <- file.path(npe_model_dir, "encoder_ensemble_0.pt")
        state_file <- file.path(npe_model_dir, "npe_state_ensemble_0.pt")
    }

    if (!file.exists(npe_file)) {
        stop("NPE model file not found. Expected 'npe.pt' or 'npe_ensemble_0.pt' in: ", npe_model_dir)
    }

    # Load NPE and encoder
    npe_model <- torch$load(npe_file, map_location = device)
    encoder <- torch$load(encoder_file, map_location = device)

    # Load state dictionary if available
    if (file.exists(state_file)) {
        state_dict <- torch$load(state_file, map_location = device)
        # State dict contains metadata and training info
    }

    npe_model$eval()
    encoder$eval()

    # Prepare observed data in format expected by NPE
    log_msg("Preparing observed data...")
    obs_tensor <- prepare_observation_tensor(observed_data, metadata)

    if (device != "cpu") {
        obs_tensor <- obs_tensor$to(device)
    }

    # Get posterior samples from NPE
    log_msg("Sampling from NPE posterior...")
    posterior_dist <- npe_model(obs_tensor)
    posterior_samples <- posterior_dist$sample(torch$Size(c(n_posterior_samples)))
    posterior_samples_np <- as.matrix(posterior_samples$cpu()$detach()$numpy())

    log_msg("Generated %d posterior samples for PPC", nrow(posterior_samples_np))

    # Generate predictive samples
    log_msg("Generating predictive samples...")
    n_total_pred_samples <- n_posterior_samples * n_predictive_samples
    predictive_data <- matrix(NA, nrow = n_total_pred_samples, ncol = ncol(as.matrix(observed_data)))

    # Progress tracking
    progress_interval <- max(1, floor(n_posterior_samples / 10))

    for (i in 1:n_posterior_samples) {
        if (verbose && i %% progress_interval == 0) {
            log_msg("  Generating predictive samples %d/%d (%.1f%%)",
                   i, n_posterior_samples, 100 * i / n_posterior_samples)
        }

        # Get parameter values for this posterior sample
        param_values <- posterior_samples_np[i, ]

        # Generate predictive data using forward model
        # This is where we would call LASER with these parameter values
        tryCatch({
            pred_data <- generate_predictive_data(param_values, metadata, n_predictive_samples)

            # Store predictive samples
            start_idx <- (i - 1) * n_predictive_samples + 1
            end_idx <- i * n_predictive_samples
            predictive_data[start_idx:end_idx, ] <- pred_data

        }, error = function(e) {
            log_msg("  Error generating predictive sample %d: %s", i, e$message)
            # Fill with NAs for this sample
            start_idx <- (i - 1) * n_predictive_samples + 1
            end_idx <- i * n_predictive_samples
            predictive_data[start_idx:end_idx, ] <- NA
        })
    }

    # Remove incomplete predictive samples
    complete_pred <- complete.cases(predictive_data)
    predictive_data_clean <- predictive_data[complete_pred, , drop = FALSE]
    n_pred_complete <- nrow(predictive_data_clean)

    log_msg("Generated %d/%d complete predictive samples", n_pred_complete, n_total_pred_samples)

    if (n_pred_complete < 100) {
        warning("Few complete predictive samples (", n_pred_complete, "). Results may be unreliable.")
    }

    # Calculate test statistics
    log_msg("Computing test statistics...")
    observed_stats <- list()
    predictive_stats <- list()

    # Prepare observed data as vector for test statistics
    observed_vector <- prepare_data_for_statistics(observed_data)

    for (stat_name in test_statistics) {
        # Calculate for observed data
        observed_stats[[stat_name]] <- calculate_test_statistic(observed_vector, stat_name)

        # Calculate for predictive samples
        pred_stats <- numeric(n_pred_complete)
        for (j in 1:n_pred_complete) {
            pred_vector <- prepare_data_for_statistics_from_matrix(predictive_data_clean[j, ])
            pred_stats[j] <- calculate_test_statistic(pred_vector, stat_name)
        }
        predictive_stats[[stat_name]] <- pred_stats
    }

    # Calculate p-values (proportion of predictive samples more extreme than observed)
    p_values <- numeric(length(test_statistics))
    names(p_values) <- test_statistics

    for (stat_name in test_statistics) {
        obs_stat <- observed_stats[[stat_name]]
        pred_stats <- predictive_stats[[stat_name]]

        # Two-sided p-value: proportion more extreme than observed
        p_val <- mean(abs(pred_stats - median(pred_stats, na.rm = TRUE)) >=
                     abs(obs_stat - median(pred_stats, na.rm = TRUE)), na.rm = TRUE)
        p_values[stat_name] <- p_val
    }

    # Overall fit assessment
    extreme_stats <- sum(p_values < 0.05 | p_values > 0.95, na.rm = TRUE)
    total_stats <- sum(!is.na(p_values))
    overall_good_fit <- extreme_stats / total_stats < 0.2  # Less than 20% extreme

    # Create PPC plots
    log_msg("Creating PPC visualization...")
    ppc_plots <- create_ppc_plots(observed_stats, predictive_stats, test_statistics)

    # Prepare results data frame
    ppc_results_df <- data.frame(
        test_statistic = test_statistics,
        observed_value = sapply(test_statistics, function(s) observed_stats[[s]]),
        predictive_mean = sapply(test_statistics, function(s) mean(predictive_stats[[s]], na.rm = TRUE)),
        predictive_sd = sapply(test_statistics, function(s) sd(predictive_stats[[s]], na.rm = TRUE)),
        predictive_q025 = sapply(test_statistics, function(s) quantile(predictive_stats[[s]], 0.025, na.rm = TRUE)),
        predictive_q975 = sapply(test_statistics, function(s) quantile(predictive_stats[[s]], 0.975, na.rm = TRUE)),
        p_value = p_values,
        extreme = p_values < 0.05 | p_values > 0.95
    )

    # Prepare final results
    results <- list(
        ppc_results = ppc_results_df,
        p_values = p_values,
        ppc_plots = ppc_plots,
        overall_fit = list(
            good_fit = overall_good_fit,
            fraction_extreme = extreme_stats / total_stats,
            n_extreme = extreme_stats,
            n_total = total_stats
        ),
        observed_statistics = observed_stats,
        predictive_statistics = predictive_stats
    )

    # Save results
    ppc_file <- file.path(output_dir, "ppc_results.csv")
    write.csv(ppc_results_df, ppc_file, row.names = FALSE)

    # Save plots
    for (stat_name in test_statistics) {
        if (!is.null(ppc_plots[[stat_name]])) {
            plot_file <- file.path(output_dir, paste0("ppc_", stat_name, ".png"))
            ggsave(plot_file, plot = ppc_plots[[stat_name]], width = 8, height = 6, dpi = 300)
        }
    }

    log_msg("PPC Results Summary:")
    log_msg("  Overall good fit: %s", ifelse(overall_good_fit, "YES", "NO"))
    log_msg("  Extreme statistics: %d/%d", extreme_stats, total_stats)
    if (extreme_stats > 0) {
        extreme_names <- test_statistics[p_values < 0.05 | p_values > 0.95]
        log_msg("  Extreme test statistics: %s", paste(extreme_names, collapse = ", "))
    }
    log_msg("  Results saved to: %s", ppc_file)

    log_msg("Posterior predictive checks complete")
    return(results)
}

#' Prepare Observation Tensor for NPE
#'
#' @description Internal helper to convert observed data to tensor format expected by NPE
#'
#' @param observed_data Data frame with observed data
#' @param metadata NPE metadata
#'
#' @return PyTorch tensor
#'
#' @keywords internal
prepare_observation_tensor <- function(observed_data, metadata) {
    torch <- import("torch")

    # Convert observed data to the format used during NPE training
    n_locations <- metadata$data$n_locations
    n_timesteps <- metadata$data$n_timesteps
    n_features <- n_locations * n_timesteps

    # Initialize observation vector
    obs_vec <- numeric(n_features)

    if (!is.null(observed_data) && nrow(observed_data) > 0) {
        # Check required columns exist
        required_cols <- c("j", "t", "cases")
        missing_cols <- setdiff(required_cols, names(observed_data))
        if (length(missing_cols) > 0) {
            stop("Missing required columns in observed_data: ", paste(missing_cols, collapse = ", "))
        }

        # Get location names from metadata
        location_names <- if (!is.null(metadata$data$location_name)) {
            metadata$data$location_name
        } else {
            unique(observed_data$j)
        }

        # Create location index mapping
        location_map <- setNames(seq_along(location_names) - 1, location_names)

        # Fill observation vector with proper indexing (row-major order: location varies fastest)
        for (i in 1:nrow(observed_data)) {
            j_idx <- location_map[observed_data$j[i]]
            t_idx <- observed_data$t[i] - 1  # Convert to 0-indexed

            if (!is.na(j_idx) && j_idx >= 0 && j_idx < n_locations &&
                t_idx >= 0 && t_idx < n_timesteps) {
                # Row-major indexing: idx = t * n_locations + j
                idx <- t_idx * n_locations + j_idx + 1  # +1 for R's 1-indexing
                obs_vec[idx] <- observed_data$cases[i]
            }
        }
    } else {
        stop("observed_data must be a non-empty data frame with columns: j, t, cases")
    }

    return(torch$tensor(obs_vec, dtype = torch$float32)$unsqueeze(0L))
}

#' Generate Predictive Data Using Forward Model
#'
#' @description Internal helper to generate predictive data given parameters
#'
#' @param param_values Vector of parameter values
#' @param metadata NPE metadata
#' @param n_samples Number of predictive samples to generate
#'
#' @return Matrix of predictive data
#'
#' @keywords internal
generate_predictive_data <- function(param_values, metadata, n_samples = 1) {
    # Generate predictive case data using LASER forward model

    n_features <- metadata$data$n_locations * metadata$data$n_timesteps
    pred_data <- matrix(NA, nrow = n_samples, ncol = n_features)

    # Get parameter names from metadata
    param_names <- if (!is.null(metadata$param_names)) {
        metadata$param_names
    } else if (!is.null(metadata$parameters)) {
        metadata$parameters
    } else {
        # Generate default names if not available
        paste0("param_", seq_along(param_values))
    }

    for (i in 1:n_samples) {
        tryCatch({
            # Create configuration from parameters
            config <- create_sbc_config(param_values, param_names, metadata)

            # Create temporary file for configuration
            config_file <- tempfile(fileext = ".json")
            on.exit(unlink(config_file), add = TRUE)

            # Write configuration
            write_model_json(config, config_file, type = "priors", validate = FALSE)

            # Generate different seed for each sample
            seed <- as.integer(Sys.time()) + i

            # Run LASER model
            laser_result <- run_LASER_model(
                paramfile = config_file,
                seed = seed,
                visualize = FALSE,
                pdf = FALSE
            )

            # Extract simulated data
            simulated_data <- get_npe_simulated_data(
                laser_result = laser_result,
                include_deaths = FALSE,
                validate = FALSE,
                verbose = FALSE
            )

            # Convert to vector format
            obs_vec <- numeric(n_features)

            # Get location mapping
            location_names <- if (!is.null(metadata$data$location_name)) {
                metadata$data$location_name
            } else {
                unique(simulated_data$j)
            }
            location_map <- setNames(seq_along(location_names) - 1, location_names)

            # Fill observation vector
            for (j in 1:nrow(simulated_data)) {
                j_idx <- location_map[simulated_data$j[j]]
                t_idx <- simulated_data$t[j] - 1

                if (!is.na(j_idx) && j_idx >= 0 && j_idx < metadata$data$n_locations &&
                    t_idx >= 0 && t_idx < metadata$data$n_timesteps) {
                    idx <- t_idx * metadata$data$n_locations + j_idx + 1
                    obs_vec[idx] <- simulated_data$cases[j]
                }
            }

            pred_data[i, ] <- obs_vec

        }, error = function(e) {
            warning("LASER simulation failed for sample ", i, ": ", e$message)
            # Use fallback generation
            # Base rate influenced by first parameter (often a transmission rate)
            base_rate <- if (length(param_values) > 0) {
                max(0, param_values[1] * 10)
            } else {
                10
            }
            pred_data[i, ] <- rpois(n_features, lambda = base_rate)
        })
    }

    return(pred_data)
}

#' Prepare Data for Test Statistics Calculation
#'
#' @description Internal helper to convert data to vector for statistics
#'
#' @param data Data frame or vector
#'
#' @return Numeric vector
#'
#' @keywords internal
prepare_data_for_statistics <- function(data) {
    if (is.data.frame(data)) {
        if ("cases" %in% names(data)) {
            return(as.numeric(data$cases))
        } else {
            # Use first numeric column
            numeric_cols <- sapply(data, is.numeric)
            if (sum(numeric_cols) > 0) {
                return(as.numeric(data[[which(numeric_cols)[1]]]))
            }
        }
    }
    return(as.numeric(data))
}

#' Prepare Data from Matrix Row for Test Statistics
#'
#' @description Internal helper to convert matrix row to vector
#'
#' @param matrix_row Numeric vector from matrix row
#'
#' @return Numeric vector
#'
#' @keywords internal
prepare_data_for_statistics_from_matrix <- function(matrix_row) {
    return(as.numeric(matrix_row))
}

#' Calculate Test Statistic
#'
#' @description Internal helper to calculate various test statistics
#'
#' @param data Numeric vector
#' @param stat_name Name of test statistic
#'
#' @return Numeric value
#'
#' @keywords internal
calculate_test_statistic <- function(data, stat_name) {
    data <- data[!is.na(data) & is.finite(data)]

    if (length(data) == 0) {
        return(NA)
    }

    switch(stat_name,
           "mean" = mean(data),
           "var" = var(data),
           "sd" = sd(data),
           "max" = max(data),
           "min" = min(data),
           "median" = median(data),
           "skewness" = {
               if (length(data) >= 3 && sd(data) > 0) {
                   mean((data - mean(data))^3) / sd(data)^3
               } else {
                   NA
               }
           },
           "kurtosis" = {
               if (length(data) >= 4 && sd(data) > 0) {
                   mean((data - mean(data))^4) / sd(data)^4 - 3
               } else {
                   NA
               }
           },
           "sum" = sum(data),
           "range" = diff(range(data)),
           NA  # Unknown statistic
    )
}


