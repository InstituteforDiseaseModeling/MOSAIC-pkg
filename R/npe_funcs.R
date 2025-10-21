
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
          verbose = TRUE
) {

     # =============================================================================
     # SETUP
     # =============================================================================

     require(reticulate)
     require(dplyr)
     require(tidyr)
     require(arrow)
     require(jsonlite)

     set.seed(seed)

     if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
     }

     log_msg <- function(msg, ...) {
          if (verbose) {
               timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
               message(sprintf(paste(timestamp, msg), ...))
          }
     }

     log_msg("Starting Lampe NPE v5.0 with proportion-based initial conditions")

     # =============================================================================
     # DATA LOADING
     # =============================================================================

     log_msg("Loading simulation data...")
     log_msg("  Simulations file: %s", simulations_file)
     log_msg("  Outputs file: %s", outputs_file)

     # Validate input files exist
     if (!file.exists(simulations_file)) {
          stop("Simulations file not found: ", simulations_file)
     }
     if (!file.exists(outputs_file)) {
          stop("Outputs file not found: ", outputs_file)
     }

     params <- arrow::read_parquet(simulations_file)
     outputs <- arrow::read_parquet(outputs_file)

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
               }
          } else {
               # Use coefficient of variation for other parameters
               cv <- sqrt(var_val) / mean_val
               if (cv < 1e-6) {  # Less than 0.0001% variation
                    constant_params <- c(constant_params, param_name)
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
     # EXTRACT PRIOR BOUNDS
     # =============================================================================

     log_msg("Extracting prior bounds...")

     priors <- if (exists("priors_default", where = "package:MOSAIC")) {
          MOSAIC::priors_default
     } else {
          NULL
     }

     prior_bounds <- .get_npe_prior_bounds(
          simulations_df = params_valid[, final_param_cols],
          param_names = final_param_cols,
          priors = priors,
          buffer = 0.1
     )

     # CRITICAL: Update bounds for proportion parameters to [0, 1]
     # This ensures consistent bounds throughout the training pipeline
     prop_params <- grep("^prop_.*_initial_", final_param_cols, value = TRUE)
     if (length(prop_params) > 0) {
          log_msg("Setting [0, 1] bounds for %d proportion parameters", length(prop_params))

          # Convert prior_bounds to data frame if it's not already
          if (!is.data.frame(prior_bounds)) {
               prior_bounds_df <- data.frame(prior_bounds)
          } else {
               prior_bounds_df <- prior_bounds
          }

          # Update bounds for proportion parameters with explicit numeric values
          for (prop_param in prop_params) {
               param_idx <- which(prior_bounds_df$parameter == prop_param)
               if (length(param_idx) > 0) {
                    # Use explicit numeric assignment to avoid R-Python conversion issues
                    prior_bounds_df$min[param_idx] <- as.numeric(0.0)
                    prior_bounds_df$max[param_idx] <- as.numeric(1.0)

                    # Validate the update
                    actual_min <- prior_bounds_df$min[param_idx]
                    actual_max <- prior_bounds_df$max[param_idx]

                    if (actual_min != 0.0 || actual_max != 1.0) {
                         warning(sprintf("Failed to set bounds for %s: got [%.6f, %.6f]",
                                         prop_param, actual_min, actual_max))
                    } else {
                         log_msg("  %s: [0.0, 1.0] ✓", prop_param)
                    }
               }
          }

          # Convert back to original format
          prior_bounds <- prior_bounds_df

          # Store proportion parameter names for Python to double-check
          py$prop_param_names <- prop_params
     } else {
          py$prop_param_names <- character(0)
     }

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

     # Pass data and spec to Python
     py$theta_matrix_r <- theta_matrix
     py$x_matrix_r <- x_matrix
     py$n_locations <- n_locations
     py$n_timesteps <- n_timesteps
     py$param_names <- final_param_cols
     py$prior_bounds <- prior_bounds
     py$npe_spec <- npe_spec
     py$use_gpu <- use_gpu
     py$seed <- as.integer(seed)
     py$output_dir <- output_dir

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

print(f"\\nData shape: θ={theta_np.shape}, x={x_np.shape}")

# Create tensors (will standardize after split to avoid data leakage)
theta_tensor = torch.tensor(theta_np, dtype=torch.float32, device=device)
x_tensor_raw = torch.tensor(x_np, dtype=torch.float32, device=device)

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

    # Dataset and splits - using raw (non-standardized) data
    dataset = TensorDataset(theta_tensor_unit, x_tensor_raw)
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

    # Recreate datasets with standardized observations
    train_dataset = TensorDataset(theta_tensor_unit[train_indices], x_train_tensor)
    val_dataset = TensorDataset(theta_tensor_unit[val_indices], x_val_tensor)

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

        for batch_idx, (batch_theta, batch_x_raw) in enumerate(train_loader):
            # Gradient accumulation (Feature #5): only zero gradients at start of accumulation
            if batch_idx % gradient_accumulate_steps == 0:
                optimizer.zero_grad()

            # Mixed precision training (Feature #2)
            if use_amp and device == "cuda" and scaler is not None:
                with torch.autocast(device):
                    x_ctx = encoder(batch_x_raw)
                    loss = -npe.flow(x_ctx).log_prob(batch_theta).mean()
                    # Scale loss for gradient accumulation
                    loss = loss / gradient_accumulate_steps

                if torch.isnan(loss) or torch.isinf(loss):
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
                loss = -npe.flow(x_ctx).log_prob(batch_theta).mean()
                # Scale loss for gradient accumulation
                loss = loss / gradient_accumulate_steps

                if torch.isnan(loss) or torch.isinf(loss):
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
            for batch_theta, batch_x_raw in val_loader:
                x_ctx = encoder(batch_x_raw)
                val_loss = -npe.flow(x_ctx).log_prob(batch_theta).mean()
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
        "n_timesteps": n_timesteps
    },
    "initial_conditions": {
        "converted_to_proportions": True,
        "proportion_bounds": [0.0, 1.0],
        "note": "Initial condition counts converted to proportions during training"
    },
    "spec_used": npe_spec
}

with open(os.path.join(output_dir, "npe_metadata.json"), "w") as f:
    json.dump(metadata, f, indent=2)

print(f"\\nModels saved to: {output_dir}")
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
#'   creates \code{posterior_estimates} subdirectory in \code{model_dir}.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#'
#' @return Data frame with posterior quantiles matching \code{\link{calc_model_posterior_quantiles}}
#'   format containing columns:
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
#' posterior_quantiles <- est_npe_posterior(
#'   model_dir = "path/to/trained/npe/models",
#'   observed_data = observed,
#'   n_samples = 5000,
#'   quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)
#' )
#' }
#'
#' @export
est_npe_posterior <- function(
          model_dir,
          observed_data,
          n_samples = 10000,
          quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
          output_dir = NULL,
          verbose = TRUE
) {

     # =============================================================================
     # SETUP AND VALIDATION
     # =============================================================================

     log_msg <- function(msg, ...) {
          if (verbose) {
               timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
               message(sprintf(paste(timestamp, msg), ...))
          }
     }

     log_msg("Starting NPE v5 parameter estimation")

     # Validate inputs
     if (!dir.exists(model_dir)) {
          stop("Model directory does not exist: ", model_dir)
     }

     if (!is.data.frame(observed_data)) {
          stop("observed_data must be a data frame")
     }

     if (n_samples <= 0 || !is.numeric(n_samples)) {
          stop("n_samples must be a positive integer")
     }

     if (!is.numeric(quantiles) || any(quantiles < 0) || any(quantiles > 1)) {
          stop("quantiles must be numeric values between 0 and 1")
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

     # Set output directory
     if (is.null(output_dir)) {
          output_dir <- file.path(model_dir, "posterior_estimates")
     }

     if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
     }

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

     # Check for and handle missing values in observed data
     if (any(is.na(observed_x_matrix))) {
          na_count <- sum(is.na(observed_x_matrix))
          log_msg("Warning: %d NaN values found in observed data - replacing with zeros", na_count)
          observed_x_matrix[is.na(observed_x_matrix)] <- 0
     }

     # Apply preprocessing if specified in model
     if (!is.null(npe_spec$preprocessing$use_log1p) && npe_spec$preprocessing$use_log1p) {
          observed_x_matrix <- log1p(observed_x_matrix)
          log_msg("Applied log1p transformation to observed data")
     }

     # Final validation of observed data
     if (any(!is.finite(observed_x_matrix))) {
          non_finite_count <- sum(!is.finite(observed_x_matrix))
          log_msg("Warning: %d non-finite values found after preprocessing - replacing with zeros", non_finite_count)
          observed_x_matrix[!is.finite(observed_x_matrix)] <- 0
     }

     log_msg("Observed data matrix shape: 1 × %d (range: %.3f to %.3f)",
             ncol(observed_x_matrix), min(observed_x_matrix), max(observed_x_matrix))

     # =============================================================================
     # PYTHON ENVIRONMENT SETUP
     # =============================================================================

     log_msg("Setting up Python environment...")

     # Load required libraries
     required_packages <- c("reticulate", "dplyr", "tidyr", "jsonlite", "arrow")
     missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

     if (length(missing_packages) > 0) {
          stop("Required R packages not available: ", paste(missing_packages, collapse = ", "))
     }

     # Import Python modules with error handling
     torch <- tryCatch({
          reticulate::import("torch", convert = FALSE)
     }, error = function(e) {
          stop("PyTorch not available. Please install: pip install torch")
     })

     np <- tryCatch({
          reticulate::import("numpy")
     }, error = function(e) {
          stop("NumPy not available. Please install: pip install numpy")
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

except Exception as e:
    print(f"ERROR in parameter estimation: {e}")
    import traceback
    traceback.print_exc()
    error_message = str(e)
    estimation_success = False
    # Create dummy samples
    if len(param_names) > 0:
        samples = np.full((n_samples, len(param_names)), np.nan)
    else:
        samples = np.full((n_samples, 1), np.nan)
        param_names = ["unknown"]

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
     # SAVE RESULTS
     # =============================================================================

     log_msg("Saving results...")

     # Save posterior quantiles CSV
     quantiles_file <- file.path(output_dir, "posterior_quantiles.csv")
     write.csv(quantile_results, quantiles_file, row.names = FALSE)

     # Save posterior samples
     samples_file <- file.path(output_dir, "posterior_samples.parquet")
     arrow::write_parquet(as.data.frame(posterior_samples), samples_file)

     # Create estimation metadata
     estimation_info <- list(
          timestamp = Sys.time(),
          estimation_time_seconds = estimation_time,
          n_samples = n_samples,
          n_parameters = ncol(posterior_samples),
          quantiles_computed = quantiles,
          model_source = model_dir,
          n_ensemble_models = length(py$ensemble_models),
          device_used = device_type,
          observed_data_summary = list(
               n_locations = n_locations,
               n_timesteps = n_timesteps,
               total_cases = sum(observed_data$cases, na.rm = TRUE),
               max_cases = max(observed_data$cases, na.rm = TRUE)
          ),
          function_version = "v5"
     )

     info_file <- file.path(output_dir, "estimation_info.json")
     jsonlite::write_json(estimation_info, info_file, pretty = TRUE, auto_unbox = TRUE)

     log_msg("Estimation completed in %.1f seconds", estimation_time)
     log_msg("Results saved to: %s", output_dir)
     log_msg("Quantiles CSV: %s", basename(quantiles_file))

     # Return results matching calc_model_posterior_quantiles format
     return(quantile_results)
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
.get_npe_prior_bounds <- function(simulations_df, param_names, priors = NULL, buffer = 0.1) {

     bounds_list <- list()

     for (param_name in param_names) {

          if (!param_name %in% names(simulations_df)) {
               warning(sprintf("Parameter %s not found in simulations", param_name))
               next
          }

          # Get empirical range from simulations
          param_values <- simulations_df[[param_name]]
          param_values <- param_values[is.finite(param_values)]  # Remove NA/Inf

          if (length(param_values) == 0) {
               warning(sprintf("No finite values for parameter %s", param_name))
               next
          }

          # Calculate empirical bounds with buffer
          emp_min <- min(param_values, na.rm = TRUE)
          emp_max <- max(param_values, na.rm = TRUE)

          # SPECIAL CASE: V1/V2 vaccination parameters use 95% CI + 2.5% bounds
          if (grepl("^V[12]_j_initial_", param_name)) {
               # Calculate 95% confidence interval bounds
               ci_lower <- quantile(param_values, 0.025, na.rm = TRUE)  # 2.5th percentile
               ci_upper <- quantile(param_values, 0.975, na.rm = TRUE) # 97.5th percentile
               ci_range <- ci_upper - ci_lower

               # Apply 2.5% buffer to the CI range
               buffered_min <- ci_lower - 0.025 * ci_range
               buffered_max <- ci_upper + 0.025 * ci_range

               # Ensure non-negative for vaccination counts
               buffered_min <- max(0, buffered_min)

               cat(sprintf("V1/V2 CI bounds for %s: [%.0f, %.0f] (CI: [%.0f, %.0f])\n",
                           param_name, buffered_min, buffered_max, ci_lower, ci_upper))
          } else {
               # Standard buffer method for other parameters
               range_width <- emp_max - emp_min
               if (range_width > 0) {
                    buffered_min <- emp_min - buffer * range_width
                    buffered_max <- emp_max + buffer * range_width
               } else {
                    # If all values are the same, add small absolute buffer
                    buffered_min <- emp_min - abs(emp_min) * buffer
                    buffered_max <- emp_max + abs(emp_max) * buffer
                    if (buffered_min == buffered_max) {
                         buffered_min <- emp_min - 0.1
                         buffered_max <- emp_max + 0.1
                    }
               }
          }

          # Get distribution type if priors provided
          dist_type <- "empirical"  # Default
          if (!is.null(priors)) {
               dist_info <- .get_distribution_type(param_name, priors)
               if (!is.null(dist_info)) {
                    dist_type <- dist_info$distribution
               }
          }

          # Apply distribution-specific constraints
          constrained_bounds <- .apply_distribution_constraints(
               buffered_min, buffered_max, dist_type, param_name
          )

          # Store bounds
          bounds_list[[param_name]] <- data.frame(
               parameter = param_name,
               min = constrained_bounds$min,
               max = constrained_bounds$max,
               distribution = dist_type,
               empirical_min = emp_min,
               empirical_max = emp_max,
               stringsAsFactors = FALSE
          )
     }

     # Combine all bounds
     if (length(bounds_list) > 0) {
          bounds_df <- do.call(rbind, bounds_list)
          rownames(bounds_df) <- NULL
          return(bounds_df)
     } else {
          stop("No valid parameter bounds could be extracted")
     }
}

#' Get distribution type for a parameter from priors
#'
#' @param param_name Parameter name
#' @param priors MOSAIC priors object
#' @return List with distribution info or NULL
.get_distribution_type <- function(param_name, priors) {

     # Check if it's location-specific
     is_location_param <- grepl("_[A-Z]{3}$", param_name)

     if (is_location_param) {
          # Extract base name and location
          base_name <- gsub("_[A-Z]{3}$", "", param_name)
          location <- regmatches(param_name, regexpr("[A-Z]{3}$", param_name))

          # Handle parameter naming mismatches
          name_mapping <- list(
               "a_1_j" = "a1", "a_2_j" = "a2",
               "b_1_j" = "b1", "b_2_j" = "b2",
               "beta_j0_tot_j" = "beta_j0_tot",
               "p_beta_j" = "p_beta",
               "tau_i_j" = "tau_i",
               "theta_j_j" = "theta_j",
               "mu_j_baseline_j" = "mu_j_baseline",
               "mu_j_slope_j" = "mu_j_slope",
               "mu_j_epidemic_factor_j" = "mu_j_epidemic_factor",
               "epidemic_threshold_j" = "epidemic_threshold",
               "psi_star_a_j" = "psi_star_a",
               "psi_star_b_j" = "psi_star_b",
               "psi_star_z_j" = "psi_star_z",
               "psi_star_k_j" = "psi_star_k"
          )

          if (base_name %in% names(name_mapping)) {
               base_name <- name_mapping[[base_name]]
          }

          # Look in location-specific parameters
          if (!is.null(priors$parameters_location[[base_name]]$location[[location]])) {
               return(priors$parameters_location[[base_name]]$location[[location]])
          }
     } else {
          # Global parameter
          if (!is.null(priors$parameters_global[[param_name]])) {
               return(priors$parameters_global[[param_name]])
          }
     }

     return(NULL)
}

#' Apply distribution-specific constraints to bounds
#'
#' @param min_val Minimum value from empirical range
#' @param max_val Maximum value from empirical range
#' @param dist_type Distribution type
#' @param param_name Parameter name for context
#' @return List with constrained min and max
.apply_distribution_constraints <- function(min_val, max_val, dist_type, param_name) {

     # Beta distribution and ALL proportion parameters: strictly [0, 1]
     # COMPREHENSIVE FIX: Include all proportion patterns and specific parameters
     if (dist_type == "beta" ||
         grepl("^prop_.*_initial_", param_name) ||  # prop_*_initial_* parameters
         grepl("^p_", param_name) ||                 # All p_* parameters (p_beta, p_detect, etc.)
         grepl("^phi_", param_name) ||               # phi_1, phi_2 (vaccination effectiveness)
         grepl("^rho", param_name) ||                # rho (correlation parameter)
         grepl("^sigma", param_name) ||              # sigma (proportion parameter)
         param_name %in% c("p_detect", "p_asymp", "p_severe", "p_beta",
                           "phi_1", "phi_2", "rho", "sigma")) {
          min_val <- max(0, min_val)
          max_val <- min(1, max_val)
     }

     # CRITICAL FIX: Environmental parameters that must be positive
     # These are biologically required to be positive regardless of distribution type
     else if (param_name %in% c("zeta_1", "zeta_2", "kappa", "decay_days_short", "decay_days_long",
                                "decay_shape_1", "decay_shape_2")) {
          min_val <- max(1e-10, min_val)  # Small positive lower bound
          # Keep empirical max + buffer
     }

     # CRITICAL FIX: Transmission and rate parameters that must be positive or non-negative
     else if (grepl("^beta_j0_|^tau_i_", param_name) ||         # Transmission parameters
              grepl("^mu_j|^mu_i", param_name) ||                # Mortality rates
              grepl("^gamma_[12]", param_name) ||                # Recovery rates
              grepl("^omega_[12]", param_name) ||                # Waning rates
              grepl("^epsilon", param_name) ||                   # Incubation rate
              grepl("^iota", param_name) ||                      # Importation rate
              grepl("mobility_gamma|mobility_omega", param_name)) {  # Mobility parameters
          min_val <- max(0, min_val)  # Cannot be negative
          # Keep empirical max + buffer
     }

     # CRITICAL FIX: Initial condition parameters (counts) must be non-negative
     else if (grepl("_j_initial_", param_name) && !grepl("^N_j_initial", param_name)) {
          min_val <- max(0, min_val)  # Counts cannot be negative
          # Keep empirical max + buffer
     }

     # Poisson, Gamma, Lognormal: non-negative (ENHANCED PATTERN)
     # This is a catch-all for any remaining rate/count parameters
     else if (dist_type %in% c("poisson", "gamma", "lognormal") ||
              grepl("rate|count|size|duration|delay", param_name)) {
          min_val <- max(0, min_val)
          # Keep max_val from empirical + buffer
     }

     # Discrete uniform (e.g., integers)
     else if (dist_type == "discrete_uniform") {
          min_val <- floor(min_val)
          max_val <- ceiling(max_val)
     }

     # Gompertz: typically small positive values
     else if (dist_type == "gompertz" || grepl("mu_j|mu_i", param_name)) {
          min_val <- max(1e-10, min_val)
          # Gompertz typically has small values
          if (max_val > 1) {
               warning(sprintf("Large max value %.3f for Gompertz parameter %s", max_val, param_name))
          }
     }

     # Temperature/climate parameters
     else if (grepl("temp|climate", param_name)) {
          # Temperature in Celsius typically [-50, 60]
          min_val <- max(-50, min_val)
          max_val <- min(60, max_val)
     }

     # Correlation parameters
     else if (grepl("cor|rho", param_name)) {
          min_val <- max(-1, min_val)
          max_val <- min(1, max_val)
     }

     # Default: use empirical + buffer as is
     # (includes normal, truncnorm, uniform)

     # BIOLOGICAL VALIDATION LAYER - Final safety check
     bv <- .apply_biological_validation(min_val, max_val, param_name)
     min_val <- bv$min
     max_val <- bv$max

     return(list(min = min_val, max = max_val))
}

#' Apply biological validation to parameter bounds
#'
#' Final safety check to ensure bounds make biological sense regardless of
#' distribution type or pattern matching. This catches any parameters that
#' fall through the constraint logic.
#'
#' @param min_val Current minimum bound
#' @param max_val Current maximum bound
#' @param param_name Parameter name
#' @return List with biologically validated min and max
.apply_biological_validation <- function(min_val, max_val, param_name) {

     # Environmental/biological parameters that MUST be positive
     biological_positive <- c(
          # Shedding rates (cannot be negative)
          "zeta_1", "zeta_2",
          # Infectious dose (cannot be negative)
          "kappa",
          # Survival times (cannot be negative)
          "decay_days_short", "decay_days_long",
          # Shape parameters (cannot be negative)
          "decay_shape_1", "decay_shape_2"
     )

     # Rate parameters that should be positive
     if (param_name %in% biological_positive) {
          if (min_val < 0) {
               warning(sprintf("BIOLOGICAL FIX: %s minimum bound was negative (%e), setting to 1e-10",
                               param_name, min_val))
               min_val <- 1e-10
          }
     }

     # Transmission rates should be positive
     if (grepl("^beta_j0_", param_name) || grepl("^tau_i_", param_name)) {
          if (min_val < 0) {
               warning(sprintf("BIOLOGICAL FIX: %s (transmission) minimum bound was negative (%e), setting to 0",
                               param_name, min_val))
               min_val <- 0
          }
     }

     # Initial condition parameters (counts) should be non-negative
     if (grepl("_j_initial_", param_name) && !grepl("^N_j_initial", param_name)) {
          if (min_val < 0) {
               warning(sprintf("BIOLOGICAL FIX: %s (count) minimum bound was negative (%e), setting to 0",
                               param_name, min_val))
               min_val <- 0
          }
     }

     # Waning/recovery rates should be positive
     if (grepl("^omega_|^gamma_1|^gamma_2|^iota|^epsilon", param_name)) {
          if (min_val < 0) {
               warning(sprintf("BIOLOGICAL FIX: %s (rate) minimum bound was negative (%e), setting to 0",
                               param_name, min_val))
               min_val <- 0
          }
     }

     # Proportions/probabilities should be [0,1]
     if (grepl("^phi_|^rho|^sigma|^p_|^prop_", param_name) ||
         grepl("_z_", param_name)) {  # psi_star_z parameters are smoothing factors [0,1]
          if (min_val < 0) {
               warning(sprintf("BIOLOGICAL FIX: %s (proportion) minimum bound was negative (%e), setting to 0",
                               param_name, min_val))
               min_val <- 0
          }
          if (max_val > 1) {
               warning(sprintf("BIOLOGICAL FIX: %s (proportion) maximum bound was > 1 (%e), setting to 1",
                               param_name, max_val))
               max_val <- 1
          }
     }

     # Alpha transmission parameters should be [0.05, 0.99] to avoid edge cases
     # alpha_1: population mixing parameter (0 = no mixing, 1 = complete mixing)
     # alpha_2: frequency vs density dependent transmission (0 = density, 1 = frequency)
     if (grepl("^alpha_[12]$", param_name)) {
          if (min_val < 0.05) {
               warning(sprintf("BIOLOGICAL FIX: %s (transmission parameter) minimum bound was below 0.05 (%e), setting to 0.05",
                               param_name, min_val))
               min_val <- 0.05
          }
          if (max_val > 0.99) {
               warning(sprintf("BIOLOGICAL FIX: %s (transmission parameter) maximum bound exceeded 0.99 (%e), setting to 0.99",
                               param_name, max_val))
               max_val <- 0.99
          }
     }

     # Ensure min < max
     if (min_val >= max_val) {
          warning(sprintf("BIOLOGICAL FIX: %s bounds invalid [%e, %e], adjusting",
                          param_name, min_val, max_val))
          if (min_val > 0) {
               max_val <- min_val * 2
          } else {
               max_val <- min_val + 1
          }
     }

     return(list(min = min_val, max = max_val))
}

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




