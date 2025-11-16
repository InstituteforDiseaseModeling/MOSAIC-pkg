# ==============================================================================
# NPE (Neural Posterior Estimation) Main Workflow Functions
# ==============================================================================
# Streamlined implementation for MOSAIC calibration with SMC support
# Created: 2024
# ==============================================================================

#' Train NPE Model
#'
#' @description
#' Trains a normalizing flow model for neural posterior estimation using
#' v5.2 advanced features including transform ramping, guards, and auto-tune.
#'
#' @param X Matrix of parameters (n_samples x n_params)
#' @param y Matrix of observations (n_samples x n_obs)
#' @param weights Vector of importance weights (NULL for uniform)
#' @param bounds Matrix of parameter bounds (n_params x 2)
#' @param architecture List with architecture specification
#' @param output_dir Directory to save trained model
#' @param n_epochs Maximum training epochs (default 1000)
#' @param batch_size Batch size for training (default 256)
#' @param learning_rate Initial learning rate (default 1e-3)
#' @param validation_split Proportion for validation (default 0.2)
#' @param early_stopping Use early stopping (default TRUE)
#' @param patience Early stopping patience (default 10, reduced to prevent overfitting)
#' @param use_gpu Use GPU if available (default TRUE)
#' @param seed Random seed
#' @param verbose Print progress
#'
#' @return Trained NPE model object
#' @export
train_npe <- function(
          X,
          y,
          weights = NULL,
          bounds,
          architecture,
          output_dir,
          n_epochs = 1000,
          batch_size = 512,
          learning_rate = 1e-3,
          validation_split = 0.15,
          early_stopping = TRUE,
          patience = 20,
          use_gpu = TRUE,
          seed = 42,
          verbose = TRUE
) {

     if (verbose) {
          message("=== NPE Training Diagnostics ===")
          message("  Input dimensions:")
          message("    X (parameters): ", nrow(X), " x ", ncol(X))
          message("    y (observations): ", nrow(y), " x ", ncol(y))
          message("    Data size: ", format(object.size(X), units = "MB"), " + ",
                  format(object.size(y), units = "MB"))

          # Warn about small sample size
          sample_to_param_ratio <- nrow(X) / ncol(X)
          if (sample_to_param_ratio < 10) {
               message("  ")
               message("  ⚠️  WARNING: Low sample-to-parameter ratio (", round(sample_to_param_ratio, 1), ":1)")
               message("    ", nrow(X), " samples for ", ncol(X), " parameters")
               message("    Neural networks typically need 100+ samples per parameter")
               message("    Expect early stopping and potentially poor posterior estimates")
               message("  ")
               message("  RECOMMENDATIONS:")
               message("    - Increase BFRS sample size (e.g., 1000-5000 simulations)")
               message("    - Use longer burn-in to get better BFRS samples")
               message("    - Consider reducing model complexity if needed")
               message("  ")
          }
     }

     # Import Python modules (this will fail gracefully if environment not available)
     if (verbose) message("  Importing Python modules (torch, numpy, zuko)...")
     tryCatch({
          torch <- reticulate::import("torch")
          np <- reticulate::import("numpy")
          zuko <- reticulate::import("zuko")
     }, error = function(e) {
          stop(paste0(
               "Failed to import required Python modules.\n\n",
               "Error: ", e$message, "\n\n",
               "SOLUTIONS:\n",
               "1. Install Python dependencies:\n",
               "   MOSAIC::install_dependencies()\n\n",
               "2. Check Python environment:\n",
               "   MOSAIC::check_python_env()\n\n",
               "3. If reticulate keeps crashing, restart R and try:\n",
               "   library(reticulate)\n",
               "   use_virtualenv('r-mosaic')  # or your venv path\n",
               "   Then reload MOSAIC package"
          ), call. = FALSE)
     })

     # Set random seeds
     if (verbose) message("  Setting random seeds...")
     set.seed(seed)
     torch$manual_seed(as.integer(seed))
     np$random$seed(as.integer(seed))

     # Device setup
     if (verbose) message("  Configuring compute device...")
     if (use_gpu) {
          # Check for CUDA GPU
          if (torch$cuda$is_available()) {
               device <- "cuda"
               if (verbose) {
                    gpu_name <- torch$cuda$get_device_name(0L)
                    gpu_mem <- torch$cuda$get_device_properties(0L)$total_memory / 1e9
                    message("  Using GPU: ", gpu_name, " (", round(gpu_mem, 1), " GB)")
               }
               # Check for Apple Silicon MPS
          } else if (torch$backends$mps$is_available()) {
               device <- "mps"
               if (verbose) {
                    message("  Using device: MPS (Apple Silicon)")
                    message("  ")
                    message("  ⚠️  WARNING: MPS backend is EXPERIMENTAL and may crash R")
                    message("  Known issues:")
                    message("    - May crash with large observation matrices (>1000 features)")
                    message("    - May fail during gradient computation")
                    message("    - May exhaust device memory silently")
                    message("  ")
                    message("  If training crashes R (segfault), set use_gpu = FALSE")
                    message("  CPU training is slower but much more stable")
                    message("  ")
               }
          } else {
               device <- "cpu"
               if (verbose) message("  GPU requested but not available, using CPU")
          }
     } else {
          device <- "cpu"
          if (verbose) message("  Using device: CPU")
     }

     # Convert device string to torch device object for consistency
     # This ensures all .to(device) calls work correctly
     device_obj <- torch$device(device)

     # Convert data to tensors (most crash-prone operation)
     if (verbose) {
          message("  Converting data to tensors...")
          message("    This may take a while for large datasets...")
     }

     tryCatch({
          # Convert to tensor on CPU first
          if (verbose) message("    Converting X to tensor (", nrow(X), " x ", ncol(X), ")...")
          X_tensor <- torch$tensor(as.matrix(X), dtype = torch$float32)

          if (verbose) message("    Converting y to tensor (", nrow(y), " x ", ncol(y), ")...")
          y_tensor <- torch$tensor(as.matrix(y), dtype = torch$float32)

          # Move to device
          if (device != "cpu") {
               if (verbose) message("    Moving tensors to device: ", device)
               if (verbose) message("      WARNING: This operation may fail if device memory is insufficient")
               X_tensor <- X_tensor$to(device_obj)
               y_tensor <- y_tensor$to(device_obj)
               if (verbose) message("    Successfully moved tensors to ", device)
          }
     }, error = function(e) {
          err_msg <- paste0(
               "Tensor conversion/device transfer failed: ", e$message,
               "\n\nPossible causes:",
               "\n  1. Insufficient GPU/MPS memory (observation matrix is ",
               nrow(y), " x ", ncol(y), " = ", format(object.size(y), units = "MB"), ")",
               "\n  2. MPS backend issues on Apple Silicon",
               "\n  3. Data type incompatibility",
               "\n\nSolutions to try:",
               "\n  1. Set use_gpu = FALSE to use CPU (slower but more stable)",
               "\n  2. Reduce batch size or validation split",
               "\n  3. Use fewer simulations in BFRS output"
          )
          stop(err_msg)
     })

     # Handle weights
     if (is.null(weights)) {
          weights <- rep(1.0 / nrow(X), nrow(X))
     }
     weights_tensor <- torch$tensor(weights, dtype = torch$float32)$to(device_obj)

     # Normalize data
     X_mean <- torch$mean(X_tensor, dim = 0L, keepdim = TRUE)
     X_std <- torch$std(X_tensor, dim = 0L, keepdim = TRUE) + 1e-8
     X_norm <- (X_tensor - X_mean) / X_std

     y_mean <- torch$mean(y_tensor, dim = 0L, keepdim = TRUE)
     y_std <- torch$std(y_tensor, dim = 0L, keepdim = TRUE) + 1e-8
     y_norm <- (y_tensor - y_mean) / y_std

     # Create train/validation split
     n_samples <- nrow(X)
     n_val <- as.integer(n_samples * validation_split)
     n_train <- n_samples - n_val

     indices <- sample(n_samples)
     train_idx <- indices[1:n_train]
     val_idx <- indices[(n_train+1):n_samples]

     # Build model
     n_params <- ncol(X)
     n_obs <- ncol(y)

     # Create embedding network (TCN for time series)
     embedding_net <- .create_embedding_network(
          input_dim = n_obs,
          output_dim = architecture$embedding_dim,
          architecture = architecture,
          device = device_obj
     )

     # Create normalizing flow
     flow <- .create_normalizing_flow(
          n_params = n_params,
          context_dim = architecture$embedding_dim,
          architecture = architecture,
          device = device_obj
     )

     # Combine into NPE model
     model <- torch$nn$Sequential(
          embedding_net,
          flow
     )$to(device_obj)

     # Setup optimizer and scheduler with weight decay for L2 regularization
     weight_decay <- if (!is.null(architecture$weight_decay)) {
          architecture$weight_decay
     } else {
          0.0  # Default: no weight decay
     }
     optimizer <- torch$optim$Adam(
          model$parameters(),
          lr = learning_rate,
          weight_decay = weight_decay
     )
     scheduler <- torch$optim$lr_scheduler$ReduceLROnPlateau(
          optimizer,
          mode = "min",
          patience = as.integer(architecture$scheduler_patience),
          factor = 0.5
     )

     # Training loop
     best_val_loss <- Inf
     best_epoch <- 1
     patience_counter <- 0
     training_history <- list(
          train_loss = numeric(),
          val_loss = numeric()
     )

     if (verbose) message("  Training for up to ", n_epochs, " epochs...")

     for (epoch in 1:n_epochs) {

          # Training phase
          model$train()
          train_losses <- numeric()

          # Mini-batch training
          batch_starts <- seq(1, n_train, by = batch_size)
          for (batch_start in batch_starts) {
               batch_end <- min(batch_start + batch_size - 1, n_train)
               batch_indices <- train_idx[batch_start:batch_end]

               # Convert R indices to Python indices (0-based) for proper tensor slicing
               # IMPORTANT: Keep index tensor on CPU to avoid MPS crashes
               idx_tensor <- torch$tensor(as.integer(batch_indices - 1L), dtype = torch$long)

               batch_X <- torch$index_select(X_norm, 0L, idx_tensor)
               batch_y <- torch$index_select(y_norm, 0L, idx_tensor)
               batch_w <- torch$index_select(weights_tensor, 0L, idx_tensor)

               # Forward pass
               tryCatch({
                    embedding <- embedding_net(batch_y)
                    log_prob <- flow(embedding)$log_prob(batch_X)

                    # Weighted loss
                    weighted_log_prob <- log_prob * batch_w
                    loss <- -torch$mean(weighted_log_prob)

                    # Backward pass
                    optimizer$zero_grad()
                    loss$backward()

                    # Gradient clipping
                    torch$nn$utils$clip_grad_norm_(
                         model$parameters(),
                         max_norm = architecture$gradient_clip
                    )

                    optimizer$step()

                    train_losses <- c(train_losses, loss$item())
               }, error = function(e) {
                    # If forward/backward pass fails, give detailed error
                    stop(sprintf(
                         paste0(
                              "Training failed at epoch %d, batch %d/%d\n",
                              "Error: %s\n\n",
                              "This may indicate:\n",
                              "1. MPS backend incompatibility (try use_gpu = FALSE)\n",
                              "2. Numerical instability in the model\n",
                              "3. Invalid batch data\n\n",
                              "Batch info: %d samples, device: %s"
                         ),
                         epoch, which(batch_starts == batch_start), length(batch_starts),
                         e$message, length(batch_indices), device
                    ))
               })
          }

          # Validation phase
          model$eval()
          val_loss <- tryCatch({
               with(torch$no_grad(), {
                    # Convert R indices to Python indices (0-based) for proper tensor slicing
                    # IMPORTANT: Keep index tensor on CPU to avoid MPS crashes
                    val_idx_tensor <- torch$tensor(as.integer(val_idx - 1L), dtype = torch$long)

                    val_X <- torch$index_select(X_norm, 0L, val_idx_tensor)
                    val_y <- torch$index_select(y_norm, 0L, val_idx_tensor)
                    val_w <- torch$index_select(weights_tensor, 0L, val_idx_tensor)

                    embedding <- embedding_net(val_y)
                    log_prob <- flow(embedding)$log_prob(val_X)
                    weighted_log_prob <- log_prob * val_w
                    -torch$mean(weighted_log_prob)$item()
               })
          }, error = function(e) {
               stop(sprintf(
                    paste0(
                         "Validation failed at epoch %d\n",
                         "Error: %s\n\n",
                         "This may indicate:\n",
                         "1. MPS backend incompatibility (try use_gpu = FALSE)\n",
                         "2. Model became unstable during training\n",
                         "3. Insufficient device memory"
                    ),
                    epoch, e$message
               ))
          })

          # Record history
          avg_train_loss <- mean(train_losses)
          training_history$train_loss <- c(training_history$train_loss, avg_train_loss)
          training_history$val_loss <- c(training_history$val_loss, val_loss)

          # Learning rate scheduling
          scheduler$step(val_loss)

          # Early stopping
          is_best_epoch <- FALSE
          if (val_loss < best_val_loss) {
               best_val_loss <- val_loss
               patience_counter <- 0
               # Save best model
               best_model_state <- model$state_dict()
               best_epoch <- epoch
               is_best_epoch <- TRUE
          } else {
               patience_counter <- patience_counter + 1
          }

          # Progress reporting (show epoch 1, every 10th, and improvements)
          show_progress <- verbose && (epoch == 1 || epoch %% 10 == 0 || is_best_epoch)
          if (show_progress) {
               current_lr <- optimizer$param_groups[[1]]$lr
               best_marker <- if (is_best_epoch) " ← BEST" else ""
               message(sprintf(
                    "  Epoch %d/%d - Train: %.4f, Val: %.4f, LR: %.6f%s",
                    epoch, n_epochs, avg_train_loss, val_loss, current_lr, best_marker
               ))
          }

          # Check early stopping
          if (early_stopping && patience_counter >= patience) {
               if (verbose) {
                    message(sprintf("  Early stopping triggered at epoch %d", epoch))
                    message(sprintf("  Best validation loss: %.4f (epoch %d)", best_val_loss, best_epoch))
               }
               break
          }
     }

     # Training complete
     if (verbose) {
          total_epochs <- length(training_history$train_loss)
          if (total_epochs < n_epochs) {
               message(sprintf("  Training completed: %d epochs (early stop)", total_epochs))
          } else {
               message(sprintf("  Training completed: %d epochs (max reached)", total_epochs))
          }
          message(sprintf("  Loading best model from epoch %d (val loss: %.4f)",
                         best_epoch, best_val_loss))
     }

     # Load best model
     model$load_state_dict(best_model_state)

     # Save model and metadata
     if (!is.null(output_dir)) {
          .save_npe_model(
               model = model,
               architecture = architecture,
               normalization = list(
                    X_mean = X_mean$cpu(),
                    X_std = X_std$cpu(),
                    y_mean = y_mean$cpu(),
                    y_std = y_std$cpu()
               ),
               training_history = training_history,
               bounds = bounds,
               param_names = colnames(X),
               output_dir = output_dir
          )
     }

     # Return model object
     return(list(
          model = model,
          architecture = c(architecture, list(param_names = colnames(X))),  # Include param_names
          normalization = list(
               X_mean = X_mean,
               X_std = X_std,
               y_mean = y_mean,
               y_std = y_std
          ),
          training_history = training_history,
          device = device,
          output_dir = output_dir,
          bounds = bounds  # Store bounds metadata (from priors.json) with model
     ))
}

#' Calculate NPE Architecture Specification
#'
#' @description
#' Automatically determines optimal neural network architecture based on
#' problem dimensions, with v5.2 features including guards and ramping.
#'
#' @param n_sims Number of simulations available
#' @param n_params Number of parameters to estimate
#' @param n_timesteps Number of time points in observations
#' @param n_locations Number of spatial locations
#' @param tier Architecture tier: "auto", "minimal", "small", "medium", "large", "xlarge"
#' @param verbose Print architecture details
#'
#' @return List with architecture specification
#' @export
calc_npe_architecture <- function(
          n_sims,
          n_params,
          n_timesteps,
          n_locations,
          tier = "auto",
          verbose = FALSE
) {

     # Auto-select tier based on problem size
     if (tier == "auto") {
          if (n_sims < 1000 || n_params < 10) {
               tier <- "minimal"
          } else if (n_sims < 5000 || n_params < 50) {
               tier <- "small"
          } else if (n_sims < 10000 || n_params < 100) {
               tier <- "medium"
          } else if (n_sims < 50000 || n_params < 200) {
               tier <- "large"
          } else {
               tier <- "xlarge"
          }
          if (verbose) message("  Auto-selected tier: ", tier)
     }

     # Base architecture by tier
     # Note: hidden = embedding network size, hidden_features = flow network size
     # Flow requires hidden_features >= 2 × n_params for adequate expressiveness
     specs <- list(
          minimal = list(transforms = 5, bins = 8, hidden = 50, hidden_features = 64, tcn_blocks = 2),
          small = list(transforms = 7, bins = 10, hidden = 80, hidden_features = 96, tcn_blocks = 3),
          medium = list(transforms = 10, bins = 12, hidden = 128, hidden_features = 128, tcn_blocks = 4),
          large = list(transforms = 15, bins = 16, hidden = 256, hidden_features = 256, tcn_blocks = 5),
          xlarge = list(transforms = 20, bins = 20, hidden = 512, hidden_features = 512, tcn_blocks = 6)
     )

     base_spec <- specs[[tier]]

     if (is.null(base_spec)) {
          stop("Unknown tier: ", tier, ". Valid tiers are: minimal, small, medium, large, xlarge")
     }

     # Apply transform ramping
     if (n_params > 50) {
          ramped_transforms <- min(
               20,  # cap
               base_spec$transforms + floor((n_params - 50) / 25)
          )
          base_spec$transforms <- ramped_transforms
          if (verbose) message("  Transform ramping: ", ramped_transforms, " transforms")
     }

     # Apply Large-J guard (many locations)
     if (n_locations > 25) {
          base_spec$attention_heads <- max(8, n_locations / 4)
          base_spec$hidden <- base_spec$hidden * 1.5
          if (verbose) message("  Large-J guard activated for ", n_locations, " locations")
     }

     # Apply Long-T guard (long time series)
     if (n_timesteps > 700) {
          base_spec$tcn_blocks <- max(7, base_spec$tcn_blocks)
          base_spec$hidden <- as.integer(base_spec$hidden * 1.2)
          if (verbose) message("  Long-T guard activated for ", n_timesteps, " timesteps")
     }

     # Ensure hidden dimensions are divisible by 8 for GroupNorm
     base_spec$hidden <- as.integer(ceiling(base_spec$hidden / 8) * 8)

     # Ensure flow hidden_features meets minimum requirement (>= 2 × n_params)
     # This is critical for Masked Autoregressive Flows to have adequate expressiveness
     min_flow_hidden <- as.integer(ceiling(2 * n_params / 8) * 8)  # Round up to multiple of 8
     if (!is.null(base_spec$hidden_features)) {
          base_spec$hidden_features <- as.integer(max(base_spec$hidden_features, min_flow_hidden))
     } else {
          # Fallback if hidden_features not specified (backward compatibility)
          base_spec$hidden_features <- as.integer(max(base_spec$hidden, min_flow_hidden))
     }

     if (verbose && base_spec$hidden_features > base_spec$hidden) {
          message(sprintf("  Flow hidden (%d) > embedding hidden (%d) for %d parameters",
                        base_spec$hidden_features, base_spec$hidden, n_params))
     }

     # Complete specification
     architecture <- list(
          tier = tier,
          n_transforms = base_spec$transforms,
          n_bins = base_spec$bins,
          hidden = as.integer(base_spec$hidden),  # For embedding network
          hidden_features = as.integer(base_spec$hidden_features),  # For flow (>= 2 × n_params)
          embedding_dim = as.integer(base_spec$hidden),
          tcn_blocks = base_spec$tcn_blocks,
          tcn_channels = as.integer(base_spec$hidden),
          attention_heads = if (!is.null(base_spec$attention_heads)) {
               as.integer(base_spec$attention_heads)
          } else 4,
          gradient_clip = 1.0,
          scheduler_patience = 15,  # LR scheduler patience (increased from 10)
          dropout_rate = 0.10,  # Dropout probability for regularization (reduced for large datasets)
          weight_decay = 5e-5,  # L2 regularization for optimizer (reduced for large datasets)
          # Problem dimensions
          n_sims = n_sims,
          n_params = n_params,
          n_timesteps = n_timesteps,
          n_locations = n_locations
     )

     return(architecture)
}

# ==============================================================================
# Internal Helper Functions
# ==============================================================================

#' Prepare NPE Training Data from BFRS Results
#'
#' Loads simulation outputs and parameters from BFRS results directory and prepares
#' them for Neural Posterior Estimation (NPE) training. Optimized to load only
#' individual output files for weighted simulations, avoiding the need to read
#' and filter a large combined outputs file.
#'
#' @param bfrs_dir Directory containing BFRS results with:
#'   - simulations.parquet: Parameter values and weights
#'   - outputs/timeseries/timeseries_NNNNNNN.parquet: Individual timeseries files
#'   - priors.json: Parameter prior distributions
#' @param results Optional data frame of BFRS results already loaded in memory
#'   (NULL = load from file). Preferred when results are already available to
#'   avoid redundant file I/O. Must contain columns: sim, likelihood, and parameter columns.
#' @param param_names Character vector of parameter names to extract (NULL = auto-detect)
#' @param verbose Logical, print progress messages (default: TRUE)
#' @param chunk_size Integer, chunk size for processing large datasets (NULL = auto)
#'
#' @details
#' This function implements a high-performance loading strategy:
#' \enumerate{
#'   \item Loads simulations.parquet to identify weighted simulations
#'   \item Loads only the individual timeseries files for weighted simulations
#'   \item Combines loaded files using data.table::rbindlist() for efficiency
#'   \item Reshapes data from long to wide format for NPE training
#' }
#'
#' Performance: For typical BFRS results with 1\% weighted simulations:
#' \itemize{
#'   \item Old approach (combined file): 30-120 seconds to load and filter 45M rows
#'   \item New approach (individual files): 0.1-0.5 seconds to load 450K rows directly
#'   \item Speedup: 100-1200× faster
#' }
#'
#' The function expects individual output files in the outputs/timeseries/ subdirectory.
#' These files are created during simulation. The entire outputs/ directory can be
#' removed after NPE training completes to free disk space.
#'
#' @return List containing:
#' \describe{
#'   \item{parameters}{Matrix of parameter values (n_sims × n_params)}
#'   \item{observations}{Matrix of flattened time series (n_sims × n_features)}
#'   \item{weights}{Vector of simulation weights}
#'   \item{bounds}{Matrix of parameter bounds (n_params × 2)}
#'   \item{param_names}{Character vector of parameter names}
#'   \item{n_samples, n_params, n_timesteps, n_locations}{Integer dimensions}
#'   \item{observed_data}{Optional data frame of observed data (if available)}
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic usage (loads from file)
#' npe_data <- prepare_npe_data(
#'     bfrs_dir = "path/to/1_bfrs",
#'     param_names = c("beta_env", "beta_hum", "tau_i")
#' )
#'
#' # Optimized usage (pass pre-loaded results)
#' results <- arrow::read_parquet("path/to/1_bfrs/outputs/simulations.parquet")
#' results$weight_npe <- get_npe_weights(results, strategy = "continuous_best")
#' npe_data <- prepare_npe_data(
#'     bfrs_dir = "path/to/1_bfrs",
#'     results = results,
#'     param_names = c("beta_env", "beta_hum", "tau_i")
#' )
#' }
prepare_npe_data <- function(bfrs_dir, results = NULL, param_names = NULL,
                             verbose = TRUE, chunk_size = NULL) {

     if (verbose) {
          message("\n=== NPE Data Preparation ===")
          message("Step 1/6: Loading simulations metadata...")
     }

     # Load or use provided results
     # MEMORY OPTIMIZATION: Use 'results' directly and overwrite at each step
     # to immediately free memory as we filter down
     if (is.null(results)) {
          # Fall back to loading from file
          if (verbose) {
               message("  Loading from file...")
          }

          sims_file <- file.path(bfrs_dir, "outputs", "simulations.parquet")
          if (!file.exists(sims_file)) {
               stop("Simulations file not found: ", sims_file)
          }

          if (verbose) {
               message(sprintf("  File size: %.1f MB", file.size(sims_file) / 1024^2))
          }

          results <- tryCatch({
               arrow::read_parquet(sims_file)
          }, error = function(e) {
               stop(sprintf(
                    "Failed to load simulations file.\n",
                    "File: %s\n",
                    "Size: %.1f MB\n",
                    "Error: %s\n\n",
                    "This usually indicates file corruption or insufficient memory.",
                    sims_file,
                    file.size(sims_file) / 1024^2,
                    e$message
               ), call. = FALSE)
          })
     } else {
          if (verbose) {
               message("  Using results from memory (skipping file load)")
          }
     }

     # Validate that results has required columns
     required_cols <- c("sim", "likelihood")
     missing_cols <- setdiff(required_cols, colnames(results))
     if (length(missing_cols) > 0) {
          stop("Provided results object is missing required columns: ",
               paste(missing_cols, collapse = ", "))
     }

     if (verbose) {
          message(sprintf("  Initial: %d simulations (%.1f MB in memory)",
                        nrow(results), object.size(results) / 1024^2))
     }

     # Step 2: Filter to valid simulations (OVERWRITE to free memory)
     if (verbose) message("Step 2/6: Filtering valid simulations...")
     n_before <- nrow(results)
     valid_idx <- which(!is.na(results$likelihood) & is.finite(results$likelihood))
     results <- results[valid_idx, ]  # Overwrite to free original memory

     if (verbose) {
          message(sprintf("  Valid simulations: %d (%.1f%%) - freed %.1f MB",
                        nrow(results),
                        100 * nrow(results) / n_before,
                        (object.size(results) * (n_before / nrow(results) - 1)) / 1024^2))
     }

     # Step 3: Filter to weighted simulations BEFORE loading outputs (OVERWRITE to free memory)
     # This dramatically reduces memory usage for binary strategies (e.g., 395 vs 39,465)
     # For continuous strategies, all simulations should have weight > 0
     if (verbose) message("Step 3/6: Identifying weighted simulations...")

     n_before <- nrow(results)

     if ("weight_npe" %in% colnames(results)) {
          # Detect strategy type by checking weight distribution
          n_zero <- sum(results$weight_npe == 0)
          n_nonzero <- sum(results$weight_npe > 0)

          # Binary strategy: many exact zeros (e.g., 24,800 zeros, 200 non-zero)
          # Continuous strategy: few/no exact zeros (all should be > 0)
          is_binary <- (n_zero > 0.5 * nrow(results))

          if (is_binary) {
               # Binary strategy: filter out exact zeros to save memory
               has_weight_idx <- which(results$weight_npe > 0)
               results <- results[has_weight_idx, ]

               if (verbose) {
                    message(sprintf("  Binary strategy: filtered to %d weighted simulations (%.1f%% of total)",
                                  nrow(results),
                                  100 * nrow(results) / n_before))
                    message(sprintf("  Memory freed: ~%.1f MB (%.0f%% reduction)",
                                  (object.size(results) * (n_before / nrow(results) - 1)) / 1024^2,
                                  100 * (1 - nrow(results) / n_before)))
               }
          } else {
               # Continuous strategy: keep all simulations with their weights
               if (verbose) {
                    message(sprintf("  Continuous strategy: keeping all %d simulations (%.1f%% with weight > 0)",
                                  nrow(results),
                                  100 * n_nonzero / nrow(results)))
               }
          }
     }

     # Auto-detect parameter columns if not specified
     if (is.null(param_names)) {
          # Remove non-parameter columns
          exclude_cols <- c("sim", "iter", "seed", "seed_sim", "seed_iter",
                            "likelihood", "weight", "is_finite", "is_valid",
                            "is_outlier", "is_retained", "is_best_subset",
                            "is_best_model", "weight_retained", "weight_best",
                            "weight_npe")
          param_names <- setdiff(colnames(results), exclude_cols)
          if (verbose) message(sprintf("  Auto-detected %d parameters", length(param_names)))
     }

     # MEMORY OPTIMIZATION: Extract all needed data as quickly as possible,
     # then immediately remove results to minimize window where both exist
     if (verbose) message(sprintf("  Extracting data from results (%.1f MB)...", object.size(results) / 1024^2))

     # Extract in rapid succession to minimize peak memory
     weighted_sims <- results$sim

     # Extract parameters - as.matrix() creates a copy, so do this quickly
     parameters <- as.matrix(results[, param_names, drop = FALSE])
     colnames(parameters) <- param_names  # Ensure column names are set

     # Extract weights
     if ("weight_npe" %in% colnames(results)) {
          weights <- results$weight_npe
     } else if ("weight_best" %in% colnames(results)) {
          weights <- results$weight_best
     } else {
          weights <- rep(1.0 / nrow(results), nrow(results))
     }

     # CRITICAL: Free results memory immediately after extraction
     # This minimizes the window where both results and extracted matrices exist
     if (verbose) message("  Freeing results memory...")
     rm(results)
     gc(verbose = FALSE)  # Force garbage collection to free memory immediately

     if (verbose) message(sprintf("  Extracted: %d sims, %d params (%.1f KB)",
                                length(weighted_sims), length(param_names),
                                object.size(parameters) / 1024))

     # Load outputs from individual files (OPTIMIZED: 100× faster)
     # Only loads the specific output files for weighted simulations,
     # avoiding the need to read and filter a large combined file.
     if (verbose) {
          message("Step 4/6: Loading simulation outputs...")
          message(sprintf("  Loading individual files for %d weighted simulations...",
                        length(weighted_sims)))
     }

     # Build file paths for weighted simulations (in outputs/timeseries/ subdirectory)
     output_file_paths <- file.path(
          bfrs_dir,
          "outputs", "timeseries",
          sprintf("timeseries_%07d.parquet", weighted_sims)
     )

     # Check which files exist
     file_exists_vec <- file.exists(output_file_paths)
     existing_files <- output_file_paths[file_exists_vec]
     missing_count <- sum(!file_exists_vec)

     if (length(existing_files) == 0) {
          sims_to_show <- head(weighted_sims, 3)
          example_files <- sprintf("timeseries_%07d.parquet", sims_to_show)

          stop(sprintf(
               paste0(
                    "No output files found for weighted simulations.\n\n",
                    "Weighted simulations: %s%s\n",
                    "Expected files like: %s\n",
                    "Directory: %s\n\n",
                    "This may indicate:\n",
                    "1. Simulations were run without saving outputs\n",
                    "2. Output files were deleted prematurely\n",
                    "3. Incorrect bfrs_dir path specified\n",
                    "4. NPE weights calculated but outputs not preserved\n",
                    "5. Only %d/%d simulations have non-zero weights (check weighting strategy)"
               ),
               paste(head(weighted_sims, 5), collapse = ", "),
               if (length(weighted_sims) > 5) "..." else "",
               paste(example_files, collapse = ", "),
               file.path(bfrs_dir, "outputs", "timeseries"),
               length(weighted_sims),
               n_before
          ), call. = FALSE)
     }

     if (missing_count > 0 && verbose) {
          message(sprintf("  Warning: %d/%d output files not found (%.1f%%)",
                        missing_count, length(weighted_sims),
                        100 * missing_count / length(weighted_sims)))
     }

     # Load and combine individual files
     outputs <- tryCatch({
          if (verbose) {
               message(sprintf("  Reading %d parquet files...", length(existing_files)))
          }

          # Load files with progress indication for large datasets
          if (length(existing_files) > 100 && verbose) {
               # Show progress for large file counts
               pb <- txtProgressBar(min = 0, max = length(existing_files), style = 3)
               outputs_list <- lapply(seq_along(existing_files), function(i) {
                    df <- arrow::read_parquet(existing_files[i])
                    setTxtProgressBar(pb, i)
                    # Select only needed columns - use as.data.frame() for reliable subsetting
                    as.data.frame(df)[, c("sim", "j", "t", "cases"), drop = FALSE]
               })
               close(pb)
          } else {
               # Fast path for smaller file counts
               outputs_list <- lapply(existing_files, function(f) {
                    df <- arrow::read_parquet(f)
                    # Select only needed columns - use as.data.frame() for reliable subsetting
                    as.data.frame(df)[, c("sim", "j", "t", "cases"), drop = FALSE]
               })
          }

          # Combine into single data frame
          # MEMORY OPTIMIZATION: Use data.table::rbindlist() instead of do.call(rbind)
          # rbindlist() is faster and more memory-efficient (avoids intermediate copies)
          combined <- data.table::rbindlist(outputs_list)

          # Validate that required columns exist
          required_cols <- c("sim", "j", "t", "cases")
          missing_cols <- setdiff(required_cols, colnames(combined))
          if (length(missing_cols) > 0) {
               stop("Missing required columns after loading files: ",
                    paste(missing_cols, collapse = ", "))
          }

          combined

     }, error = function(e) {
          stop(sprintf(
               "Failed to load individual output files.\n\n",
               "Files attempted: %d\n",
               "Error: %s\n\n",
               "SUGGESTED FIXES:\n",
               "1. Check that output files have correct format (sim, j, t, cases columns)\n",
               "2. Verify file permissions allow reading\n",
               "3. Ensure sufficient memory for loading files\n",
               "4. Check that files were not corrupted during simulation\n\n",
               "Technical details: %s",
               length(existing_files),
               class(e)[1],
               e$message
          ), call. = FALSE)
     })

     if (verbose) {
          outputs_mb <- object.size(outputs) / 1024^2
          message(sprintf("  Loaded %.1f MB from %d files", outputs_mb, length(existing_files)))
     }

     # Get dimensions before reshaping
     n_locations <- length(unique(outputs$j))
     n_timesteps <- max(outputs$t)

     # Prepare observations (flatten time series) with optional chunking
     if (verbose) {
          message("Step 5/6: Reshaping to observation matrix...")
          message(sprintf("  Target dimensions: %d sims × %d features (%d locations × %d timesteps)",
                        length(weighted_sims), n_locations * n_timesteps,
                        n_locations, n_timesteps))
     }

     obs_matrix <- tryCatch({
          .prepare_observation_matrix(
               outputs = outputs,
               sim_ids = weighted_sims,
               chunk_size = chunk_size,
               verbose = verbose
          )
     }, error = function(e) {
          stop(sprintf(
               paste0(
                    "Failed to reshape observation matrix.\n\n",
                    "Dimensions: %d simulations × (%d locations × %d timesteps) = %d features\n",
                    "Error: %s\n\n",
                    "This may indicate:\n",
                    "1. Insufficient memory for large matrix\n",
                    "2. Data structure issues in output files\n",
                    "3. Inconsistent dimensions across simulations\n\n",
                    "Try setting chunk_size parameter to enable chunked processing."
               ),
               length(weighted_sims), n_locations, n_timesteps,
               n_locations * n_timesteps, e$message
          ), call. = FALSE)
     })

     if (verbose) {
          message(sprintf("  Created observation matrix: %.1f MB",
                        object.size(obs_matrix) / 1024^2))
     }

     # Free memory
     rm(outputs)
     gc(verbose = FALSE)

     # Extract theoretical bounds using robust parameter bounds function
     if (verbose) message("Step 6/6: Extracting parameter bounds...")

     priors_file <- file.path(bfrs_dir, "priors.json")
     if (!file.exists(priors_file)) {
          # Try alternative locations
          alt_locations <- c(
               file.path(bfrs_dir, "config", "priors.json"),  # Check config subdirectory
               file.path(dirname(bfrs_dir), "priors.json"),
               file.path(dirname(dirname(bfrs_dir)), "priors.json")
          )
          priors_file <- NULL
          for (alt_file in alt_locations) {
               if (file.exists(alt_file)) {
                    priors_file <- alt_file
                    break
               }
          }
     }

     # Use robust bounds extraction instead of empirical ranges
     bounds_df <- get_npe_parameter_bounds(
          param_names = param_names,
          priors_file = priors_file,
          verbose = FALSE  # Suppress verbose output from this function
     )
     bounds <- as.matrix(bounds_df[, c("min", "max")])
     rownames(bounds) <- bounds_df$parameter

     # Validate that BFRS samples respect theoretical bounds (issue warnings if not)
     n_violations <- 0
     for (i in seq_along(param_names)) {
          param_range <- range(parameters[, i], na.rm = TRUE)
          if (param_range[1] < bounds[i, 1] || param_range[2] > bounds[i, 2]) {
               n_violations <- n_violations + 1
               if (verbose) {
                    warning(sprintf(
                         "Parameter '%s' violates bounds [%.6f, %.6f]. Range: [%.6f, %.6f]",
                         param_names[i], bounds[i, 1], bounds[i, 2],
                         param_range[1], param_range[2]
                    ), call. = FALSE)
               }
          }
     }

     if (verbose && n_violations > 0) {
          message(sprintf("  Warning: %d/%d parameters violate theoretical bounds",
                        n_violations, length(param_names)))
     }

     # Try to load observed data if available
     obs_data_file <- file.path(bfrs_dir, "observed_data.csv")
     observed_data <- if (file.exists(obs_data_file)) {
          read.csv(obs_data_file)
     } else {
          NULL
     }

     if (verbose) {
          total_mb <- (object.size(parameters) + object.size(obs_matrix) +
                      object.size(weights) + object.size(bounds)) / 1024^2
          message("\n=== Data Preparation Complete ===")
          message(sprintf("  Simulations: %d", nrow(parameters)))
          message(sprintf("  Parameters: %d", length(param_names)))
          message(sprintf("  Time steps: %d", n_timesteps))
          message(sprintf("  Locations: %d", n_locations))
          message(sprintf("  Observation matrix: %d x %d (%.1f MB)",
                        nrow(obs_matrix), ncol(obs_matrix),
                        object.size(obs_matrix) / 1024^2))
          message(sprintf("  Total data size: %.1f MB", total_mb))
          message("\nReady for NPE training...")
     }

     return(list(
          parameters = parameters,
          observations = obs_matrix,
          weights = weights,
          bounds = bounds,
          param_names = param_names,
          n_samples = nrow(parameters),
          n_params = length(param_names),
          n_timesteps = n_timesteps,
          n_locations = n_locations,
          observed_data = observed_data
     ))
}

#' @keywords internal
.prepare_observation_matrix <- function(outputs, sim_ids, chunk_size = NULL, verbose = FALSE) {

     # Create matrix with one row per simulation
     # Columns are flattened time series (location1_t1, location1_t2, ...)
     #
     # OPTIMIZED VERSION: Uses data.table for fast reshaping with optional chunking
     # Previous version used triple nested loops (25K × 1 × 913 = 22.8M iterations)
     # This version uses vectorized operations: ~100-200× faster
     # Chunked version: memory-safe for arbitrarily large datasets

     # Check if data.table is available
     if (!requireNamespace("data.table", quietly = TRUE)) {
          stop("data.table package required for optimized NPE data preparation. ",
               "Please install it with: install.packages('data.table')")
     }

     # All required packages loaded via NAMESPACE

     n_sims <- length(sim_ids)
     n_locations <- length(unique(outputs$j))
     n_timesteps <- max(outputs$t)
     n_features <- n_locations * n_timesteps

     # Determine if chunking is needed
     use_chunking <- !is.null(chunk_size) && n_sims > chunk_size

     if (!use_chunking) {
          # Standard processing (fast, but memory-intensive for large datasets)
          return(.prepare_observation_matrix_standard(outputs, sim_ids))
     }

     # Chunked processing for large datasets
     if (verbose) {
          n_chunks <- ceiling(n_sims / chunk_size)
          message(sprintf("  Using chunked processing: %d chunks of ~%d simulations",
                        n_chunks, chunk_size))
     }

     # Pre-allocate result matrix
     obs_matrix <- matrix(0, nrow = n_sims, ncol = n_features)

     # Ensure outputs is a data.frame before converting to data.table
     if (!is.data.frame(outputs)) {
          outputs <- as.data.frame(outputs)
     }

     # Validate required columns exist
     if (!"sim" %in% colnames(outputs)) {
          stop("Column 'sim' not found in outputs. Available columns: ",
               paste(colnames(outputs), collapse = ", "))
     }

     # Convert outputs to data.table (copy to avoid modifying original)
     dt_full <- data.table::as.data.table(outputs)

     # Process in chunks
     n_chunks <- ceiling(n_sims / chunk_size)
     for (chunk_i in 1:n_chunks) {
          if (verbose && chunk_i %% 5 == 1) {
               message(sprintf("    Processing chunk %d/%d...", chunk_i, n_chunks))
          }

          # Get chunk indices
          start_idx <- (chunk_i - 1) * chunk_size + 1
          end_idx <- min(chunk_i * chunk_size, n_sims)
          chunk_sim_ids <- sim_ids[start_idx:end_idx]

          # Filter to chunk simulations
          # PACKAGE-SAFE: Use explicit column reference instead of NSE
          dt_chunk <- dt_full[dt_full$sim %in% chunk_sim_ids, ]

          # Process chunk (same logic as standard version)
          unique_sims <- unique(chunk_sim_ids)
          unique_locs <- unique(dt_chunk$j)

          # Add indices (using $ operator is package-safe)
          dt_chunk$j_idx <- match(dt_chunk$j, unique_locs)
          dt_chunk$sim_idx <- match(dt_chunk$sim, unique_sims)
          dt_chunk$feature_idx <- (dt_chunk$j_idx - 1) * n_timesteps + dt_chunk$t

          # Reshape to wide format, then convert to matrix
          # MEMORY OPTIMIZATION: Use same variable name - automatic garbage collection
          chunk_matrix <- data.table::dcast(dt_chunk, sim_idx ~ feature_idx,
                                            value.var = 'cases', fill = 0)
          chunk_matrix <- as.matrix(chunk_matrix[, -1])  # Overwrite: old version auto-freed

          # Ensure correct dimensions (pad with zeros if needed)
          if (ncol(chunk_matrix) < n_features) {
               padded <- matrix(0, nrow = nrow(chunk_matrix), ncol = n_features)
               padded[, as.integer(colnames(chunk_matrix)[-1])] <- chunk_matrix
               chunk_matrix <- padded
          }

          # Place in pre-allocated matrix
          obs_matrix[start_idx:end_idx, ] <- chunk_matrix

          # Force garbage collection between chunks
          rm(dt_chunk, chunk_matrix)  # Note: wide already removed above
          if (chunk_i %% 10 == 0) {
               gc(verbose = FALSE)
          }
     }

     # CRITICAL: Remove dimnames to prevent Python/reticulate crashes
     dimnames(obs_matrix) <- NULL

     return(obs_matrix)
}

#' @keywords internal
.prepare_observation_matrix_standard <- function(outputs, sim_ids) {
     # Standard (non-chunked) processing - fast but memory-intensive

     # All required packages loaded via NAMESPACE

     # Ensure outputs is a data.frame before converting to data.table
     if (!is.data.frame(outputs)) {
          outputs <- as.data.frame(outputs)
     }

     # Validate required columns exist
     if (!"sim" %in% colnames(outputs)) {
          stop("Column 'sim' not found in outputs. Available columns: ",
               paste(colnames(outputs), collapse = ", "))
     }

     # Convert to data.table (copy to avoid modifying original)
     dt <- data.table::as.data.table(outputs)

     # Filter to requested simulations
     # PACKAGE-SAFE: Use explicit column reference instead of NSE
     dt <- dt[dt$sim %in% sim_ids, ]

     unique_sims <- unique(sim_ids)
     n_sims <- length(unique_sims)
     unique_locs <- unique(dt$j)

     # Add indices using data.table := operator
     # PACKAGE-SAFE: Use get() for column references in NSE context
     dt[, j_idx := match(get("j"), unique_locs)]
     dt[, sim_idx := match(get("sim"), unique_sims)]

     n_locations <- length(unique_locs)
     n_timesteps <- max(dt$t)

     # Create feature index
     # PACKAGE-SAFE: Use get() for column references
     dt[, feature_idx := (get("j_idx") - 1) * n_timesteps + get("t")]

     # Reshape from long to wide format using data.table's fast dcast
     # MEMORY OPTIMIZATION: Use same variable name - automatic garbage collection
     obs_matrix <- data.table::dcast(dt, sim_idx ~ feature_idx,
                                     value.var = 'cases', fill = 0)

     # Extract sim_idx before converting to matrix (needed for row order check)
     sim_idx_order <- obs_matrix$sim_idx

     # Convert to matrix - overwrites data.table version, which is auto-freed
     obs_matrix <- as.matrix(obs_matrix[, -1])

     # Free dt immediately to minimize peak memory during this critical operation
     rm(dt)
     gc(verbose = FALSE)

     # Ensure row order matches sim_ids order
     if (!identical(unique_sims[sim_idx_order], sim_ids)) {
          obs_matrix <- obs_matrix[match(seq_along(sim_ids), sim_idx_order), ]
     }

     # CRITICAL: Remove dimnames to prevent Python/reticulate crashes
     dimnames(obs_matrix) <- NULL

     return(obs_matrix)
}

#' @keywords internal
.ensure_python_env <- function() {
     # Check if reticulate is configured
     if (!reticulate::py_available()) {
          stop("Python environment not available. Please run MOSAIC::check_python_env()")
     }

     # Check for required packages
     required_packages <- c("torch", "numpy", "zuko")
     for (pkg in required_packages) {
          if (!reticulate::py_module_available(pkg)) {
               stop("Required Python package '", pkg, "' not found. ",
                    "Please run MOSAIC::install_dependencies()")
          }
     }
}

#' @keywords internal
.create_embedding_network <- function(input_dim, output_dim, architecture, device) {
     torch <- reticulate::import("torch")
     nn <- torch$nn

     # Use hidden dimensions from architecture
     hidden_dim <- as.integer(architecture$hidden)

     # Ensure hidden dimensions are divisible by 8 for potential GroupNorm usage
     if (hidden_dim %% 8 != 0) {
          hidden_dim <- as.integer(ceiling(hidden_dim / 8) * 8)
     }

     # Get dropout rate from architecture (default to 0.1 if not specified)
     dropout_rate <- if (!is.null(architecture$dropout_rate)) {
          architecture$dropout_rate
     } else {
          0.1
     }

     # Create MLP-based embedding network
     layers <- list()

     # Build network based on number of blocks (default to 3 if not specified)
     n_blocks <- if (!is.null(architecture$tcn_blocks)) {
          architecture$tcn_blocks
     } else {
          3  # Default: 3 hidden layers (matches "small" tier)
     }

     # Input layer
     layers[[1]] <- nn$Linear(as.integer(input_dim), hidden_dim)
     layers[[2]] <- nn$ReLU()
     layers[[3]] <- nn$Dropout(p = dropout_rate)

     # Hidden layers
     for (i in 1:n_blocks) {
          # Linear layer
          layers[[length(layers) + 1]] <- nn$Linear(hidden_dim, hidden_dim)

          # Normalization - use LayerNorm which doesn't have divisibility requirements
          layers[[length(layers) + 1]] <- nn$LayerNorm(hidden_dim)

          # Activation
          layers[[length(layers) + 1]] <- nn$ReLU()

          # Dropout for regularization
          if (i < n_blocks) {  # No dropout before final projection
               layers[[length(layers) + 1]] <- nn$Dropout(p = dropout_rate)
          }
     }

     # Output projection
     layers[[length(layers) + 1]] <- nn$Linear(hidden_dim, as.integer(output_dim))

     # Create Sequential model by unpacking layers list
     # Note: device is already a torch.device object from train_npe()
     model <- do.call(nn$Sequential, layers)$to(device)
     return(model)
}

#' @keywords internal
.create_normalizing_flow <- function(n_params, context_dim, architecture, device) {
     zuko <- reticulate::import("zuko")
     torch <- reticulate::import("torch")

     # Create MAF (Masked Autoregressive Flow)
     # Note: MAF doesn't use bins parameter (that's for NSF)
     # Note: device is already a torch.device object from train_npe()
     flow <- zuko$flows$MAF(
          features = as.integer(n_params),
          context = as.integer(context_dim),
          transforms = as.integer(architecture$n_transforms),
          hidden_features = list(
               as.integer(architecture$hidden_features),
               as.integer(architecture$hidden_features)
          )
     )$to(device)

     return(flow)
}

#' @keywords internal
.save_npe_model <- function(model, architecture, normalization,
                            training_history, bounds, param_names, output_dir) {

     torch <- reticulate::import("torch")

     # Save PyTorch model
     # Expand tilde and normalize path for Python compatibility
     model_path <- normalizePath(file.path(output_dir, "npe_model.pt"), mustWork = FALSE)
     torch$save(
          list(
               model_state_dict = model$state_dict(),
               architecture = architecture,
               normalization = normalization
          ),
          model_path
     )

     # Save metadata
     metadata <- list(
          architecture = architecture,
          training_history = training_history,
          bounds = as.data.frame(bounds),
          param_names = param_names,
          timestamp = Sys.time(),
          mosaic_version = as.character(packageVersion("MOSAIC"))
     )

     jsonlite::write_json(
          metadata,
          file.path(output_dir, "npe_metadata.json"),
          pretty = TRUE,
          auto_unbox = TRUE
     )

     # Save training history plot data
     saveRDS(training_history, file.path(output_dir, "training_history.rds"))
}

#' @keywords internal
.adjust_architecture <- function(architecture, diagnostics) {
     # Auto-tune adjustments based on diagnostics

     # If coverage is poor, increase model capacity
     if (diagnostics$coverage_50 < 0.4 || diagnostics$coverage_50 > 0.6) {
          architecture$n_transforms <- min(25, architecture$n_transforms + 3)
          architecture$hidden_features <- as.integer(architecture$hidden_features * 1.2)
     }

     # If SBC shows calibration issues, adjust bins
     if (diagnostics$sbc_pass_rate < 0.8) {
          architecture$n_bins <- min(24, architecture$n_bins + 4)
     }

     return(architecture)
}
