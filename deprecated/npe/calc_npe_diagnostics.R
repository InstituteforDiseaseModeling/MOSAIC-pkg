#' Calculate NPE Model Diagnostics with SBC
#'
#' Performs Simulation-Based Calibration (SBC) diagnostics for NPE models by running
#' multiple test simulations to assess coverage and calibration of posterior estimates.
#'
#' @param n_sbc_sims Integer number of SBC test simulations to run (each samples a different "true" theta)
#' @param npe_model_dir Character path to trained NPE model directory
#' @param PATHS List containing project paths from get_paths()
#' @param priors Prior distributions object
#' @param config_base Base configuration template
#' @param n_npe_samples Number of NPE posterior samples to draw per test simulation (default 1000)
#' @param probs Numeric vector of probabilities for quantiles (default c(0.025, 0.25, 0.75, 0.975))
#' @param seed_offset Integer offset for random seeds (default 1000)
#' @param output_dir Character path to save results (NULL = don't save)
#' @param verbose Logical whether to print progress messages
#' @param parallel Logical whether to run simulations in parallel
#' @param n_cores Number of cores for parallel processing (NULL = use all - 1)
#'
#' @return List containing:
#'   - coverage: Matrix of coverage indicators (n_sbc_sims x n_params)
#'   - sbc_ranks: Matrix of SBC ranks (n_sbc_sims x n_params)
#'   - summary: Data frame with summary statistics per parameter
#'   - param_names: Character vector of parameter names
#'   - diagnostics: Additional diagnostic information
#'
#' @export
calc_npe_diagnostics <- function(
          n_sbc_sims,
          npe_model_dir,
          PATHS,
          priors,
          config_base,
          n_npe_samples = 1000,
          probs = c(0.025, 0.25, 0.75, 0.975),
          seed_offset = 1000,
          output_dir = NULL,
          verbose = TRUE,
          parallel = FALSE,
          n_cores = NULL
) {

     # =============================================================================
     # SETUP
     # =============================================================================

     start_time <- Sys.time()

     # Validate inputs
     if (n_sbc_sims <= 0) stop("n_sbc_sims must be positive")
     if (!dir.exists(npe_model_dir)) stop("NPE model directory not found: ", npe_model_dir)

     # Validate probs argument
     if (!is.numeric(probs) || any(probs < 0) || any(probs > 1)) {
          stop("probs must be a numeric vector with values between 0 and 1")
     }
     if (length(probs) < 2) {
          stop("probs must contain at least 2 values for coverage calculation")
     }

     # Extract quantiles for coverage calculation (using first and last)
     coverage_lower <- min(probs)
     coverage_upper <- max(probs)
     coverage_level <- coverage_upper - coverage_lower

     # Check Python environment and import laser_cholera model module
     if (!reticulate::py_module_available("laser_cholera")) {
          stop("laser_cholera Python module not found. Use check_python_env() to diagnose or use_mosaic_env() to activate environment.")
     }
     lc <- reticulate::import("laser_cholera.metapop.model")

     # Get location information
     location_names <- config_base$location_name
     n_locations <- length(location_names)

     if (verbose) {
          cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
          cat("NPE POSTERIOR DIAGNOSTICS (SBC)\n")
          cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")
          cat("SBC test simulations:", n_sbc_sims, "\n")
          cat("Locations:", paste(location_names, collapse = ", "), "\n")
          cat("NPE samples per test:", n_npe_samples, "\n")
          cat("Quantiles:", paste(probs, collapse = ", "), "\n")
          cat("Coverage level:", sprintf("%.1f%%", coverage_level * 100), "\n")
          cat("Parallel processing:", ifelse(parallel, paste("YES (", n_cores, " cores)", sep = ""), "NO"), "\n")
          cat(paste(rep("-", 60), collapse = ""), "\n\n", sep = "")
     }

     # =============================================================================
     # HELPER FUNCTION: MAP PARAMETERS BETWEEN FORMATS
     # =============================================================================

     map_config_to_npe_params <- function(config, npe_param_names) {
          # Maps config parameters to NPE parameter names
          # Handles location-specific parameter naming conventions

          mapped_values <- list()

          # Define location-specific parameter base names
          location_params_base <- c(
               "beta_j0_env", "beta_j0_hum", "beta_j0_tot",
               "tau_i", "theta_j", "p_beta", "mu_j",
               "a_1_j", "a_2_j", "b_1_j", "b_2_j"
          )

          for (npe_param in npe_param_names) {

               # Check if parameter has location suffix
               has_location_suffix <- any(sapply(location_names, function(loc) {
                    grepl(paste0("_", loc, "$"), npe_param)
               }))

               if (has_location_suffix) {
                    # Extract base name and location
                    for (loc in location_names) {
                         loc_suffix <- paste0("_", loc)
                         if (grepl(paste0(loc_suffix, "$"), npe_param)) {
                              base_param <- sub(loc_suffix, "", npe_param)
                              loc_idx <- which(location_names == loc)

                              # Get value from config
                              if (base_param %in% names(config)) {
                                   param_value <- config[[base_param]]
                                   if (length(param_value) >= loc_idx) {
                                        mapped_values[[npe_param]] <- param_value[loc_idx]
                                   } else if (length(param_value) == 1) {
                                        # Single location case - scalar becomes vector element
                                        mapped_values[[npe_param]] <- param_value
                                   }
                              }
                              break
                         }
                    }
               } else {
                    # Global parameter - direct mapping
                    if (npe_param %in% names(config)) {
                         mapped_values[[npe_param]] <- config[[npe_param]]
                    }
               }
          }

          return(mapped_values)
     }

     # =============================================================================
     # SIMULATION FUNCTION
     # =============================================================================

     run_single_simulation <- function(sim_idx) {

          tryCatch({

               if (verbose && !parallel) {
                    cat(sprintf("[Test %d/%d] Starting...\n", sim_idx, n_sbc_sims))
               }

               # Import laser_cholera within the function for parallel workers
               if (parallel) {
                    lc <- reticulate::import("laser_cholera.metapop.model")
               }

               # Sample parameters from prior
               theta_true <- sample_parameters(
                    PATHS = PATHS,
                    priors = priors,
                    config = config_base,
                    seed = seed_offset + sim_idx,
                    verbose = FALSE
               )

               # Forward simulate with LASER
               if (verbose && !parallel) cat(sprintf("[Test %d/%d] Running LASER forward simulation...\n", sim_idx, n_sbc_sims))
               model_output <- lc$run_model(paramfile = theta_true, quiet = TRUE)

               # Extract simulated data in NPE format
               y_sim <- get_npe_simulated_data(model_output, verbose = FALSE)

               # Run NPE inference
               if (verbose && !parallel) cat(sprintf("[Test %d/%d] Running NPE inference...\n", sim_idx, n_sbc_sims))
               npe_result <- est_npe_posterior(
                    model_dir = npe_model_dir,
                    observed_data = y_sim,
                    n_samples = n_npe_samples,
                    quantiles = probs,
                    output_dir = NULL,  # Don't save files
                    verbose = FALSE
               )

               # Extract results
               theta_npe_quantiles <- npe_result$quantiles
               theta_npe_samples <- npe_result$samples
               param_names <- npe_result$param_names

               # Map true parameters to NPE parameter names
               true_values <- map_config_to_npe_params(theta_true, param_names)

               # Calculate coverage and SBC ranks for each parameter
               coverage <- numeric(length(param_names))
               sbc_ranks <- numeric(length(param_names))

               for (i in seq_along(param_names)) {
                    param <- param_names[i]

                    if (param %in% names(true_values)) {
                         true_val <- true_values[[param]]

                         # Coverage: Check if true value is within CI based on probs
                         param_row <- which(theta_npe_quantiles$parameter == param)
                         if (length(param_row) > 0) {
                              # Get column names for the min and max quantiles
                              lower_col <- paste0("q", format(coverage_lower, nsmall = 3))
                              upper_col <- paste0("q", format(coverage_upper, nsmall = 3))

                              coverage[i] <- as.numeric(
                                   true_val >= theta_npe_quantiles[[lower_col]][param_row] &
                                        true_val <= theta_npe_quantiles[[upper_col]][param_row]
                              )
                         } else {
                              coverage[i] <- NA
                         }

                         # SBC rank: Proportion of posterior samples < true value
                         sbc_ranks[i] <- mean(theta_npe_samples[, param] < true_val)

                    } else {
                         # Parameter not found in true values
                         coverage[i] <- NA
                         sbc_ranks[i] <- NA
                         if (verbose && !parallel) {
                              cat(sprintf("  Warning: Parameter %s not found in true values\n", param))
                         }
                    }
               }

               names(coverage) <- param_names
               names(sbc_ranks) <- param_names

               if (verbose && !parallel) {
                    cat(sprintf("[Test %d/%d] Complete. Coverage: %.1f%%\n",
                                sim_idx, n_sbc_sims, mean(coverage, na.rm = TRUE) * 100))
               }

               return(list(
                    success = TRUE,
                    coverage = coverage,
                    sbc_ranks = sbc_ranks,
                    param_names = param_names
               ))

          }, error = function(e) {
               error_msg <- paste(e$message, collapse = "\n")

               # Add more detailed error information
               if (exists("lc")) {
                    error_msg <- paste(error_msg, "\nPython module loaded: TRUE")
               } else {
                    error_msg <- paste(error_msg, "\nPython module loaded: FALSE")
               }

               if (verbose || parallel) {
                    cat(sprintf("[Test %d/%d] ERROR: %s\n", sim_idx, n_sbc_sims, error_msg))
               }

               return(list(
                    success = FALSE,
                    error = error_msg,
                    coverage = NA,
                    sbc_ranks = NA
               ))
          })
     }

     # =============================================================================
     # RUN SIMULATIONS
     # =============================================================================

     if (parallel) {
          # Parallel execution
          if (is.null(n_cores)) {
               n_cores <- parallel::detectCores() - 1
          }

          if (verbose) cat("Starting parallel SBC tests on", n_cores, "cores...\n\n")

          cl <- parallel::makeCluster(n_cores)
          parallel::clusterEvalQ(cl, {
               library(MOSAIC)  # Load MOSAIC package for required functions
               library(reticulate)
               library(dplyr)
               library(tidyr)

               # Activate MOSAIC Python environment in each worker
               tryCatch({
                    MOSAIC::use_mosaic_env()
               }, error = function(e) {
                    # If use_mosaic_env fails, try to configure Python manually
                    reticulate::use_python(Sys.which("python3"), required = FALSE)
               })
          })

          # Export required objects to cluster (lc will be imported within each worker)
          parallel::clusterExport(cl, c(
               "sample_parameters", "get_npe_simulated_data",
               "est_npe_posterior",
               "PATHS", "priors", "config_base",
               "npe_model_dir", "n_npe_samples", "probs",
               "seed_offset", "n_sbc_sims", "location_names", "n_locations",
               "map_config_to_npe_params", "verbose", "parallel",
               "coverage_lower", "coverage_upper"
          ), envir = environment())

          results <- parallel::parLapply(cl, 1:n_sbc_sims, run_single_simulation)
          parallel::stopCluster(cl)

     } else {
          # Sequential execution
          results <- lapply(1:n_sbc_sims, run_single_simulation)
     }

     # =============================================================================
     # PROCESS RESULTS
     # =============================================================================

     if (verbose) cat("\nProcessing results...\n")

     # Extract successful simulations
     successful_sims <- sapply(results, function(x) x$success)
     n_successful <- sum(successful_sims)

     if (n_successful == 0) {
          stop("All simulations failed. Check error messages above.")
     }

     if (n_successful < n_sbc_sims) {
          warning(sprintf("%d out of %d SBC tests failed", n_sbc_sims - n_successful, n_sbc_sims))
     }

     # Get parameter names from first successful simulation
     first_success <- which(successful_sims)[1]
     param_names <- results[[first_success]]$param_names
     n_params <- length(param_names)

     # Initialize result matrices
     coverage_matrix <- matrix(NA, nrow = n_sbc_sims, ncol = n_params)
     sbc_ranks_matrix <- matrix(NA, nrow = n_sbc_sims, ncol = n_params)
     colnames(coverage_matrix) <- param_names
     colnames(sbc_ranks_matrix) <- param_names

     # Fill matrices with successful results
     for (i in 1:n_sbc_sims) {
          if (results[[i]]$success) {
               coverage_matrix[i, ] <- results[[i]]$coverage
               sbc_ranks_matrix[i, ] <- results[[i]]$sbc_ranks
          }
     }

     # =============================================================================
     # CALCULATE SUMMARY STATISTICS
     # =============================================================================

     summary_df <- data.frame(
          parameter = param_names,
          n_valid = colSums(!is.na(coverage_matrix)),
          coverage_mean = colMeans(coverage_matrix, na.rm = TRUE),
          coverage_se = apply(coverage_matrix, 2, function(x) {
               sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
          }),
          sbc_rank_mean = colMeans(sbc_ranks_matrix, na.rm = TRUE),
          sbc_rank_sd = apply(sbc_ranks_matrix, 2, sd, na.rm = TRUE),
          sbc_rank_p25 = apply(sbc_ranks_matrix, 2, quantile, probs = 0.25, na.rm = TRUE),
          sbc_rank_p50 = apply(sbc_ranks_matrix, 2, quantile, probs = 0.50, na.rm = TRUE),
          sbc_rank_p75 = apply(sbc_ranks_matrix, 2, quantile, probs = 0.75, na.rm = TRUE),
          stringsAsFactors = FALSE
     )

     # Add coverage CI (Wilson score interval for proportions)
     # Use confidence level matching the coverage level from probs
     z <- qnorm((1 + coverage_level) / 2)  # Two-sided CI
     summary_df$coverage_ci_lower <- with(summary_df, {
          p <- coverage_mean
          n <- n_valid
          (p + z^2/(2*n) - z*sqrt(p*(1-p)/n + z^2/(4*n^2))) / (1 + z^2/n)
     })

     summary_df$coverage_ci_upper <- with(summary_df, {
          p <- coverage_mean
          n <- n_valid
          (p + z^2/(2*n) + z*sqrt(p*(1-p)/n + z^2/(4*n^2))) / (1 + z^2/n)
     })

     # Kolmogorov-Smirnov test for SBC uniformity
     # Note: Ties in ranks can occur when posterior samples equal true value
     # This generates warnings but doesn't invalidate the test
     summary_df$sbc_ks_pvalue <- apply(sbc_ranks_matrix, 2, function(x) {
          valid_ranks <- x[!is.na(x)]
          if (length(valid_ranks) > 1) {
               suppressWarnings(ks.test(valid_ranks, "punif")$p.value)
          } else {
               NA
          }
     })

     # Flag parameters with poor calibration
     # Allow 10% deviation from expected coverage level
     summary_df$coverage_ok <- abs(summary_df$coverage_mean - coverage_level) < 0.1
     summary_df$sbc_ok <- summary_df$sbc_ks_pvalue > 0.05

     # =============================================================================
     # COMPUTE OVERALL DIAGNOSTICS
     # =============================================================================

     overall_coverage <- mean(coverage_matrix, na.rm = TRUE)
     overall_coverage_se <- sd(as.vector(coverage_matrix), na.rm = TRUE) /
          sqrt(sum(!is.na(coverage_matrix)))

     # Test if all SBC ranks together are uniform
     all_ranks <- as.vector(sbc_ranks_matrix)
     all_ranks <- all_ranks[!is.na(all_ranks)]
     overall_sbc_ks <- if (length(all_ranks) > 1) {
          suppressWarnings(ks.test(all_ranks, "punif"))
     } else {
          list(statistic = NA, p.value = NA)
     }

     elapsed_time <- as.numeric(Sys.time() - start_time, units = "mins")

     # =============================================================================
     # PRINT SUMMARY
     # =============================================================================

     if (verbose) {
          cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
          cat("DIAGNOSTIC SUMMARY\n")
          cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")
          cat(sprintf("Successful SBC tests: %d / %d\n", n_successful, n_sbc_sims))
          cat(sprintf("Overall coverage: %.1f%% (target: %.1f%%)\n",
                      overall_coverage * 100, coverage_level * 100))
          cat(sprintf("Overall SBC KS p-value: %.3f %s\n",
                      overall_sbc_ks$p.value,
                      ifelse(overall_sbc_ks$p.value > 0.05, "✓", "⚠")))
          cat(sprintf("Parameters with good coverage (%.0f-%.0f%%): %d / %d\n",
                      (coverage_level - 0.1) * 100, min((coverage_level + 0.1) * 100, 100),
                      sum(summary_df$coverage_ok, na.rm = TRUE), n_params))
          cat(sprintf("Parameters with uniform SBC (p > 0.05): %d / %d\n",
                      sum(summary_df$sbc_ok, na.rm = TRUE), n_params))
          cat(sprintf("Total time: %.1f minutes\n", elapsed_time))
          cat(paste(rep("=", 60), collapse = ""), "\n\n", sep = "")

          # Show problematic parameters
          problem_params <- summary_df[!summary_df$coverage_ok | !summary_df$sbc_ok, ]
          if (nrow(problem_params) > 0) {
               cat("Parameters requiring attention:\n")
               for (i in 1:min(5, nrow(problem_params))) {
                    cat(sprintf("  - %s: Coverage=%.1f%%, SBC p=%.3f\n",
                                problem_params$parameter[i],
                                problem_params$coverage_mean[i] * 100,
                                problem_params$sbc_ks_pvalue[i]))
               }
               if (nrow(problem_params) > 5) {
                    cat(sprintf("  ... and %d more\n", nrow(problem_params) - 5))
               }
          }
     }

     # =============================================================================
     # SAVE RESULTS IF OUTPUT_DIR PROVIDED
     # =============================================================================

     results <- list(
          coverage = coverage_matrix,
          sbc_ranks = sbc_ranks_matrix,
          summary = summary_df,
          param_names = param_names,
          diagnostics = list(
               n_sbc_sims_requested = n_sbc_sims,
               n_sbc_sims_successful = n_successful,
               overall_coverage = overall_coverage,
               overall_coverage_se = overall_coverage_se,
               overall_sbc_ks_statistic = overall_sbc_ks$statistic,
               overall_sbc_ks_pvalue = overall_sbc_ks$p.value,
               elapsed_time_minutes = elapsed_time,
               timestamp = Sys.time(),
               npe_model_dir = npe_model_dir,
               n_npe_samples_per_test = n_npe_samples,
               quantiles_used = probs,
               coverage_level = coverage_level
          )
     )

     if (!is.null(output_dir)) {
          if (verbose) {
               cat("\n", paste(rep("-", 60), collapse = ""), "\n", sep = "")
               cat("SAVING RESULTS\n")
               cat(paste(rep("-", 60), collapse = ""), "\n", sep = "")
          }

          # Create output directory if it doesn't exist
          if (!dir.exists(output_dir)) {
               dir.create(output_dir, recursive = TRUE)
               if (verbose) cat("Created output directory: ", output_dir, "\n")
          }

          # Save coverage matrix as CSV
          coverage_file <- file.path(output_dir, "sbc_coverage.csv")
          write.csv(results$coverage, coverage_file, row.names = FALSE)
          if (verbose) cat("  ✓ Coverage matrix saved: sbc_coverage.csv\n")

          # Save SBC ranks matrix as CSV
          ranks_file <- file.path(output_dir, "sbc_ranks.csv")
          write.csv(results$sbc_ranks, ranks_file, row.names = FALSE)
          if (verbose) cat("  ✓ SBC ranks saved: sbc_ranks.csv\n")

          # Save summary statistics as CSV
          summary_file <- file.path(output_dir, "sbc_summary.csv")
          write.csv(results$summary, summary_file, row.names = FALSE)
          if (verbose) cat("  ✓ Summary statistics saved: sbc_summary.csv\n")

          # Save parameter names as text file
          params_file <- file.path(output_dir, "param_names.txt")
          writeLines(results$param_names, params_file)
          if (verbose) cat("  ✓ Parameter names saved: param_names.txt\n")

          # Save diagnostics as JSON
          diagnostics_file <- file.path(output_dir, "sbc_diagnostics.json")
          jsonlite::write_json(results$diagnostics, diagnostics_file, pretty = TRUE, auto_unbox = TRUE)
          if (verbose) cat("  ✓ Diagnostics saved: sbc_diagnostics.json\n")

          # Save full results as RDS for easy reloading
          rds_file <- file.path(output_dir, "sbc_results.rds")
          saveRDS(results, rds_file)
          if (verbose) cat("  ✓ Complete results saved: sbc_results.rds\n")

          if (verbose) {
               cat(paste(rep("-", 60), collapse = ""), "\n", sep = "")
               cat("All results saved to: ", output_dir, "\n")
          }
     }

     # =============================================================================
     # RETURN RESULTS
     # =============================================================================

     return(results)
}


# Example usage:
# results <- calc_npe_diagnostics(
#     n_sbc_sims = 100,
#     npe_model_dir = npe_model$output_dir,
#     PATHS = PATHS,
#     priors = priors_base,
#     config_base = config_base,
#     n_npe_samples = 1000,
#     probs = c(0.025, 0.25, 0.75, 0.975),
#     verbose = TRUE,
#     parallel = TRUE,
#     n_cores = 4
# )
