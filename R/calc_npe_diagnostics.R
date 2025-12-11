#' Calculate NPE Model Diagnostics with Simulation-Based Calibration (SBC)
#'
#' Performs comprehensive SBC diagnostics for NPE models trained with train_npe().
#' Tests calibration by sampling from priors, generating synthetic data, running
#' NPE inference, and checking coverage/uniformity of posterior distributions.
#'
#' @param n_sbc_sims Integer number of SBC test simulations to run
#' @param model NPE model object from train_npe() (required)
#' @param npe_model_dir Character path to NPE model directory (optional, extracted from model)
#' @param PATHS List containing project paths from get_paths()
#' @param priors Prior distributions object from get_location_priors()
#' @param config_base Base configuration from get_location_config()
#' @param param_names Character vector of parameter names (optional, extracted from model)
#' @param n_npe_samples Number of NPE posterior samples per test (default 100)
#' @param probs Numeric vector of quantiles for coverage (default c(0.025, 0.25, 0.75, 0.975))
#' @param seed_offset Integer offset for random seeds (default 1000)
#' @param output_dir Character path to save diagnostic files (required for plotting)
#' @param verbose Logical whether to print progress messages (default TRUE)
#' @param parallel Logical whether to run simulations in parallel (default FALSE)
#' @param n_cores Number of cores for parallel processing (default NULL = auto)
#'
#' @return List with SBC diagnostic results compatible with test_17 workflow:
#'   \item{coverage}{Matrix of coverage indicators (n_sbc_sims x n_params)}
#'   \item{sbc_ranks}{Matrix of SBC ranks (n_sbc_sims x n_params)}
#'   \item{summary}{Data frame with per-parameter summary statistics}
#'   \item{param_names}{Character vector of parameter names}
#'   \item{diagnostics}{List with overall diagnostic metrics for retraining decisions}
#'
#' @details
#' This function is specifically designed for the test_17 calibration workflow.
#' It uses estimate_npe_posterior() for inference and produces output files
#' compatible with plot_npe_convergence_status().
#'
#' @export
calc_npe_diagnostics <- function(
    n_sbc_sims,
    model,  # Required: NPE model object from train_npe()
    npe_model_dir = NULL,  # Optional: extracted from model if not provided
    PATHS,
    priors,
    config_base,
    param_names = NULL,  # Optional: extracted from model if not provided
    n_npe_samples = 100,
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
    if (is.null(model)) stop("model object is required (from train_npe)")
    if (!is.list(model) || is.null(model$model)) {
        stop("Invalid model object - must be output from train_npe()")
    }

    # Extract model directory if not provided
    if (is.null(npe_model_dir)) {
        if (!is.null(model$output_dir)) {
            npe_model_dir <- model$output_dir
        } else {
            stop("Model object must contain output_dir")
        }
    }

    # Extract parameter names from model if not provided
    if (is.null(param_names)) {
        if (!is.null(model$architecture$param_names)) {
            param_names <- model$architecture$param_names
        } else if (!is.null(names(model$architecture)) && "param_names" %in% names(model$architecture)) {
            param_names <- model$architecture[["param_names"]]
        } else {
            # Will extract from NPE results during first simulation
            param_names <- NULL
        }
    }

    # Validate probs argument
    if (!is.numeric(probs) || any(probs < 0) || any(probs > 1)) {
        stop("probs must be a numeric vector with values between 0 and 1")
    }
    if (length(probs) < 2) {
        stop("probs must contain at least 2 values for coverage calculation")
    }

    # Extract quantiles for coverage calculation
    coverage_lower <- min(probs)
    coverage_upper <- max(probs)
    coverage_level <- coverage_upper - coverage_lower

    # Check Python environment
    if (!reticulate::py_module_available("laser_cholera")) {
        stop("laser_cholera Python module not found. Use MOSAIC::use_mosaic_env() to activate.")
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
    # HELPER FUNCTION: MAP PARAMETERS
    # =============================================================================

    map_config_to_npe_params <- function(config, npe_param_names) {
        mapped_values <- list()

        location_params_base <- c(
            "beta_j0_env", "beta_j0_hum", "beta_j0_tot",
            "tau_i", "theta_j", "p_beta", "mu_j",
            "a_1_j", "a_2_j", "b_1_j", "b_2_j"
        )

        for (npe_param in npe_param_names) {
            has_location_suffix <- any(sapply(location_names, function(loc) {
                grepl(paste0("_", loc, "$"), npe_param)
            }))

            if (has_location_suffix) {
                for (loc in location_names) {
                    loc_suffix <- paste0("_", loc)
                    if (grepl(paste0(loc_suffix, "$"), npe_param)) {
                        base_param <- sub(loc_suffix, "", npe_param)
                        loc_idx <- which(location_names == loc)

                        if (base_param %in% names(config)) {
                            param_value <- config[[base_param]]
                            if (length(param_value) >= loc_idx) {
                                mapped_values[[npe_param]] <- param_value[loc_idx]
                            } else if (length(param_value) == 1) {
                                mapped_values[[npe_param]] <- param_value
                            }
                        }
                        break
                    }
                }
            } else {
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

            # Import laser_cholera for parallel workers (sequential uses parent scope)
            lc_local <- if (parallel) {
                reticulate::import("laser_cholera.metapop.model")
            } else {
                lc  # From parent scope
            }

            # Sample true parameters from prior
            theta_true <- sample_parameters(
                PATHS = PATHS,
                priors = priors,
                config = config_base,
                seed = seed_offset + sim_idx,
                verbose = FALSE
            )

            # Forward simulate with LASER
            if (verbose && !parallel) {
                cat(sprintf("[Test %d/%d] Running LASER forward simulation...\n", sim_idx, n_sbc_sims))
            }
            model_output <- lc_local$run_model(paramfile = theta_true, quiet = TRUE)

            # Extract simulated data in NPE format
            y_sim <- get_npe_simulated_data(model_output, verbose = FALSE)

            # Run NPE inference using estimate_npe_posterior (matches test_17 workflow)
            if (verbose && !parallel) {
                cat(sprintf("[Test %d/%d] Running NPE inference...\n", sim_idx, n_sbc_sims))
            }

            npe_result <- estimate_npe_posterior(
                model = model,
                observed_data = y_sim,
                n_samples = n_npe_samples,
                quantiles = probs,
                return_log_probs = FALSE,  # Not needed for diagnostics
                output_dir = NULL,
                verbose = FALSE
            )

            # Extract NPE results
            theta_npe_quantiles <- npe_result$quantiles
            theta_npe_samples <- npe_result$samples

            # Determine parameter names (prioritize NPE results)
            result_param_names <- NULL
            if (!is.null(npe_result$param_names) && length(npe_result$param_names) > 0) {
                result_param_names <- npe_result$param_names
            } else if (!is.null(colnames(theta_npe_samples))) {
                result_param_names <- colnames(theta_npe_samples)
            } else if (!is.null(param_names)) {
                result_param_names <- param_names
            }

            if (is.null(result_param_names) || length(result_param_names) == 0) {
                stop("Could not determine parameter names from NPE results or inputs")
            }

            # Map config parameters to NPE parameter format
            true_values <- map_config_to_npe_params(theta_true, result_param_names)

            # Calculate coverage and SBC ranks
            coverage <- numeric(length(result_param_names))
            sbc_ranks <- numeric(length(result_param_names))

            for (i in seq_along(result_param_names)) {
                param <- result_param_names[i]

                if (param %in% names(true_values)) {
                    true_val <- true_values[[param]]

                    # Coverage check
                    param_row <- which(theta_npe_quantiles$parameter == param)
                    if (length(param_row) > 0) {
                        # Generate expected column names to match quantiles dataframe
                        # Use sprintf %s to match the column naming in .calculate_quantiles()
                        lower_col <- sprintf("q%s", coverage_lower)
                        upper_col <- sprintf("q%s", coverage_upper)

                        # Check if these columns exist in the quantiles dataframe
                        if (lower_col %in% names(theta_npe_quantiles) && upper_col %in% names(theta_npe_quantiles)) {
                            lower_val <- theta_npe_quantiles[[lower_col]][param_row[1]]
                            upper_val <- theta_npe_quantiles[[upper_col]][param_row[1]]

                            if (!is.na(lower_val) && !is.na(upper_val)) {
                                coverage[i] <- as.numeric(true_val >= lower_val & true_val <= upper_val)
                            } else {
                                coverage[i] <- NA
                            }
                        } else {
                            coverage[i] <- NA
                            if (verbose && !parallel) {
                                available_cols <- paste(names(theta_npe_quantiles), collapse = ", ")
                                cat(sprintf("  Warning: Expected columns %s, %s not found. Available: %s\n",
                                           lower_col, upper_col, available_cols))
                            }
                        }
                    } else {
                        coverage[i] <- NA
                        if (verbose && !parallel) {
                            cat(sprintf("  Warning: Parameter %s not found in quantiles\n", param))
                        }
                    }

                    # SBC rank calculation
                    if (param %in% colnames(theta_npe_samples)) {
                        param_samples <- theta_npe_samples[, param]
                        if (length(param_samples) > 0 && !all(is.na(param_samples))) {
                            sbc_ranks[i] <- mean(param_samples < true_val, na.rm = TRUE)
                        } else {
                            sbc_ranks[i] <- NA
                            if (verbose && !parallel) {
                                cat(sprintf("  Warning: No valid samples for parameter %s\n", param))
                            }
                        }
                    } else {
                        sbc_ranks[i] <- NA
                        if (verbose && !parallel) {
                            available_params <- paste(head(colnames(theta_npe_samples), 5), collapse = ", ")
                            if (ncol(theta_npe_samples) > 5) available_params <- paste0(available_params, ", ...")
                            cat(sprintf("  Warning: Parameter '%s' not found. Available: %s\n",
                                       param, available_params))
                        }
                    }

                } else {
                    coverage[i] <- NA
                    sbc_ranks[i] <- NA
                    if (verbose && !parallel) {
                        cat(sprintf("  Warning: Parameter %s not found in true values\n", param))
                    }
                }
            }

            names(coverage) <- result_param_names
            names(sbc_ranks) <- result_param_names

            if (verbose && !parallel) {
                cat(sprintf("[Test %d/%d] Complete. Coverage: %.1f%%\n",
                           sim_idx, n_sbc_sims, mean(coverage, na.rm = TRUE) * 100))
            }

            return(list(
                success = TRUE,
                coverage = coverage,
                sbc_ranks = sbc_ranks,
                param_names = result_param_names
            ))

        }, error = function(e) {
            error_msg <- paste(e$message, collapse = "\n")

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

        # CRITICAL: Set threading environment variables to prevent fork issues
        Sys.setenv(
            TBB_NUM_THREADS = "1",
            NUMBA_NUM_THREADS = "1",
            OMP_NUM_THREADS = "1",
            MKL_NUM_THREADS = "1",
            OPENBLAS_NUM_THREADS = "1"
        )

        cl <- parallel::makeCluster(n_cores)
        parallel::clusterEvalQ(cl, {
            # Set library path for VM user installation
            .libPaths(c('~/R/library', .libPaths()))

            library(MOSAIC)  # Auto-attaches r-mosaic via .onAttach() hook
            library(reticulate)
            library(dplyr)
            library(tidyr)

            # CRITICAL: Limit each worker to single-threaded operations
            MOSAIC:::.mosaic_set_blas_threads(1L)

            # TBB/Numba threading
            Sys.setenv(
                TBB_NUM_THREADS = "1",
                NUMBA_NUM_THREADS = "1",
                OMP_NUM_THREADS = "1",
                MKL_NUM_THREADS = "1",
                OPENBLAS_NUM_THREADS = "1"
            )
        })

        # Export required objects to parallel workers
        parallel::clusterExport(cl, c(
            # Core functions
            "sample_parameters", "get_npe_simulated_data", "estimate_npe_posterior",
            # Data objects
            "PATHS", "priors", "config_base", "model",
            "npe_model_dir", "n_npe_samples", "probs",
            # Parameters
            "seed_offset", "n_sbc_sims", "location_names", "n_locations",
            "coverage_lower", "coverage_upper", "param_names",
            # Internal functions
            "map_config_to_npe_params", "run_single_simulation",
            # Control variables
            "verbose", "parallel"
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
    final_param_names <- results[[first_success]]$param_names
    n_params <- length(final_param_names)

    # Initialize result matrices
    coverage_matrix <- matrix(NA, nrow = n_sbc_sims, ncol = n_params)
    sbc_ranks_matrix <- matrix(NA, nrow = n_sbc_sims, ncol = n_params)
    colnames(coverage_matrix) <- final_param_names
    colnames(sbc_ranks_matrix) <- final_param_names

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
        parameter = final_param_names,
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

    # Add coverage confidence intervals
    z <- qnorm((1 + coverage_level) / 2)
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
    summary_df$sbc_ks_pvalue <- apply(sbc_ranks_matrix, 2, function(x) {
        valid_ranks <- x[!is.na(x)]
        if (length(valid_ranks) > 1) {
            suppressWarnings(ks.test(valid_ranks, "punif")$p.value)
        } else {
            NA
        }
    })

    # Flag parameters with calibration issues
    summary_df$coverage_ok <- abs(summary_df$coverage_mean - coverage_level) < 0.1
    summary_df$sbc_ok <- summary_df$sbc_ks_pvalue > 0.05

    # =============================================================================
    # COMPUTE OVERALL DIAGNOSTICS
    # =============================================================================

    overall_coverage <- mean(coverage_matrix, na.rm = TRUE)
    overall_coverage_se <- sd(as.vector(coverage_matrix), na.rm = TRUE) /
        sqrt(sum(!is.na(coverage_matrix)))

    # Overall SBC KS test
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
    # PREPARE RESULTS
    # =============================================================================

    # Calculate parameter-wise coverage for plot compatibility
    # Calculate 50% coverage (ranks between 0.25 and 0.75) and 80% coverage (ranks between 0.1 and 0.9)
    coverage_50_param <- apply(sbc_ranks_matrix, 2, function(ranks) {
        mean((ranks >= 0.25 & ranks <= 0.75), na.rm = TRUE)
    })

    coverage_80_param <- apply(sbc_ranks_matrix, 2, function(ranks) {
        mean((ranks >= 0.1 & ranks <= 0.9), na.rm = TRUE)
    })

    results <- list(
        coverage = list(
            coverage_50 = coverage_50_param,
            coverage_80 = coverage_80_param
        ),
        coverage_matrix = coverage_matrix,  # Keep original matrix for advanced analysis
        sbc_ranks = sbc_ranks_matrix,
        summary = summary_df,
        param_names = final_param_names,
        sbc = list(
            ks_pvalues = summary_df$sbc_ks_pvalue,
            coverage_mean = summary_df$coverage_mean,
            coverage_ok = summary_df$coverage_ok,
            sbc_ok = summary_df$sbc_ok
        ),
        diagnostics = list(
            n_sbc_sims_requested = n_sbc_sims,
            n_sbc_sims_successful = n_successful,
            overall_coverage = overall_coverage,
            overall_coverage_se = overall_coverage_se,
            overall_sbc_ks_statistic = overall_sbc_ks$statistic,
            overall_sbc_ks_pvalue = overall_sbc_ks$p.value,
            elapsed_time_minutes = elapsed_time,
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            npe_model_dir = npe_model_dir,
            n_npe_samples_per_test = n_npe_samples,
            quantiles_used = probs,
            coverage_level = coverage_level
        )
    )

    # =============================================================================
    # SAVE RESULTS (TRADITIONAL SBC FORMAT)
    # =============================================================================

    if (!is.null(output_dir)) {
        if (verbose) {
            cat("\n", paste(rep("-", 60), collapse = ""), "\n", sep = "")
            cat("SAVING RESULTS\n")
            cat(paste(rep("-", 60), collapse = ""), "\n", sep = "")
        }

        # Create output directory
        if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
            if (verbose) cat("Created output directory: ", output_dir, "\n")
        }

        # Save all traditional SBC output files

        # 1. Coverage matrix
        write.csv(results$coverage, file.path(output_dir, "sbc_coverage.csv"), row.names = FALSE)
        if (verbose) cat("  ✓ Coverage matrix saved: sbc_coverage.csv\n")

        # 2. SBC ranks matrix
        write.csv(results$sbc_ranks, file.path(output_dir, "sbc_ranks.csv"), row.names = FALSE)
        if (verbose) cat("  ✓ SBC ranks saved: sbc_ranks.csv\n")

        # 3. Summary statistics
        write.csv(results$summary, file.path(output_dir, "sbc_summary.csv"), row.names = FALSE)
        if (verbose) cat("  ✓ Summary statistics saved: sbc_summary.csv\n")

        # 4. Parameter names
        writeLines(results$param_names, file.path(output_dir, "param_names.txt"))
        if (verbose) cat("  ✓ Parameter names saved: param_names.txt\n")

        # 5. Diagnostics JSON
        jsonlite::write_json(results$diagnostics, file.path(output_dir, "sbc_diagnostics.json"),
                           pretty = TRUE, auto_unbox = TRUE)
        if (verbose) cat("  ✓ Diagnostics saved: sbc_diagnostics.json\n")

        # 6. Complete results RDS
        saveRDS(results, file.path(output_dir, "sbc_results.rds"))
        if (verbose) cat("  ✓ Complete results saved: sbc_results.rds\n")

        if (verbose) {
            cat(paste(rep("-", 60), collapse = ""), "\n", sep = "")
            cat("All results saved to: ", output_dir, "\n")
        }
    }

    return(results)
}

#' Extract NPE-formatted simulated data from LASER model output
#'
#' Converts LASER model output to the data frame format expected by NPE posterior estimation.
#' Handles both R list structure and Python object structure from laser_cholera.
#'
#' @param laser_result Output from laser_cholera run_model
#' @param verbose Logical whether to print messages
#'
#' @return Data frame with columns j (location), t (time), cases
#' @keywords internal
get_npe_simulated_data <- function(laser_result, verbose = FALSE) {

    # Helper function for logging
    log_msg <- function(msg, ...) {
        if (verbose) {
            message(sprintf(msg, ...))
        }
    }

    if (is.null(laser_result)) {
        stop("laser_result cannot be NULL")
    }

    # =============================================================================
    # EXTRACT CASES DATA
    # =============================================================================

    cases_data <- NULL

    # Check if this is a Python Model object first
    if (inherits(laser_result, "python.builtin.object")) {
        # Handle Python laser_cholera Model object
        tryCatch({
            # The Model object has a results attribute which is a dict-like object
            if (reticulate::py_has_attr(laser_result, "results")) {
                # Get the results object
                results_obj <- laser_result$results

                # Access expected_cases from results
                # Try different access methods for Python dict-like objects
                if (!is.null(results_obj$expected_cases)) {
                    cases_data <- results_obj$expected_cases
                } else if (!is.null(results_obj[["expected_cases"]])) {
                    cases_data <- results_obj[["expected_cases"]]
                } else {
                    # Try using Python getitem
                    cases_data <- reticulate::py_get_item(results_obj, "expected_cases")
                }
            }
        }, error = function(e) {
            # Try alternative access patterns
            tryCatch({
                # Some versions might have direct access
                cases_data <- laser_result$expected_cases
            }, error = function(e2) {
                stop("Cannot access 'expected_cases' in Python Model object: ", e$message)
            })
        })
    } else {
        # Handle R list/object structure
        if (!is.null(laser_result$expected_cases)) {
            cases_data <- laser_result$expected_cases
        } else if (!is.null(laser_result$results$expected_cases)) {
            cases_data <- laser_result$results$expected_cases
        } else if (!is.null(laser_result[["results"]][["expected_cases"]])) {
            cases_data <- laser_result[["results"]][["expected_cases"]]
        }
    }

    if (is.null(cases_data)) {
        # Provide more debugging information
        if (inherits(laser_result, "python.builtin.object")) {
            attrs <- tryCatch({
                paste(reticulate::py_list_attributes(laser_result), collapse = ", ")
            }, error = function(e) "could not list attributes")
            stop("laser_result (Python Model) must contain 'results$expected_cases'. Available attributes: ", attrs)
        } else {
            stop("laser_result must contain 'expected_cases' (tried multiple access patterns). Structure: ",
                 paste(names(laser_result), collapse = ", "))
        }
    }

    # Convert Python numpy array to R if needed
    if (inherits(cases_data, "python.builtin.object")) {
        cases_data <- reticulate::py_to_r(cases_data)
    }

    # =============================================================================
    # PROCESS CASES DATA INTO STANDARD FORMAT
    # =============================================================================

    # Determine data dimensions
    if (is.vector(cases_data) && !is.matrix(cases_data)) {
        # Single location case - convert to matrix (1 row, n columns)
        log_msg("Processing single location simulation data")
        cases_matrix <- matrix(cases_data, nrow = 1)
        n_locations <- 1
        n_timesteps <- length(cases_data)
    } else if (is.matrix(cases_data)) {
        cases_matrix <- cases_data
        n_locations <- nrow(cases_data)
        n_timesteps <- ncol(cases_data)
        log_msg("Processing %d location(s) with %d time points", n_locations, n_timesteps)
    } else {
        stop("expected_cases must be a vector or matrix")
    }

    # =============================================================================
    # EXTRACT LOCATION IDENTIFIERS
    # =============================================================================

    # Extract location identifiers from model parameters
    location_codes <- tryCatch({
        # Try to extract location names from model params
        params <- if (inherits(laser_result, "python.builtin.object") && reticulate::py_has_attr(laser_result, "params")) {
            laser_result$params
        } else if (!is.null(laser_result$params)) {
            laser_result$params
        } else {
            NULL
        }

        if (!is.null(params)) {
            # Try different parameter name fields
            codes <- NULL
            for (field in c("location_name", "iso_code", "location_code")) {
                if (!is.null(params[[field]])) {
                    codes <- as.character(params[[field]])
                    break
                }
            }

            # Validate location codes
            if (!is.null(codes) && length(codes) == n_locations) {
                codes
            } else {
                NULL  # Will use default indices
            }
        } else {
            NULL  # Will use default indices
        }
    }, error = function(e) {
        if (verbose) log_msg("Could not extract location names: %s", e$message)
        NULL
    })

    # Use default indices if location extraction failed
    if (is.null(location_codes)) {
        location_codes <- as.character(seq_len(n_locations))
        log_msg("Using default location indices: %s", paste(location_codes, collapse = ", "))
    } else {
        log_msg("Using extracted location codes: %s", paste(location_codes, collapse = ", "))
    }

    # =============================================================================
    # CREATE LONG-FORMAT DATA FRAME
    # =============================================================================

    log_msg("Converting to long format...")

    # Initialize list to store each location's data
    result_list <- list()

    for (i in seq_len(n_locations)) {
        location_data <- data.frame(
            j = location_codes[i],
            t = seq_len(n_timesteps),
            cases = as.numeric(cases_matrix[i, ]),
            stringsAsFactors = FALSE
        )
        result_list[[i]] <- location_data
    }

    # Combine all locations
    simulated_data <- do.call(rbind, result_list)

    # Ensure cases are non-negative
    simulated_data$cases[simulated_data$cases < 0] <- 0

    # Reset row names
    rownames(simulated_data) <- NULL

    if (verbose) {
        total_cases <- sum(simulated_data$cases, na.rm = TRUE)
        message(sprintf("  Created data frame: %d rows, %.0f total cases across %d location(s)",
                       nrow(simulated_data), total_cases, n_locations))
    }

    return(simulated_data)
}