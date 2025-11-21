#' Plot Stochastic Model Fit with Parameter and Stochastic Uncertainty
#'
#' Runs multiple simulations with both parameter uncertainty (different parameter sets)
#' and stochastic uncertainty (different random seeds) to create timeseries plots
#' showing median predictions with comprehensive uncertainty envelopes.
#'
#' @param configs List of pre-sampled configuration objects. If provided, these
#'   configurations will be used directly. Must be a list of valid config objects.
#' @param config Base configuration object for internal parameter sampling. Required
#'   if configs is NULL and parameter_seeds is provided.
#' @param parameter_seeds Numeric vector of seeds for parameter sampling. Each seed
#'   will generate a different parameter set using sample_parameters(). Required
#'   if configs is NULL.
#' @param parameter_weights Numeric vector of weights for each parameter set, same length
#'   as configs or parameter_seeds. Weights are used when calculating median and quantiles
#'   across parameter sets. If NULL, all parameter sets are weighted equally. Weights are
#'   automatically normalized to sum to 1.
#' @param n_simulations_per_config Integer specifying number of stochastic simulations
#'   to run for each parameter configuration. Default is 30.
#' @param PATHS List of paths as returned by get_paths(). Required if using
#'   parameter_seeds for internal sampling.
#' @param priors Priors object for parameter sampling. Required if using
#'   parameter_seeds for internal sampling.
#' @param sampling_args Named list of additional arguments to pass to sample_parameters().
#'   For example: list(sample_tau_i = FALSE, sample_mobility_gamma = FALSE).
#'   Default is empty list.
#' @param output_dir Character string specifying the directory where plots should
#'   be saved. Directory will be created if it doesn't exist.
#' @param envelope_quantiles Numeric vector specifying the quantiles for confidence
#'   intervals. Default is c(0.025, 0.25, 0.75, 0.975) for 50% and 95% CIs.
#'   Must have an even number of elements to form pairs of (lower, upper) bounds.
#' @param save_predictions Logical indicating whether to save prediction data to CSV files.
#'   Default is FALSE. If TRUE, saves predictions_ensemble_{location}.csv for each location.
#' @param parallel Logical indicating whether to use parallel computation. Default is FALSE.
#'   When TRUE, simulations are run in parallel across multiple cores.
#' @param n_cores Integer specifying number of cores to use for parallel computation.
#'   Default is NULL (uses detectCores() - 1). Only used when parallel = TRUE.
#' @param root_dir Character string specifying the root directory for MOSAIC project.
#'   Required when parallel = TRUE. Workers need this to initialize PATHS correctly.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#' @param plot_decomposed Logical indicating whether to create additional plots
#'   showing decomposed uncertainty. Default is FALSE.
#'
#' @return Invisibly returns a list containing:
#'   \itemize{
#'     \item \code{individual}: Named list of plots for each location
#'     \item \code{cases_faceted}: Faceted plot of cases by location (if n_locations > 1)
#'     \item \code{deaths_faceted}: Faceted plot of deaths by location (if n_locations > 1)
#'     \item \code{simulation_stats}: Detailed statistics from all simulations
#'     \item \code{param_configs}: List of parameter configurations used
#'   }
#'
#' @details
#' This function handles two types of uncertainty:
#' \enumerate{
#'   \item \strong{Parameter uncertainty}: Different parameter sets from calibration
#'   \item \strong{Stochastic uncertainty}: Random variation within each parameter set
#' }
#'
#' The function can operate in two modes:
#' \itemize{
#'   \item \strong{Direct mode}: Provide pre-sampled configs via the configs parameter
#'   \item \strong{Sampling mode}: Provide config + parameter_seeds for internal sampling
#' }
#'
#' The total number of simulations = length(configs) × n_simulations_per_config
#'
#' @examples
#' \dontrun{
#' # Mode 1: Using pre-sampled configurations
#' configs <- list(config1, config2, config3)
#' plots <- plot_model_fit_stochastic_param(
#'     configs = configs,
#'     n_simulations_per_config = 20,
#'     output_dir = "output/plots",
#'     save_predictions = TRUE
#' )
#'
#' # Mode 2: Using parameter seeds (typical calibration workflow)
#' best_seeds <- c(123, 456, 789)  # Top seeds from calibration
#' plots <- plot_model_fit_stochastic_param(
#'     config = base_config,
#'     parameter_seeds = best_seeds,
#'     n_simulations_per_config = 20,
#'     PATHS = PATHS,
#'     priors = priors,
#'     sampling_args = list(sample_tau_i = FALSE),
#'     output_dir = "output/plots",
#'     save_predictions = TRUE  # Save predictions to CSV
#' )
#' # Saves: predictions_ensemble_ETH.csv (or other location names)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_point geom_line facet_grid facet_wrap scale_color_manual scale_fill_manual scale_alpha_manual scale_y_continuous scale_x_date theme_minimal theme element_text element_blank labs ggsave
#' @importFrom dplyr filter mutate
#' @importFrom scales comma
#' @importFrom reticulate import
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom parallel makeCluster stopCluster clusterEvalQ clusterExport clusterCall detectCores
#' @importFrom pbapply pblapply
plot_model_fit_stochastic_param <- function(
    configs = NULL,
    config = NULL,
    parameter_seeds = NULL,
    parameter_weights = NULL,
    n_simulations_per_config = 30,
    PATHS = NULL,
    priors = NULL,
    sampling_args = list(),
    output_dir,
    envelope_quantiles = c(0.025, 0.25, 0.75, 0.975),
    save_predictions = FALSE,
    parallel = FALSE,
    n_cores = NULL,
    root_dir = NULL,
    verbose = TRUE,
    plot_decomposed = FALSE) {

    # ============================================================================
    # Input validation and mode determination
    # ============================================================================

    if (missing(output_dir) || is.null(output_dir)) {
        stop("output_dir is required")
    }

    # Determine which mode we're operating in
    if (!is.null(configs)) {
        # Mode 1: Direct configs provided
        if (verbose) message("Using ", length(configs), " provided configurations")
        param_configs <- configs
        n_param_sets <- length(configs)

    } else if (!is.null(config) && !is.null(parameter_seeds)) {
        # Mode 2: Sample parameters internally
        if (is.null(priors)) {
            stop("priors required when using parameter_seeds")
        }
        if (is.null(PATHS)) {
            stop("PATHS required when using parameter_seeds")
        }

        # Filter to non-zero weights if weights provided
        if (!is.null(parameter_weights)) {
            if (length(parameter_weights) != length(parameter_seeds)) {
                stop("parameter_weights must have same length as parameter_seeds")
            }

            # Identify non-zero weights
            nonzero_idx <- parameter_weights > 0
            n_total <- length(parameter_seeds)
            n_zero <- sum(!nonzero_idx)
            n_nonzero <- sum(nonzero_idx)

            if (verbose && n_zero > 0) {
                message("\n=== Filtering to Non-Zero Weighted Parameter Sets ===")
                message("  Total seeds provided: ", n_total)
                message("  Zero-weight seeds (skipped): ", n_zero, " (", round(100*n_zero/n_total, 1), "%)")
                message("  Non-zero weighted seeds: ", n_nonzero, " (", round(100*n_nonzero/n_total, 1), "%)")
            }

            # Filter both seeds and weights
            parameter_seeds <- parameter_seeds[nonzero_idx]
            parameter_weights <- parameter_weights[nonzero_idx]

            if (length(parameter_seeds) == 0) {
                stop("No parameter sets with non-zero weights")
            }
        }

        if (verbose) {
            message("\n=== Parameter Sampling ===")
            message("  Parameter sets to sample: ", length(parameter_seeds))
            message("  Stochastic sims per set: ", n_simulations_per_config)
            message("  Total simulations: ", length(parameter_seeds) * n_simulations_per_config)
        }

        # Sample parameter configurations with progress bar
        param_configs <- list()
        if (verbose) {
            pb <- txtProgressBar(min = 0, max = length(parameter_seeds), style = 1,
                                char = "█")
        }

        for (i in seq_along(parameter_seeds)) {
            if (verbose) setTxtProgressBar(pb, i)

            # Sample parameters using sample_args
            param_configs[[i]] <- tryCatch({
                sample_parameters(
                    PATHS = PATHS,
                    priors = priors,
                    config = config,
                    seed = parameter_seeds[i],
                    sample_args = sampling_args,
                    verbose = FALSE
                )
            }, error = function(e) {
                warning("Failed to sample parameters with seed ", parameter_seeds[i], ": ", e$message)
                NULL
            })
        }

        # Close progress bar
        if (verbose) {
            close(pb)
            message("  ✓ Sampled ", length(parameter_seeds), " parameter configurations")
        }

        # Remove any failed samplings
        param_configs <- Filter(Negate(is.null), param_configs)
        n_param_sets <- length(param_configs)

        if (n_param_sets == 0) {
            stop("All parameter sampling attempts failed")
        }

        if (n_param_sets < length(parameter_seeds) && verbose) {
            message("Warning: ", length(parameter_seeds) - n_param_sets,
                   " parameter sets failed to sample")
        }

    } else {
        stop("Must provide either 'configs' or 'config + parameter_seeds + priors + PATHS'")
    }

    # Validate and process parameter weights
    if (!is.null(parameter_weights)) {
        if (length(parameter_weights) != n_param_sets) {
            stop("parameter_weights must have the same length as configs or parameter_seeds")
        }
        if (any(!is.finite(parameter_weights)) || any(parameter_weights < 0)) {
            stop("parameter_weights must be non-negative finite values")
        }
        # Normalize weights to sum to 1
        parameter_weights <- parameter_weights / sum(parameter_weights)
        if (verbose) {
            message("Using weighted statistics with weights ranging from ",
                   round(min(parameter_weights), 4), " to ",
                   round(max(parameter_weights), 4))
        }
    } else {
        # Equal weights if not provided
        parameter_weights <- rep(1/n_param_sets, n_param_sets)
    }

    # Validate envelope quantiles
    if (length(envelope_quantiles) %% 2 != 0) {
        stop("envelope_quantiles must have an even number of elements to form CI pairs")
    }
    if (any(!is.finite(envelope_quantiles)) || any(envelope_quantiles < 0) || any(envelope_quantiles > 1)) {
        stop("envelope_quantiles must be finite values between 0 and 1")
    }
    if (any(diff(envelope_quantiles) <= 0)) {
        stop("envelope_quantiles must be in ascending order")
    }

    # Validate parallel parameters
    if (parallel && is.null(root_dir)) {
        stop("root_dir is required when parallel = TRUE")
    }

    if (parallel && !is.null(n_cores) && n_cores < 1) {
        stop("n_cores must be at least 1")
    }

    # Create output directory
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        if (verbose) message("Created output directory: ", output_dir)
    }

    # ============================================================================
    # Extract metadata from first config
    # ============================================================================

    first_config <- param_configs[[1]]
    obs_cases <- first_config$reported_cases
    obs_deaths <- first_config$reported_deaths
    location_names <- first_config$location_name
    date_start <- first_config$date_start
    date_stop <- first_config$date_stop

    if (is.null(obs_cases) || is.null(obs_deaths)) {
        stop("configs must contain reported_cases and reported_deaths")
    }

    if (is.null(location_names)) {
        n_locations <- if(is.matrix(obs_cases)) nrow(obs_cases) else 1
        location_names <- if(n_locations == 1) "Location" else paste0("Location_", 1:n_locations)
    }

    n_locations <- length(location_names)
    n_time_points <- if(is.matrix(obs_cases)) ncol(obs_cases) else length(obs_cases)
    total_simulations <- n_param_sets * n_simulations_per_config

    if (verbose) {
        message("\n=== Simulation Configuration ===")
        message("  Parameter sets: ", n_param_sets)
        message("  Stochastic runs per set: ", n_simulations_per_config)
        message("  Total simulations: ", total_simulations)
        message("  Locations: ", n_locations, " (", paste(location_names, collapse = ", "), ")")
        message("  Time points: ", n_time_points)
    }

    # ============================================================================
    # Run all simulations (Parallel or Sequential)
    # ============================================================================

    if (verbose) message("\n=== Running Simulations ===")

    # Arrays to store results: [location, time, param_set, stochastic_run]
    cases_array <- array(NA, dim = c(n_locations, n_time_points, n_param_sets, n_simulations_per_config))
    deaths_array <- array(NA, dim = c(n_locations, n_time_points, n_param_sets, n_simulations_per_config))

    # Worker function (must be self-contained for parallel execution)
    run_param_stoch_simulation <- function(task_info, param_configs_list) {
        param_idx <- task_info$param_idx
        stoch_idx <- task_info$stoch_idx

        tryCatch({
            # Get laser_cholera from worker global environment
            # Parallel mode: lc exists in worker global environment (imported during init)
            # Sequential mode: import here
            if (!exists("lc", where = .GlobalEnv, inherits = FALSE)) {
                lc <- reticulate::import("laser_cholera.metapop.model")
            } else {
                lc <- get("lc", envir = .GlobalEnv)
            }

            # Get parameter configuration
            param_config <- param_configs_list[[param_idx]]

            # Generate stochastic seed
            stochastic_seed <- (param_idx * 1000) + stoch_idx
            param_config$seed <- stochastic_seed

            # Run model
            model <- lc$run_model(paramfile = param_config, quiet = TRUE)

            # Extract results before cleanup
            result <- list(
                param_idx = param_idx,
                stoch_idx = stoch_idx,
                expected_cases = model$results$expected_cases,
                disease_deaths = model$results$disease_deaths,
                success = TRUE
            )

            # Cleanup Python objects to prevent accumulation across tasks
            gc(verbose = FALSE)
            reticulate::import("gc")$collect()

            # Return results
            result

        }, error = function(e) {
            list(
                param_idx = param_idx,
                stoch_idx = stoch_idx,
                success = FALSE,
                error = as.character(e)
            )
        })
    }

    # Execute simulations (parallel or sequential)
    if (parallel) {
        # ========================================================================
        # PARALLEL EXECUTION
        # ========================================================================

        # Create flattened task list
        task_list <- expand.grid(
            param_idx = 1:n_param_sets,
            stoch_idx = 1:n_simulations_per_config
        )

        # Determine number of cores
        n_cores_use <- if (is.null(n_cores)) {
            max(1, parallel::detectCores() - 1)
        } else {
            n_cores
        }

        if (verbose) {
            message("Setting up parallel cluster with ", n_cores_use, " cores...")
        }

        # Create cluster
        cl <- parallel::makeCluster(n_cores_use, type = "PSOCK")

        # Ensure cluster is stopped on exit
        on.exit(parallel::stopCluster(cl), add = TRUE)

        # Setup workers
        parallel::clusterEvalQ(cl, {
            # Set library path for VM user installation
            .libPaths(c('~/R/library', .libPaths()))

            library(MOSAIC)
            library(reticulate)

            # Import laser-cholera ONCE per worker (not per task)
            # This avoids repeated import overhead (~200-500ms per import on cluster)
            lc <- reticulate::import("laser_cholera.metapop.model")
            assign("lc", lc, envir = .GlobalEnv)

            NULL
        })

        # Set root directory on workers
        if (!is.null(root_dir)) {
            parallel::clusterCall(cl, function(rd) {
                MOSAIC::set_root_directory(rd)
                MOSAIC::get_paths()
            }, root_dir)
        }

        # Export param_configs to workers
        parallel::clusterExport(cl, c("param_configs"), envir = environment())

        # Run parallel simulations
        if (verbose) {
            message("Running ", total_simulations, " simulations on ", n_cores_use, " cores...")
            # Simple progress bar with block character (no color codes)
            # style = 1: Shows elapsed and remaining time with percentage
            pbo <- pbapply::pboptions(type = "timer", char = "█", style = 1)
            on.exit(pbapply::pboptions(pbo), add = TRUE)
        } else {
            # Explicitly disable progress bar for non-verbose mode (prevents cluster hangs)
            pbo <- pbapply::pboptions(type = "none")
            on.exit(pbapply::pboptions(pbo), add = TRUE)
        }

        results_list <- pbapply::pblapply(
            split(task_list, seq(nrow(task_list))),
            function(row) run_param_stoch_simulation(row, param_configs),
            cl = cl
        )

        # Reconstruct arrays from parallel results
        for (result in results_list) {
            if (result$success) {
                p <- result$param_idx
                s <- result$stoch_idx

                if (is.matrix(result$expected_cases)) {
                    cases_array[,,p,s] <- result$expected_cases
                    deaths_array[,,p,s] <- result$disease_deaths
                } else {
                    cases_array[1,,p,s] <- result$expected_cases
                    deaths_array[1,,p,s] <- result$disease_deaths
                }
            }
        }

    } else {
        # ========================================================================
        # SEQUENTIAL EXECUTION
        # ========================================================================

        if (verbose) {
            message("Running ", total_simulations, " simulations sequentially...")
            # Simple progress bar with block character (no color codes)
            # style = 1: Shows elapsed and remaining time with percentage
            pbo <- pbapply::pboptions(type = "timer", char = "█", style = 1)
            on.exit(pbapply::pboptions(pbo), add = TRUE)
        } else {
            # Explicitly disable progress bar for non-verbose mode (prevents cluster hangs)
            pbo <- pbapply::pboptions(type = "none")
            on.exit(pbapply::pboptions(pbo), add = TRUE)
        }

        # Create flattened task list for sequential execution with progress bar
        task_list <- expand.grid(
            param_idx = 1:n_param_sets,
            stoch_idx = 1:n_simulations_per_config
        )

        results_list <- pbapply::pblapply(
            split(task_list, seq(nrow(task_list))),
            function(row) run_param_stoch_simulation(row, param_configs)
        )

        # Reconstruct arrays from sequential results
        for (result in results_list) {
            if (result$success) {
                p <- result$param_idx
                s <- result$stoch_idx

                if (is.matrix(result$expected_cases)) {
                    cases_array[,,p,s] <- result$expected_cases
                    deaths_array[,,p,s] <- result$disease_deaths
                } else {
                    cases_array[1,,p,s] <- result$expected_cases
                    deaths_array[1,,p,s] <- result$disease_deaths
                }
            }
        }
    }

    # ============================================================================
    # Calculate statistics
    # ============================================================================

    if (verbose) message("\n=== Calculating Statistics ===")

    # Helper function for weighted quantiles
    weighted_quantile <- function(x, weights, probs) {
        # Remove NAs
        na_idx <- is.na(x) | is.na(weights)
        x <- x[!na_idx]
        weights <- weights[!na_idx]

        if (length(x) == 0) return(NA)
        if (all(weights == 0)) return(NA)

        # Sort by x values
        ord <- order(x)
        x <- x[ord]
        weights <- weights[ord]

        # Normalize weights
        weights <- weights / sum(weights)

        # Calculate cumulative weights
        cum_weights <- cumsum(weights)

        # Find quantile values
        sapply(probs, function(p) {
            if (p == 0) return(min(x))
            if (p == 1) return(max(x))
            idx <- which(cum_weights >= p)[1]
            if (is.na(idx)) return(max(x))
            return(x[idx])
        })
    }

    # Overall statistics across all simulations with weights
    calculate_overall_stats <- function(data_array) {
        dims <- dim(data_array)
        n_locs <- dims[1]
        n_times <- dims[2]
        n_params <- dims[3]
        n_stoch <- dims[4]

        # Create weight vector for all simulations
        # Each parameter set's weight is divided equally among its stochastic runs
        sim_weights <- rep(parameter_weights, each = n_stoch) / n_stoch

        # Initialize result matrices - one for median, plus matrices for each CI pair
        n_ci_pairs <- length(envelope_quantiles) / 2
        stats_median <- matrix(NA, nrow = n_locs, ncol = n_times)
        stats_mean <- matrix(NA, nrow = n_locs, ncol = n_times)

        # Create list to hold CI bounds
        ci_bounds <- list()
        for (ci_idx in 1:n_ci_pairs) {
            ci_bounds[[ci_idx]] <- list(
                lower = matrix(NA, nrow = n_locs, ncol = n_times),
                upper = matrix(NA, nrow = n_locs, ncol = n_times)
            )
        }

        # Calculate statistics for each location and time
        for (i in 1:n_locs) {
            for (j in 1:n_times) {
                # Get all simulations for this location and time
                values <- as.vector(data_array[i, j, , ])

                # Weighted statistics
                stats_mean[i, j] <- sum(values * sim_weights, na.rm = TRUE)
                stats_median[i, j] <- weighted_quantile(values, sim_weights, 0.5)

                # Calculate all quantiles at once
                all_quantiles <- weighted_quantile(values, sim_weights, envelope_quantiles)

                # Assign to CI pairs (pair from inside out)
                # For c(0.025, 0.25, 0.75, 0.975): pair 1 = (0.25, 0.75), pair 2 = (0.025, 0.975)
                for (ci_idx in 1:n_ci_pairs) {
                    # Calculate indices for symmetric pairing from center outward
                    lower_idx <- ci_idx
                    upper_idx <- length(envelope_quantiles) - ci_idx + 1
                    ci_bounds[[ci_idx]]$lower[i, j] <- all_quantiles[lower_idx]
                    ci_bounds[[ci_idx]]$upper[i, j] <- all_quantiles[upper_idx]
                }
            }
        }

        list(median = stats_median, mean = stats_mean, ci_bounds = ci_bounds)
    }

    # Calculate unified statistics
    cases_overall <- calculate_overall_stats(cases_array)
    deaths_overall <- calculate_overall_stats(deaths_array)

    # ============================================================================
    # Handle dates
    # ============================================================================

    if (!is.null(date_start) && !is.null(date_stop)) {
        dates <- seq(as.Date(date_start), as.Date(date_stop), length.out = n_time_points)
    } else if (!is.null(date_start)) {
        dates <- seq(as.Date(date_start), length.out = n_time_points, by = "week")
    } else {
        dates <- 1:n_time_points
    }
    use_date_axis <- inherits(dates, "Date")

    # ============================================================================
    # Prepare plotting data
    # ============================================================================

    if (verbose) message("\n=== Creating Plots ===")

    # Helper function
    extract_location_data <- function(data, i) {
        if(is.matrix(data)) data[i,] else data
    }

    # Build plot data with multiple CI levels
    plot_data <- data.frame()
    n_ci_pairs <- length(envelope_quantiles) / 2

    for (i in 1:n_locations) {
        # Extract observed data
        obs_cases_i <- extract_location_data(obs_cases, i)
        obs_deaths_i <- extract_location_data(obs_deaths, i)

        # Create base dataframe for this location
        loc_data <- data.frame(
            location = location_names[i],
            date = rep(dates, 2),
            metric = c(rep("Cases", n_time_points), rep("Deaths", n_time_points)),
            observed = c(obs_cases_i, obs_deaths_i),
            predicted_median = c(cases_overall$median[i,], deaths_overall$median[i,])
        )

        # Add CI bounds dynamically
        for (ci_idx in 1:n_ci_pairs) {
            # Create column names for this CI pair
            lower_col <- paste0("ci_", ci_idx, "_lower")
            upper_col <- paste0("ci_", ci_idx, "_upper")

            # Add the bounds
            loc_data[[lower_col]] <- c(cases_overall$ci_bounds[[ci_idx]]$lower[i,],
                                      deaths_overall$ci_bounds[[ci_idx]]$lower[i,])
            loc_data[[upper_col]] <- c(cases_overall$ci_bounds[[ci_idx]]$upper[i,],
                                      deaths_overall$ci_bounds[[ci_idx]]$upper[i,])
        }

        plot_data <- rbind(plot_data, loc_data)
    }

    plot_data$metric <- factor(plot_data$metric, levels = c("Cases", "Deaths"))

    # ============================================================================
    # Save predictions to CSV (if requested)
    # ============================================================================

    if (save_predictions) {
        if (verbose) message("\n=== Saving Predictions ===")

        for (i in 1:n_locations) {
            loc_name <- location_names[i]
            loc_data <- plot_data[plot_data$location == loc_name,]

            # Create filename
            csv_file <- file.path(output_dir, paste0("predictions_ensemble_", loc_name, ".csv"))

            # Save to CSV
            write.csv(loc_data, csv_file, row.names = FALSE)

            if (verbose) {
                message("Predictions saved: ", csv_file)
            }
        }
    }

    # ============================================================================
    # Create plots
    # ============================================================================

    plot_list <- list()
    plot_list$individual <- list()

    # Individual location plots with decomposed uncertainty
    for (i in 1:n_locations) {
        loc_name <- location_names[i]
        loc_data <- plot_data[plot_data$location == loc_name,]

        # Calculate summary statistics
        obs_cases_i <- extract_location_data(obs_cases, i)
        obs_deaths_i <- extract_location_data(obs_deaths, i)
        pred_cases_i <- cases_overall$median[i,]
        pred_deaths_i <- deaths_overall$median[i,]

        sum_obs_cases <- sum(obs_cases_i, na.rm = TRUE)
        sum_pred_cases <- round(sum(pred_cases_i, na.rm = TRUE))
        sum_obs_deaths <- sum(obs_deaths_i, na.rm = TRUE)
        sum_pred_deaths <- round(sum(pred_deaths_i, na.rm = TRUE))

        cor_cases <- tryCatch(round(cor(obs_cases_i, pred_cases_i, use = "complete.obs"), 3), error = function(e) NA)
        cor_deaths <- tryCatch(round(cor(obs_deaths_i, pred_deaths_i, use = "complete.obs"), 3), error = function(e) NA)

        # Create plot with layered uncertainty
        p_individual <- ggplot2::ggplot(loc_data, ggplot2::aes(x = date))

        # Add CI ribbons from widest to narrowest (so narrower ones show on top)
        # Lower alpha for wider CIs, higher alpha for narrower CIs
        ribbon_alphas <- seq(0.2, 0.5, length.out = n_ci_pairs)

        for (ci_idx in 1:n_ci_pairs) {  # Natural order: widest (1) first, narrowest (n) last
            lower_col <- paste0("ci_", ci_idx, "_lower")
            upper_col <- paste0("ci_", ci_idx, "_upper")

            p_individual <- p_individual +
                ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[lower_col]],
                                                 ymax = .data[[upper_col]],
                                                 fill = metric),
                                   alpha = ribbon_alphas[ci_idx])
        }

        # Add observed points and median line
        p_individual <- p_individual +
            # Observed points
            ggplot2::geom_point(ggplot2::aes(y = observed),
                              color = "black", size = 1.5, alpha = 0.6) +
            # Median prediction line
            ggplot2::geom_line(ggplot2::aes(y = predicted_median, color = metric),
                             linewidth = 0.75) +
            # Facet by metric
            ggplot2::facet_grid(metric ~ ., scales = "free_y", switch = "y") +
            ggplot2::scale_color_manual(values = c("Cases" = "steelblue", "Deaths" = "darkred"),
                                       guide = "none") +
            ggplot2::scale_fill_manual(values = c("Cases" = "steelblue", "Deaths" = "darkred"),
                                      guide = "none") +
            ggplot2::scale_y_continuous(labels = scales::comma) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(
                strip.text = ggplot2::element_text(size = 9, face = "bold"),
                strip.background = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
                plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic"),
                strip.placement = "outside"
            ) +
            ggplot2::labs(
                x = if(use_date_axis) "Date" else "Time",
                y = NULL,
                title = paste0("Stochastic Model Fit with Parameter Uncertainty: ", loc_name),
                subtitle = paste0(
                    n_param_sets, " parameter sets × ",
                    n_simulations_per_config, " stochastic runs = ",
                    total_simulations, " total simulations"
                ),
                caption = paste0(
                    "Ribbons show ", paste(
                        paste0(round(envelope_quantiles[seq(1, length(envelope_quantiles), by=2)]*100), "-",
                               round(envelope_quantiles[seq(2, length(envelope_quantiles), by=2)]*100), "%"),
                        collapse = " and "
                    ), " confidence intervals\n",
                    "Cases: Obs = ", format(sum_obs_cases, big.mark = ","),
                    ", Pred = ", format(sum_pred_cases, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_cases), "NA", cor_cases),
                    " | Deaths: Obs = ", format(sum_obs_deaths, big.mark = ","),
                    ", Pred = ", format(sum_pred_deaths, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_deaths), "NA", cor_deaths)
                )
            )

        if (use_date_axis) {
            p_individual <- p_individual +
                ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
        }

        plot_list$individual[[loc_name]] <- p_individual

        if (verbose) print(p_individual)

        # Save plot
        output_file <- file.path(output_dir, paste0("model_stochastic_param_", loc_name, ".pdf"))
        ggplot2::ggsave(output_file, plot = p_individual, width = 10, height = 6, dpi = 300)

        if (verbose) message("Plot saved: ", output_file)
    }

    # Store simulation statistics
    plot_list$simulation_stats <- list(
        n_param_sets = n_param_sets,
        n_simulations_per_config = n_simulations_per_config,
        total_simulations = total_simulations,
        envelope_quantiles = envelope_quantiles,
        cases_overall = cases_overall,
        deaths_overall = deaths_overall
    )

    plot_list$param_configs <- param_configs

    if (verbose) message("\n=== Plotting Complete ===")

    invisible(plot_list)
}
