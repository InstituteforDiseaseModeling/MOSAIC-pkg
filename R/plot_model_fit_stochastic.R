#' Plot Stochastic Model Fit: Timeseries with Uncertainty Envelopes
#'
#' Runs multiple stochastic simulations of a model configuration and creates
#' timeseries plots showing mean predictions with confidence envelopes.
#'
#' @param config A configuration list/object as produced by sample_parameters().
#'   Must contain all necessary parameters for running laser_cholera model including:
#'   \itemize{
#'     \item reported_cases - observed cases data
#'     \item reported_deaths - observed deaths data
#'     \item location_name - location identifiers
#'     \item date_start and date_stop - time range for the plot
#'   }
#' @param n_simulations Integer specifying number of stochastic simulations to run.
#'   Default is 100.
#' @param output_dir Character string specifying the directory where plots should
#'   be saved. Directory will be created if it doesn't exist.
#' @param envelope_quantiles Numeric vector of length 2 specifying the quantiles
#'   for the confidence envelope. Default is c(0.1, 0.9) for 80% CI.
#' @param save_predictions Logical indicating whether to save prediction data to CSV files.
#'   Default is FALSE. If TRUE, saves predictions_stochastic_{location}.csv for each location.
#' @param parallel Logical indicating whether to use parallel computation. Default is FALSE.
#'   When TRUE, simulations are run in parallel across multiple cores.
#' @param n_cores Integer specifying number of cores to use for parallel computation.
#'   Default is NULL (uses detectCores() - 1). Only used when parallel = TRUE.
#' @param root_dir Character string specifying the root directory for MOSAIC project.
#'   Required when parallel = TRUE. Workers need this to initialize PATHS correctly.
#' @param verbose Logical indicating whether to print progress messages. Default is TRUE.
#'
#' @return Invisibly returns a list containing:
#'   \itemize{
#'     \item \code{individual}: Named list of plots for each location
#'     \item \code{cases_faceted}: Faceted plot of cases by location (if n_locations > 1)
#'     \item \code{deaths_faceted}: Faceted plot of deaths by location (if n_locations > 1)
#'     \item \code{simulation_stats}: Statistics from the simulations including
#'       successful run count and aggregated predictions
#'   }
#'
#' @details
#' This function performs multiple stochastic runs of the laser-cholera model
#' with identical configuration but different random seeds. It then aggregates
#' the results to show:
#' \itemize{
#'   \item Mean predictions across all simulations
#'   \item Confidence envelopes based on specified quantiles
#'   \item Observed data for comparison
#'   \item Summary statistics (total counts, correlations)
#' }
#'
#' Unlike plot_model_fit(), this function does not calculate or display likelihood
#' values, focusing instead on prediction uncertainty through stochastic simulation.
#'
#' @examples
#' \dontrun{
#' # Sample parameters for best model
#' best_config <- sample_parameters(PATHS, priors = priors,
#'                                  config = base_config, seed = 123)
#'
#' # Create stochastic plots with 100 simulations
#' plots <- plot_model_fit_stochastic(
#'     config = best_config,
#'     n_simulations = 100,
#'     output_dir = "output/plots",
#'     envelope_quantiles = c(0.1, 0.9),
#'     save_predictions = TRUE  # Save predictions to CSV
#' )
#' # Saves: predictions_stochastic_ETH.csv (or other location names)
#'
#' # Access individual plots
#' plots$individual[["ETH"]]  # Individual location plot for Ethiopia
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_point geom_line facet_grid facet_wrap scale_color_manual scale_fill_manual scale_y_continuous scale_x_date theme_minimal theme element_text element_blank labs ggsave
#' @importFrom dplyr filter mutate
#' @importFrom scales comma
#' @importFrom reticulate import
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom parallel makeCluster stopCluster clusterEvalQ clusterExport detectCores
#' @importFrom pbapply pblapply
plot_model_fit_stochastic <- function(config,
                                     n_simulations = 100,
                                     output_dir,
                                     envelope_quantiles = c(0.1, 0.9),
                                     save_predictions = FALSE,
                                     parallel = FALSE,
                                     n_cores = NULL,
                                     root_dir = NULL,
                                     verbose = TRUE) {

    # ============================================================================
    # Input validation
    # ============================================================================

    if (missing(config) || is.null(config)) {
        stop("config is required")
    }

    if (missing(output_dir) || is.null(output_dir)) {
        stop("output_dir is required")
    }

    if (length(envelope_quantiles) != 2 || envelope_quantiles[1] >= envelope_quantiles[2]) {
        stop("envelope_quantiles must be a vector of length 2 with first value < second value")
    }

    if (n_simulations < 2) {
        stop("n_simulations must be at least 2")
    }

    # Validate parallel parameters
    if (parallel && is.null(root_dir)) {
        stop("root_dir is required when parallel = TRUE")
    }

    if (parallel && !is.null(n_cores) && n_cores < 1) {
        stop("n_cores must be at least 1")
    }

    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        if (verbose) message("Created output directory: ", output_dir)
    }

    # ============================================================================
    # Setup seeds for reproducible simulations
    # ============================================================================

    # Always use 1:n_simulations for reproducibility
    seeds <- 1:n_simulations
    if (verbose) message("Using seeds: 1 to ", n_simulations)

    # ============================================================================
    # Extract observed data from config
    # ============================================================================

    obs_cases <- config$reported_cases
    obs_deaths <- config$reported_deaths
    location_names <- config$location_name
    date_start <- config$date_start
    date_stop <- config$date_stop

    if (is.null(obs_cases) || is.null(obs_deaths)) {
        stop("config must contain reported_cases and reported_deaths")
    }

    if (is.null(location_names)) {
        n_locations <- if(is.matrix(obs_cases)) nrow(obs_cases) else 1
        location_names <- if(n_locations == 1) "Location" else paste0("Location_", 1:n_locations)
        if (verbose) message("Warning: No location names found. Using defaults: ",
                           paste(location_names, collapse = ", "))
    }

    n_locations <- length(location_names)
    n_time_points <- if(is.matrix(obs_cases)) ncol(obs_cases) else length(obs_cases)

    if (verbose) {
        message("Configuration loaded:")
        message("  - Locations: ", n_locations, " (", paste(location_names, collapse = ", "), ")")
        message("  - Time points: ", n_time_points)
        message("  - Simulations to run: ", n_simulations)
    }

    # ============================================================================
    # Run multiple simulations (Parallel or Sequential)
    # ============================================================================

    if (verbose) message("\n=== Running Stochastic Simulations ===")

    # Worker function (must be self-contained for parallel execution)
    run_single_simulation <- function(seed_i, config_template) {
        tryCatch({
            # Import laser_cholera on worker (each worker needs its own instance)
            lc <- reticulate::import("laser_cholera.metapop.model")

            # Modify config with specific seed
            config_i <- config_template
            config_i$seed <- seed_i

            # Run model
            model <- lc$run_model(paramfile = config_i, quiet = TRUE)

            # Extract results before cleanup
            result <- list(
                expected_cases = model$results$expected_cases,
                disease_deaths = model$results$disease_deaths,
                success = TRUE,
                seed = seed_i
            )

            # Cleanup Python objects to prevent accumulation across tasks
            gc(verbose = FALSE)
            reticulate::import("gc")$collect()

            # Return results
            result

        }, error = function(e) {
            list(success = FALSE, seed = seed_i, error = as.character(e))
        })
    }

    # Execute simulations (parallel or sequential)
    if (parallel) {
        # ========================================================================
        # PARALLEL EXECUTION
        # ========================================================================

        # Determine number of cores
        n_cores_use <- if (is.null(n_cores)) {
            max(1, parallel::detectCores() - 1)
        } else {
            n_cores
        }

        if (verbose) {
            message("Setting up parallel cluster with ", n_cores_use, " cores...")
        }

        # CRITICAL: Set threading environment variables to prevent fork issues
        Sys.setenv(
            TBB_NUM_THREADS = "1",
            NUMBA_NUM_THREADS = "1",
            OMP_NUM_THREADS = "1",
            MKL_NUM_THREADS = "1",
            OPENBLAS_NUM_THREADS = "1"
        )

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

        # Set root directory on workers if provided
        if (!is.null(root_dir)) {
            parallel::clusterCall(cl, function(rd) {
                MOSAIC::set_root_directory(rd)
                MOSAIC::get_paths()
            }, root_dir)
        }

        # Export config and worker function to workers
        parallel::clusterExport(cl, c("config", "run_single_simulation"), envir = environment())

        # Run parallel simulations
        if (verbose) {
            message("Running ", n_simulations, " simulations on ", n_cores_use, " cores...")
            # Simple progress bar with block character (no color codes)
            # style = 1: Shows elapsed and remaining time with percentage
            pbo <- pbapply::pboptions(type = "timer", char = "█", style = 1)
            on.exit(pbapply::pboptions(pbo), add = TRUE)
        }

        simulation_results <- pbapply::pblapply(
            seeds,
            function(s) run_single_simulation(s, config),
            cl = cl
        )

    } else {
        # ========================================================================
        # SEQUENTIAL EXECUTION
        # ========================================================================

        if (verbose) {
            message("Running ", n_simulations, " simulations sequentially...")
            # Simple progress bar with block character (no color codes)
            # style = 1: Shows elapsed and remaining time with percentage
            pbo <- pbapply::pboptions(type = "timer", char = "█", style = 1)
            on.exit(pbapply::pboptions(pbo), add = TRUE)
        }

        simulation_results <- pbapply::pblapply(
            seeds,
            function(s) run_single_simulation(s, config)
        )
    }

    # ============================================================================
    # Filter successful simulations
    # ============================================================================

    successful_results <- simulation_results[sapply(simulation_results, function(x) x$success)]
    n_successful <- length(successful_results)

    if (n_successful == 0) {
        # Show sample error messages for debugging
        failed_with_errors <- simulation_results[sapply(simulation_results, function(x) !is.null(x$error))]
        if (length(failed_with_errors) > 0) {
            sample_errors <- unique(sapply(failed_with_errors[1:min(3, length(failed_with_errors))],
                                          function(x) x$error))
            error_msg <- paste0("All simulations failed. Sample errors:\n  ",
                              paste(sample_errors, collapse = "\n  "))
            stop(error_msg)
        } else {
            stop("All simulations failed. Check model configuration.")
        }
    }

    if (verbose) {
        message("\nSimulation summary:")
        message("  - Successful: ", n_successful, "/", n_simulations,
                " (", round(n_successful/n_simulations * 100, 1), "%)")
        if (n_successful < n_simulations) {
            message("  - Failed: ", n_simulations - n_successful)
        }
    }

    # ============================================================================
    # Aggregate simulation results
    # ============================================================================

    if (verbose) message("\n=== Aggregating Results ===")

    # Create arrays to store all simulation results
    # Dimensions: [location, time, simulation]
    cases_array <- array(NA, dim = c(n_locations, n_time_points, n_successful))
    deaths_array <- array(NA, dim = c(n_locations, n_time_points, n_successful))

    # Fill arrays with simulation results
    for (i in 1:n_successful) {
        result <- successful_results[[i]]

        # Handle both matrix and vector formats
        if (is.matrix(result$expected_cases)) {
            cases_array[,,i] <- result$expected_cases
            deaths_array[,,i] <- result$disease_deaths
        } else {
            cases_array[1,,i] <- result$expected_cases
            deaths_array[1,,i] <- result$disease_deaths
        }
    }

    # Calculate statistics across simulations
    calculate_stats <- function(data_array) {
        # Apply functions across simulations (3rd dimension)
        # Use drop=FALSE to maintain matrix structure even for single location
        stats_mean <- apply(data_array, c(1,2), mean, na.rm = TRUE)
        stats_median <- apply(data_array, c(1,2), median, na.rm = TRUE)
        stats_lower <- apply(data_array, c(1,2), quantile, probs = envelope_quantiles[1], na.rm = TRUE)
        stats_upper <- apply(data_array, c(1,2), quantile, probs = envelope_quantiles[2], na.rm = TRUE)

        # Ensure results are always matrices
        if (!is.matrix(stats_mean)) {
            stats_mean <- matrix(stats_mean, nrow = 1)
            stats_median <- matrix(stats_median, nrow = 1)
            stats_lower <- matrix(stats_lower, nrow = 1)
            stats_upper <- matrix(stats_upper, nrow = 1)
        }

        list(
            mean = stats_mean,
            median = stats_median,
            lower = stats_lower,
            upper = stats_upper
        )
    }

    cases_stats <- calculate_stats(cases_array)
    deaths_stats <- calculate_stats(deaths_array)

    # Diagnostic output for envelope verification
    if (verbose) {
        # Sample the first location, first 5 time points for diagnostics
        sample_loc <- 1
        sample_times <- 1:min(5, n_time_points)

        message("\n=== Envelope Diagnostics (Location ", sample_loc, ", Time points ",
                min(sample_times), "-", max(sample_times), ") ===")

        for (t in sample_times) {
            # Get all simulation values for this time point
            sim_values <- cases_array[sample_loc, t, ]

            # Calculate quantiles manually for verification
            manual_lower <- quantile(sim_values, probs = envelope_quantiles[1], na.rm = TRUE)
            manual_upper <- quantile(sim_values, probs = envelope_quantiles[2], na.rm = TRUE)
            manual_mean <- mean(sim_values, na.rm = TRUE)

            message(sprintf("  Time %d: Mean=%.1f, [%.0f%%-%.0f%% CI: %.1f-%.1f], Range=[%.1f-%.1f]",
                          t, manual_mean,
                          envelope_quantiles[1] * 100, envelope_quantiles[2] * 100,
                          manual_lower, manual_upper,
                          min(sim_values, na.rm = TRUE), max(sim_values, na.rm = TRUE)))
        }
    }

    # ============================================================================
    # Handle dates
    # ============================================================================

    if (!is.null(date_start) && !is.null(date_stop)) {
        dates <- seq(as.Date(date_start), as.Date(date_stop), length.out = n_time_points)
    } else if (!is.null(date_start)) {
        dates <- seq(as.Date(date_start), length.out = n_time_points, by = "week")
    } else {
        dates <- 1:n_time_points
        if (verbose) message("Warning: No date information found. Using numeric time points.")
    }

    use_date_axis <- inherits(dates, "Date")

    # ============================================================================
    # Prepare plotting data
    # ============================================================================

    if (verbose) message("\n=== Creating Plots ===")

    # Helper function to extract data for single location
    extract_location_data <- function(data, i) {
        if(is.matrix(data)) data[i,] else data
    }

    # Build plot data
    plot_data <- data.frame()

    for (i in 1:n_locations) {
        # Extract observed data
        obs_cases_i <- extract_location_data(obs_cases, i)
        obs_deaths_i <- extract_location_data(obs_deaths, i)

        # Extract statistics (now always matrices)
        cases_mean_i <- cases_stats$mean[i,]
        cases_lower_i <- cases_stats$lower[i,]
        cases_upper_i <- cases_stats$upper[i,]

        deaths_mean_i <- deaths_stats$mean[i,]
        deaths_lower_i <- deaths_stats$lower[i,]
        deaths_upper_i <- deaths_stats$upper[i,]

        # Create dataframe for this location
        loc_data <- data.frame(
            location = location_names[i],
            date = rep(dates, 2),
            metric = c(rep("Cases", n_time_points), rep("Deaths", n_time_points)),
            observed = c(obs_cases_i, obs_deaths_i),
            predicted_mean = c(cases_mean_i, deaths_mean_i),
            predicted_lower = c(cases_lower_i, deaths_lower_i),
            predicted_upper = c(cases_upper_i, deaths_upper_i)
        )

        plot_data <- rbind(plot_data, loc_data)
    }

    # Convert metric to factor
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
            csv_file <- file.path(output_dir, paste0("predictions_stochastic_", loc_name, ".csv"))

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

    # List to store all plots
    plot_list <- list()
    plot_list$individual <- list()

    # ============================================================================
    # 1. Individual location plots
    # ============================================================================

    for (i in 1:n_locations) {
        loc_name <- location_names[i]

        # Filter data for this location
        loc_data <- plot_data[plot_data$location == loc_name,]

        # Calculate summary statistics
        obs_cases_i <- extract_location_data(obs_cases, i)
        obs_deaths_i <- extract_location_data(obs_deaths, i)
        pred_cases_i <- cases_stats$mean[i,]
        pred_deaths_i <- deaths_stats$mean[i,]

        sum_obs_cases <- sum(obs_cases_i, na.rm = TRUE)
        sum_pred_cases <- round(sum(pred_cases_i, na.rm = TRUE))
        sum_obs_deaths <- sum(obs_deaths_i, na.rm = TRUE)
        sum_pred_deaths <- round(sum(pred_deaths_i, na.rm = TRUE))

        # Calculate correlations
        cor_cases <- tryCatch({
            round(cor(obs_cases_i, pred_cases_i, use = "complete.obs"), 3)
        }, error = function(e) NA)

        cor_deaths <- tryCatch({
            round(cor(obs_deaths_i, pred_deaths_i, use = "complete.obs"), 3)
        }, error = function(e) NA)

        # Create plot
        p_individual <- ggplot2::ggplot(loc_data, ggplot2::aes(x = date)) +
            # Confidence envelope
            ggplot2::geom_ribbon(ggplot2::aes(ymin = predicted_lower,
                                             ymax = predicted_upper,
                                             fill = metric),
                                alpha = 0.3) +
            # Observed points
            ggplot2::geom_point(ggplot2::aes(y = observed),
                              color = "black",
                              size = 1.5,
                              alpha = 0.6) +
            # Mean prediction line
            ggplot2::geom_line(ggplot2::aes(y = predicted_mean, color = metric),
                             linewidth = 0.8) +
            # Facet by metric
            ggplot2::facet_grid(metric ~ .,
                              scales = "free_y",
                              switch = "y") +
            ggplot2::scale_color_manual(values = c("Cases" = "steelblue",
                                                  "Deaths" = "darkred"),
                                       guide = "none") +
            ggplot2::scale_fill_manual(values = c("Cases" = "steelblue",
                                                 "Deaths" = "darkred"),
                                      guide = "none") +
            ggplot2::scale_y_continuous(labels = scales::comma) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(
                strip.text = ggplot2::element_text(size = 9, face = "bold"),
                strip.background = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "gray85"),
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
                axis.text.y = ggplot2::element_text(size = 8),
                axis.title = ggplot2::element_text(size = 10),
                plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic"),
                strip.placement = "outside"
            ) +
            ggplot2::labs(
                x = if(use_date_axis) "Date" else "Time",
                y = NULL,
                title = paste0("Stochastic Model Fit: ", loc_name),
                subtitle = paste0(
                    "Median prediction (line) with ",
                    round((envelope_quantiles[2] - envelope_quantiles[1]) * 100), "% range (ribbon) | ",
                    n_successful, " simulations"
                ),
                caption = paste0(
                    "Cases: Obs = ", format(sum_obs_cases, big.mark = ","),
                    ", Pred = ", format(sum_pred_cases, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_cases), "NA", cor_cases),
                    " | Deaths: Obs = ", format(sum_obs_deaths, big.mark = ","),
                    ", Pred = ", format(sum_pred_deaths, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_deaths), "NA", cor_deaths),
                    "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                )
            )

        # Add appropriate x-axis scale
        if (use_date_axis) {
            p_individual <- p_individual +
                ggplot2::scale_x_date(date_breaks = "3 months",
                                    date_labels = "%b %Y")
        }

        # Store plot
        plot_list$individual[[loc_name]] <- p_individual

        # Display plot
        if (verbose) {
            print(p_individual)
        }

        # Save individual location plot
        output_file <- file.path(output_dir, paste0("model_stochastic_", loc_name, ".pdf"))
        ggplot2::ggsave(output_file,
                       plot = p_individual,
                       width = 10,
                       height = 6,
                       dpi = 300)

        if (verbose) {
            message("Stochastic plot saved: ", output_file)
        }
    }

    # ============================================================================
    # 2. Cases faceted plot (skip if only 1 location)
    # ============================================================================

    if (n_locations > 1) {
        # Filter for cases only
        cases_data <- plot_data[plot_data$metric == "Cases",]

        # Calculate overall statistics
        all_obs_cases <- if(is.matrix(obs_cases)) as.vector(obs_cases) else obs_cases
        all_pred_cases <- if(is.matrix(cases_stats$mean)) as.vector(cases_stats$mean) else cases_stats$mean

        cor_cases_overall <- tryCatch({
            round(cor(all_obs_cases, all_pred_cases, use = "complete.obs"), 3)
        }, error = function(e) NA)

        sum_obs_cases_all <- sum(obs_cases, na.rm = TRUE)
        sum_pred_cases_all <- round(sum(cases_stats$mean, na.rm = TRUE))

        p_cases <- ggplot2::ggplot(cases_data, ggplot2::aes(x = date)) +
            # Confidence envelope
            ggplot2::geom_ribbon(ggplot2::aes(ymin = predicted_lower,
                                             ymax = predicted_upper),
                                fill = "steelblue",
                                alpha = 0.3) +
            # Observed points
            ggplot2::geom_point(ggplot2::aes(y = observed),
                              color = "black",
                              size = 1.5,
                              alpha = 0.6) +
            # Mean prediction line
            ggplot2::geom_line(ggplot2::aes(y = predicted_mean),
                             color = "steelblue",
                             linewidth = 0.8) +
            # Facet by location
            ggplot2::facet_wrap(~ location,
                              scales = "free_y",
                              ncol = min(3, n_locations)) +
            ggplot2::scale_y_continuous(labels = scales::comma) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(
                strip.text = ggplot2::element_text(size = 9, face = "bold"),
                strip.background = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "gray85"),
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
                axis.text.y = ggplot2::element_text(size = 8),
                axis.title = ggplot2::element_text(size = 10),
                plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic")
            ) +
            ggplot2::labs(
                x = if(use_date_axis) "Date" else "Time",
                y = "Cases",
                title = "Stochastic Model Fit: Cases by Location",
                subtitle = paste0(
                    "Median prediction with ",
                    round((envelope_quantiles[2] - envelope_quantiles[1]) * 100), "% range | ",
                    n_successful, " simulations"
                ),
                caption = paste0(
                    "Total Cases: Obs = ", format(sum_obs_cases_all, big.mark = ","),
                    ", Pred = ", format(sum_pred_cases_all, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_cases_overall), "NA", cor_cases_overall),
                    "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                )
            )

        # Add appropriate x-axis scale
        if (use_date_axis) {
            p_cases <- p_cases +
                ggplot2::scale_x_date(date_breaks = "3 months",
                                    date_labels = "%b %Y")
        }

        plot_list$cases_faceted <- p_cases

        # Display plot
        if (verbose) {
            print(p_cases)
        }

        # Determine dimensions
        if (n_locations <= 3) {
            plot_width <- 12
            plot_height <- 5
        } else if (n_locations <= 6) {
            plot_width <- 14
            plot_height <- 8
        } else {
            plot_width <- 16
            plot_height <- max(10, ceiling(n_locations / 3) * 3)
        }

        # Save cases plot
        output_file <- file.path(output_dir, "model_stochastic_cases_all.pdf")
        ggplot2::ggsave(output_file,
                       plot = p_cases,
                       width = plot_width,
                       height = plot_height,
                       dpi = 300,
                       limitsize = FALSE)

        if (verbose) {
            message("Cases faceted plot saved: ", output_file)
        }
    }

    # ============================================================================
    # 3. Deaths faceted plot (skip if only 1 location)
    # ============================================================================

    if (n_locations > 1) {
        # Filter for deaths only
        deaths_data <- plot_data[plot_data$metric == "Deaths",]

        # Calculate overall statistics
        all_obs_deaths <- if(is.matrix(obs_deaths)) as.vector(obs_deaths) else obs_deaths
        all_pred_deaths <- if(is.matrix(deaths_stats$mean)) as.vector(deaths_stats$mean) else deaths_stats$mean

        cor_deaths_overall <- tryCatch({
            round(cor(all_obs_deaths, all_pred_deaths, use = "complete.obs"), 3)
        }, error = function(e) NA)

        sum_obs_deaths_all <- sum(obs_deaths, na.rm = TRUE)
        sum_pred_deaths_all <- round(sum(deaths_stats$mean, na.rm = TRUE))

        p_deaths <- ggplot2::ggplot(deaths_data, ggplot2::aes(x = date)) +
            # Confidence envelope
            ggplot2::geom_ribbon(ggplot2::aes(ymin = predicted_lower,
                                             ymax = predicted_upper),
                                fill = "darkred",
                                alpha = 0.3) +
            # Observed points
            ggplot2::geom_point(ggplot2::aes(y = observed),
                              color = "black",
                              size = 1.5,
                              alpha = 0.6) +
            # Mean prediction line
            ggplot2::geom_line(ggplot2::aes(y = predicted_mean),
                             color = "darkred",
                             linewidth = 0.8) +
            # Facet by location
            ggplot2::facet_wrap(~ location,
                              scales = "free_y",
                              ncol = min(3, n_locations)) +
            ggplot2::scale_y_continuous(labels = scales::comma) +
            ggplot2::theme_minimal(base_size = 10) +
            ggplot2::theme(
                strip.text = ggplot2::element_text(size = 9, face = "bold"),
                strip.background = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "gray85"),
                axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
                axis.text.y = ggplot2::element_text(size = 8),
                axis.title = ggplot2::element_text(size = 10),
                plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
                plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
                plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic")
            ) +
            ggplot2::labs(
                x = if(use_date_axis) "Date" else "Time",
                y = "Deaths",
                title = "Stochastic Model Fit: Deaths by Location",
                subtitle = paste0(
                    "Median prediction with ",
                    round((envelope_quantiles[2] - envelope_quantiles[1]) * 100), "% range | ",
                    n_successful, " simulations"
                ),
                caption = paste0(
                    "Total Deaths: Obs = ", format(sum_obs_deaths_all, big.mark = ","),
                    ", Pred = ", format(sum_pred_deaths_all, big.mark = ","),
                    ", Cor = ", ifelse(is.na(cor_deaths_overall), "NA", cor_deaths_overall),
                    "\nGenerated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                )
            )

        # Add appropriate x-axis scale
        if (use_date_axis) {
            p_deaths <- p_deaths +
                ggplot2::scale_x_date(date_breaks = "3 months",
                                    date_labels = "%b %Y")
        }

        plot_list$deaths_faceted <- p_deaths

        # Display plot
        if (verbose) {
            print(p_deaths)
        }

        # Determine dimensions
        if (n_locations <= 3) {
            plot_width <- 12
            plot_height <- 5
        } else if (n_locations <= 6) {
            plot_width <- 14
            plot_height <- 8
        } else {
            plot_width <- 16
            plot_height <- max(10, ceiling(n_locations / 3) * 3)
        }

        # Save deaths plot
        output_file <- file.path(output_dir, "model_stochastic_deaths_all.pdf")
        ggplot2::ggsave(output_file,
                       plot = p_deaths,
                       width = plot_width,
                       height = plot_height,
                       dpi = 300,
                       limitsize = FALSE)

        if (verbose) {
            message("Deaths faceted plot saved: ", output_file)
        }
    }

    # ============================================================================
    # Add simulation statistics to return object
    # ============================================================================

    plot_list$simulation_stats <- list(
        n_simulations = n_simulations,
        n_successful = n_successful,
        success_rate = n_successful / n_simulations,
        envelope_quantiles = envelope_quantiles,
        seeds = seeds,
        cases_stats = cases_stats,
        deaths_stats = deaths_stats
    )

    if (verbose) {
        message("\n=== Stochastic Plotting Complete ===")
    }

    # Return plot objects invisibly
    invisible(plot_list)
}
