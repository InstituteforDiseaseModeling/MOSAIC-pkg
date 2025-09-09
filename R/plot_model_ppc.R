#' Plot Posterior Predictive Check Results
#'
#' Creates diagnostic plots comparing observed data with posterior predictive
#' samples from the best-fitting model. Generates time series plots with
#' credible intervals and diagnostic plots for model assessment.
#'
#' @param ppc_results Object of class "mosaic_ppc" from calc_model_ppc()
#' @param output_dir Character. Directory for saving plots. Created if doesn't exist.
#' @param location_names Character vector. Names for locations (default: auto-generated)
#' @param time_labels Character/Date vector. Labels for time axis (default: indices)
#' @param plot_types Character vector. Types of plots to generate:
#'   "timeseries", "diagnostics", "density", "coverage" (default: c("timeseries", "diagnostics"))
#' @param credible_levels Numeric vector. Credible interval levels (default: c(0.5, 0.8, 0.95))
#' @param max_traces Integer. Maximum individual traces to show (default: 30)
#' @param save_plots Logical. Save plots to files (default: TRUE)
#' @param plot_format Character. File format: "pdf", "png", "both" (default: "pdf")
#' @param width Numeric. Plot width in inches (default: 12)
#' @param height Numeric. Plot height in inches (default: 8)
#' @param verbose Logical. Print progress messages (default: TRUE)
#'
#' @return Invisibly returns a list of ggplot objects organized by location and type
#'
#' @details
#' Generates comprehensive visualization of posterior predictive checks:
#' 
#' **Time Series Plots**: Show observed data points, posterior mean, credible intervals,
#' and sample trajectories from the predictive distribution.
#' 
#' **Diagnostic Plots**: Include density comparisons, Q-Q plots, residual analysis,
#' and scatter plots of observed vs predicted.
#' 
#' **Coverage Plots**: Show empirical coverage of credible intervals across locations.
#' 
#' **Density Plots**: Compare distributions of observed and predicted values.
#'
#' @examples
#' \dontrun{
#' # Generate PPC results
#' ppc <- calc_model_ppc(results, schema, config_base = config, PATHS = paths)
#' 
#' # Create all plots
#' plots <- plot_model_ppc(
#'   ppc_results = ppc,
#'   output_dir = "ppc_plots",
#'   plot_types = c("timeseries", "diagnostics")
#' )
#' 
#' # Access specific plot
#' print(plots$location_1$timeseries)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_ribbon theme_minimal
#' @importFrom ggplot2 labs scale_fill_manual scale_color_manual facet_wrap
#' @seealso [calc_model_ppc()], [calc_ppc_diagnostics()]
plot_model_ppc <- function(ppc_results,
                          output_dir = "ppc_plots",
                          location_names = NULL,
                          time_labels = NULL,
                          plot_types = c("timeseries", "diagnostics"),
                          credible_levels = c(0.5, 0.8, 0.95),
                          max_traces = 30,
                          save_plots = TRUE,
                          plot_format = c("pdf", "png", "both"),
                          width = 12,
                          height = 8,
                          verbose = TRUE) {
  
  # ============================================================================
  # Input validation
  # ============================================================================
  if (!inherits(ppc_results, "mosaic_ppc")) {
    stop("ppc_results must be output from calc_model_ppc()")
  }
  
  plot_format <- match.arg(plot_format)
  valid_plot_types <- c("timeseries", "diagnostics", "density", "coverage")
  invalid_types <- setdiff(plot_types, valid_plot_types)
  if (length(invalid_types) > 0) {
    stop("Invalid plot_types: ", paste(invalid_types, collapse = ", "))
  }
  
  if (!is.numeric(credible_levels) || 
      any(credible_levels <= 0) || 
      any(credible_levels >= 1)) {
    stop("credible_levels must be between 0 and 1")
  }
  credible_levels <- sort(unique(credible_levels))
  
  # Create output directory if saving
  if (save_plots && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) cat("Created output directory: ", output_dir, "\n", sep = "")
  }
  
  # ============================================================================
  # Extract data
  # ============================================================================
  observed <- ppc_results$observed
  predicted <- ppc_results$predicted
  best_model <- ppc_results$best_model
  
  n_locations <- nrow(observed$cases)
  n_time <- ncol(observed$cases)
  n_reps <- dim(predicted$cases)[1]
  
  # Set labels
  if (is.null(location_names)) {
    location_names <- paste("Location", seq_len(n_locations))
  }
  if (length(location_names) != n_locations) {
    stop("location_names length must match number of locations")
  }
  
  if (is.null(time_labels)) {
    time_labels <- seq_len(n_time)
  }
  if (length(time_labels) != n_time) {
    stop("time_labels length must match number of time points")
  }
  
  if (verbose) {
    cat("Plotting PPC results:\n")
    cat("- Locations: ", n_locations, "\n", sep = "")
    cat("- Time points: ", n_time, "\n", sep = "")
    cat("- Replications: ", n_reps, "\n", sep = "")
    cat("- Plot types: ", paste(plot_types, collapse = ", "), "\n", sep = "")
  }
  
  # ============================================================================
  # Generate plots
  # ============================================================================
  all_plots <- list()
  
  # Time series plots
  if ("timeseries" %in% plot_types) {
    if (verbose) cat("Creating time series plots...\n")
    ts_plots <- create_timeseries_plots(
      observed = observed,
      predicted = predicted,
      best_model = best_model,
      location_names = location_names,
      time_labels = time_labels,
      credible_levels = credible_levels,
      max_traces = max_traces
    )
    all_plots$timeseries <- ts_plots
    
    if (save_plots) {
      save_plot_list(ts_plots, output_dir, "timeseries", 
                    plot_format, width, height, verbose)
    }
  }
  
  # Diagnostic plots
  if ("diagnostics" %in% plot_types) {
    if (verbose) cat("Creating diagnostic plots...\n")
    diag_plots <- create_diagnostic_plots(
      observed = observed,
      predicted = predicted,
      location_names = location_names
    )
    all_plots$diagnostics <- diag_plots
    
    if (save_plots) {
      save_plot_list(diag_plots, output_dir, "diagnostics",
                    plot_format, width * 1.5, height, verbose)
    }
  }
  
  # Density plots
  if ("density" %in% plot_types) {
    if (verbose) cat("Creating density plots...\n")
    dens_plots <- create_density_plots(
      observed = observed,
      predicted = predicted,
      location_names = location_names
    )
    all_plots$density <- dens_plots
    
    if (save_plots) {
      save_plot_list(dens_plots, output_dir, "density",
                    plot_format, width * 0.8, height * 0.8, verbose)
    }
  }
  
  # Coverage plots
  if ("coverage" %in% plot_types) {
    if (verbose) cat("Creating coverage plots...\n")
    cov_plots <- create_coverage_plots(
      ppc_results = ppc_results,
      credible_levels = credible_levels,
      location_names = location_names
    )
    all_plots$coverage <- cov_plots
    
    if (save_plots) {
      save_plot_list(cov_plots, output_dir, "coverage",
                    plot_format, width, height * 0.6, verbose)
    }
  }
  
  if (verbose) {
    cat("Plot generation complete\n")
    if (save_plots) {
      cat("Plots saved to: ", output_dir, "\n", sep = "")
    }
  }
  
  # Organize by location for easy access
  plots_by_location <- reorganize_plots_by_location(all_plots, location_names)
  
  invisible(plots_by_location)
}


#' Create Time Series Plots
#' @keywords internal
create_timeseries_plots <- function(observed, predicted, best_model,
                                   location_names, time_labels,
                                   credible_levels, max_traces) {
  
  requireNamespace("ggplot2", quietly = TRUE)
  plots <- list()
  
  # Calculate summary statistics
  pred_mean_cases <- apply(predicted$cases, c(2, 3), mean, na.rm = TRUE)
  pred_mean_deaths <- apply(predicted$deaths, c(2, 3), mean, na.rm = TRUE)
  
  # Calculate credible intervals
  ci_cases <- calculate_credible_intervals(predicted$cases, credible_levels)
  ci_deaths <- calculate_credible_intervals(predicted$deaths, credible_levels)
  
  # Select traces to show
  n_reps <- dim(predicted$cases)[1]
  if (n_reps > max_traces) {
    trace_idx <- round(seq(1, n_reps, length.out = max_traces))
  } else {
    trace_idx <- seq_len(n_reps)
  }
  
  for (loc in seq_len(nrow(observed$cases))) {
    
    # Prepare data frame for cases
    df_cases <- data.frame(
      time = seq_along(time_labels),
      observed = observed$cases[loc, ],
      mean = pred_mean_cases[loc, ]
    )
    
    # Add CI columns
    for (level in credible_levels) {
      ci_name <- paste0("ci_", gsub("\\.", "", as.character(level)))
      level_key <- as.character(level)
      df_cases[[paste0(ci_name, "_lower")]] <- ci_cases[[level_key]]$lower[loc, ]
      df_cases[[paste0(ci_name, "_upper")]] <- ci_cases[[level_key]]$upper[loc, ]
    }
    
    # Create cases plot
    p_cases <- ggplot2::ggplot(df_cases, ggplot2::aes(x = time)) +
      ggplot2::theme_minimal(base_size = 11)
    
    # Add credible intervals (widest first)
    colors <- c("#B0E2FF", "#87CEEB", "#4682B4")
    for (i in rev(seq_along(credible_levels))) {
      level <- credible_levels[i]
      ci_name <- paste0("ci_", gsub("\\.", "", as.character(level)))
      p_cases <- p_cases +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = .data[[paste0(ci_name, "_lower")]],
            ymax = .data[[paste0(ci_name, "_upper")]]
          ),
          fill = colors[i],
          alpha = 0.5
        )
    }
    
    # Add traces
    for (idx in trace_idx) {
      trace_data <- data.frame(
        time = seq_along(time_labels),
        value = predicted$cases[idx, loc, ]
      )
      p_cases <- p_cases +
        ggplot2::geom_line(
          data = trace_data,
          ggplot2::aes(y = value),
          color = "steelblue",
          alpha = 0.1,
          size = 0.3
        )
    }
    
    # Add mean and observed
    p_cases <- p_cases +
      ggplot2::geom_line(ggplot2::aes(y = mean), 
                        color = "darkblue", size = 1) +
      ggplot2::geom_point(ggplot2::aes(y = observed),
                         color = "black", size = 1.5, alpha = 0.7) +
      ggplot2::labs(
        title = paste0(location_names[loc], " - Cases"),
        subtitle = paste0("Best model (LL = ", 
                         sprintf("%.2f", best_model$likelihood), ")"),
        x = "Time",
        y = "Cases"
      )
    
    # Repeat for deaths
    df_deaths <- data.frame(
      time = seq_along(time_labels),
      observed = observed$deaths[loc, ],
      mean = pred_mean_deaths[loc, ]
    )
    
    for (level in credible_levels) {
      ci_name <- paste0("ci_", gsub("\\.", "", as.character(level)))
      level_key <- as.character(level)
      df_deaths[[paste0(ci_name, "_lower")]] <- ci_deaths[[level_key]]$lower[loc, ]
      df_deaths[[paste0(ci_name, "_upper")]] <- ci_deaths[[level_key]]$upper[loc, ]
    }
    
    p_deaths <- ggplot2::ggplot(df_deaths, ggplot2::aes(x = time)) +
      ggplot2::theme_minimal(base_size = 11)
    
    # Add credible intervals
    colors_deaths <- c("#FFB0B0", "#FF8787", "#DC143C")
    for (i in rev(seq_along(credible_levels))) {
      level <- credible_levels[i]
      ci_name <- paste0("ci_", gsub("\\.", "", as.character(level)))
      p_deaths <- p_deaths +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = .data[[paste0(ci_name, "_lower")]],
            ymax = .data[[paste0(ci_name, "_upper")]]
          ),
          fill = colors_deaths[i],
          alpha = 0.5
        )
    }
    
    # Add traces
    for (idx in trace_idx) {
      trace_data <- data.frame(
        time = seq_along(time_labels),
        value = predicted$deaths[idx, loc, ]
      )
      p_deaths <- p_deaths +
        ggplot2::geom_line(
          data = trace_data,
          ggplot2::aes(y = value),
          color = "darkred",
          alpha = 0.1,
          size = 0.3
        )
    }
    
    p_deaths <- p_deaths +
      ggplot2::geom_line(ggplot2::aes(y = mean),
                        color = "darkred", size = 1) +
      ggplot2::geom_point(ggplot2::aes(y = observed),
                         color = "black", size = 1.5, alpha = 0.7) +
      ggplot2::labs(
        title = paste0(location_names[loc], " - Deaths"),
        x = "Time",
        y = "Deaths"
      )
    
    # Combine if patchwork available
    if (requireNamespace("patchwork", quietly = TRUE)) {
      combined <- p_cases / p_deaths
    } else {
      combined <- list(cases = p_cases, deaths = p_deaths)
    }
    
    plots[[paste0("location_", loc)]] <- combined
  }
  
  return(plots)
}


#' Create Diagnostic Plots
#' @keywords internal
create_diagnostic_plots <- function(observed, predicted, location_names) {
  
  requireNamespace("ggplot2", quietly = TRUE)
  plots <- list()
  
  pred_mean_cases <- apply(predicted$cases, c(2, 3), mean, na.rm = TRUE)
  pred_mean_deaths <- apply(predicted$deaths, c(2, 3), mean, na.rm = TRUE)
  
  for (loc in seq_len(nrow(observed$cases))) {
    
    obs_c <- observed$cases[loc, ]
    obs_d <- observed$deaths[loc, ]
    pred_c <- pred_mean_cases[loc, ]
    pred_d <- pred_mean_deaths[loc, ]
    
    # Remove NAs for plotting
    valid_c <- !is.na(obs_c) & !is.na(pred_c)
    valid_d <- !is.na(obs_d) & !is.na(pred_d)
    
    # Q-Q plot for cases
    if (sum(valid_c) > 3) {
      qq_data_c <- data.frame(
        theoretical = sort(pred_c[valid_c]),
        sample = sort(obs_c[valid_c])
      )
      p_qq_cases <- ggplot2::ggplot(qq_data_c, 
                                    ggplot2::aes(x = theoretical, y = sample)) +
        ggplot2::geom_point(color = "steelblue", alpha = 0.6) +
        ggplot2::geom_abline(intercept = 0, slope = 1, 
                             color = "red", linetype = "dashed") +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::labs(title = "Q-Q Plot - Cases",
                     x = "Predicted Quantiles",
                     y = "Observed Quantiles")
    } else {
      p_qq_cases <- create_empty_plot("Insufficient data")
    }
    
    # Q-Q plot for deaths
    if (sum(valid_d) > 3) {
      qq_data_d <- data.frame(
        theoretical = sort(pred_d[valid_d]),
        sample = sort(obs_d[valid_d])
      )
      p_qq_deaths <- ggplot2::ggplot(qq_data_d,
                                     ggplot2::aes(x = theoretical, y = sample)) +
        ggplot2::geom_point(color = "darkred", alpha = 0.6) +
        ggplot2::geom_abline(intercept = 0, slope = 1,
                             color = "red", linetype = "dashed") +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::labs(title = "Q-Q Plot - Deaths",
                     x = "Predicted Quantiles",
                     y = "Observed Quantiles")
    } else {
      p_qq_deaths <- create_empty_plot("Insufficient data")
    }
    
    # Residual plots
    if (sum(valid_c) > 3) {
      resid_data_c <- data.frame(
        predicted = pred_c[valid_c],
        residual = obs_c[valid_c] - pred_c[valid_c]
      )
      p_resid_cases <- ggplot2::ggplot(resid_data_c,
                                       ggplot2::aes(x = predicted, y = residual)) +
        ggplot2::geom_point(color = "steelblue", alpha = 0.6) +
        ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        ggplot2::geom_smooth(method = "loess", se = FALSE, 
                            color = "blue", size = 0.5) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::labs(title = "Residuals - Cases",
                     x = "Predicted",
                     y = "Residual")
    } else {
      p_resid_cases <- create_empty_plot("Insufficient data")
    }
    
    if (sum(valid_d) > 3) {
      resid_data_d <- data.frame(
        predicted = pred_d[valid_d],
        residual = obs_d[valid_d] - pred_d[valid_d]
      )
      p_resid_deaths <- ggplot2::ggplot(resid_data_d,
                                        ggplot2::aes(x = predicted, y = residual)) +
        ggplot2::geom_point(color = "darkred", alpha = 0.6) +
        ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        ggplot2::geom_smooth(method = "loess", se = FALSE,
                            color = "darkred", size = 0.5) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::labs(title = "Residuals - Deaths",
                     x = "Predicted",
                     y = "Residual")
    } else {
      p_resid_deaths <- create_empty_plot("Insufficient data")
    }
    
    # Combine plots
    if (requireNamespace("patchwork", quietly = TRUE)) {
      combined <- (p_qq_cases | p_qq_deaths) / (p_resid_cases | p_resid_deaths) +
        patchwork::plot_annotation(
          title = paste0("Diagnostics - ", location_names[loc])
        )
    } else {
      combined <- list(
        qq_cases = p_qq_cases,
        qq_deaths = p_qq_deaths,
        resid_cases = p_resid_cases,
        resid_deaths = p_resid_deaths
      )
    }
    
    plots[[paste0("location_", loc)]] <- combined
  }
  
  return(plots)
}


#' Create Density Comparison Plots
#' @keywords internal
create_density_plots <- function(observed, predicted, location_names) {
  
  requireNamespace("ggplot2", quietly = TRUE)
  plots <- list()
  
  for (loc in seq_len(nrow(observed$cases))) {
    
    # Flatten predictions across replications and time
    pred_cases_flat <- as.vector(predicted$cases[, loc, ])
    pred_deaths_flat <- as.vector(predicted$deaths[, loc, ])
    
    # Observed data
    obs_cases_flat <- observed$cases[loc, ]
    obs_deaths_flat <- observed$deaths[loc, ]
    
    # Remove NAs
    pred_cases_flat <- pred_cases_flat[!is.na(pred_cases_flat)]
    pred_deaths_flat <- pred_deaths_flat[!is.na(pred_deaths_flat)]
    obs_cases_flat <- obs_cases_flat[!is.na(obs_cases_flat)]
    obs_deaths_flat <- obs_deaths_flat[!is.na(obs_deaths_flat)]
    
    # Cases density
    if (length(obs_cases_flat) > 5 && length(pred_cases_flat) > 5) {
      df_cases <- rbind(
        data.frame(type = "Observed", value = obs_cases_flat),
        data.frame(type = "Predicted", 
                  value = sample(pred_cases_flat, 
                                min(1000, length(pred_cases_flat))))
      )
      
      p_cases <- ggplot2::ggplot(df_cases, 
                                 ggplot2::aes(x = value, fill = type)) +
        ggplot2::geom_density(alpha = 0.5) +
        ggplot2::scale_fill_manual(values = c("Observed" = "black",
                                             "Predicted" = "steelblue")) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::labs(title = paste0(location_names[loc], " - Cases Distribution"),
                     x = "Cases",
                     y = "Density",
                     fill = "Type")
    } else {
      p_cases <- create_empty_plot("Insufficient data")
    }
    
    # Deaths density
    if (length(obs_deaths_flat) > 5 && length(pred_deaths_flat) > 5) {
      df_deaths <- rbind(
        data.frame(type = "Observed", value = obs_deaths_flat),
        data.frame(type = "Predicted",
                  value = sample(pred_deaths_flat,
                                min(1000, length(pred_deaths_flat))))
      )
      
      p_deaths <- ggplot2::ggplot(df_deaths,
                                  ggplot2::aes(x = value, fill = type)) +
        ggplot2::geom_density(alpha = 0.5) +
        ggplot2::scale_fill_manual(values = c("Observed" = "black",
                                             "Predicted" = "darkred")) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::labs(title = paste0(location_names[loc], " - Deaths Distribution"),
                     x = "Deaths",
                     y = "Density",
                     fill = "Type")
    } else {
      p_deaths <- create_empty_plot("Insufficient data")
    }
    
    if (requireNamespace("patchwork", quietly = TRUE)) {
      combined <- p_cases | p_deaths
    } else {
      combined <- list(cases = p_cases, deaths = p_deaths)
    }
    
    plots[[paste0("location_", loc)]] <- combined
  }
  
  return(plots)
}


#' Create Coverage Plots
#' @keywords internal
create_coverage_plots <- function(ppc_results, credible_levels, location_names) {
  
  requireNamespace("ggplot2", quietly = TRUE)
  
  # Calculate diagnostics
  diagnostics <- calc_ppc_diagnostics(ppc_results, credible_levels)
  
  # Prepare data
  df_coverage <- diagnostics$coverage
  df_long <- reshape(df_coverage, 
                     direction = "long",
                     varying = c("cases", "deaths"),
                     v.names = "coverage",
                     timevar = "type",
                     times = c("cases", "deaths"))
  df_long$type <- factor(df_long$type, levels = c("cases", "deaths"))
  
  # Create plot
  p <- ggplot2::ggplot(df_long, 
                       ggplot2::aes(x = level, y = coverage, 
                                   color = type, group = type)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_abline(intercept = 0, slope = 1, 
                        linetype = "dashed", color = "gray50") +
    ggplot2::scale_color_manual(values = c("cases" = "steelblue",
                                          "deaths" = "darkred")) +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = "Coverage of Credible Intervals",
      subtitle = "Diagonal line shows perfect calibration",
      x = "Credible Level",
      y = "Empirical Coverage",
      color = "Outcome"
    )
  
  return(list(coverage = p))
}


#' Calculate Credible Intervals
#' @keywords internal
calculate_credible_intervals <- function(predictions, levels) {
  
  ci_list <- list()
  
  for (level in levels) {
    alpha <- (1 - level) / 2
    lower <- apply(predictions, c(2, 3), quantile, 
                  probs = alpha, na.rm = TRUE)
    upper <- apply(predictions, c(2, 3), quantile,
                  probs = 1 - alpha, na.rm = TRUE)
    ci_list[[as.character(level)]] <- list(lower = lower, upper = upper)
  }
  
  return(ci_list)
}


#' Create Empty Plot
#' @keywords internal
create_empty_plot <- function(message = "No data") {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                     size = 5, color = "gray50") +
    ggplot2::theme_void() +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1)
}


#' Save List of Plots
#' @keywords internal
save_plot_list <- function(plots, output_dir, prefix, format, 
                          width, height, verbose) {
  
  for (name in names(plots)) {
    filename_base <- file.path(output_dir, paste0(prefix, "_", name))
    
    if (format %in% c("pdf", "both")) {
      filename <- paste0(filename_base, ".pdf")
      ggplot2::ggsave(filename, plot = plots[[name]], 
                     width = width, height = height, dpi = 300)
      if (verbose) cat("  Saved: ", basename(filename), "\n", sep = "")
    }
    
    if (format %in% c("png", "both")) {
      filename <- paste0(filename_base, ".png")
      ggplot2::ggsave(filename, plot = plots[[name]],
                     width = width, height = height, dpi = 300)
      if (verbose) cat("  Saved: ", basename(filename), "\n", sep = "")
    }
  }
}


#' Reorganize Plots by Location
#' @keywords internal
reorganize_plots_by_location <- function(all_plots, location_names) {
  
  plots_by_location <- list()
  
  for (loc_idx in seq_along(location_names)) {
    loc_name <- gsub("[^A-Za-z0-9_]", "_", location_names[loc_idx])
    loc_key <- paste0("location_", loc_idx)
    
    plots_by_location[[loc_name]] <- list()
    
    for (plot_type in names(all_plots)) {
      if (loc_key %in% names(all_plots[[plot_type]])) {
        plots_by_location[[loc_name]][[plot_type]] <- 
          all_plots[[plot_type]][[loc_key]]
      }
    }
  }
  
  # Also keep the original organization
  plots_by_location$by_type <- all_plots
  
  return(plots_by_location)
}