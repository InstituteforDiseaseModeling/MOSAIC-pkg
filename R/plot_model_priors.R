#' Plot Prior Distributions for MOSAIC Model Parameters
#'
#' Creates visualizations of global and location-specific parameter priors.
#' Generates three types of plots: global parameters, initial conditions per location,
#' and other location-specific parameters.
#'
#' @param PATHS A list containing paths for the MOSAIC project, from \code{get_paths()}
#' @param priors A priors list object. If NULL, uses MOSAIC::priors_default
#' @param config A configuration object containing date_start and other settings. If NULL, uses default date.
#' @param output_dir Output directory for plots. If NULL, uses PATHS$DOCS_FIGURES
#'
#' @return Invisibly returns a list of generated plot objects
#'
#' @export
plot_model_priors <- function(PATHS = NULL, priors = NULL, config = NULL, output_dir = NULL) {

  library(ggplot2)
  library(dplyr)
  library(patchwork)
  library(cowplot)

  # Setup priors
  if (is.null(priors)) {
    if (requireNamespace("MOSAIC", quietly = TRUE)) {
      priors <- MOSAIC::priors_default
    } else {
      stop("Please provide priors object or ensure MOSAIC::priors_default is available")
    }
  }

  # Setup output directory
  if (is.null(output_dir)) {
    if (!is.null(PATHS$DOCS_FIGURES)) {
      output_dir <- PATHS$DOCS_FIGURES
    } else if (dir.exists("../MOSAIC-docs/figures")) {
      output_dir <- "../MOSAIC-docs/figures"
    } else {
      output_dir <- "figures"
    }
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  plots <- list()

  # Extract location codes
  location_codes <- NULL
  if (!is.null(priors$parameters_location)) {
    for (param in names(priors$parameters_location)) {
      if (!is.null(priors$parameters_location[[param]]$parameters$location)) {
        location_codes <- names(priors$parameters_location[[param]]$parameters$location)
        break
      }
    }
  }

  # =========================================================================
  # PLOT 1: Global Parameters
  # =========================================================================
  cat("Plotting global parameter distributions...\n")

  if (!is.null(priors$parameters_global)) {
    plot_list <- list()

    for (param_name in names(priors$parameters_global)) {
      param <- priors$parameters_global[[param_name]]
      if (is.null(param$distribution)) next

      x <- NULL; y <- NULL
      dist_str <- NULL
      mean_val <- NULL

      if (param$distribution == "gamma" && !is.null(param$parameters)) {
        shape <- param$parameters$shape
        rate <- param$parameters$rate
        if (!is.null(shape) && !is.null(rate)) {
          x_max <- qgamma(0.99, shape, rate)
          x <- seq(0, x_max, length.out = 1000)
          y <- dgamma(x, shape, rate)
          dist_str <- sprintf("Gamma(%.2f, %.2f)", shape, rate)
          mean_val <- shape / rate
        }
      } else if (param$distribution == "beta" && !is.null(param$parameters)) {
        shape1 <- param$parameters$shape1
        shape2 <- param$parameters$shape2
        if (!is.null(shape1) && !is.null(shape2)) {
          x <- seq(0, 1, length.out = 1000)
          y <- dbeta(x, shape1, shape2)
          if (shape1 < 1) {
            dist_str <- sprintf("Beta(%.2g, %.0f)", shape1, shape2)
          } else {
            dist_str <- sprintf("Beta(%.1f, %.1f)", shape1, shape2)
          }
          mean_val <- shape1 / (shape1 + shape2)
        }
      } else if (param$distribution == "lognormal" && !is.null(param$parameters)) {
        # Handle both parameter naming conventions: (meanlog, sdlog) and (mean, sd)
        meanlog_val <- NULL
        sdlog_val <- NULL
        
        if (!is.null(param$parameters$meanlog) && !is.null(param$parameters$sdlog)) {
          # Standard R lognormal parameterization
          meanlog_val <- param$parameters$meanlog
          sdlog_val <- param$parameters$sdlog
        } else if (!is.null(param$parameters$mean) && !is.null(param$parameters$sd)) {
          # Alternative parameterization: convert mean and sd to meanlog and sdlog
          # For lognormal: if X ~ LN(meanlog, sdlog), then E[X] = exp(meanlog + sdlog^2/2)
          # and Var[X] = (exp(sdlog^2) - 1) * exp(2*meanlog + sdlog^2)
          mu <- param$parameters$mean
          sigma <- param$parameters$sd
          if (mu > 0 && sigma > 0) {
            # Convert to log-scale parameters
            meanlog_val <- log(mu^2 / sqrt(mu^2 + sigma^2))
            sdlog_val <- sqrt(log(1 + sigma^2 / mu^2))
          }
        }
        
        if (!is.null(meanlog_val) && !is.null(sdlog_val)) {
          x_max <- qlnorm(0.99, meanlog_val, sdlog_val)
          x <- seq(0, x_max, length.out = 1000)
          y <- dlnorm(x, meanlog_val, sdlog_val)
          dist_str <- sprintf("LogNormal(%.2g, %.2g)", meanlog_val, sdlog_val)
          mean_val <- exp(meanlog_val + sdlog_val^2/2)
        }
      } else if (param$distribution == "uniform" && !is.null(param$parameters)) {
        if (!is.null(param$parameters$min) && !is.null(param$parameters$max)) {
          buffer <- (param$parameters$max - param$parameters$min) * 0.1
          x <- seq(param$parameters$min - buffer, param$parameters$max + buffer, length.out = 1000)
          y <- dunif(x, param$parameters$min, param$parameters$max)
          dist_str <- sprintf("Uniform(%.2f, %.2f)", param$parameters$min, param$parameters$max)
          mean_val <- (param$parameters$min + param$parameters$max) / 2
        }
      }

      if (!is.null(x) && !is.null(y) && !is.null(dist_str)) {
        plot_data <- data.frame(x = x, y = y)

        # Format mean value
        mean_str <- if (!is.null(mean_val)) {
          if (mean_val < 0.01) {
            sprintf("Mean: %.4f", mean_val)
          } else if (mean_val < 1) {
            sprintf("Mean: %.3f", mean_val)
          } else {
            sprintf("Mean: %.2f", mean_val)
          }
        } else {
          ""
        }

        # Create subtitle
        subtitle_str <- if (mean_str != "") {
          paste0(dist_str, "\n", mean_str)
        } else {
          dist_str
        }

        # Create individual plot
        p <- ggplot(plot_data, aes(x = x, y = y)) +
          geom_area(fill = "#457b9d", alpha = 0.3) +
          geom_line(color = "#457b9d", linewidth = 1) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray50", face = "italic"),
            axis.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color = "gray90")
          ) +
          labs(
            title = param_name,
            subtitle = subtitle_str
          )

        plot_list[[param_name]] <- p
      }
    }

    if (length(plot_list) > 0) {
      # Determine grid layout
      n_plots <- length(plot_list)
      ncol <- min(4, n_plots)
      nrow <- ceiling(n_plots / ncol)

      # Combine plots using cowplot
      p_global <- cowplot::plot_grid(plotlist = plot_list, ncol = ncol, nrow = nrow)

      # Create subtitle with version information
      subtitle_text <- ""
      if (!is.null(priors$metadata$version)) {
        subtitle_text <- paste0("Priors version: ", priors$metadata$version)
      }

      # Add title with subtitle
      if (subtitle_text != "") {
        title <- cowplot::ggdraw() +
          cowplot::draw_label("Global Parameter Prior Distributions",
                             fontface = 'bold', size = 16, x = 0.5, y = 0.7, hjust = 0.5) +
          cowplot::draw_label(subtitle_text,
                             fontface = 'plain', size = 12, x = 0.5, y = 0.3, hjust = 0.5, color = "gray30")
      } else {
        title <- cowplot::ggdraw() +
          cowplot::draw_label("Global Parameter Prior Distributions",
                             fontface = 'bold', size = 16, x = 0.5, hjust = 0.5)
      }

      # Combine title and plots
      p_global <- cowplot::plot_grid(title, p_global,
                                     ncol = 1, rel_heights = c(0.08, 0.92))

      # Add common axis labels
      p_global <- cowplot::add_sub(p_global, "Value", size = 12, vjust = 0)
      p_global <- cowplot::ggdraw(p_global) +
        cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 12)

      ggsave(file.path(output_dir, "priors_global.pdf"), p_global, width = 12, height = 10)
      plots$global <- p_global
      cat("  Saved: priors_global.pdf\n")
    }
  }

  # =========================================================================
  # PLOT 2 & 3 COMBINED: Initial Conditions + Location-Specific Parameters
  # =========================================================================
  if (!is.null(location_codes) && length(location_codes) > 0) {
    cat("Plotting combined location-specific priors (initial conditions + parameters)...\n")

    ic_params <- c("prop_E_initial", "prop_I_initial", "prop_R_initial",
                   "prop_S_initial", "prop_V1_initial", "prop_V2_initial")
    compartment_labels <- c(
      prop_E_initial = "E (Exposed)",
      prop_I_initial = "I (Infected)",
      prop_R_initial = "R (Recovered)",
      prop_S_initial = "S (Susceptible)",
      prop_V1_initial = "V1 (One dose)",
      prop_V2_initial = "V2 (Two doses)"
    )

    # Define colors for each parameter - using same color as priors in posterior plots
    # All use the same light blue color for consistency
    param_colors <- c(
      prop_S_initial = "#457b9d",      # Light blue
      prop_E_initial = "#457b9d",      # Light blue
      prop_I_initial = "#457b9d",      # Light blue
      prop_R_initial = "#457b9d",      # Light blue
      prop_V1_initial = "#457b9d",     # Light blue
      prop_V2_initial = "#457b9d"      # Light blue
    )

    # Load population data once for all countries using PATHS
    pop_data <- NULL

    if (!is.null(PATHS) && !is.null(PATHS$DATA_PROCESSED)) {
      un_pop_file <- file.path(PATHS$DATA_PROCESSED, "demographics",
                               "UN_world_population_prospects_daily.csv")
      if (file.exists(un_pop_file)) {
        pop_data <- read.csv(un_pop_file, stringsAsFactors = FALSE)
      }
    }

    # Get target date from config
    target_date <- "2023-01-01"  # Default date
    if (!is.null(config$date_start)) {
      target_date <- as.character(config$date_start)
    }

    for (iso in location_codes) {
      ic_plot_list <- list()  # Initial conditions plots
      param_plot_list <- list()  # Other parameter plots

      # Get population size from pre-loaded UN data
      pop_size <- NULL

      if (!is.null(pop_data)) {
        # Filter for this country and date (or closest available)
        country_data <- pop_data[pop_data$iso_code == iso, ]
        if (nrow(country_data) > 0) {
          # Try exact date match first
          exact_match <- country_data[country_data$date == target_date, ]
          if (nrow(exact_match) > 0) {
            pop_size <- exact_match$total_population[1]
          } else {
            # Use most recent available data
            pop_size <- tail(country_data$total_population, 1)
          }
        }
      }

      # -----------------------------------------------------------------------
      # Part A: Initial Condition Plots
      # -----------------------------------------------------------------------
      for (param_name in ic_params) {
        if (!is.null(priors$parameters_location[[param_name]])) {
          loc_param <- priors$parameters_location[[param_name]]$parameters$location[[iso]]

          if (!is.null(loc_param) && !is.null(loc_param$shape1) && !is.null(loc_param$shape2)) {
            compartment_label <- compartment_labels[param_name]
            plot_color <- param_colors[param_name]

            # Calculate mean
            mean_val <- loc_param$shape1 / (loc_param$shape1 + loc_param$shape2)

            # Generate x values based on distribution characteristics
            if (param_name %in% c("prop_E_initial", "prop_I_initial")) {
              # For highly skewed distributions, use wider range to capture full density
              x_max <- qbeta(0.9999, loc_param$shape1, loc_param$shape2)
              # Ensure we capture enough of the distribution
              x_max <- max(x_max, mean_val * 5)  # At least 5x the mean
              x <- seq(0, x_max, length.out = 1000)
            } else if (param_name == "prop_V2_initial" && loc_param$shape2 > 50) {
              # For V2 with high shape2, focus on small range
              x_max <- min(0.1, qbeta(0.999, loc_param$shape1, loc_param$shape2))
              x <- seq(0, x_max, length.out = 1000)
            } else {
              # For other distributions, use full range
              x <- seq(0, 1, length.out = 1000)
            }

            y <- dbeta(x, loc_param$shape1, loc_param$shape2)

            # Create individual plot
            plot_data <- data.frame(x = x, y = y)

            # Format Beta parameters for subtitle
            if (loc_param$shape1 < 1) {
              beta_str <- sprintf("Beta(%.2g, %.0f)", loc_param$shape1, loc_param$shape2)
            } else {
              beta_str <- sprintf("Beta(%.1f, %.1f)", loc_param$shape1, loc_param$shape2)
            }

            # Format mean percentage
            if (mean_val < 0.0001) {
              mean_str <- sprintf("%.4f%%", mean_val * 100)
            } else if (mean_val < 0.01) {
              mean_str <- sprintf("%.3f%%", mean_val * 100)
            } else {
              mean_str <- sprintf("%.1f%%", mean_val * 100)
            }

            # Add population count if available
            subtitle_str <- paste0(beta_str, "\nMean: ", mean_str)
            if (!is.null(pop_size)) {
              pop_count <- formatC(round(mean_val * pop_size), format = "d", big.mark = ",")
              subtitle_str <- paste0(subtitle_str, " (", pop_count, ")")
            }

            p <- ggplot(plot_data, aes(x = x, y = y)) +
              geom_area(fill = plot_color, alpha = 0.3) +
              geom_line(color = plot_color, linewidth = 1.2) +
              geom_vline(xintercept = mean_val, linetype = "dashed",
                        color = "gray50", alpha = 0.7) +
              scale_x_continuous(labels = function(x) {
                if (param_name %in% c("prop_E_initial", "prop_I_initial")) {
                  # For very small values, use scientific notation or more decimal places
                  if (max(x) < 1e-4) {
                    scales::scientific(x)
                  } else {
                    sprintf("%.6f", x)
                  }
                } else {
                  # For larger values, use percentage or decimal
                  ifelse(x < 0.01, sprintf("%.3f", x), sprintf("%.2f", x))
                }
              }) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 10, hjust = 0.5,
                                           color = "gray50", face = "italic"),
                axis.title = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(color = "gray90"),
                panel.background = element_rect(fill = "white", color = NA)
              ) +
              labs(
                title = compartment_label,
                subtitle = subtitle_str
              )

            ic_plot_list[[param_name]] <- p
          }
        }
      }

      # -----------------------------------------------------------------------
      # Part B: Transmission Parameter Plots (Primary and Derived) - REDS
      # -----------------------------------------------------------------------

      transmission_plot_list <- list()

      # Total transmission rate (beta_j0_tot)
      if (!is.null(priors$parameters_location$beta_j0_tot)) {
        tot_params <- priors$parameters_location$beta_j0_tot$parameters$location[[iso]]
        
        # Determine distribution type
        dist_type <- ifelse(!is.null(priors$parameters_location$beta_j0_tot$distribution),
                            priors$parameters_location$beta_j0_tot$distribution,
                            "lognormal")  # Default to lognormal if not specified
        
        if (!is.null(tot_params)) {
          if (dist_type == "gompertz" && !is.null(tot_params$b) && !is.null(tot_params$eta)) {
            # Handle Gompertz distribution
            x_max <- qgompertz(0.99, b = tot_params$b, eta = tot_params$eta)
            x <- seq(0, x_max, length.out = 1000)
            y <- dgompertz(x, b = tot_params$b, eta = tot_params$eta)
            
            # Calculate statistics (use sampling for mean/median since closed forms are complex)
            set.seed(123)
            samples <- rgompertz(10000, b = tot_params$b, eta = tot_params$eta)
            median_val <- median(samples)
            mean_val <- mean(samples)
            mode_val <- (1 / tot_params$b) * log(tot_params$eta / tot_params$b)
            
            transmission_plot_list$beta_tot <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
              geom_area(fill = "#457b9d", alpha = 0.3) +
              geom_line(color = "#457b9d", linewidth = 1.2) +
              geom_vline(xintercept = mode_val, linetype = "dashed", color = "red", alpha = 0.7) +
              geom_vline(xintercept = median_val, linetype = "dashed", color = "gray50", alpha = 0.7) +
              theme_minimal() +
              scale_x_continuous(labels = scales::scientific) +
              labs(title = "Total Transmission Rate (β₀)",
                   subtitle = sprintf("Gompertz(b=%.1f, η=%.1f)\nMode: %.2e, Median: %.2e",
                                    tot_params$b, tot_params$eta, mode_val, median_val),
                   x = "Rate", y = "Density")
            
          } else if (dist_type == "lognormal" && !is.null(tot_params$meanlog) && !is.null(tot_params$sdlog)) {
            # Handle Lognormal distribution (original code)
            x_max <- qlnorm(0.99, tot_params$meanlog, tot_params$sdlog)
            x <- seq(0, x_max, length.out = 1000)
            y <- dlnorm(x, tot_params$meanlog, tot_params$sdlog)
            
            # Calculate median and mean
            median_val <- exp(tot_params$meanlog)
            mean_val <- exp(tot_params$meanlog + tot_params$sdlog^2/2)
            
            transmission_plot_list$beta_tot <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
              geom_area(fill = "#457b9d", alpha = 0.3) +
              geom_line(color = "#457b9d", linewidth = 1.2) +
              geom_vline(xintercept = median_val, linetype = "dashed", color = "gray50", alpha = 0.7) +
              theme_minimal() +
              scale_x_continuous(labels = scales::scientific) +
              labs(title = "Total Transmission Rate (β₀)",
                   subtitle = sprintf("LogNormal(%.3f, %.2f)\nMedian: %.2e",
                                    tot_params$meanlog, tot_params$sdlog, median_val),
                   x = "Rate", y = "Density")
          }
        }
      }

      # Proportion of human-to-human transmission (p_beta)
      if (!is.null(priors$parameters_location$p_beta)) {
        p_params <- priors$parameters_location$p_beta$parameters$location[[iso]]
        if (!is.null(p_params) && !is.null(p_params$shape1) && !is.null(p_params$shape2)) {
          x <- seq(0, 1, length.out = 1000)
          y <- dbeta(x, p_params$shape1, p_params$shape2)

          # Calculate mean
          mean_val <- p_params$shape1 / (p_params$shape1 + p_params$shape2)

          transmission_plot_list$p_beta <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
            geom_area(fill = "#457b9d", alpha = 0.3) +
            geom_line(color = "#457b9d", linewidth = 1.2) +
            geom_vline(xintercept = mean_val, linetype = "dashed", color = "gray50", alpha = 0.7) +
            theme_minimal() +
            labs(title = "Proportion Human Transmission (p_β)",
                 subtitle = sprintf("Beta(%.2f, %.2f)\nMean: %.3f",
                                  p_params$shape1, p_params$shape2, mean_val),
                 x = "Proportion", y = "Density")
        }
      }

      # Generate derived distributions for beta_j0_hum and beta_j0_env
      if (!is.null(priors$parameters_location$beta_j0_tot) &&
          !is.null(priors$parameters_location$p_beta)) {

        tot_params <- priors$parameters_location$beta_j0_tot$parameters$location[[iso]]
        p_params <- priors$parameters_location$p_beta$parameters$location[[iso]]

        if (!is.null(tot_params) && !is.null(p_params)) {
          # Sample from the priors to get derived distributions
          n_samples <- 10000
          beta_tot_samples <- NULL  # Initialize to NULL
          
          # Handle different distribution types for beta_j0_tot
          if (!is.null(priors$parameters_location$beta_j0_tot$distribution)) {
            dist_type <- priors$parameters_location$beta_j0_tot$distribution
            if (dist_type == "gompertz") {
              # Sample from Gompertz distribution
              if (!is.null(tot_params$b) && !is.null(tot_params$eta)) {
                beta_tot_samples <- rgompertz(n_samples, b = tot_params$b, eta = tot_params$eta)
              } else {
                warning("Gompertz parameters (b, eta) not found for beta_j0_tot")
              }
            } else if (dist_type == "lognormal") {
              # Sample from lognormal distribution
              if (!is.null(tot_params$meanlog) && !is.null(tot_params$sdlog)) {
                beta_tot_samples <- rlnorm(n_samples, tot_params$meanlog, tot_params$sdlog)
              } else {
                warning("Lognormal parameters (meanlog, sdlog) not found for beta_j0_tot")
              }
            } else {
              warning(paste("Unsupported distribution type for beta_j0_tot:", dist_type))
            }
          } else {
            # Fallback: assume lognormal if distribution type not specified
            if (!is.null(tot_params$meanlog) && !is.null(tot_params$sdlog) && 
                is.numeric(tot_params$meanlog) && is.numeric(tot_params$sdlog)) {
              beta_tot_samples <- rlnorm(n_samples, tot_params$meanlog, tot_params$sdlog)
            } else {
              warning("Distribution type not specified and lognormal parameters not found or invalid")
            }
          }
          
          # Only proceed if we have valid beta_tot_samples
          if (is.null(beta_tot_samples) || all(is.na(beta_tot_samples))) {
            # Skip derived distributions if beta_tot_samples couldn't be generated
            # Do nothing - just skip this section
          } else if (!is.null(p_params$shape1) && !is.null(p_params$shape2)) {
            p_beta_samples <- rbeta(n_samples, p_params$shape1, p_params$shape2)
            
            # Derive the transmission components
            beta_hum_derived <- beta_tot_samples * p_beta_samples
            beta_env_derived <- beta_tot_samples * (1 - p_beta_samples)

            # Create density plots for derived distributions
            dens_hum <- density(beta_hum_derived)
            dens_env <- density(beta_env_derived)

            # Plot derived human transmission
            transmission_plot_list$beta_hum_derived <- ggplot(data.frame(x = dens_hum$x, y = dens_hum$y),
                                                       aes(x = x, y = y)) +
              geom_area(fill = "#457b9d", alpha = 0.3) +
              geom_line(color = "#457b9d", linewidth = 1.2) +
              theme_minimal() +
              scale_x_continuous(labels = scales::scientific) +
              labs(title = "Human Transmission (Derived)",
                   subtitle = sprintf("β_hum = p_β × β_tot\nMedian: %.2e", median(beta_hum_derived)),
                   x = "Rate", y = "Density")

            # Plot derived environmental transmission
            transmission_plot_list$beta_env_derived <- ggplot(data.frame(x = dens_env$x, y = dens_env$y),
                                                       aes(x = x, y = y)) +
              geom_area(fill = "#457b9d", alpha = 0.3) +
              geom_line(color = "#457b9d", linewidth = 1.2) +
              theme_minimal() +
              scale_x_continuous(labels = scales::scientific) +
              labs(title = "Environmental Transmission (Derived)",
                   subtitle = sprintf("β_env = (1-p_β) × β_tot\nMedian: %.2e", median(beta_env_derived)),
                   x = "Rate", y = "Density")
          }
        }
      }

      # -----------------------------------------------------------------------
      # Part C: Seasonality Parameters - GREENS
      # -----------------------------------------------------------------------

      seasonality_plot_list <- list()

      # Seasonality parameters - all use same light blue
      seasonality_params <- c("a1", "a2", "b1", "b2")
      seasonality_colors <- c(a1 = "#457b9d", a2 = "#457b9d", b1 = "#457b9d", b2 = "#457b9d")
      seasonality_titles <- c(a1 = "Seasonality: Cosine 1", a2 = "Seasonality: Cosine 2",
                             b1 = "Seasonality: Sine 1", b2 = "Seasonality: Sine 2")

      for (param_name in seasonality_params) {
        if (!is.null(priors$parameters_location[[param_name]])) {
          param_vals <- priors$parameters_location[[param_name]]$parameters$location[[iso]]
          if (!is.null(param_vals) && !is.null(param_vals$mean) && !is.null(param_vals$sd)) {
            if (is.finite(param_vals$mean) && is.finite(param_vals$sd) && param_vals$sd > 0) {
              x <- seq(param_vals$mean - 4*param_vals$sd, param_vals$mean + 4*param_vals$sd, length.out = 1000)
              y <- dnorm(x, param_vals$mean, param_vals$sd)

              seasonality_plot_list[[param_name]] <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
                geom_area(fill = seasonality_colors[param_name], alpha = 0.3) +
                geom_line(color = seasonality_colors[param_name], linewidth = 1.2) +
                geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
                theme_minimal() +
                labs(title = seasonality_titles[param_name],
                     subtitle = sprintf("Normal(%.3f, %.3f)", param_vals$mean, param_vals$sd),
                     x = "Value", y = "Density")
            }
          }
        }
      }

      # -----------------------------------------------------------------------
      # Part D: Other Location-Specific Parameter Plots - PURPLES
      # -----------------------------------------------------------------------

      # Travel probability
      if (!is.null(priors$parameters_location$tau_i)) {
        tau_params <- priors$parameters_location$tau_i$parameters$location[[iso]]
        if (!is.null(tau_params) && !is.null(tau_params$shape1) && !is.null(tau_params$shape2)) {
          x <- seq(0, 1, length.out = 1000)
          y <- dbeta(x, tau_params$shape1, tau_params$shape2)

          # Focus on relevant range for highly skewed distributions
          if (tau_params$shape2 > 10000) {
            mean_val <- tau_params$shape1 / (tau_params$shape1 + tau_params$shape2)
            x_max <- min(1, mean_val * 100)
            x <- seq(0, x_max, length.out = 1000)
            y <- dbeta(x, tau_params$shape1, tau_params$shape2)
          }

          param_plot_list$tau <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
            geom_area(fill = "#457b9d", alpha = 0.3) +
            geom_line(color = "#457b9d", linewidth = 1.2) +
            theme_minimal() +
            scale_x_continuous(labels = scales::scientific) +
            labs(title = "Travel Probability",
                 subtitle = sprintf("Beta(%.2f, %.2f)", tau_params$shape1, tau_params$shape2),
                 x = "Probability", y = "Density")
        }
      }

      # WASH coverage (theta_j)
      if (!is.null(priors$parameters_location$theta_j)) {
        theta_params <- priors$parameters_location$theta_j$parameters$location[[iso]]
        if (!is.null(theta_params) && !is.null(theta_params$shape1) && !is.null(theta_params$shape2)) {
          x <- seq(0, 1, length.out = 1000)
          y <- dbeta(x, theta_params$shape1, theta_params$shape2)

          # Calculate mean and point estimate
          mean_val <- theta_params$shape1 / (theta_params$shape1 + theta_params$shape2)

          # Create subtitle with point estimate if available
          subtitle_str <- sprintf("Beta(%.2f, %.2f)", theta_params$shape1, theta_params$shape2)
          if (!is.null(theta_params$point_estimate)) {
            subtitle_str <- paste0(subtitle_str, sprintf("\nPoint est: %.3f, Mean: %.3f",
                                                        theta_params$point_estimate, mean_val))
          } else {
            subtitle_str <- paste0(subtitle_str, sprintf("\nMean: %.3f", mean_val))
          }

          plot_data <- data.frame(x = x, y = y)

          param_plot_list$theta_j <- ggplot(plot_data, aes(x = x, y = y)) +
            geom_area(fill = "#457b9d", alpha = 0.3) +  # Light blue for WASH
            geom_line(color = "#457b9d", linewidth = 1.2) +
            geom_vline(xintercept = mean_val, linetype = "dashed",
                      color = "gray50", alpha = 0.7) +
            theme_minimal() +
            scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
            labs(title = "WASH Coverage (θ_j)",
                 subtitle = subtitle_str,
                 x = "Coverage Proportion", y = "Density") +
            theme(plot.subtitle = element_text(size = 9))
        }
      }

      # Note: Seasonality parameters have been moved to Part C above

      # -----------------------------------------------------------------------
      # Combine all parameter sections into Single Plot
      # -----------------------------------------------------------------------
      if (length(ic_plot_list) > 0 || length(transmission_plot_list) > 0 ||
          length(seasonality_plot_list) > 0 || length(param_plot_list) > 0) {

        # Create initial conditions grid (3 columns, preserving square aspect)
        ic_combined <- NULL
        if (length(ic_plot_list) > 0) {
          ic_combined <- cowplot::plot_grid(plotlist = ic_plot_list, ncol = 3, nrow = 2)


          # Add Initial Conditions heading
          ic_heading <- cowplot::ggdraw() +
            cowplot::draw_label("Initial Conditions",
                               fontface = 'bold', size = 14, x = 0.5, y = 0.5, hjust = 0.5)

          # Add common axis labels for IC plot
          ic_combined <- cowplot::add_sub(ic_combined, "Proportion of Population", size = 10, vjust = 0)
          ic_combined <- cowplot::ggdraw(ic_combined) +
            cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 10)
        }

        # Create transmission parameters grid (2x2 for the 4 transmission plots)
        transmission_combined <- NULL
        if (length(transmission_plot_list) > 0) {
          # Arrange transmission plots in 2x2 grid
          transmission_combined <- cowplot::plot_grid(plotlist = transmission_plot_list,
                                                      ncol = 2, nrow = 2)

          # Add Transmission Parameters heading
          transmission_heading <- cowplot::ggdraw() +
            cowplot::draw_label("Transmission Parameters",
                               fontface = 'bold', size = 14, x = 0.5, y = 0.5, hjust = 0.5)
        }

        # Create seasonality parameters grid (1x4 for the 4 seasonality plots on same row)
        seasonality_combined <- NULL
        if (length(seasonality_plot_list) > 0) {
          # Arrange seasonality plots in single row
          seasonality_combined <- cowplot::plot_grid(plotlist = seasonality_plot_list,
                                                     ncol = 4, nrow = 1)

          # Add Seasonality Parameters heading
          seasonality_heading <- cowplot::ggdraw() +
            cowplot::draw_label("Seasonality Parameters",
                               fontface = 'bold', size = 14, x = 0.5, y = 0.5, hjust = 0.5)
        }

        # Create other parameters grid
        param_combined <- NULL
        if (length(param_plot_list) > 0) {
          # Arrange parameter plots in grid (use 3 columns to match IC layout)
          n_param_plots <- length(param_plot_list)
          n_cols <- 3
          n_rows <- ceiling(n_param_plots / n_cols)

          param_combined <- cowplot::plot_grid(plotlist = param_plot_list,
                                              ncol = n_cols, nrow = n_rows)

          # Add Other Parameters heading
          param_heading <- cowplot::ggdraw() +
            cowplot::draw_label("Other Location-Specific Parameters",
                               fontface = 'bold', size = 14, x = 0.5, y = 0.5, hjust = 0.5)
        }

        # Combine everything into final plot
        final_plot_list <- list()

        # Add main title
        title_text <- paste0("Location-Specific Prior Distributions: ", iso)
        subtitle_parts <- paste0("Start date: ", target_date)
        if (!is.null(priors$metadata$version)) {
          subtitle_parts <- paste0(subtitle_parts, " | Priors version: ", priors$metadata$version)
        }
        subtitle_text <- subtitle_parts

        main_title <- cowplot::ggdraw() +
          cowplot::draw_label(title_text,
                             fontface = 'bold', size = 16, x = 0.5, y = 0.7, hjust = 0.5) +
          cowplot::draw_label(subtitle_text,
                             fontface = 'plain', size = 12, x = 0.5, y = 0.3, hjust = 0.5, color = "gray30")

        final_plot_list[[1]] <- main_title

        # Add IC section if exists
        if (!is.null(ic_combined)) {
          final_plot_list[[length(final_plot_list) + 1]] <- ic_heading
          final_plot_list[[length(final_plot_list) + 1]] <- ic_combined
        }

        # Add transmission section if exists
        if (!is.null(transmission_combined)) {
          final_plot_list[[length(final_plot_list) + 1]] <- transmission_heading
          final_plot_list[[length(final_plot_list) + 1]] <- transmission_combined
        }

        # Add seasonality section if exists
        if (!is.null(seasonality_combined)) {
          final_plot_list[[length(final_plot_list) + 1]] <- seasonality_heading
          final_plot_list[[length(final_plot_list) + 1]] <- seasonality_combined
        }

        # Add parameter section if exists
        if (!is.null(param_combined)) {
          final_plot_list[[length(final_plot_list) + 1]] <- param_heading
          final_plot_list[[length(final_plot_list) + 1]] <- param_combined
        }

        # Add footer
        footer <- cowplot::ggdraw() +
          cowplot::draw_label("Initial condition proportions are normalized to sum to 1.0 during sampling",
                             fontface = 'italic', size = 10, x = 0.5, hjust = 0.5, color = "gray50")

        final_plot_list[[length(final_plot_list) + 1]] <- footer

        # Calculate relative heights
        heights <- c(0.05)  # Title (slightly smaller proportion with larger total height)
        if (!is.null(ic_combined)) {
          heights <- c(heights, 0.03, 0.32)  # IC heading + IC plots
        }
        if (!is.null(transmission_combined)) {
          heights <- c(heights, 0.03, 0.35)  # Transmission heading + transmission plots (2x2 grid, more space)
        }
        if (!is.null(seasonality_combined)) {
          heights <- c(heights, 0.03, 0.15)  # Seasonality heading + seasonality plots (single row)
        }
        if (!is.null(param_combined)) {
          param_height <- 0.35 * (n_rows / 2)  # Scale based on number of rows
          heights <- c(heights, 0.03, param_height)  # Param heading + param plots
        }
        heights <- c(heights, 0.02)  # Footer

        # Normalize heights
        heights <- heights / sum(heights)

        # Combine all sections
        p_combined_final <- cowplot::plot_grid(plotlist = final_plot_list,
                                              ncol = 1, rel_heights = heights)

        # Save combined plot
        filename <- file.path(output_dir, paste0("priors_", iso, ".pdf"))

        # Calculate appropriate height based on content
        # Base height accounts for title, IC section, transmission section, and footer
        plot_height <- 16  # Increased base height for better proportions
        if (!is.null(param_combined)) {
          plot_height <- plot_height + (n_rows - 1) * 3  # Add extra height for additional parameter rows
        }

        tryCatch({
          ggsave(filename, p_combined_final, width = 14, height = plot_height)
          if (!exists("plots$location_specific")) plots$location_specific <- list()
          plots$location_specific[[iso]] <- p_combined_final
          cat("  Saved:", basename(filename), "\n")
        }, error = function(e) {
          cat("  Warning: Could not save", basename(filename), "-", e$message, "\n")
        })
      }
    }
  }

  return(invisible(plots))
}
