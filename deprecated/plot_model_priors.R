#' Plot Prior Distributions for MOSAIC Model Parameters
#'
#' Creates visualizations of global and location-specific parameter priors using
#' metadata from the estimated_parameters inventory. Parameters are organized by
#' biological category and ordered consistently across all plots.
#'
#' @param PATHS A list containing paths for the MOSAIC project, from \code{get_paths()}
#' @param priors A priors list object. If NULL, uses MOSAIC::priors_default
#' @param config A configuration object containing date_start and other settings. If NULL, uses default date.
#' @param output_dir Output directory for plots. If NULL, uses PATHS$DOCS_FIGURES
#' @param selected_locations Character vector of ISO codes for location-specific plots. If NULL, plots all available locations.
#'
#' @return Invisibly returns a list of generated plot objects
#'
#' @export
plot_model_priors <- function(PATHS = NULL, priors = NULL, config = NULL,
                             output_dir = NULL, selected_locations = NULL) {

  library(ggplot2)
  library(dplyr)
  library(cowplot)

  # Store original warning setting for later restoration
  old_warn <- getOption("warn")

  # Load estimated parameters inventory
  data("estimated_parameters", package = "MOSAIC")

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
      if (!is.null(priors$parameters_location[[param]]$location)) {
        location_codes <- names(priors$parameters_location[[param]]$location)
        break
      }
    }
  }

  # Filter locations if specified
  if (!is.null(selected_locations)) {
    location_codes <- intersect(location_codes, selected_locations)
  }

  # =========================================================================
  # HELPER FUNCTION: Create Individual Parameter Plot
  # =========================================================================
  create_parameter_plot <- function(param_name, param_data, param_info, location = NULL) {

    if (is.null(param_data$distribution)) return(NULL)

    x <- NULL; y <- NULL; dist_str <- NULL; mean_val <- NULL

    # Handle different distribution types using metadata
    if (param_data$distribution == "gamma" && !is.null(param_data$parameters)) {
      shape <- param_data$parameters$shape
      rate <- param_data$parameters$rate
      if (!is.null(shape) && !is.null(rate)) {
        x_max <- qgamma(0.99, shape, rate)
        x <- seq(0, x_max, length.out = 1000)
        y <- dgamma(x, shape, rate)
        dist_str <- sprintf("Gamma(%.2f, %.2f)", shape, rate)
        mean_val <- shape / rate
      }
    } else if (param_data$distribution == "beta" && !is.null(param_data$parameters)) {
      shape1 <- param_data$parameters$shape1
      shape2 <- param_data$parameters$shape2
      if (!is.null(shape1) && !is.null(shape2)) {
        x <- seq(0, 1, length.out = 1000)
        y <- dbeta(x, shape1, shape2)

        # Special handling for highly skewed distributions
        if (param_info$category == "initial_conditions" && param_name %in% c("prop_E_initial", "prop_I_initial")) {
          x_max <- qbeta(0.9999, shape1, shape2)
          x_max <- max(x_max, mean_val * 5)
          x <- seq(0, x_max, length.out = 1000)
          y <- dbeta(x, shape1, shape2)
        }

        if (shape1 < 1) {
          dist_str <- sprintf("Beta(%.2g, %.0f)", shape1, shape2)
        } else {
          dist_str <- sprintf("Beta(%.1f, %.1f)", shape1, shape2)
        }
        mean_val <- shape1 / (shape1 + shape2)
      }
    } else if (param_data$distribution == "lognormal" && !is.null(param_data$parameters)) {
      # Handle both parameter naming conventions
      meanlog_val <- param_data$parameters$meanlog %||%
                     (if(!is.null(param_data$parameters$mean) && !is.null(param_data$parameters$sd)) {
                       mu <- param_data$parameters$mean
                       sigma <- param_data$parameters$sd
                       log(mu^2 / sqrt(mu^2 + sigma^2))
                     } else NULL)

      sdlog_val <- param_data$parameters$sdlog %||%
                   (if(!is.null(param_data$parameters$mean) && !is.null(param_data$parameters$sd)) {
                     mu <- param_data$parameters$mean
                     sigma <- param_data$parameters$sd
                     sqrt(log(1 + sigma^2 / mu^2))
                   } else NULL)

      if (!is.null(meanlog_val) && !is.null(sdlog_val) &&
          is.finite(meanlog_val) && is.finite(sdlog_val) && sdlog_val > 0) {
        x_max <- qlnorm(0.99, meanlog_val, sdlog_val)
        if (is.finite(x_max) && x_max > 0) {
          x <- seq(0, x_max, length.out = 1000)
          y <- dlnorm(x, meanlog_val, sdlog_val)
          dist_str <- sprintf("LogNormal(%.2g, %.2g)", meanlog_val, sdlog_val)
          mean_val <- exp(meanlog_val + sdlog_val^2/2)
        }
      }
    } else if (param_data$distribution == "uniform" && !is.null(param_data$parameters)) {
      if (!is.null(param_data$parameters$min) && !is.null(param_data$parameters$max) &&
          is.finite(param_data$parameters$min) && is.finite(param_data$parameters$max) &&
          param_data$parameters$max > param_data$parameters$min) {
        buffer <- (param_data$parameters$max - param_data$parameters$min) * 0.1
        x <- seq(param_data$parameters$min - buffer, param_data$parameters$max + buffer, length.out = 1000)
        y <- dunif(x, param_data$parameters$min, param_data$parameters$max)
        dist_str <- sprintf("Uniform(%.2f, %.2f)", param_data$parameters$min, param_data$parameters$max)
        mean_val <- (param_data$parameters$min + param_data$parameters$max) / 2
      }
    } else if (param_data$distribution == "normal" && !is.null(param_data$parameters)) {
      mean_param <- param_data$parameters$mean
      sd_param <- param_data$parameters$sd
      if (!is.null(mean_param) && !is.null(sd_param) &&
          is.finite(mean_param) && is.finite(sd_param) && sd_param > 0) {
        x <- seq(mean_param - 4*sd_param, mean_param + 4*sd_param, length.out = 1000)
        y <- dnorm(x, mean_param, sd_param)
        dist_str <- sprintf("Normal(%.3f, %.3f)", mean_param, sd_param)
        mean_val <- mean_param
      }
    } else if (param_data$distribution == "gompertz" && !is.null(param_data$parameters)) {
      if (!is.null(param_data$parameters$b) && !is.null(param_data$parameters$eta) &&
          is.finite(param_data$parameters$b) && is.finite(param_data$parameters$eta) &&
          param_data$parameters$b > 0 && param_data$parameters$eta > 0) {
        tryCatch({
          x_max <- qgompertz(0.99, b = param_data$parameters$b, eta = param_data$parameters$eta)
          if (is.finite(x_max) && x_max > 0) {
            x <- seq(0, x_max, length.out = 1000)
            y <- dgompertz(x, b = param_data$parameters$b, eta = param_data$parameters$eta)
            dist_str <- sprintf("Gompertz(b=%.1f, eta=%.1f)", param_data$parameters$b, param_data$parameters$eta)
            # Use mode for Gompertz
            mean_val <- (1 / param_data$parameters$b) * log(param_data$parameters$eta / param_data$parameters$b)
            if (!is.finite(mean_val)) mean_val <- NULL
          }
        }, error = function(e) {
          # Skip problematic Gompertz parameters
          return(NULL)
        })
      }
    } else if (param_data$distribution == "truncnorm" && !is.null(param_data$parameters)) {
      mean_param <- param_data$parameters$mean
      sd_param <- param_data$parameters$sd
      a_bound <- param_data$parameters$a %||% -45
      b_bound <- param_data$parameters$b %||% 45

      if (!is.null(mean_param) && !is.null(sd_param) &&
          is.finite(mean_param) && is.finite(sd_param) && sd_param > 0 &&
          is.finite(a_bound) && is.finite(b_bound) && b_bound > a_bound) {
        x <- seq(a_bound, b_bound, length.out = 1000)
        y <- truncnorm::dtruncnorm(x, a = a_bound, b = b_bound, mean = mean_param, sd = sd_param)
        dist_str <- sprintf("TruncNorm(%.1f, %.1f, [%.0f, %.0f])", mean_param, sd_param, a_bound, b_bound)
        mean_val <- mean_param
      }
    }

    if (is.null(x) || is.null(y) || is.null(dist_str)) return(NULL)

    # Remove non-finite values to prevent warnings
    finite_mask <- is.finite(x) & is.finite(y) & y > 0
    if (sum(finite_mask) < 10) return(NULL)  # Need at least 10 points for meaningful plot

    x <- x[finite_mask]
    y <- y[finite_mask]

    # Create plot data
    plot_data <- data.frame(x = x, y = y)

    # Create subtitle (distribution info and derived note if applicable)
    subtitle_str <- dist_str
    if (!is.null(param_data$derived_note)) {
      subtitle_str <- paste0(dist_str, "\n", param_data$derived_note)
    }

    # Determine x-axis formatting
    x_scale <- if (param_info$category == "initial_conditions" && param_name %in% c("prop_E_initial", "prop_I_initial")) {
      scale_x_continuous(labels = scales::scientific)
    } else if (grepl("beta.*transmission", param_name)) {
      scale_x_continuous(labels = scales::scientific)
    } else {
      scale_x_continuous()
    }

    # Create plot
    p <- ggplot(plot_data, aes(x = x, y = y)) +
      geom_area(fill = "#457b9d", alpha = 0.4) +
      geom_line(color = "#457b9d", linewidth = 0.7) +
      x_scale +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray50", face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray90")
      ) +
      labs(
        title = paste0(param_info$display_name, " (", param_name, ")"),
        subtitle = subtitle_str,
        x = param_info$units
      )

    # Add mean line for applicable distributions (only if within plot range)
    if (!is.null(mean_val) && is.finite(mean_val) && !param_data$distribution %in% c("uniform")) {
      x_range <- range(plot_data$x, na.rm = TRUE)
      if (mean_val >= x_range[1] && mean_val <= x_range[2]) {
        p <- p + geom_vline(xintercept = mean_val, linetype = "dotted",
                           color = "gray50", alpha = 0.7)
      }
    }

    return(p)
  }

  # =========================================================================
  # PLOT 1: Global Parameters (Category-Based Organization)
  # =========================================================================
  cat("Plotting global parameter distributions organized by category...\n")

  # Get global parameters from estimated_parameters, ordered appropriately
  global_params_info <- estimated_parameters %>%
    filter(scale == "global") %>%
    arrange(order)

  if (!is.null(priors$parameters_global) && nrow(global_params_info) > 0) {
    # Organize global parameters by category
    global_categories <- unique(global_params_info$category)
    global_category_plots <- list()

    for (category in global_categories) {
      category_info <- global_params_info %>%
        filter(category == !!category) %>%
        arrange(order)

      category_plot_list <- list()

      for (i in 1:nrow(category_info)) {
        param_name <- category_info$parameter_name[i]
        param_info <- category_info[i, ]

        if (param_name %in% names(priors$parameters_global)) {
          param_data <- priors$parameters_global[[param_name]]

          p <- create_parameter_plot(param_name, param_data, param_info)
          if (!is.null(p)) {
            category_plot_list[[param_name]] <- p
          }
        }
      }

      if (length(category_plot_list) > 0) {
        global_category_plots[[category]] <- category_plot_list
      }
    }

    # Create combined global plot with category sections
    if (length(global_category_plots) > 0) {
      final_plot_list <- list()

      # Main title
      subtitle_text <- ""
      if (!is.null(priors$metadata$version)) {
        subtitle_text <- paste0("Priors version: ", priors$metadata$version)
      }

      if (subtitle_text != "") {
        main_title <- cowplot::ggdraw() +
          cowplot::draw_label("Global Parameter Prior Distributions",
                             fontface = 'bold', size = 16, x = 0.5, y = 0.7, hjust = 0.5) +
          cowplot::draw_label(subtitle_text,
                             fontface = 'plain', size = 12, x = 0.5, y = 0.3, hjust = 0.5, color = "gray30")
      } else {
        main_title <- cowplot::ggdraw() +
          cowplot::draw_label("Global Parameter Prior Distributions",
                             fontface = 'bold', size = 16, x = 0.5, hjust = 0.5)
      }

      final_plot_list[[1]] <- main_title
      heights <- c(0.05)

      # Add each category section
      for (category in names(global_category_plots)) {
        category_title <- switch(category,
                               "transmission" = "Transmission Parameters",
                               "environmental" = "Environmental Parameters",
                               "disease" = "Disease Parameters",
                               "immunity" = "Immunity Parameters",
                               "surveillance" = "Surveillance Parameters",
                               "mobility" = "Mobility Parameters",
                               tools::toTitleCase(gsub("_", " ", category)))

        # Category heading
        category_heading <- cowplot::ggdraw() +
          cowplot::draw_label(category_title, fontface = 'bold', size = 14, x = 0.5, y = 0.5, hjust = 0.5)

        # Category plots grid - use 2 columns with equal subplot dimensions
        n_plots <- length(global_category_plots[[category]])
        ncol <- 2
        nrow <- ceiling(n_plots / ncol)

        # Create grid with equal subplot dimensions (always use 2-column layout)
        category_combined <- cowplot::plot_grid(plotlist = global_category_plots[[category]],
                                              ncol = ncol, nrow = nrow,
                                              align = "hv", axis = "tblr")

        final_plot_list[[length(final_plot_list) + 1]] <- category_heading
        final_plot_list[[length(final_plot_list) + 1]] <- category_combined

        # Adjust height based on number of rows - provide more space for readability
        section_height <- max(0.15, nrow * 0.12)  # Reasonable minimum and per-row height
        heights <- c(heights, 0.03, section_height)
      }

      # Normalize heights
      heights <- heights / sum(heights)

      # Combine all sections
      p_global <- cowplot::plot_grid(plotlist = final_plot_list,
                                    ncol = 1, rel_heights = heights)

      # Add common y-axis label
      p_global <- cowplot::ggdraw(p_global) +
        cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 12)

      # Calculate appropriate height with extra space for disease and immunity
      base_height <- 4  # Base height for title and margins
      category_height <- sum(sapply(names(global_category_plots), function(cat_name) {
        cat <- global_category_plots[[cat_name]]
        n_plots <- length(cat)
        nrow <- ceiling(n_plots / 2)  # 2 columns
        # Give extra height to disease and immunity categories
        if (cat_name %in% c("disease", "immunity")) {
          if (nrow == 1) {
            return(3.2)  # Disease/immunity single row: 3.2 units (increased)
          } else {
            return(nrow * 3.2)  # Disease/immunity multi-row: 3.2 units per row (increased)
          }
        } else {
          if (nrow == 1) {
            return(2.8)  # Other single row: 2.8 units
          } else {
            return(nrow * 2.8)  # Other multi-row: 2.8 units per row
          }
        }
      }))
      plot_height <- base_height + category_height

      # Save plot with increased dimensions and limitsize override
      ggsave(file.path(output_dir, "priors_global.pdf"), p_global,
             width = 12, height = plot_height, limitsize = FALSE)
      plots$global <- p_global
      cat("  Saved: priors_global.pdf\n")
    }
  }

  # =========================================================================
  # PLOT 2: Location-Specific Parameters (Category-Based Organization)
  # =========================================================================
  if (!is.null(location_codes) && length(location_codes) > 0) {
    cat("Plotting location-specific priors organized by category...\n")

    # Get location-specific parameters organized by category
    location_params_info <- estimated_parameters %>%
      filter(scale == "location") %>%
      arrange(order)

    # Get target date from config
    target_date <- "2023-01-01"
    if (!is.null(config$date_start)) {
      target_date <- as.character(config$date_start)
    }

    # Load population data for context (if available)
    pop_data <- NULL
    if (!is.null(PATHS) && !is.null(PATHS$DATA_PROCESSED)) {
      un_pop_file <- file.path(PATHS$DATA_PROCESSED, "demographics", "UN_world_population_prospects_daily.csv")
      if (file.exists(un_pop_file)) {
        pop_data <- read.csv(un_pop_file, stringsAsFactors = FALSE)
      }
    }

    for (iso in location_codes) {
      # Get population size for context
      pop_size <- NULL
      if (!is.null(pop_data)) {
        country_data <- pop_data[pop_data$iso_code == iso, ]
        if (nrow(country_data) > 0) {
          exact_match <- country_data[country_data$date == target_date, ]
          if (nrow(exact_match) > 0) {
            pop_size <- exact_match$total_population[1]
          } else {
            pop_size <- tail(country_data$total_population, 1)
          }
        }
      }

      # Organize plots by category
      category_plots <- list()
      categories_available <- unique(location_params_info$category)

      for (category in categories_available) {
        category_info <- location_params_info %>%
          filter(category == !!category) %>%
          arrange(order)

        category_plot_list <- list()

        for (i in 1:nrow(category_info)) {
          param_name <- category_info$parameter_name[i]
          param_info <- category_info[i, ]

          # Handle parameter name mapping for seasonality and derived parameters
          prior_param_name <- switch(param_name,
                                   "a_1_j" = "a1",
                                   "a_2_j" = "a2",
                                   "b_1_j" = "b1",
                                   "b_2_j" = "b2",
                                   "beta_j0_hum" = "beta_j0_tot",  # Use beta_j0_tot as base for human component
                                   "beta_j0_env" = "beta_j0_tot",  # Use beta_j0_tot as base for environmental component
                                   param_name)

          if (prior_param_name %in% names(priors$parameters_location)) {
            param_data_raw <- priors$parameters_location[[prior_param_name]]$location[[iso]]

            if (!is.null(param_data_raw)) {
              # Handle nested parameter structure
              param_data <- if (!is.null(param_data_raw$parameters)) {
                list(distribution = param_data_raw$distribution %||%
                                  priors$parameters_location[[prior_param_name]]$distribution,
                     parameters = param_data_raw$parameters)
              } else {
                list(distribution = priors$parameters_location[[prior_param_name]]$distribution,
                     parameters = param_data_raw)
              }

              # Special handling for derived beta parameters
              if (param_name %in% c("beta_j0_hum", "beta_j0_env")) {
                # For derived parameters, we need both beta_j0_tot and p_beta
                p_beta_data <- priors$parameters_location[["p_beta"]]$location[[iso]]
                if (!is.null(p_beta_data)) {
                  # Create a note that this is derived from beta_j0_tot * p_beta or beta_j0_tot * (1-p_beta)
                  if (param_name == "beta_j0_hum") {
                    param_data$derived_note <- "Derived from beta_j0_tot × p_beta"
                  } else {
                    param_data$derived_note <- "Derived from beta_j0_tot × (1-p_beta)"
                  }
                }
              }

              p <- create_parameter_plot(param_name, param_data, param_info, location = iso)
              if (!is.null(p)) {
                # Add population context for initial conditions
                if (param_info$category == "initial_conditions" && !is.null(pop_size)) {
                  mean_val <- if (param_data$distribution == "beta") {
                    param_data$parameters$shape1 / (param_data$parameters$shape1 + param_data$parameters$shape2)
                  } else NULL

                  if (!is.null(mean_val)) {
                    pop_count <- formatC(round(mean_val * pop_size), format = "d", big.mark = ",")
                    current_subtitle <- p$labels$subtitle
                    p$labels$subtitle <- paste0(current_subtitle, " (", pop_count, ")")
                  }
                }

                category_plot_list[[param_name]] <- p
              }
            }
          }
        }

        if (length(category_plot_list) > 0) {
          category_plots[[category]] <- category_plot_list
        }
      }

      # Create combined plot for this location
      if (length(category_plots) > 0) {
        final_plot_list <- list()

        # Main title
        title_text <- paste0("Location-Specific Prior Distributions: ", iso)
        subtitle_parts <- paste0("Start date: ", target_date)
        if (!is.null(priors$metadata$version)) {
          subtitle_parts <- paste0(subtitle_parts, " | Priors version: ", priors$metadata$version)
        }

        main_title <- cowplot::ggdraw() +
          cowplot::draw_label(title_text, fontface = 'bold', size = 16, x = 0.5, y = 0.7, hjust = 0.5) +
          cowplot::draw_label(subtitle_parts, fontface = 'plain', size = 12, x = 0.5, y = 0.3, hjust = 0.5, color = "gray30")

        final_plot_list[[1]] <- main_title
        heights <- c(0.05)

        # Add each category section
        for (category in names(category_plots)) {
          category_title <- switch(category,
                                 "initial_conditions" = "Initial Conditions",
                                 "transmission" = "Transmission Parameters",
                                 "seasonality" = "Seasonality Parameters",
                                 "environmental" = "Environmental Calibration (psi*)",
                                 "disease" = "Disease Parameters",
                                 "mobility" = "Mobility Parameters",
                                 "spatial" = "Spatial Parameters",
                                 tools::toTitleCase(gsub("_", " ", category)))

          # Category heading
          category_heading <- cowplot::ggdraw() +
            cowplot::draw_label(category_title, fontface = 'bold', size = 14, x = 0.5, y = 0.5, hjust = 0.5)

          # Category plots grid - use 2 columns with equal subplot dimensions
          n_plots <- length(category_plots[[category]])
          ncol <- 2
          nrow <- ceiling(n_plots / ncol)

          # Create grid with equal subplot dimensions (always use 2-column layout)
          category_combined <- cowplot::plot_grid(plotlist = category_plots[[category]],
                                                ncol = ncol, nrow = nrow,
                                                align = "hv", axis = "tblr")

          final_plot_list[[length(final_plot_list) + 1]] <- category_heading
          final_plot_list[[length(final_plot_list) + 1]] <- category_combined

          # Adjust height based on number of rows for better readability
          section_height <- max(0.15, nrow * 0.12)
          heights <- c(heights, 0.03, section_height)  # Heading + plots
        }

        # Footer
        footer <- cowplot::ggdraw() +
          cowplot::draw_label("Initial condition proportions are normalized to sum to 1.0 during sampling",
                             fontface = 'italic', size = 10, x = 0.5, hjust = 0.5, color = "gray50")
        final_plot_list[[length(final_plot_list) + 1]] <- footer
        heights <- c(heights, 0.02)

        # Normalize heights
        heights <- heights / sum(heights)

        # Combine all sections
        p_combined_final <- cowplot::plot_grid(plotlist = final_plot_list,
                                              ncol = 1, rel_heights = heights)

        # Add common y-axis label
        p_combined_final <- cowplot::ggdraw(p_combined_final) +
          cowplot::draw_label("Density", x = 0.02, y = 0.5, angle = 90, size = 12)

        # Save plot
        filename <- file.path(output_dir, paste0("priors_", iso, ".pdf"))

        # Calculate appropriate height based on categories and plots - increased for readability
        base_height <- 4  # Base height for title, footer, and margins
        category_height <- sum(sapply(names(category_plots), function(cat_name) {
          n_plots <- length(category_plots[[cat_name]])
          nrow <- ceiling(n_plots / 2)  # 2 columns
          # Proportional height: single-row categories get adequate space, multi-row get more
          if (nrow == 1) {
            return(2.8)  # Single row: 2.8 units (increased for better visibility)
          } else {
            return(nrow * 2.8)  # Multi-row: 2.8 units per row (same as before)
          }
        }))
        plot_height <- base_height + category_height

        tryCatch({
          # Save with increased dimensions and limitsize override
          ggsave(filename, p_combined_final, width = 12, height = plot_height, limitsize = FALSE)
          if (!exists("plots$location_specific")) plots$location_specific <- list()
          plots$location_specific[[iso]] <- p_combined_final
          cat("  Saved:", basename(filename), "\n")
        }, error = function(e) {
          cat("  Warning: Could not save", basename(filename), "-", e$message, "\n")
        })
      }
    }
  }

  # Update todo status
  cat("✓ Successfully refactored plot_model_priors to use estimated_parameters\n")

  # Restore original warning setting
  options(warn = old_warn)

  return(invisible(plots))
}

# Define null-coalescing operator if not available
`%||%` <- function(x, y) if (is.null(x)) y else x
