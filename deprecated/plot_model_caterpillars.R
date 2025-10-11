#' Plot Caterpillar Plots for Model Posterior Quantiles
#'
#' Creates caterpillar plots showing prior and posterior distributions from
#' the output of calc_model_posterior_quantiles.
#'
#' @param quantiles_file Path to the posterior_quantiles.csv file from calc_model_posterior_quantiles
#' @param output_dir Directory to save the plots (default: "./results")
#' @param selected_locations Vector of ISO3 codes to plot (NULL = all locations)
#' @param show_kl Logical; whether to show KL divergence values (default: TRUE)
#' @param verbose Logical; print progress messages (default: TRUE)
#'
#' @return List of ggplot objects (invisible)
#'
#' @examples
#' \dontrun{
#' plot_model_caterpillars(
#'   quantiles_file = "./results/posterior_quantiles.csv",
#'   output_dir = "./results/plots",
#'   selected_locations = c("ETH", "KEN")
#' )
#' }
#'
#' @export
plot_model_caterpillars <- function(quantiles_file,
                                    output_dir = "./results",
                                    selected_locations = NULL,
                                    show_kl = TRUE,
                                    verbose = TRUE) {

  library(ggplot2)
  library(dplyr)
  library(cowplot)

  # Color scheme matching plot_model_distributions
  prior_color <- "#457b9d"
  posterior_color <- "#e63946"

  if (verbose) cat("Loading quantiles from:", quantiles_file, "\n")

  # Load quantiles data
  if (!file.exists(quantiles_file)) {
    stop("Quantiles file not found: ", quantiles_file)
  }

  quantiles_df <- read.csv(quantiles_file, stringsAsFactors = FALSE)

  # Check if we have both prior and posterior
  if (!"type" %in% names(quantiles_df)) {
    stop("Quantiles file must have a 'type' column with 'prior' and 'posterior' values")
  }

  # Filter locations if specified
  if (!is.null(selected_locations)) {
    # Handle both location column and parameters with location suffixes
    quantiles_df <- quantiles_df %>%
      filter(is.na(location) | location %in% selected_locations |
             grepl(paste0("_", paste(selected_locations, collapse = "|_")), parameter))
  }

  # Load estimated_parameters for ordering
  data(estimated_parameters, package = "MOSAIC", envir = environment())

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Store all plots
  plots <- list()

  # =========================================================================
  # HELPER FUNCTION: Create Caterpillar Plot
  # =========================================================================
  create_caterpillar <- function(data, title, subtitle = NULL) {

    # Separate prior and posterior
    prior_data <- data %>% filter(type == "prior")
    posterior_data <- data %>% filter(type == "posterior")

    # Merge for plotting
    plot_data <- prior_data %>%
      select(parameter, display_name = description,
             prior_q025 = q0.0275, prior_q50 = q0.5, prior_q975 = q0.975) %>%
      left_join(
        posterior_data %>%
          select(parameter,
                 post_q025 = q0.0275, post_q50 = q0.5, post_q975 = q0.975, kl),
        by = "parameter"
      )

    # Create display labels with KL if requested
    if (show_kl) {
      plot_data <- plot_data %>%
        mutate(
          label = ifelse(!is.na(kl) & kl > 0.01,
                        sprintf("%s\n(KL: %.2f)", display_name, kl),
                        display_name)
        )
    } else {
      plot_data <- plot_data %>%
        mutate(label = display_name)
    }

    # Order parameters
    plot_data$label <- factor(plot_data$label, levels = rev(plot_data$label))

    # Create plot
    p <- ggplot(plot_data) +
      # Prior intervals (wider, lighter)
      geom_segment(aes(x = prior_q025, xend = prior_q975,
                      y = label, yend = label),
                  color = prior_color, alpha = 0.3, linewidth = 3) +
      # Prior median
      geom_point(aes(x = prior_q50, y = label),
                color = prior_color, size = 2, shape = 124) +  # Vertical line symbol

      # Posterior intervals (thinner, darker)
      geom_segment(aes(x = post_q025, xend = post_q975,
                      y = label, yend = label),
                  color = posterior_color, alpha = 0.8, linewidth = 1.5) +
      # Posterior median
      geom_point(aes(x = post_q50, y = label),
                color = posterior_color, size = 3, shape = 16) +

      # Styling
      labs(
        title = title,
        subtitle = subtitle,
        x = "Parameter Value",
        y = NULL
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray50"),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        plot.margin = margin(10, 10, 10, 10)
      )

    return(p)
  }

  # =========================================================================
  # PLOT 1: Global Parameters
  # =========================================================================
  if (verbose) cat("Creating global parameters caterpillar plot...\n")

  # Get global parameters ordered by estimated_parameters
  global_params_info <- estimated_parameters %>%
    filter(scale == "global") %>%
    arrange(order)

  # Filter quantiles for global parameters
  global_quantiles <- quantiles_df %>%
    filter(parameter %in% global_params_info$parameter_name |
           (param_type == "global" & !grepl("_[A-Z]{3}$", parameter)))

  if (nrow(global_quantiles) > 0) {
    # Order according to estimated_parameters
    global_quantiles$parameter <- factor(
      global_quantiles$parameter,
      levels = global_params_info$parameter_name[global_params_info$parameter_name %in% global_quantiles$parameter]
    )
    global_quantiles <- global_quantiles %>% arrange(parameter)

    # Group by category
    categories <- unique(global_params_info$category)
    category_plots <- list()

    for (cat in categories) {
      cat_params <- global_params_info %>%
        filter(category == cat) %>%
        pull(parameter_name)

      cat_data <- global_quantiles %>%
        filter(parameter %in% cat_params)

      if (nrow(cat_data) > 0) {
        n_params <- length(unique(cat_data$parameter))
        plot_height <- max(3, n_params * 0.4)  # Dynamic height based on parameters

        p <- create_caterpillar(
          cat_data,
          title = paste("Global Parameters -", tools::toTitleCase(gsub("_", " ", cat))),
          subtitle = "Prior (blue) vs Posterior (red) 95% Credible Intervals"
        )

        category_plots[[cat]] <- list(plot = p, height = plot_height)
      }
    }

    if (length(category_plots) > 0) {
      # Combine category plots
      plot_list <- lapply(category_plots, function(x) x$plot)
      total_height <- sum(sapply(category_plots, function(x) x$height))

      p_global <- cowplot::plot_grid(
        plotlist = plot_list,
        ncol = 1,
        align = "v",
        rel_heights = sapply(category_plots, function(x) x$height)
      )

      # Add title
      title <- cowplot::ggdraw() +
        cowplot::draw_label("Global Parameter Estimates",
                           fontface = "bold", size = 14, x = 0.5)

      # Add legend
      legend <- cowplot::ggdraw() +
        cowplot::draw_label("Prior:", size = 10, x = 0.35, y = 0.5, color = prior_color) +
        cowplot::draw_line(c(0.40, 0.45), c(0.5, 0.5),
                          color = prior_color, size = 3, alpha = 0.3) +
        cowplot::draw_label("Posterior:", size = 10, x = 0.55, y = 0.5, color = posterior_color) +
        cowplot::draw_line(c(0.62, 0.67), c(0.5, 0.5),
                          color = posterior_color, size = 1.5, alpha = 0.8)

      # Combine with title and legend
      p_global_final <- cowplot::plot_grid(
        title, p_global, legend,
        ncol = 1,
        rel_heights = c(0.05, 0.9, 0.05)
      )

      # Save plot
      filename <- "posterior_caterpillars_global.pdf"
      ggsave(file.path(output_dir, filename), p_global_final,
             width = 10, height = total_height + 2, limitsize = FALSE)
      plots$global <- p_global_final
      if (verbose) cat("  Saved:", filename, "\n")
    }
  }

  # =========================================================================
  # PLOT 2: Location-Specific Parameters
  # =========================================================================

  # Get unique locations from both location column and parameter suffixes
  locations_from_column <- unique(quantiles_df$location[!is.na(quantiles_df$location) & quantiles_df$location != ""])

  # Extract locations from parameter names (e.g., _ETH, _KEN)
  location_pattern <- "_([A-Z]{3})$"
  locations_from_params <- unique(gsub(".*_([A-Z]{3})$", "\\1",
                                       quantiles_df$parameter[grepl(location_pattern, quantiles_df$parameter)]))

  # Combine all locations
  locations <- unique(c(locations_from_column, locations_from_params))

  # If specific locations selected, filter
  if (!is.null(selected_locations)) {
    locations <- intersect(locations, selected_locations)
  }

  for (iso in locations) {
    if (verbose) cat("Creating caterpillar plot for", iso, "...\n")

    # Get location-specific parameters from estimated_parameters
    location_params_info <- estimated_parameters %>%
      filter(scale == "location") %>%
      arrange(order)

    if (verbose) cat("  Found", nrow(location_params_info), "location parameter types\n")

    # Filter quantiles for this location
    location_quantiles <- quantiles_df %>%
      filter(location == iso | grepl(paste0("_", iso, "$"), parameter))

    if (verbose) cat("  Location", iso, "quantiles rows:", nrow(location_quantiles), "\n")

    if (nrow(location_quantiles) > 0) {
      # Order parameters
      param_order <- c()
      for (param_base in location_params_info$parameter_name) {
        # Check for exact match or with location suffix
        matching <- location_quantiles$parameter[
          location_quantiles$parameter == param_base |
          location_quantiles$parameter == paste0(param_base, "_", iso)
        ]
        if (length(matching) > 0) {
          param_order <- c(param_order, unique(matching))
        }
      }

      if (verbose) cat("  Ordered parameters found:", length(param_order), "\n")

      # Filter and order
      if (length(param_order) > 0) {
        location_quantiles <- location_quantiles %>%
          filter(parameter %in% param_order)

        if (verbose) cat("  Rows after filtering by param_order:", nrow(location_quantiles), "\n")

        if (nrow(location_quantiles) > 0) {
          location_quantiles$parameter <- factor(location_quantiles$parameter, levels = param_order)
          location_quantiles <- location_quantiles %>% arrange(parameter)
        }
      }

      # Group by category
      category_plots <- list()
      categories_in_params <- unique(location_params_info$category)

      if (verbose) cat("  Categories to process:", paste(categories_in_params, collapse=", "), "\n")

      for (cat in categories_in_params) {
        cat_params_base <- location_params_info %>%
          filter(category == cat) %>%
          pull(parameter_name)

        # Get matching parameters (with or without location suffix)
        cat_params <- c()
        for (param_base in cat_params_base) {
          # Find parameters that match this base (with or without location suffix)
          # Convert to character to avoid factor level issues
          param_chars <- as.character(location_quantiles$parameter)
          matches <- param_chars[
            param_chars == param_base |
            param_chars == paste0(param_base, "_", iso)
          ]
          if (length(matches) > 0) {
            cat_params <- c(cat_params, unique(matches))
          }
        }

        if (verbose) {
          cat("    Category", cat, "- parameters found:", length(cat_params), "\n")
          if (length(cat_params) > 0) {
            cat("      Parameters:", paste(head(cat_params, 3), collapse=", "),
                if(length(cat_params) > 3) "..." else "", "\n")
          }
        }

        cat_data <- location_quantiles %>%
          filter(parameter %in% cat_params)

        if (verbose) {
          cat("      Data rows after filter:", nrow(cat_data), "\n")
        }

        if (nrow(cat_data) > 0) {
          n_params <- length(unique(cat_data$parameter))
          plot_height <- max(3, n_params * 0.4)

          p <- create_caterpillar(
            cat_data,
            title = paste(iso, "-", tools::toTitleCase(gsub("_", " ", cat))),
            subtitle = "Prior (blue) vs Posterior (red) 95% Credible Intervals"
          )

          category_plots[[cat]] <- list(plot = p, height = plot_height)
        }
      }

      if (verbose) cat("  Total category plots created:", length(category_plots), "\n")

      if (length(category_plots) > 0) {
        # Combine category plots
        plot_list <- lapply(category_plots, function(x) x$plot)
        total_height <- sum(sapply(category_plots, function(x) x$height))

        p_location <- cowplot::plot_grid(
          plotlist = plot_list,
          ncol = 1,
          align = "v",
          rel_heights = sapply(category_plots, function(x) x$height)
        )

        # Add title
        title <- cowplot::ggdraw() +
          cowplot::draw_label(paste(iso, "Parameter Estimates"),
                             fontface = "bold", size = 14, x = 0.5)

        # Add legend
        legend <- cowplot::ggdraw() +
          cowplot::draw_label("Prior:", size = 10, x = 0.35, y = 0.5, color = prior_color) +
          cowplot::draw_line(c(0.40, 0.45), c(0.5, 0.5),
                            color = prior_color, size = 3, alpha = 0.3) +
          cowplot::draw_label("Posterior:", size = 10, x = 0.55, y = 0.5, color = posterior_color) +
          cowplot::draw_line(c(0.62, 0.67), c(0.5, 0.5),
                            color = posterior_color, size = 1.5, alpha = 0.8)

        # Combine with title and legend
        p_location_final <- cowplot::plot_grid(
          title, p_location, legend,
          ncol = 1,
          rel_heights = c(0.05, 0.9, 0.05)
        )

        # Save plot
        filename <- paste0("posterior_caterpillars_", iso, ".pdf")
        ggsave(file.path(output_dir, filename), p_location_final,
               width = 10, height = total_height + 2, limitsize = FALSE)
        plots[[iso]] <- p_location_final
        if (verbose) cat("  Saved:", filename, "\n")
      }
    }
  }

  if (verbose) cat("✓ Successfully created caterpillar plots\n")

  invisible(plots)
}