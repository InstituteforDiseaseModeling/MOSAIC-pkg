#' Plot Model Posterior Quantiles
#'
#' Creates visualizations of posterior parameter quantiles from calibration or NPE results.
#' Generates both global parameter plots and location-specific parameter plots.
#' Supports comparison of multiple posterior sources.
#'
#' @param csv_files Character vector of paths to posterior_quantiles.csv files
#'   from calc_model_posterior_quantiles(), estimate_parameters_npe(), or estimate_parameters_lampe()
#' @param output_dir Directory to save plots (default: "./figures")
#' @param plot_types Character vector specifying which plot types to create
#'   ("global", "location", "both"). Default: "both"
#' @param verbose Logical, whether to print progress messages (default: TRUE)
#'
#' @return List of ggplot objects (invisibly)
#'
#' @details
#' The function creates:
#' - Caterpillar plots showing median and credible intervals for each estimation type
#' - Separate plots for global and location-specific parameters
#' - Color-coded visualization by estimation type (automatically detected):
#'   - Prior samples (dark grey)
#'   - Calibration posterior (blue)
#'   - NPE estimates (red/orange)
#'   - Lampe estimates (bright red)
#'   - Other types (automatically assigned from color palette)
#' - Supports comparison of any number of estimation types
#' - Dynamically adapts to whatever types are present in the data
#' - Expects standardized CSV format with consistent column names
#' - Saves plots as PNG files in the output directory
#'
#' @export
plot_model_posterior_quantiles <- function(csv_files,
                                          output_dir = "./figures",
                                          plot_types = "both",
                                          verbose = TRUE) {

    # Load required libraries
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(patchwork)

    # Load estimated_parameters for proper ordering
    data(estimated_parameters, package = "MOSAIC", envir = environment())

    # Validate inputs
    if (missing(csv_files) || length(csv_files) == 0) {
        stop("At least one CSV file path must be provided")
    }

    # Check files exist
    for (file in csv_files) {
        if (!file.exists(file)) {
            stop(sprintf("File not found: %s", file))
        }
    }

    # Create output directory if needed
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    if (verbose) {
        message("=== PLOTTING POSTERIOR QUANTILES ===")
        message(sprintf("Input files: %d", length(csv_files)))
        message(sprintf("Output directory: %s", output_dir))
    }

    # Determine plot types to create
    if (plot_types == "both") {
        plot_types <- c("global", "location")
    }

    # =============================================================================
    # LOAD AND COMBINE DATA
    # =============================================================================

    all_data <- list()

    for (i in seq_along(csv_files)) {
        file_path <- csv_files[i]

        # Create a source label
        if (length(csv_files) == 1) {
            source_label <- "Posterior"
        } else {
            # Use file name without extension as label
            source_label <- tools::file_path_sans_ext(basename(file_path))
            # Clean up common patterns
            source_label <- gsub("posterior_quantiles", "", source_label)
            source_label <- gsub("^_|_$", "", source_label)
            if (source_label == "") source_label <- paste0("Source ", i)
        }

        if (verbose) {
            message(sprintf("Loading %s: %s", source_label, basename(file_path)))
        }

        # Read CSV
        data <- read.csv(file_path, stringsAsFactors = FALSE)

        # Add source column
        data$source <- source_label

        # Validate required columns exist (standardized format expected)
        # Note: Fallback column mapping removed since both NPE and calibration
        # functions now produce consistent format

        # Validate required columns are present (flexible quantile detection)
        required_base_cols <- c("parameter")
        missing_base <- setdiff(required_base_cols, names(data))
        if (length(missing_base) > 0) {
            stop(sprintf("Required columns missing in %s: %s",
                        basename(file_path),
                        paste(missing_base, collapse = ", ")))
        }

        # Detect available quantile columns
        quantile_cols <- grep("^q[0-9]", names(data), value = TRUE)
        if (length(quantile_cols) < 3) {
            stop(sprintf("Insufficient quantile columns in %s. Found: %s\nExpected at least 3 quantile columns (e.g., q0.025, q0.5, q0.975)",
                        basename(file_path),
                        paste(quantile_cols, collapse = ", ")))
        }

        # Find median column (q0.5 or similar)
        median_col <- quantile_cols[grepl("q0\\.5", quantile_cols)]
        if (length(median_col) == 0) {
            # Fallback: look for any column near 0.5
            median_patterns <- c("q50", "q0\\.50", "median")
            median_col <- quantile_cols[grepl(paste(median_patterns, collapse = "|"), quantile_cols)]
            if (length(median_col) == 0) {
                stop(sprintf("No median quantile column found in %s. Available: %s",
                            basename(file_path), paste(quantile_cols, collapse = ", ")))
            }
        }
        # Use first median match if multiple found
        data$q_median <- data[[median_col[1]]]

        # Find lower and upper quantiles (first and last available)
        quantile_vals <- as.numeric(gsub("q", "", quantile_cols))
        quantile_vals <- quantile_vals[!is.na(quantile_vals)]
        if (length(quantile_vals) >= 2) {
            lower_idx <- which.min(quantile_vals)
            upper_idx <- which.max(quantile_vals)
            data$q_lower <- data[[quantile_cols[lower_idx]]]
            data$q_upper <- data[[quantile_cols[upper_idx]]]
        } else {
            stop(sprintf("Unable to identify lower/upper quantiles in %s", basename(file_path)))
        }

        # Validate expected columns are present (no fallback needed for standardized format)
        expected_cols <- c("description", "param_type", "location", "type")
        missing_expected <- setdiff(expected_cols, names(data))
        if (length(missing_expected) > 0) {
            stop(sprintf("Expected columns missing in %s: %s\nBoth NPE and calibration functions should provide standardized format",
                        basename(file_path),
                        paste(missing_expected, collapse = ", ")))
        }

        # Accept all estimation types found in the data (no filtering)
        if ("type" %in% names(data)) {
            # Keep all types - no filtering based on hardcoded values
            if (nrow(data) == 0) {
                warning(sprintf("No data found in %s", basename(file_path)))
            }
        }

        all_data[[i]] <- data
    }

    # Combine all data using bind_rows which handles different columns
    combined_data <- dplyr::bind_rows(all_data)

    if (verbose) {
        n_params <- length(unique(combined_data$parameter))
        n_global <- sum(combined_data$param_type == "global" & !duplicated(combined_data$parameter))
        n_location <- sum(combined_data$param_type == "location" & !duplicated(combined_data$parameter))
        message(sprintf("Total parameters: %d (Global: %d, Location-specific: %d)",
                       n_params, n_global, n_location))
    }

    # Set up color scheme based on estimation types (prior, posterior, npe)
    estimation_types <- unique(combined_data$type)

    # Define colors for known estimation types
    known_type_colors <- list(
        "prior" = "#4a4a4a",        # Dark grey for prior
        "posterior" = "#2166ac",    # Blue for calibration posterior
        "npe" = "#d6604d",          # Red/orange for NPE
        "lampe" = "#d73027"         # Bright red for Lampe
    )

    # Create a color palette for any additional unknown types
    additional_colors <- c("#1b9e77", "#7570b3", "#e7298a", "#66a61e", "#e6ab02",
                          "#a6761d", "#666666", "#8dd3c7", "#ffffb3", "#bebada")

    # Create color mapping for all available types
    color_values <- c()
    additional_color_idx <- 1

    for (est_type in estimation_types) {
        if (est_type %in% names(known_type_colors)) {
            color_values[est_type] <- known_type_colors[[est_type]]
        } else {
            # Assign colors from additional palette for unknown types
            if (additional_color_idx <= length(additional_colors)) {
                color_values[est_type] <- additional_colors[additional_color_idx]
                additional_color_idx <- additional_color_idx + 1
            } else {
                # Fallback to grey if we run out of colors
                color_values[est_type] <- "#999999"
            }
        }
    }

    if (verbose) {
        message(sprintf("Estimation types found: %s", paste(estimation_types, collapse = ", ")))
        message(sprintf("Color mapping: %s", paste(names(color_values), color_values, sep = "=", collapse = ", ")))
    }

    # List to store all plots
    plot_list <- list()

    # =============================================================================
    # GLOBAL PARAMETERS PLOT
    # =============================================================================

    if ("global" %in% plot_types) {
        global_data <- combined_data %>%
            filter(param_type == "global") %>%
            arrange(parameter)

        if (nrow(global_data) > 0) {
            if (verbose) message("Creating global parameters plot...")

            # Create parameter labels with parameter name in parentheses for panel titles
            global_data$param_label <- paste0(global_data$description, " (", global_data$parameter, ")")

            # Merge with estimated_parameters to get proper ordering
            # Use parameter_name from estimated_parameters to match with parameter column
            param_ordering <- estimated_parameters %>%
                select(parameter_name, order, category, scale) %>%
                rename(parameter = parameter_name)

            global_data <- global_data %>%
                left_join(param_ordering, by = "parameter")

            # Order parameters by the 'order' column from estimated_parameters
            # If order is missing (parameter not in estimated_parameters), put at end
            global_data <- global_data %>%
                arrange(!is.na(order), order, parameter)

            # Create factor for proper ordering (don't reverse for correct display order)
            param_order <- unique(global_data$param_label)
            global_data$param_label <- factor(global_data$param_label,
                                             levels = param_order)

            # Group parameters by category or create groups if too many
            if (!"category" %in% names(global_data) || all(is.na(global_data$category))) {
                # Create artificial groups if no categories exist
                n_params_per_group <- 8
                params_unique <- unique(global_data$parameter)
                n_groups <- ceiling(length(params_unique) / n_params_per_group)

                global_data$category <- NA
                for (i in seq_along(params_unique)) {
                    group_num <- ceiling(i / n_params_per_group)
                    global_data$category[global_data$parameter == params_unique[i]] <- paste0("Group ", group_num)
                }
            }

            # Check if 50% CI columns exist
            has_50_ci <- all(c("q0.25", "q0.75") %in% names(global_data))

            # Create the plot with faceting by individual parameter (each gets own panel)
            p_global <- ggplot(global_data, aes(x = q_median, y = type, color = type)) +
                # Outer CI - thinner line (using detected lower/upper quantiles)
                geom_errorbar(aes(xmin = q_lower, xmax = q_upper),
                             orientation = "y",
                             position = position_dodge(width = 0.3),
                             width = 0, linewidth = 0.4, alpha = 0.6) +
                # Inner CI - thicker line (if available)
                {if(has_50_ci) geom_errorbar(aes(xmin = q0.25, xmax = q0.75),
                             orientation = "y",
                             position = position_dodge(width = 0.3),
                             width = 0, linewidth = 1.2)} +
                # Median point
                geom_point(position = position_dodge(width = 0.3), size = 2.5) +
                scale_color_manual(values = color_values) +
                facet_wrap(~ param_label, scales = "free_x", ncol = 2) +  # Two columns for better layout
                labs(
                    title = "Global Parameters - Parameter Estimates",
                    subtitle = if(has_50_ci) "Median with 50% (thick) and outer (thin) Credible Intervals"
                              else "Median and outer Credible Intervals",
                    x = "Parameter Value",
                    y = "Estimation Type",
                    color = "Estimation Type"
                ) +
                theme_bw() +
                theme(
                    plot.title = element_text(size = 14, face = "bold"),
                    plot.subtitle = element_text(size = 11),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 9),
                    legend.position = if(length(estimation_types) > 1) "bottom" else "none",
                    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
                    panel.grid.minor = element_blank(),
                    strip.text = element_text(size = 11, face = "bold"),
                    strip.background = element_rect(fill = "gray95")
                ) +
                geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)

            plot_list$global <- p_global

            # Save plot with appropriate dimensions for individual parameter panels
            n_parameters <- length(unique(global_data$param_label))

            output_file <- file.path(output_dir, "posterior_quantiles_global.png")
            ggsave(output_file, p_global,
                   width = 14, height = max(12, n_parameters * 1.0),  # Balanced dimensions for 2 columns
                   dpi = 300, limitsize = FALSE)

            if (verbose) message(sprintf("  Saved: %s", basename(output_file)))
        } else {
            if (verbose) message("  No global parameters found")
        }
    }

    # =============================================================================
    # LOCATION-SPECIFIC PARAMETERS PLOTS
    # =============================================================================

    if ("location" %in% plot_types) {
        location_data <- combined_data %>%
            filter(param_type == "location")

        if (nrow(location_data) > 0) {
            locations <- unique(location_data$location[!is.na(location_data$location)])

            if (verbose) message(sprintf("Creating location-specific plots for %d locations...",
                                        length(locations)))

            for (loc in locations) {
                loc_data <- location_data %>%
                    filter(location == loc) %>%
                    arrange(parameter)

                if (nrow(loc_data) == 0) next

                # Clean parameter names (remove location suffix)
                loc_data$param_clean <- gsub(paste0("_", loc, "$"), "", loc_data$parameter)

                # Create parameter labels with parameter name in parentheses for panel titles
                if ("description" %in% names(loc_data)) {
                    # Remove location from description and add parameter name in parentheses
                    clean_description <- gsub(paste0(" ", loc, "$"), "", loc_data$description)
                    loc_data$param_label <- paste0(clean_description, " (", loc_data$param_clean, ")")
                } else {
                    loc_data$param_label <- paste0(loc_data$param_clean, " (", loc_data$param_clean, ")")
                }

                # Merge with estimated_parameters to get proper ordering
                # Use parameter_name from estimated_parameters to match with cleaned parameter names
                param_ordering <- estimated_parameters %>%
                    select(parameter_name, order, category, scale) %>%
                    rename(param_clean = parameter_name)

                loc_data <- loc_data %>%
                    left_join(param_ordering, by = "param_clean")

                # Order parameters by the 'order' column from estimated_parameters
                # If order is missing (parameter not in estimated_parameters), put at end
                loc_data <- loc_data %>%
                    arrange(!is.na(order), order, param_clean)

                param_order <- unique(loc_data$param_label)
                loc_data$param_label <- factor(loc_data$param_label, levels = param_order)

                # Group parameters by category or create groups if too many
                if (!"category" %in% names(loc_data) || all(is.na(loc_data$category))) {
                    # Create artificial groups if no categories exist
                    n_params_per_group <- 8
                    params_unique <- unique(loc_data$param_clean)
                    n_groups <- ceiling(length(params_unique) / n_params_per_group)

                    loc_data$category <- NA
                    for (i in seq_along(params_unique)) {
                        group_num <- ceiling(i / n_params_per_group)
                        loc_data$category[loc_data$param_clean == params_unique[i]] <- paste0("Group ", group_num)
                    }
                }

                # Check if 50% CI columns exist
                has_50_ci <- all(c("q0.25", "q0.75") %in% names(loc_data))

                # Create the plot with faceting by individual parameter (each gets own panel)
                p_loc <- ggplot(loc_data, aes(x = q_median, y = type, color = type)) +
                    # Outer CI - thinner line (using detected lower/upper quantiles)
                    geom_errorbar(aes(xmin = q_lower, xmax = q_upper),
                                 orientation = "y",
                                 position = position_dodge(width = 0.3),
                                 width = 0, linewidth = 0.4, alpha = 0.6) +
                    # Inner CI - thicker line (if available)
                    {if(has_50_ci) geom_errorbar(aes(xmin = q0.25, xmax = q0.75),
                                 orientation = "y",
                                 position = position_dodge(width = 0.3),
                                 width = 0, linewidth = 1.2)} +
                    # Median point
                    geom_point(position = position_dodge(width = 0.3), size = 2.5) +
                    scale_color_manual(values = color_values) +
                    facet_wrap(~ param_label, scales = "free_x", ncol = 2) +  # Two columns for better layout
                    labs(
                        title = sprintf("Location-Specific Parameters - %s", loc),
                        subtitle = if(has_50_ci) "Median with 50% (thick) and outer (thin) Credible Intervals"
                                  else "Median and outer Credible Intervals",
                        x = "Parameter Value",
                        y = "Estimation Type",
                        color = "Estimation Type"
                    ) +
                    theme_bw() +
                    theme(
                        plot.title = element_text(size = 14, face = "bold"),
                        plot.subtitle = element_text(size = 11),
                        axis.text.y = element_text(size = 10),
                        axis.text.x = element_text(size = 9),
                        legend.position = if(length(estimation_types) > 1) "bottom" else "none",
                        panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
                        panel.grid.minor = element_blank(),
                        strip.text = element_text(size = 11, face = "bold"),
                        strip.background = element_rect(fill = "gray95")
                    ) +
                    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)

                plot_list[[paste0("location_", loc)]] <- p_loc

                # Save plot with appropriate dimensions for individual parameter panels
                n_parameters <- length(unique(loc_data$param_label))

                output_file <- file.path(output_dir, sprintf("posterior_quantiles_%s.png", loc))
                ggsave(output_file, p_loc,
                       width = 14, height = max(12, n_parameters * 1.0),  # Balanced dimensions for 2 columns
                       dpi = 300, limitsize = FALSE)

                if (verbose) message(sprintf("  Saved: %s", basename(output_file)))
            }

            # =============================================================================
            # COMBINED LOCATION COMPARISON PLOT
            # =============================================================================

            if (length(locations) > 1) {
                if (verbose) message("Creating location comparison plot...")

                # Select a few key parameters for comparison
                key_params <- c("beta", "gamma", "sigma", "alpha", "rho", "epsilon")

                comparison_data <- location_data %>%
                    mutate(param_base = gsub("_(MWI|MOZ|ZMB|ZWE)$", "", parameter)) %>%
                    filter(param_base %in% key_params) %>%
                    arrange(param_base, location)

                if (nrow(comparison_data) > 0) {
                    # Create comparison plot
                    p_compare <- ggplot(comparison_data,
                                      aes(x = location, y = q_median, color = type)) +
                        geom_point(position = position_dodge(width = 0.3), size = 2) +
                        geom_errorbar(aes(ymin = q_lower, ymax = q_upper),
                                    position = position_dodge(width = 0.3),
                                    width = 0, linewidth = 0.5) +
                        facet_wrap(~ param_base, scales = "free_y", ncol = 3) +
                        scale_color_manual(values = color_values) +
                        labs(
                            title = "Location Parameter Comparison",
                            subtitle = "Key parameters across locations",
                            x = "Location",
                            y = "Parameter Value",
                            color = "Estimation Type"
                        ) +
                        theme_bw() +
                        theme(
                            plot.title = element_text(size = 14, face = "bold"),
                            plot.subtitle = element_text(size = 11),
                            axis.text.x = element_text(angle = 45, hjust = 1),
                            legend.position = if(length(estimation_types) > 1) "bottom" else "none",
                            strip.text = element_text(size = 10, face = "bold"),
                            panel.grid.minor = element_blank()
                        ) +
                        geom_hline(yintercept = 0, linetype = "dashed",
                                 color = "gray50", alpha = 0.5)

                    plot_list$location_comparison <- p_compare

                    # Save plot
                    output_file <- file.path(output_dir, "posterior_quantiles_location_comparison.png")
                    ggsave(output_file, p_compare, width = 12, height = 8, dpi = 300)

                    if (verbose) message(sprintf("  Saved: %s", basename(output_file)))
                }
            }
        } else {
            if (verbose) message("  No location-specific parameters found")
        }
    }

    # =============================================================================
    # SUMMARY
    # =============================================================================

    if (verbose) {
        message("=== PLOTTING COMPLETE ===")
        message(sprintf("Generated %d plots", length(plot_list)))
        message(sprintf("Output directory: %s", output_dir))
    }

    # Return plot list invisibly
    invisible(plot_list)
}