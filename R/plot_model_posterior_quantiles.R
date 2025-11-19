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
#' @examples
#' \dontrun{
#' # Single posterior file
#' plot_model_posterior_quantiles("posterior_quantiles.csv")
#'
#' # Compare multiple sources
#' plot_model_posterior_quantiles(
#'   csv_files = c("bfrs_quantiles.csv", "npe_quantiles.csv"),
#'   output_dir = "./comparison_plots"
#' )
#' }
#'
#' @export
plot_model_posterior_quantiles <- function(csv_files,
                                          output_dir = "./figures",
                                          plot_types = "both",
                                          verbose = TRUE) {

    # Load required libraries
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required but not installed.")
    }
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Package 'dplyr' is required but not installed.")
    }
    if (!requireNamespace("tools", quietly = TRUE)) {
        stop("Package 'tools' is required but not installed.")
    }

    # All required packages loaded via NAMESPACE

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
            # Use file path to create meaningful labels
            source_label <- tools::file_path_sans_ext(basename(file_path))
            source_label <- gsub("posterior_quantiles", "", source_label)
            source_label <- gsub("^_|_$", "", source_label)
            if (source_label == "") {
                # Use parent directory name as fallback
                parent_dir <- basename(dirname(file_path))
                source_label <- paste0("Source_", parent_dir)
            }
        }

        if (verbose) {
            message(sprintf("Loading %s: %s", source_label, basename(file_path)))
        }

        # Read CSV
        data <- read.csv(file_path, stringsAsFactors = FALSE)

        # Add source column
        data$source <- source_label

        # Validate required columns
        required_cols <- c("parameter", "type")
        missing_cols <- setdiff(required_cols, names(data))
        if (length(missing_cols) > 0) {
            stop(sprintf("Required columns missing in %s: %s",
                        basename(file_path),
                        paste(missing_cols, collapse = ", ")))
        }

        # Detect quantile columns
        quantile_cols <- grep("^q[0-9]", names(data), value = TRUE)
        if (length(quantile_cols) < 3) {
            stop(sprintf("Insufficient quantile columns in %s. Found: %s",
                        basename(file_path),
                        paste(quantile_cols, collapse = ", ")))
        }

        # Find median column
        median_col <- quantile_cols[grepl("q0\\.5", quantile_cols)]
        if (length(median_col) == 0) {
            median_col <- quantile_cols[grepl("q50|q0500|q500", quantile_cols)]
        }
        if (length(median_col) == 0) {
            stop(sprintf("No median column found in %s. Available: %s",
                        basename(file_path), paste(quantile_cols, collapse = ", ")))
        }
        data$q_median <- data[[median_col[1]]]

        # Find lower and upper quantiles
        quantile_vals <- numeric(0)
        for (col in quantile_cols) {
            val <- suppressWarnings(as.numeric(gsub("^q", "", col)))
            if (!is.na(val)) {
                quantile_vals <- c(quantile_vals, val)
            }
        }

        if (length(quantile_vals) >= 2) {
            lower_idx <- which.min(quantile_vals)
            upper_idx <- which.max(quantile_vals)
            data$q_lower <- data[[quantile_cols[lower_idx]]]
            data$q_upper <- data[[quantile_cols[upper_idx]]]
        } else {
            # Fallback to first and last columns
            data$q_lower <- data[[quantile_cols[1]]]
            data$q_upper <- data[[quantile_cols[length(quantile_cols)]]]
        }

        # Ensure param_type exists (fallback to "global" if missing)
        if (!"param_type" %in% names(data)) {
            data$param_type <- "global"
        }

        all_data[[i]] <- data
    }

    # Combine all data
    combined_data <- dplyr::bind_rows(all_data)

    # Ensure param_type exists in combined data (safety check)
    if (!"param_type" %in% names(combined_data)) {
        combined_data$param_type <- "global"
    }

    if (verbose) {
        n_params <- length(unique(combined_data$parameter))
        n_global <- sum(combined_data$param_type == "global" & !duplicated(paste(combined_data$parameter, combined_data$type)))
        n_location <- sum(combined_data$param_type == "location" & !duplicated(paste(combined_data$parameter, combined_data$type)))
        message(sprintf("Total parameters: %d (Global: %d, Location-specific: %d)",
                       n_params, n_global, n_location))
        message(sprintf("Estimation types found: %s", paste(unique(combined_data$type), collapse = ", ")))
    }

    # MOSAIC color palette - consistent across all plots
    estimation_types <- unique(combined_data$type)
    known_type_colors <- list(
        "prior" = "#4a4a4a",        # Dark gray (neutral baseline)
        "posterior" = "#1f77b4",    # Blue (BFRS/calibration)
        "bfrs" = "#1f77b4",         # Alternative naming for BFRS
        "npe" = "#d00000",          # Red (neural posterior estimation)
        "smc" = "#d00000"           # Red (sequential Monte Carlo - deprecated)
    )

    # Assign colors
    color_values <- c()
    additional_colors <- c("#1b9e77", "#7570b3", "#e7298a", "#66a61e", "#e6ab02")
    additional_idx <- 1

    for (est_type in estimation_types) {
        if (est_type %in% names(known_type_colors)) {
            color_values[est_type] <- known_type_colors[[est_type]]
        } else {
            if (additional_idx <= length(additional_colors)) {
                color_values[est_type] <- additional_colors[additional_idx]
                additional_idx <- additional_idx + 1
            } else {
                color_values[est_type] <- "#999999"
            }
        }
    }

    plot_list <- list()

    # =============================================================================
    # GLOBAL PARAMETERS PLOT
    # =============================================================================

    if ("global" %in% plot_types) {
        global_data <- combined_data %>%
            dplyr::filter(param_type == "global") %>%
            dplyr::arrange(parameter)

        if (nrow(global_data) > 0) {
            if (verbose) message("Creating global parameters plot...")

            # **KEY FIX**: Use parameter name only for grouping, but show description for readability
            # This ensures all estimation types for the same parameter are grouped together
            global_data$param_label <- global_data$parameter

            # If description exists, use it for display but keep parameter for grouping
            if ("description" %in% names(global_data) && !all(is.na(global_data$description))) {
                # Use first description found for each parameter (they might differ across sources)
                desc_lookup <- global_data %>%
                    dplyr::filter(!is.na(description) & description != "") %>%
                    dplyr::group_by(parameter) %>%
                    dplyr::summarise(display_desc = dplyr::first(description), .groups = 'drop')

                global_data <- global_data %>%
                    dplyr::left_join(desc_lookup, by = "parameter")

                # Create display label but keep grouping by parameter only
                global_data$display_label <- ifelse(
                    !is.na(global_data$display_desc),
                    paste0(global_data$display_desc, " (", global_data$parameter, ")"),
                    global_data$parameter
                )
            } else {
                global_data$display_label <- global_data$parameter
            }

            # Check for 50% CI
            has_50_ci <- all(c("q0.25", "q0.75") %in% names(global_data))

            # Create the plot - group by param_label (parameter name), display with display_label
            p_global <- ggplot2::ggplot(global_data, ggplot2::aes(x = q_median, y = type, color = type)) +
                ggplot2::geom_errorbar(ggplot2::aes(xmin = q_lower, xmax = q_upper),
                             orientation = "y", position = ggplot2::position_dodge(width = 0.3),
                             width = 0, linewidth = 0.4, alpha = 0.6) +
                {if(has_50_ci) ggplot2::geom_errorbar(ggplot2::aes(xmin = q0.25, xmax = q0.75),
                             orientation = "y", position = ggplot2::position_dodge(width = 0.3),
                             width = 0, linewidth = 1.2)} +
                ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.3), size = 2.5) +
                ggplot2::scale_color_manual(values = color_values) +
                ggplot2::facet_wrap(~ param_label, scales = "free_x", ncol = 2,
                          labeller = ggplot2::labeller(param_label = function(x) {
                              # Use display_label for facet titles
                              lookup <- setNames(global_data$display_label, global_data$param_label)
                              lookup[x]
                          })) +
                ggplot2::labs(
                    title = "Global Parameters - Posterior Quantiles",
                    subtitle = if(has_50_ci) "Median with 50% (thick) and outer (thin) Credible Intervals"
                              else "Median and outer Credible Intervals",
                    x = "Parameter Value",
                    y = "Estimation Type",
                    color = "Estimation Type"
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(size = 14, face = "bold"),
                    plot.subtitle = ggplot2::element_text(size = 11),
                    axis.text.y = ggplot2::element_text(size = 10),
                    axis.text.x = ggplot2::element_text(size = 9),
                    legend.position = if(length(estimation_types) > 1) "bottom" else "none",
                    panel.grid.major.y = ggplot2::element_line(color = "gray90", linetype = "dashed"),
                    panel.grid.minor = ggplot2::element_blank(),
                    strip.text = ggplot2::element_text(size = 10),
                    strip.background = ggplot2::element_rect(fill = "gray95")
                ) +
                ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)

            plot_list$global <- p_global

            # Calculate plot dimensions
            n_parameters <- length(unique(global_data$param_label))
            plot_height <- max(12, min(30, 6 + n_parameters * 1.2))
            plot_width <- 16

            output_file <- file.path(output_dir, "posterior_quantiles_global.png")
            ggplot2::ggsave(output_file, p_global, width = plot_width, height = plot_height,
                   units = "in", dpi = 600, bg = "white")

            if (verbose) message(sprintf("  Saved: %s", basename(output_file)))
        }
    }

    # =============================================================================
    # LOCATION-SPECIFIC PARAMETERS PLOTS (one per location)
    # =============================================================================

    if ("location" %in% plot_types) {
        loc_data <- combined_data %>%
            dplyr::filter(param_type == "location") %>%
            dplyr::arrange(parameter)

        if (nrow(loc_data) > 0) {
            # Get unique locations
            unique_locations <- unique(loc_data$location[!is.na(loc_data$location)])

            if (length(unique_locations) > 0) {
                if (verbose) message(sprintf("Creating location-specific plots for %d locations: %s",
                                           length(unique_locations), paste(unique_locations, collapse = ", ")))

                # Create a plot for each location
                for (location_code in unique_locations) {
                    location_data <- loc_data %>%
                        dplyr::filter(location == location_code) %>%
                        dplyr::arrange(parameter)

                    if (nrow(location_data) > 0) {
                        if (verbose) message(sprintf("  Processing location: %s (%d parameters)",
                                                   location_code, length(unique(location_data$parameter))))

                        # Same grouping logic for location parameters
                        location_data$param_label <- location_data$parameter

                        if ("description" %in% names(location_data) && !all(is.na(location_data$description))) {
                            desc_lookup <- location_data %>%
                                dplyr::filter(!is.na(description) & description != "") %>%
                                dplyr::group_by(parameter) %>%
                                dplyr::summarise(display_desc = dplyr::first(description), .groups = 'drop')

                            location_data <- location_data %>%
                                dplyr::left_join(desc_lookup, by = "parameter")

                            location_data$display_label <- ifelse(
                                !is.na(location_data$display_desc),
                                paste0(location_data$display_desc, " (", location_data$parameter, ")"),
                                location_data$parameter
                            )
                        } else {
                            location_data$display_label <- location_data$parameter
                        }

                        has_50_ci_loc <- all(c("q0.25", "q0.75") %in% names(location_data))

                        p_location <- ggplot2::ggplot(location_data, ggplot2::aes(x = q_median, y = type, color = type)) +
                            ggplot2::geom_errorbar(ggplot2::aes(xmin = q_lower, xmax = q_upper),
                                         orientation = "y", position = ggplot2::position_dodge(width = 0.3),
                                         width = 0, linewidth = 0.4, alpha = 0.6) +
                            {if(has_50_ci_loc) ggplot2::geom_errorbar(ggplot2::aes(xmin = q0.25, xmax = q0.75),
                                         orientation = "y", position = ggplot2::position_dodge(width = 0.3),
                                         width = 0, linewidth = 1.2)} +
                            ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.3), size = 2.5) +
                            ggplot2::scale_color_manual(values = color_values) +
                            ggplot2::facet_wrap(~ param_label, scales = "free_x", ncol = 2,
                                      labeller = ggplot2::labeller(param_label = function(x) {
                                          lookup <- setNames(location_data$display_label, location_data$param_label)
                                          lookup[x]
                                      })) +
                            ggplot2::labs(
                                title = sprintf("%s - Location-Specific Parameters", location_code),
                                subtitle = if(has_50_ci_loc) "Median with 50% (thick) and outer (thin) Credible Intervals"
                                          else "Median and outer Credible Intervals",
                                x = "Parameter Value",
                                y = "Estimation Type",
                                color = "Estimation Type"
                            ) +
                            ggplot2::theme_bw() +
                            ggplot2::theme(
                                plot.title = ggplot2::element_text(size = 14, face = "bold"),
                                plot.subtitle = ggplot2::element_text(size = 11),
                                axis.text.y = ggplot2::element_text(size = 10),
                                axis.text.x = ggplot2::element_text(size = 9),
                                legend.position = if(length(estimation_types) > 1) "bottom" else "none",
                                panel.grid.major.y = ggplot2::element_line(color = "gray90", linetype = "dashed"),
                                panel.grid.minor = ggplot2::element_blank(),
                                strip.text = ggplot2::element_text(size = 10),
                                strip.background = ggplot2::element_rect(fill = "gray95")
                            ) +
                            ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5)

                        # Store plot with location-specific key
                        plot_list[[location_code]] <- p_location

                        # Calculate dimensions and save with location-specific filename
                        n_loc_params <- length(unique(location_data$param_label))
                        plot_height_loc <- max(12, min(30, 6 + n_loc_params * 1.2))
                        plot_width_loc <- 16

                        output_file_loc <- file.path(output_dir, sprintf("posterior_quantiles_%s.png", location_code))
                        ggplot2::ggsave(output_file_loc, p_location, width = plot_width_loc, height = plot_height_loc,
                               units = "in", dpi = 600, bg = "white")

                        if (verbose) message(sprintf("  Saved: %s", basename(output_file_loc)))
                    }
                }
            } else {
                if (verbose) message("No location codes found in location-specific parameters")
            }
        }
    }

    if (verbose) {
        message("=== PLOTTING COMPLETE ===")
        message(sprintf("Generated %d plots", length(plot_list)))
        message(sprintf("Output directory: %s", output_dir))
    }

    return(invisible(plot_list))
}