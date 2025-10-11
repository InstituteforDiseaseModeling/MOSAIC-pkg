#' Plot model convergence status table
#'
#' Creates a visual status table showing convergence diagnostics with color-coded
#' pass/warn/fail indicators for each metric. Works with both likelihood-based
#' and loss-based calibration methods by auto-detecting the appropriate diagnostics file.
#'
#' @param results_dir Path to results directory containing convergence diagnostics
#'   (expects either "convergence_diagnostics.json" or "convergence_diagnostics_loss.json")
#' @param plots_dir Path to plots directory (default: "../plots" relative to results_dir)
#' @param verbose Logical indicating whether to print messages
#'
#' @return Invisible NULL. Creates a PDF file with the convergence status table.
#'
#' @details
#' The function automatically detects whether to use likelihood-based or loss-based
#' diagnostics by checking for the presence of diagnostics files in this order:
#' \itemize{
#'   \item \code{convergence_diagnostics.json} (likelihood-based, e.g., calibration_test_10.R)
#'   \item \code{convergence_diagnostics_loss.json} (loss-based, older methods)
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' # Works with likelihood-based calibration (calibration_test_10.R)
#' plot_model_convergence_status(
#'   results_dir = "./local/calibration/calibration_test_10/results"
#' )
#'
#' # Also works with loss-based calibration (older methods)
#' plot_model_convergence_status(
#'   results_dir = "./local/calibration/calibration_test_9/results"
#' )
#' }
plot_model_convergence_status <- function(results_dir,
                                         plots_dir = NULL,
                                         verbose = TRUE) {

    # --- I/O checks -------------------------------------------------------------
    if (!dir.exists(results_dir)) stop("results_dir does not exist: ", results_dir)
    if (is.null(plots_dir)) plots_dir <- file.path(dirname(results_dir), "plots")
    if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE)
        if (verbose) message("Created plots directory: ", plots_dir)
    }

    # Auto-detect diagnostics file (try likelihood-based first, then loss-based)
    diagnostics_files <- c(
        file.path(results_dir, "convergence_diagnostics.json"),          # Likelihood-based (calibration_test_10.R)
        file.path(results_dir, "convergence_diagnostics_loss.json")      # Loss-based (older methods)
    )

    diagnostics_file <- NULL
    for (file in diagnostics_files) {
        if (file.exists(file)) {
            diagnostics_file <- file
            break
        }
    }

    if (is.null(diagnostics_file)) {
        stop("No convergence diagnostics file found. Expected one of:\n  - ",
             paste(basename(diagnostics_files), collapse = "\n  - "))
    }

    if (verbose) message("Reading convergence diagnostics from: ", diagnostics_file)

    # --- Read diagnostics -------------------------------------------------------
    diagnostics <- jsonlite::read_json(diagnostics_file)

    # --- Prepare data for table -------------------------------------------------
    # Extract metrics with their status
    metrics_data <- data.frame(
        Metric = character(),
        Description = character(),
        Target = character(),
        Value = character(),
        Status = character(),
        stringsAsFactors = FALSE
    )

    # Store display expressions separately since data.frame can't hold expressions properly
    metric_expressions <- list()

    # First add the custom rows as requested
    # Row 1: N_sim - Total simulations that completed successfully
    n_valid <- if (!is.null(diagnostics$summary$total_simulations_original)) {
        # Count is_valid simulations from the summary
        if (!is.null(diagnostics$summary$n_successful)) {
            diagnostics$summary$n_successful
        } else {
            diagnostics$summary$total_simulations_original
        }
    } else {
        NA
    }

    if (!is.na(n_valid)) {
        metrics_data <- rbind(metrics_data, data.frame(
            Metric = "N_sim",
            Description = "Total number of simulations that completed successfully",
            Target = "-",
            Value = format(n_valid, big.mark = ","),
            Status = "info",
            stringsAsFactors = FALSE
        ))
        metric_expressions[[length(metric_expressions) + 1]] <- expression(bold(N[sim]))
    }

    # Row 2: N_retained - Number retained after removing non-finite and outliers
    if (!is.null(diagnostics$summary$retained_simulations)) {
        n_retained <- diagnostics$summary$retained_simulations
        # Use same target as ESS_retained
        target_retained <- if (!is.null(diagnostics$targets$ess_min$value)) {
            paste(">=", diagnostics$targets$ess_min$value)
        } else {
            "-"
        }
        # Determine status based on ESS_retained target
        status_retained <- if (!is.null(diagnostics$targets$ess_min$value)) {
            if (n_retained >= diagnostics$targets$ess_min$value) "pass"
            else if (n_retained >= diagnostics$targets$ess_min$value / 2) "warn"
            else "fail"
        } else {
            "info"
        }

        metrics_data <- rbind(metrics_data, data.frame(
            Metric = "N_retained",
            Description = "Number of simulations retained after removing non-finite and outliers",
            Target = target_retained,
            Value = format(n_retained, big.mark = ","),
            Status = status_retained,
            stringsAsFactors = FALSE
        ))
        metric_expressions[[length(metric_expressions) + 1]] <- expression(bold(N[retained]))
    }


    # Process metrics in specific order for better table organization
    # Order: ESS_retained, B_size, ESS_best, A_B, cvw_B
    if (verbose) message("Processing metrics in specified order...")

    metric_order <- c("ess_retained", "B_size", "ess_best", "A_B", "cvw_B")

    for (metric_name in metric_order) {
        # Skip if metric doesn't exist
        if (!(metric_name %in% names(diagnostics$metrics))) {
            if (verbose) message("  Metric not found, skipping: ", metric_name)
            next
        }

        metric <- diagnostics$metrics[[metric_name]]
        if (verbose) message("  Processing metric: ", metric_name)

        # Safety check: ensure metric has required structure
        if (is.null(metric) || (!is.list(metric) && !is.atomic(metric))) {
            if (verbose) message("    Skipping metric with invalid structure: ", metric_name)
            next
        }

        # Ensure metric has a value field (create if missing)
        if (is.null(metric$value)) {
            if (verbose) message("    Metric missing value field, using metric as value: ", metric_name)
            metric <- list(value = metric, status = "info", description = metric_name)
        }

        # Get corresponding target if exists
        target_value <- NA
        if (metric_name %in% c("ess_all", "ess_retained")) {
            target_value <- paste(">=", diagnostics$targets$ess_min$value)
        } else if (metric_name == "ess_best") {
            # ESS_best uses B_min target
            target_value <- paste(">=", diagnostics$targets$B_min$value)
        } else if (metric_name == "A_B") {
            target_value <- paste(">=", diagnostics$targets$A_min$value)
        } else if (metric_name == "cvw_B") {
            target_value <- paste("<=", diagnostics$targets$cvw_max$value)
        } else if (metric_name == "B_size") {
            target_value <- paste(">=", diagnostics$targets$B_min$value)
        } else {
            target_value <- "-"
        }

        # Format value
        formatted_value <- if (is.numeric(metric$value)) {
            if (metric$value >= 100) {
                format(round(metric$value, 0), scientific = FALSE)
            } else if (metric$value >= 1) {
                format(round(metric$value, 2), scientific = FALSE)
            } else {
                format(round(metric$value, 4), scientific = FALSE)
            }
        } else {
            as.character(metric$value)
        }

        # Create display name with better descriptions (method-agnostic)
        display_name <- switch(metric_name,
            "ess_all" = "ESS_retained",
            "ess_retained" = "ESS_retained",
            "ess_best" = "ESS_B",
            "A_B" = "A_B",
            "cvw_B" = "CV_B",
            "B_size" = "Best Subset (B)",
            metric_name
        )

        # Create corresponding expression for rendering
        display_expression <- switch(metric_name,
            "ess_all" = expression(bold(ESS[retained])),
            "ess_retained" = expression(bold(ESS[retained])),
            "ess_best" = expression(bold(ESS[B])),
            "A_B" = expression(bold(A[B])),
            "cvw_B" = expression(bold(CV[B])),
            "B_size" = expression(bold("Best Subset (B)")),
            NULL
        )

        # Override with clearer descriptions (method-agnostic)
        better_description <- switch(metric_name,
            "ess_all" = "Effective sample size across retained simulations",
            "ess_retained" = "Effective sample size across retained simulations",
            "ess_best" = "Effective sample size in best subset",
            "A_B" = "Agreement between simulations in best subset",
            "cvw_B" = "Variability of weights in best subset",
            "B_size" = "Number of simulations in best performing subset",
            # Fallback to provided description or metric name
            if (!is.null(metric$description)) metric$description else metric_name
        )

        # Ensure all variables are non-null and have content
        if (is.null(display_name) || length(display_name) == 0) display_name <- metric_name
        if (is.null(better_description) || length(better_description) == 0) better_description <- metric_name
        if (is.null(target_value) || length(target_value) == 0) target_value <- "-"
        if (is.null(formatted_value) || length(formatted_value) == 0) formatted_value <- "N/A"
        if (is.null(metric$status) || length(metric$status) == 0) metric$status <- "info"

        # Create new row with error handling
        tryCatch({
            new_row <- data.frame(
                Metric = display_name,
                Description = better_description,
                Target = target_value,
                Value = formatted_value,
                Status = metric$status,
                stringsAsFactors = FALSE
            )
            metrics_data <- rbind(metrics_data, new_row)

            # Add the expression to the list if available
            if (!is.null(display_expression)) {
                metric_expressions[[length(metric_expressions) + 1]] <- display_expression
            }
        }, error = function(e) {
            if (verbose) {
                message("    Error creating row for metric: ", metric_name)
                message("    display_name: ", paste(display_name, collapse=", "))
                message("    better_description: ", paste(better_description, collapse=", "))
                message("    target_value: ", paste(target_value, collapse=", "))
                message("    formatted_value: ", paste(formatted_value, collapse=", "))
                message("    status: ", paste(metric$status, collapse=", "))
                message("    Error: ", e$message)
            }
        })
    }

    # Check if we have any data to plot
    if (nrow(metrics_data) == 0) {
        if (verbose) {
            message("No metrics data to plot. Available metrics in diagnostics:")
            message("  ", paste(names(diagnostics$metrics), collapse = ", "))
        }
        stop("No valid metrics found to create convergence status plot")
    }

    if (verbose) message("Successfully processed ", nrow(metrics_data), " metrics")

    # --- Create plot ------------------------------------------------------------
    pdf(file.path(plots_dir, "convergence_status.pdf"), width = 14, height = 7)

    # Set up plot area with minimal margins (reduced bottom margin since no URL)
    par(mar = c(2, 1, 3, 1), xpd = TRUE, family = "sans")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))

    # Professional color scheme
    col_header_bg <- "#2C3E50"
    col_header_text <- "white"
    col_row_even <- "#F8F9FA"
    col_row_odd <- "white"
    col_border <- "#DEE2E6"
    col_text_primary <- "#212529"
    col_text_secondary <- "#6C757D"

    # Status colors (professional palette)
    status_colors <- list(
        pass = "#28A745",
        warn = "#FFC107",
        fail = "#DC3545",
        info = "#17A2B8"
    )

    # Overall status color
    overall_status_color <- if (diagnostics$summary$convergence_status == "PASS") {
        status_colors$pass
    } else if (diagnostics$summary$convergence_status == "WARN") {
        status_colors$warn
    } else {
        status_colors$fail
    }

    # Title section
    rect(0, 0.92, 1, 1, col = "white", border = NA)
    text(0.5, 0.96, "MOSAIC Model Convergence Diagnostics",
         cex = 1.8, font = 2, col = col_text_primary)

    # Overall status badge
    badge_width <- 0.15
    badge_x <- 0.5
    badge_y <- 0.89
    rect(badge_x - badge_width/2, badge_y - 0.02,
         badge_x + badge_width/2, badge_y + 0.02,
         col = overall_status_color, border = NA)
    text(badge_x, badge_y, diagnostics$summary$convergence_status,
         col = "white", font = 2, cex = 1.2)

    # Table layout (wider margins for larger page)
    table_top <- 0.82
    table_bottom <- 0.25
    table_left <- 0.05
    table_right <- 0.95

    n_rows <- nrow(metrics_data)
    row_height <- (table_top - table_bottom) / (n_rows + 1)  # +1 for header

    # Column positions and widths (balanced layout)
    col_x <- c(table_left, 0.18, 0.62, 0.73, 0.84, table_right)
    col_widths <- diff(col_x)

    # Draw header
    header_y <- table_top
    rect(table_left, header_y - row_height, table_right, header_y,
         col = col_header_bg, border = NA)

    # Header text (reordered: Value before Target)
    headers <- c("Metric", "Description", "Value", "Target", "Status")
    header_x_centers <- col_x[1:5] + col_widths/2

    for (i in 1:length(headers)) {
        text(header_x_centers[i], header_y - row_height/2,
             headers[i], col = col_header_text, font = 2, cex = 1.1)
    }

    # Draw data rows
    for (i in 1:nrow(metrics_data)) {
        row_y <- header_y - (i * row_height)

        # Row background
        row_col <- if (i %% 2 == 0) col_row_even else col_row_odd
        rect(table_left, row_y - row_height, table_right, row_y,
             col = row_col, border = NA)

        # Add subtle grid lines
        segments(table_left, row_y - row_height, table_right, row_y - row_height,
                col = col_border, lwd = 0.5)

        # Metric name - use expression if available (bold is in the expression itself)
        if (i <= length(metric_expressions)) {
            text(col_x[1] + 0.01, row_y - row_height/2,
                 metric_expressions[[i]],
                 adj = 0, cex = 0.95, col = col_text_primary)
        } else {
            text(col_x[1] + 0.01, row_y - row_height/2,
                 metrics_data$Metric[i],
                 adj = 0, cex = 0.95, font = 2, col = col_text_primary)
        }

        # Description (increased font size for better readability)
        text(col_x[2] + 0.01, row_y - row_height/2,
             metrics_data$Description[i],
             adj = 0, cex = 0.92, col = col_text_secondary)

        # Value (bold, centered) - now in column 3
        text(header_x_centers[3], row_y - row_height/2,
             metrics_data$Value[i],
             cex = 0.95, font = 2, col = col_text_primary)

        # Target (centered) - now in column 4
        text(header_x_centers[4], row_y - row_height/2,
             metrics_data$Target[i],
             cex = 0.9, col = col_text_primary)

        # Status indicator
        status <- metrics_data$Status[i]
        status_x <- header_x_centers[5]

        # Normalize status values (handle case variations and different spellings)
        status_normalized <- tolower(trimws(status))
        if (status_normalized %in% c("pass", "passing", "success", "successful")) {
            status_normalized <- "pass"
        } else if (status_normalized %in% c("warn", "warning", "caution")) {
            status_normalized <- "warn"
        } else if (status_normalized %in% c("fail", "failing", "failure", "failed")) {
            status_normalized <- "fail"
        } else if (status_normalized %in% c("info", "information", "-", "na", "n/a")) {
            status_normalized <- "info"
        }

        if (status_normalized != "info") {
            # Get color, with fallback for unknown status
            status_col <- status_colors[[status_normalized]]
            if (is.null(status_col)) {
                status_col <- status_colors$fail  # Default to fail color for unknown status
            }

            # Draw circle (smaller size)
            symbols(status_x, row_y - row_height/2,
                   circles = 0.008,
                   fg = status_col, bg = status_col,
                   add = TRUE, inches = FALSE)
        } else {
            # For info items, just show text
            text(status_x, row_y - row_height/2,
                 "-", cex = 0.9, col = col_text_secondary)
        }
    }

    # Draw table border
    rect(table_left, table_bottom, table_right, table_top,
         col = NA, border = col_text_primary, lwd = 1.5)

    # Vertical lines for columns
    for (x in col_x[2:5]) {
        segments(x, table_bottom, x, table_top, col = col_border, lwd = 0.5)
    }

    # Summary section (moved up slightly for more spacing) - now shows threshold info
    summary_y <- 0.20
    rect(table_left, summary_y - 0.05, table_right, summary_y + 0.02,
         col = col_row_even, border = col_border, lwd = 1)

    # Get threshold and method information for summary box
    threshold_value <- if (!is.null(diagnostics$metrics$threshold)) {
        sprintf("%.2f", diagnostics$metrics$threshold$value)
    } else if (!is.null(diagnostics$metrics$loss_threshold)) {
        sprintf("%.4f", diagnostics$metrics$loss_threshold$value)
    } else if (!is.null(diagnostics$settings$threshold)) {
        sprintf("%.2f", diagnostics$settings$threshold)
    } else {
        "N/A"
    }

    # Get percentile information
    percentile_info <- if (!is.null(diagnostics$settings$top_percentile)) {
        sprintf("top %.1f%%", diagnostics$settings$top_percentile * 100)
    } else if (!is.null(diagnostics$metrics$threshold$description)) {
        # Extract percentile from description if available
        desc <- diagnostics$metrics$threshold$description
        if (grepl("percentile", desc)) {
            desc
        } else {
            "top 1%"
        }
    } else {
        "top 1%"
    }

    # Get outlier information
    n_outliers <- if (!is.null(diagnostics$metrics$n_outliers$value)) {
        diagnostics$metrics$n_outliers$value
    } else if (!is.null(diagnostics$summary$outliers_removed)) {
        diagnostics$summary$outliers_removed
    } else {
        0
    }

    # Create centered summary text
    summary_base_y <- summary_y - 0.015
    cex_summary <- 0.9

    # Build the threshold information text
    threshold_text <- sprintf("Selection Threshold: %s (%s) | Removed %d outliers",
                             threshold_value, percentile_info, n_outliers)

    # Calculate width and center the text
    text_width <- strwidth(threshold_text, cex = cex_summary)
    text_x <- 0.5 - text_width/2

    # Render the threshold information
    text(text_x, summary_base_y, threshold_text,
         cex = cex_summary, col = col_text_primary, adj = 0)

    # Legend (positioned well above footer)
    legend_y <- 0.10
    legend_items <- c("Pass", "Warning", "Fail")
    legend_cols <- c(status_colors$pass, status_colors$warn, status_colors$fail)

    legend_x_start <- 0.35
    legend_spacing <- 0.10

    for (i in 1:length(legend_items)) {
        x_pos <- legend_x_start + (i - 1) * legend_spacing
        symbols(x_pos, legend_y, circles = 0.006,
               fg = legend_cols[i], bg = legend_cols[i],
               add = TRUE, inches = FALSE)
        text(x_pos + 0.02, legend_y, legend_items[i],
             adj = 0, cex = 0.8, col = col_text_secondary)
    }

    # Footer with timestamp
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    footer_text <- paste0("Generated: ", timestamp)
    text(0.5, 0.04, footer_text, cex = 0.7, col = col_text_secondary)

    dev.off()

    if (verbose) {
        message("Convergence status plot saved to: ",
                file.path(plots_dir, "convergence_status.pdf"))
        message("Overall convergence status: ", diagnostics$summary$convergence_status)
    }

    invisible(NULL)
}