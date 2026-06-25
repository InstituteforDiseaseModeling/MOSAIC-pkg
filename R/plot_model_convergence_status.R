#' Plot model convergence status table
#'
#' Creates a visual status table showing convergence diagnostics with color-coded
#' pass/warn/fail indicators for each metric. Works with both likelihood-based
#' and loss-based calibration methods by auto-detecting the appropriate diagnostics file.
#'
#' The status-table assembly and the \code{convergence_status.csv} write live in
#' \code{\link{calc_model_convergence_status}}; this function renders the PDF
#' table from that result. Pass a precomputed \code{status} object to skip
#' recomputation (and the CSV write).
#'
#' @param results_dir Path to results directory containing convergence diagnostics
#'   (expects either "convergence_diagnostics.json" or "convergence_diagnostics_loss.json")
#' @param plots_dir Path to plots directory (default: "../plots" relative to results_dir)
#' @param status Optional precomputed result list from
#'   \code{\link{calc_model_convergence_status}}. When supplied the recomputation
#'   and CSV write are skipped.
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
                                         status = NULL,
                                         verbose = TRUE) {

    # --- I/O checks -------------------------------------------------------------
    if (!dir.exists(results_dir)) stop("results_dir does not exist: ", results_dir)
    if (is.null(plots_dir)) plots_dir <- file.path(dirname(results_dir), "plots")
    if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE)
        if (verbose) message("Created plots directory: ", plots_dir)
    }

    # --- Assemble the status table (data layer) ---------------------------------
    # The table assembly + convergence_status.csv write live in
    # calc_model_convergence_status(). The plot is a pure consumer. A precomputed
    # `status` skips recomputation (and the CSV write).
    if (is.null(status)) {
        status <- calc_model_convergence_status(
            results_dir = results_dir,
            output_dir  = plots_dir,
            verbose     = verbose
        )
    }

    if (is.null(status) || is.null(status$metrics_data) ||
        nrow(status$metrics_data) == 0) {
        stop("No valid metrics found to create convergence status plot")
    }

    metrics_data       <- status$metrics_data
    metric_expressions <- status$metric_expressions
    diagnostics        <- status$diagnostics
    param_ess_data     <- status$param_ess_data
    n_params           <- status$n_params
    n_pass             <- status$n_pass
    target_ess_param   <- status$target_ess_param

    # --- Create plot ------------------------------------------------------------
    # Increased height to accommodate parameter table, new B_size_upper row, and footer
    pdf(file.path(plots_dir, "convergence_status.pdf"), width = 14, height = 15)

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
    text(0.5, 0.96, "Brute Force Random Sampling (BFRS) Convergence Diagnostics",
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
    # Main table: compact rows to leave room for parameter table at bottom
    table_top <- 0.82
    table_bottom <- 0.48
    table_left <- 0.05
    table_right <- 0.95

    n_rows <- nrow(metrics_data)
    row_height <- (table_top - table_bottom) / (n_rows + 1)  # +1 for header

    # Column positions and widths (adjusted for wider Metric and Description columns)
    col_x <- c(table_left, 0.20, 0.65, 0.77, 0.88, table_right)
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

    # Removed the grey summary box with "Selection threshold" info as requested
    # This section previously showed threshold and outlier information

    # --- Add Parameter ESS Table (if data exists and some parameters fail) ---
    if (!is.null(param_ess_data) && !is.na(n_params) && !is.na(n_pass)) {
        # Filter for parameters below threshold
        params_below <- param_ess_data[param_ess_data$ess_marginal < target_ess_param, ]

        # Only show table if there are failing parameters or if less than 100% pass
        if (nrow(params_below) > 0 || n_pass < n_params) {
            # Position for parameter table (below main diagnostics table)
            param_table_top <- table_bottom - 0.06  # Position below main table with smaller gap
            param_table_height <- min(0.30, 0.025 + nrow(params_below) * 0.018)  # Dynamic height
            param_table_bottom <- param_table_top - param_table_height

            # Add section header
            text(0.5, param_table_top + 0.03, "Parameter-Specific ESS Details",
                 cex = 1.1, font = 2, col = col_text_primary)

            # Show failing parameters (limit to worst 15)
            params_to_show <- if (nrow(params_below) > 0) {
                params_below <- params_below[order(params_below$ess_marginal), ]
                head(params_below, 15)
            } else {
                # If all pass, show the 5 lowest for reference
                param_ess_data[order(param_ess_data$ess_marginal), ][1:min(5, nrow(param_ess_data)), ]
            }

            # Create parameter table
            param_table_left <- 0.15
            param_table_right <- 0.85

            # Header
            param_header_top <- param_table_top
            param_header_bottom <- param_table_top - 0.025
            rect(param_table_left, param_header_bottom, param_table_right, param_header_top,
                 col = col_header_bg, border = col_header_bg)

            # Header text
            param_col_x <- c(param_table_left,
                           param_table_left + 0.35,  # Parameter name
                           param_table_left + 0.50,  # ESS value
                           param_table_left + 0.60)  # Target

            text(param_col_x[1] + 0.01, param_header_bottom + 0.012,
                 "Parameter", adj = 0, cex = 0.9, col = col_header_text, font = 2)
            text(param_col_x[2] + 0.01, param_header_bottom + 0.012,
                 "ESS", adj = 0, cex = 0.9, col = col_header_text, font = 2)
            text(param_col_x[3] + 0.01, param_header_bottom + 0.012,
                 "Target", adj = 0, cex = 0.9, col = col_header_text, font = 2)
            text(param_table_right - 0.05, param_header_bottom + 0.012,
                 "Status", adj = 0.5, cex = 0.9, col = col_header_text, font = 2)

            # Data rows
            for (i in 1:nrow(params_to_show)) {
                row_top <- param_header_bottom - (i - 1) * 0.02
                row_bottom <- row_top - 0.02

                # Alternating row colors
                if (i %% 2 == 0) {
                    rect(param_table_left, row_bottom, param_table_right, row_top,
                         col = col_row_even, border = NA)
                }

                # Parameter name
                text(param_col_x[1] + 0.01, row_bottom + 0.01,
                     params_to_show$parameter[i],
                     adj = 0, cex = 0.85, col = col_text_primary)

                # ESS value
                text(param_col_x[2] + 0.01, row_bottom + 0.01,
                     sprintf("%.1f", params_to_show$ess_marginal[i]),
                     adj = 0, cex = 0.85, col = col_text_primary, font = 2)

                # Target
                text(param_col_x[3] + 0.01, row_bottom + 0.01,
                     sprintf(">=%.0f", target_ess_param),
                     adj = 0, cex = 0.85, col = col_text_secondary)

                # Status indicator
                status_val <- if (params_to_show$ess_marginal[i] >= target_ess_param) "pass"
                             else if (params_to_show$ess_marginal[i] >= target_ess_param * 0.5) "warn"
                             else "fail"

                status_col <- status_colors[[status_val]]
                symbols(param_table_right - 0.05, row_bottom + 0.01,
                       circles = 0.006,
                       fg = status_col, bg = status_col,
                       add = TRUE, inches = FALSE)
            }

            # Border for parameter table
            param_actual_bottom <- param_header_bottom - nrow(params_to_show) * 0.02
            rect(param_table_left, param_actual_bottom, param_table_right, param_header_top,
                 col = NA, border = col_border, lwd = 1)

            # If there are more parameters not shown
            if (nrow(params_below) > 15) {
                text(0.5, param_actual_bottom - 0.02,
                     sprintf("... and %d more parameters below threshold", nrow(params_below) - 15),
                     cex = 0.8, col = col_text_secondary, font = 3)
            }

            # Adjust legend position based on parameter table
            legend_y <- param_actual_bottom - 0.08
        } else {
            # No parameter table needed, keep original legend position
            legend_y <- 0.15
        }
    } else {
        legend_y <- 0.15
    }

    # Legend (positioned dynamically based on content above)
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

    # Footer with timestamp (positioned at very bottom)
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    footer_text <- paste0("Generated: ", timestamp)
    text(0.5, 0.02, footer_text, cex = 0.7, col = col_text_secondary)

    dev.off()

    if (verbose) {
        message("Convergence status plot saved to: ",
                file.path(plots_dir, "convergence_status.pdf"))
        message("Overall convergence status: ", diagnostics$summary$convergence_status)
    }

    invisible(NULL)
}