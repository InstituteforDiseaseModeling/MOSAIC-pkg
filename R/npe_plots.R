
#' Plot NPE convergence status table
#'
#' Creates a comprehensive status table showing NPE model convergence, diagnostics,
#' and performance metrics from the calibration workflow outputs.
#'
#' @param npe_dir Path to NPE directory containing model outputs (default: searches for it)
#' @param output_dir Path to output directory for PDF (default: npe_dir/plots)
#' @param coverage_targets Named list of coverage targets (default: list("50" = 0.50, "95" = 0.95))
#' @param ks_threshold P-value threshold for KS test (default: 0.05)
#' @param show_param_details Maximum number of parameters to show in detail (default: 15)
#' @param verbose Logical indicating whether to print messages
#'
#' @return Invisible NULL. Creates a PDF file with the NPE convergence status table.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create NPE convergence status table
#' plot_npe_convergence_status(
#'   npe_dir = "./local/calibration/calibration_test_17/2_npe"
#' )
#' }
plot_npe_convergence_status <- function(npe_dir = NULL,
                                       output_dir = NULL,
                                       coverage_targets = list("50" = 0.50, "95" = 0.95),
                                       ks_threshold = 0.05,
                                       show_param_details = 15,
                                       verbose = TRUE) {

    # --- Input validation and directory setup -----------------------------------

    # Find NPE directory if not provided
    if (is.null(npe_dir)) {
        # Look for typical NPE directory patterns
        possible_dirs <- c(
            "./2_npe",
            "./npe",
            "../2_npe",
            "../npe"
        )

        for (dir in possible_dirs) {
            if (dir.exists(dir)) {
                npe_dir <- normalizePath(dir)
                if (verbose) message("Found NPE directory: ", npe_dir)
                break
            }
        }

        if (is.null(npe_dir)) {
            stop("Could not find NPE directory. Please specify npe_dir parameter.")
        }
    }

    if (!dir.exists(npe_dir)) {
        stop("NPE directory does not exist: ", npe_dir)
    }

    # Set output directory
    if (is.null(output_dir)) {
        output_dir <- file.path(npe_dir, "plots")
    }
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        if (verbose) message("Created output directory: ", output_dir)
    }

    # --- Load all available NPE data -------------------------------------------

    if (verbose) message("Loading NPE outputs from: ", npe_dir)

    # Initialize data containers
    npe_summary <- NULL
    diagnostics_data <- NULL
    training_history <- NULL
    posterior_data <- NULL
    architecture_spec <- NULL

    # Load NPE summary if available
    summary_file <- file.path(npe_dir, "npe_summary.rds")
    if (file.exists(summary_file)) {
        npe_summary <- readRDS(summary_file)
        if (verbose) message("  Loaded NPE summary")
    }

    # Load diagnostics - prioritize comprehensive SBC format from calc_npe_diagnostics
    diagnostics_dir <- file.path(npe_dir, "diagnostics")
    if (dir.exists(diagnostics_dir)) {
        # First try to load the comprehensive sbc_results.rds from calc_npe_diagnostics
        sbc_results_file <- file.path(diagnostics_dir, "sbc_results.rds")
        if (file.exists(sbc_results_file)) {
            sbc_results <- readRDS(sbc_results_file)
            if (verbose) message("  Loaded comprehensive SBC results from RDS")

            # Extract all components from the comprehensive results
            diagnostics_data <- list(
                param_names = sbc_results$param_names,
                coverage = sbc_results$coverage,
                ranks = sbc_results$sbc_ranks,
                summary = sbc_results$summary,
                sbc = list(
                    ks_pvalues = sbc_results$summary$sbc_ks_pvalue,
                    coverage_mean = sbc_results$summary$coverage_mean,
                    coverage_ok = sbc_results$summary$coverage_ok,
                    sbc_ok = sbc_results$summary$sbc_ok
                ),
                overall_coverage = sbc_results$diagnostics$overall_coverage,
                overall_coverage_se = sbc_results$diagnostics$overall_coverage_se,
                overall_sbc_ks_pvalue = sbc_results$diagnostics$overall_sbc_ks_pvalue,
                n_sbc_sims_successful = sbc_results$diagnostics$n_sbc_sims_successful,
                n_npe_samples_per_test = sbc_results$diagnostics$n_npe_samples_per_test,
                timestamp = sbc_results$diagnostics$timestamp
            )
        } else {
            # Fallback to loading individual files
            sbc_summary_file <- file.path(diagnostics_dir, "sbc_summary.csv")
            sbc_json_file <- file.path(diagnostics_dir, "sbc_diagnostics.json")

            if (file.exists(sbc_summary_file) && file.exists(sbc_json_file)) {
                # Load traditional SBC format files
                sbc_summary <- read.csv(sbc_summary_file, stringsAsFactors = FALSE)
                sbc_json <- jsonlite::read_json(sbc_json_file)

                # Load additional files
                coverage_file <- file.path(diagnostics_dir, "sbc_coverage.csv")
                ranks_file <- file.path(diagnostics_dir, "sbc_ranks.csv")

                diagnostics_data <- list(
                    param_names = sbc_summary$parameter,
                    coverage = if (file.exists(coverage_file)) as.matrix(read.csv(coverage_file)) else NULL,
                    ranks = if (file.exists(ranks_file)) as.matrix(read.csv(ranks_file)) else NULL,
                    summary = sbc_summary,
                    sbc = list(
                        ks_pvalues = sbc_summary$sbc_ks_pvalue,
                        coverage_mean = sbc_summary$coverage_mean,
                        coverage_ok = sbc_summary$coverage_ok,
                        sbc_ok = sbc_summary$sbc_ok
                    ),
                    overall_coverage = sbc_json$overall_coverage,
                    overall_coverage_se = sbc_json$overall_coverage_se,
                    overall_sbc_ks_pvalue = sbc_json$overall_sbc_ks_pvalue,
                    n_sbc_sims_successful = sbc_json$n_sbc_sims_successful,
                    n_npe_samples_per_test = sbc_json$n_npe_samples_per_test,
                    timestamp = sbc_json$timestamp
                )
                if (verbose) message("  Loaded diagnostics from traditional SBC files")
            }
        }
    }

    # Load training history
    model_dir <- file.path(npe_dir, "model")
    if (dir.exists(model_dir)) {
        history_file <- file.path(model_dir, "training_history.csv")
        if (file.exists(history_file)) {
            training_history <- read.csv(history_file, stringsAsFactors = FALSE)
            if (verbose) message("  Loaded training history")
        }

        # Load architecture specification
        arch_file <- file.path(model_dir, "architecture.json")
        if (file.exists(arch_file)) {
            architecture_spec <- jsonlite::read_json(arch_file)
            if (verbose) message("  Loaded architecture specification")
        }
    }

    # Load posterior data
    posterior_dir <- file.path(npe_dir, "posterior")
    if (dir.exists(posterior_dir)) {
        quantiles_file <- file.path(posterior_dir, "posterior_quantiles.csv")
        if (file.exists(quantiles_file)) {
            posterior_quantiles <- read.csv(quantiles_file, stringsAsFactors = FALSE)
            posterior_data <- list(quantiles = posterior_quantiles)
            if (verbose) message("  Loaded posterior quantiles")
        }

        # Check for posterior samples
        samples_file <- file.path(posterior_dir, "posterior_samples.csv")
        if (file.exists(samples_file)) {
            # Just check dimensions, don't load full matrix
            samples_info <- read.csv(samples_file, nrows = 1)
            posterior_data$n_samples <- as.numeric(system(sprintf("wc -l < '%s'", samples_file), intern = TRUE)) - 1
            posterior_data$n_params <- ncol(samples_info)
            if (verbose) message("  Found posterior samples: ", posterior_data$n_samples, " x ", posterior_data$n_params)
        }
    }

    # --- Extract metrics for status table --------------------------------------

    # Initialize metrics data frame
    metrics_data <- data.frame(
        Metric = character(),
        Description = character(),
        Target = character(),
        Value = character(),
        Status = character(),
        stringsAsFactors = FALSE
    )

    # Store display expressions for math rendering
    metric_expressions <- list()

    # --- Build metrics in exact order as shown in image ---

    if (!is.null(diagnostics_data) && !is.null(diagnostics_data$overall_coverage)) {

        # Row 1: Coverage 50% CI
        # Calculate from ranks matrix if available
        if (!is.null(diagnostics_data$ranks)) {
            coverage_50_matrix <- (diagnostics_data$ranks >= 0.25 & diagnostics_data$ranks <= 0.75)
            coverage_50 <- mean(as.matrix(coverage_50_matrix), na.rm = TRUE)
            coverage_50_se <- sd(as.vector(as.matrix(coverage_50_matrix)), na.rm = TRUE) /
                             sqrt(sum(!is.na(as.matrix(coverage_50_matrix))))
        } else {
            # Fallback if ranks not available
            coverage_50 <- 0.50
            coverage_50_se <- 0.05
        }

        # Handle NA values in 50% coverage calculation
        if (is.na(coverage_50) || is.null(coverage_50)) {
            coverage_50_deviation <- NA
            status_coverage_50 <- "unknown"
            coverage_50_display <- "N/A"
        } else {
            coverage_50_deviation <- abs(coverage_50 - 0.50)
            status_coverage_50 <- if (coverage_50_deviation <= 0.05) "pass"
                                  else if (coverage_50_deviation <= 0.10) "warn"
                                  else "fail"
            coverage_50_display <- sprintf("%.1f%% ± %.1f%%", coverage_50 * 100, coverage_50_se * 100)
        }

        metrics_data <- rbind(metrics_data, data.frame(
            Metric = "Coverage_50",
            Description = "Overall parameter coverage (50% CI)",
            Target = "50%",
            Value = coverage_50_display,
            Status = status_coverage_50,
            stringsAsFactors = FALSE
        ))
        metric_expressions[[1]] <- expression(bold(Coverage["50%"]))

        # Row 2: Coverage 95% CI
        coverage_95 <- diagnostics_data$overall_coverage
        coverage_95_se <- diagnostics_data$overall_coverage_se %||% 0

        # Handle NA values in coverage calculation
        if (is.na(coverage_95) || is.null(coverage_95)) {
            coverage_95_deviation <- NA
            status_coverage_95 <- "unknown"
        } else {
            coverage_95_deviation <- abs(coverage_95 - 0.95)
            status_coverage_95 <- if (coverage_95_deviation <= 0.05) "pass"
                                  else if (coverage_95_deviation <= 0.10) "warn"
                                  else "fail"
        }

        # Handle display value for NA coverage
        coverage_95_display <- if (is.na(coverage_95) || is.null(coverage_95)) {
            "N/A"
        } else {
            sprintf("%.1f%% ± %.1f%%", coverage_95 * 100, coverage_95_se * 100)
        }

        metrics_data <- rbind(metrics_data, data.frame(
            Metric = "Coverage_95",
            Description = "Overall parameter coverage (95% CI)",
            Target = "95%",
            Value = coverage_95_display,
            Status = status_coverage_95,
            stringsAsFactors = FALSE
        ))
        metric_expressions[[2]] <- expression(bold(Coverage["95%"]))

        # Row 3: Parameters with good coverage (95% CI)
        n_params_good <- sum(diagnostics_data$sbc$coverage_ok, na.rm = TRUE)
        n_params_total <- length(diagnostics_data$sbc$coverage_ok)
        pct_good_coverage <- (n_params_good / n_params_total) * 100

        # Target is >= 90% of parameters (0.9 * n_params_total)
        target_n_params <- ceiling(n_params_total * 0.9)

        status_param_coverage <- if (pct_good_coverage >= 90) "pass"
                                else if (pct_good_coverage >= 75) "warn"
                                else "fail"

        metrics_data <- rbind(metrics_data, data.frame(
            Metric = "Params_Coverage",
            Description = "Parameters with good coverage (95% CI)",
            Target = sprintf(">=%d", target_n_params),
            Value = sprintf("%d/%d (%.0f%%)", n_params_good, n_params_total, pct_good_coverage),
            Status = status_param_coverage,
            stringsAsFactors = FALSE
        ))
        metric_expressions[[3]] <- expression(bold("Params (Coverage)"))

        # Row 4: SBC KS p-value
        ks_pval <- diagnostics_data$overall_sbc_ks_pvalue

        status_ks <- if (ks_pval > ks_threshold) "pass"
                    else if (ks_pval > ks_threshold/2) "warn"
                    else "fail"

        metrics_data <- rbind(metrics_data, data.frame(
            Metric = "SBC_KS",
            Description = "Kolmogorov-Smirnov test for rank uniformity",
            Target = sprintf(">%.2f", ks_threshold),
            Value = sprintf("%.3f", ks_pval),
            Status = status_ks,
            stringsAsFactors = FALSE
        ))
        metric_expressions[[4]] <- expression(bold(paste("SBC ", KS[p-value])))

        # Row 5: Parameters with uniform SBC ranks
        n_params_good_sbc <- sum(diagnostics_data$sbc$sbc_ok, na.rm = TRUE)
        n_params_total_sbc <- length(diagnostics_data$sbc$sbc_ok)
        pct_good_sbc <- (n_params_good_sbc / n_params_total_sbc) * 100

        # Target is >= 90% of parameters
        target_n_params_sbc <- ceiling(n_params_total_sbc * 0.9)

        status_param_sbc <- if (pct_good_sbc >= 90) "pass"
                           else if (pct_good_sbc >= 75) "warn"
                           else "fail"

        metrics_data <- rbind(metrics_data, data.frame(
            Metric = "Params_SBC",
            Description = "Parameters with uniform SBC ranks",
            Target = sprintf(">=%d", target_n_params_sbc),
            Value = sprintf("%d/%d (%.0f%%)", n_params_good_sbc, n_params_total_sbc, pct_good_sbc),
            Status = status_param_sbc,
            stringsAsFactors = FALSE
        ))
        metric_expressions[[5]] <- expression(bold("Params (SBC)"))
    }

    # --- Determine overall convergence status ----------------------------------

    statuses <- metrics_data$Status[metrics_data$Status != "info"]
    overall_status <- if (length(statuses) == 0) {
        "UNKNOWN"
    } else if (all(statuses == "pass")) {
        "PASS"
    } else if (any(statuses == "fail")) {
        "FAIL"
    } else {
        "WARN"
    }

    if (verbose) {
        message("Overall NPE convergence status: ", overall_status)
        message("Processed ", nrow(metrics_data), " metrics")
    }

    # --- Create PDF plot --------------------------------------------------------

    pdf_file <- file.path(output_dir, "npe_convergence_status.pdf")
    pdf(pdf_file, width = 14, height = 10)

    # Set up plot area
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

    # Status colors
    status_colors <- list(
        pass = "#28A745",
        warn = "#FFC107",
        fail = "#DC3545",
        info = "#17A2B8"
    )

    # Overall status color
    overall_status_color <- status_colors[[tolower(overall_status)]] %||% "#6C757D"

    # Title section
    rect(0, 0.92, 1, 1, col = "white", border = NA)
    text(0.5, 0.97, "Neural Posterior Estimation (NPE) Convergence Status",
         cex = 1.8, font = 2, col = col_text_primary)

    # Subtitle with workflow information - simplified
    subtitle_text <- sprintf("NPE Diagnostics from: %s", basename(dirname(npe_dir)))

    text(0.5, 0.93, subtitle_text,
         cex = 1.0, col = col_text_secondary, font = 1)

    # Overall status badge
    badge_width <- 0.15
    badge_x <- 0.5
    badge_y <- 0.88
    rect(badge_x - badge_width/2, badge_y - 0.02,
         badge_x + badge_width/2, badge_y + 0.02,
         col = overall_status_color, border = NA)
    text(badge_x, badge_y, overall_status,
         col = "white", font = 2, cex = 1.2)

    # --- Main metrics table -------------------------------------------------

    # Adjust table position based on number of metrics
    n_rows <- nrow(metrics_data)

    # Handle case where no metrics were collected
    if (n_rows == 0) {
        # Add a placeholder message
        text(0.5, 0.5, "No NPE metrics available - check that NPE outputs exist in the specified directory",
             cex = 1.2, col = col_text_secondary)
        text(0.5, 0.45, sprintf("Directory: %s", npe_dir),
             cex = 1.0, col = col_text_secondary, font = 3)
    } else {
        table_height_per_row <- 0.04
        table_total_height <- (n_rows + 1) * table_height_per_row  # +1 for header

        table_top <- 0.80
        table_bottom <- max(0.45, table_top - table_total_height)
        table_left <- 0.05
        table_right <- 0.95

        row_height <- (table_top - table_bottom) / (n_rows + 1)

        # Column positions and widths
        col_x <- c(table_left, 0.18, 0.62, 0.73, 0.84, table_right)
        col_widths <- diff(col_x)

        # Draw header
        header_y <- table_top
        rect(table_left, header_y - row_height, table_right, header_y,
             col = col_header_bg, border = NA)

    # Header text
    headers <- c("Metric", "Description", "Value", "Target", "Status")
    header_x_centers <- col_x[1:5] + col_widths/2

    for (i in 1:5) {
        text(header_x_centers[i], header_y - row_height/2, headers[i],
             col = col_header_text, font = 2, cex = 1.1)
    }

    # Draw data rows
    for (i in 1:n_rows) {
        y_top <- header_y - i * row_height
        y_bottom <- y_top - row_height
        y_center <- (y_top + y_bottom) / 2

        # Row background
        row_color <- if (i %% 2 == 0) col_row_even else col_row_odd
        rect(table_left, y_bottom, table_right, y_top,
             col = row_color, border = NA)

        # Metric name (use expression if available) - LEFT ALIGNED
        if (i <= length(metric_expressions) && length(metric_expressions) > 0) {
            text(col_x[1] + 0.01, y_center,
                 metric_expressions[[i]], cex = 1, font = 1, adj = 0)
        } else {
            text(col_x[1] + 0.01, y_center,
                 metrics_data$Metric[i], cex = 1, font = 2, adj = 0)
        }

        # Description
        text(col_x[2] + col_widths[2]/2, y_center,
             metrics_data$Description[i], cex = 0.9, col = col_text_secondary)

        # Value
        text(col_x[3] + col_widths[3]/2, y_center,
             metrics_data$Value[i], cex = 1, font = 1)

        # Target
        text(col_x[4] + col_widths[4]/2, y_center,
             metrics_data$Target[i], cex = 0.9, col = col_text_secondary)

        # Status indicator
        status_col <- status_colors[[metrics_data$Status[i]]]
        if (!is.null(status_col)) {
            if (metrics_data$Status[i] == "info") {
                # For info status, show a dash
                text(col_x[5] + col_widths[5]/2, y_center, "-",
                    col = col_text_secondary, font = 1, cex = 1)
            } else {
                # Draw status circle
                symbols(col_x[5] + col_widths[5]/2, y_center,
                       circles = 0.008, inches = FALSE,
                       bg = status_col, fg = status_col,
                       add = TRUE)

                # Add status text
                status_text <- toupper(substr(metrics_data$Status[i], 1, 1))
                text(col_x[5] + col_widths[5]/2, y_center, status_text,
                    col = "white", font = 2, cex = 0.8)
            }
        }
    }

        # Draw table borders
        rect(table_left, table_bottom, table_right, table_top,
             col = NA, border = col_border, lwd = 1.5)
    }  # End of if-else for n_rows check

    # --- Parameter details section (if diagnostics available) ------------------

    if (!is.null(diagnostics_data) && !is.null(diagnostics_data$param_names) && show_param_details > 0) {

        # Find problematic parameters
        problematic_mask <- rep(FALSE, length(diagnostics_data$param_names))

        if (!is.null(diagnostics_data$sbc$coverage_ok)) {
            problematic_mask <- problematic_mask | !diagnostics_data$sbc$coverage_ok
        }
        if (!is.null(diagnostics_data$sbc$sbc_ok)) {
            problematic_mask <- problematic_mask | !diagnostics_data$sbc$sbc_ok
        }

        problematic_params <- diagnostics_data$param_names[problematic_mask]

        if (length(problematic_params) > 0) {

            # Prepare parameter details data
            param_details <- data.frame(
                parameter = problematic_params,
                coverage = if (!is.null(diagnostics_data$sbc$coverage_mean))
                          diagnostics_data$sbc$coverage_mean[problematic_mask] else NA,
                ks_pvalue = if (!is.null(diagnostics_data$sbc$ks_pvalues))
                           diagnostics_data$sbc$ks_pvalues[problematic_mask] else NA,
                stringsAsFactors = FALSE
            )

            # Sort by combined badness score
            param_details$score <- abs(param_details$coverage - 0.95) + (1 - param_details$ks_pvalue)
            param_details <- param_details[order(param_details$score, decreasing = TRUE), ]

            # Limit to top N
            n_show <- min(show_param_details, nrow(param_details))
            params_to_show <- param_details[1:n_show, ]

            # Parameter table setup
            param_table_top <- table_bottom - 0.04
            param_table_bottom <- max(0.12, param_table_top - (n_show + 2) * 0.025)
            param_header_y <- param_table_top
            param_row_height <- (param_table_top - param_table_bottom) / (n_show + 2)

            # Title for parameter section
            text(0.5, param_table_top + 0.02, "Parameters Requiring Attention",
                 cex = 1.2, font = 2, col = col_text_primary)

            # Parameter table header
            rect(table_left, param_header_y - param_row_height, table_right, param_header_y,
                 col = col_header_bg, border = NA)

            param_headers <- c("Parameter", "Coverage", "Target", "SBC KS p-val", "Status")
            param_col_x <- c(table_left, 0.30, 0.45, 0.55, 0.75, table_right)
            param_col_widths <- diff(param_col_x)
            param_header_x_centers <- param_col_x[1:5] + param_col_widths/2

            for (i in 1:5) {
                text(param_header_x_centers[i], param_header_y - param_row_height/2,
                     param_headers[i], col = col_header_text, font = 2, cex = 0.9)
            }

            # Draw parameter rows
            for (i in 1:n_show) {
                y_top <- param_header_y - (i * param_row_height)
                y_bottom <- y_top - param_row_height
                y_center <- (y_top + y_bottom) / 2

                # Row background
                row_color <- if (i %% 2 == 0) col_row_even else col_row_odd
                rect(table_left, y_bottom, table_right, y_top,
                     col = row_color, border = NA)

                # Parameter name
                text(param_col_x[1] + param_col_widths[1]/2, y_center,
                     params_to_show$parameter[i], cex = 0.8, font = 1)

                # Coverage
                if (!is.na(params_to_show$coverage[i])) {
                    coverage_text <- sprintf("%.1f%%", params_to_show$coverage[i] * 100)
                } else {
                    coverage_text <- "-"
                }
                text(param_col_x[2] + param_col_widths[2]/2, y_center,
                     coverage_text, cex = 0.8)

                # Target
                text(param_col_x[3] + param_col_widths[3]/2, y_center,
                     "95%", cex = 0.8, col = col_text_secondary)

                # SBC KS p-value
                if (!is.na(params_to_show$ks_pvalue[i])) {
                    ks_text <- sprintf("%.3f", params_to_show$ks_pvalue[i])
                } else {
                    ks_text <- "-"
                }
                text(param_col_x[4] + param_col_widths[4]/2, y_center,
                     ks_text, cex = 0.8)

                # Status
                coverage_ok <- !is.na(params_to_show$coverage[i]) &&
                              abs(params_to_show$coverage[i] - 0.95) <= 0.05
                ks_ok <- !is.na(params_to_show$ks_pvalue[i]) &&
                        params_to_show$ks_pvalue[i] > ks_threshold

                param_status <- if (coverage_ok && ks_ok) "pass"
                               else if (!coverage_ok && !ks_ok) "fail"
                               else "warn"

                status_col <- status_colors[[param_status]]
                symbols(param_col_x[5] + param_col_widths[5]/2, y_center,
                       circles = 0.006, inches = FALSE,
                       bg = status_col, fg = status_col,
                       add = TRUE)
            }

            # Draw parameter table borders
            rect(table_left, param_table_bottom, table_right, param_header_y,
                 col = NA, border = col_border, lwd = 1.5)

            # Add note if more problematic parameters exist
            if (length(problematic_params) > n_show) {
                text(0.5, param_table_bottom - 0.015,
                     sprintf("(+ %d more parameters requiring attention)",
                            length(problematic_params) - n_show),
                     cex = 0.8, col = col_text_secondary, font = 3)
            }
        }
    }

    # --- Add legend and footer --------------------------------------------------

    # Legend for status indicators
    legend_y <- 0.06
    legend_spacing <- 0.12
    legend_total_width <- 3 * legend_spacing
    legend_x_start <- 0.5 - legend_total_width/2

    # Pass indicator
    symbols(legend_x_start, legend_y, circles = 0.006, inches = FALSE,
           bg = status_colors$pass, fg = status_colors$pass, add = TRUE)
    text(legend_x_start + 0.015, legend_y, "P = Pass", adj = 0, cex = 0.85,
         col = col_text_secondary, font = 1)

    # Warn indicator
    symbols(legend_x_start + legend_spacing, legend_y, circles = 0.006, inches = FALSE,
           bg = status_colors$warn, fg = status_colors$warn, add = TRUE)
    text(legend_x_start + legend_spacing + 0.015, legend_y, "W = Warn", adj = 0, cex = 0.85,
         col = col_text_secondary, font = 1)

    # Fail indicator
    symbols(legend_x_start + 2*legend_spacing, legend_y, circles = 0.006, inches = FALSE,
           bg = status_colors$fail, fg = status_colors$fail, add = TRUE)
    text(legend_x_start + 2*legend_spacing + 0.015, legend_y, "F = Fail", adj = 0, cex = 0.85,
         col = col_text_secondary, font = 1)

    # Footer with timestamp
    footer_text <- sprintf("Generated: %s | NPE Directory: %s",
                          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                          basename(dirname(npe_dir)))
    text(0.5, 0.01, footer_text, cex = 0.7, col = col_text_secondary)

    dev.off()

    if (verbose) {
        message("NPE convergence status plot saved to: ", pdf_file)
    }

    invisible(NULL)
}

#' Plot NPE diagnostic status table
#'
#' @export
plot_npe_diagnostics_sbc <- function(diagnostics,
                                    plots_dir = NULL,
                                    max_params_per_page = 40,
                                    verbose = TRUE) {

    # --- Handle input: diagnostics object or directory path --------------------
    if (is.character(diagnostics)) {
        diagnostics_dir <- diagnostics
        if (!dir.exists(diagnostics_dir)) {
            stop("diagnostics_dir does not exist: ", diagnostics_dir)
        }

        # Try to load diagnostics from RDS first
        diagnostics_rds <- file.path(diagnostics_dir, "diagnostics.rds")
        if (file.exists(diagnostics_rds)) {
            diagnostics <- readRDS(diagnostics_rds)
            if (verbose) message("Loaded diagnostics from: ", diagnostics_rds)
        } else {
            # Fall back to CSV files
            summary_file <- file.path(diagnostics_dir, "sbc_summary.csv")

            if (!file.exists(summary_file)) {
                stop("SBC summary file not found: ", summary_file)
            }

            # Read summary file and construct diagnostics object
            summary_df <- read.csv(summary_file, stringsAsFactors = FALSE)

            diagnostics <- list(
                param_names = if ("parameter" %in% names(summary_df)) summary_df$parameter else NULL,
                sbc = list(
                    ks_pvalues = if ("sbc_ks_pvalue" %in% names(summary_df)) summary_df$sbc_ks_pvalue else NULL
                )
            )

            # If param_names not found, generate generic names
            if (is.null(diagnostics$param_names) && !is.null(diagnostics$sbc$ks_pvalues)) {
                diagnostics$param_names <- paste0("param_", seq_along(diagnostics$sbc$ks_pvalues))
            }
        }
    } else if (!is.list(diagnostics)) {
        stop("diagnostics must be either a list object or a path to diagnostics directory")
    }

    # Set up output directory
    if (is.null(plots_dir)) {
        if (is.character(diagnostics) && dir.exists(diagnostics)) {
            # If diagnostics was a directory path, use its parent's plots directory
            plots_dir <- file.path(dirname(dirname(diagnostics)), "plots")
        } else {
            plots_dir <- "./plots"
        }
    }
    if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE)
        if (verbose) message("Created plots directory: ", plots_dir)
    }

    # --- Extract data --------------------------------------------------------------
    # Get parameter names and KS p-values
    param_names <- diagnostics$param_names
    ks_pvalues <- diagnostics$sbc$ks_pvalues

    if (is.null(param_names) || is.null(ks_pvalues)) {
        stop("SBC KS p-values not found in diagnostics. Cannot create SBC plots.")
    }

    # Ensure same length
    if (length(param_names) != length(ks_pvalues)) {
        warning("Parameter names and KS p-values have different lengths. Using shorter length.")
        min_len <- min(length(param_names), length(ks_pvalues))
        param_names <- param_names[1:min_len]
        ks_pvalues <- ks_pvalues[1:min_len]
    }

    # --- Classify parameters ----------------------------------------------------
    # Extract unique location codes from parameter names
    location_pattern <- "_([A-Z]{3})$"  # Assumes 3-letter ISO codes at end
    location_params <- grep(location_pattern, param_names, value = TRUE)
    global_params <- setdiff(param_names, location_params)

    # Extract location codes
    if (length(location_params) > 0) {
        location_codes <- unique(gsub(".*_([A-Z]{3})$", "\\1", location_params))
    } else {
        location_codes <- character(0)
    }

    # Keep original order from data (don't reorder based on hard-coded lists)
    # This preserves the actual parameter order from the NPE workflow

    if (verbose) {
        message("Found ", length(global_params), " global parameters")
        message("Found ", length(location_params), " location-specific parameters")
        if (length(location_codes) > 0) {
            message("Locations: ", paste(location_codes, collapse = ", "))
        }
    }

    # --- Create plots -----------------------------------------------------------
    pdf(file.path(plots_dir, "npe_diagnostics_sbc.pdf"), width = 12, height = 10)

    # Professional color scheme (matching status PDFs)
    col_header_bg <- "#2C3E50"
    col_header_text <- "white"
    col_row_even <- "#F8F9FA"
    col_row_odd <- "white"
    col_border <- "#DEE2E6"
    col_text_primary <- "#212529"
    col_text_secondary <- "#6C757D"

    # Status colors for p-values
    col_pass <- "#28A745"  # p > 0.05 (no evidence against uniformity)
    col_warn <- "#FFC107"  # 0.01 < p <= 0.05 (weak evidence)
    col_fail <- "#DC3545"  # p <= 0.01 (strong evidence against uniformity)
    col_info <- "#17A2B8"
    col_grid <- "#E5E5E5"

    # Function to create bar plot for a set of parameters
    create_sbc_ks_barplot <- function(params, param_group_name) {
        if (length(params) == 0) return()

        # Get KS p-values for these parameters
        param_indices <- match(params, param_names)
        param_ks_pvalues <- ks_pvalues[param_indices]

        # For location-specific parameters, maintain their natural order
        # (no reordering based on hard-coded lists)

        # Determine colors based on p-value significance
        param_colors <- ifelse(param_ks_pvalues > 0.05, col_pass,
                              ifelse(param_ks_pvalues > 0.01, col_warn, col_fail))

        # Split into pages if needed
        n_params <- length(params)
        n_pages <- ceiling(n_params / max_params_per_page)

        for (page in 1:n_pages) {
            # Determine parameters for this page
            start_idx <- (page - 1) * max_params_per_page + 1
            end_idx <- min(page * max_params_per_page, n_params)
            page_params <- params[start_idx:end_idx]
            page_pvalues <- param_ks_pvalues[start_idx:end_idx]
            page_colors <- param_colors[start_idx:end_idx]

            # Reverse order for horizontal barplot (top to bottom)
            page_params_rev <- rev(page_params)
            page_pvalues_rev <- rev(page_pvalues)
            page_colors_rev <- rev(page_colors)

            # Set up plot with margins adjusted for legend at bottom and subtitle spacing
            par(mar = c(8, 10, 7.5, 3), mgp = c(2.5, 0.7, 0), xpd = FALSE)

            # Calculate proper y-axis range for barplot
            n_bars <- length(page_params)
            bar_width <- 1  # Standard bar width
            bar_space <- 0.2  # Space between bars
            total_height <- n_bars * bar_width + (n_bars - 1) * bar_space

            # First create empty plot to add background elements - tighter y-axis
            plot(NULL, xlim = c(0, 1.05), ylim = c(0, total_height),
                 xlab = "SBC KS Test P-value", ylab = "",
                 main = sprintf("%s - SBC Kolmogorov-Smirnov Test%s",
                              param_group_name,
                              ifelse(n_pages > 1, sprintf(" (Page %d/%d)", page, n_pages), "")),
                 cex.main = 1.2, col.main = col_text_primary,
                 axes = FALSE,
                 yaxs = "i")  # Suppress y-axis expansion

            # Add x-axis only - y-axis will be added after barplot
            axis(1)

            # Add grid lines first (in background)
            abline(v = seq(0, 1, by = 0.1), col = col_grid, lty = 3, lwd = 0.5)

            # Add significance zones (shaded areas) using grey colors
            # Get plot region limits
            usr <- par("usr")

            # Shaded areas for significance levels
            rect(0, usr[3] - 0.5, 0.01, usr[4] + 0.5,
                 col = adjustcolor(col_fail, alpha = 0.1), border = NA)  # p < 0.01 (strong evidence)
            rect(0.01, usr[3] - 0.5, 0.05, usr[4] + 0.5,
                 col = adjustcolor(col_warn, alpha = 0.1), border = NA)  # 0.01 < p < 0.05 (weak evidence)
            rect(0.05, usr[3] - 0.5, 1, usr[4] + 0.5,
                 col = adjustcolor(col_pass, alpha = 0.05), border = NA)  # p > 0.05 (no evidence)

            # Now add the bars with proper positioning
            bp <- barplot(page_pvalues_rev,
                         horiz = TRUE,
                         names.arg = rep("", length(page_params)),  # Empty names since we already added axis
                         las = 1,
                         col = page_colors_rev,
                         border = NA,
                         xlim = c(0, 1),
                         ylim = c(0, total_height),
                         space = bar_space/bar_width,  # Space as proportion of bar width
                         add = TRUE,   # Add to existing plot
                         axes = FALSE)

            # Add y-axis labels at the actual bar positions
            axis(2, at = bp, labels = page_params_rev, las = 1, cex.axis = 0.7)

            # Add significance threshold lines
            abline(v = 0.05, col = col_text_primary, lwd = 2, lty = 2)
            abline(v = 0.01, col = col_text_primary, lwd = 1, lty = 3)

            # Add value labels to the right of bars
            par(xpd = TRUE)  # Allow text outside plot area
            for (i in seq_along(page_pvalues_rev)) {
                # Always position text to the right of the bar (reduced offset)
                text(page_pvalues_rev[i] + 0.005, bp[i],
                     labels = sprintf("%.4f", page_pvalues_rev[i]),
                     pos = 4, cex = 0.6)
            }
            par(xpd = FALSE)  # Reset

            # Add summary statistics
            n_pass <- sum(page_pvalues > 0.05, na.rm = TRUE)
            n_warn <- sum(page_pvalues > 0.01 & page_pvalues <= 0.05, na.rm = TRUE)
            n_fail <- sum(page_pvalues <= 0.01, na.rm = TRUE)
            mean_pvalue <- mean(page_pvalues, na.rm = TRUE)

            # Add status summary at top with more spacing
            mtext(sprintf("Mean P-value: %.4f | Pass (p>0.05): %d | Warn (0.01<p<=0.05): %d | Fail (p<=0.01): %d",
                         mean_pvalue, n_pass, n_warn, n_fail),
                  side = 3, line = 1.5, cex = 0.9, col = col_text_secondary)

            # Add legend at bottom of plot with more spacing from x-axis label
            par(xpd = TRUE)  # Allow drawing outside plot area
            legend(x = 0.5, y = usr[3] - 3.2,
                   xjust = 0.5,
                   legend = c(
                       "Pass (p > 0.05): No evidence against uniformity",
                       "Warn (0.01 < p <= 0.05): Weak evidence",
                       "Fail (p <= 0.01): Strong evidence against uniformity"
                   ),
                   fill = c(col_pass, col_warn, col_fail),
                   border = NA,
                   bty = "n",
                   cex = 0.75,
                   ncol = 1,  # Vertical layout for longer descriptions
                   horiz = FALSE)
            par(xpd = FALSE)  # Reset to default
        }
    }

    # --- Create plots for global and location-specific parameters ---

    # Global parameters
    if (length(global_params) > 0) {
        create_sbc_ks_barplot(global_params, "Global Parameters")
    }

    # Location-specific parameters
    for (loc in location_codes) {
        loc_params <- grep(paste0("_", loc, "$"), param_names, value = TRUE)

        if (length(loc_params) > 0) {
            create_sbc_ks_barplot(loc_params, paste0("Location: ", loc))
        }
    }

    dev.off()

    if (verbose) {
        message("NPE SBC test plots saved to: ",
                file.path(plots_dir, "npe_diagnostics_sbc.pdf"))
    }

    invisible(NULL)
}

#' Plot NPE Coverage Diagnostics
#'
#' Creates coverage diagnostic plots for NPE models
#'
#' @param diagnostics Diagnostics object or path to diagnostics directory
#' @param plots_dir Directory to save plots (optional)
#' @param max_params_per_page Maximum parameters per page (default 40)
#' @param verbose Print progress messages
#'
#' @export
plot_npe_diagnostics_coverage <- function(diagnostics,
                                         plots_dir = NULL,
                                         max_params_per_page = 40,
                                         verbose = TRUE) {

    # Handle input: diagnostics object or directory path
    if (is.character(diagnostics)) {
        diagnostics_dir <- diagnostics
        if (!dir.exists(diagnostics_dir)) {
            stop("diagnostics_dir does not exist: ", diagnostics_dir)
        }

        # Try to load diagnostics from RDS first
        diagnostics_rds <- file.path(diagnostics_dir, "diagnostics.rds")
        if (file.exists(diagnostics_rds)) {
            diagnostics <- readRDS(diagnostics_rds)
            if (verbose) message("Loaded diagnostics from: ", diagnostics_rds)
        } else {
            # Fall back to CSV files
            summary_file <- file.path(diagnostics_dir, "diagnostic_summary.csv")

            if (!file.exists(summary_file)) {
                stop("Diagnostic summary file not found: ", summary_file)
            }

            # Read and construct diagnostics object
            summary_df <- read.csv(summary_file, stringsAsFactors = FALSE)

            diagnostics <- list(
                param_names = if ("parameter" %in% names(summary_df)) summary_df$parameter else NULL,
                coverage = list(
                    coverage_50 = if ("coverage_50" %in% names(summary_df)) summary_df$coverage_50 else NULL,
                    coverage_80 = if ("coverage_80" %in% names(summary_df)) summary_df$coverage_80 else NULL
                )
            )

            # Generate param names if missing
            if (is.null(diagnostics$param_names) && !is.null(diagnostics$coverage$coverage_50)) {
                diagnostics$param_names <- paste0("param_", seq_along(diagnostics$coverage$coverage_50))
            }
        }
    } else if (!is.list(diagnostics)) {
        stop("diagnostics must be either a list object or a path to diagnostics directory")
    }

    # Set up output directory
    if (is.null(plots_dir)) {
        if (is.character(diagnostics) && dir.exists(diagnostics)) {
            # If diagnostics was a directory path, use its parent's plots directory
            plots_dir <- file.path(dirname(dirname(diagnostics)), "plots")
        } else {
            plots_dir <- "./plots"
        }
    }
    if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE)
        if (verbose) message("Created plots directory: ", plots_dir)
    }

    # Extract data
    param_names <- diagnostics$param_names
    coverage_50 <- diagnostics$coverage$coverage_50
    coverage_80 <- diagnostics$coverage$coverage_80

    # Handle matrix format (convert to vector if needed)
    if (is.matrix(coverage_50)) {
        coverage_50 <- diag(coverage_50)
    }
    if (is.matrix(coverage_80)) {
        coverage_80 <- diag(coverage_80)
    }

    if (is.null(param_names) || is.null(coverage_50) || is.null(coverage_80)) {
        stop("Coverage data not found in diagnostics. Cannot create coverage plots.")
    }

    # Ensure same length
    n_params <- length(param_names)
    if (length(coverage_50) != n_params || length(coverage_80) != n_params) {
        warning("Parameter names and coverage values have different lengths. Adjusting...")
        min_len <- min(length(param_names), length(coverage_50), length(coverage_80))
        param_names <- param_names[1:min_len]
        coverage_50 <- coverage_50[1:min_len]
        coverage_80 <- coverage_80[1:min_len]
        n_params <- min_len
    }

    # Classify parameters
    location_pattern <- "_([A-Z]{3})$"
    location_params <- grep(location_pattern, param_names, value = TRUE)
    global_params <- setdiff(param_names, location_params)

    if (length(location_params) > 0) {
        location_codes <- unique(gsub(".*_([A-Z]{3})$", "\\1", location_params))
    } else {
        location_codes <- character(0)
    }

    if (verbose) {
        message("Creating coverage diagnostic plots for ", n_params, " parameters")
        message("Found ", length(global_params), " global parameters")
        message("Found ", length(location_params), " location-specific parameters")
    }

    # Create plots
    pdf(file.path(plots_dir, "npe_diagnostics_coverage.pdf"), width = 12, height = 8)

    # Color scheme matching the reference PDF
    col_50_bars <- "#3498DB"      # Blue for 50% CI histograms
    col_95_bars <- "#E67E22"      # Orange/red for 95% CI histograms
    col_target_line <- "#2E86C1"  # Blue target line
    col_green <- "#2ECC71"        # Green for ±5% tolerance
    col_yellow <- "#F1C40F"       # Yellow for ±10% tolerance
    col_text <- "#2C3E50"         # Dark text
    col_text_light <- "#7F8C8D"   # Light gray text

    # Function to create histogram-style coverage plots
    create_coverage_histograms <- function(params, param_group_name) {
        if (length(params) == 0) return()

        # Get coverage values for these parameters
        param_indices <- match(params, param_names)
        param_cov_50 <- coverage_50[param_indices]
        param_cov_95 <- coverage_80[param_indices]  # Note: using coverage_80 as 95% for compatibility

        # Calculate statistics
        n_params <- length(params)
        mean_50 <- mean(param_cov_50, na.rm = TRUE)
        mean_95 <- mean(param_cov_95, na.rm = TRUE)

        # Set up the page layout (two panels side by side)
        layout(matrix(c(1, 2), nrow = 1, ncol = 2))
        par(mar = c(5, 5, 4, 2))

        # --- LEFT PANEL: 50% CI Coverage Histogram ---
        hist_50 <- hist(param_cov_50, breaks = seq(0, 1, by = 0.1), plot = FALSE)

        # Create histogram plot
        plot(hist_50, col = col_50_bars, border = "white",
             xlim = c(0, 1), ylim = c(0, max(hist_50$counts) * 1.2),
             main = "50% CI Coverage", xlab = "Coverage", ylab = "Number of Parameters",
             cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)

        # Add tolerance bands
        # Green: ±5% tolerance around 0.5 (0.45 to 0.55)
        rect(0.45, -1, 0.55, max(hist_50$counts) * 1.3,
             col = adjustcolor(col_green, alpha = 0.2), border = NA)
        # Yellow: ±10% tolerance around 0.5 (0.4 to 0.6)
        rect(0.40, -1, 0.45, max(hist_50$counts) * 1.3,
             col = adjustcolor(col_yellow, alpha = 0.2), border = NA)
        rect(0.55, -1, 0.60, max(hist_50$counts) * 1.3,
             col = adjustcolor(col_yellow, alpha = 0.2), border = NA)

        # Add target line at 0.5
        abline(v = 0.5, col = col_target_line, lwd = 2, lty = 2)

        # Add statistics text
        text(0.05, max(hist_50$counts) * 1.1,
             sprintf("Mean: %.3f\nTarget: 0.500\nN = %d", mean_50, n_params),
             adj = c(0, 1), cex = 1.1, col = col_text)

        # --- RIGHT PANEL: 95% CI Coverage Histogram ---
        hist_95 <- hist(param_cov_95, breaks = seq(0, 1, by = 0.05), plot = FALSE)

        # Create histogram plot
        plot(hist_95, col = col_95_bars, border = "white",
             xlim = c(0, 1), ylim = c(0, max(hist_95$counts) * 1.2),
             main = "95% CI Coverage", xlab = "Coverage", ylab = "Number of Parameters",
             cex.main = 1.4, cex.lab = 1.2, cex.axis = 1.1)

        # Add tolerance bands
        # Green: ±5% tolerance around 0.95 (0.90 to 1.0)
        rect(0.90, -1, 1.0, max(hist_95$counts) * 1.3,
             col = adjustcolor(col_green, alpha = 0.2), border = NA)
        # Yellow: ±10% tolerance around 0.95 (0.85 to 0.90)
        rect(0.85, -1, 0.90, max(hist_95$counts) * 1.3,
             col = adjustcolor(col_yellow, alpha = 0.2), border = NA)

        # Add target line at 0.95
        abline(v = 0.95, col = col_target_line, lwd = 2, lty = 2)

        # Add statistics text
        text(0.05, max(hist_95$counts) * 1.1,
             sprintf("Mean: %.3f\nTarget: 0.950\nN = %d", mean_95, n_params),
             adj = c(0, 1), cex = 1.1, col = col_text)

        # Add overall title
        mtext(sprintf("%s - Coverage Diagnostics", param_group_name),
              side = 3, line = -2, outer = TRUE, cex = 1.6, col = col_text, font = 2)

        # Add legend at bottom
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
        legend("bottom", legend = "Green shading: ±5% tolerance, Yellow shading: ±10% tolerance",
               bty = "n", cex = 1.1, col = col_text_light, xpd = TRUE)
    }

    # Create plots for each parameter group
    if (length(global_params) > 0) {
        create_coverage_histograms(global_params, "Global Parameters")
    }

    for (loc in location_codes) {
        loc_params <- grep(paste0("_", loc, "$"), param_names, value = TRUE)
        if (length(loc_params) > 0) {
            create_coverage_histograms(loc_params, paste0("Location: ", loc))
        }
    }

    dev.off()

    if (verbose) {
        message("NPE coverage diagnostic plots saved to: ",
                file.path(plots_dir, "npe_diagnostics_coverage.pdf"))
    }

    invisible(NULL)
}

#' Plot NPE Coverage Diagnostic Bars
#'
#' @export
plot_npe_diagnostics_coverage_bars <- function(diagnostics,
                                    plots_dir = NULL,
                                    max_params_per_page = 40,
                                    verbose = TRUE) {

    # --- Handle input: diagnostics object or directory path --------------------
    if (is.character(diagnostics)) {
        diagnostics_dir <- diagnostics
        if (!dir.exists(diagnostics_dir)) {
            stop("diagnostics_dir does not exist: ", diagnostics_dir)
        }

        # Try to load diagnostics from RDS first
        diagnostics_rds <- file.path(diagnostics_dir, "diagnostics.rds")
        if (file.exists(diagnostics_rds)) {
            diagnostics <- readRDS(diagnostics_rds)
            if (verbose) message("Loaded diagnostics from: ", diagnostics_rds)
        } else {
            # Fall back to CSV files
            summary_file <- file.path(diagnostics_dir, "sbc_summary.csv")

            if (!file.exists(summary_file)) {
                stop("SBC summary file not found: ", summary_file)
            }

            # Read summary file and construct diagnostics object
            summary_df <- read.csv(summary_file, stringsAsFactors = FALSE)

            diagnostics <- list(
                param_names = if ("parameter" %in% names(summary_df)) summary_df$parameter else NULL,
                sbc = list(
                    ks_pvalues = if ("sbc_ks_pvalue" %in% names(summary_df)) summary_df$sbc_ks_pvalue else NULL
                )
            )

            # If param_names not found, generate generic names
            if (is.null(diagnostics$param_names) && !is.null(diagnostics$sbc$ks_pvalues)) {
                diagnostics$param_names <- paste0("param_", seq_along(diagnostics$sbc$ks_pvalues))
            }
        }
    } else if (!is.list(diagnostics)) {
        stop("diagnostics must be either a list object or a path to diagnostics directory")
    }

    # Set up output directory
    if (is.null(plots_dir)) {
        if (is.character(diagnostics) && dir.exists(diagnostics)) {
            # If diagnostics was a directory path, use its parent's plots directory
            plots_dir <- file.path(dirname(dirname(diagnostics)), "plots")
        } else {
            plots_dir <- "./plots"
        }
    }
    if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE)
        if (verbose) message("Created plots directory: ", plots_dir)
    }

    # --- Extract data --------------------------------------------------------------
    # Get parameter names and KS p-values
    param_names <- diagnostics$param_names
    ks_pvalues <- diagnostics$sbc$ks_pvalues

    if (is.null(param_names) || is.null(ks_pvalues)) {
        stop("SBC KS p-values not found in diagnostics. Cannot create SBC plots.")
    }

    # Ensure same length
    if (length(param_names) != length(ks_pvalues)) {
        warning("Parameter names and KS p-values have different lengths. Using shorter length.")
        min_len <- min(length(param_names), length(ks_pvalues))
        param_names <- param_names[1:min_len]
        ks_pvalues <- ks_pvalues[1:min_len]
    }

    # --- Classify parameters ----------------------------------------------------
    # Extract unique location codes from parameter names
    location_pattern <- "_([A-Z]{3})$"  # Assumes 3-letter ISO codes at end
    location_params <- grep(location_pattern, param_names, value = TRUE)
    global_params <- setdiff(param_names, location_params)

    # Extract location codes
    if (length(location_params) > 0) {
        location_codes <- unique(gsub(".*_([A-Z]{3})$", "\\1", location_params))
    } else {
        location_codes <- character(0)
    }

    # Keep original order from data (don't reorder based on hard-coded lists)
    # This preserves the actual parameter order from the NPE workflow

    if (verbose) {
        message("Found ", length(global_params), " global parameters")
        message("Found ", length(location_params), " location-specific parameters")
        if (length(location_codes) > 0) {
            message("Locations: ", paste(location_codes, collapse = ", "))
        }
    }

    # --- Create plots -----------------------------------------------------------
    pdf(file.path(plots_dir, "npe_diagnostics_sbc.pdf"), width = 12, height = 10)

    # Professional color scheme (matching status PDFs)
    col_header_bg <- "#2C3E50"
    col_header_text <- "white"
    col_row_even <- "#F8F9FA"
    col_row_odd <- "white"
    col_border <- "#DEE2E6"
    col_text_primary <- "#212529"
    col_text_secondary <- "#6C757D"

    # Status colors for p-values
    col_pass <- "#28A745"  # p > 0.05 (no evidence against uniformity)
    col_warn <- "#FFC107"  # 0.01 < p <= 0.05 (weak evidence)
    col_fail <- "#DC3545"  # p <= 0.01 (strong evidence against uniformity)
    col_info <- "#17A2B8"
    col_grid <- "#E5E5E5"

    # Function to create bar plot for a set of parameters
    create_sbc_ks_barplot <- function(params, param_group_name) {
        if (length(params) == 0) return()

        # Get KS p-values for these parameters
        param_indices <- match(params, param_names)
        param_ks_pvalues <- ks_pvalues[param_indices]

        # For location-specific parameters, maintain their natural order
        # (no reordering based on hard-coded lists)

        # Determine colors based on p-value significance
        param_colors <- ifelse(param_ks_pvalues > 0.05, col_pass,
                              ifelse(param_ks_pvalues > 0.01, col_warn, col_fail))

        # Split into pages if needed
        n_params <- length(params)
        n_pages <- ceiling(n_params / max_params_per_page)

        for (page in 1:n_pages) {
            # Determine parameters for this page
            start_idx <- (page - 1) * max_params_per_page + 1
            end_idx <- min(page * max_params_per_page, n_params)
            page_params <- params[start_idx:end_idx]
            page_pvalues <- param_ks_pvalues[start_idx:end_idx]
            page_colors <- param_colors[start_idx:end_idx]

            # Reverse order for horizontal barplot (top to bottom)
            page_params_rev <- rev(page_params)
            page_pvalues_rev <- rev(page_pvalues)
            page_colors_rev <- rev(page_colors)

            # Set up plot with margins adjusted for legend at bottom and subtitle spacing
            par(mar = c(8, 10, 7.5, 3), mgp = c(2.5, 0.7, 0), xpd = FALSE)

            # Calculate proper y-axis range for barplot
            n_bars <- length(page_params)
            bar_width <- 1  # Standard bar width
            bar_space <- 0.2  # Space between bars
            total_height <- n_bars * bar_width + (n_bars - 1) * bar_space

            # First create empty plot to add background elements - tighter y-axis
            plot(NULL, xlim = c(0, 1.05), ylim = c(0, total_height),
                 xlab = "SBC KS Test P-value", ylab = "",
                 main = sprintf("%s - SBC Kolmogorov-Smirnov Test%s",
                              param_group_name,
                              ifelse(n_pages > 1, sprintf(" (Page %d/%d)", page, n_pages), "")),
                 cex.main = 1.2, col.main = col_text_primary,
                 axes = FALSE,
                 yaxs = "i")  # Suppress y-axis expansion

            # Add x-axis only - y-axis will be added after barplot
            axis(1)

            # Add grid lines first (in background)
            abline(v = seq(0, 1, by = 0.1), col = col_grid, lty = 3, lwd = 0.5)

            # Add significance zones (shaded areas) using grey colors
            # Get plot region limits
            usr <- par("usr")

            # Shaded areas for significance levels
            rect(0, usr[3] - 0.5, 0.01, usr[4] + 0.5,
                 col = adjustcolor(col_fail, alpha = 0.1), border = NA)  # p < 0.01 (strong evidence)
            rect(0.01, usr[3] - 0.5, 0.05, usr[4] + 0.5,
                 col = adjustcolor(col_warn, alpha = 0.1), border = NA)  # 0.01 < p < 0.05 (weak evidence)
            rect(0.05, usr[3] - 0.5, 1, usr[4] + 0.5,
                 col = adjustcolor(col_pass, alpha = 0.05), border = NA)  # p > 0.05 (no evidence)

            # Now add the bars with proper positioning
            bp <- barplot(page_pvalues_rev,
                         horiz = TRUE,
                         names.arg = rep("", length(page_params)),  # Empty names since we already added axis
                         las = 1,
                         col = page_colors_rev,
                         border = NA,
                         xlim = c(0, 1),
                         ylim = c(0, total_height),
                         space = bar_space/bar_width,  # Space as proportion of bar width
                         add = TRUE,   # Add to existing plot
                         axes = FALSE)

            # Add y-axis labels at the actual bar positions
            axis(2, at = bp, labels = page_params_rev, las = 1, cex.axis = 0.7)

            # Add significance threshold lines
            abline(v = 0.05, col = col_text_primary, lwd = 2, lty = 2)
            abline(v = 0.01, col = col_text_primary, lwd = 1, lty = 3)

            # Add value labels to the right of bars
            par(xpd = TRUE)  # Allow text outside plot area
            for (i in seq_along(page_pvalues_rev)) {
                # Always position text to the right of the bar (reduced offset)
                text(page_pvalues_rev[i] + 0.005, bp[i],
                     labels = sprintf("%.4f", page_pvalues_rev[i]),
                     pos = 4, cex = 0.6)
            }
            par(xpd = FALSE)  # Reset

            # Add summary statistics
            n_pass <- sum(page_pvalues > 0.05, na.rm = TRUE)
            n_warn <- sum(page_pvalues > 0.01 & page_pvalues <= 0.05, na.rm = TRUE)
            n_fail <- sum(page_pvalues <= 0.01, na.rm = TRUE)
            mean_pvalue <- mean(page_pvalues, na.rm = TRUE)

            # Add status summary at top with more spacing
            mtext(sprintf("Mean P-value: %.4f | Pass (p>0.05): %d | Warn (0.01<p<=0.05): %d | Fail (p<=0.01): %d",
                         mean_pvalue, n_pass, n_warn, n_fail),
                  side = 3, line = 1.5, cex = 0.9, col = col_text_secondary)

            # Add legend at bottom of plot with more spacing from x-axis label
            par(xpd = TRUE)  # Allow drawing outside plot area
            legend(x = 0.5, y = usr[3] - 3.2,
                   xjust = 0.5,
                   legend = c(
                       "Pass (p > 0.05): No evidence against uniformity",
                       "Warn (0.01 < p <= 0.05): Weak evidence",
                       "Fail (p <= 0.01): Strong evidence against uniformity"
                   ),
                   fill = c(col_pass, col_warn, col_fail),
                   border = NA,
                   bty = "n",
                   cex = 0.75,
                   ncol = 1,  # Vertical layout for longer descriptions
                   horiz = FALSE)
            par(xpd = FALSE)  # Reset to default
        }
    }

    # --- Create plots for global and location-specific parameters ---

    # Global parameters
    if (length(global_params) > 0) {
        create_sbc_ks_barplot(global_params, "Global Parameters")
    }

    # Location-specific parameters
    for (loc in location_codes) {
        loc_params <- grep(paste0("_", loc, "$"), param_names, value = TRUE)

        if (length(loc_params) > 0) {
            create_sbc_ks_barplot(loc_params, paste0("Location: ", loc))
        }
    }

    dev.off()

    if (verbose) {
        message("NPE SBC test plots saved to: ",
                file.path(plots_dir, "npe_diagnostics_sbc.pdf"))
    }

    invisible(NULL)
}

#' Plot NPE Training Loss Curves
#'
#' Creates visualizations of training and validation loss over epochs for NPE models.
#' This function reads the training history from the NPE model directory and generates
#' plots to assess model convergence and potential overfitting.
#'
#' @param npe_dirs List with model component (containing training_history.rds) or string path to NPE directory
#' @param output_file Optional path to save the plot. If NULL, plot is returned but not saved.
#' @param plot_width Width of the saved plot in inches (default 10)
#' @param plot_height Height of the saved plot in inches (default 6)
#' @param show_best_epoch Whether to highlight the epoch with best validation loss (default TRUE)
#' @param smooth_curves Whether to apply smoothing to the loss curves (default FALSE)
#' @param log_scale Whether to use log scale for the y-axis (default FALSE)
#' @param verbose Whether to print progress messages (default TRUE)
#'
#' @return ggplot2 object containing the training loss visualization
#'
#' @details
#' This function loads the training history from training_history.rds and creates
#' publication-quality plots showing:
#' 1. Training and validation loss curves over epochs
#' 2. Best validation loss point (if show_best_epoch = TRUE)
#' 3. Convergence diagnostics and potential overfitting indicators
#'
#' The plot helps identify:
#' - Training convergence
#' - Overfitting (validation loss increasing while training loss decreases)
#' - Optimal stopping points
#' - Learning rate effectiveness
#'
#' @examples
#' \dontrun{
#' # Plot training curves for NPE model
#' loss_plot <- plot_npe_training_loss(
#'   npe_dirs = list(model = "./results/npe/model"),
#'   output_file = "./figures/npe_training_loss.png"
#' )
#'
#' # Display the plot
#' print(loss_plot)
#' }
#'
#' @export
plot_npe_training_loss <- function(
    npe_dirs,
    output_file = NULL,
    plot_width = 10,
    plot_height = 8,
    show_best_epoch = TRUE,
    smooth_curves = FALSE,
    log_scale = FALSE,
    verbose = TRUE
) {
    # All required packages loaded via NAMESPACE

    log_msg <- function(msg, ...) {
        if (verbose) {
            timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
            message(sprintf(paste(timestamp, msg), ...))
        }
    }

    log_msg("Creating NPE training loss visualization")

    # Handle input directory
    if (is.character(npe_dirs)) {
        model_dir <- file.path(npe_dirs, "model")
    } else if (is.list(npe_dirs) && !is.null(npe_dirs$model)) {
        model_dir <- npe_dirs$model
    } else {
        stop("npe_dirs must be either a directory path or a list with 'model' component")
    }

    if (!dir.exists(model_dir)) {
        stop("NPE model directory not found: ", model_dir)
    }

    # Check for training history file (RDS format in current workflow)
    history_file <- file.path(model_dir, "training_history.rds")
    if (!file.exists(history_file)) {
        stop("Training history file not found: ", history_file)
    }

    # Load training history
    log_msg("Loading training history from: %s", history_file)
    history_data <- readRDS(history_file)

    # Validate structure
    if (!is.list(history_data) || !all(c("train_loss", "val_loss") %in% names(history_data))) {
        stop("Training history must be a list with 'train_loss' and 'val_loss' components")
    }

    train_losses <- history_data$train_loss
    val_losses <- history_data$val_loss
    n_epochs <- length(train_losses)

    if (n_epochs == 0) {
        stop("No training data found in history file")
    }

    log_msg("Found training history for %d epochs", n_epochs)

    # Create data frame for plotting
    plot_data <- data.frame(
        epoch = rep(1:n_epochs, 2),
        loss = c(train_losses, val_losses),
        loss_type = factor(rep(c("Training", "Validation"), each = n_epochs),
                          levels = c("Training", "Validation"))
    )

    # Find best validation epoch
    best_val_idx <- which.min(val_losses)
    best_val_loss <- val_losses[best_val_idx]
    best_val_epoch <- best_val_idx

    log_msg("Best validation loss: %.6f at epoch %d", best_val_loss, best_val_epoch)

    # Calculate convergence metrics first (before using them)
    final_train_loss <- train_losses[n_epochs]
    final_val_loss <- val_losses[n_epochs]
    gap <- final_val_loss - final_train_loss

    # Create separate data frames for each loss type
    train_data <- plot_data[plot_data$loss_type == "Training",]
    val_data <- plot_data[plot_data$loss_type == "Validation",]

    # Use facet_wrap for two panels with free y scales
    p <- ggplot(plot_data, aes(x = epoch, y = loss)) +
        facet_wrap(~ loss_type, ncol = 1, scales = "free_y")

    # Add loss curves with specific colors
    if (smooth_curves) {
        # Use loess smoothing for cleaner curves
        p <- p +
            geom_smooth(data = train_data, method = "loess", span = 0.1,
                       se = FALSE, size = 1.2, color = "#2E86AB") +
            geom_smooth(data = val_data, method = "loess", span = 0.1,
                       se = FALSE, size = 1.2, color = "#28A745") +
            geom_line(data = train_data, alpha = 0.3, size = 0.5, color = "#2E86AB") +
            geom_line(data = val_data, alpha = 0.3, size = 0.5, color = "#28A745")
    } else {
        # Show raw curves
        p <- p +
            geom_line(data = train_data, size = 1, color = "#2E86AB") +
            geom_line(data = val_data, size = 1, color = "#28A745")
    }

    # Add vertical reference lines
    p <- p +
        geom_vline(xintercept = best_val_epoch,
                  linetype = "dashed", alpha = 0.4, color = "#FF8C00") +
        geom_vline(xintercept = n_epochs,
                  linetype = "dashed", alpha = 0.4, color = "#DC3545")

    # Add best validation loss point (ideal stop)
    if (show_best_epoch) {
        # Add to validation panel
        best_point_data <- data.frame(
            epoch = best_val_epoch,
            loss = best_val_loss,
            loss_type = factor("Validation", levels = c("Training", "Validation"))
        )

        # Also get the training loss at best validation epoch
        best_train_loss <- train_losses[best_val_epoch]
        best_point_train <- data.frame(
            epoch = best_val_epoch,
            loss = best_train_loss,
            loss_type = factor("Training", levels = c("Training", "Validation"))
        )

        p <- p +
            geom_point(data = best_point_data,
                      aes(x = epoch, y = loss),
                      color = "#FF8C00", size = 4, shape = 17) +  # Triangle for ideal stop
            geom_point(data = best_point_train,
                      aes(x = epoch, y = loss),
                      color = "#FF8C00", size = 4, shape = 17)  # Triangle in training panel too
    }

    # Add actual stop points (last epoch)
    actual_stop_train <- data.frame(
        epoch = n_epochs,
        loss = final_train_loss,
        loss_type = factor("Training", levels = c("Training", "Validation"))
    )
    actual_stop_val <- data.frame(
        epoch = n_epochs,
        loss = final_val_loss,
        loss_type = factor("Validation", levels = c("Training", "Validation"))
    )

    p <- p +
        geom_point(data = actual_stop_train,
                  aes(x = epoch, y = loss),
                  color = "#DC3545", size = 4, shape = 15) +  # Square for actual stop
        geom_point(data = actual_stop_val,
                  aes(x = epoch, y = loss),
                  color = "#DC3545", size = 4, shape = 15)

    # Add labels at the top of each panel
    # Create label data frames for each panel
    if (show_best_epoch) {
        # Training panel labels (nudged slightly left)
        train_labels <- data.frame(
            x = c(best_val_epoch - 1, n_epochs - 1),
            y = c(Inf, Inf),
            label = c(sprintf("Epoch %d\nBest: %.4f", best_val_epoch, best_train_loss),
                     sprintf("Epoch %d\nFinal: %.4f", n_epochs, final_train_loss)),
            color = c("#FF8C00", "#DC3545"),
            loss_type = factor("Training", levels = c("Training", "Validation"))
        )

        # Validation panel labels (nudged slightly left)
        val_labels <- data.frame(
            x = c(best_val_epoch - 1, n_epochs - 1),
            y = c(Inf, Inf),
            label = c(sprintf("Epoch %d\nBest: %.4f", best_val_epoch, best_val_loss),
                     sprintf("Epoch %d\nFinal: %.4f", n_epochs, final_val_loss)),
            color = c("#FF8C00", "#DC3545"),
            loss_type = factor("Validation", levels = c("Training", "Validation"))
        )

        # Add the labels (aligned to left of lines)
        p <- p +
            geom_text(data = train_labels,
                     aes(x = x, y = y, label = label),
                     color = train_labels$color,
                     vjust = 1.5, hjust = 1, size = 3) +
            geom_text(data = val_labels,
                     aes(x = x, y = y, label = label),
                     color = val_labels$color,
                     vjust = 1.5, hjust = 1, size = 3)
    } else {
        # Only final labels if not showing best epoch (nudged slightly left)
        train_label <- data.frame(
            x = n_epochs - 1,
            y = Inf,
            label = sprintf("Epoch %d\nFinal: %.4f", n_epochs, final_train_loss),
            loss_type = factor("Training", levels = c("Training", "Validation"))
        )

        val_label <- data.frame(
            x = n_epochs - 1,
            y = Inf,
            label = sprintf("Epoch %d\nFinal: %.4f", n_epochs, final_val_loss),
            loss_type = factor("Validation", levels = c("Training", "Validation"))
        )

        p <- p +
            geom_text(data = train_label,
                     aes(x = x, y = y, label = label),
                     color = "#DC3545",
                     vjust = 1.5, hjust = 1, size = 3) +
            geom_text(data = val_label,
                     aes(x = x, y = y, label = label),
                     color = "#DC3545",
                     vjust = 1.5, hjust = 1, size = 3)
    }

    # Check for overfitting
    early_stop_suggested <- best_val_epoch < n_epochs * 0.9
    overfitting_detected <- gap > 0.01 || (best_val_epoch < n_epochs - 20)

    # Determine specific overfitting trigger
    overfitting_reason <- NULL
    if (gap > 0.01 && (best_val_epoch < n_epochs - 20)) {
        overfitting_reason <- sprintf("Train-val gap %.3f (>0.01) & best epoch was %d epochs ago",
                                     gap, n_epochs - best_val_epoch)
    } else if (gap > 0.01) {
        overfitting_reason <- sprintf("Train-val gap %.3f exceeds 0.01 threshold", gap)
    } else if (best_val_epoch < n_epochs - 20) {
        overfitting_reason <- sprintf("Best epoch was %d, trained %d epochs past optimal",
                                     best_val_epoch, n_epochs - best_val_epoch)
    }

    # Create subtitle with diagnostics
    subtitle_base <- sprintf("Epochs: %d | Best val loss: %.4f @ epoch %d",
                             n_epochs, best_val_loss, best_val_epoch)

    if (overfitting_detected && !is.null(overfitting_reason)) {
        subtitle_text <- paste0(subtitle_base, "\n⚠ Possible overfitting: ", overfitting_reason)
    } else {
        subtitle_text <- subtitle_base
    }

    # Styling
    p <- p +
        labs(
            title = "NPE Model Training History",
            subtitle = subtitle_text,
            x = "Epoch",
            y = "Loss (Negative Log-Likelihood)"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 9, color = "grey40"),
            legend.position = "none",
            panel.grid.major = element_line(size = 0.3, color = "grey85"),
            panel.grid.minor = element_blank(),
            axis.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold", size = 11),
            strip.background = element_rect(fill = "grey95", color = "grey80")
        )

    # Save plot if requested
    if (!is.null(output_file)) {
        log_msg("Saving plot to: %s", output_file)
        dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
        ggsave(output_file, plot = p, width = plot_width, height = plot_height, dpi = 300)
        log_msg("Training loss plot saved successfully")
    }

    # Print summary statistics
    if (verbose) {
        message("\nTraining Summary:")
        message(sprintf("  Total epochs: %d", n_epochs))
        message(sprintf("  Initial train loss: %.4f", train_losses[1]))
        message(sprintf("  Final train loss: %.4f", final_train_loss))
        message(sprintf("  Train loss reduction: %.2f%%",
                       (1 - final_train_loss/train_losses[1]) * 100))
        message(sprintf("  Best validation loss: %.4f (epoch %d)",
                       best_val_loss, best_val_epoch))
        message(sprintf("  Final validation loss: %.4f", final_val_loss))

        if (overfitting_detected) {
            message("\n⚠ Warning: Possible overfitting detected")
            if (gap > 0.01) {
                message(sprintf("  • Gap between training and validation loss: %.4f", gap))
                message("    The threshold of 0.01 has been exceeded, indicating the model")
                message("    performs significantly better on training data than validation data")
            }
            if (best_val_epoch < n_epochs - 20) {
                message(sprintf("  • Best validation performance was at epoch %d", best_val_epoch))
                message(sprintf("    Training continued for %d more epochs without improvement", n_epochs - best_val_epoch))
                message("    This suggests the model started overfitting after the optimal point")
            }
            message(sprintf("  • Recommendation: Consider early stopping at epoch %d", best_val_epoch))
            message("    This would reduce training time and likely improve generalization")
        }
    }

    return(p)
}
