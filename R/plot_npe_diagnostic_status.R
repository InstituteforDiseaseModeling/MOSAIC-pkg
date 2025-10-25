#' Plot NPE diagnostic status table
#'
#' Creates a visual status table showing NPE SBC diagnostics with color-coded
#' pass/warn/fail indicators for each metric, similar to convergence status plots.
#'
#' @param diagnostics_dir Path to NPE diagnostics directory containing SBC results
#'   (expects "sbc_diagnostics.json" and "sbc_summary.csv")
#' @param plots_dir Path to plots directory (default: creates "plots" in parent of diagnostics_dir)
#' @param coverage_target Expected coverage level (default: 0.95 for 95% CI)
#' @param ks_threshold P-value threshold for KS test (default: 0.05)
#' @param n_params_display Maximum number of parameters to display in detail (default: 15)
#' @param verbose Logical indicating whether to print messages
#'
#' @return Invisible NULL. Creates a PDF file with the NPE diagnostic status table.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create NPE diagnostic status table
#' plot_npe_diagnostic_status(
#'   diagnostics_dir = "./local/calibration/calibration_test_13/npe/diagnostics"
#' )
#' }
plot_npe_diagnostic_status <- function(diagnostics_dir,
                                      plots_dir = NULL,
                                      coverage_target = 0.95,
                                      ks_threshold = 0.05,
                                      n_params_display = 15,
                                      verbose = TRUE) {

    # --- I/O checks -------------------------------------------------------------
    if (!dir.exists(diagnostics_dir)) stop("diagnostics_dir does not exist: ", diagnostics_dir)
    if (is.null(plots_dir)) plots_dir <- file.path(dirname(dirname(diagnostics_dir)), "plots")
    if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE)
        if (verbose) message("Created plots directory: ", plots_dir)
    }

    # Check for required files
    diagnostics_file <- file.path(diagnostics_dir, "sbc_diagnostics.json")
    summary_file <- file.path(diagnostics_dir, "sbc_summary.csv")
    
    if (!file.exists(diagnostics_file)) {
        stop("SBC diagnostics file not found: ", diagnostics_file)
    }
    if (!file.exists(summary_file)) {
        stop("SBC summary file not found: ", summary_file)
    }

    if (verbose) {
        message("Reading NPE diagnostics from: ", diagnostics_dir)
    }

    # --- Read diagnostics -------------------------------------------------------
    diagnostics <- jsonlite::read_json(diagnostics_file)
    summary_df <- read.csv(summary_file, stringsAsFactors = FALSE)

    # --- Calculate coverage at different CI levels ------------------------------
    # Read the raw coverage matrix to calculate coverage at different levels
    coverage_file <- file.path(diagnostics_dir, "sbc_coverage.csv")
    if (file.exists(coverage_file)) {
        coverage_matrix <- read.csv(coverage_file)

        # Calculate 95% CI coverage (using the overall from diagnostics)
        coverage_95 <- diagnostics$overall_coverage
        coverage_95_se <- diagnostics$overall_coverage_se

        # Calculate 50% CI coverage if we have the data
        # Need to check if parameters have 0.25 and 0.75 quantiles in the summary
        if ("sbc_rank_p25" %in% names(summary_df) && "sbc_rank_p75" %in% names(summary_df)) {
            # For 50% CI, check if rank is between 0.25 and 0.75
            ranks_file <- file.path(diagnostics_dir, "sbc_ranks.csv")
            if (file.exists(ranks_file)) {
                ranks_matrix <- read.csv(ranks_file)
                coverage_50_matrix <- (ranks_matrix >= 0.25 & ranks_matrix <= 0.75)
                coverage_50 <- mean(as.matrix(coverage_50_matrix), na.rm = TRUE)
                coverage_50_se <- sd(as.vector(as.matrix(coverage_50_matrix)), na.rm = TRUE) /
                                 sqrt(sum(!is.na(as.matrix(coverage_50_matrix))))
            } else {
                coverage_50 <- NA
                coverage_50_se <- NA
            }
        } else {
            coverage_50 <- NA
            coverage_50_se <- NA
        }
    } else {
        coverage_50 <- NA
        coverage_50_se <- NA
    }

    # --- Prepare overall metrics data -------------------------------------------
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

    # Coverage Metrics (grouped together)
    # Row 1: 50% CI Coverage (if available)
    if (!is.na(coverage_50)) {
        coverage_50_deviation <- abs(coverage_50 - 0.50)
        status_coverage_50 <- if (coverage_50_deviation <= 0.05) "pass" else if (coverage_50_deviation <= 0.10) "warn" else "fail"

        metrics_data <- rbind(metrics_data, data.frame(
            Metric = "Coverage_50",
            Description = "Overall parameter coverage (50% CI)",
            Target = "50%",
            Value = sprintf("%.1f%% ± %.1f%%", coverage_50 * 100, coverage_50_se * 100),
            Status = status_coverage_50,
            stringsAsFactors = FALSE
        ))
        metric_expressions[[1]] <- expression(bold(Coverage["50%"]))
    }

    # Row 2: 95% CI Coverage
    coverage_val <- diagnostics$overall_coverage
    coverage_deviation <- abs(coverage_val - 0.95)
    status_coverage <- if (coverage_deviation <= 0.05) "pass" else if (coverage_deviation <= 0.10) "warn" else "fail"

    metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Coverage_95",
        Description = "Overall parameter coverage (95% CI)",
        Target = "95%",
        Value = sprintf("%.1f%% ± %.1f%%", coverage_val * 100, diagnostics$overall_coverage_se * 100),
        Status = status_coverage,
        stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- expression(bold(Coverage["95%"]))

    # Row 3: Parameters with good coverage
    n_params_good_coverage <- sum(summary_df$coverage_ok, na.rm = TRUE)
    n_params_total <- nrow(summary_df)
    pct_good_coverage <- n_params_good_coverage / n_params_total
    status_param_coverage <- if (pct_good_coverage >= 0.9) "pass" else if (pct_good_coverage >= 0.75) "warn" else "fail"

    metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Params_Coverage",
        Description = "Parameters with good coverage (95% CI)",
        Target = sprintf(">=%d", round(n_params_total * 0.9)),
        Value = sprintf("%d/%d (%.0f%%)", n_params_good_coverage, n_params_total, pct_good_coverage * 100),
        Status = status_param_coverage,
        stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- expression(bold("Params (Coverage)"))

    # SBC Metrics (grouped together)
    # Row 4: SBC KS Test
    ks_pval <- diagnostics$overall_sbc_ks_pvalue
    status_ks <- if (ks_pval > ks_threshold) "pass" else if (ks_pval > ks_threshold/2) "warn" else "fail"

    metrics_data <- rbind(metrics_data, data.frame(
        Metric = "SBC_KS",
        Description = "Kolmogorov-Smirnov test for rank uniformity",
        Target = sprintf(">%.2f", ks_threshold),
        Value = sprintf("%.3f", ks_pval),
        Status = status_ks,
        stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- expression(bold(paste("SBC ", KS[p-value])))

    # Row 5: Parameters with uniform SBC ranks
    n_params_good_sbc <- sum(summary_df$sbc_ok, na.rm = TRUE)
    pct_good_sbc <- n_params_good_sbc / n_params_total
    status_param_sbc <- if (pct_good_sbc >= 0.9) "pass" else if (pct_good_sbc >= 0.75) "warn" else "fail"

    metrics_data <- rbind(metrics_data, data.frame(
        Metric = "Params_SBC",
        Description = "Parameters with uniform SBC ranks",
        Target = sprintf(">=%d", round(n_params_total * 0.9)),
        Value = sprintf("%d/%d (%.0f%%)", n_params_good_sbc, n_params_total, pct_good_sbc * 100),
        Status = status_param_sbc,
        stringsAsFactors = FALSE
    ))
    metric_expressions[[length(metric_expressions) + 1]] <- expression(bold("Params (SBC)"))

    # --- Determine overall status -----------------------------------------------
    statuses <- metrics_data$Status[metrics_data$Status != "info"]
    overall_status <- if (all(statuses == "pass")) {
        "PASS"
    } else if (any(statuses == "fail")) {
        "FAIL"
    } else {
        "WARN"
    }

    if (verbose) {
        message("Overall NPE diagnostic status: ", overall_status)
        message("Successfully processed ", nrow(metrics_data), " metrics")
    }

    # --- Create plot ------------------------------------------------------------
    pdf(file.path(plots_dir, "npe_diagnostic_status.pdf"), width = 14, height = 10)

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
    overall_status_color <- status_colors[[tolower(overall_status)]]

    # Title section
    rect(0, 0.92, 1, 1, col = "white", border = NA)
    text(0.5, 0.97, "Neural Posterior Estimator (NPE) Diagnostic Status",
         cex = 1.8, font = 2, col = col_text_primary)

    # Subtitle with SBC parameters
    subtitle_text <- sprintf("Simulation-Based Calibration (SBC): Number simulations = %d, NPE samples = %d",
                            diagnostics$n_sbc_sims_successful,
                            diagnostics$n_npe_samples_per_test)
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

    # --- Main metrics table -----------------------------------------------------
    table_top <- 0.80
    table_bottom <- 0.48
    table_left <- 0.05
    table_right <- 0.95

    n_rows <- nrow(metrics_data)
    row_height <- (table_top - table_bottom) / (n_rows + 1)  # +1 for header

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
        if (i <= length(metric_expressions)) {
            text(col_x[1] + 0.01, y_center,
                 metric_expressions[[i]], cex = 1, font = 1, adj = 0)  # adj = 0 for left align
        } else {
            text(col_x[1] + 0.01, y_center,
                 metrics_data$Metric[i], cex = 1, font = 2, adj = 0)  # adj = 0 for left align
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
                # For info status, just show a dash
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

    # Note: Summary text removed as it's now in the subtitle

    # --- Parameter details table ------------------------------------------------
    # Show top problematic parameters
    problematic_params <- summary_df[!summary_df$coverage_ok | !summary_df$sbc_ok, ]
    
    if (nrow(problematic_params) > 0 && n_params_display > 0) {
        # Sort by coverage deviation + KS p-value (worst first)
        problematic_params$sort_score <- 
            abs(problematic_params$coverage_mean - diagnostics$coverage_level) + 
            (1 - problematic_params$sbc_ks_pvalue)
        problematic_params <- problematic_params[order(problematic_params$sort_score, decreasing = TRUE), ]
        
        # Limit to top N
        n_show <- min(n_params_display, nrow(problematic_params))
        params_to_show <- problematic_params[1:n_show, ]
        
        # Parameter table setup
        param_table_top <- 0.42
        param_table_bottom <- 0.12
        param_header_y <- param_table_top
        param_row_height <- (param_table_top - param_table_bottom) / (n_show + 2)  # +2 for header and title
        
        # Title for parameter section
        text(0.5, param_table_top + 0.02, "Problematic Parameters",
             cex = 1.2, font = 2, col = col_text_primary)
        
        # Parameter table header
        rect(table_left, param_header_y - param_row_height, table_right, param_header_y,
             col = col_header_bg, border = NA)
        
        param_headers <- c("Parameter", "Coverage", "Target", "SBC KS p-val", "Status")
        param_col_x <- c(table_left, 0.25, 0.45, 0.55, 0.75, table_right)
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
            coverage_text <- sprintf("%.1f%%", params_to_show$coverage_mean[i] * 100)
            text(param_col_x[2] + param_col_widths[2]/2, y_center,
                 coverage_text, cex = 0.8)
            
            # Target coverage
            text(param_col_x[3] + param_col_widths[3]/2, y_center,
                 sprintf("%.0f%%", diagnostics$coverage_level * 100),
                 cex = 0.8, col = col_text_secondary)
            
            # SBC KS p-value
            text(param_col_x[4] + param_col_widths[4]/2, y_center,
                 sprintf("%.3f", params_to_show$sbc_ks_pvalue[i]), cex = 0.8)
            
            # Combined status
            param_status <- if (params_to_show$coverage_ok[i] && params_to_show$sbc_ok[i]) {
                "pass"
            } else if (!params_to_show$coverage_ok[i] && !params_to_show$sbc_ok[i]) {
                "fail"
            } else {
                "warn"
            }
            
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
        if (nrow(problematic_params) > n_show) {
            text(0.5, param_table_bottom - 0.015,
                 sprintf("(+ %d more problematic parameters)", nrow(problematic_params) - n_show),
                 cex = 0.8, col = col_text_secondary, font = 3)
        }
    }

    # Add legend for status indicators (centered)
    legend_y <- 0.035
    legend_spacing <- 0.10
    legend_total_width <- 2 * legend_spacing + 0.08  # Approximate total width
    legend_x_start <- 0.5 - legend_total_width/2  # Center the legend

    # Pass indicator
    symbols(legend_x_start, legend_y, circles = 0.006, inches = FALSE,
           bg = status_colors$pass, fg = status_colors$pass, add = TRUE)
    text(legend_x_start + 0.012, legend_y, "P = Pass", adj = 0, cex = 0.85, col = col_text_secondary, font = 1)

    # Warn indicator
    symbols(legend_x_start + legend_spacing, legend_y, circles = 0.006, inches = FALSE,
           bg = status_colors$warn, fg = status_colors$warn, add = TRUE)
    text(legend_x_start + legend_spacing + 0.012, legend_y, "W = Warn", adj = 0, cex = 0.85, col = col_text_secondary, font = 1)

    # Fail indicator
    symbols(legend_x_start + 2*legend_spacing, legend_y, circles = 0.006, inches = FALSE,
           bg = status_colors$fail, fg = status_colors$fail, add = TRUE)
    text(legend_x_start + 2*legend_spacing + 0.012, legend_y, "F = Fail", adj = 0, cex = 0.85, col = col_text_secondary, font = 1)

    # Footer info below legend
    footer_text <- sprintf("Generated: %s",
                          diagnostics$timestamp)
    text(0.5, 0.01, footer_text, cex = 0.7, col = col_text_secondary)

    dev.off()

    if (verbose) {
        message("NPE diagnostic status plot saved to: ", 
                file.path(plots_dir, "npe_diagnostic_status.pdf"))
    }

    invisible(NULL)
}

# Helper function to prepare NPE diagnostics for status table
#' @export
prepare_npe_diagnostic_json <- function(npe_diagnostic_results,
                                       output_dir,
                                       coverage_target = 0.95,
                                       ks_threshold = 0.05,
                                       verbose = TRUE) {
    # Prepare a structured JSON file for NPE diagnostic status visualization.
    # This creates a format analogous to convergence_diagnostics.json but for NPE/SBC.
    #
    # Args:
    #     npe_diagnostic_results: Output from calc_npe_diagnostics()
    #     output_dir: Directory to save the JSON file
    #     coverage_target: Expected coverage level
    #     ks_threshold: P-value threshold for KS test
    #     verbose: Print messages
    
    # Calculate summary statistics
    n_params <- length(npe_diagnostic_results$param_names)
    n_params_good_coverage <- sum(npe_diagnostic_results$summary$coverage_ok, na.rm = TRUE)
    n_params_good_sbc <- sum(npe_diagnostic_results$summary$sbc_ok, na.rm = TRUE)
    
    # Determine overall status
    overall_coverage_ok <- abs(npe_diagnostic_results$diagnostics$overall_coverage - coverage_target) <= 0.05
    overall_sbc_ok <- npe_diagnostic_results$diagnostics$overall_sbc_ks_pvalue > ks_threshold
    params_coverage_ok <- (n_params_good_coverage / n_params) >= 0.9
    params_sbc_ok <- (n_params_good_sbc / n_params) >= 0.9
    
    overall_status <- if (overall_coverage_ok && overall_sbc_ok && params_coverage_ok && params_sbc_ok) {
        "PASS"
    } else if (!overall_coverage_ok || !overall_sbc_ok || 
               (n_params_good_coverage / n_params) < 0.75 || 
               (n_params_good_sbc / n_params) < 0.75) {
        "FAIL"
    } else {
        "WARN"
    }
    
    # Create structured diagnostics object
    structured_diagnostics <- list(
        settings = list(
            n_sbc_sims = npe_diagnostic_results$diagnostics$n_sbc_sims_requested,
            n_npe_samples_per_test = npe_diagnostic_results$diagnostics$n_npe_samples_per_test,
            coverage_level = npe_diagnostic_results$diagnostics$coverage_level,
            quantiles_used = npe_diagnostic_results$diagnostics$quantiles_used,
            npe_model_dir = npe_diagnostic_results$diagnostics$npe_model_dir,
            description = "Simulation-Based Calibration (SBC) diagnostics for NPE model"
        ),
        targets = list(
            coverage_target = list(
                value = coverage_target,
                description = "Target coverage probability"
            ),
            ks_threshold = list(
                value = ks_threshold,
                description = "KS test p-value threshold for SBC uniformity"
            ),
            params_coverage_min = list(
                value = 0.9,
                description = "Minimum fraction of parameters with good coverage"
            ),
            params_sbc_min = list(
                value = 0.9,
                description = "Minimum fraction of parameters with uniform SBC ranks"
            )
        ),
        metrics = list(
            n_successful = list(
                value = npe_diagnostic_results$diagnostics$n_sbc_sims_successful,
                target = npe_diagnostic_results$diagnostics$n_sbc_sims_requested,
                description = "Number of successful SBC simulations",
                status = if (npe_diagnostic_results$diagnostics$n_sbc_sims_successful == 
                           npe_diagnostic_results$diagnostics$n_sbc_sims_requested) "pass" 
                        else if (npe_diagnostic_results$diagnostics$n_sbc_sims_successful >= 
                                npe_diagnostic_results$diagnostics$n_sbc_sims_requested * 0.95) "warn" 
                        else "fail"
            ),
            overall_coverage = list(
                value = npe_diagnostic_results$diagnostics$overall_coverage,
                se = npe_diagnostic_results$diagnostics$overall_coverage_se,
                target = coverage_target,
                description = "Overall parameter coverage across all SBC tests",
                status = if (overall_coverage_ok) "pass" 
                        else if (abs(npe_diagnostic_results$diagnostics$overall_coverage - coverage_target) <= 0.10) "warn" 
                        else "fail"
            ),
            overall_sbc_ks = list(
                value = npe_diagnostic_results$diagnostics$overall_sbc_ks_pvalue,
                statistic = npe_diagnostic_results$diagnostics$overall_sbc_ks_statistic,
                target = ks_threshold,
                description = "Kolmogorov-Smirnov test for SBC rank uniformity",
                status = if (overall_sbc_ok) "pass" 
                        else if (npe_diagnostic_results$diagnostics$overall_sbc_ks_pvalue > ks_threshold/2) "warn" 
                        else "fail"
            ),
            params_good_coverage = list(
                value = n_params_good_coverage,
                total = n_params,
                fraction = n_params_good_coverage / n_params,
                description = "Parameters with coverage within tolerance",
                status = if (params_coverage_ok) "pass" 
                        else if ((n_params_good_coverage / n_params) >= 0.75) "warn" 
                        else "fail"
            ),
            params_good_sbc = list(
                value = n_params_good_sbc,
                total = n_params,
                fraction = n_params_good_sbc / n_params,
                description = "Parameters with uniform SBC ranks (KS p > threshold)",
                status = if (params_sbc_ok) "pass" 
                        else if ((n_params_good_sbc / n_params) >= 0.75) "warn" 
                        else "fail"
            )
        ),
        summary = list(
            diagnostic_status = overall_status,
            elapsed_time_minutes = npe_diagnostic_results$diagnostics$elapsed_time_minutes,
            timestamp = npe_diagnostic_results$diagnostics$timestamp,
            n_parameters = n_params,
            problematic_parameters = npe_diagnostic_results$summary$parameter[
                !npe_diagnostic_results$summary$coverage_ok | 
                !npe_diagnostic_results$summary$sbc_ok
            ]
        )
    )
    
    # Save to file
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    
    output_file <- file.path(output_dir, "npe_diagnostic_status.json")
    jsonlite::write_json(structured_diagnostics, output_file, 
                        pretty = TRUE, auto_unbox = TRUE)
    
    if (verbose) {
        message("NPE diagnostic status JSON saved to: ", output_file)
    }
    
    return(structured_diagnostics)
}