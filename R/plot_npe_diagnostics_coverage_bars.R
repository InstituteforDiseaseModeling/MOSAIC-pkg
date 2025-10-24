#' Plot NPE diagnostics coverage as horizontal bar plots
#'
#' Creates horizontal bar plots showing coverage values for each parameter,
#' color-coded by deviation from target and grouped by parameter type.
#'
#' @param diagnostics_dir Path to NPE diagnostics directory containing SBC results
#'   (expects "sbc_coverage.csv" and "sbc_summary.csv")
#' @param plots_dir Path to plots directory (default: creates "plots" in parent of diagnostics_dir)
#' @param coverage_levels Numeric vector of coverage levels to plot (default: c(0.5, 0.95))
#' @param max_params_per_page Maximum number of parameters per page (default: 40)
#' @param verbose Logical indicating whether to print messages
#'
#' @return Invisible NULL. Creates a PDF file with coverage bar plots.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create NPE coverage bar plots
#' plot_npe_diagnostics_coverage_bars(
#'   diagnostics_dir = "./local/calibration/calibration_test_13/npe/diagnostics"
#' )
#' }
plot_npe_diagnostics_coverage_bars <- function(diagnostics_dir,
                                               plots_dir = NULL,
                                               coverage_levels = c(0.5, 0.95),
                                               max_params_per_page = 40,
                                               verbose = TRUE) {

    # --- I/O checks -------------------------------------------------------------
    if (!dir.exists(diagnostics_dir)) stop("diagnostics_dir does not exist: ", diagnostics_dir)
    if (is.null(plots_dir)) plots_dir <- file.path(dirname(dirname(diagnostics_dir)), "plots")
    if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE)
        if (verbose) message("Created plots directory: ", plots_dir)
    }

    # Check for required files
    coverage_file <- file.path(diagnostics_dir, "sbc_coverage.csv")
    summary_file <- file.path(diagnostics_dir, "sbc_summary.csv")
    ranks_file <- file.path(diagnostics_dir, "sbc_ranks.csv")

    if (!file.exists(coverage_file)) {
        stop("SBC coverage file not found: ", coverage_file)
    }
    if (!file.exists(summary_file)) {
        stop("SBC summary file not found: ", summary_file)
    }
    if (!file.exists(ranks_file)) {
        stop("SBC ranks file not found: ", ranks_file)
    }

    if (verbose) {
        message("Reading NPE coverage data from: ", diagnostics_dir)
    }

    # --- Read data --------------------------------------------------------------
    coverage_matrix <- read.csv(coverage_file)
    summary_df <- read.csv(summary_file, stringsAsFactors = FALSE)
    ranks_matrix <- read.csv(ranks_file)

    # Get parameter names
    param_names <- names(coverage_matrix)

    # --- Get MOSAIC estimated parameters order ---------------------------------
    # Try to load the estimated_parameters object for ordering
    estimated_params_order <- tryCatch({
        # Load MOSAIC package and get the actual parameter order
        if (requireNamespace("MOSAIC", quietly = TRUE)) {
            # Extract parameter names from MOSAIC::estimated_parameters data frame
            MOSAIC::estimated_parameters$parameter_name
        } else {
            # Fallback to correct canonical order if MOSAIC not available
            c("alpha_1", "alpha_2",
              "decay_days_short", "decay_days_long", "decay_shape_1", "decay_shape_2",
              "zeta_1", "zeta_2", "kappa",
              "iota", "sigma", "gamma_1", "gamma_2",
              "phi_1", "phi_2", "omega_1", "omega_2", "epsilon",
              "delta_reporting", "rho",
              "mobility_gamma", "mobility_omega",
              "prop_S_initial", "prop_E_initial", "prop_I_initial",
              "prop_R_initial", "prop_V1_initial", "prop_V2_initial",
              "beta_j0_tot", "p_beta", "beta_j0_hum", "beta_j0_env",
              "a_1_j", "a_2_j", "b_1_j", "b_2_j",
              "psi_star_a", "psi_star_b", "psi_star_z", "psi_star_k",
              "mu_j", "tau_i", "theta_j")
        }
    }, error = function(e) {
        # Final fallback to correct canonical order
        c("alpha_1", "alpha_2",
          "decay_days_short", "decay_days_long", "decay_shape_1", "decay_shape_2",
          "zeta_1", "zeta_2", "kappa",
          "iota", "sigma", "gamma_1", "gamma_2",
          "phi_1", "phi_2", "omega_1", "omega_2", "epsilon",
          "delta_reporting", "rho",
          "mobility_gamma", "mobility_omega",
          "prop_S_initial", "prop_E_initial", "prop_I_initial",
          "prop_R_initial", "prop_V1_initial", "prop_V2_initial",
          "beta_j0_tot", "p_beta", "beta_j0_hum", "beta_j0_env",
          "a_1_j", "a_2_j", "b_1_j", "b_2_j",
          "psi_star_a", "psi_star_b", "psi_star_z", "psi_star_k",
          "mu_j", "tau_i", "theta_j")
    })

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

    # Order global parameters according to MOSAIC estimated_parameters
    if (!is.null(estimated_params_order)) {
        # Match parameters to the canonical order
        global_params_ordered <- character(0)
        for (param in estimated_params_order) {
            if (param %in% global_params) {
                global_params_ordered <- c(global_params_ordered, param)
            }
        }
        # Add any remaining parameters not in the canonical list
        remaining <- setdiff(global_params, global_params_ordered)
        global_params <- c(global_params_ordered, remaining)
    }

    if (verbose) {
        message("Found ", length(global_params), " global parameters")
        message("Found ", length(location_params), " location-specific parameters")
        if (length(location_codes) > 0) {
            message("Locations: ", paste(location_codes, collapse = ", "))
        }
    }

    # --- Calculate coverage at different levels --------------------------------
    coverage_results <- list()

    for (level in coverage_levels) {
        if (level == 0.95) {
            # Use the existing 95% coverage from file
            coverage_results[[paste0("level_", level)]] <- coverage_matrix
        } else if (level == 0.5) {
            # Calculate 50% coverage from ranks
            coverage_50_matrix <- (ranks_matrix >= 0.25 & ranks_matrix <= 0.75)
            # Convert to data frame with same structure
            coverage_50_df <- as.data.frame(coverage_50_matrix)
            names(coverage_50_df) <- names(coverage_matrix)
            coverage_results[[paste0("level_", level)]] <- coverage_50_df
        } else {
            # Calculate custom coverage level from ranks
            lower_q <- (1 - level) / 2
            upper_q <- 1 - lower_q
            coverage_custom_matrix <- (ranks_matrix >= lower_q & ranks_matrix <= upper_q)
            coverage_custom_df <- as.data.frame(coverage_custom_matrix)
            names(coverage_custom_df) <- names(coverage_matrix)
            coverage_results[[paste0("level_", level)]] <- coverage_custom_df
        }
    }

    # --- Create plots -----------------------------------------------------------
    pdf(file.path(plots_dir, "npe_diagnostics_coverage_bars.pdf"), width = 12, height = 10)

    # Professional color scheme (matching status PDFs)
    col_header_bg <- "#2C3E50"
    col_header_text <- "white"
    col_row_even <- "#F8F9FA"
    col_row_odd <- "white"
    col_border <- "#DEE2E6"
    col_text_primary <- "#212529"
    col_text_secondary <- "#6C757D"

    # Status colors (professional palette from status PDFs)
    col_pass <- "#28A745"
    col_warn <- "#FFC107"
    col_fail <- "#DC3545"
    col_info <- "#17A2B8"
    col_grid <- "#E5E5E5"

    # Function to create bar plot for a set of parameters
    create_coverage_barplot <- function(params, param_group_name, level, coverage_data) {
        if (length(params) == 0) return()

        # Calculate mean coverage for each parameter
        param_coverage <- colMeans(coverage_data[, params, drop = FALSE], na.rm = TRUE)

        # For location-specific parameters, order them by the canonical order
        if (grepl("Location:", param_group_name)) {
            # Extract base parameter names (without location suffix)
            loc_code <- gsub("Location: ", "", param_group_name)
            base_names <- gsub(paste0("_", loc_code, "$"), "", params)

            # Create ordering based on canonical parameter order
            ordered_indices <- integer(0)
            for (base_param in estimated_params_order) {
                matching_idx <- which(base_names == base_param)
                if (length(matching_idx) > 0) {
                    ordered_indices <- c(ordered_indices, matching_idx)
                }
            }
            # Add any remaining parameters
            remaining_idx <- setdiff(seq_along(params), ordered_indices)
            ordered_indices <- c(ordered_indices, remaining_idx)

            params <- params[ordered_indices]
            param_coverage <- param_coverage[ordered_indices]
        }

        # Determine colors based on deviation from target
        param_colors <- ifelse(abs(param_coverage - level) <= 0.05, col_pass,
                              ifelse(abs(param_coverage - level) <= 0.10, col_warn, col_fail))

        # Split into pages if needed
        n_params <- length(params)
        n_pages <- ceiling(n_params / max_params_per_page)

        for (page in 1:n_pages) {
            # Determine parameters for this page
            start_idx <- (page - 1) * max_params_per_page + 1
            end_idx <- min(page * max_params_per_page, n_params)
            page_params <- params[start_idx:end_idx]
            page_coverage <- param_coverage[start_idx:end_idx]
            page_colors <- param_colors[start_idx:end_idx]

            # Reverse order for horizontal barplot (top to bottom)
            page_params_rev <- rev(page_params)
            page_coverage_rev <- rev(page_coverage)
            page_colors_rev <- rev(page_colors)

            # Set up plot with margins adjusted for legend at bottom and subtitle spacing
            par(mar = c(8, 10, 6, 3), mgp = c(2.5, 0.7, 0), xpd = FALSE)

            # Calculate proper y-axis range for barplot
            n_bars <- length(page_params)
            bar_width <- 1  # Standard bar width
            bar_space <- 0.2  # Space between bars
            total_height <- n_bars * bar_width + (n_bars - 1) * bar_space

            # First create empty plot to add background elements - tighter y-axis
            plot(NULL, xlim = c(0, 1.05), ylim = c(0, total_height),
                 xlab = "Coverage", ylab = "",
                 main = sprintf("%s - %.0f%% CI Coverage%s",
                              param_group_name, level * 100,
                              ifelse(n_pages > 1, sprintf(" (Page %d/%d)", page, n_pages), "")),
                 cex.main = 1.2, col.main = col_text_primary,
                 axes = FALSE,
                 yaxs = "i")  # Suppress y-axis expansion

            # Add x-axis only - y-axis will be added after barplot
            axis(1)

            # Add grid lines first (in background)
            abline(v = seq(0, 1, by = 0.1), col = col_grid, lty = 3, lwd = 0.5)

            # Add tolerance zones (shaded areas) using grey colors
            # Get plot region limits
            usr <- par("usr")

            # Ensure shaded areas don't extend past 0 or 1 horizontally, but use full plot height with extension
            rect(max(0, level - 0.05), usr[3] - 0.5, min(1, level + 0.05), usr[4] + 0.5,
                 col = adjustcolor(col_text_secondary, alpha = 0.15), border = NA)
            rect(max(0, level - 0.10), usr[3] - 0.5, max(0, level - 0.05), usr[4] + 0.5,
                 col = adjustcolor(col_text_secondary, alpha = 0.10), border = NA)
            rect(min(1, level + 0.05), usr[3] - 0.5, min(1, level + 0.10), usr[4] + 0.5,
                 col = adjustcolor(col_text_secondary, alpha = 0.10), border = NA)

            # Now add the bars with proper positioning
            bp <- barplot(page_coverage_rev,
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

            # Add target line on top
            abline(v = level, col = col_text_primary, lwd = 3, lty = 2)

            # Add value labels to the right of bars
            par(xpd = TRUE)  # Allow text outside plot area
            for (i in seq_along(page_coverage_rev)) {
                # Always position text to the right of the bar (reduced offset)
                text(page_coverage_rev[i] + 0.005, bp[i],
                     labels = sprintf("%.3f", page_coverage_rev[i]),
                     pos = 4, cex = 0.6)
            }
            par(xpd = FALSE)  # Reset

            # Add summary statistics
            mean_coverage <- mean(page_coverage, na.rm = TRUE)
            n_pass <- sum(abs(page_coverage - level) <= 0.05, na.rm = TRUE)
            n_warn <- sum(abs(page_coverage - level) > 0.05 & abs(page_coverage - level) <= 0.10, na.rm = TRUE)
            n_fail <- sum(abs(page_coverage - level) > 0.10, na.rm = TRUE)

            # Add status summary at top with more spacing
            mtext(sprintf("Mean: %.3f | Target: %.2f | Pass: %d | Warn: %d | Fail: %d",
                         mean_coverage, level, n_pass, n_warn, n_fail),
                  side = 3, line = 1.0, cex = 0.9, col = col_text_secondary)

            # Add legend at bottom of plot with more spacing from x-axis label
            par(xpd = TRUE)  # Allow drawing outside plot area
            legend(x = 0.5, y = usr[3] - 3.2,
                   xjust = 0.5,
                   legend = c(
                       "Pass (within ±5%)",
                       "Warn (within ±10%)",
                       "Fail (outside ±10%)"
                   ),
                   fill = c(col_pass, col_warn, col_fail),
                   border = NA,
                   bty = "n",
                   cex = 0.8,
                   ncol = 3,  # Horizontal layout
                   horiz = FALSE)
            par(xpd = FALSE)  # Reset to default
        }
    }

    # --- Create plots in sequence: global 50%, global 95%, location1 50%, location1 95%, ... ---

    # Global parameters - 50% CI
    if (length(global_params) > 0 && 0.5 %in% coverage_levels) {
        create_coverage_barplot(global_params, "Global Parameters", 0.5,
                              coverage_results[["level_0.5"]])
    }

    # Global parameters - 95% CI
    if (length(global_params) > 0 && 0.95 %in% coverage_levels) {
        create_coverage_barplot(global_params, "Global Parameters", 0.95,
                              coverage_results[["level_0.95"]])
    }

    # Location-specific parameters
    for (loc in location_codes) {
        loc_params <- grep(paste0("_", loc, "$"), param_names, value = TRUE)

        if (length(loc_params) > 0) {
            # 50% CI for this location
            if (0.5 %in% coverage_levels) {
                create_coverage_barplot(loc_params, paste0("Location: ", loc), 0.5,
                                      coverage_results[["level_0.5"]])
            }

            # 95% CI for this location
            if (0.95 %in% coverage_levels) {
                create_coverage_barplot(loc_params, paste0("Location: ", loc), 0.95,
                                      coverage_results[["level_0.95"]])
            }
        }
    }

    dev.off()

    if (verbose) {
        message("NPE coverage bar plots saved to: ",
                file.path(plots_dir, "npe_diagnostics_coverage_bars.pdf"))
    }

    invisible(NULL)
}