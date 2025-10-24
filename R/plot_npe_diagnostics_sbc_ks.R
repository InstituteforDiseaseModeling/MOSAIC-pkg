#' Plot NPE diagnostics SBC KS test results as horizontal bar plots
#'
#' Creates horizontal bar plots showing SBC Kolmogorov-Smirnov test p-values
#' for each parameter, color-coded by significance level and grouped by parameter type.
#'
#' @param diagnostics_dir Path to NPE diagnostics directory containing SBC results
#'   (expects "sbc_summary.csv")
#' @param plots_dir Path to plots directory (default: creates "plots" in parent of diagnostics_dir)
#' @param max_params_per_page Maximum number of parameters per page (default: 40)
#' @param verbose Logical indicating whether to print messages
#'
#' @return Invisible NULL. Creates a PDF file with SBC KS test bar plots.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create NPE SBC KS bar plots
#' plot_npe_diagnostics_sbc_ks(
#'   diagnostics_dir = "./local/calibration/calibration_test_13/npe/diagnostics"
#' )
#' }
plot_npe_diagnostics_sbc_ks <- function(diagnostics_dir,
                                        plots_dir = NULL,
                                        max_params_per_page = 40,
                                        verbose = TRUE) {

    # --- I/O checks -------------------------------------------------------------
    if (!dir.exists(diagnostics_dir)) stop("diagnostics_dir does not exist: ", diagnostics_dir)
    if (is.null(plots_dir)) plots_dir <- file.path(dirname(dirname(diagnostics_dir)), "plots")
    if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE)
        if (verbose) message("Created plots directory: ", plots_dir)
    }

    # Check for required file
    summary_file <- file.path(diagnostics_dir, "sbc_summary.csv")

    if (!file.exists(summary_file)) {
        stop("SBC summary file not found: ", summary_file)
    }

    if (verbose) {
        message("Reading NPE SBC KS test data from: ", diagnostics_dir)
    }

    # --- Read data --------------------------------------------------------------
    summary_df <- read.csv(summary_file, stringsAsFactors = FALSE)

    # Get parameter names and KS p-values
    param_names <- summary_df$parameter
    ks_pvalues <- summary_df$sbc_ks_pvalue

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

    # --- Create plots -----------------------------------------------------------
    pdf(file.path(plots_dir, "npe_diagnostics_sbc_ks.pdf"), width = 12, height = 10)

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
        param_indices <- match(params, summary_df$parameter)
        param_ks_pvalues <- ks_pvalues[param_indices]

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
            param_ks_pvalues <- param_ks_pvalues[ordered_indices]
        }

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
        message("NPE SBC KS test plots saved to: ",
                file.path(plots_dir, "npe_diagnostics_sbc_ks.pdf"))
    }

    invisible(NULL)
}