#' Plot NPE diagnostics coverage histograms
#'
#' Creates histograms showing the distribution of coverage values across parameters,
#' split by global parameters and location-specific parameters for each location.
#'
#' @param diagnostics_dir Path to NPE diagnostics directory containing SBC results
#'   (expects "sbc_coverage.csv" and "sbc_summary.csv")
#' @param plots_dir Path to plots directory (default: creates "plots" in parent of diagnostics_dir)
#' @param coverage_levels Numeric vector of coverage levels to plot (default: c(0.5, 0.95))
#' @param verbose Logical indicating whether to print messages
#'
#' @return Invisible NULL. Creates a PDF file with coverage histograms.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create NPE coverage histograms
#' plot_npe_diagnostics_coverage(
#'   diagnostics_dir = "./local/calibration/calibration_test_13/npe/diagnostics"
#' )
#' }
plot_npe_diagnostics_coverage <- function(diagnostics_dir,
                                         plots_dir = NULL,
                                         coverage_levels = c(0.5, 0.95),
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
    # Number of pages = 1 (global) + number of locations
    n_pages <- 1 + length(location_codes)

    pdf(file.path(plots_dir, "npe_diagnostics_coverage.pdf"), width = 12, height = 6)

    # Color scheme
    col_primary <- "#2C3E50"
    col_secondary <- "#7F8C8D"
    col_pass <- "#27AE60"
    col_warn <- "#F39C12"
    col_fail <- "#E74C3C"
    col_50 <- "#3498DB"
    col_95 <- "#E74C3C"

    # --- Page 1: Global parameters ---
    if (length(global_params) > 0) {
        # Calculate coverage for both levels
        global_coverage_50 <- colMeans(coverage_results[["level_0.5"]][, global_params, drop = FALSE], na.rm = TRUE)
        global_coverage_95 <- colMeans(coverage_results[["level_0.95"]][, global_params, drop = FALSE], na.rm = TRUE)

        # Set up 2-panel layout
        par(mfrow = c(1, 2),
            mar = c(4, 4, 3, 2),
            mgp = c(2.5, 0.7, 0),
            oma = c(2, 2, 3, 2))

        # Left panel: 50% CI
        hist(global_coverage_50,
             breaks = seq(0, 1, by = 0.05),
             main = "50% CI Coverage",
             xlab = "Coverage",
             ylab = "Number of Parameters",
             col = adjustcolor(col_50, alpha = 0.6),
             border = col_50,
             xlim = c(0, 1),
             las = 1)

        # Add target line and tolerance bands
        abline(v = 0.5, col = col_50, lwd = 3, lty = 2)
        rect(0.45, -10, 0.55, 100, col = adjustcolor(col_pass, alpha = 0.1), border = NA)
        rect(0.4, -10, 0.6, 100, col = adjustcolor(col_warn, alpha = 0.1), border = NA)

        # Add summary statistics
        mean_50 <- mean(global_coverage_50, na.rm = TRUE)
        text(0.95, par("usr")[4] * 0.95,
             sprintf("Mean: %.3f", mean_50),
             pos = 2, cex = 1.0)
        text(0.95, par("usr")[4] * 0.88,
             sprintf("Target: 0.500", mean_50),
             pos = 2, cex = 1.0, col = col_secondary)
        text(0.95, par("usr")[4] * 0.81,
             sprintf("N = %d", length(global_params)),
             pos = 2, cex = 1.0, col = col_secondary)

        # Right panel: 95% CI
        hist(global_coverage_95,
             breaks = seq(0, 1, by = 0.05),
             main = "95% CI Coverage",
             xlab = "Coverage",
             ylab = "Number of Parameters",
             col = adjustcolor(col_95, alpha = 0.6),
             border = col_95,
             xlim = c(0, 1),
             las = 1)

        # Add target line and tolerance bands
        abline(v = 0.95, col = col_95, lwd = 3, lty = 2)
        rect(0.90, -10, 1.0, 100, col = adjustcolor(col_pass, alpha = 0.1), border = NA)
        rect(0.85, -10, 1.05, 100, col = adjustcolor(col_warn, alpha = 0.1), border = NA)

        # Add summary statistics
        mean_95 <- mean(global_coverage_95, na.rm = TRUE)
        text(0.05, par("usr")[4] * 0.95,
             sprintf("Mean: %.3f", mean_95),
             pos = 4, cex = 1.0)
        text(0.05, par("usr")[4] * 0.88,
             sprintf("Target: 0.950", mean_95),
             pos = 4, cex = 1.0, col = col_secondary)
        text(0.05, par("usr")[4] * 0.81,
             sprintf("N = %d", length(global_params)),
             pos = 4, cex = 1.0, col = col_secondary)

        # Add overall title
        mtext("Global Parameters - Coverage Diagnostics",
              outer = TRUE, cex = 1.4, font = 2, line = 1)

        # Add legend
        mtext("Green shading: ±5% tolerance, Yellow shading: ±10% tolerance",
              outer = TRUE, side = 1, cex = 0.8, col = col_secondary, line = 0.5)

    } else {
        plot.new()
        text(0.5, 0.5, "No global parameters", cex = 1.2, col = col_secondary)
    }

    # --- Additional pages: Location-specific parameters ---
    for (loc in location_codes) {
        loc_params <- grep(paste0("_", loc, "$"), param_names, value = TRUE)

        if (length(loc_params) > 0) {
            # Calculate coverage for both levels
            loc_coverage_50 <- colMeans(coverage_results[["level_0.5"]][, loc_params, drop = FALSE], na.rm = TRUE)
            loc_coverage_95 <- colMeans(coverage_results[["level_0.95"]][, loc_params, drop = FALSE], na.rm = TRUE)

            # Set up 2-panel layout
            par(mfrow = c(1, 2),
                mar = c(4, 4, 3, 2),
                mgp = c(2.5, 0.7, 0),
                oma = c(2, 2, 3, 2))

            # Left panel: 50% CI
            hist(loc_coverage_50,
                 breaks = seq(0, 1, by = 0.05),
                 main = "50% CI Coverage",
                 xlab = "Coverage",
                 ylab = "Number of Parameters",
                 col = adjustcolor(col_50, alpha = 0.6),
                 border = col_50,
                 xlim = c(0, 1),
                 las = 1)

            # Add target line and tolerance bands
            abline(v = 0.5, col = col_50, lwd = 3, lty = 2)
            rect(0.45, -10, 0.55, 100, col = adjustcolor(col_pass, alpha = 0.1), border = NA)
            rect(0.4, -10, 0.6, 100, col = adjustcolor(col_warn, alpha = 0.1), border = NA)

            # Add summary statistics
            mean_50 <- mean(loc_coverage_50, na.rm = TRUE)
            text(0.95, par("usr")[4] * 0.95,
                 sprintf("Mean: %.3f", mean_50),
                 pos = 2, cex = 1.0)
            text(0.95, par("usr")[4] * 0.88,
                 sprintf("Target: 0.500", mean_50),
                 pos = 2, cex = 1.0, col = col_secondary)
            text(0.95, par("usr")[4] * 0.81,
                 sprintf("N = %d", length(loc_params)),
                 pos = 2, cex = 1.0, col = col_secondary)

            # Right panel: 95% CI
            hist(loc_coverage_95,
                 breaks = seq(0, 1, by = 0.05),
                 main = "95% CI Coverage",
                 xlab = "Coverage",
                 ylab = "Number of Parameters",
                 col = adjustcolor(col_95, alpha = 0.6),
                 border = col_95,
                 xlim = c(0, 1),
                 las = 1)

            # Add target line and tolerance bands
            abline(v = 0.95, col = col_95, lwd = 3, lty = 2)
            rect(0.90, -10, 1.0, 100, col = adjustcolor(col_pass, alpha = 0.1), border = NA)
            rect(0.85, -10, 1.05, 100, col = adjustcolor(col_warn, alpha = 0.1), border = NA)

            # Add summary statistics
            mean_95 <- mean(loc_coverage_95, na.rm = TRUE)
            text(0.05, par("usr")[4] * 0.95,
                 sprintf("Mean: %.3f", mean_95),
                 pos = 4, cex = 1.0)
            text(0.05, par("usr")[4] * 0.88,
                 sprintf("Target: 0.950", mean_95),
                 pos = 4, cex = 1.0, col = col_secondary)
            text(0.05, par("usr")[4] * 0.81,
                 sprintf("N = %d", length(loc_params)),
                 pos = 4, cex = 1.0, col = col_secondary)

            # Add overall title
            mtext(paste0("Location: ", loc, " - Coverage Diagnostics"),
                  outer = TRUE, cex = 1.4, font = 2, line = 1)

            # Add legend
            mtext("Green shading: ±5% tolerance, Yellow shading: ±10% tolerance",
                  outer = TRUE, side = 1, cex = 0.8, col = col_secondary, line = 0.5)
        }
    }

    dev.off()

    if (verbose) {
        message("NPE coverage diagnostic plots saved to: ",
                file.path(plots_dir, "npe_diagnostics_coverage.pdf"))
    }

    invisible(NULL)
}