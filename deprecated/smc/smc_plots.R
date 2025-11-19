# ==============================================================================
# SMC Plotting Functions
# ==============================================================================
# Visualization functions for SMC exact posterior diagnostics and comparisons
# ==============================================================================

#' Plot SMC Weight Diagnostics
#'
#' @description
#' Creates diagnostic plots for SMC importance weights distribution.
#'
#' @param smc_result SMC result object from compute_smc_posterior
#' @param output_dir Directory to save plots
#' @param verbose Logical, print progress
#'
#' @return Invisible NULL
#' @export
plot_smc_weight_diagnostics <- function(smc_result, output_dir, verbose = FALSE) {

    if (verbose) message("Creating SMC weight diagnostic plots...")

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    weights <- smc_result$weights$weights
    log_weights <- smc_result$weights$log_weights

    # Create multi-panel plot
    pdf(file.path(output_dir, "smc_weight_diagnostics.pdf"), width = 12, height = 8)
    par(mfrow = c(2, 3))

    # 1. Linear weight distribution
    hist(weights, breaks = 50, main = "Weight Distribution (Linear)",
         xlab = "Weight", col = "steelblue", border = "white")
    abline(v = mean(weights), col = "red", lwd = 2, lty = 2)

    # 2. Log weight distribution
    hist(log_weights[is.finite(log_weights)], breaks = 50,
         main = "Weight Distribution (Log)",
         xlab = "Log Weight", col = "steelblue", border = "white")

    # 3. Sorted weights
    plot(sort(weights, decreasing = TRUE), type = "l",
         main = "Sorted Weights (Linear)",
         xlab = "Rank", ylab = "Weight", col = "steelblue", lwd = 2)
    grid()

    # 4. Cumulative weight contribution
    cum_weights <- cumsum(sort(weights, decreasing = TRUE))
    plot(cum_weights, type = "l", main = "Cumulative Weight Distribution",
         xlab = "Rank", ylab = "Cumulative Weight", col = "steelblue", lwd = 2)
    abline(h = 0.5, col = "red", lty = 2)
    abline(h = 0.95, col = "red", lty = 2)
    grid()

    # 5. ESS tracking
    ess_seq <- sapply(10:length(weights), function(n) {
        w <- weights[1:n]
        1 / sum((w / sum(w))^2)
    })
    plot(10:length(weights), ess_seq, type = "l",
         main = sprintf("ESS Evolution (Final: %.0f)", smc_result$weights$ess),
         xlab = "Number of Samples", ylab = "ESS", col = "steelblue", lwd = 2)
    grid()

    # 6. Summary statistics text
    plot.new()
    text(0.5, 0.8, "SMC Weight Summary", cex = 1.5, font = 2)
    text(0.1, 0.6, sprintf("ESS: %.1f (%.1f%%)",
                          smc_result$weights$ess,
                          100 * smc_result$weights$ess / length(weights)),
         adj = 0, cex = 1.2)
    text(0.1, 0.5, sprintf("Max weight: %.6f", max(weights)), adj = 0, cex = 1.2)
    text(0.1, 0.4, sprintf("Min weight: %.6f", min(weights)), adj = 0, cex = 1.2)
    text(0.1, 0.3, sprintf("Mean weight: %.6f", mean(weights)), adj = 0, cex = 1.2)
    text(0.1, 0.2, sprintf("Samples: %d", length(weights)), adj = 0, cex = 1.2)

    dev.off()

    if (verbose) message("  Saved: smc_weight_diagnostics.pdf")

    return(invisible(NULL))
}


#' Plot NPE vs SMC Posterior Comparison
#'
#' @description
#' Creates comparison plots showing how SMC correction affects posteriors.
#'
#' @param npe_samples Matrix of NPE posterior samples
#' @param smc_samples Matrix of SMC posterior samples
#' @param output_dir Directory to save plots
#' @param max_params_per_page Maximum parameters per PDF page
#' @param verbose Logical, print progress
#'
#' @return Invisible NULL
#' @export
plot_npe_vs_smc_comparison <- function(
    npe_samples,
    smc_samples,
    output_dir,
    max_params_per_page = 40,
    verbose = FALSE
) {

    if (verbose) message("Creating NPE vs SMC comparison plots...")

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    param_names <- colnames(npe_samples)
    if (is.null(param_names)) param_names <- paste0("param_", 1:ncol(npe_samples))

    n_params <- length(param_names)
    n_pages <- ceiling(n_params / max_params_per_page)

    for (page in 1:n_pages) {

        start_idx <- (page - 1) * max_params_per_page + 1
        end_idx <- min(page * max_params_per_page, n_params)
        page_params <- param_names[start_idx:end_idx]

        pdf_file <- if (n_pages == 1) {
            file.path(output_dir, "npe_vs_smc_comparison.pdf")
        } else {
            file.path(output_dir, sprintf("npe_vs_smc_comparison_%d.pdf", page))
        }

        pdf(pdf_file, width = 12, height = 10)

        # Calculate layout
        n_params_page <- length(page_params)
        n_cols <- min(4, n_params_page)
        n_rows <- ceiling(n_params_page / n_cols)
        par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 2, 1))

        for (param in page_params) {

            if (!(param %in% colnames(npe_samples)) ||
                !(param %in% colnames(smc_samples))) {
                next
            }

            npe_vals <- npe_samples[, param]
            smc_vals <- smc_samples[, param]

            # Determine shared x-axis range
            x_range <- range(c(npe_vals, smc_vals), na.rm = TRUE)

            # Plot densities
            npe_dens <- density(npe_vals, na.rm = TRUE)
            smc_dens <- density(smc_vals, na.rm = TRUE)

            y_max <- max(c(npe_dens$y, smc_dens$y))

            plot(npe_dens, col = "steelblue", lwd = 2, main = param,
                 xlim = x_range, ylim = c(0, y_max * 1.1),
                 xlab = "", ylab = "")
            lines(smc_dens, col = "darkred", lwd = 2)

            # Add legend on first plot
            if (param == page_params[1]) {
                legend("topright", legend = c("NPE", "SMC"),
                      col = c("steelblue", "darkred"), lwd = 2, cex = 0.8)
            }
        }

        dev.off()

        if (verbose) message("  Saved: ", basename(pdf_file))
    }

    # Create summary statistics comparison
    summary_file <- file.path(output_dir, "npe_vs_smc_summary.csv")

    summary_df <- data.frame(
        parameter = param_names,
        npe_mean = apply(npe_samples[, param_names], 2, mean, na.rm = TRUE),
        npe_sd = apply(npe_samples[, param_names], 2, sd, na.rm = TRUE),
        npe_q025 = apply(npe_samples[, param_names], 2, quantile, 0.025, na.rm = TRUE),
        npe_q975 = apply(npe_samples[, param_names], 2, quantile, 0.975, na.rm = TRUE),
        smc_mean = apply(smc_samples[, param_names], 2, mean, na.rm = TRUE),
        smc_sd = apply(smc_samples[, param_names], 2, sd, na.rm = TRUE),
        smc_q025 = apply(smc_samples[, param_names], 2, quantile, 0.025, na.rm = TRUE),
        smc_q975 = apply(smc_samples[, param_names], 2, quantile, 0.975, na.rm = TRUE),
        stringsAsFactors = FALSE
    )

    # Calculate difference metrics
    summary_df$mean_shift <- summary_df$smc_mean - summary_df$npe_mean
    summary_df$mean_shift_pct <- 100 * summary_df$mean_shift / abs(summary_df$npe_mean)

    write.csv(summary_df, summary_file, row.names = FALSE)

    if (verbose) message("  Saved: npe_vs_smc_summary.csv")

    return(invisible(NULL))
}


#' Plot SMC Empirical Posterior Distributions (No Smoothing)
#'
#' @description
#' Creates empirical distribution plots for SMC posterior samples without
#' kernel density smoothing. This reveals when ESS is low and samples are
#' collapsed to a single point.
#'
#' @param smc_samples Matrix of SMC posterior samples
#' @param output_dir Directory to save plots
#' @param max_params_per_page Maximum parameters per PDF page
#' @param verbose Logical, print progress
#'
#' @return Invisible NULL
#' @export
plot_smc_empirical_distributions <- function(
    smc_samples,
    output_dir,
    max_params_per_page = 40,
    verbose = FALSE
) {

    if (verbose) message("Creating SMC empirical distribution plots...")

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    param_names <- colnames(smc_samples)
    if (is.null(param_names)) param_names <- paste0("param_", 1:ncol(smc_samples))

    n_params <- length(param_names)
    n_pages <- ceiling(n_params / max_params_per_page)

    for (page in 1:n_pages) {

        start_idx <- (page - 1) * max_params_per_page + 1
        end_idx <- min(page * max_params_per_page, n_params)
        page_params <- param_names[start_idx:end_idx]

        pdf_file <- if (n_pages == 1) {
            file.path(output_dir, "smc_empirical_distributions.pdf")
        } else {
            file.path(output_dir, sprintf("smc_empirical_distributions_%d.pdf", page))
        }

        # Calculate layout first to determine optimal height
        n_params_page <- length(page_params)
        n_cols <- min(4, n_params_page)
        n_rows <- ceiling(n_params_page / n_cols)

        # Increase height based on number of rows (3 inches per row)
        pdf_height <- max(12, n_rows * 3)

        pdf(pdf_file, width = 14, height = pdf_height)

        par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))

        for (param in page_params) {

            if (!(param %in% colnames(smc_samples))) {
                next
            }

            param_vals <- smc_samples[, param]
            n_unique <- length(unique(param_vals))

            # Use histogram to show empirical distribution
            # This will reveal collapse to single value
            hist(param_vals,
                 breaks = min(50, max(10, n_unique)),
                 main = sprintf("%s\n(n_unique = %d)", param, n_unique),
                 xlab = "Value",
                 ylab = "Frequency",
                 col = "darkred",
                 border = "white")

            # Add vertical line at mean
            abline(v = mean(param_vals, na.rm = TRUE),
                   col = "blue", lwd = 2, lty = 2)

            # Add text annotation if collapsed
            if (n_unique == 1) {
                text(mean(param_vals), max(table(param_vals)) * 0.5,
                     "COLLAPSED\n(all samples\nidentical)",
                     col = "red", cex = 1.2, font = 2)
            } else if (n_unique < 10) {
                text(mean(param_vals), max(hist(param_vals, plot=FALSE)$counts) * 0.8,
                     sprintf("Only %d\nunique values", n_unique),
                     col = "orange", cex = 1.0, font = 2)
            }
        }

        dev.off()

        if (verbose) message("  Saved: ", basename(pdf_file))
    }

    return(invisible(NULL))
}


#' Plot NPE vs SMC Comparison (Empirical, No KDE Smoothing)
#'
#' @description
#' Creates comparison plots showing NPE vs SMC using histograms instead of
#' kernel density smoothing. This reveals weight collapse more honestly.
#'
#' @param npe_samples Matrix of NPE posterior samples
#' @param smc_samples Matrix of SMC posterior samples
#' @param output_dir Directory to save plots
#' @param max_params_per_page Maximum parameters per PDF page
#' @param verbose Logical, print progress
#'
#' @return Invisible NULL
#' @export
plot_npe_vs_smc_empirical <- function(
    npe_samples,
    smc_samples,
    output_dir,
    max_params_per_page = 40,
    verbose = FALSE
) {

    if (verbose) message("Creating NPE vs SMC empirical comparison plots...")

    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    param_names <- colnames(npe_samples)
    if (is.null(param_names)) param_names <- paste0("param_", 1:ncol(npe_samples))

    n_params <- length(param_names)
    n_pages <- ceiling(n_params / max_params_per_page)

    for (page in 1:n_pages) {

        start_idx <- (page - 1) * max_params_per_page + 1
        end_idx <- min(page * max_params_per_page, n_params)
        page_params <- param_names[start_idx:end_idx]

        pdf_file <- if (n_pages == 1) {
            file.path(output_dir, "npe_vs_smc_empirical.pdf")
        } else {
            file.path(output_dir, sprintf("npe_vs_smc_empirical_%d.pdf", page))
        }

        # Calculate layout first to determine optimal height
        n_params_page <- length(page_params)
        n_cols <- min(4, n_params_page)
        n_rows <- ceiling(n_params_page / n_cols)

        # Increase height based on number of rows (3 inches per row)
        pdf_height <- max(12, n_rows * 3)

        pdf(pdf_file, width = 14, height = pdf_height)

        par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))

        for (param in page_params) {

            if (!(param %in% colnames(npe_samples)) ||
                !(param %in% colnames(smc_samples))) {
                next
            }

            npe_vals <- npe_samples[, param]
            smc_vals <- smc_samples[, param]
            n_unique_smc <- length(unique(smc_vals))

            # Shared x-axis range
            x_range <- range(c(npe_vals, smc_vals), na.rm = TRUE)

            # Create histograms with transparency
            npe_hist <- hist(npe_vals, breaks = 30, plot = FALSE)
            smc_hist <- hist(smc_vals, breaks = 30, plot = FALSE)

            # Normalize to density scale for comparison
            npe_hist$density <- npe_hist$counts / sum(npe_hist$counts)
            smc_hist$density <- smc_hist$counts / sum(smc_hist$counts)

            y_max <- max(c(npe_hist$density, smc_hist$density))

            # Plot NPE
            plot(npe_hist, freq = FALSE, col = rgb(0, 0, 1, 0.3),
                 main = sprintf("%s (SMC: %d unique)", param, n_unique_smc),
                 xlab = "Value", ylab = "Density",
                 xlim = x_range, ylim = c(0, y_max * 1.1),
                 border = "steelblue")

            # Overlay SMC
            plot(smc_hist, freq = FALSE, col = rgb(1, 0, 0, 0.5),
                 border = "darkred", add = TRUE)

            # Add legend on first plot
            if (param == page_params[1]) {
                legend("topright", legend = c("NPE", "SMC"),
                      fill = c(rgb(0, 0, 1, 0.3), rgb(1, 0, 0, 0.5)),
                      border = c("steelblue", "darkred"),
                      cex = 0.8)
            }

            # Add warning if collapsed
            if (n_unique_smc == 1) {
                text(mean(x_range), y_max * 0.9, "COLLAPSED",
                     col = "red", cex = 1.2, font = 2)
            }
        }

        dev.off()

        if (verbose) message("  Saved: ", basename(pdf_file))
    }

    return(invisible(NULL))
}
