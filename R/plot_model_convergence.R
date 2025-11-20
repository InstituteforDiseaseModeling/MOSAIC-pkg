#' Plot Model Convergence Diagnostics from Files
#'
#' Creates a comprehensive diagnostic plot by reading convergence results from files
#' written by [calc_model_convergence()]. Visualizes log-likelihood rankings with
#' the best model highlighted and displays convergence metrics.
#'
#' @param results_dir Character string specifying the directory containing
#'   convergence_results.parquet and convergence_diagnostics.json files.
#' @param plots_dir Character string specifying the directory where the plot should
#'   be saved. If NULL, defaults to ../plots/ relative to results_dir.
#' @param verbose Logical indicating whether to print messages. Default is TRUE.
#'
#' @return Invisibly returns the ggplot object.
#'
#' @details
#' This function creates a publication-quality diagnostic plot showing:
#' \itemize{
#'   \item Ranked log-likelihood curve with the best model highlighted
#'   \item Convergence diagnostics displayed in the plot subtitle/caption
#'   \item Clear indication of model ensemble agreement metrics
#' }
#'
#' The function reads from two files:
#' \itemize{
#'   \item \code{convergence_results.parquet} — Per-simulation data
#'   \item \code{convergence_diagnostics.json} — Aggregate metrics and descriptions
#' }
#'
#' @section Important Note:
#' This visualizes **model-weight agreement** diagnostics, not MCMC convergence.
#' The "convergence" here refers to agreement across parameter draws, not chain
#' convergence. For MCMC diagnostics, use trace plots, R-hat, etc.
#'
#' @examples
#' \dontrun{
#' # First calculate convergence (writes files)
#' calc_model_convergence(
#'   PATHS = get_paths(),
#'   results = my_results,
#'   output_dir = "path/to/results"
#' )
#'
#' # Then create plot from files
#' plot <- plot_model_convergence(
#'   results_dir = "path/to/results",
#'   plots_dir = "path/to/plots"
#' )
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_hline theme_minimal theme element_text element_rect element_blank element_line labs ggsave scale_color_manual annotate
#' @importFrom scales pretty_breaks
#' @seealso [calc_model_convergence()]
#' @family calibration-metrics
plot_model_convergence <- function(results_dir,
                                   plots_dir = NULL,
                                   verbose = TRUE) {

    # ============================================================================
    # Input validation and setup
    # ============================================================================

    if (!dir.exists(results_dir)) {
        stop("results_dir does not exist: ", results_dir)
    }

    # Set default plots_dir if not provided
    if (is.null(plots_dir)) {
        plots_dir <- file.path(dirname(results_dir), "plots")
    }

    # Create plots directory if it doesn't exist
    if (!dir.exists(plots_dir)) {
        dir.create(plots_dir, recursive = TRUE)
        if (verbose) message("Created plots directory: ", plots_dir)
    }

    # Define expected file paths
    results_file <- file.path(results_dir, "convergence_results.parquet")
    diagnostics_file <- file.path(results_dir, "convergence_diagnostics.json")

    # Check that required files exist
    if (!file.exists(results_file)) {
        stop("Required file not found: ", results_file)
    }
    if (!file.exists(diagnostics_file)) {
        stop("Required file not found: ", diagnostics_file)
    }

    if (verbose) {
        message("Reading convergence files from: ", results_dir)
    }

    # ============================================================================
    # Read data from files
    # ============================================================================

    # Read results data
    results_data <- arrow::read_parquet(results_file)

    # Filter out outliers if the column exists (for cleaner visualization)
    n_outliers_removed <- 0
    if ("is_outlier" %in% names(results_data)) {
        n_total_with_outliers <- nrow(results_data)
        n_outliers <- sum(results_data$is_outlier, na.rm = TRUE)

        if (n_outliers > 0) {
            results_data <- results_data[!results_data$is_outlier, ]
            n_outliers_removed <- n_outliers

            if (verbose) {
                message(sprintf("Excluded %d outliers (%.1f%%) from plot for clarity",
                               n_outliers_removed,
                               100 * n_outliers_removed / n_total_with_outliers))
            }
        }
    }

    # Read diagnostics data
    diagnostics <- jsonlite::read_json(diagnostics_file)

    # Extract data for plotting (now without outliers)
    loglik <- results_data$likelihood
    delta <- results_data$delta_aic
    retained <- results_data$retained
    seeds <- results_data$seed
    sims <- if("sim" %in% names(results_data)) results_data$sim else results_data$seed
    n_draws <- nrow(results_data)
    n_successful <- sum(is.finite(loglik))

    # Null-coalescing operator for handling both old and new JSON structures
    `%||%` <- function(a, b) if (is.null(a)) b else a

    # Extract metrics and targets from diagnostics with safe conversion
    safe_numeric <- function(x, default = NA_real_) {
        if (is.null(x)) return(default)
        if (length(x) == 0) return(default)
        tryCatch(as.numeric(x), error = function(e) default, warning = function(w) default)
    }

    safe_character <- function(x, default = "unknown") {
        tryCatch(as.character(x), error = function(e) default)
    }

    # Extract metrics for quantile-based method
    metrics <- c(
        ESS = safe_numeric(diagnostics$metrics$ess_best$value),
        A = safe_numeric(diagnostics$metrics$A_B$value),
        CVw = safe_numeric(diagnostics$metrics$cvw_B$value),
        B_size = safe_numeric(diagnostics$metrics$B_size$value),
        n_outliers = safe_numeric(diagnostics$metrics$n_outliers$value)
    )

    # Extract simulation counts from summary
    n_total_original <- safe_numeric(diagnostics$summary$total_simulations_original) %||%
                       safe_numeric(diagnostics$summary$total_simulations) %||%
                       n_draws
    n_retained_all <- safe_numeric(diagnostics$summary$retained_simulations) %||% n_draws
    n_best_subset <- safe_numeric(diagnostics$summary$best_subset_simulations) %||% sum(retained)

    # Safely extract targets (handle both schemas: _min/_max and _best)
    targets <- c(
        ESS_min = safe_numeric(diagnostics$targets$ess_min$value) %||%
                  safe_numeric(diagnostics$targets$ess_best$value),
        A_min = safe_numeric(diagnostics$targets$A_min$value) %||%
                safe_numeric(diagnostics$targets$A_best$value),
        CVw_max = safe_numeric(diagnostics$targets$cvw_max$value) %||%
                  safe_numeric(diagnostics$targets$cvw_best$value),
        B_min = safe_numeric(diagnostics$targets$B_min$value),
        max_w_max = safe_numeric(diagnostics$targets$max_w_max$value)
    )

    # Extract status for quantile-based method
    status <- c(
        ESS = safe_character(diagnostics$metrics$ess_best$status),
        A = safe_character(diagnostics$metrics$A_B$status),
        CVw = safe_character(diagnostics$metrics$cvw_B$status),
        B_size = safe_character(diagnostics$metrics$B_size$status)
    )

    # ============================================================================
    # Prepare data for plotting
    # ============================================================================

    # Sort by log-likelihood to create ranking (worst to best, so best is at the end)
    sort_idx <- order(loglik, decreasing = FALSE, na.last = FALSE)
    plot_data <- data.frame(
        rank = seq_len(n_draws),
        loglik = loglik[sort_idx],
        delta = delta[sort_idx],
        retained = retained[sort_idx],
        seed = seeds[sort_idx],
        sim = sims[sort_idx]
    )

    # Find best simulation (last in sorted data since we sorted ascending)
    best_idx <- n_draws  # Last in sorted data (highest likelihood)
    best_ll <- plot_data$loglik[best_idx]
    best_seed <- plot_data$seed[best_idx]
    best_sim <- plot_data$sim[best_idx]

    # ============================================================================
    # Prepare convergence diagnostics text for annotation
    # ============================================================================

    # Create simple metrics annotation for lower right corner
    n_best <- as.numeric(metrics["B_size"])
    metrics_text <- sprintf("Total: %d\nRetained: %d\nBest subset: %d",
                           n_total_original, n_retained_all, n_best)

    # ============================================================================
    # Create footnote with metric definitions
    # ============================================================================

    # Footnote for quantile-based method
    top_percentile <- (diagnostics$settings$top_percentile %||% 0.01) * 100
    b_description <- sprintf("B: Best subset (top %.1f%% of non-outliers)", top_percentile)

    footnote_lines <- c(
        "Retained: Models passing outlier removal (finite, non-outlier)",
        "ESS: Effective Sample Size (all retained models)",
        "A: Agreement Index (entropy-based consensus within B, 0-1)",
        "CVw: Coefficient of Variation of weights (within B)",
        b_description
    )

    footnote_text <- paste(footnote_lines, collapse = "\n")

    # ============================================================================
    # Create main plot
    # ============================================================================

    # Determine title with best model info
    title_text <- sprintf("Log-Likelihoods Across Runs | Best model fit: Simulation: %d", best_sim)

    # Add note about outliers if any were removed
    if (n_outliers_removed > 0) {
        title_text <- paste0(title_text,
                           sprintf("\n(%d outliers excluded from visualization)",
                                  n_outliers_removed))
    }

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = rank, y = loglik)) +
        # Dashed horizontal line at best likelihood (drawn first so it's in back)
        ggplot2::geom_hline(yintercept = best_ll,
                           linetype = "dashed", color = "#2E8B57", alpha = 0.3, linewidth = 0.5) +
        # Line connecting all points
        ggplot2::geom_line(color = "gray70", linewidth = 0.8, alpha = 0.5, na.rm = TRUE) +
        # Non-retained points (gray)
        ggplot2::geom_point(data = plot_data[!plot_data$retained, ],
                           color = "gray60", size = 1.2, alpha = 0.6, na.rm = TRUE) +
        # Retained points (bold black)
        ggplot2::geom_point(data = plot_data[plot_data$retained, ],
                           color = "black", size = 2, na.rm = TRUE) +
        # Best likelihood point highlighted (green with green fill)
        ggplot2::geom_point(data = plot_data[best_idx, ],
                           color = "black", fill = "#2E8B57", size = 4, shape = 21, na.rm = TRUE) +
        # Best likelihood value label (centered below the green dot)
        ggplot2::annotate("text",
                         x = best_idx,  # Centered on best model
                         y = best_ll - (max(plot_data$loglik) - min(plot_data$loglik)) * 0.03,
                         label = sprintf("%.2f", best_ll),
                         hjust = 0.5, vjust = -3, size = 3.25, color = "#2E8B57", fontface = "bold") +
        # Metrics annotation in lower right corner
        ggplot2::annotate("text",
                         x = n_draws * 0.98,  # Position at 98% of x-axis
                         y = min(plot_data$loglik, na.rm = TRUE) +
                             (max(plot_data$loglik, na.rm = TRUE) - min(plot_data$loglik, na.rm = TRUE)) * 0.08,
                         label = metrics_text,
                         hjust = 1, vjust = 0, size = 3.5, color = "black",
                         lineheight = 1.1, family = "sans") +
        # Theme and styling
        ggplot2::theme_classic(base_size = 12) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(color = "gray90", linewidth = 0.25),
            panel.grid.major.y = ggplot2::element_line(color = "gray90", linewidth = 0.25),
            plot.title = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),
            plot.subtitle = ggplot2::element_text(size = 9, hjust = 0.5, color = "gray30"),
            plot.caption = ggplot2::element_text(size = 8, hjust = 0.5, color = "gray40",
                                                  margin = ggplot2::margin(t = 10)),
            axis.title.x = ggplot2::element_text(size = 10, margin = ggplot2::margin(t = 10)),
            axis.title.y = ggplot2::element_text(size = 10, margin = ggplot2::margin(r = 10)),
            plot.margin = ggplot2::margin(15, 15, 20, 15)
        ) +
        ggplot2::labs(
            x = "Run index (sorted by log-likelihood)",
            y = "Log-Likelihood",
            title = title_text,
            caption = footnote_text
        )

    # ============================================================================
    # Display and save plot
    # ============================================================================

    if (verbose) {
        print(p)
    }

    # Save plot
    plot_file <- file.path(plots_dir, "convergence_diagnostic.pdf")
    ggplot2::ggsave(plot_file, plot = p,
                   width = 10, height = 7, dpi = 600)

    if (verbose) {
        message("Convergence diagnostic plot saved to: ", plot_file)
        message("\n=== Convergence Summary ===")
        message(sprintf("Retained models: %d/%d (%.1f%%)",
                       n_retained_all, n_total_original, 100 * n_retained_all / n_total_original))
        message(sprintf("Best subset B: %d/%d retained (%.1f%%)",
                       n_best_subset, n_retained_all, 100 * n_best_subset / n_retained_all))

        # Use safe_sprintf for metrics display
        message(safe_sprintf("ESS: %.0f (target >= %.0f) - %s",
                            as.numeric(metrics["ESS"]), as.numeric(targets["ESS_min"]), as.character(status["ESS"])))
        message(safe_sprintf("Agreement Index: %.3f (target >= %.3f) - %s",
                            as.numeric(metrics["A"]), as.numeric(targets["A_min"]), as.character(status["A"])))
        message(safe_sprintf("Weight CV: %.3f (target <= %.3f) - %s",
                            as.numeric(metrics["CVw"]), as.numeric(targets["CVw_max"]), as.character(status["CVw"])))
        message(safe_sprintf("Retained Set Size: %.0f (target >= %.0f) - %s",
                            as.numeric(metrics["B_size"]), as.numeric(targets["B_min"]), as.character(status["B_size"])))
        message(sprintf("Overall Status: %s", safe_character(diagnostics$summary$convergence_status)))
    }

    # Return plot object
    invisible(p)
}
