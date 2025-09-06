#' Plot Model Convergence Diagnostics
#'
#' Creates a single comprehensive diagnostic plot from model convergence analysis results.
#' Visualizes log-likelihood rankings with the best model highlighted and displays
#' convergence metrics with clear pass/warn/fail indicators in the legend.
#'
#' @param convergence_results A list object returned by [calc_model_convergence()].
#'   Must contain elements: loglik, delta, weights, metrics, targets, pass, status.
#' @param output_dir Character string specifying the directory where the plot should
#'   be saved. Directory will be created if it doesn't exist.
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
#' The plot helps diagnose:
#' \itemize{
#'   \item Whether the model ensemble has adequate effective sample size
#'   \item If weights are too concentrated in few draws (dominance issues)
#'   \item Agreement across the retained parameter set
#'   \item Overall convergence quality
#' }
#'
#' @section Important Note:
#' This visualizes **model-weight agreement** diagnostics, not MCMC convergence.
#' The "convergence" here refers to agreement across parameter draws, not chain
#' convergence. For MCMC diagnostics, use trace plots, R-hat, etc.
#'
#' @examples
#' \dontrun{
#' # Generate sample log-likelihoods
#' set.seed(42)
#' loglik <- 1500 + rnorm(1000, sd = 3)
#'
#' # Calculate convergence diagnostics
#' conv_results <- calc_model_convergence(loglik)
#'
#' # Create diagnostic plot
#' plot <- plot_model_convergence(
#'   convergence_results = conv_results,
#'   output_dir = "convergence_diagnostics"
#' )
#'
#' # View plot
#' print(plot)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_hline theme_minimal theme element_text element_rect element_blank element_line labs ggsave scale_color_manual annotate
#' @importFrom scales pretty_breaks
#' @seealso [calc_model_convergence()], [calc_model_akaike_weights()], [calc_model_ess()]
#' @family calibration-metrics
plot_model_convergence <- function(convergence_results,
                                  output_dir,
                                  verbose = TRUE) {

    # ============================================================================
    # Input validation
    # ============================================================================

    if (!is.list(convergence_results)) {
        stop("convergence_results must be a list (output from calc_model_convergence)")
    }

    required_elements <- c("loglik", "delta", "weights", "metrics", "targets", "pass", "status")
    missing_elements <- setdiff(required_elements, names(convergence_results))
    if (length(missing_elements) > 0) {
        stop("convergence_results missing required elements: ",
             paste(missing_elements, collapse = ", "))
    }

    if (missing(output_dir) || is.null(output_dir)) {
        stop("output_dir is required")
    }

    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        if (verbose) message("Created output directory: ", output_dir)
    }

    # Extract data
    loglik <- convergence_results$loglik
    delta <- convergence_results$delta
    weights <- convergence_results$weights
    metrics <- convergence_results$metrics
    targets <- convergence_results$targets
    pass <- convergence_results$pass
    status <- convergence_results$status
    seeds <- convergence_results$seeds  # May be NULL

    # ============================================================================
    # Prepare data for plotting
    # ============================================================================

    # Create main data frame with ranking
    n_draws <- length(loglik)

    # Count successful simulations (non-NA/non-infinite log-likelihoods)
    n_successful <- sum(is.finite(loglik))

    # Sort by log-likelihood to create ranking (worst to best, so best is at the end)
    sort_idx <- order(loglik, decreasing = FALSE, na.last = FALSE)
    plot_data <- data.frame(
        rank = seq_len(n_draws),
        loglik = loglik[sort_idx],
        delta = delta[sort_idx],
        retained = weights$retained[sort_idx]
    )

    # Add seeds if available
    if (!is.null(seeds)) {
        plot_data$seed <- seeds[sort_idx]
    }

    # Find best simulation (last in sorted data since we sorted ascending)
    best_idx <- n_draws  # Last in sorted data (highest likelihood)
    best_ll <- plot_data$loglik[best_idx]
    best_seed <- if (!is.null(seeds)) plot_data$seed[best_idx] else NA

    # ============================================================================
    # Prepare convergence diagnostics text for annotation
    # ============================================================================

    # Format metrics for display in lower right (excluding max(w))
    # Add extra line breaks for spacing
    diagnostic_lines <- c(
        sprintf("Retained: %d/%d", sum(weights$retained), n_draws),
        "",  # Empty line for spacing
        sprintf("ESS: %.0f [target ≥ %.0f]",
                metrics["ESS"], targets["ESS_min"]),
        "",  # Empty line for spacing
        sprintf("A: %.3f [target ≥ %.3f]",
                metrics["A"], targets["A_min"]),
        "",  # Empty line for spacing
        sprintf("CVw: %.3f [target ≤ %.3f]",
                metrics["CVw"], targets["CVw_max"]),
        "",  # Empty line for spacing
        sprintf("B: %.0f [target ≥ %.0f]",
                metrics["B_size"], targets["B_min"])
    )

    # ============================================================================
    # Create footnote with metric definitions
    # ============================================================================

    # Create concise definitions for each metric (bold names will be handled in theme)
    footnote_lines <- c(
        "ESS: Effective Sample Size (number of equally-weighted draws)",
        "A: Agreement Index (entropy-based consensus, 0-1)",
        "CVw: Coefficient of Variation of weights (dispersion measure)",
        "B: Retained set size (draws with ΔAIC ≤ 6)"
    )

    # Combine footnote lines with newline for better readability
    footnote_text <- paste(footnote_lines, collapse = "\n")

    # ============================================================================
    # Create main plot
    # ============================================================================

    # Determine title with best model info
    if (!is.na(best_seed)) {
        title_text <- sprintf("Log-Likelihoods Across Runs\nBest model fit | Simulation: %d | Iteration: 1 | Seed: %s",
                             n_draws, best_seed)
    } else {
        title_text <- sprintf("Log-Likelihoods Across Runs\nBest model fit | Simulation: %d | Iteration: 1",
                             n_draws)
    }

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = rank, y = loglik)) +
        # Dashed horizontal line at best likelihood (drawn first so it's in back)
        ggplot2::geom_hline(yintercept = best_ll,
                           linetype = "dashed", color = "#2E8B57", alpha = 0.3, linewidth = 0.5) +
        # Line connecting all points
        ggplot2::geom_line(color = "gray70", linewidth = 0.8, alpha = 0.7) +
        # Non-retained points (gray)
        ggplot2::geom_point(data = plot_data[!plot_data$retained, ],
                           color = "gray60", size = 1.2, alpha = 0.6) +
        # Retained points (bold black)
        ggplot2::geom_point(data = plot_data[plot_data$retained, ],
                           color = "black", size = 2) +
        # Best likelihood point highlighted (green with green fill)
        ggplot2::geom_point(data = plot_data[best_idx, ],
                           color = "#2E8B57", fill = "#2E8B57", size = 4, shape = 21) +
        # Best likelihood value label (centered below the green dot)
        ggplot2::annotate("text",
                         x = best_idx,  # Centered on best model
                         y = best_ll - (max(plot_data$loglik) - min(plot_data$loglik)) * 0.03,
                         label = sprintf("%.2f", best_ll),
                         hjust = 0.5, vjust = 1, size = 3, color = "#2E8B57", fontface = "bold") +
        # Stacked metrics annotation in lower right
        ggplot2::annotate("text",
                         x = n_draws * 0.98,  # Position at 98% of x-axis
                         y = min(plot_data$loglik) + (max(plot_data$loglik) - min(plot_data$loglik)) * 0.08,  # Lowered position
                         label = paste(diagnostic_lines, collapse = "\n"),
                         hjust = 1, vjust = 0, size = 3.8, color = "black",
                         lineheight = 0.8, family = "sans") +
        # Theme and styling
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major.x = ggplot2::element_line(color = "gray90"),
            panel.grid.major.y = ggplot2::element_line(color = "gray90"),
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
    plot_file <- file.path(output_dir, "convergence.pdf")
    ggplot2::ggsave(plot_file, plot = p,
                   width = 10, height = 7, dpi = 600)

    if (verbose) {
        message("Convergence diagnostic plot saved to: ", plot_file)
        message("\n=== Convergence Summary ===")
        message(sprintf("Successful simulations: %d/%d (%.1f%%)",
                       n_successful, n_draws, 100 * n_successful / n_draws))
        message(sprintf("ESS: %.0f (target ≥ %.0f) - %s",
                       metrics["ESS"], targets["ESS_min"], toupper(status["ESS"])))
        message(sprintf("Agreement Index: %.3f (target ≥ %.3f) - %s",
                       metrics["A"], targets["A_min"], toupper(status["A"])))
        message(sprintf("Weight CV: %.3f (target ≤ %.3f) - %s",
                       metrics["CVw"], targets["CVw_max"], toupper(status["CVw"])))
        message(sprintf("Retained Set Size: %.0f (target ≥ %.0f) - %s",
                       metrics["B_size"], targets["B_min"], toupper(status["B_size"])))
    }

    # Return plot object
    invisible(p)
}
