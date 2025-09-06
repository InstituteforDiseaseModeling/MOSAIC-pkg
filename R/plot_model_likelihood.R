#' Plot Model Likelihood Curve
#'
#' Creates a sorted likelihood curve plot showing the performance of all 
#' calibration simulations. The plot displays simulations sorted by their 
#' log-likelihood values, with the best simulation highlighted.
#'
#' @param results A data frame containing calibration results with at minimum
#'   a 'likelihood' column, and optionally 'sim' and 'iter' columns for 
#'   identifying individual simulations. This is typically the output from 
#'   a model calibration run.
#' @param output_dir Character string specifying the directory where the plot 
#'   should be saved. Directory will be created if it doesn't exist.
#' @param verbose Logical indicating whether to print messages. Default is TRUE.
#'
#' @return Invisibly returns the ggplot object for the likelihood curve.
#'
#' @details
#' This function creates a publication-quality plot showing:
#' \itemize{
#'   \item All simulations sorted by log-likelihood (x-axis: simulation index, y-axis: log-likelihood)
#'   \item A smooth line connecting all points to show the curve
#'   \item Individual points for each simulation
#'   \item The best simulation highlighted with a red point
#'   \item A horizontal dotted line at the maximum likelihood value
#' }
#'
#' The plot helps visualize:
#' \itemize{
#'   \item The overall calibration performance
#'   \item The distribution of likelihood values
#'   \item How many simulations achieved good fits
#'   \item Whether there's a clear optimum or multiple good solutions
#' }
#'
#' @examples
#' \dontrun{
#' # Run calibration and collect results
#' results <- run_calibration()
#' 
#' # Create likelihood curve plot
#' p_likelihood <- plot_model_likelihood(
#'     results = results,
#'     output_dir = "calibration_output"
#' )
#' 
#' # The plot is automatically saved, but you can also access it
#' print(p_likelihood)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline theme_minimal theme element_text element_blank element_line labs ggsave scale_x_continuous scale_y_continuous
#' @importFrom dplyr filter arrange mutate slice_max
#' @importFrom scales pretty_breaks
plot_model_likelihood <- function(results, 
                                 output_dir, 
                                 verbose = TRUE) {
    
    # ============================================================================
    # Input validation
    # ============================================================================
    
    if (!is.data.frame(results)) {
        stop("results must be a data frame")
    }
    
    if (!"likelihood" %in% names(results)) {
        stop("results must contain a 'likelihood' column")
    }
    
    if (missing(output_dir) || is.null(output_dir)) {
        stop("output_dir is required")
    }
    
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        if (verbose) message("Created output directory: ", output_dir)
    }
    
    # ============================================================================
    # Prepare data
    # ============================================================================
    
    # Filter out NA likelihoods first
    results_valid <- results %>%
        dplyr::filter(!is.na(likelihood))
    
    # Check if we have any valid results
    if (nrow(results_valid) == 0) {
        warning("No simulations with valid likelihood values found")
        return(invisible(NULL))
    }
    
    # Sort results by likelihood and add simulation index
    results_sorted <- results_valid %>%
        dplyr::arrange(likelihood) %>%
        dplyr::mutate(sim_index = 1:dplyr::n())
    
    # Find the best simulation
    best_sim <- results_sorted %>%
        dplyr::slice_max(likelihood, n = 1)
    
    # ============================================================================
    # Build subtitle based on available information
    # ============================================================================
    
    n_successful <- nrow(results_sorted)
    n_total <- nrow(results)
    
    # Create informative subtitle
    if ("sim" %in% names(best_sim) && "iter" %in% names(best_sim)) {
        subtitle_text <- paste0(
            "N = ", n_successful, " successful simulations | ",
            "Best LL = ", sprintf("%.2f", best_sim$likelihood), 
            " (Sim ", best_sim$sim, ", Iter ", best_sim$iter, ")"
        )
    } else if ("sim" %in% names(best_sim)) {
        subtitle_text <- paste0(
            "N = ", n_successful, " successful simulations | ",
            "Best LL = ", sprintf("%.2f", best_sim$likelihood), 
            " (Sim ", best_sim$sim, ")"
        )
    } else {
        subtitle_text <- paste0(
            "N = ", n_successful, " successful simulations | ",
            "Best LL = ", sprintf("%.2f", best_sim$likelihood)
        )
    }
    
    # Add failed simulation count if any
    if (n_total > n_successful) {
        n_failed <- n_total - n_successful
        subtitle_text <- paste0(subtitle_text, " | ", n_failed, " failed")
    }
    
    # ============================================================================
    # Create the likelihood curve plot
    # ============================================================================
    
    p_likelihood_curve <- ggplot2::ggplot(results_sorted, 
                                          ggplot2::aes(x = sim_index, y = likelihood)) +
        # Main line
        ggplot2::geom_line(color = "gray30", linewidth = 0.8) +
        # Individual points
        ggplot2::geom_point(size = 0.5, color = "gray50", alpha = 0.6) +
        # Add horizontal dotted line at best likelihood
        ggplot2::geom_hline(yintercept = best_sim$likelihood,
                           linetype = "dotted",
                           color = "red3",
                           linewidth = 0.7,
                           alpha = 0.8) +
        # Add best point
        ggplot2::geom_point(data = best_sim,
                           ggplot2::aes(x = sim_index, y = likelihood),
                           fill = "red3",
                           shape = 21,
                           size = 3.5,
                           stroke = 1,
                           color = "black") +
        # Theme
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(linewidth = 0.25, color = "gray85"),
            axis.text = ggplot2::element_text(size = 9),
            axis.title = ggplot2::element_text(size = 10),
            plot.title = ggplot2::element_text(size = 12, face = "bold", hjust = 0),
            plot.subtitle = ggplot2::element_text(size = 10, hjust = 0),
            plot.caption = ggplot2::element_text(size = 8, hjust = 1, face = "italic")
        ) +
        # Labels
        ggplot2::labs(
            x = "Simulation Index (sorted by log-likelihood)",
            y = "Log-Likelihood",
            title = "Calibration Performance: Sorted Log-Likelihood Curve",
            subtitle = subtitle_text,
            caption = paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        ) +
        # Scales
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8))
    
    # ============================================================================
    # Display and save the plot
    # ============================================================================
    
    # Display the plot
    if (verbose) {
        print(p_likelihood_curve)
    }
    
    # Save the plot
    output_file <- file.path(output_dir, "calibration_likelihood_curve.pdf")
    ggplot2::ggsave(output_file,
                   plot = p_likelihood_curve,
                   width = 10,
                   height = 6,
                   dpi = 300)
    
    if (verbose) {
        message("Likelihood curve plot saved as '", output_file, "'")
        
        # Print summary statistics
        message("\n=== Calibration Summary ===")
        message("Total simulations: ", n_total)
        message("Successful simulations: ", n_successful)
        message("Failed simulations: ", n_total - n_successful)
        message("Best likelihood: ", sprintf("%.2f", max(results_sorted$likelihood)))
        message("Worst likelihood: ", sprintf("%.2f", min(results_sorted$likelihood)))
        
        # Print percentiles
        quantiles <- quantile(results_sorted$likelihood, 
                            probs = c(0.25, 0.5, 0.75, 0.9, 0.95))
        message("\nLikelihood percentiles:")
        message("  25th: ", sprintf("%.2f", quantiles[1]))
        message("  50th (median): ", sprintf("%.2f", quantiles[2]))
        message("  75th: ", sprintf("%.2f", quantiles[3]))
        message("  90th: ", sprintf("%.2f", quantiles[4]))
        message("  95th: ", sprintf("%.2f", quantiles[5]))
    }
    
    # Return plot object invisibly
    invisible(p_likelihood_curve)
}