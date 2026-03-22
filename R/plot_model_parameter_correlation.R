#' Plot Posterior Parameter Correlation Heatmap
#'
#' Generates a heatmap of pairwise Spearman correlations between parameters in
#' the posterior (best subset), revealing trade-offs and redundancies. Parameters
#' are clustered hierarchically and only correlations exceeding a threshold are
#' labeled.
#'
#' @param results_file Path to samples.parquet file containing calibration results
#' @param priors_file Path to priors.json (used for parameter descriptions)
#' @param output_dir Directory to write the output figure
#' @param cor_threshold Minimum absolute correlation to display as text label.
#'   Default 0.3.
#' @param max_params Maximum number of parameters to display. The top
#'   \code{max_params} most-correlated parameters are selected. Default 25.
#' @param verbose Print progress messages
#'
#' @return Invisible NULL. Writes a PNG file to \code{output_dir}.
#'
#' @export
plot_model_parameter_correlation <- function(results_file,
                                             priors_file = NULL,
                                             output_dir = ".",
                                             cor_threshold = 0.3,
                                             max_params = 25,
                                             verbose = TRUE) {

  if (!file.exists(results_file)) {
    warning("Results file not found: ", results_file)
    return(invisible(NULL))
  }

  results <- arrow::read_parquet(results_file)

  # Identify parameter columns (exclude metadata, status, weight columns)
  meta_cols <- c("sim", "iter", "seed_sim", "seed_iter", "likelihood",
                 "is_finite", "is_valid", "is_outlier", "is_retained",
                 "is_best_subset", "is_best_model",
                 "weight_all", "weight_retained", "weight_best",
                 "N_j_initial")

  param_cols <- setdiff(names(results), meta_cols)

  # Use best subset if available, otherwise retained

  if ("is_best_subset" %in% names(results) && any(results$is_best_subset)) {
    sims <- results[results$is_best_subset == TRUE, param_cols, drop = FALSE]
    subset_label <- "best subset"
  } else if ("is_retained" %in% names(results) && any(results$is_retained)) {
    sims <- results[results$is_retained == TRUE, param_cols, drop = FALSE]
    subset_label <- "retained"
  } else {
    sims <- results[, param_cols, drop = FALSE]
    subset_label <- "all"
  }

  # Remove zero-variance columns (unsampled parameters)
  vars <- apply(sims, 2, stats::var, na.rm = TRUE)
  param_cols_active <- names(vars[vars > 0 & !is.na(vars)])

  if (length(param_cols_active) < 3) {
    warning("Fewer than 3 active parameters. Cannot compute correlation matrix.")
    return(invisible(NULL))
  }

  sims <- sims[, param_cols_active, drop = FALSE]

  # Compute Spearman correlation matrix
  cor_mat <- stats::cor(sims, use = "pairwise.complete.obs", method = "spearman")

  # Select top parameters by mean absolute off-diagonal Spearman correlation.
  # Ranking by mean |r| identifies parameters that are broadly correlated across
  # the posterior, rather than parameters that happen to appear in the single
  # highest-correlation pair (which was the previous, incorrect approach).
  diag(cor_mat) <- NA
  mean_abs_cor <- rowMeans(abs(cor_mat), na.rm = TRUE)
  diag(cor_mat) <- 1
  top_params <- names(sort(mean_abs_cor, decreasing = TRUE)[seq_len(min(max_params, length(mean_abs_cor)))])

  cor_subset <- cor_mat[top_params, top_params]

  # Hierarchical clustering for ordering
  if (nrow(cor_subset) > 2) {
    dist_mat <- stats::as.dist(1 - abs(cor_subset))
    hc <- stats::hclust(dist_mat, method = "complete")
    ord <- hc$order
    cor_subset <- cor_subset[ord, ord]
  }

  # Clean labels
  clean_label <- function(x) {
    x <- gsub("_([A-Z]{3})$", " (\\1)", x)
    x <- gsub("_", " ", x)
    x
  }

  rownames(cor_subset) <- clean_label(rownames(cor_subset))
  colnames(cor_subset) <- clean_label(colnames(cor_subset))

  # Convert to long format
  cor_plot_df <- as.data.frame(as.table(cor_subset))
  names(cor_plot_df) <- c("Param1", "Param2", "Correlation")
  cor_plot_df$Param1 <- factor(cor_plot_df$Param1, levels = rownames(cor_subset))
  cor_plot_df$Param2 <- factor(cor_plot_df$Param2, levels = colnames(cor_subset))

  n_params <- length(top_params)
  n_sims <- nrow(sims)

  # Significance test for each Spearman r: t = r * sqrt((n-2)/(1-r^2)) ~ t(n-2).
  # Only label off-diagonal cells that meet the threshold AND are significant.
  off_diag <- as.character(cor_plot_df$Param1) != as.character(cor_plot_df$Param2)
  r_vals <- cor_plot_df$Correlation
  t_stat <- r_vals * sqrt((n_sims - 2) / pmax(1 - r_vals^2, 1e-10))
  p_vals <- 2 * stats::pt(-abs(t_stat), df = n_sims - 2)
  cor_plot_df$sig <- ifelse(!off_diag, "",
                     ifelse(p_vals < 0.001, "***",
                     ifelse(p_vals < 0.01,  "**",
                     ifelse(p_vals < 0.05,  "*", ""))))

  cor_plot_df$label <- ifelse(
    abs(cor_plot_df$Correlation) >= cor_threshold &
      off_diag &
      cor_plot_df$sig != "",
    paste0(sprintf("%.2f", cor_plot_df$Correlation), cor_plot_df$sig),
    ""
  )

  p <- ggplot2::ggplot(cor_plot_df,
                       ggplot2::aes(x = .data$Param1, y = .data$Param2,
                                    fill = .data$Correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label),
                       size = max(2.5, 3.5 - n_params / 20),
                       color = "white") +
    ggplot2::scale_fill_gradientn(
      colors = mosaic_pal_diverging(11, "blue_red"),
      limits = c(-1, 1),
      name = "Spearman\ncorrelation"
    ) +
    ggplot2::labs(
      title = "Posterior Parameter Correlations",
      subtitle = sprintf("Top %d parameters by mean |correlation| (%s, n = %d); labels: r \u2265 %.1f and significant (*p<0.05 **p<0.01 ***p<0.001)",
                         n_params, subset_label, n_sims, cor_threshold)
    ) +
    theme_mosaic(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                           size = max(7, 10 - n_params / 6)),
      axis.text.y = ggplot2::element_text(size = max(7, 10 - n_params / 6)),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  # Size adapts to number of parameters
  fig_size <- max(10, n_params * 0.5 + 2)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(output_dir, "parameter_correlation.png")
  ggplot2::ggsave(out_file, p, width = fig_size, height = fig_size - 1,
                  dpi = 150, bg = "white")

  if (verbose) log_msg("Saved %s", out_file)
  invisible(NULL)
}
