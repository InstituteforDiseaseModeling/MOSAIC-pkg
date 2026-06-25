#' Plot Parameter Sensitivity Ranking (HSIC)
#'
#' Computes R²-HSIC (Hilbert-Schmidt Independence Criterion) between each sampled
#' parameter and the log-likelihood, then plots a ranked horizontal bar chart.
#' HSIC measures statistical dependence without assuming linearity, monotonicity,
#' or independence among inputs, making it valid for importance-weighted posterior
#' samples where parameters are correlated (Da Veiga 2015).
#'
#' The R²-HSIC index equals zero if and only if the parameter and log-likelihood are
#' statistically independent (under the RBF kernel), and equals one if the
#' log-likelihood is a deterministic function of that parameter alone.
#'
#' Significance is assessed via an asymptotic Gamma approximation of the null
#' distribution (valid when n >= 100). Bars are coloured by significance level.
#'
#' The HSIC computation and the \code{parameter_sensitivity.csv} write live in
#' \code{\link{calc_model_parameter_sensitivity}}; this function renders the
#' ranked bar chart from that result. Pass a precomputed \code{sensitivity}
#' object to avoid recomputing, or let the function compute it from
#' \code{results_file}.
#'
#' @param results_file Path to samples.parquet file containing calibration results.
#' @param priors_file Path to priors.json (used for parameter descriptions in labels).
#' @param output_dir Directory to write the output figure. The CSV is written by
#'   \code{\link{calc_model_parameter_sensitivity}}, not here.
#' @param sensitivity Optional precomputed result list from
#'   \code{\link{calc_model_parameter_sensitivity}} (\code{$sens_df},
#'   \code{$subset_label}, \code{$n_used}). When supplied the recomputation is
#'   skipped and the CSV is not (re)written.
#' @param max_params Maximum number of parameters to display. Default 30.
#' @param n_samples Maximum number of posterior draws to use. Samples are drawn with
#'   replacement using importance weights. Default 2000.
#' @param kernel Kernel type for input parameters passed to
#'   \code{\link[sensitivity]{sensiHSIC}}. One of \code{"rbf"} (default),
#'   \code{"laplace"}, or \code{"dcov"}.
#' @param test_method Significance test method. \code{"Asymptotic"} (default, fast,
#'   requires n >= 100) or \code{"Permutation"} (exact, slower).
#' @param subset_col Character name of the boolean subset-membership column used
#'   as the fallback when importance-weight ESS is degenerate. Defaults to
#'   \code{"is_best_subset"}. Pass \code{"is_best_subset_opt"} to read the
#'   optimizer-refined subset.
#' @param verbose Print progress messages.
#'
#' @return A data.frame with columns \code{parameter}, \code{hsic_r2},
#'   \code{p_value}, \code{sig}, and \code{description} (invisibly). Writes a PNG
#'   to \code{output_dir}.
#'
#' @references
#' Da Veiga S (2015). Global sensitivity analysis with dependence measures.
#' \emph{Journal of Statistical Computation and Simulation} 85(7):1283-1305.
#' \url{https://arxiv.org/abs/1311.2483}
#'
#' Gretton A, Bousquet O, Smola A, Scholkopf B (2005). Measuring statistical
#' dependence with Hilbert-Schmidt norms. \emph{Algorithmic Learning Theory},
#' LNCS 3734:63-77.
#'
#' @export
plot_model_parameter_sensitivity <- function(results_file,
                                             priors_file = NULL,
                                             output_dir = ".",
                                             sensitivity = NULL,
                                             max_params = 30,
                                             n_samples = 2000,
                                             kernel = "rbf",
                                             test_method = "Asymptotic",
                                             subset_col = "is_best_subset",
                                             verbose = TRUE) {

  # Compute (and write the CSV) via the data-layer helper unless a precomputed
  # result was supplied. The plot is a pure consumer of `sens_df`.
  if (is.null(sensitivity)) {
    sensitivity <- calc_model_parameter_sensitivity(
      results_file = results_file,
      priors_file  = priors_file,
      output_dir   = output_dir,
      n_samples    = n_samples,
      kernel       = kernel,
      test_method  = test_method,
      subset_col   = subset_col,
      verbose      = verbose
    )
  }

  if (is.null(sensitivity) || is.null(sensitivity$sens_df) ||
      nrow(sensitivity$sens_df) == 0L) {
    return(invisible(NULL))
  }

  sens_df      <- sensitivity$sens_df
  subset_label <- sensitivity$subset_label %||% "importance-weighted"
  n_used       <- sensitivity$n_used %||% nrow(sens_df)
  p            <- nrow(sens_df)

  top_n <- min(max_params, nrow(sens_df))
  sens_top <- sens_df[seq_len(top_n), ]

  # -----------------------------------------------------------------------
  # Build labels
  # -----------------------------------------------------------------------
  clean_label <- function(x) {
    x <- gsub("_([A-Z]{3})$", " (\\1)", x)
    x <- gsub("_", " ", x)
    x
  }

  max_desc_chars <- 55

  sens_top$param_label <- sapply(seq_len(nrow(sens_top)), function(i) {
    nm   <- clean_label(sens_top$parameter[i])
    desc <- sens_top$description[i]
    if (!is.na(desc) && nchar(desc) > 0) {
      desc <- substr(desc, 1, max_desc_chars)
      paste0(nm, "   ", desc)
    } else {
      nm
    }
  })

  sens_top$param_label <- factor(sens_top$param_label,
                                 levels = rev(sens_top$param_label))

  # Significance tier: significant at p<0.05, marginal at p<0.1, not significant
  sens_top$sig_tier <- ifelse(
    is.na(sens_top$p_value), "Unknown",
    ifelse(sens_top$p_value < 0.05, "Significant (p < 0.05)",
    ifelse(sens_top$p_value < 0.10, "Marginal (p < 0.10)",
           "Not significant"))
  )

  sig_colors <- c(
    "Significant (p < 0.05)" = unname(mosaic_colors("mosaic_blue")),
    "Marginal (p < 0.10)"    = "#6FA8CC",
    "Not significant"        = "#BBBBBB",
    "Unknown"                = "#DDDDDD"
  )

  # -----------------------------------------------------------------------
  # Plot
  # -----------------------------------------------------------------------
  sig_note <- "bars coloured by significance (*p<0.05 **p<0.01 ***p<0.001)"

  p_sens <- ggplot2::ggplot(
    sens_top,
    ggplot2::aes(x = .data$hsic_r2, y = .data$param_label, fill = .data$sig_tier)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$sig, x = .data$hsic_r2 + 0.002),
      hjust = 0, size = 3.5, color = "#333333"
    ) +
    ggplot2::geom_vline(xintercept = 0, color = "#333333", linewidth = 0.4) +
    ggplot2::scale_fill_manual(
      values = sig_colors,
      name   = "Significance"
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::labs(
      title    = "Parameter Sensitivity (HSIC)",
      subtitle = sprintf(
        "Top %d of %d parameters by R\u00b2-HSIC with log-likelihood (%s, n = %d); %s",
        top_n, p, subset_label, n_used, sig_note
      ),
      x = expression(R^2 * "-HSIC  (kernel independence measure)"),
      y = NULL
    ) +
    theme_mosaic(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 11),
      legend.position    = c(0.80, 0.15)
    )

  fig_height <- max(7, top_n * 0.4 + 2)
  fig_width  <- max(10, 10 + max(nchar(as.character(sens_top$param_label)),
                                 na.rm = TRUE) * 0.06)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(output_dir, "parameter_sensitivity.png")
  ggplot2::ggsave(out_file, p_sens, width = fig_width, height = fig_height,
                  dpi = 150, bg = "white")

  if (verbose) log_msg("Saved %s", out_file)

  invisible(sens_df)
}
