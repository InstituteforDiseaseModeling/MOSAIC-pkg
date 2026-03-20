#' Plot Parameter Sensitivity Ranking (PRCC)
#'
#' Computes Partial Rank Correlation Coefficients (PRCC) between each sampled
#' parameter and log-likelihood, then plots a ranked horizontal bar chart. PRCC
#' isolates the unique contribution of each parameter after controlling for all
#' others via rank-transformed linear regression (Marino et al. 2008).
#'
#' When the number of parameters approaches the number of simulations
#' (\code{n / p < 5}), a warning is emitted because the regression step becomes
#' unstable. In that case the function falls back to marginal Spearman rank
#' correlation and labels the plot accordingly.
#'
#' @param results_file Path to samples.parquet file containing calibration results
#' @param priors_file Path to priors.json (used for parameter descriptions in labels)
#' @param output_dir Directory to write the output figure
#' @param max_params Maximum number of parameters to display. Default 30.
#' @param verbose Print progress messages
#'
#' @return A data.frame with columns \code{parameter}, \code{prcc}, \code{description},
#'   and \code{method} (invisibly). Writes a PNG file to \code{output_dir}.
#'
#' @references
#' Marino S, Hogue IB, Ray CJ, Kirschner DE (2008). A methodology for
#' performing global uncertainty and sensitivity analysis in systems biology.
#' \emph{J Theor Biol} 254(1):178-196.
#'
#' @export
plot_model_parameter_sensitivity <- function(results_file,
                                             priors_file = NULL,
                                             output_dir = ".",
                                             max_params = 30,
                                             verbose = TRUE) {

  if (!file.exists(results_file)) {
    warning("Results file not found: ", results_file)
    return(invisible(NULL))
  }

  results <- arrow::read_parquet(results_file)

  # -----------------------------------------------------------------------
  # Identify parameter columns
  # -----------------------------------------------------------------------
  meta_cols <- c("sim", "iter", "seed_sim", "seed_iter", "likelihood",
                 "is_finite", "is_valid", "is_outlier", "is_retained",
                 "is_best_subset", "is_best_model",
                 "weight_all", "weight_retained", "weight_best",
                 "N_j_initial")

  param_cols <- setdiff(names(results), meta_cols)

  # Filter to valid simulations with finite likelihood
  sims <- results[results$is_finite == TRUE & is.finite(results$likelihood), ]
  n <- nrow(sims)

  if (n < 10) {
    warning("Fewer than 10 valid simulations. Cannot compute sensitivity.")
    return(invisible(NULL))
  }

  # Remove zero-variance columns
  vars <- apply(sims[, param_cols, drop = FALSE], 2, stats::var, na.rm = TRUE)
  param_cols_active <- names(vars[vars > 0 & !is.na(vars)])
  p <- length(param_cols_active)

  if (p < 2) {
    warning("Fewer than 2 active parameters. Cannot compute sensitivity.")
    return(invisible(NULL))
  }

  # -----------------------------------------------------------------------
  # Load parameter descriptions from priors.json
  # -----------------------------------------------------------------------
  descriptions <- stats::setNames(rep("", length(param_cols_active)), param_cols_active)

  if (!is.null(priors_file) && file.exists(priors_file)) {
    priors <- jsonlite::fromJSON(priors_file, simplifyVector = FALSE)

    for (nm in param_cols_active) {
      # Check global params
      if (!is.null(priors$parameters_global[[nm]]$description)) {
        descriptions[nm] <- priors$parameters_global[[nm]]$description
      }
      # Check location params (search all locations)
      for (loc in names(priors$parameters_location)) {
        if (!is.null(priors$parameters_location[[loc]][[nm]]$description)) {
          descriptions[nm] <- priors$parameters_location[[loc]][[nm]]$description
        }
      }
    }
  }

  # -----------------------------------------------------------------------
  # Decide method: PRCC (if n >> p) or fallback to marginal Spearman
  # -----------------------------------------------------------------------
  ratio <- n / p

  if (ratio < 5) {
    method <- "Spearman"
    if (verbose) {
      log_msg(paste0(
        "Warning: n/p ratio = %.1f (n=%d, p=%d). ",
        "PRCC requires n >> p for stable regression. ",
        "Falling back to marginal Spearman rank correlation."
      ), ratio, n, p)
    }
  } else {
    method <- "PRCC"
    if (verbose) {
      log_msg("Computing PRCC (n=%d, p=%d, n/p=%.1f)", n, p, ratio)
    }
  }

  # -----------------------------------------------------------------------
  # Compute sensitivity coefficients
  # -----------------------------------------------------------------------
  params_matrix <- as.matrix(sims[, param_cols_active, drop = FALSE])
  y <- sims$likelihood

  if (method == "PRCC") {
    # Rank-transform everything
    params_ranked <- apply(params_matrix, 2, rank)
    y_ranked <- rank(y)

    prcc_vals <- numeric(p)
    names(prcc_vals) <- param_cols_active

    for (i in seq_len(p)) {
      others <- params_ranked[, -i, drop = FALSE]
      res_x <- stats::residuals(stats::lm.fit(cbind(1, others), params_ranked[, i]))
      res_y <- stats::residuals(stats::lm.fit(cbind(1, others), y_ranked))
      prcc_vals[i] <- stats::cor(res_x, res_y, use = "complete.obs")
    }

  } else {
    # Marginal Spearman
    prcc_vals <- numeric(p)
    names(prcc_vals) <- param_cols_active

    for (i in seq_len(p)) {
      prcc_vals[i] <- tryCatch(
        stats::cor(rank(params_matrix[, i]), rank(y), use = "complete.obs"),
        warning = function(w) NA_real_,
        error = function(e) NA_real_
      )
    }
  }

  # -----------------------------------------------------------------------
  # Build results data.frame
  # -----------------------------------------------------------------------
  sens_df <- data.frame(
    parameter = names(prcc_vals),
    prcc = unname(prcc_vals),
    description = unname(descriptions[names(prcc_vals)]),
    method = method,
    stringsAsFactors = FALSE
  )

  sens_df <- sens_df[!is.na(sens_df$prcc), ]
  sens_df$abs_prcc <- abs(sens_df$prcc)
  sens_df$direction <- ifelse(sens_df$prcc > 0, "Positive", "Negative")
  sens_df <- sens_df[order(-sens_df$abs_prcc), ]

  # Limit to top parameters
  top_n <- min(max_params, nrow(sens_df))
  sens_top <- sens_df[seq_len(top_n), ]

  # -----------------------------------------------------------------------
  # Build labels: "clean_name  --  description" (truncated)
  # -----------------------------------------------------------------------
  clean_label <- function(x) {
    x <- gsub("_([A-Z]{3})$", " (\\1)", x)
    x <- gsub("_", " ", x)
    x
  }

  max_desc_chars <- 55

  sens_top$param_label <- sapply(seq_len(nrow(sens_top)), function(i) {
    nm <- clean_label(sens_top$parameter[i])
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

  # -----------------------------------------------------------------------
  # Build method label for plot
  # -----------------------------------------------------------------------
  method_label <- if (method == "PRCC") {
    "Partial Rank Correlation Coefficient"
  } else {
    "Spearman Rank Correlation (marginal)"
  }

  method_note <- if (method == "Spearman") {
    sprintf("\nn/p = %.1f < 5: marginal correlation used (does not control for other parameters)",
            ratio)
  } else {
    ""
  }

  # -----------------------------------------------------------------------
  # Plot
  # -----------------------------------------------------------------------
  p_sens <- ggplot2::ggplot(sens_top,
                            ggplot2::aes(x = .data$prcc, y = .data$param_label,
                                         fill = .data$direction)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, color = "#333333", linewidth = 0.4) +
    ggplot2::scale_fill_manual(
      values = c("Positive" = unname(mosaic_colors("mosaic_blue")),
                 "Negative" = unname(mosaic_colors("deaths"))),
      name = "Effect on\nlikelihood"
    ) +
    ggplot2::labs(
      title = sprintf("Parameter Sensitivity (%s)", method),
      subtitle = paste0(
        sprintf("Top %d of %d parameters by |%s| with log-likelihood (n = %d sims)",
                top_n, p, ifelse(method == "PRCC", "PRCC", "rho"), n),
        method_note
      ),
      x = method_label,
      y = NULL
    ) +
    theme_mosaic(base_size = 9) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 7, family = "mono"),
      legend.position = c(0.85, 0.12)
    )

  # Adaptive figure sizing
  fig_height <- max(6, top_n * 0.35 + 2)
  fig_width <- max(9, 9 + max(nchar(as.character(sens_top$param_label)), na.rm = TRUE) * 0.04)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(output_dir, "parameter_sensitivity.png")
  ggplot2::ggsave(out_file, p_sens, width = fig_width, height = fig_height,
                  dpi = 150, bg = "white")

  if (verbose) log_msg("Saved %s", out_file)

  # Also save the full sensitivity table as CSV
  csv_file <- file.path(output_dir, "parameter_sensitivity.csv")
  utils::write.csv(sens_df[, c("parameter", "prcc", "description", "method")],
                   csv_file, row.names = FALSE)
  if (verbose) log_msg("Saved %s", csv_file)

  invisible(sens_df)
}
