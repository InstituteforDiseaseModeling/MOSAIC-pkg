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
#' @param results_file Path to samples.parquet file containing calibration results.
#' @param priors_file Path to priors.json (used for parameter descriptions in labels).
#' @param output_dir Directory to write the output figure and CSV.
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
#'   and CSV to \code{output_dir}.
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
                                             max_params = 30,
                                             n_samples = 2000,
                                             kernel = "rbf",
                                             test_method = "Asymptotic",
                                             subset_col = "is_best_subset",
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
                 "is_best_subset", "is_best_subset_opt", "is_best_model",
                 "weight_all", "weight_retained", "weight_best", "weight_best_opt",
                 "N_j_initial")

  param_cols <- setdiff(names(results), meta_cols)

  # Filter to valid simulations with finite likelihood
  sims <- results[results$is_finite == TRUE & is.finite(results$likelihood), ]
  n_total <- nrow(sims)

  if (n_total < 10) {
    warning("Fewer than 10 valid simulations. Cannot compute sensitivity.")
    return(invisible(NULL))
  }

  # Remove zero-variance columns (frozen / unsampled parameters)
  vars <- apply(sims[, param_cols, drop = FALSE], 2, stats::var, na.rm = TRUE)
  param_cols_active <- names(vars[vars > 0 & !is.na(vars)])
  p <- length(param_cols_active)

  if (p < 2) {
    warning("Fewer than 2 active parameters. Cannot compute sensitivity.")
    return(invisible(NULL))
  }

  # -----------------------------------------------------------------------
  # Sample selection: prefer best_subset when weights are degenerate
  # -----------------------------------------------------------------------
  # Compute ESS of weight_retained to detect degenerate posteriors.
  # ESS = (sum w)^2 / sum(w^2). When one sample dominates (ESS << n),
  # importance-weighted resampling just replicates that sample, inflating
  # all R2-HSIC values uniformly. In that case use is_best_subset directly.
  .compute_ess <- function(w) {
    w <- w[is.finite(w) & w > 0]
    if (length(w) == 0) return(0)
    calc_model_ess(w, method = "kish")
  }

  has_best_subset <- subset_col %in% names(results) &&
                     any(as.logical(results[[subset_col]]), na.rm = TRUE)

  w_retained <- if ("weight_retained" %in% names(sims)) sims$weight_retained else NULL
  ess_retained <- if (!is.null(w_retained)) .compute_ess(w_retained) else 0
  ess_threshold <- max(10, n_samples * 0.05)   # ESS must be >= 5% of requested n

  if (ess_retained < ess_threshold && has_best_subset) {
    # Degenerate weights: fall back to best_subset samples
    sims_sub <- results[as.logical(results[[subset_col]]), ]
    n_use <- nrow(sims_sub)
    label_name <- if (identical(subset_col, "is_best_subset_opt")) "best_subset_opt" else "best_subset"
    subset_label <- sprintf("%s (ESS of retained = %.0f < threshold %.0f)",
                            label_name, ess_retained, ess_threshold)
    # Use U-statistic at small n: V-stat positive bias is non-trivial when n < 200
    estimator_type <- if (n_use < 200) "U-stat" else "V-stat"
    if (verbose) {
      log_msg(paste0("Warning: ESS of weight_retained = %.0f (n = %d). ",
                     "Falling back to best_subset (n = %d, estimator = %s)."),
              ess_retained, n_total, n_use, estimator_type)
    }
  } else {
    # Normal path: importance-weighted resampling
    weight_col <- if (!is.null(w_retained) && ess_retained >= ess_threshold) {
      "weight_retained"
    } else if ("weight_all" %in% names(sims) &&
               any(is.finite(sims$weight_all) & sims$weight_all > 0)) {
      "weight_all"
    } else {
      NULL
    }

    n_use <- min(n_samples, n_total)

    if (!is.null(weight_col)) {
      w <- sims[[weight_col]]
      w[!is.finite(w) | w < 0] <- 0
      idx <- sample(n_total, size = n_use, prob = w + .Machine$double.eps, replace = TRUE)
    } else {
      idx <- sample(n_total, size = n_use, replace = FALSE)
    }

    sims_sub <- sims[idx, ]
    subset_label <- if (!is.null(weight_col)) "importance-weighted" else "uniform"
    estimator_type <- "V-stat"
  }

  # Remove zero-variance columns again after subset selection (some params may
  # be frozen in the best_subset that were variable in the full sample)
  vars2 <- apply(sims_sub[, param_cols_active, drop = FALSE], 2,
                 stats::var, na.rm = TRUE)
  param_cols_active <- names(vars2[vars2 > 0 & !is.na(vars2)])
  p <- length(param_cols_active)

  if (p < 2) {
    warning("Fewer than 2 active parameters after subset selection.")
    return(invisible(NULL))
  }

  params_df <- as.data.frame(sims_sub[, param_cols_active, drop = FALSE])
  y_vec     <- sims_sub$likelihood

  if (verbose) {
    log_msg("Computing HSIC sensitivity (n=%d, p=%d params, kernel=%s, estimator=%s)",
            nrow(params_df), p, kernel, estimator_type)
  }

  # -----------------------------------------------------------------------
  # Load parameter descriptions from priors.json
  # -----------------------------------------------------------------------
  descriptions <- stats::setNames(rep("", p), param_cols_active)

  if (!is.null(priors_file) && file.exists(priors_file)) {
    priors <- jsonlite::fromJSON(priors_file, simplifyVector = FALSE)

    for (nm in param_cols_active) {
      if (!is.null(priors$parameters_global[[nm]]$description)) {
        descriptions[nm] <- priors$parameters_global[[nm]]$description
      }
      for (loc in names(priors$parameters_location)) {
        if (!is.null(priors$parameters_location[[loc]][[nm]]$description)) {
          descriptions[nm] <- priors$parameters_location[[loc]][[nm]]$description
        }
      }
    }
  }

  # -----------------------------------------------------------------------
  # Compute HSIC sensitivity indices
  # -----------------------------------------------------------------------
  hsic_result <- tryCatch({
    res <- sensitivity::sensiHSIC(
      model          = NULL,
      X              = params_df,
      kernelX        = kernel,
      kernelY        = "rbf",
      paramX         = NA,
      paramY         = NA,
      estimator.type = estimator_type,
      test.method    = "No"   # significance computed separately via testHSIC
    )
    res <- sensitivity::tell(res, y_vec)  # must capture return value
    res
  }, error = function(e) {
    warning("HSIC computation failed: ", e$message)
    NULL
  })

  if (is.null(hsic_result)) return(invisible(NULL))

  # -----------------------------------------------------------------------
  # Extract R²-HSIC indices  (res$S is a data.frame, column "original")
  # -----------------------------------------------------------------------
  hsic_r2_vals <- stats::setNames(hsic_result$S[, "original"], param_cols_active)

  # -----------------------------------------------------------------------
  # Significance testing  (testHSIC returns $pval data.frame, column 1)
  # -----------------------------------------------------------------------
  p_vals <- tryCatch({
    test_res <- sensitivity::testHSIC(hsic_result, test.method = test_method)
    stats::setNames(test_res$pval[, 1], param_cols_active)
  }, error = function(e) {
    warning("HSIC significance test failed: ", e$message,
            ". Proceeding without p-values.")
    stats::setNames(rep(NA_real_, p), param_cols_active)
  })

  sig_stars <- ifelse(is.na(p_vals), "",
               ifelse(p_vals < 0.001, "***",
               ifelse(p_vals < 0.01,  "**",
               ifelse(p_vals < 0.05,  "*", ""))))

  # -----------------------------------------------------------------------
  # Build results data.frame
  # -----------------------------------------------------------------------
  sens_df <- data.frame(
    parameter   = param_cols_active,
    hsic_r2     = unname(hsic_r2_vals),
    p_value     = unname(p_vals),
    sig         = unname(sig_stars),
    description = unname(descriptions[param_cols_active]),
    stringsAsFactors = FALSE
  )

  sens_df <- sens_df[is.finite(sens_df$hsic_r2), ]
  sens_df <- sens_df[order(-sens_df$hsic_r2), ]

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
        top_n, p, subset_label, nrow(params_df), sig_note
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

  # -----------------------------------------------------------------------
  # CSV output
  # -----------------------------------------------------------------------
  csv_file <- file.path(output_dir, "parameter_sensitivity.csv")
  utils::write.csv(
    sens_df[, c("parameter", "hsic_r2", "p_value", "sig", "description")],
    csv_file, row.names = FALSE
  )
  if (verbose) log_msg("Saved %s", csv_file)

  invisible(sens_df)
}
