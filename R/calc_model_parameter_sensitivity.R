#' Compute Parameter Sensitivity Ranking (R2-HSIC)
#'
#' Computes R2-HSIC (Hilbert-Schmidt Independence Criterion) between each sampled
#' parameter and the log-likelihood from a calibration \code{samples.parquet}
#' file. HSIC measures statistical dependence without assuming linearity,
#' monotonicity, or independence among inputs, making it valid for
#' importance-weighted posterior samples where parameters are correlated
#' (Da Veiga 2015). The R2-HSIC index equals zero iff the parameter and
#' log-likelihood are statistically independent (under the RBF kernel).
#'
#' This is the data-layer companion to \code{\link{plot_model_parameter_sensitivity}}:
#' the computation (and the \code{parameter_sensitivity.csv} write) live here so
#' the sensitivity table is produced independently of plotting, and the plot
#' consumes the returned/loaded data.frame.
#'
#' @param results_file Path to \code{samples.parquet} containing calibration results.
#' @param priors_file Path to \code{priors.json} (used for parameter descriptions).
#' @param output_dir Optional directory to write \code{parameter_sensitivity.csv}.
#'   When \code{NULL} (default) no file is written and the data.frame is returned.
#' @param n_samples Maximum number of posterior draws to use. Samples are drawn
#'   with replacement using importance weights. Default 2000.
#' @param kernel Kernel type for input parameters passed to
#'   \code{\link[sensitivity]{sensiHSIC}}. One of \code{"rbf"} (default),
#'   \code{"laplace"}, or \code{"dcov"}.
#' @param test_method Significance test method. \code{"Asymptotic"} (default) or
#'   \code{"Permutation"}.
#' @param subset_col Boolean subset-membership column used as the fallback when
#'   importance-weight ESS is degenerate. Default \code{"is_best_subset"}.
#' @param verbose Print progress messages.
#'
#' @return A list with \code{sens_df} (data.frame with columns \code{parameter},
#'   \code{hsic_r2}, \code{p_value}, \code{sig}, \code{description}),
#'   \code{descriptions} (named character), \code{subset_label}, and
#'   \code{n_used}. Returns \code{NULL} when the computation cannot proceed
#'   (missing file, too few sims/params, or HSIC failure).
#'
#' @references
#' Da Veiga S (2015). Global sensitivity analysis with dependence measures.
#' \emph{Journal of Statistical Computation and Simulation} 85(7):1283-1305.
#'
#' @export
calc_model_parameter_sensitivity <- function(results_file,
                                              priors_file = NULL,
                                              output_dir  = NULL,
                                              n_samples   = 2000,
                                              kernel      = "rbf",
                                              test_method = "Asymptotic",
                                              subset_col  = "is_best_subset",
                                              verbose     = TRUE) {

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
    estimator_type <- if (n_use < 200) "U-stat" else "V-stat"
    if (verbose) {
      log_msg(paste0("Warning: ESS of weight_retained = %.0f (n = %d). ",
                     "Falling back to best_subset (n = %d, estimator = %s)."),
              ess_retained, n_total, n_use, estimator_type)
    }
  } else {
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

  # Remove zero-variance columns again after subset selection
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
      test.method    = "No"
    )
    res <- sensitivity::tell(res, y_vec)
    res
  }, error = function(e) {
    warning("HSIC computation failed: ", e$message)
    NULL
  })

  if (is.null(hsic_result)) return(invisible(NULL))

  hsic_r2_vals <- stats::setNames(hsic_result$S[, "original"], param_cols_active)

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

  # -----------------------------------------------------------------------
  # CSV output (unconditional when output_dir supplied)
  # -----------------------------------------------------------------------
  if (!is.null(output_dir)) {
    if (!dir.exists(output_dir))
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    csv_file <- file.path(output_dir, "parameter_sensitivity.csv")
    utils::write.csv(
      sens_df[, c("parameter", "hsic_r2", "p_value", "sig", "description")],
      csv_file, row.names = FALSE
    )
    if (verbose) log_msg("Saved %s", csv_file)
  }

  invisible(list(
    sens_df      = sens_df,
    descriptions = descriptions,
    subset_label = subset_label,
    n_used       = nrow(params_df)
  ))
}
