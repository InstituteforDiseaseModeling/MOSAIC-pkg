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
#' The indices are computed a chunk of parameters at a time, so the Gram
#' matrices held at once are bounded (1 GiB: 33 parameters at
#' \code{n_samples = 2000}) instead of growing with the number of parameters
#' (MOSAIC 1.5.0). On a synthetic 900-parameter run the peak memory fell from
#' 51.6 GB to 3.7 GB (a 916-parameter Kenya run had held 67 GB). Each parameter's
#' R2-HSIC and asymptotic p-value depend only on its own column and the
#' log-likelihood, so the results equal a single call over every parameter.
#' With \code{test_method = "Permutation"} each chunk draws its own
#' permutations, so those p-values are a different Monte Carlo draw.
#'
#' \code{run_MOSAIC()} calls it in every run, whatever \code{plots} is, and
#' writes \code{parameter_sensitivity.csv} (atomically: a temporary file
#' renamed into place, so a killed run never leaves a truncated table);
#' \code{\link{render_MOSAIC_figures}} plots that table and recomputes it only
#' when it is missing.
#'
#' @param results_file Path to \code{samples.parquet} containing calibration results.
#' @param priors_file Path to \code{priors.json} (used for parameter descriptions).
#' @param output_dir Optional directory to write \code{parameter_sensitivity.csv} (the \code{sens_df} columns plus run-level \code{weighting} and \code{n_used}). When \code{NULL} (default) no file is written.
#' @param n_samples Maximum number of posterior draws to use. Samples are drawn
#'   with replacement using the ensemble members' weights (\code{weight_best},
#'   or \code{weight_best_opt} when \code{subset_col = "is_best_subset_opt"}),
#'   the posterior the other outputs use; a file without them falls back to
#'   \code{weight_all} (all valid draws). Default 2000.
#' @param kernel Kernel type for input parameters passed to
#'   \code{\link[sensitivity]{sensiHSIC}}. One of \code{"rbf"} (default),
#'   \code{"laplace"}, or \code{"dcov"}.
#' @param test_method Significance test method. \code{"Asymptotic"} (default) or
#'   \code{"Permutation"}.
#' @param subset_col Boolean subset-membership column (the ensemble members)
#'   used instead when the posterior weights are degenerate (Kish ESS below
#'   \code{max(10, 0.05 * n_samples)}) or absent. Default \code{"is_best_subset"}.
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
  # Sampled parameters only: the compartment counts (S_j_initial, ...) and the
  # route split of beta_j0_tot are derived from sampled ones and would pad the
  # figure with exact duplicates (R14). Custom columns outside the parameter
  # table are kept when nothing matches.
  est_cols <- .mosaic_weight_health_param_cols(param_cols)
  if (length(est_cols) >= 2L) param_cols <- est_cols

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

  # The posterior weights: the ensemble members' weights (weight_best, or
  # weight_best_opt for the optimised members), the posterior every other
  # output is built from since MOSAIC 1.4.0 (R19). A run without them falls
  # back to weight_all, the coarsened posterior over every valid draw. The
  # legacy Akaike weight_retained is not a posterior and is not used.
  member_wcol <- switch(subset_col, is_best_subset = "weight_best",
                        is_best_subset_opt = "weight_best_opt", NULL)
  post_wcol <- if (!is.null(member_wcol) && member_wcol %in% names(sims) &&
                   any(is.finite(sims[[member_wcol]]) & sims[[member_wcol]] > 0)) member_wcol
               else if ("weight_all" %in% names(sims)) "weight_all" else NULL
  w_post <- if (!is.null(post_wcol)) sims[[post_wcol]] else NULL
  ess_post <- if (!is.null(w_post)) .compute_ess(w_post) else 0
  ess_threshold <- max(10, n_samples * 0.05)   # ESS must be >= 5% of requested n

  if (ess_post < ess_threshold && has_best_subset) {
    # Degenerate or absent posterior weights: use the ensemble members.
    sims_sub <- results[as.logical(results[[subset_col]]), ]
    n_use <- nrow(sims_sub)
    label_name <- if (identical(subset_col, "is_best_subset_opt")) "optimised ensemble members" else "ensemble members"
    subset_label <- sprintf("%s (posterior ESS = %.0f < threshold %.0f)",
                            label_name, ess_post, ess_threshold)
    estimator_type <- if (n_use < 200) "U-stat" else "V-stat"
    if (verbose) {
      log_msg(paste0("Posterior ESS = %.0f (n = %d) is below %.0f: using the %s (n = %d, ",
                     "estimator = %s)."),
              ess_post, n_total, ess_threshold, label_name, n_use, estimator_type)
    }
  } else {
    n_use <- min(n_samples, n_total)
    if (!is.null(w_post) && ess_post >= ess_threshold) {
      w <- w_post
      w[!is.finite(w) | w < 0] <- 0
      idx <- sample(n_total, size = n_use, prob = w + .Machine$double.eps, replace = TRUE)
      subset_label <- if (identical(post_wcol, "weight_all")) "posterior-weighted by weight_all (all valid draws)"
                      else sprintf("ensemble members, weighted by %s", post_wcol)
    } else {
      idx <- sample(n_total, size = n_use, replace = FALSE)
      subset_label <- "uniform"
    }
    sims_sub <- sims[idx, ]
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
    descriptions <- .mosaic_sensitivity_descriptions(priors, param_cols_active)
  }

  # -----------------------------------------------------------------------
  # Compute HSIC sensitivity indices, a chunk of parameters at a time
  # -----------------------------------------------------------------------
  # sensiHSIC() keeps one n x n Gram matrix per input (save.GM, which
  # testHSIC() reads), so one call over every parameter holds n^2 x p doubles:
  # 29 GB at n = 2,000 and p = 916, the 67 GB master of the Kenya counties run.
  # Each parameter's R2-HSIC and asymptotic p-value depend only on its own
  # column and the log-likelihood (per-column bandwidths), so the chunked
  # results equal the single call's exactly while the Gram matrices held at
  # once are bounded by .mosaic_hsic_chunks().
  hsic_r2_vals <- stats::setNames(rep(NA_real_, p), param_cols_active)
  p_vals       <- stats::setNames(rep(NA_real_, p), param_cols_active)
  test_errors  <- character(0)
  n_untested   <- 0L
  for (ix in .mosaic_hsic_chunks(p, nrow(params_df))) {
    hsic_result <- tryCatch({
      res <- sensitivity::sensiHSIC(
        model          = NULL,
        X              = params_df[, ix, drop = FALSE],
        kernelX        = kernel,
        kernelY        = "rbf",
        paramX         = NA,
        paramY         = NA,
        estimator.type = estimator_type,
        test.method    = "No"
      )
      sensitivity::tell(res, y_vec)
    }, error = function(e) {
      warning("HSIC computation failed: ", e$message)
      NULL
    })
    if (is.null(hsic_result)) return(invisible(NULL))
    hsic_r2_vals[ix] <- hsic_result$S[, "original"]
    p_vals[ix] <- tryCatch(
      sensitivity::testHSIC(hsic_result, test.method = test_method)$pval[, 1],
      error = function(e) {
        test_errors <<- c(test_errors, conditionMessage(e))
        n_untested <<- n_untested + length(ix)
        NA_real_
      })
    rm(hsic_result)
  }
  if (length(test_errors))
    warning("HSIC significance test failed: ", test_errors[1],
            ". Proceeding without p-values for ", n_untested, " of ", p, " parameters.")

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
    # weighting/n_used are run-level (constant per row) so a post-hoc render
    # of the CSV can state how the draws were chosen and how many were used.
    csv_df <- sens_df[, c("parameter", "hsic_r2", "p_value", "sig", "description")]
    csv_df$weighting <- rep(subset_label, nrow(csv_df))
    csv_df$n_used    <- rep(nrow(params_df), nrow(csv_df))
    .mosaic_atomic_write(csv_df, csv_file, function(data, file)
      utils::write.csv(data, file, row.names = FALSE))
    if (verbose) log_msg("Saved %s", csv_file)
  }

  invisible(list(
    sens_df      = sens_df,
    descriptions = descriptions,
    subset_label = subset_label,
    n_used       = nrow(params_df)
  ))
}


# Parameter descriptions for sensitivity output. Global parameters are looked
# up by name; location-scale columns ("<base>_<ISO>") use the single
# description stored per base parameter at parameters_location[[base]].
# @keywords internal
.mosaic_sensitivity_descriptions <- function(priors, param_cols) {
  descriptions <- stats::setNames(rep("", length(param_cols)), param_cols)
  for (nm in param_cols) {
    if (!is.null(priors$parameters_global[[nm]]$description)) {
      descriptions[nm] <- priors$parameters_global[[nm]]$description
    }
    base_nm <- sub("_[A-Z]{3}$", "", nm)
    if (!identical(base_nm, nm) &&
        !is.null(priors$parameters_location[[base_nm]]$description)) {
      descriptions[nm] <- priors$parameters_location[[base_nm]]$description
    }
  }
  descriptions
}


#' Split the HSIC parameters into chunks bounded by Gram-matrix memory
#'
#' \code{sensitivity::sensiHSIC()} keeps one \code{n x n} double Gram matrix
#' per input parameter. A chunk holds at most \code{budget_bytes} of them
#' (1 GiB: 33 parameters at n = 2,000, 134 at n = 1,000), and at least one.
#' Each chunk also pays a fixed cost (the output Gram matrix and the test), so
#' a smaller budget is slower: measured at 900 parameters and n = 2,000, 256 MiB
#' took 1.5x the single call's time at 1.9 GB and 1 GiB 1.13x at 3.7-4.0 GB.
#'
#' @param p Number of parameters.
#' @param n Number of draws (rows of the HSIC input).
#' @param budget_bytes Gram-matrix bytes a chunk may hold.
#' @return A list of integer index vectors covering \code{seq_len(p)} in order.
#' @keywords internal
#' @noRd
.mosaic_hsic_chunks <- function(p, n, budget_bytes = 1024^3) {
  size <- max(1L, as.integer(floor(budget_bytes / (8 * as.numeric(n)^2))))
  idx <- seq_len(p)
  unname(split(idx, ceiling(idx / size)))
}
