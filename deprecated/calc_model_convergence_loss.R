#' Calculate convergence diagnostics from a loss and write to files
#'
#' Uses a Gibbs posterior constructed from a user-supplied loss to compute
#' weights and convergence diagnostics. Keeps only simulations with finite loss.
#' Effective sample size (ESS) is computed over all finite-loss simulations.
#' The best subset \eqn{\mathcal{B}} is defined by a robust loss band above the
#' minimum loss, and agreement metrics (A, CVw) are computed within \eqn{\mathcal{B}}.
#'
#' @details
#' Let \eqn{L_i} be the scalar loss (smaller is better). Define
#' \eqn{\Delta L_i = L_i - \min_j L_j} and the Gibbs weights
#' \deqn{w_i(\eta) \propto \exp\!\left(-\eta\,\Delta L_i\right),}
#' normalized across all finite-loss simulations. The best subset \eqn{\mathcal{B}}
#' is defined as:
#' \deqn{\mathcal{B}=\{i:\ L_i \le Q_{p}(L_{\text{non-outlier}})\},}
#' where \eqn{Q_p} is the p-quantile (e.g., p = 0.05 for top 5\%) calculated on
#' losses after removing high outliers. This provides robust and predictable selection.
#'
#' @section File Outputs:
#' \itemize{
#'   \item \code{convergence_results_loss.parquet} — columns:
#'         sim, seed, loss, delta_loss, w, w_tilde, in_best_band, in_top_pct, rank_loss
#'   \item \code{convergence_diagnostics_loss.json} — aggregate metrics and settings
#' }
#'
#' @param PATHS MOSAIC paths list from \code{get_paths()} (kept for API consistency).
#' @param results Data frame with columns: \code{sim}, \code{loss} (numeric; lower is better).
#' @param output_dir Directory to write output files.
#' @param eta Positive scalar for Gibbs weighting (default \code{1}).
#' @param top_quantile Quantile threshold for selection (default \code{0.05} = top 5\%).
#' @param outlier_mult Multiplier for IQR to define high outliers (default \code{1.5}). Values above Q3 + outlier_mult*IQR
#'   are excluded before applying quantile selection. Use \code{Inf} to disable outlier removal.
#' @param ess_min Target minimum ESS over all finite-loss sims (default \code{1000}).
#' @param A_min Target minimum agreement index within \eqn{\mathcal{B}} (default \code{0.8}).
#' @param cvw_max Target maximum coefficient of variation within \eqn{\mathcal{B}} (default \code{1.0}).
#' @param B_min Target minimum size of \eqn{\mathcal{B}} (default \code{50}).
#' @param verbose Print progress messages (default \code{TRUE}).
#'
#' @return A list with:
#' \itemize{
#'   \item \code{status} — "PASS" / "WARN" / "FAIL"
#'   \item \code{n_total} — total rows in \code{results}
#'   \item \code{n_successful} — number with finite loss
#'   \item \code{n_best} — size of best subset \eqn{\mathcal{B}}
#'   \item \code{files_written} — written file names
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' results <- data.frame(
#'   sim  = 1:1000,
#'   loss = rexp(1000, rate = 1) + rnorm(1000, sd = 0.1)
#' )
#' out <- calc_model_convergence_loss(
#'   PATHS = get_paths(),
#'   results = results,
#'   output_dir = "path/to/output",
#'   eta = 1.5,
#'   top_quantile = 0.05
#' )
#' print(out)
#' }
#'
#' @seealso
#'   [plot_model_convergence_loss()],
#'   [calc_model_loss_delta()],
#'   [calc_gibbs_weights()],
#'   [calc_model_ess()],
#'   [calc_model_agreement_index()],
#'   [calc_model_cvw()],
#'   [select_best_by_loss_band()]
#'
#' @family calibration-metrics
#' @export
calc_model_convergence_loss <- function(PATHS,
                                        results,
                                        output_dir,
                                        eta           = 1,
                                        top_quantile  = 0.05,
                                        outlier_mult  = 1,
                                        ess_min       = 1000,
                                        A_min         = 0.8,
                                        cvw_max       = 1.0,
                                        B_min         = 50,
                                        verbose       = TRUE) {

     # ---- Input validation -----------------------------------------------------
     if (!is.data.frame(results)) stop("results must be a data frame")
     required_cols <- c("sim", "loss")
     missing_cols <- setdiff(required_cols, names(results))
     if (length(missing_cols) > 0) {
          stop("results data frame missing required columns: ", paste(missing_cols, collapse = ", "))
     }
     if (!dir.exists(output_dir)) {
          dir.create(output_dir, recursive = TRUE)
          if (verbose) message("Created output directory: ", output_dir)
     }
     if (!(is.numeric(top_quantile) && length(top_quantile) == 1L && top_quantile > 0 && top_quantile <= 1)) {
          stop("top_quantile must be a scalar in (0, 1].")
     }

     # Keep only finite-loss simulations
     n_total <- nrow(results)
     ok <- is.finite(results$loss)
     if (!any(ok)) stop("No finite losses found in `results$loss`.")
     df <- results[ok, , drop = FALSE]
     loss <- df$loss
     n_successful <- nrow(df)

     if (verbose) {
          message("Processing loss-based convergence diagnostics for ", n_total, " simulations")
          message("Finite-loss simulations kept: ", n_successful, " (",
                  round(100 * n_successful / n_total, 1), "%)")
     }

     # ---- Weights (Gibbs) ------------------------------------------------------
     delta_loss <- calc_model_loss_delta(loss)
     gw <- calc_gibbs_weights(delta_loss, eta = eta)  # no truncation
     w <- gw$w
     w_tilde <- gw$w_tilde

     # ---- Metrics over ALL finite-loss sims ------------------------------------
     ESS_all <- calc_model_ess(w)  # normalizes internally

     # ---- Best subset B by quantile selection ------------------------------------
     top_sel <- select_best_by_loss_band(loss, top_quantile = top_quantile, outlier_mult = outlier_mult)
     in_best  <- top_sel$best
     B_size   <- top_sel$n_best
     thr_loss <- top_sel$threshold

     # Prepare weights within B (zero outside; helpers normalize within >0)
     w_B <- ifelse(in_best, w, 0)

     # Agreement & variability within B
     ag_B  <- calc_model_agreement_index(w_B)  # returns A, H, B_size
     A_B   <- ag_B$A
     CVw_B <- calc_model_cvw(w_B)

     # ---- Status thresholds ----------------------------------------------------
     status <- c(
          ESS  = if (!is.finite(ESS_all)) "fail" else if (ESS_all < 500) "fail" else if (ESS_all < ess_min) "warn" else "pass",
          A    = if (!is.finite(A_B))     "fail" else if (A_B < 0.5)    "fail" else if (A_B < A_min)      "warn" else "pass",
          CVw  = if (!is.finite(CVw_B))   "fail" else if (CVw_B > 2*cvw_max) "fail" else if (CVw_B > cvw_max) "warn" else "pass",
          B_sz = if (B_size < 1) "fail" else if (B_size < B_min) "warn" else "pass"
     )
     overall_status <- if (all(status == "pass")) "PASS" else if (any(status == "fail")) "FAIL" else "WARN"

     if (verbose) {
          message("Convergence metrics:")
          message("  ESS (all finite-loss): ", round(ESS_all, 0), " (target ≥ ", ess_min, ") - ", toupper(status["ESS"]))
          message("  A (best band): ", round(A_B, 3), " (target ≥ ", A_min, ") - ", toupper(status["A"]))
          message("  CVw (best band): ", round(CVw_B, 3), " (target ≤ ", cvw_max, ") - ", toupper(status["CVw"]))
          message("  |B| (best band): ", B_size, " - ", toupper(status["B_sz"]))
          message("  Overall status: ", overall_status)
     }

     # ---- Per-simulation results (Parquet) -------------------------------------
     rank_loss <- rank(loss, ties.method = "min")

     # in_top_pct is retained for compatibility with existing plotting code.
     # Here we set it equal to in_best (band-selected subset).
     in_top_pct <- in_best

     results_df <- data.frame(
          sim          = df$sim,
          seed         = df$sim,
          loss         = loss,
          delta_loss   = delta_loss,
          w            = w,
          w_tilde      = w_tilde,
          in_best_band = in_best,
          in_top_pct   = in_top_pct,
          rank_loss    = rank_loss
     )

     # ---- Diagnostics (JSON) ----------------------------------------------------
     diagnostics <- list(
          settings = list(
               eta          = eta,
               selection    = list(method = "quantile", top_quantile = top_quantile),
               # For backwards compatibility with plotting code that expects this:
               top_pct      = top_quantile,
               description  = "ESS over all finite-loss sims; best subset B defined by top quantile after outlier removal; A and CVw computed within B"
          ),
          targets = list(
               ess_min = list(value = ess_min, description = "Minimum ESS over all finite-loss sims"),
               A_min   = list(value = A_min,   description = "Minimum agreement index within B"),
               cvw_max = list(value = cvw_max, description = "Maximum coefficient of variation within B"),
               B_min   = list(value = B_min,   description = "Minimum size of best subset B")
          ),
          metrics = list(
               ess_all       = list(value = ESS_all,   description = "ESS over all finite-loss sims", status = status["ESS"]),
               A_B           = list(value = A_B,       description = "Agreement index within B",      status = status["A"]),
               cvw_B         = list(value = CVw_B,     description = "Coefficient of variation within B", status = status["CVw"]),
               B_size        = list(value = B_size,    description = "Size of best subset B",         status = status["B_sz"]),
               loss_threshold= list(value = thr_loss,  description = "Loss threshold defining the band", status = "info"),
               n_successful  = list(value = n_successful, description = "Number of finite-loss sims", status = "info")
          ),
          summary = list(
               total_simulations      = n_total,
               successful_simulations = n_successful,
               best_subset_size       = B_size,
               convergence_status     = overall_status
          )
     )

     # ---- Write files -----------------------------------------------------------
     results_file     <- file.path(output_dir, "convergence_results_loss.parquet")
     diagnostics_file <- file.path(output_dir, "convergence_diagnostics_loss.json")
     arrow::write_parquet(results_df, results_file)
     jsonlite::write_json(diagnostics, diagnostics_file, pretty = TRUE, auto_unbox = TRUE)

     if (verbose) {
          message("Files written to ", output_dir, ":")
          message("  - ", basename(results_file))
          message("  - ", basename(diagnostics_file))
     }

     # ---- Return summary --------------------------------------------------------
     list(
          status         = overall_status,
          n_total        = n_total,
          n_successful   = n_successful,
          n_best         = B_size,
          files_written  = c(basename(results_file), basename(diagnostics_file))
     )
}



# ---- Exported sub-functions ---------------------------------------------------

#' Compute ΔLoss relative to the best (minimum) loss
#'
#' @param loss Numeric vector of losses (smaller is better).
#' @return Numeric vector of \eqn{\Delta L_i = L_i - \min_j L_j}.
#' @examples
#' set.seed(1); loss <- rexp(10); calc_model_loss_delta(loss)
#' @family calibration-metrics
#' @export
calc_model_loss_delta <- function(loss) {
     stopifnot(is.numeric(loss), length(loss) > 0)
     if (all(!is.finite(loss))) return(rep(Inf, length(loss)))
     L_min <- min(loss[is.finite(loss)], na.rm = TRUE)
     dL <- loss - L_min
     dL[!is.finite(loss)] <- Inf
     dL
}

#' Gibbs posterior weights from ΔLoss (no truncation)
#'
#' @param delta_loss Numeric vector of \eqn{\Delta L}.
#' @param eta Positive scalar. Larger \code{eta} concentrates mass on low-loss draws.
#'
#' @return A list with \code{w} and \code{w_tilde}.
#' @examples
#' set.seed(1)
#' loss <- rexp(50); dL <- calc_model_loss_delta(loss)
#' gw <- calc_gibbs_weights(dL, eta = 2)
#' @family calibration-metrics
#' @export
calc_gibbs_weights <- function(delta_loss, eta = 0.5) {
     stopifnot(is.numeric(delta_loss), length(eta) == 1L, is.finite(eta), eta > 0)
     w <- exp(-eta * delta_loss)
     w[!is.finite(delta_loss)] <- 0
     s <- sum(w)
     w_tilde <- if (s > 0) w / s else w
     list(w = w, w_tilde = w_tilde)
}

#' Select best subset by quantile after removing outliers
#'
#' @description
#' Defines the best subset \eqn{\mathcal{B}} as the top quantile of models after
#' removing high outliers. High outliers are identified using the IQR method:
#' values above Q3 + outlier_mult*IQR are excluded before applying quantile selection.
#' This ensures robust and predictable selection of the best-performing models.
#'
#' @param loss Numeric vector of finite losses (smaller is better).
#' @param top_quantile Quantile threshold for selection (default 0.05 = top 5\%).
#' @param outlier_mult Multiplier for IQR to define outliers (default 1.5). Use Inf to disable outlier removal.
#'
#' @return List with \code{best} (logical), \code{idx}, \code{threshold}, \code{n_best}.
#' @examples
#' set.seed(1); loss <- rexp(100)
#' sel <- select_best_by_loss_band(loss, top_quantile = 0.05)
#' table(sel$best)
#' @family calibration-metrics
#' @export
select_best_by_loss_band <- function(loss, top_quantile = 0.05, outlier_mult = 1.5) {
     stopifnot(is.numeric(loss), all(is.finite(loss)))
     stopifnot(length(top_quantile) == 1L && top_quantile > 0 && top_quantile <= 1)
     stopifnot(length(outlier_mult) == 1L && outlier_mult > 0)

     # Remove high outliers using IQR method
     if (is.finite(outlier_mult)) {
          Q1 <- quantile(loss, 0.25)
          Q3 <- quantile(loss, 0.75)
          IQR <- Q3 - Q1
          outlier_threshold <- Q3 + outlier_mult * IQR
          # Identify non-outliers
          non_outlier_idx <- which(loss <= outlier_threshold)
          loss_robust <- loss[non_outlier_idx]
     } else {
          # If outlier_mult is Inf, use all data (no outlier removal)
          non_outlier_idx <- 1:length(loss)
          loss_robust <- loss
     }

     # Apply quantile selection on non-outlier data
     if (length(loss_robust) > 0) {
          thr <- quantile(loss_robust, probs = top_quantile, type = 1)
     } else {
          # Fallback if all data were outliers (shouldn't happen)
          thr <- quantile(loss, probs = top_quantile, type = 1)
     }

     best <- loss <= thr
     # Ensure at least one selected (guard against degenerate ranges)
     if (!any(best)) {
          best[which.min(loss)] <- TRUE
          thr <- min(loss)
     }
     list(best = best, idx = which(best), threshold = thr, n_best = sum(best))
}


#' Agreement index A based on entropy of weights (within B)
#'
#' For the set \eqn{\mathcal B = \{i : w_i > 0\}},
#' computes entropy \eqn{H(\tilde{\mathbf w}) = -\sum_{i\in \mathcal B}\tilde w_i \log \tilde w_i}
#' and \eqn{A = H(\tilde{\mathbf w}) / \log|\mathcal B|}, with the convention
#' \eqn{A=0} when \eqn{|\mathcal B| \le 1}.
#'
#' @param w Numeric vector of weights (raw or normalized). Only strictly positive entries form \eqn{\mathcal B}.
#'   Internally normalized within \eqn{\mathcal B}; larger \eqn{A} is better.
#'
#' @return A list with `A` (agreement index in \\[0,1\\]), `H` (entropy), and `B_size` (set size).
#'
#' @examples
#' set.seed(1)
#' loss <- 1000 + rnorm(200, sd = 3)
#' dL  <- calc_model_loss_delta(loss)
#' gw  <- calc_gibbs_weights(dL)
#' # pretend top 10 are "B"
#' wB <- gw$w; wB[-order(loss)[1:10]] <- 0
#' calc_model_agreement_index(wB)
#'
#' @seealso [calc_model_convergence_loss()]
#' @family calibration-metrics
#' @export
calc_model_agreement_index <- function(w) {
     stopifnot(is.numeric(w))
     B_idx <- which(w > 0)
     B <- length(B_idx)
     if (B <= 1) return(list(A = 0, H = 0, B_size = B))
     wB <- w[B_idx]
     wB <- wB / sum(wB)
     H  <- -sum(wB * log(wB))
     A  <- H / log(B)
     list(A = as.numeric(A), H = as.numeric(H), B_size = B)
}

#' Coefficient of variation of weights (within B)
#'
#' Computes \eqn{\mathrm{CV}_{\tilde{\mathbf w}} = \mathrm{sd}(\tilde{\mathbf w}) / \mathrm{mean}(\tilde{\mathbf w})}
#' over the set \eqn{\mathcal B = \{i : w_i > 0\}}, after normalizing within \eqn{\mathcal B}.
#'
#' @param w Numeric vector of weights (raw or normalized). Smaller CV indicates less skew.
#'
#' @return Scalar CV of weights over the set; `NA` if \eqn{|\mathcal B| < 2}.
#'
#' @examples
#' set.seed(1)
#' loss <- 1000 + rnorm(200, sd = 3)
#' dL  <- calc_model_loss_delta(loss)
#' gw  <- calc_gibbs_weights(dL)
#' wB <- ifelse(rank(loss, ties.method="min") <= 10, gw$w, 0)
#' calc_model_cvw(wB)
#'
#' @seealso [calc_model_convergence_loss()]
#' @family calibration-metrics
#' @export
calc_model_cvw <- function(w) {
     stopifnot(is.numeric(w))
     B_idx <- which(w > 0)
     if (length(B_idx) < 2) return(NA_real_)
     wB <- w[B_idx]
     wB <- wB / sum(wB)
     m  <- mean(wB)
     if (m == 0) return(NA_real_)
     s  <- sqrt(mean((wB - m)^2))  # population SD
     as.numeric(s / m)
}
