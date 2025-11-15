#' Calculate convergence diagnostics and write to files
#'
#' Calculates model-weight agreement diagnostics from calibration results and writes
#' both per-simulation data (Parquet) and aggregate diagnostics (JSON) to files.
#' This function measures \strong{model-weight agreement} and \strong{evidence concentration},
#' not MCMC convergence.
#'
#' @section Important Note:
#' This function measures \strong{model-weight agreement} and \strong{evidence concentration},
#' not MCMC convergence. For MCMC diagnostics, use R-hat, effective sample size from chains,
#' trace plots, etc. The "convergence" terminology here refers to agreement across the
#' ensemble of parameter draws, not chain convergence.
#'
#' @section File Outputs:
#' The function writes two files to the output directory:
#' \itemize{
#'   \item \code{convergence_results.parquet} — Per-simulation data with columns:
#'         sim, seed, likelihood, aic, delta_aic, w, w_tilde, retained
#'   \item \code{convergence_diagnostics.json} — Aggregate metrics with human-readable
#'         descriptions, targets, and pass/fail status
#' }
#'
#' @param PATHS MOSAIC paths object from \code{get_paths()}.
#' @param results Data frame with columns: sim, seed, likelihood (and optionally others).
#' @param output_dir Character string specifying directory to write output files.
#' @param delta_max Numeric Δ cutoff used for truncation (default \code{6}).
#' @param temperature Positive scalar temperature for weight scaling (default \code{1}).
#'   See [calc_model_akaike_weights()] for guidance.
#' @param ess_min Numeric target minimum effective sample size (ESS; default \code{1000}).
#' @param A_min Numeric target minimum agreement index (default \code{0.75}).
#' @param cvw_max Numeric target maximum coefficient of variation of weights (default \code{1.0}).
#' @param B_min Integer target minimum retained set size \eqn{B} (default \code{2}).
#' @param max_w_max Numeric in (0, 1] for the maximum allowed normalized weight (default \code{0.5}).
#' @param verbose Logical indicating whether to print progress messages (default \code{TRUE}).
#'
#' @return A list with summary information:
#' \itemize{
#'   \item \code{status} — Overall convergence status ("PASS", "WARN", "FAIL")
#'   \item \code{n_total} — Total number of simulations
#'   \item \code{n_successful} — Number of successful simulations (finite likelihood)
#'   \item \code{n_retained} — Number of simulations in retained set
#'   \item \code{files_written} — Vector of output file names
#' }
#'
#' @examples
#' \dontrun{
#' # Prepare results data frame
#' results <- data.frame(
#'   sim = 1:1000,
#'   seed = 1001:2000,
#'   likelihood = 1500 + rnorm(1000, sd = 3)
#' )
#'
#' # Calculate convergence and write files
#' summary <- calc_model_convergence(
#'   PATHS = get_paths(),
#'   results = results,
#'   output_dir = "path/to/output",
#'   temperature = 1.5
#' )
#'
#' print(summary)
#' }
#'
#' @seealso
#'   [plot_model_convergence()],
#'   [calc_model_aic_delta()],
#'   [calc_model_akaike_weights()]
#'
#' @family calibration-metrics
#' @export
calc_model_convergence <- function(PATHS,
                                   results,
                                   output_dir,
                                   delta_max   = 6,
                                   temperature = 1,
                                   ess_min     = 1000,
                                   A_min       = 0.75,
                                   cvw_max     = 1.0,
                                   B_min       = 2,
                                   max_w_max   = 0.5,
                                   verbose     = TRUE) {

    # ============================================================================
    # Input validation
    # ============================================================================

    if (!is.data.frame(results)) {
        stop("results must be a data frame")
    }

    required_cols <- c("sim", "likelihood")
    missing_cols <- setdiff(required_cols, names(results))
    if (length(missing_cols) > 0) {
        stop("results data frame missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
        if (verbose) message("Created output directory: ", output_dir)
    }

    # Extract likelihood data
    loglik <- results$likelihood
    seeds <- results$sim
    n_total <- length(loglik)
    n_successful <- sum(is.finite(loglik))

    if (verbose) {
        message("Processing convergence diagnostics for ", n_total, " simulations")
        message("Successful simulations: ", n_successful, " (", round(100 * n_successful / n_total, 1), "%)")
    }

    # ============================================================================
    # Calculate convergence metrics
    # ============================================================================

    delta   <- calc_model_aic_delta(loglik)
    weights <- calc_model_akaike_weights(delta,
                                         delta_max   = delta_max,
                                         temperature = temperature)

    ESS   <- calc_model_ess(weights$w_tilde)
    ag    <- calc_model_agreement_index(weights$w)
    CVw   <- calc_model_cvw(weights$w)
    max_w <- calc_model_max_weight(weights$w)

    metrics <- c(ESS = ESS, A = ag$A, CVw = CVw, B_size = ag$B_size, max_w = max_w)
    targets <- c(ESS_min = ess_min, A_min = A_min, CVw_max = cvw_max,
                 B_min = B_min, max_w_max = max_w_max)

    pass <- c(
        ESS    = is.finite(ESS)   && ESS   >= ess_min,
        A      = is.finite(ag$A)  && ag$A  >= A_min,
        CVw    = is.finite(CVw)   && CVw   <= cvw_max,
        B_size = ag$B_size        >= B_min,
        max_w  = is.finite(max_w) && max_w <= max_w_max
    )

    status <- c(
        ESS    = if (!is.finite(ESS)) "fail" else if (ESS < 500) "fail" else if (ESS < ess_min) "warn" else "pass",
        A      = if (!is.finite(ag$A)) "fail" else if (ag$A < 0.5) "fail" else if (ag$A < A_min) "warn" else "pass",
        CVw    = if (!is.finite(CVw)) "fail" else if (CVw > 2 * cvw_max) "fail" else if (CVw > cvw_max) "warn" else "pass",
        B_size = if (ag$B_size < 1) "fail" else if (ag$B_size < B_min) "warn" else "pass",
        max_w  = if (!is.finite(max_w)) "fail" else if (max_w > 0.9) "fail" else if (max_w > max_w_max) "warn" else "pass"
    )

    n_retained <- sum(weights$retained)
    overall_status <- if (all(status == "pass")) "PASS" else if (any(status == "fail")) "FAIL" else "WARN"

    if (verbose) {
        message("Convergence metrics calculated:")
        message("  ESS: ", round(ESS, 0), " (target >= ", ess_min, ") - ", toupper(status["ESS"]))
        message("  Agreement: ", round(ag$A, 3), " (target >= ", A_min, ") - ", toupper(status["A"]))
        message("  Retained: ", n_retained, " simulations")
        message("  Overall status: ", overall_status)
    }

    # ============================================================================
    # Create results data frame for Parquet export
    # ============================================================================

    results_df <- data.frame(
        sim = results$sim,
        seed = results$sim,
        likelihood = loglik,
        aic = -2 * loglik,
        delta_aic = delta,
        w = weights$w,
        w_tilde = weights$w_tilde,
        retained = weights$retained
    )

    # ============================================================================
    # Create diagnostics structure for JSON export
    # ============================================================================

    diagnostics <- list(
        settings = list(
            delta_max = delta_max,
            temperature = temperature,
            description = "Temperature-scaled truncated Akaike weights"
        ),
        targets = list(
            ess_min = list(value = ess_min, description = "Minimum effective sample size"),
            A_min = list(value = A_min, description = "Minimum agreement index (entropy-based consensus)"),
            cvw_max = list(value = cvw_max, description = "Maximum coefficient of variation of weights"),
            B_min = list(value = B_min, description = "Minimum retained set size"),
            max_w_max = list(value = max_w_max, description = "Maximum allowed normalized weight")
        ),
        metrics = list(
            # Legacy names for compatibility
            ess = list(value = ESS, description = "Effective sample size", status = status["ESS"]),
            agreement_index = list(value = ag$A, description = "Entropy-based model agreement", status = status["A"]),
            cvw = list(value = CVw, description = "Coefficient of variation of weights", status = status["CVw"]),
            retained_count = list(value = ag$B_size, description = "Number of models in retained set", status = status["B_size"]),
            max_weight = list(value = max_w, description = "Maximum normalized weight", status = status["max_w"]),
            # Expected names for plot_model_convergence_status
            ess_retained = list(value = ESS, description = "Effective sample size for retained simulations", status = status["ESS"]),
            B_size = list(value = ag$B_size, description = "Number of models in best subset", status = status["B_size"]),
            ess_best = list(value = ESS, description = "Effective sample size in best subset", status = status["ESS"]),
            A_B = list(value = ag$A, description = "Agreement index for best subset", status = status["A"]),
            cvw_B = list(value = CVw, description = "Coefficient of variation for best subset", status = status["CVw"])
        ),
        summary = list(
            total_simulations = n_total,
            successful_simulations = n_successful,
            retained_simulations = n_retained,
            convergence_status = overall_status,
            # Additional fields for plot_model_convergence_status compatibility
            total_simulations_original = n_total,
            n_successful = n_successful
        )
    )

    # ============================================================================
    # Write output files
    # ============================================================================

    # Write Parquet file
    results_file <- file.path(output_dir, "convergence_results.parquet")
    arrow::write_parquet(results_df, results_file)

    # Write JSON file
    diagnostics_file <- file.path(output_dir, "convergence_diagnostics.json")
    jsonlite::write_json(diagnostics, diagnostics_file, pretty = TRUE, auto_unbox = TRUE)

    files_written <- c("convergence_results.parquet", "convergence_diagnostics.json")

    if (verbose) {
        message("Files written to ", output_dir, ":")
        message("  - ", basename(results_file))
        message("  - ", basename(diagnostics_file))
    }

    # ============================================================================
    # Return summary
    # ============================================================================

    return(list(
        status = overall_status,
        n_total = n_total,
        n_successful = n_successful,
        n_retained = n_retained,
        n_best_subset = ag$B_size,
        ESS = ESS,
        A = ag$A,
        CVw = CVw,
        files_written = files_written
    ))
}




# ---- Exported sub-functions ---------------------------------------------------

#' Compute ΔAIC from log-likelihoods
#'
#' Computes AIC differences for each draw using
#' \deqn{\Delta_i = \text{AIC}_i - \text{AIC}_{\min}
#'       = -2\{\log \mathcal{L}(\theta^{(i)}) - \log \mathcal{L}_{\max}\}.}
#'
#' @param loglik Numeric vector of log-likelihoods (one per draw). Higher is better.
#'
#' @return Numeric vector of \eqn{\Delta} values (same length as `loglik`).
#'
#' @examples
#' set.seed(1)
#' ll <- 1000 + rnorm(10, sd = 2)
#' calc_model_aic_delta(ll)
#'
#' @seealso [calc_model_akaike_weights()], [calc_model_convergence()]
#' @family calibration-metrics
#' @export
calc_model_aic_delta <- function(loglik) {
     stopifnot(is.numeric(loglik), length(loglik) > 0)
     if (all(!is.finite(loglik))) {
          return(rep(Inf, length(loglik)))
     }
     ll_max <- max(loglik[is.finite(loglik)], na.rm = TRUE)
     delta  <- -2 * (loglik - ll_max)
     delta[!is.finite(loglik)] <- Inf
     delta
}





###############################################################################
## calc_model_akaike_weights.R  (UPDATED: adds temperature scaling)
###############################################################################

#' Truncated Akaike weights with optional temperature scaling
#'
#' Converts \eqn{\Delta} values into Akaike weights
#' \deqn{w_i(\tau) \propto \exp\!\left(-\tfrac{1}{2}\Delta_i / \tau\right)}
#' and normalized weights \eqn{\tilde w_i = w_i/\sum_j w_j}, with optional
#' truncation at \code{delta_max}. The temperature \eqn{\tau} flattens
#' (\eqn{\tau > 1}) or sharpens (\eqn{\tau < 1}) the weight distribution.
#'
#' @param delta Numeric vector of AIC differences \eqn{\Delta_i}. Derived from [calc_model_aic_delta()].
#' @param delta_max Numeric Δ cutoff for truncation (default \code{6}). Weights with
#'   \eqn{\Delta_i > \texttt{delta_max}} are set to zero; set to \code{Inf} to disable.
#' @param temperature Positive scalar temperature \eqn{\tau}; \code{1} reproduces
#'   standard Akaike weights (default). \eqn{\tau > 1} flattens weights (raises ESS),
#'   \eqn{\tau < 1} sharpens (more concentration on the top draw).
#'
#' @return A list with:
#' \itemize{
#'   \item `w`        — raw (possibly truncated) Akaike weights (after temperature).
#'   \item `w_tilde`  — normalized weights \eqn{\tilde w}.
#'   \item `retained` — logical vector; TRUE if \eqn{\Delta_i \le \texttt{delta_max}}.
#'   \item `B_idx`    — integer indices of retained draws.
#'   \item `delta_max`— the cutoff used.
#'   \item `temperature` — the temperature used.
#' }
#'
#' @section Choosing \code{temperature}:
#' \itemize{
#'   \item \strong{Default:} \code{temperature = 1}.
#'   \item \strong{Mild flattening:} \code{1.5–2}. Use when one or two draws dominate
#'         and you want smoother posterior plots and higher ESS.
#'   \item \strong{Strong flattening:} \code{3–5}. Use sparingly; weights become
#'         nearly uniform and discrimination drops. A practical target is to pick
#'         \code{temperature} so that \code{max(normalized weight)} \eqn{\le} \code{0.5}.
#' }
#'
#' @examples
#' set.seed(1)
#' ll <- 1000 + rnorm(200, sd = 3)
#' d  <- calc_model_aic_delta(ll)
#' # Standard (tau = 1)
#' aw1 <- calc_model_akaike_weights(d, delta_max = 20)
#' # Mild flattening
#' aw2 <- calc_model_akaike_weights(d, delta_max = 20, temperature = 2)
#' c(max_w1 = max(aw1$w / sum(aw1$w)), max_w2 = max(aw2$w / sum(aw2$w)))
#'
#' @seealso [calc_model_aic_delta()], [calc_model_convergence()]
#' @family calibration-metrics
#' @export
calc_model_akaike_weights <- function(delta,
                                      delta_max   = 6,
                                      temperature = 1) {
     stopifnot(is.numeric(delta),
               length(delta_max) == 1L,
               !is.na(delta_max),
               delta_max >= 0,
               is.numeric(temperature),
               length(temperature) == 1L,
               is.finite(temperature),
               temperature > 0)

     # Temperature-scaled (untruncated) weights
     w <- exp(-0.5 * delta / temperature)
     w[!is.finite(delta)] <- 0

     # Truncation by delta_max (if finite)
     if (is.finite(delta_max)) {
          w[delta > delta_max | is.na(delta)] <- 0
     } else {
          w[is.na(delta)] <- 0
     }

     s <- sum(w)
     if (s == 0) {
          w_tilde <- w
          retained <- rep(FALSE, length(w))
          B_idx <- integer(0)
          warning("All weights are zero after truncation; check inputs or delta_max.")
     } else {
          w_tilde <- w / s
          retained <- w > 0
          B_idx <- which(retained)
     }

     list(w = w, w_tilde = w_tilde, retained = retained, B_idx = B_idx,
          delta_max = delta_max, temperature = temperature)
}








#' Agreement index A based on entropy of weights
#'
#' For the retained set \eqn{\mathcal B = \{i : w_i > 0\}},
#' computes entropy \eqn{H(\tilde{\mathbf w}) = -\sum_{i\in \mathcal B}\tilde w_i \log \tilde w_i}
#' and \eqn{A = H(\tilde{\mathbf w}) / \log|\mathcal B|}, with the convention
#' \eqn{A=0} when \eqn{|\mathcal B| \le 1}.
#'
#' @param w Numeric vector of weights (raw or normalized). Only strictly positive entries form \eqn{\mathcal B}.
#'   Internally normalized within \eqn{\mathcal B}; larger \eqn{A} is better.
#'
#' @return A list with `A` (agreement index in \\[0,1\\]), `H` (entropy), and `B_size` (retained set size).
#'
#' @examples
#' set.seed(1)
#' ll <- 1000 + rnorm(200, sd = 3)
#' d  <- calc_model_aic_delta(ll)
#' aw <- calc_model_akaike_weights(d)
#' calc_model_agreement_index(aw$w)
#'
#' @seealso [calc_model_convergence()]
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

#' Coefficient of variation of (retained) weights
#'
#' Computes \eqn{\mathrm{CV}_{\tilde{\mathbf w}} = \mathrm{sd}(\tilde{\mathbf w}) / \mathrm{mean}(\tilde{\mathbf w})}
#' over the retained set \eqn{\mathcal B = \{i : w_i > 0\}}, after normalizing within \eqn{\mathcal B}.
#'
#' @param w Numeric vector of weights (raw or normalized). Smaller CV indicates less skew.
#'
#' @return Scalar CV of weights over the retained set; `NA` if \eqn{|\mathcal B| < 2}.
#'
#' @examples
#' set.seed(1)
#' ll <- 1000 + rnorm(200, sd = 3)
#' d  <- calc_model_aic_delta(ll)
#' aw <- calc_model_akaike_weights(d)
#' calc_model_cvw(aw$w)
#'
#' @seealso [calc_model_convergence()]
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
     s  <- sqrt(mean((wB - m)^2))    # population SD
     as.numeric(s / m)
}

#' Maximum normalized weight over the retained set
#'
#' Computes \eqn{\max_{i\in \mathcal B} \tilde w_i} where \eqn{\mathcal B = \{i : w_i > 0\}}
#' and \eqn{\tilde w} are weights normalized within \eqn{\mathcal B}.
#'
#' @param w Numeric vector of weights (raw or normalized). Smaller is better; 1 indicates a single retained draw.
#'
#' @return Scalar maximum normalized weight \eqn{\max_i \tilde w_i}; `NA` if none retained.
#'
#' @examples
#' set.seed(1)
#' ll <- 1000 + rnorm(200, sd = 3)
#' d  <- calc_model_aic_delta(ll)
#' aw <- calc_model_akaike_weights(d)
#' calc_model_max_weight(aw$w)
#'
#' @seealso [calc_model_convergence()]
#' @family calibration-metrics
#' @export
calc_model_max_weight <- function(w) {
     stopifnot(is.numeric(w))
     B_idx <- which(w > 0)
     B <- length(B_idx)
     if (B == 0) return(NA_real_)
     if (B == 1) return(1.0)
     wB <- w[B_idx]
     wB <- wB / sum(wB)
     max(wB)
}

