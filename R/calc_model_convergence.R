#' Calculate model-weight agreement diagnostics from log-likelihoods
#'
#' Convenience wrapper that computes ΔAIC, truncated/temperature-scaled Akaike weights,
#' ESS, agreement index \eqn{A}, coefficient of variation of weights (CVw),
#' retained set size \eqn{B}, and the maximum normalized weight
#' \eqn{\max_i \tilde w_i}. Results are compared to recommended thresholds.
#'
#' @section Important Note:
#' This function measures \strong{model-weight agreement} and \strong{evidence concentration},
#' not MCMC convergence. For MCMC diagnostics, use R-hat, effective sample size from chains,
#' trace plots, etc. The "convergence" terminology here refers to agreement across the
#' ensemble of parameter draws, not chain convergence.
#'
#' @section Assumptions:
#' The ΔAIC computation assumes all draws have the same number of parameters k.
#' For models with different k, compute AIC = -2*loglik + 2*k first, then use Δ = AIC - min(AIC).
#'
#' @param loglik Numeric vector of log-likelihoods (one per draw). Higher is better.
#' @param seeds Optional vector of seeds corresponding to each likelihood value (same length as \code{loglik}).
#' @param delta_max Numeric Δ cutoff used for truncation (default \code{6}).
#' @param temperature Positive scalar temperature for weight scaling (default \code{1}).
#'   See [calc_model_akaike_weights()] for guidance.
#' @param ess_min Numeric target minimum effective sample size (ESS; default \code{1000}).
#' @param A_min Numeric target minimum agreement index (default \code{0.75}).
#' @param cvw_max Numeric target maximum coefficient of variation of weights (default \code{1.0}).
#' @param B_min Integer target minimum retained set size \eqn{B} (default \code{2}).
#' @param max_w_max Numeric in (0, 1] for the maximum allowed normalized weight (default \code{0.5}).
#'
#' @return A list with:
#' \itemize{
#'   \item `loglik`   — original log-likelihood vector.
#'   \item `delta`    — vector of \eqn{\Delta_i}.
#'   \item `delta_max` — the Δ cutoff used.
#'   \item `temperature` — the temperature used.
#'   \item `weights`  — list from [calc_model_akaike_weights()] with `w`, `w_tilde`, `retained`, `B_idx`.
#'   \item `retained` — logical vector indicating retained draws (same as `weights$retained`).
#'   \item `H`        — entropy value from agreement index calculation.
#'   \item `metrics`  — named numeric vector: `ESS`, `A`, `CVw`, `B_size`, `max_w`.
#'   \item `targets`  — named numeric vector of thresholds.
#'   \item `pass`     — named logical vector indicating if each metric meets target.
#'   \item `status`   — named character vector: "pass", "warn", or "fail" for each metric.
#' }
#'
#' @section Recommended usage:
#' \itemize{
#'   \item If the top model dominates (\code{max_w \u2192 1}), try \code{temperature = 1.5–2}
#'         and/or a looser \code{delta_max} (e.g., 20–100) to increase ESS while preserving ranking.
#'   \item Keep \code{temperature \u2264 3} for most workflows to avoid overly flat weights.
#' }
#'
#' @examples
#' set.seed(42)
#' ll <- 1500 + rnorm(3000, sd = 3)
#' # Mild flattening for smoother posterior plots
#' res <- calc_model_convergence(ll, delta_max = 20, temperature = 1.8)
#' res$metrics; res$pass
#'
#' @seealso
#'   [calc_model_aic_delta()],
#'   [calc_model_akaike_weights()],
#'   [calc_model_ess()],
#'   [calc_model_agreement_index()],
#'   [calc_model_cvw()],
#'   [calc_model_max_weight()]
#'
#' @family calibration-metrics
#' @export
calc_model_convergence <- function(loglik,
                                   seeds       = NULL,
                                   delta_max   = 6,
                                   temperature = 1,
                                   ess_min     = 1000,
                                   A_min       = 0.75,
                                   cvw_max     = 1.0,
                                   B_min       = 2,
                                   max_w_max   = 0.5) {

     stopifnot(is.numeric(loglik), length(loglik) > 0)
     if (!is.null(seeds)) stopifnot(length(seeds) == length(loglik))

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

     result <- list(
          loglik      = loglik,
          delta       = delta,
          delta_max   = delta_max,
          temperature = temperature,
          weights     = weights,
          retained    = weights$retained,
          H           = ag$H,
          metrics     = metrics,
          targets     = targets,
          pass        = pass,
          status      = status
     )
     if (!is.null(seeds)) result$seeds <- seeds
     result
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
#'         \code{temperature} so that \code{max(normalized weight) \u2264 0.5}.
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







#' Effective sample size (ESS) of weights
#'
#' Computes \eqn{\widehat{\mathrm{ESS}} = 1 / \sum_i \tilde w_i^2}.
#' If weights are not normalized, they are normalized internally before computing ESS.
#'
#' @param w Numeric vector of weights (raw or normalized). Larger ESS is better.
#'
#' @return Scalar ESS.
#'
#' @examples
#' set.seed(1)
#' ll <- 1000 + rnorm(500, sd = 3)
#' d  <- calc_model_aic_delta(ll)
#' aw <- calc_model_akaike_weights(d)
#' calc_model_ess(aw$w_tilde)
#'
#' @seealso [calc_model_convergence()]
#' @family calibration-metrics
#' @export
calc_model_ess <- function(w) {
     stopifnot(is.numeric(w))
     s <- sum(w)
     if (s <= 0) return(NA_real_)
     wt <- w / s
     denom <- sum(wt^2)
     if (denom <= 0) return(NA_real_)
     1 / denom
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
#' @return A list with `A` (agreement index in [0,1]), `H` (entropy), and `B_size` (retained set size).
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

