#' Calculate Effective Sample Size (ESS)
#'
#' @description
#' Computes the effective sample size using either the Kish formula or
#' the perplexity (entropy-based) method.
#'
#' @param w Numeric vector of nonnegative weights (normalized or not).
#' @param method Character string specifying the calculation method:
#'   \code{"kish"} (default) or \code{"perplexity"}.
#' @param na_rm Logical; remove non-finite entries in \code{w}. Default \code{TRUE}.
#'
#' @return Numeric scalar: the effective sample size.
#'
#' @details
#' Two methods are available:
#'
#' \strong{Kish method} (\code{method = "kish"}):
#' Uses the standard Kish ESS formula: \eqn{\mathrm{ESS} = (\sum w)^2 / \sum w^2}.
#' This provides a measure of how much information is retained when using weighted
#' samples compared to an unweighted sample of the same size.
#'
#' \strong{Perplexity method} (\code{method = "perplexity"}):
#' Uses the exponential of the entropy: \eqn{\mathrm{ESS} = \exp(-\sum w_i \log w_i)},
#' where weights are first normalized to sum to 1. This measures the effective number
#' of models based on the entropy of the weight distribution. Also known as the
#' perplexity or exponential entropy.
#'
#' Both methods yield values between 1 (all weight on one model) and \eqn{n}
#' (uniform weights), but they measure slightly different aspects of weight
#' concentration. The Kish formula is more commonly used in survey sampling,
#' while perplexity is common in information theory and model averaging.
#'
#' @references
#' Kish, L. (1965). \emph{Survey Sampling}. Wiley.
#'
#' @examples
#' # Uniform weights
#' w_uniform <- rep(1/10, 10)
#' calc_model_ess(w_uniform, method = "kish")       # 10
#' calc_model_ess(w_uniform, method = "perplexity") # 10
#'
#' # Concentrated weights
#' w_conc <- c(0.9, rep(0.01, 10))
#' calc_model_ess(w_conc, method = "kish")       # ~1.2
#' calc_model_ess(w_conc, method = "perplexity") # ~1.5
#'
#' @family calibration-metrics
#' @export
#'

calc_model_ess <- function(w, method = c("kish", "perplexity"), na_rm = TRUE) {
     stopifnot(is.numeric(w), length(w) > 0L)
     method <- match.arg(method)

     if (na_rm) w <- w[is.finite(w)]
     n <- length(w)
     if (n == 0L) stop("No valid weights after filtering.")

     # Remove zero and negative weights
     w <- w[w > 0]
     if (length(w) == 0L) stop("No positive weights after filtering.")

     if (method == "kish") {
          # Kish formula: (sum w)^2 / sum(w^2)
          s2 <- sum(w^2)
          if (s2 <= 0) stop("sum(w^2) must be > 0.")
          ess <- (sum(w)^2) / s2
     } else if (method == "perplexity") {
          # Normalize weights to sum to 1
          w_norm <- w / sum(w)
          # Calculate entropy
          H <- -sum(w_norm * log(w_norm))
          # Perplexity = exp(entropy)
          ess <- exp(H)
     }

     ess
}
