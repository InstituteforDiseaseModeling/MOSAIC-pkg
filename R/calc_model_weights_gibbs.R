#' Gibbs-Posterior Model Weights
#'
#' @description
#' Converts a numeric vector of model scores (treated as **loss-like**, i.e.,
#' lower-is-better) into normalized posterior weights using the Gibbs
#' (generalized Bayes) formulation:
#' \deqn{w_m(\eta) = \frac{\exp(-\eta \, x_m)}{\sum_{j=1}^K \exp(-\eta \, x_j)} \,,}
#' where \eqn{x_m} is the loss (or information criterion) for model \eqn{m} and
#' \eqn{\eta \ge 0} is the inverse temperature (sharpness) parameter.
#'
#' @details
#' This function is agnostic to the specific form of \eqn{x}; smaller values are
#' assumed to indicate better models. Common choices include:
#' \itemize{
#'   \item \eqn{x = \Delta \mathrm{AIC}} \; (use \code{eta = 1/2} to
#'         recover Akaike weights),
#'   \item \eqn{x = -\log L} \; (use \code{eta = 1} for normalized likelihood weights),
#'   \item Cross-validated losses (e.g., RMSE, MSLE), or any proper scoring rule.
#' }
#' If you start from a higher-is-better score (e.g., log-likelihood), pass
#' \code{x = -score} so that lower-is-better holds.
#'
#' Numerical stability: the implementation subtracts \eqn{\min(x)} prior to
#' exponentiation; this shift leaves the normalized weights unchanged but helps
#' avoid underflow/overflow.
#'
#' Special cases:
#' \itemize{
#'   \item \code{eta = 0}: uniform weights.
#'   \item \code{eta = Inf}: hard selection (ties share weight equally).
#'   \item All \code{x} equal: uniform weights.
#' }
#'
#' @param x Numeric vector of model scores (treated as loss-like; lower is better).
#' @param eta Non-negative scalar inverse temperature \eqn{\eta}. Higher
#'   values concentrate weight on the best models; lower values flatten weights.
#'   Typical choices: \code{0.5} for \eqn{\Delta}AIC (Akaike weights),
#'   \code{1} for \eqn{-\log L}.
#' @param verbose Logical; if \code{TRUE}, prints brief diagnostics (range of
#'   \code{x}, applied shift, entropy, and effective number of models).
#' @param ... Reserved for backward compatibility. The deprecated argument
#'   \code{temperature} is accepted here and mapped to \code{eta} with a warning.
#'
#' @return
#' A numeric vector of length \code{length(x)} containing weights that sum to 1.
#' Names are preserved when present.
#'
#' @references
#' Bissiri, P. G., Holmes, C. C., & Walker, S. G. (2016).
#' A general framework for updating belief distributions.
#' \emph{Journal of the Royal Statistical Society: Series B}, 78(5), 1103--1130.
#' \href{https://doi.org/10.1111/rssb.12158}{doi:10.1111/rssb.12158}
#'
#' @examples
#' # Three models with Delta AIC = (0, 2, 6): Akaike weights via eta = 0.5
#' calc_model_weights_gibbs(x = c(0, 2, 6), eta = 0.5)
#'
#' # From negative log-likelihoods: normalized likelihood weights with eta = 1
#' calc_model_weights_gibbs(x = c(120.3, 121.1, 124.8), eta = 1)
#'
#' # Using a CV loss (lower is better) with moderate sharpness
#' set.seed(1)
#' losses <- c(A = 0.83, B = 0.81, C = 0.92)
#' calc_model_weights_gibbs(losses, eta = 3, verbose = TRUE)
#'
#' # Eta extremes
#' calc_model_weights_gibbs(c(1, 2, 5), eta = 0)      # uniform
#' calc_model_weights_gibbs(c(1, 2, 5), eta = Inf)    # hard selection
#'
#' @export
#'

calc_model_weights_gibbs <- function(x, eta, verbose = FALSE, ...) {

     # ---- backward compatibility: accept deprecated `temperature` ---------------
     dots <- list(...)
     if ("temperature" %in% names(dots)) {
          if (!missing(eta)) {
               stop("Cannot specify both `eta` and deprecated `temperature`.", call. = FALSE)
          }
          warning("`temperature` is deprecated; use `eta` instead.", call. = FALSE)
          eta <- dots[["temperature"]]
     }

     # ---- input checks ----------------------------------------------------------
     if (missing(x)) {
          stop("`x` is required and must be a numeric vector (lower is better).", call. = FALSE)
     }
     if (!is.numeric(x) || length(x) == 0L) {
          stop("`x` must be a non-empty numeric vector.", call. = FALSE)
     }
     if (anyNA(x) || any(!is.finite(x))) {
          stop("`x` must contain only finite, non-missing values.", call. = FALSE)
     }
     if (missing(eta)) {
          stop("`eta` is required (non-negative scalar).", call. = FALSE)
     }
     if (!is.numeric(eta) || length(eta) != 1L || is.na(eta)) {
          stop("`eta` must be a single non-missing numeric value.", call. = FALSE)
     }
     if (eta < 0) {
          stop("`eta` must be >= 0.", call. = FALSE)
     }

     n <- length(x)
     nm <- names(x)

     # ---- special cases ---------------------------------------------------------
     if (eta == 0 || diff(range(x)) == 0) {
          w <- rep(1 / n, n)
          if (!is.null(nm)) names(w) <- nm
          if (isTRUE(verbose)) {
               message("eta = 0 or all x equal: returning uniform weights.")
          }
          return(w)
     }

     if (is.infinite(eta)) {
          min_x <- min(x)
          idx   <- which(x == min_x)
          w     <- rep(0, n)
          w[idx] <- 1 / length(idx)
          if (!is.null(nm)) names(w) <- nm
          if (isTRUE(verbose)) {
               message(sprintf(
                    "eta = Inf: hard selection on min(x)=%.6g across %d tied model(s).",
                    min_x, length(idx)
               ))
          }
          return(w)
     }

     # ---- stabilized softmax over losses ---------------------------------------
     x_min   <- min(x)
     x_shift <- x - x_min                  # >= 0; preserves normalized weights
     s       <- exp(-eta * x_shift)         # in (0, 1]; exp(0) = 1 for best model
     z       <- sum(s)
     w       <- s / z

     if (!is.null(nm)) names(w) <- nm

     if (isTRUE(verbose)) {
          rng <- range(x)
          H   <- -sum(ifelse(w > 0, w * log(w), 0))
          eff <- exp(H)  # effective number of models (perplexity)
          message(sprintf("Gibbs weights with eta = %.6g", eta))
          message(sprintf("Input range: min = %.6g, max = %.6g, spread = %.6g", rng[1], rng[2], diff(rng)))
          message(sprintf("Stabilization: subtract min(x) = %.6g before exponentiation.", x_min))
          message(sprintf("Entropy = %.4f; effective number of models = %.3f", H, eff))
     }

     w
}
