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
#'   \item \eqn{x = \Delta \mathrm{AIC}} \; (use \code{temperature = 1/2} to
#'         recover Akaike weights),
#'   \item \eqn{x = -\log L} \; (use \code{temperature = 1} for normalized likelihood weights),
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
#'   \item \code{temperature = 0}: uniform weights.
#'   \item \code{temperature = Inf}: hard selection (ties share weight equally).
#'   \item All \code{x} equal: uniform weights.
#' }
#'
#' @param x Numeric vector of model scores (treated as loss-like; lower is better).
#' @param temperature Non-negative scalar inverse temperature \eqn{\eta}. Higher
#'   values concentrate weight on the best models; lower values flatten weights.
#'   Typical choices: \code{0.5} for \eqn{\Delta}AIC (Akaike weights),
#'   \code{1} for \eqn{-\log L}.
#' @param verbose Logical; if \code{TRUE}, prints brief diagnostics (range of
#'   \code{x}, applied shift, entropy, and effective number of models).
#'
#' @return
#' A numeric vector of length \code{length(x)} containing weights that sum to 1.
#' Names are preserved when present.
#'
#' @references
#' Bissiri, P. G., Holmes, C. C., & Walker, S. G. (2016).
#' A general framework for updating belief distributions.
#' \emph{Journal of the Royal Statistical Society: Series B}, 78(5), 1103–1130.
#' \href{https://doi.org/10.1111/rssb.12158}{doi:10.1111/rssb.12158}
#'
#' @examples
#' # Three models with Delta AIC = (0, 2, 6): Akaike weights via temperature = 0.5
#' calc_model_weights_gibbs(x = c(0, 2, 6), temperature = 0.5)
#'
#' # From negative log-likelihoods: normalized likelihood weights with temperature = 1
#' calc_model_weights_gibbs(x = c(120.3, 121.1, 124.8), temperature = 1)
#'
#' # Using a CV loss (lower is better) with moderate sharpness
#' set.seed(1)
#' losses <- c(A = 0.83, B = 0.81, C = 0.92)
#' calc_model_weights_gibbs(losses, temperature = 3, verbose = TRUE)
#'
#' # Temperature extremes
#' calc_model_weights_gibbs(c(1, 2, 5), temperature = 0)      # uniform
#' calc_model_weights_gibbs(c(1, 2, 5), temperature = Inf)    # hard selection
#'
#' @export
#'

calc_model_weights_gibbs <- function(x, temperature, verbose = FALSE) {

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
     if (missing(temperature)) {
          stop("`temperature` is required (non-negative scalar).", call. = FALSE)
     }
     if (!is.numeric(temperature) || length(temperature) != 1L || is.na(temperature)) {
          stop("`temperature` must be a single non-missing numeric value.", call. = FALSE)
     }
     if (temperature < 0) {
          stop("`temperature` must be >= 0.", call. = FALSE)
     }

     n <- length(x)
     nm <- names(x)

     # ---- special cases ---------------------------------------------------------
     if (temperature == 0 || diff(range(x)) == 0) {
          w <- rep(1 / n, n)
          if (!is.null(nm)) names(w) <- nm
          if (isTRUE(verbose)) {
               message("Temperature = 0 or all x equal: returning uniform weights.")
          }
          return(w)
     }

     if (is.infinite(temperature)) {
          min_x <- min(x)
          idx   <- which(x == min_x)
          w     <- rep(0, n)
          w[idx] <- 1 / length(idx)
          if (!is.null(nm)) names(w) <- nm
          if (isTRUE(verbose)) {
               message(sprintf(
                    "Temperature = Inf: hard selection on min(x)=%.6g across %d tied model(s).",
                    min_x, length(idx)
               ))
          }
          return(w)
     }

     # ---- stabilized softmax over losses ---------------------------------------
     x_min   <- min(x)
     x_shift <- x - x_min                  # >= 0; preserves normalized weights
     s       <- exp(-temperature * x_shift)  # in (0, 1]; exp(0) = 1 for best model
     z       <- sum(s)
     w       <- s / z

     if (!is.null(nm)) names(w) <- nm

     if (isTRUE(verbose)) {
          rng <- range(x)
          H   <- -sum(ifelse(w > 0, w * log(w), 0))
          eff <- exp(H)  # effective number of models (perplexity)
          message(sprintf("Gibbs weights with temperature = %.6g", temperature))
          message(sprintf("Input range: min = %.6g, max = %.6g, spread = %.6g", rng[1], rng[2], diff(rng)))
          message(sprintf("Stabilization: subtract min(x) = %.6g before exponentiation.", x_min))
          message(sprintf("Entropy = %.4f; effective number of models = %.3f", H, eff))
     }

     w
}
