#' Pearson (or Rank) Correlation Between Observed and Estimated Series
#'
#' @description
#' Computes the correlation between an observed series and a model-estimated
#' series. By default this is **Pearson's correlation**. Inputs may be numeric
#' vectors or matrices; values are flattened and compared **pairwise**. An
#' optional set of non-negative weights can be supplied for a **weighted Pearson
#' correlation**.
#'
#' @details
#' After removing invalid pairs (see \code{na_rm} and \code{finite_only}), the
#' **weighted Pearson correlation** is:
#' \deqn{
#' r_w \;=\; \frac{\sum_i w_i (x_i - \bar{x}_w) (y_i - \bar{y}_w)}{
#'            \sqrt{\sum_i w_i (x_i - \bar{x}_w)^2}\;
#'            \sqrt{\sum_i w_i (y_i - \bar{y}_w)^2}},
#' }
#' where \eqn{\bar{x}_w = \frac{\sum_i w_i x_i}{\sum_i w_i}} and similarly for
#' \eqn{\bar{y}_w}. If \code{weights = NULL}, the standard (unweighted) Pearson
#' correlation is returned. For \code{method = "spearman"} or \code{"kendall"},
#' any provided \code{weights} are **ignored**.
#'
#' @param observed A numeric vector or matrix of observed values.
#' @param estimated A numeric vector or matrix of model-estimated values
#'   of the same length (after flattening) as \code{observed}.
#' @param method Correlation type. One of \code{"pearson"} (default),
#'   \code{"spearman"}, or \code{"kendall"}.
#' @param na_rm Logical; if \code{TRUE} (default), remove any pair with
#'   \code{NA}. If \code{FALSE} and any \code{NA}/\code{Inf} are found (or lengths
#'   mismatch), returns \code{NA_real_}.
#' @param finite_only Logical; if \code{TRUE} (default), also drop non-finite
#'   values (\code{Inf}, \code{-Inf}, \code{NaN}) during pairwise filtering.
#' @param weights Optional numeric vector of non-negative weights (recycled if
#'   length 1). Only used when \code{method = "pearson"}; ignored otherwise.
#' @param verbose Logical; if \code{TRUE}, emit brief diagnostic messages.
#'
#' @return A single numeric value: the correlation coefficient for the chosen
#'   \code{method}. Returns \code{NA_real_} if fewer than two valid pairs remain
#'   or if a variance term is zero.
#'
#' @section Input shapes:
#' Vectors and matrices are supported. Matrices are flattened (column-major)
#' in both \code{observed} and \code{estimated} before pairwise comparison.
#'
#' @seealso [calc_model_mse()], [calc_model_likelihood()], [calc_model_ppc()]
#'
#' @examples
#' # Vector example
#' set.seed(2)
#' y  <- rpois(20, 10)
#' yh <- y + rnorm(20, 0, 2)
#' calc_model_cor(y, yh)                   # Pearson (default)
#' calc_model_cor(y, yh, method = "spearman")
#'
#' # Matrix example
#' Y  <- matrix(rpois(30, 5), nrow = 3)
#' Yh <- Y + matrix(rnorm(30, 0, 1), nrow = 3)
#' calc_model_cor(Y, Yh)
#'
#' # Weighted Pearson (e.g., emphasize recent weeks)
#' w <- rep(seq(0.5, 1.5, length.out = ncol(Y)), each = nrow(Y))
#' calc_model_cor(Y, Yh, weights = w)
#'
#' @export
calc_model_cor <- function(observed,
                           estimated,
                           method = c("pearson", "spearman", "kendall"),
                           na_rm = TRUE,
                           finite_only = TRUE,
                           weights = NULL,
                           verbose = FALSE) {

     method <- match.arg(method)

     # ---------------------------------------------------------------------------
     # Flatten & basic checks
     # ---------------------------------------------------------------------------
     x <- as.numeric(observed)
     y <- as.numeric(estimated)

     if (length(x) != length(y)) {
          if (verbose) message("calc_model_cor: length mismatch after flattening.")
          return(NA_real_)
     }

     # Pairwise validity filter
     if (!na_rm && (any(is.na(x)) || any(is.na(y)))) {
          if (verbose) message("calc_model_cor: NA present and na_rm = FALSE -> NA.")
          return(NA_real_)
     }

     valid <- !(is.na(x) | is.na(y))
     if (finite_only) {
          valid <- valid & is.finite(x) & is.finite(y)
     }

     if (sum(valid) < 2L) {
          if (verbose) message("calc_model_cor: fewer than two valid pairs.")
          return(NA_real_)
     }

     x <- x[valid]
     y <- y[valid]

     # ---------------------------------------------------------------------------
     # Rank-based correlations (weights ignored)
     # ---------------------------------------------------------------------------
     if (method != "pearson") {
          if (!is.null(weights) && verbose) {
               message("calc_model_cor: weights are ignored for method = '", method, "'.")
          }
          return(stats::cor(x, y, method = method))
     }

     # ---------------------------------------------------------------------------
     # Pearson correlation (weighted or unweighted)
     # ---------------------------------------------------------------------------
     if (is.null(weights)) {
          # Unweighted Pearson
          r <- suppressWarnings(stats::cor(x, y, method = "pearson"))
          if (!is.finite(r)) r <- NA_real_
          return(r)
     }

     # Weighted Pearson
     w <- as.numeric(weights)
     if (length(w) == 1L) {
          w <- rep(w, length(x))
     }
     if (length(w) != length(x)) {
          if (verbose) message("calc_model_cor: weights length does not match valid pairs.")
          return(NA_real_)
     }
     if (any(!is.finite(w)) || any(w < 0)) {
          if (verbose) message("calc_model_cor: weights must be finite and non-negative.")
          return(NA_real_)
     }

     sw <- sum(w)
     if (sw <= 0) {
          if (verbose) message("calc_model_cor: sum of weights is zero.")
          return(NA_real_)
     }

     mx <- sum(w * x) / sw
     my <- sum(w * y) / sw

     vx <- sum(w * (x - mx)^2) / sw
     vy <- sum(w * (y - my)^2) / sw
     if (vx <= 0 || vy <= 0) {
          if (verbose) message("calc_model_cor: zero variance encountered.")
          return(NA_real_)
     }

     cxy <- sum(w * (x - mx) * (y - my)) / sw
     r <- cxy / sqrt(vx * vy)
     # clamp slight numeric excursions
     r <- max(min(r, 1), -1)
     r
}
