#' Mean Squared Error Between Observed and Estimated Series
#'
#' @description
#' Computes the **mean squared error (MSE)** between an observed series and a
#' model-estimated series. Inputs may be numeric vectors or matrices (e.g.,
#' \eqn{L \times T} location-by-time arrays); values are flattened and compared
#' **pairwise** with optional weights.
#'
#' @details
#' Let \eqn{\{(y_i, \hat{y}_i)\}_{i=1}^{n}} be the set of **valid pairs** after
#' removing any entries with \code{NA}/\code{Inf} (when \code{na_rm = TRUE} and
#' \code{finite_only = TRUE}). The weighted MSE is
#' \deqn{
#' \mathrm{MSE}_w \;=\; \frac{\sum_{i=1}^{n} w_i \, ( \hat{y}_i - y_i )^2}{\sum_{i=1}^{n} w_i},
#' }
#' which reduces to the standard unweighted MSE when \eqn{w_i \equiv 1}.
#'
#' @param observed A numeric vector or matrix of observed values.
#' @param estimated A numeric vector or matrix of model-estimated values
#'   of the same length (after flattening) as \code{observed}.
#' @param na_rm Logical; if \code{TRUE} (default), remove any pair with
#'   \code{NA}. If \code{FALSE} and any \code{NA}/\code{Inf} are found (or lengths
#'   mismatch), returns \code{NA_real_}.
#' @param finite_only Logical; if \code{TRUE} (default), also drop non-finite
#'   values (\code{Inf}, \code{-Inf}, \code{NaN}) during pairwise filtering.
#' @param weights Optional numeric vector of non-negative weights (recycled if
#'   length 1). Must align with the number of **valid pairs** after filtering.
#'   If \code{NULL} (default), all pairs receive equal weight.
#' @param verbose Logical; if \code{TRUE}, emit brief diagnostic messages.
#'
#' @return A single numeric value: the mean squared error. Returns \code{NA_real_}
#'   if fewer than one valid pair remains or if total weight is zero.
#'
#' @section Input shapes:
#' Vectors and matrices are supported. Matrices are flattened (column-major)
#' in both \code{observed} and \code{estimated} before pairwise comparison.
#'
#' @seealso [calc_model_cor()], [calc_model_likelihood()]
#'
#' @examples
#' # Vector example
#' set.seed(1)
#' y  <- rpois(20, 10)
#' yh <- y + rnorm(20, 0, 2)
#' calc_model_mse(y, yh)
#'
#' # Matrix example (locations x time)
#' Y  <- matrix(rpois(30, 5), nrow = 3)
#' Yh <- Y + matrix(rnorm(30, 0, 1), nrow = 3)
#' calc_model_mse(Y, Yh)
#'
#' # With weights (e.g., emphasize recent time points)
#' w <- rep(seq(0.5, 1.5, length.out = ncol(Y)), each = nrow(Y))
#' calc_model_mse(Y, Yh, weights = w)
#'
#' @export
#'

calc_model_mse <- function(observed,
                           estimated,
                           na_rm = TRUE,
                           finite_only = TRUE,
                           weights = NULL,
                           verbose = FALSE) {

     # ---------------------------------------------------------------------------
     # Flatten & basic checks
     # ---------------------------------------------------------------------------
     x <- as.numeric(observed)
     y <- as.numeric(estimated)

     if (length(x) != length(y)) {
          if (verbose) message("calc_model_mse: length mismatch after flattening.")
          return(NA_real_)
     }

     # Pairwise validity filter
     valid <- is.na(x) | is.na(y)
     if (!na_rm && any(valid)) {
          if (verbose) message("calc_model_mse: NA present and na_rm = FALSE -> NA.")
          return(NA_real_)
     }

     valid <- !(is.na(x) | is.na(y))
     if (finite_only) {
          valid <- valid & is.finite(x) & is.finite(y)
     }

     if (!any(valid)) {
          if (verbose) message("calc_model_mse: no valid pairs after filtering.")
          return(NA_real_)
     }

     x <- x[valid]
     y <- y[valid]

     # Weights
     if (!is.null(weights)) {
          w <- as.numeric(weights)
          if (length(w) == 1L) {
               w <- rep(w, length(x))
          }
          if (length(w) != length(x)) {
               if (verbose) message("calc_model_mse: weights length does not match valid pairs.")
               return(NA_real_)
          }
          if (any(!is.finite(w)) || any(w < 0)) {
               if (verbose) message("calc_model_mse: weights must be finite and non-negative.")
               return(NA_real_)
          }
     } else {
          w <- rep(1, length(x))
     }

     sw <- sum(w)
     if (sw <= 0) {
          if (verbose) message("calc_model_mse: sum of weights is zero.")
          return(NA_real_)
     }

     # ---------------------------------------------------------------------------
     # Compute weighted MSE
     # ---------------------------------------------------------------------------
     e2 <- (y - x)^2
     mse <- sum(w * e2) / sw

     mse
}
