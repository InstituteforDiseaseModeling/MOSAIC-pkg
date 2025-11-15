#' Calculate the Composite Loss of a Model
#'
#' @description
#' Computes a simple composite **loss** for model fit defined as
#' \deqn{
#' \mathcal{L} \;=\; \alpha \cdot \mathrm{MSE}(y, \hat{y}) \;-\; \beta \cdot \mathrm{Corr}(y, \hat{y}),
#' }
#' where \eqn{\mathrm{MSE}} is the (optionally weighted) mean squared error and
#' \eqn{\mathrm{Corr}} is the (optionally weighted) **Pearson correlation**.
#' Lower values indicate better fit (since large positive correlation reduces the loss).
#'
#' @details
#' - Inputs may be vectors or matrices (e.g., locations \eqn{\times} time). They are
#'   flattened and compared **pairwise** with optional removal of non-finite values.
#' - MSE and correlation are computed by delegating to [calc_model_mse()] and
#'   [calc_model_cor()] (with \code{method = "pearson"}), using identical filtering
#'   options so valid pairs are consistent for both components.
#' - If either component is \code{NA} (e.g., no valid pairs or zero variance), the
#'   loss is returned as \code{NA_real_}.
#'
#' @param observed A numeric vector or matrix of observed values.
#' @param estimated A numeric vector or matrix of model-estimated values of the
#'   same length (after flattening) as \code{observed}.
#' @param alpha Non-negative scalar weight for the MSE term. Default \code{1}.
#' @param beta Non-negative scalar weight for the correlation term. Default \code{1}.
#' @param na_rm Logical; if \code{TRUE} (default), drop any pair with \code{NA}.
#' @param finite_only Logical; if \code{TRUE} (default), also drop non-finite
#'   values (\code{Inf}, \code{-Inf}, \code{NaN}) pairwise.
#' @param weights Optional numeric vector of non-negative weights (recycled if
#'   length 1). Used for both MSE and Pearson correlation to keep consistency.
#'   If \code{NULL} (default), all valid pairs receive equal weight.
#' @param verbose Logical; if \code{TRUE}, emit brief diagnostic messages.
#'
#' @return A single numeric value: \eqn{\alpha \cdot \mathrm{MSE} - \beta \cdot \mathrm{Corr}}.
#'   Returns \code{NA_real_} if either component cannot be computed.
#'
#' @section Interpretation:
#' \itemize{
#'   \item \strong{Lower is better}. High MSE increases the loss; high positive correlation
#'         decreases the loss. Negative correlation increases the loss.
#'   \item Units depend on the scale of \code{observed}/\code{estimated} (via MSE) and the
#'         dimensionless correlation term. Choose \code{alpha}, \code{beta} to balance scales.
#' }
#'
#' @seealso [calc_model_mse()], [calc_model_cor()], [calc_model_likelihood()]
#'
#' @examples
#' set.seed(3)
#' y  <- rpois(50, 10)
#' yh <- y + rnorm(50, 0, 2)
#'
#' # Default: alpha = 1, beta = 1
#' calc_model_loss(y, yh)
#'
#' # Emphasize correlation relative to MSE
#' calc_model_loss(y, yh, alpha = 0.5, beta = 2)
#'
#' # Matrix example (locations x time) with recency weights
#' Y  <- matrix(rpois(60, 5), nrow = 3)     # 3 locations x 20 weeks
#' Yh <- Y + matrix(rnorm(60, 0, 1), nrow = 3)
#' w  <- rep(seq(0.5, 1.5, length.out = ncol(Y)), each = nrow(Y))
#' calc_model_loss(Y, Yh, weights = w)
#'
#' @export
#'

calc_model_loss <- function(observed,
                            estimated,
                            alpha = 1,
                            beta  = 1,
                            na_rm = TRUE,
                            finite_only = TRUE,
                            weights = NULL,
                            verbose = FALSE) {

     # ---------------------------------------------------------------------------
     # Validate weights and scalars
     # ---------------------------------------------------------------------------
     if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0) {
          stop("calc_model_loss: 'alpha' must be a non-negative finite scalar.")
     }
     if (!is.numeric(beta) || length(beta) != 1L || !is.finite(beta) || beta < 0) {
          stop("calc_model_loss: 'beta' must be a non-negative finite scalar.")
     }

     # ---------------------------------------------------------------------------
     # Compute components
     # ---------------------------------------------------------------------------
     mse_val <- calc_model_mse(
          observed  = observed,
          estimated = estimated,
          na_rm = na_rm,
          finite_only = finite_only,
          weights = weights,
          verbose = verbose
     )

     cor_val <- calc_model_cor(
          observed  = observed,
          estimated = estimated,
          method = "pearson",
          na_rm = na_rm,
          finite_only = finite_only,
          weights = weights,
          verbose = verbose
     )

     if (!is.finite(mse_val) || !is.finite(cor_val)) {
          if (verbose) message("calc_model_loss: returning NA (component is NA or non-finite).")
          return(NA_real_)
     }

     # ---------------------------------------------------------------------------
     # Composite loss
     # ---------------------------------------------------------------------------
     loss <- alpha * mse_val - beta * cor_val
     loss
}

