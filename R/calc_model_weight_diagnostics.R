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
#' @return A list with `A` (agreement index in \[0,1\]), `H` (entropy), and `B_size` (retained set size).
#'
#' @seealso [calc_model_cvw()]
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
#' @seealso [calc_model_agreement_index()]
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
