#' Calculate spatial correlation between two locations
#'
#' Computes the Pearson spatial–correlation coefficient
#' \eqn{\mathcal{C}_{ij}} (Keeling & Rohani 2002) for a pair of locations
#' *i* and *j* from **single** infection time-series per location.
#'
#' The prevalence series are
#' \eqn{y_{it} = I_{it} / N_{i,t}} and
#' \eqn{y_{jt} = I_{jt} / N_{j,t}}, where the population sizes
#' \eqn{N_{i,t}} and \eqn{N_{j,t}} may be either **scalars** (constant over
#' time) or **vectors** the same length as the infection series
#' (time-varying).
#'
#' Rows containing `NA` in either prevalence series are removed
#' automatically. If fewer than two paired observations remain, the function
#' returns `NA_real_`.
#' A consistency check stops execution if any computed prevalence exceeds 1.
#'
#' @param I_i Numeric vector. Total infections in location *i* at each
#'   reporting time.
#' @param I_j Numeric vector. Total infections in location *j* at each
#'   reporting time. Must be the same length as `I_i`.
#' @param N_i Numeric **scalar or vector**. Population size(s) of location *i*.
#'   * If a scalar, the value is recycled across all time points.
#'   * If a vector, it must be the same length as the infection series and
#'     supplies time-varying population sizes.
#'   * Default is `1`, implying that infection counts are already expressed
#'     as prevalences.
#' @param N_j Numeric **scalar or vector**. Population size(s) of location *j*.
#'   Recycling and length rules mirror those of `N_i`. Default `1`.
#'
#' @return Numeric scalar in the interval \[-1, 1\] giving the spatial
#'   correlation between the two locations, or `NA_real_` if the calculation is
#'   not possible (e.g. all values `NA`, zero variance).
#'
#' @examples
#' set.seed(1)
#' t  <- 20
#' I_i <- rpois(t,  8)
#' I_j <- rpois(t, 12)
#' N_i <- round(seq(9.8e4, 1.02e5, len = t))   # growing population
#' N_j <- 8e4                                   # constant population
#' calc_spatial_correlation(I_i, I_j, N_i, N_j)
#'
#' @references
#' Keeling M.J., Rohani P. (2002) *Ecology Letters* **5**, 20–29.
#'
#' @export
#'

calc_spatial_correlation <- function(I_i,
                                     I_j,
                                     N_i = 1,
                                     N_j = 1) {

     ## --- consistency checks ------------------------------------------------
     len <- length(I_i)
     if (length(I_j) != len)
          stop("I_i and I_j must have the same length.")

     vec_ok <- function(x) length(x) %in% c(1L, len)
     if (!vec_ok(N_i) || !vec_ok(N_j))
          stop("N_i and N_j must be scalars or vectors the same length as the series.")

     ## --- recycle scalar population sizes ----------------------------------
     if (length(N_i) == 1L) N_i <- rep.int(N_i, len)
     if (length(N_j) == 1L) N_j <- rep.int(N_j, len)

     ## --- prevalence series -------------------------------------------------
     y_i <- I_i / N_i
     y_j <- I_j / N_j

     ## check: prevalence cannot exceed 1
     if (any(y_i[!is.na(y_i)] > 1) || any(y_j[!is.na(y_j)] > 1))
          stop("Prevalence values exceed 1; check counts or population sizes.")

     ## --- remove missing pairs ---------------------------------------------
     keep <- stats::complete.cases(y_i, y_j)
     y_i  <- y_i[keep]
     y_j  <- y_j[keep]
     if (length(y_i) < 2L) return(NA_real_)   # not enough paired data

     ## --- compute spatial correlation --------------------------------------
     mu_i  <- mean(y_i)
     mu_j  <- mean(y_j)

     num   <- sum((y_i - mu_i) * (y_j - mu_j))
     denom <- sqrt(sum((y_i - mu_i)^2) * sum((y_j - mu_j)^2))

     if (denom == 0 || is.na(num) || is.na(denom)) return(NA_real_)
     num / denom
}
