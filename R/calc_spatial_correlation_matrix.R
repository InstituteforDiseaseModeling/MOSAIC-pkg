#' Build the full spatial–correlation matrix across all locations
#'
#' Given LASER simulation outputs, this function constructs the
#' \eqn{J \times J} matrix
#' \eqn{\mathbf C = [\mathcal{C}_{ij}]} of Pearson
#' spatial–correlation coefficients (Keeling & Rohani 2002) for every pair of
#' locations.
#'
#' All three input objects must be arranged as (locations J × time T)
#' (rows are the J locations; columns are the T time steps).
#'
#' Total infections at time t in location j are taken as
#' \eqn{I_{jt} = I^{(\mathrm{sym})}_{jt} + I^{(\mathrm{asym})}_{jt}}; these
#' are converted to prevalence by dividing by the matching population size
#' \eqn{N_{jt}}.  Pair-wise correlations are then computed with
#' [`calc_spatial_correlation()`].
#'
#' @param I_sym  Numeric matrix (J × T).  Symptomatic infections.
#' @param I_asym Numeric matrix (J × T).  Asymptomatic infections.
#'               Must have the same dimensions as `I_sym`.
#' @param N      Population sizes. Accepts
#'   * a single scalar (same for every j & t),
#'   * a length-J vector (different per location, constant in time), or
#'   * a full J × T matrix (time‑varying populations).
#'
#' @return A J × J symmetric numeric matrix whose \((i,j)\) entry is
#'   \eqn{\mathcal{C}_{ij}}; diagonal elements are set to 1.
#'   Pairs that cannot be computed (all `NA`, zero variance, etc.) are `NA_real_`.
#'
#' @seealso \code{\link{calc_spatial_correlation}}
#' @export
calc_spatial_correlation_matrix <- function(I_sym,
                                            I_asym,
                                            N) {
     # Dimension checks
     if (!is.matrix(I_sym) || !is.matrix(I_asym)) {
          stop("`I_sym` and `I_asym` must both be J×T matrices (locations × time).")
     }
     if (!all(dim(I_sym) == dim(I_asym))) {
          stop("`I_sym` and `I_asym` must have identical dimensions.")
     }

     J <- nrow(I_sym)  # number of locations
     T <- ncol(I_sym)  # number of time steps

     # Conform N to a J×T matrix
     if (length(N) == 1L) {
          N_mat <- matrix(N, nrow = J, ncol = T)
     } else if (is.vector(N) && length(N) == J) {
          N_mat <- matrix(rep(N, times = T), nrow = J, ncol = T)
     } else if (is.matrix(N) && all(dim(N) == c(J, T))) {
          N_mat <- N
     } else {
          stop("`N` must be a scalar, a length-J vector, or a J×T matrix.")
     }

     # Total infections (J×T)
     I_tot <- I_sym + I_asym

     # Prepare result matrix (J×J)
     loc_names <- rownames(I_sym)
     C <- matrix(NA_real_, nrow = J, ncol = J,
                 dimnames = list(loc_names, loc_names))

     # Compute correlations for each pair
     for (i in seq_len(J)) {
          C[i, i] <- 1
          if (i < J) {
               for (j in seq.int(i + 1L, J)) {
                    prev_i <- I_tot[i, ] / N_mat[i, ]
                    prev_j <- I_tot[j, ] / N_mat[j, ]
                    C_ij   <- calc_spatial_correlation(prev_i, prev_j)
                    C[i, j] <- C_ij
                    C[j, i] <- C_ij
               }
          }
     }

     return(C)
}
