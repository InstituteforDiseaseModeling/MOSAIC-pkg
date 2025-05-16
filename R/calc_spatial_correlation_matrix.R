#' Build the full spatial–correlation matrix across all locations
#'
#' Given LASER simulation outputs, this function constructs the
#' \eqn{L \times L} matrix
#' \eqn{\mathbf C = \bigl[\mathcal{C}_{ij}\bigr]} of Pearson
#' spatial–correlation coefficients (Keeling & Rohani 2002) for every pair of
#' locations.
#'
#' *All three input objects must be arranged as* `[ time steps , locations ]`
#' (rows are the *T* simulation steps; columns are the *L* locations).
#'
#' Total infections at time *t* in location *\ell* are taken as
#' \eqn{I_{t\ell}=I^{(\mathrm{sym})}_{t\ell}+I^{(\mathrm{asym})}_{t\ell}}; these
#' are converted to prevalence by dividing by the matching population size
#' \eqn{N_{t\ell}}.  Pair-wise correlations are then computed with
#' [`calc_spatial_correlation()`].
#'
#' @param I_sym  Numeric **matrix** (*T × L*).  Symptomatic infections.
#' @param I_asym Numeric **matrix** (*T × L*).  Asymptomatic infections.
#'               Must have identical dimensions to `I_sym`.
#' @param N      Population sizes. Accepts
#'   * a single scalar (same for every row & column),
#'   * a length-*L* vector (different per location, constant in time), **or**
#'   * a full *T × L* matrix (time-varying populations).
#'
#' @return An *L × L* symmetric numeric matrix whose \((i,j)\) entry is
#'   \eqn{\mathcal{C}_{ij}}; diagonal elements are set to 1.  Pairs that cannot
#'   be computed (all `NA`, zero variance, etc.) are returned as `NA_real_`.
#'
#' @examples
#' # Toy LASER-style arrays: 10 time steps, 3 locations
#' I_sym  <- matrix(c(1,1,0,0,1,1,1,2,2,1,
#'                    0,0,0,0,0,0,0,0,0,0,
#'                    0,0,0,0,0,0,0,0,0,0),
#'                  nrow = 10, byrow = FALSE)
#' I_asym <- 0.3 * I_sym                      # 30 % asymptomatic
#' N      <- c(100, 120, 80)                  # constant by location
#'
#' calc_spatial_correlation_matrix(I_sym, I_asym, N)
#'
#' @seealso \code{\link{calc_spatial_correlation}}
#' @export
calc_spatial_correlation_matrix <- function(I_sym,
                                            I_asym,
                                            N) {

     ## -------- dimension checks ---------------------------------------------
     if (!is.matrix(I_sym) || !is.matrix(I_asym))
          stop("I_sym and I_asym must both be matrices (time steps × locations).")
     if (!all(dim(I_sym) == dim(I_asym)))
          stop("I_sym and I_asym must have identical dimensions.")

     T_ <- nrow(I_sym)           # number of time steps
     L  <- ncol(I_sym)           # number of locations

     ## -------- conform N to a T × L matrix ----------------------------------
     if (length(N) == 1L) {
          N_mat <- matrix(N, nrow = T_, ncol = L)
     } else if (is.vector(N) && length(N) == L) {
          N_mat <- matrix(rep(N, each = T_), nrow = T_)
     } else if (is.matrix(N) && all(dim(N) == c(T_, L))) {
          N_mat <- N
     } else {
          stop("N must be a scalar, a length-L vector, or a T×L matrix.")
     }

     ## -------- total infections ---------------------------------------------
     I_tot <- I_sym + I_asym      # T × L matrix of totals

     ## -------- allocate result matrix ---------------------------------------
     C <- matrix(NA_real_, nrow = L, ncol = L,
                 dimnames = list(colnames(I_sym), colnames(I_sym)))

     ## -------- compute correlations for each pair ---------------------------
     for (i in seq_len(L)) {
          C[i, i] <- 1                         # diagonal
          if (i < L) {
               for (j in seq.int(i + 1L, L)) {
                    C_ij <- calc_spatial_correlation(I_tot[, i],
                                                     I_tot[, j],
                                                     N_mat[, i],
                                                     N_mat[, j])
                    C[i, j] <- C_ij
                    C[j, i] <- C_ij                  # symmetry
               }
          }
     }

     C
}
