#' Compute Diffusion Matrix Based on Gravity Model
#'
#' Computes a connectivity matrix \eqn{\pi_{ij}} between locations using a normalized gravity model.
#'
#' The normalized gravity model has the following form:
#'
#' \deqn{\pi_{ij} = \frac{N_j^\omega \, d_{ij}^{-\gamma}}{\sum_{j \ne i} N_j^\omega \, d_{ij}^{-\gamma}}, \quad \forall \; i,\, j}
#'
#' where \eqn{d_{ij}} is the distance between locations \eqn{i} and \eqn{j}, and \eqn{N_j} is the population
#' or weight of location \eqn{j}. The diagonal of the output matrix is set to \code{NA}, as \eqn{\pi_{ij}} is intended
#' to be used conditionally alongside the departure probability \eqn{\tau_i}.
#'
#' @param D A square numeric distance matrix between locations. Units are kilometers.
#' @param N A numeric vector of population sizes. Length must match \code{nrow(D)}. May be named.
#' @param omega Numeric exponent for population scaling.
#' @param gamma Numeric exponent for distance decay.
#'
#' @return A numeric matrix of diffusion probabilities \eqn{\pi_{ij}}, with diagonals set to \code{NA}.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' D <- matrix(runif(100, 1, 10), nrow = 10)
#' D <- (D + t(D)) / 2; diag(D) <- 0
#' N <- runif(10, 100, 1000)
#' names(N) <- LETTERS[1:10]
#'
#' pi_mat <- calc_diffusion_matrix_pi(D, N, omega = 0.5, gamma = 2)
#' }
#'
#' @export
#'

calc_diffusion_matrix_pi <- function(D, N, omega, gamma) {

     if (!is.matrix(D) || !is.numeric(D)) stop("D must be a numeric matrix.")
     if (nrow(D) != ncol(D)) stop("D must be a square matrix.")
     if (length(N) != nrow(D)) stop("Length of N must equal the number of rows in D.")
     if (!is.numeric(omega) || length(omega) != 1 || omega <= 0) stop("omega must be a positive numeric scalar.")
     if (!is.numeric(gamma) || length(gamma) != 1 || gamma <= 0) stop("gamma must be a positive numeric scalar.")

     n <- length(N)
     pi_mat <- matrix(NA_real_, nrow = n, ncol = n)
     if (!is.null(names(N))) dimnames(pi_mat) <- list(origin = names(N), destination = names(N))

     for (i in 1:n) {

          num <- numeric(n)

          for (j in 1:n) {

               if (i != j) {

                    num[j] <- (N[j]^omega) * (D[i, j]^(-gamma))

               } else {

                    num[j] <- 0

               }
          }

          denom <- sum(num)

          for (j in 1:n) {

               if (i != j) {

                    pi_mat[i, j] <- num[j] / denom

               } else {

                    pi_mat[i, j] <- NA

               }
          }
     }

     return(pi_mat)
}
