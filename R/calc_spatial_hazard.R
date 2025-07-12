#' Calculate spatial importation hazard
#'
#' @description
#' Computes the **daily probability that at least one new infection is introduced**
#' into each destination location *j* on day *t* using a gravity‐style coupling of
#' infectious prevalence across all locations.  The formulation is adapted from
#' Bjørnstad & Grenfell (2008) but (i) folds the gravity weights directly into
#' the prevalence term instead of using a separate coupling constant, and (ii)
#' allows for under‑reporting through the location‐specific parameter `tau`.
#'
#' Concretely, for time *t* and destination *j* the hazard is
#' \deqn{\mathcal H_{jt} \,=\, \frac{\beta_{jt}\,S^{\*}_{jt}
#'        \bigl[1-\exp\{-(S^{\*}_{jt}/N_{jt})\,\bar y_{jt}\}\bigr]}
#'       {1+\beta_{jt}\,S^{\*}_{jt}},}
#' where
#' * \eqn{S^{\*}_{jt} = (1-\tau_j)\bigl(S_{jt}+V^{\mathrm{sus}}_{1,jt}+V^{\mathrm{sus}}_{2,jt}\bigr)} is the locally susceptible pool (adjusted for under‑reporting when `tau<1`).
#' * \eqn{\bar y_{jt}} is the **gravity‑weighted prevalence of infection** in the
#'   entire metapopulation (including the destination):
#'   \deqn{\bar y_{jt} = \frac{ (1-\tau_j)(I_{1,jt}+I_{2,jt}) +
#'         \sum_{i\neq j}\tau_i\,\pi_{ij}\,(I_{1,it}+I_{2,it}) }
#'        {\sum_{k} N_{kt} }.}
#'
#' Note: This specification differs from the two‑stage form in the original
#' paper but has been used in later work where the mobility matrix itself
#' carries the full coupling strength.
#'
#' @param beta Numeric **T × J** matrix. Human force‑of‑infection parameter
#'   \eqn{\beta_{jt}} for each time step (*rows*) and location (*columns*).
#' @param tau Numeric vector of length **J**. Reporting proportion
#'   \eqn{\tau_j \in (0,1]} for each location.  A value of 1 means perfect
#'   reporting; smaller values down‑weight local susceptibles and up‑weight
#'   non‑local infections.
#' @param pie Numeric **J × J** matrix of gravity‑model mobility weights
#'   \eqn{\pi_{ij}} from origin *i* to destination *j*.  Diagonal entries are
#'   ignored.
#' @param N Numeric **T × J** matrix of total population \eqn{N_{jt}}.
#' @param S Numeric **T × J** matrix of fully susceptible individuals
#'   \eqn{S_{jt}}.
#' @param V1_sus, V2_sus Numeric **T × J** matrices of vaccine‑derived
#'   susceptibles after one and two doses, respectively.
#' @param I1, I2 Numeric **T × J** matrices of symptomatic and asymptomatic
#'   infectives.
#' @param time_names Optional character vector of length **T** for output row
#'   names.
#' @param location_names Optional character vector of length **J** for output
#'   column names.
#'
#' @return A numeric **T × J** matrix `H` with elements in \[0,1\] giving the
#'   spatial importation hazard for each time–location pair.
#'
#' @details
#' *Diagonal of* `pie` *is set to zero internally.*  All matrices must have
#' matching dimensions; dimension checks throw informative errors.
#'
#' @section Warning:
#' The current formulation intentionally **includes local infectious
#' individuals** in `\bar y_{jt}`.  If you wish to model *importation only*,
#' set the local contribution to zero in the numerator.
#'
#' @references
#' * Bjørnstad, O. N., & Grenfell, B. T. (2008). *Hazards, spatial transmission
#'   and timing of outbreaks in epidemic metapopulations.* Journal of Theoretical
#'   Ecology, 1, 145‑153.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' T_steps <- 10; J <- 5
#' beta <- matrix(runif(T_steps * J), nrow = T_steps, ncol = J)
#' tau <- runif(J)
#' pie <- matrix(runif(J^2), J, J); diag(pie) <- 0
#' N <- matrix(sample(200:400, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
#' S <- matrix(sample(100:200, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
#' V1_sus <- matrix(sample(0:50, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
#' V2_sus <- matrix(sample(0:50, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
#' I1 <- matrix(sample(0:10, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
#' I2 <- matrix(sample(0:10, T_steps * J, replace = TRUE), nrow = T_steps, ncol = J)
#'
#' H <- calc_spatial_hazard(beta, tau, pie, N, S, V1_sus, V2_sus, I1, I2,
#'                          time_names = paste0("day_", 1:T_steps),
#'                          location_names = paste0("loc_", 1:J))
#'
#' plot_spatial_hazard(H)
#' }
#'
#' @export
#'

calc_spatial_hazard <- function(
          beta,
          tau,
          pie,
          N,
          S,
          V1_sus,
          V2_sus,
          I1,
          I2,
          time_names = NULL,
          location_names = NULL
) {

     # Validate dimensions
     if (!is.matrix(S)) stop("`S` must be a matrix T x J.")
     Tt <- ncol(S); J <- nrow(S)

     # Validate name args
     if (!is.null(time_names) && length(time_names) != Tt) stop("`time_names` must have length T.")
     if (!is.null(location_names) && length(location_names) != J) stop("`location_names` must have length J.")

     # Validate other inputs
     if (!is.matrix(beta)    || any(dim(beta)    != c(J, Tt))) stop("`beta` must be a matrix T x J.")
     if (!is.numeric(tau)    || length(tau)      != J)        stop("`tau` must be a vector of length J.")
     if (!is.matrix(pie)     || any(dim(pie)     != c(J, J))) stop("`pie` must be a matrix J x J.")
     if (!is.matrix(N)       || any(dim(N)       != c(J, Tt))) stop("`N` must be a matrix T x J.")
     if (!is.matrix(V1_sus)  || any(dim(V1_sus)  != c(J, Tt))) stop("`V1_sus` must be a matrix T x J.")
     if (!is.matrix(V2_sus)  || any(dim(V2_sus)  != c(J, Tt))) stop("`V2_sus` must be a matrix T x J.")
     if (!is.matrix(I1)      || any(dim(I1)      != c(J, Tt))) stop("`I1` must be a matrix T x J.")
     if (!is.matrix(I2)      || any(dim(I2)      != c(J, Tt))) stop("`I2` must be a matrix T x J.")

     # Remove self-mobility
     if (sum(diag(pie)) != 0) diag(pie) <- 0

     H <- matrix(NA_real_, ncol = Tt, nrow = J)

     # Loop over times and destinations

     for (j in seq_len(J)) {
          for (t in seq_len(Tt)) {

               # Total local susceptibles including waned vaccinated
               sus_j <- (1 - tau[j]) * (S[j, t] + V1_sus[j, t] + V2_sus[j, t])

               # Probability of local susceptible
               x_j <- sus_j / N[j, t]

               # Total local and non-local infected
               tot_inf <- ((1 - tau[j]) * (I1[j, t] + I2[j, t]))
               for (i in seq_len(J)) {
                    if (i != j) tot_inf <- tot_inf + (tau[i] * pie[i, j] * (I1[i, t] + I2[i, t]))
               }

               # Total local and non-local population size
               tot_pop <- sum(N[,t])

               # Probability of local and non-local infectious from all origins
               y_bar <- tot_inf / tot_pop

               # Spatial hazard from Bjornstad & Grenfell 2008
               H[j, t] <- ( beta[j, t] * sus_j * (1 - exp(-x_j * y_bar)) ) / ( 1 + beta[j, t] * sus_j )

          }
     }

     if (any(is.na(H))) warning('Could not compute spatial hazard for some observations')
     if (any(H < 0 | H > 1, na.rm = TRUE)) warning('Some values of spatial hazard are out of bounds')

     # Assign names if provided
     if (!is.null(time_names)) rownames(H) <- time_names
     if (!is.null(location_names)) colnames(H) <- location_names

     return(H)
}

