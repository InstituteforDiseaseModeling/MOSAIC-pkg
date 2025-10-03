#' Map infections to suspected and true cholera cases
#'
#' @description
#' Given a vector of incident *true infections* \eqn{I_{\text{new}}(t)} and constants
#' \eqn{\sigma} (symptomatic fraction) and \eqn{\rho} (reporting probability among symptomatic),
#' this function returns vectors of expected **suspected** and **true/confirmed** cases aligned
#' on report day using a simple time shift \eqn{\Delta} (days).
#'
#' By default, the positive predictive value \eqn{\chi} among suspected cases is set to
#' \code{chi_endemic} for all times. If \code{epidemic_threshold} is provided, the function
#' switches \eqn{\chi} between \code{chi_endemic} and \code{chi_epidemic} according to whether
#' \eqn{I_{\text{new}}(t-\Delta)/N(t-\Delta)} exceeds the threshold.
#'
#' @details
#' Shift-only delay model with constant \eqn{\sigma,\rho}:
#' \deqn{\mathbb{E}[C_{\text{true}}(t)] \;=\; \rho\,\sigma\,I_{\text{new}}(t-\Delta),}
#' \deqn{\mathbb{E}[C_{\text{sus}}(t)] \;=\; \frac{\rho\,\sigma}{\chi_t}\,I_{\text{new}}(t-\Delta),}
#' where \eqn{\chi_t \in (0,1]} is the PPV among suspected cases (fraction that are truly cholera).
#'
#' **Epidemic switching rule (optional):**
#' \deqn{\chi_t \;=\; \begin{cases}
#' \text{chi\_epidemic}, & \text{if } I_{\text{new}}(t-\Delta)/N(t-\Delta) > \text{epidemic\_threshold},\\
#' \text{chi\_endemic},  & \text{otherwise.}
#' \end{cases}}
#' If \code{epidemic_threshold} is \code{NULL}, the function uses \code{chi_endemic} for all times
#' (i.e., the switching is bypassed and \code{N} is ignored).
#'
#' When \code{delta_t > 0}, the first \code{delta_t} elements of the outputs are \code{NA}
#' (no earlier infections available to map forward). When \code{delta_t = 0}, outputs are aligned
#' one-to-one with \code{infections} and contain no leading \code{NA}s.
#'
#' @param infections Numeric vector of incident true infections \eqn{I_{\text{new}}(t)} (length \eqn{n}).
#'   Must be non-negative; \code{NA}s are allowed and propagate.
#' @param sigma Scalar in \eqn{[0,1]}: symptomatic fraction.
#' @param rho   Scalar in \eqn{[0,1]}: probability a symptomatic cholera infection is reported as \emph{suspected}.
#' @param chi_endemic  Scalar in \eqn{(0,1]}: PPV among suspected during endemic levels (also the default PPV if
#'   \code{epidemic_threshold} is \code{NULL}).
#' @param chi_epidemic Scalar in \eqn{(0,1]}: PPV among suspected during epidemic levels (used only when
#'   \code{epidemic_threshold} is provided).
#' @param N Numeric scalar or vector of length \eqn{n}: population size used to compute the infection proportion
#'   \eqn{I_{\text{new}}(t)/N(t)} for the switching rule. **Ignored if** \code{epidemic_threshold} is \code{NULL}.
#' @param epidemic_threshold \code{NULL} or a scalar in \eqn{[0,1]}: threshold on \eqn{I_{\text{new}}(t)/N(t)} that
#'   determines whether \code{chi_epidemic} (above threshold) or \code{chi_endemic} (otherwise) is used.
#'   If \code{NULL}, no switching is applied and \code{chi_endemic} is used for all times.
#' @param delta_t Non-negative integer number of days for the infection→report delay \eqn{\Delta}.
#'
#' @return A named list with two numeric vectors, each length \eqn{n}:
#' \itemize{
#'   \item \code{cases_suspected}: expected suspected cholera cases \eqn{C_{\text{sus}}(t)}.
#'   \item \code{cases_true}: expected true/confirmed cholera cases \eqn{C_{\text{true}}(t)}.
#' }
#'
#' @section Alignment note:
#' Outputs are indexed by report day \eqn{t}. Values at indices \eqn{1:\Delta} are \code{NA}
#' because they depend on \eqn{I_{\text{new}}(t-\Delta)} before the start of the series.
#'
#' @examples
#' infections <- c(0, 1, 2, 5, 40, 120, 200, 75, 50, 30, 15, 10, 3, 0, 0)
#' N <- 20000
#'
#' # 1) No switching: epidemic_threshold = NULL (chi_endemic used everywhere; N ignored)
#' out1 <- calc_cases_from_infections(
#'   infections = infections,
#'   sigma = 0.25,
#'   rho = 0.70,
#'   chi_endemic = 0.50,
#'   chi_epidemic = 0.75,
#'   N = NULL,
#'   epidemic_threshold = NULL,
#'   delta_t = 0
#' )
#' out1$cases_suspected; out1$cases_true
#'
#' # 2) Switching on: use chi_epidemic when infections/N > 15/100,000
#' out2 <- calc_cases_from_infections(
#'   infections = infections,
#'   sigma = 0.25,
#'   rho = 0.70,
#'   chi_endemic = 0.50,
#'   chi_epidemic = 0.75,
#'   N = N,
#'   epidemic_threshold = 15/100000,
#'   delta_t = 2
#' )
#' out2$cases_suspected; out2$cases_true
#'
#' @export
calc_cases_from_infections <- function(infections,
                                       sigma,
                                       rho,
                                       chi_endemic,
                                       chi_epidemic,
                                       N = NULL,
                                       epidemic_threshold = NULL,
                                       delta_t) {
     # ---- input checks ----
     if (!is.numeric(infections) || !is.vector(infections)) {
          stop("`infections` must be a numeric vector.", call. = FALSE)
     }
     n <- length(infections)

     if (!is.numeric(sigma) || length(sigma) != 1L || is.na(sigma) || sigma < 0 || sigma > 1) {
          stop("`sigma` must be a scalar in [0, 1].", call. = FALSE)
     }
     if (!is.numeric(rho) || length(rho) != 1L || is.na(rho) || rho < 0 || rho > 1) {
          stop("`rho` must be a scalar in [0, 1].", call. = FALSE)
     }

     if (!is.numeric(chi_endemic)  || length(chi_endemic)  != 1L || is.na(chi_endemic)  ||
         chi_endemic  <= 0 || chi_endemic  > 1) {
          stop("`chi_endemic` must be a scalar in (0, 1].", call. = FALSE)
     }
     if (!is.numeric(chi_epidemic) || length(chi_epidemic) != 1L || is.na(chi_epidemic) ||
         chi_epidemic <= 0 || chi_epidemic > 1) {
          stop("`chi_epidemic` must be a scalar in (0, 1].", call. = FALSE)
     }

     if (!is.null(epidemic_threshold)) {
          if (!is.numeric(epidemic_threshold) || length(epidemic_threshold) != 1L ||
              is.na(epidemic_threshold) || epidemic_threshold < 0 || epidemic_threshold > 1) {
               stop("`epidemic_threshold` must be NULL or a scalar in [0, 1].", call. = FALSE)
          }
          # Only validate N when thresholding is requested
          if (is.null(N)) {
               stop("`N` must be provided (scalar or vector) when `epidemic_threshold` is not NULL.", call. = FALSE)
          }
          if (!is.numeric(N) || length(N) < 1L) {
               stop("`N` must be a numeric scalar or a numeric vector of length length(infections).", call. = FALSE)
          }
          if (length(N) == 1L) {
               N_vec <- rep(N, n)
          } else {
               if (length(N) != n) {
                    stop("When `N` is a vector, it must have the same length as `infections`.", call. = FALSE)
               }
               N_vec <- N
          }
          if (any(!is.finite(N_vec)) || any(N_vec <= 0, na.rm = TRUE)) {
               stop("All `N` values must be finite and > 0.", call. = FALSE)
          }
     } else {
          N_vec <- NULL  # ignored
     }

     if (!is.numeric(delta_t) || length(delta_t) != 1L || is.na(delta_t) ||
         delta_t < 0 || delta_t != floor(delta_t)) {
          stop("`delta_t` must be a non-negative integer.", call. = FALSE)
     }

     # ---- allocate outputs ----
     cases_true      <- rep(NA_real_, n)
     cases_suspected <- rep(NA_real_, n)

     # ---- map infections at (t - delta_t) to reports at t ----
     k <- as.integer(delta_t)
     if (k < n) {
          idx_report <- seq.int(from = k + 1L, to = n)
          idx_infect <- seq.int(from = 1L,      to = n - k)

          # Determine chi to use
          if (is.null(epidemic_threshold)) {
               # No switching: use chi_endemic for all report days
               chi_use <- rep(chi_endemic, length(idx_report))
          } else {
               # Switching based on infection-day intensity
               inf_prop <- infections[idx_infect] / N_vec[idx_infect]
               epidemic_flag <- inf_prop > epidemic_threshold
               chi_use <- ifelse(epidemic_flag, chi_epidemic, chi_endemic)  # length n - k
          }

          # true/confirmed cases: C_true(t) = rho * sigma * I_new(t - delta_t)
          cases_true[idx_report] <- rho * sigma * infections[idx_infect]

          # suspected cases: C_sus(t) = (rho * sigma / chi_use) * I_new(t - delta_t)
          cases_suspected[idx_report] <- (rho * sigma / chi_use) * infections[idx_infect]
     }
     # else: all NAs (no infections available before start)

     list(
          cases_suspected = cases_suspected,
          cases_true = cases_true
     )
}
