#' Dispatcher for multiple distribution log-likelihood functions
#'
#' This function routes to the appropriate \code{calc_log_likelihood_*()} sub-function
#' based on the specified \code{family}. It supports the six distributions (Beta, Binomial, Gamma, Negative Binomial, Normal, Poisson)
#' and also allows a top-level \code{weights} argument that is passed down to the chosen sub-function.
#'
#' @param observed Numeric or integer vector of observed data (depends on distribution). NAs allowed.
#' @param estimated Numeric vector of model-predicted values (depends on distribution). NAs allowed.
#' @param family Character string, one of:
#'   \itemize{
#'     \item \code{"beta"}
#'     \item \code{"binomial"}
#'     \item \code{"gamma"}
#'     \item \code{"negbin"}
#'     \item \code{"normal"}
#'     \item \code{"poisson"}
#'   }
#' @param weights Optional numeric vector of non-negative weights, same length as \code{observed}.
#'                Default is \code{NULL}, which sets all weights to 1.
#' @param ... Additional arguments passed to the underlying distribution function:
#'   \itemize{
#'     \item \code{mean_precision} (for \code{\link{calc_log_likelihood_beta}})
#'     \item \code{trials} (for \code{\link{calc_log_likelihood_binomial}})
#'     \item \code{k} (for \code{\link{calc_log_likelihood_negbin}})
#'     \item \code{verbose} (common to all distributions)
#'   }
#' Dispatcher for multiple distribution log-likelihood functions
#' ...
#' @param ... Additional arguments passed to the underlying distribution function:
#'   \itemize{
#'     \item \code{mean_precision} (for \code{\link{calc_log_likelihood_beta}})
#'     \item \code{trials} (for \code{\link{calc_log_likelihood_binomial}})
#'     \item \code{k}, \code{k_min} (for \code{\link{calc_log_likelihood_negbin}})
#'     \item \code{verbose} (common to all distributions)
#'   }
#'
#' @details
#' Based on the value of \code{family}, this function internally calls:
#' \itemize{
#'   \item \code{\link{calc_log_likelihood_beta}} for \code{family = "beta"}
#'   \item \code{\link{calc_log_likelihood_binomial}} for \code{family = "binomial"}
#'   \item \code{\link{calc_log_likelihood_gamma}} for \code{family = "gamma"}
#'   \item \code{\link{calc_log_likelihood_negbin}} for \code{family = "negbin"}
#'   \item \code{\link{calc_log_likelihood_normal}} for \code{family = "normal"}
#'   \item \code{\link{calc_log_likelihood_poisson}} for \code{family = "poisson"}
#' }
#'
#' The \code{weights} argument (if not \code{NULL}) is passed to the chosen sub-likelihood
#' function, which multiplies each observation's log-likelihood contribution by \code{weights[i]}.
#'
#' @return The total log-likelihood (scalar) for most families, or (in the case of Normal)
#'   a list with \code{log_likelihood}, \code{sigma}, \code{shapiro_p}.
#'
#' @seealso
#'   \code{\link{calc_log_likelihood_beta}},
#'   \code{\link{calc_log_likelihood_binomial}},
#'   \code{\link{calc_log_likelihood_gamma}},
#'   \code{\link{calc_log_likelihood_negbin}},
#'   \code{\link{calc_log_likelihood_normal}},
#'   \code{\link{calc_log_likelihood_poisson}}
#'
#' @export
#'
#' @examples
#' # Poisson example with weights
#' calc_log_likelihood(
#'   observed  = c(2, 3, 4),
#'   estimated = c(2.2, 2.9, 3.8),
#'   family    = "poisson",
#'   weights   = c(1, 2, 1),
#'   verbose   = TRUE
#' )
#'
#' # Negative Binomial with known k and weights
#' calc_log_likelihood(
#'   observed  = c(0, 5, 9),
#'   estimated = c(3, 4, 5),
#'   family    = "negbin",
#'   k         = 2.0,
#'   weights   = c(1, 1, 2),
#'   verbose   = TRUE
#' )
#'

calc_log_likelihood <- function(observed, estimated,
                                family = c("beta", "binomial", "gamma",
                                           "negbin", "normal", "poisson"),
                                weights = NULL,
                                ...) {

     family <- match.arg(family)

     switch(
          family,
          "beta" = {
               MOSAIC::calc_log_likelihood_beta(
                    observed  = observed,
                    estimated = estimated,
                    weights   = weights,
                    ...
               )
          },
          "binomial" = {
               MOSAIC::calc_log_likelihood_binomial(
                    observed  = observed,
                    estimated = estimated,
                    weights   = weights,
                    ...
               )
          },
          "gamma" = {
               MOSAIC::calc_log_likelihood_gamma(
                    observed  = observed,
                    estimated = estimated,
                    weights   = weights,
                    ...
               )
          },
          "negbin" = {
               MOSAIC::calc_log_likelihood_negbin(
                    observed  = observed,
                    estimated = estimated,
                    weights   = weights,
                    ...
               )
          },
          "normal" = {
               MOSAIC::calc_log_likelihood_normal(
                    observed  = observed,
                    estimated = estimated,
                    weights   = weights,
                    ...
               )
          },
          "poisson" = {
               MOSAIC::calc_log_likelihood_poisson(
                    observed  = observed,
                    estimated = estimated,
                    weights   = weights,
                    ...
               )
          }
     )
}
