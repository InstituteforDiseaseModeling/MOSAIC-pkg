% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_log_likelihood_distributions.R
\name{calc_log_likelihood_gamma}
\alias{calc_log_likelihood_gamma}
\title{Calculate log-likelihood for Gamma-distributed data}
\usage{
calc_log_likelihood_gamma(observed, estimated, weights = NULL, verbose = TRUE)
}
\arguments{
\item{observed}{Numeric vector of observed positive values.}

\item{estimated}{Numeric vector of expected means from the model (same length as \code{observed}).}

\item{weights}{Optional numeric vector of non-negative weights, same length as \code{observed}.
Default is \code{NULL}, which sets all weights to 1.}

\item{verbose}{Logical; if \code{TRUE}, prints estimated shape parameter and total log-likelihood.}
}
\value{
A scalar representing the total log-likelihood (numeric).
}
\description{
Computes the total log-likelihood for continuous positive data under the Gamma distribution.
The shape parameter is estimated via the method of moments. Each observation can be weighted.
}
\details{
Weighted log-likelihood is summed over each observation. The shape \eqn{\\alpha} is estimated
via method of moments from \code{observed}.
}
\examples{
calc_log_likelihood_gamma(c(2.5, 3.2, 1.8), c(2.4, 3.0, 2.0))
}
