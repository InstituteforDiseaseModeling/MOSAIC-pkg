% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_affine_normalization.R
\name{calc_affine_normalization}
\alias{calc_affine_normalization}
\title{Affine Normalization of a Numeric Vector (Zero-Centered Min-Max Scaling)}
\usage{
calc_affine_normalization(x)
}
\arguments{
\item{x}{A numeric vector to be normalized. This vector may contain \code{NA} values.}
}
\value{
A numeric vector of the same length as \code{x} that has been affine normalized.
}
\description{
Transforms a numeric vector by centering it at zero and scaling it so that the
minimum value becomes \code{-1}. Specifically, the affine normalization is defined as:
}
\details{
\deqn{x_{\text{scaled}} = \frac{x - \mu}{\mu - \min(x)}}

where \eqn{\mu} is the mean of \code{x}.

The transformation produces a vector with a mean that is approximately zero (ignoring
\code{NA} values), a minimum of \code{-1}, and a maximum above zero if the original
maximum exceeds the mean.

The function computes the mean and minimum of \code{x} with \code{na.rm = TRUE}.
If the mean equals the minimum (or is nearly equal within machine precision),
the function issues a warning and returns a vector of \code{NA} values to
avoid division by zero.
}
\examples{
# Example vector with NA values
x <- c(3, 5, 7, NA, 10, 12)

# Apply affine normalization
normalized_x <- calc_affine_normalization(x)

# The normalized vector should have a mean of approximately zero,
# a minimum of -1, and a maximum above zero (if the original maximum exceeds the mean).

}
