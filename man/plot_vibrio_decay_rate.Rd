% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_vibrio_decay_rate.R
\name{plot_vibrio_decay_rate}
\alias{plot_vibrio_decay_rate}
\title{Plot Vibrio Decay Rate as a Function of Environmental Suitability}
\usage{
plot_vibrio_decay_rate(PATHS, decay_days_short = 3, decay_days_long = 90)
}
\arguments{
\item{PATHS}{A list containing path locations for saving output. Must include:
\itemize{
\item \code{DOCS_FIGURES}: File path to the directory for saving the plot
}}

\item{decay_days_short}{Numeric. Minimum survival time (in days). Default is 3.}

\item{decay_days_long}{Numeric. Maximum survival time (in days). Default is 90.}
}
\value{
Saves a PNG file to \code{PATHS$DOCS_FIGURES}. Invisibly returns the ggplot object.
}
\description{
This function generates a plot that visualizes the suitability-dependent decay rate of
\emph{V. cholerae} survival based on climate-driven environmental suitability (\eqn{\psi_{jt}}).
The figure compares five transformation types for survival time using the cumulative
Beta distribution:
\itemize{
\item \strong{Linear}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 1, s_2 = 1)}
\item \strong{Concave}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 1, s_2 = 5)}
\item \strong{Convex}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 5, s_2 = 1)}
\item \strong{Sigmoidal}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 5, s_2 = 5)}
\item \strong{Arcsine}: \eqn{f(\psi_{jt}) = \text{pbeta}(\psi_{jt} \mid s_1 = 0.5, s_2 = 0.5)}
}
}
\details{
The primary y-axis shows survival time in days. The secondary y-axis shows the decay
rate \eqn{\delta_{jt} = 1 / \text{days}(\psi_{jt})}. Horizontal dashed lines indicate
the minimum and maximum bounds on survival time. The output plot is saved as a PNG
file in the directory specified by \code{PATHS$DOCS_FIGURES}.
}
\examples{
\dontrun{
PATHS <- get_paths()
plot_vibrio_decay_rate(PATHS)
plot_vibrio_decay_rate(PATHS, decay_days_short = 2, decay_days_long = 100)
}
}
