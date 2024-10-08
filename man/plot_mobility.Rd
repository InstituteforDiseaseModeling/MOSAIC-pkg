% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mobility.R
\name{plot_mobility}
\alias{plot_mobility}
\title{Plot Mobility Data, Mobility Network, and Results}
\usage{
plot_mobility(PATHS)
}
\arguments{
\item{PATHS}{A list containing paths where data, models, and figures are stored. Typically generated by the \code{get_paths()} function and should include:
\itemize{
\item \strong{MODEL_INPUT}: Path to the directory where the mobility matrices and parameter estimates are saved.
\item \strong{DATA_SHAPEFILES}: Path to the directory containing shapefiles for African countries.
\item \strong{DOCS_FIGURES}: Path to the directory where figures will be saved.
}}
}
\value{
The function generates and saves mobility-related visualizations as PNG files.
}
\description{
This function loads and visualizes the mobility data, including the flight data, travel probabilities, diffusion model estimates, and mobility network.
}
