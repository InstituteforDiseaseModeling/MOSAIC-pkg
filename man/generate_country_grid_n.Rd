% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_country_grid_n.R
\name{generate_country_grid_n}
\alias{generate_country_grid_n}
\title{Generate a Grid of Points Within a Country's Shapefile Based on Number of Points}
\usage{
generate_country_grid_n(country_shp, n_points)
}
\arguments{
\item{country_shp}{An \code{sf} object representing the country's shapefile.}

\item{n_points}{A numeric value specifying the total number of evenly spaced points to generate within the country boundary.}
}
\value{
An \code{sf} object containing the generated grid of points within the country boundary.
}
\description{
This function generates a grid of points within a country's shapefile, where the grid contains a specified number of evenly spaced points.
}
\details{
The function creates a regular grid of points within the polygon defined by the country's shapefile, containing the specified number of points.
}
\examples{
\dontrun{
# Example usage with a specified number of points
grid_points_sf <- generate_country_grid_n(country_shapefile, n_points = 100)
}
}
