#' Generate a Grid of Points Within a Country's Shapefile Based on Number of Points
#'
#' This function generates a grid of points within a country's shapefile, where the grid contains a specified number of evenly spaced points.
#'
#' @param country_shp An `sf` object representing the country's shapefile.
#' @param n_points A numeric value specifying the total number of evenly spaced points to generate within the country boundary.
#'
#' @return An `sf` object containing the generated grid of points within the country boundary.
#'
#' @details The function creates a regular grid of points within the polygon defined by the country's shapefile, containing the specified number of points.
#'
#' @examples
#' \dontrun{
#' # Example usage with a specified number of points
#' grid_points_sf <- generate_country_grid_n(country_shapefile, n_points = 100)
#'}
#' @export

generate_country_grid_n <- function(country_shp, n_points) {

     # Convert the sf object to a SpatialPolygons object (sp format)
     country_sp <- as(country_shp, "Spatial")

     # Generate uniformly spaced points within the polygon using spsample
     grid_points <- sp::spsample(country_sp, n = n_points, type = "regular")

     # Convert the points back to an sf object
     grid_points_sf <- st_as_sf(grid_points)

     return(grid_points_sf)
}
