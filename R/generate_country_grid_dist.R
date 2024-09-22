#' Generate a Grid of Points Within a Country's Shapefile Based on Distance
#'
#' This function generates a grid of points within a country's shapefile, where the points are spaced by a specified distance in kilometers.
#'
#' @param country_shp An `sf` object representing the country's shapefile.
#' @param distance_km A numeric value specifying the distance (in kilometers) between grid points.
#'
#' @return An `sf` object containing the generated grid of points within the country boundary.
#'
#' @details The function creates a regular grid of points within the polygon defined by the country's shapefile, spaced according to the specified distance in kilometers.
#'
#' @examples
#' \dontrun{
#' # Example usage with distance in kilometers
#' grid_points_sf <- generate_country_grid_dist(country_shapefile, distance_km = 50)
#'}
#' @export

generate_country_grid_dist <- function(country_shp, distance_km) {


     # Get the central longitude of the country's bounding box
     bbox <- st_bbox(country_shp)
     central_longitude <- (bbox["xmin"] + bbox["xmax"]) / 2

     # Determine the UTM zone based on central longitude
     utm_zone <- floor((central_longitude + 180) / 6) %% 60 + 1

     # Determine if it's in the northern or southern hemisphere
     if (bbox["ymin"] >= 0) {
          hemisphere <- "north"
     } else {
          hemisphere <- "south"
     }

     # Construct the CRS string for the appropriate UTM zone
     crs_string <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")


     # Transform the country geometry to a projected coordinate system to measure distances in meters
     country_proj <- st_transform(country_shp, crs = crs_string)

     # Create a grid based on the specified distance between points
     grid_points <- st_make_grid(country_proj, cellsize = distance_km * 1000, what = "centers")

     # Clip the grid points to the country boundary
     grid_points_sf <- st_intersection(grid_points, st_geometry(country_proj))

     # Transform the grid points back to the original coordinate system (longitude/latitude)
     grid_points_sf <- st_transform(grid_points_sf, crs = st_crs(country_shp))

     return(grid_points_sf)
}
