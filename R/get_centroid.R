#' Calculate the Centroid of a Country from its Shapefile
#'
#' This function reads a country shapefile, calculates its centroid, and returns the centroid coordinates (longitude and latitude) along with the ISO3 country code.
#'
#' @param shapefile A character string representing the file path to a country shapefile (in `.shp` format).
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \strong{iso3}: The ISO3 country code extracted from the shapefile name.
#'   \item \strong{lon}: The longitude of the country's centroid.
#'   \item \strong{lat}: The latitude of the country's centroid.
#' }
#'
#' @details
#' The function reads the shapefile using **`sf::st_read()`**, calculates the centroid of the country using **`sf::st_centroid()`**, and extracts the coordinates of the centroid with **`sf::st_coordinates()`**. The ISO3 code is extracted from the filename of the shapefile, assuming the first three characters of the shapefile name represent the ISO3 country code.
#'
#' @importFrom sf st_read st_centroid st_coordinates
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assuming you have a shapefile "ZAF_ADM0.shp" for South Africa
#' shapefile_path <- "path/to/shapefile/ZAF_ADM0.shp"
#' centroid_data <- get_centroid(shapefile_path)
#' print(centroid_data)
#'}
#' @export

get_centroid <- function(shapefile) {

     # Read the shapefile
     shape_data <- sf::st_read(shapefile, quiet = TRUE)

     # Calculate the centroid of the shapefile
     centroids <- sf::st_centroid(shape_data)

     # Return a data frame with ISO3 code and centroid coordinates
     out <- data.frame(
          iso3 = substr(gsub(".shp", "", basename(shapefile)), 1, 3),  # Extract ISO3 from filename
          lon = sf::st_coordinates(centroids)[, 1],  # Longitude
          lat = sf::st_coordinates(centroids)[, 2]   # Latitude
     )

     return(out)
}
