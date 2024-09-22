#' Download and Save Africa Shapefile
#'
#' This function downloads the shapefile for African countries using the `rnaturalearth` package and saves it in a specified directory. If the output directory does not exist, it will be created.
#'
#' @param PATHS A list or environment containing the path structure, where `PATHS$DATA_PROCESSED` specifies the directory to save the processed shapefile.
#'
#' @return A message indicating that the shapefile has been saved, including the full path to the saved file.
#'
#' @details This function uses the `rnaturalearth` package to download the shapefile for the African continent. The shapefile is saved as `AFRICA_ADM0.shp` in the directory specified by `PATHS$DATA_PROCESSED/shapefiles`. The function will create the directory if it does not already exist.
#'
#' @examples
#' \dontrun{
#'   # Example PATHS object
#'   PATHS <- list(DATA_PROCESSED = "path/to/processed/data")
#'
#'   # Download and save the Africa shapefile
#'   download_africa_shapefile(PATHS)
#' }
#'
#' @export
download_africa_shapefile <- function(PATHS) {

     # Load necessary packages
     requireNamespace('rnaturalearth')
     requireNamespace('sf')

     # Define output path for shapefiles
     path_out <- file.path(PATHS$DATA_PROCESSED, "shapefiles")

     # Create output directory if it doesn't exist
     if (!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)

     # Download shapefile for the world and filter for Africa
     world <- rnaturalearth::ne_countries(scale = "medium", type = "countries", returnclass = "sf")

     # Filter African countries
     africa <- world[world$continent == "Africa", ]

     # Define path to save the Africa shapefile
     shp_path <- file.path(path_out, "AFRICA_ADM0.shp")

     # Save the Africa shapefile
     sf::st_write(africa, dsn = shp_path, delete_dsn = TRUE)

     # Message to confirm where the shapefile was saved
     message(paste0("Shapefile saved here: ", shp_path))
}
