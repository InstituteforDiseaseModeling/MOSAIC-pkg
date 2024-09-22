#' Download and Save Shapefiles for All African Countries
#'
#' This function downloads shapefiles for each African country and saves them in a specified directory.
#' The function iterates through ISO3 country codes for Africa (from the MOSAIC framework) and saves each country's shapefile individually.
#'
#' @param PATHS A list containing the paths where processed data should be saved.
#' PATHS is typically the output of the `get_paths()` function and should include:
#' \itemize{
#'   \item \strong{DATA_PROCESSED}: Path to the directory where the processed shapefiles should be saved.
#' }
#'
#' @return The function does not return a value. It downloads the shapefiles for each African country and saves them as individual shapefiles in the processed data directory.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Creates the output directory for shapefiles if it does not exist.
#'   \item Loops through the list of ISO3 country codes for African countries.
#'   \item Downloads each country's shapefile using `MOSAIC::get_country_shp()`.
#'   \item Saves the shapefile in the processed data directory as `ISO3_ADM0.shp`.
#' }
#'
#' @importFrom sf st_write
#'
#' @examples
#' \dontrun{
#' # Define paths for processed data using get_paths()
#' PATHS <- get_paths()
#'
#' # Download and save shapefiles for all African countries
#' download_all_country_shapefiles(PATHS)
#' }
#'
#' @export
download_all_country_shapefiles <- function(PATHS) {

     # Ensure 'sf' package is available
     requireNamespace('sf')

     # Define output path for shapefiles
     path_out <- file.path(PATHS$DATA_PROCESSED, "shapefiles")

     # Create the directory if it doesn't exist
     if (!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)

     # Get the list of ISO3 codes for African countries from the MOSAIC framework
     iso_codes <- MOSAIC::iso_codes_africa

     # Loop through each country code, download, and save its shapefile
     for (i in iso_codes) {

          message(i)
          shp <- MOSAIC::get_country_shp(i)
          shp_name <- paste(i, "ADM0.shp", sep = "_")
          shp_path <- file.path(path_out, shp_name)

          # Save the shapefile using sf::st_write
          sf::st_write(shp, shp_path)
          message(paste0("Shapefile saved here: ", shp_path))
     }

     message("Done.")
}
