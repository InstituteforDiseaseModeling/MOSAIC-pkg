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

          # Replace the existing shapefile only if its content changed
          changed <- .write_shapefile_if_changed(shp, shp_path)
          message(if (changed) paste0("Shapefile saved here: ", shp_path)
                  else paste0("Shapefile unchanged: ", shp_path))
     }

     message("Done.")
}


#' Write a shapefile only when its content differs from the one on disk
#'
#' Writes \code{x} to a temporary directory and compares every component file
#' with the existing one. The dBASE header stores a last-update date (bytes
#' 2-4), so a byte comparison would report every rewrite as a change; those
#' bytes are ignored. When nothing else differs the existing files are left
#' untouched, so a refresh does not dirty the data repository.
#'
#' @param x An \code{sf} object.
#' @param shp_path Destination \code{.shp} path.
#' @return Invisibly, \code{TRUE} if files were (re)written, \code{FALSE} if unchanged.
#' @keywords internal
#' @noRd
.write_shapefile_if_changed <- function(x, shp_path) {
     tmp_dir <- tempfile("shp_")
     dir.create(tmp_dir)
     on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
     tmp_shp <- file.path(tmp_dir, basename(shp_path))
     sf::st_write(x, tmp_shp, quiet = TRUE)

     stem <- tools::file_path_sans_ext(basename(shp_path))
     new_files <- list.files(tmp_dir, pattern = paste0("^", stem, "\\."), full.names = TRUE)
     old_files <- file.path(dirname(shp_path), basename(new_files))

     read_cmp <- function(f) {
          b <- readBin(f, "raw", file.info(f)$size)
          if (tolower(tools::file_ext(f)) == "dbf" && length(b) >= 4L) b[2:4] <- as.raw(0L)
          b
     }
     unchanged <- all(file.exists(old_files)) &&
          all(vapply(seq_along(new_files), function(i)
               identical(read_cmp(new_files[i]), read_cmp(old_files[i])), logical(1)))
     if (unchanged) return(invisible(FALSE))

     dir.create(dirname(shp_path), recursive = TRUE, showWarnings = FALSE)
     ok <- file.copy(new_files, old_files, overwrite = TRUE)
     if (!all(ok)) stop("Could not write shapefile components for ", shp_path, call. = FALSE)
     invisible(TRUE)
}
