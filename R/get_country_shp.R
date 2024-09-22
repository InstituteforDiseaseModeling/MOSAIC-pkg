#' Get a country shapefile from GeoBoundaries API
#'
#' This function retrieves a shapefile for the given iso3 code from the GeoBoundaries data API.
#'
#' @param iso3 A three-letter capitalized character string. Must follow the ISO-3166 Alpha-3 country code
#' standard ([https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3)).
#' @param path_output A file path of where to save the country shapefile
#'
#' @return sf class shapefile
#'
#' @examples
#' \dontrun{
#'
#' tmp <- get_country_shp(iso3 = 'MCO')
#' plot(tmp)
#'
#' }

get_country_shp <- function(iso3, path_output=NULL) {

     if (is.null(path_output)) path_output <- tempdir()

     api_url <- paste0("https://www.geoboundaries.org/api/current/gbOpen/", iso3, "/ADM0/")
     response <- httr::GET(api_url)

     if (response$status_code == 200) {

          data <- suppressMessages(httr::content(response, "text"))
          data_parsed <- jsonlite::fromJSON(data)

     } else {

          stop("Error: ", response$status_code)

     }


     download_url <- data_parsed$gjDownloadURL
     download_path <- file.path(path_output, basename(download_url))

     download.file(url = download_url,
                   destfile = download_path,
                   method='auto',
                   quiet = FALSE,
                   mode = "wb",
                   cacheOK = TRUE,
                   extra = getOption("download.file.extra"))

     out <- sf::read_sf(download_path, drivers='geojson', quiet=TRUE)
     shapeType <- out$shapeType[1]
     out <- out[,c('shapeName', 'geometry')]
     colnames(out)[colnames(out) == 'shapeName'] <- shapeType

     return(out)

}
