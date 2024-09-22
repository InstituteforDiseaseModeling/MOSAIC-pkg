#' Convert ISO3 to ISO2 or ISO2 to ISO3 Country Codes
#'
#' This function converts a vector or scalar of ISO3 country codes to ISO2 country codes, or vice versa, based on the input. The function uses the `countrycode` package and auto-detects the input format.
#'
#' @param iso_codes A vector or scalar of ISO2 or ISO3 country codes (e.g., "USA", "GBR", "ZA", "GB").
#'
#' @return A character vector of converted ISO2 or ISO3 country codes corresponding to the provided input.
#'
#' @details The function detects whether the input is ISO2 or ISO3 format based on the code length and converts accordingly using the `countrycode` package.
#'
#' @examples
#' # Convert a vector of ISO3 codes to ISO2 codes
#' convert_iso_codes(c("USA", "GBR", "DZA"))
#'
#' # Convert a vector of ISO2 codes to ISO3 codes
#' convert_iso_codes(c("US", "GB", "DZ"))
#'
#' @importFrom countrycode countrycode
#' @export

convert_iso_codes <- function(iso_codes) {

     # Remove NA values temporarily for detection purposes
     non_na_codes <- iso_codes[!is.na(iso_codes)]

     # Detect whether the input is ISO2 or ISO3 based on the length of non-NA codes
     if (all(nchar(non_na_codes) == 3)) {
          origin_format <- "iso3c"  # Assume ISO3 if the length is 3
          destination_format <- "iso2c"  # Convert to ISO2
     } else if (all(nchar(non_na_codes) == 2)) {
          origin_format <- "iso2c"  # Assume ISO2 if the length is 2
          destination_format <- "iso3c"  # Convert to ISO3
     } else {
          stop("Mixed or invalid ISO codes provided.")
     }

     # Convert ISO codes using the countrycode package
     converted_codes <- suppressWarnings(
          countrycode::countrycode(iso_codes, origin = origin_format, destination = destination_format)
     )
     # Special handling for Congo and DR Congo
     converted_codes[iso_codes %in% c('COD', 'ZAR')] <- "CD"
     converted_codes[iso_codes %in% c('CD', 'ZR')] <- "COD"
     converted_codes[iso_codes %in% c('CG')] <- "COG"
     converted_codes[iso_codes %in% c('COG')] <- "CG"

     return(converted_codes)
}
