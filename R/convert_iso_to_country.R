#' Convert ISO2 or ISO3 Country Codes to Country Names
#'
#' This function converts a vector or scalar of ISO2 or ISO3 country codes to their corresponding country names using the `countrycode` package.
#'
#' @param iso A vector or scalar of ISO2 or ISO3 country codes (e.g., "USA", "GB", "DZA").
#'
#' @return A character vector of country names corresponding to the provided ISO2 or ISO3 country codes.
#'
#' @details The function automatically detects if the input is in ISO2 or ISO3 format and returns the corresponding country names. Special handling is applied for Congo and the Democratic Republic of Congo.
#'
#' @examples
#' # Convert a vector of ISO3 and ISO2 codes to country names
#' convert_iso_to_country(c("USA", "GBR", "DZA", "COD"))
#'
#' @importFrom countrycode countrycode
#' @export

convert_iso_to_country <- function(iso) {

     # Remove NA values temporarily for detection purposes
     non_na_iso <- iso[!is.na(iso)]

     # Detect if the input is ISO2 or ISO3 based on the length of non-NA codes
     if (all(nchar(non_na_iso) == 3)) {
          origin_format <- "iso3c"
     } else if (all(nchar(non_na_iso) == 2)) {
          origin_format <- "iso2c"
     } else {
          stop("Input contains mixed or invalid country codes.")
     }

     # Convert country codes to country names
     country_names <- suppressWarnings(
          countrycode::countrycode(iso, origin = origin_format, destination = "country.name")
     )

     # Special handling for Congo and DR Congo
     country_names[iso == 'COD'] <- "Democratic Republic of Congo"
     country_names[iso == 'ZAR'] <- "Democratic Republic of Congo"
     country_names[iso == 'CD'] <- "Democratic Republic of Congo"
     country_names[iso == 'ZR'] <- "Democratic Republic of Congo"
     country_names[iso == 'COG'] <- "Congo"
     country_names[iso == 'CG'] <- "Congo"

     return(country_names)
}
