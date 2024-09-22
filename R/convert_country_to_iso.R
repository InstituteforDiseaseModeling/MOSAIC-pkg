#' Convert Country Names to ISO3 or ISO2 Country Codes
#'
#' This function converts country names to their corresponding ISO3 or ISO2 country codes. The function uses the `countrycode` package, which allows for multiple spellings and variations of country names.
#'
#' @param x A character vector of country names (e.g., "United States", "UK", "Democratic Republic of Congo").
#' @param iso3 A logical value. If TRUE (default), returns ISO3 country codes. If FALSE, returns ISO2 country codes.
#'
#' @return A character vector of ISO3 or ISO2 country codes corresponding to the country names.
#'
#' @details The function converts country names to ISO3 or ISO2 codes using the `countrycode` package. It handles various common and alternative spellings of country names.
#'
#' @examples
#' # Convert a single country name to ISO3
#' convert_country_to_iso("United States")
#'
#' # Convert a vector of country names to ISO2
#' convert_country_to_iso(c("United States", "UK", "Democratic Republic of Congo"), iso3 = FALSE)
#'
#' @importFrom countrycode countrycode
#' @export

convert_country_to_iso <- function(x, iso3 = TRUE) {

     # Determine destination based on iso3 argument
     destination_code <- ifelse(iso3, "iso3c", "iso2c")

     # Convert country names to ISO codes
     iso_codes <- countrycode::countrycode(x, origin = "country.name", destination = destination_code, warn = TRUE)

     return(iso_codes)
}
