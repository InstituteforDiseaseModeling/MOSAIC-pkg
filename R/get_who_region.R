#' Get WHO Region for ISO Country Codes
#'
#' This function maps ISO 3-letter country codes to WHO African regional classifications.
#' Uses the pre-loaded regional ISO code vectors available in the MOSAIC package data.
#'
#' @param iso_codes Character vector of ISO 3-letter country codes (e.g., "NGA", "KEN")
#'
#' @return Character vector of the same length as input with WHO regional classifications:
#' \itemize{
#'   \item \strong{West Africa}: Countries in West African region
#'   \item \strong{East Africa}: Countries in East African region  
#'   \item \strong{Central Africa}: Countries in Central African region
#'   \item \strong{North Africa}: Countries in North African region
#'   \item \strong{Southern Africa}: Countries in Southern African region
#'   \item \strong{Unknown}: Countries not found in any regional classification
#' }
#'
#' @details
#' The function uses the following MOSAIC package data objects:
#' \itemize{
#'   \item \code{iso_codes_africa_west}: West African countries
#'   \item \code{iso_codes_africa_east}: East African countries
#'   \item \code{iso_codes_africa_central}: Central African countries
#'   \item \code{iso_codes_africa_north}: North African countries
#'   \item \code{iso_codes_africa_south}: Southern African countries
#' }
#'
#' @examples
#' \dontrun{
#' # Single country
#' get_who_region("NGA")  # Returns "West Africa"
#' 
#' # Multiple countries
#' countries <- c("NGA", "KEN", "COD", "ZAF")
#' get_who_region(countries)  # Returns regional classifications
#' 
#' # Handle missing/unknown codes
#' get_who_region(c("NGA", "XXX"))  # Returns c("West Africa", "Unknown")
#' }
#'
#' @export
get_who_region <- function(iso_codes) {
     
     # Input validation
     if (!is.character(iso_codes)) {
          stop("iso_codes must be a character vector")
     }
     
     if (length(iso_codes) == 0) {
          return(character(0))
     }
     
     # Initialize result vector
     regions <- rep("Unknown", length(iso_codes))
     
     # Define regional groupings directly (based on MOSAIC package data)
     # This avoids dependency issues with loading .rda files
     
     # West Africa
     west_africa <- c("BEN", "BFA", "CIV", "CPV", "GHA", "GIN", "GMB", "GNB", 
                     "LBR", "MLI", "MRT", "NER", "NGA", "SEN", "SLE", "TGO")
     
     # East Africa  
     east_africa <- c("BDI", "COM", "DJI", "ERI", "ETH", "KEN", "MDG", "MWI", 
                     "MOZ", "RWA", "SOM", "SSD", "TZA", "UGA", "ZMB", "ZWE")
     
     # Central Africa
     central_africa <- c("AGO", "CAF", "CMR", "COD", "COG", "GAB", "GNQ", 
                        "STP", "TCD")
     
     # North Africa
     north_africa <- c("DZA", "EGY", "LBY", "MAR", "SDN", "TUN")
     
     # Southern Africa
     southern_africa <- c("BWA", "LSO", "NAM", "SWZ", "ZAF")
     
     # Check each regional group
     west_match <- iso_codes %in% west_africa
     regions[west_match] <- "West Africa"
     
     east_match <- iso_codes %in% east_africa
     regions[east_match] <- "East Africa"
     
     central_match <- iso_codes %in% central_africa
     regions[central_match] <- "Central Africa"
     
     north_match <- iso_codes %in% north_africa
     regions[north_match] <- "North Africa"
     
     south_match <- iso_codes %in% southern_africa
     regions[south_match] <- "Southern Africa"
     
     # Handle NA inputs
     regions[is.na(iso_codes)] <- NA_character_
     
     return(regions)
}