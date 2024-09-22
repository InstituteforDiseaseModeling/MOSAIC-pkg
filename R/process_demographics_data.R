#' Process UN World Prospects Data for African Countries
#'
#' This function processes demographic data from the UN World Population Prospects for African countries. It filters the data by specified years, calculates population, birth rates, and death rates per day, and saves the processed data as a CSV file.
#'
#' @param PATHS A list containing paths where raw and processed data are stored.
#' PATHS is typically the output of the `get_paths()` function and should include the following components:
#' \itemize{
#'   \item \strong{DATA_RAW}: Path to the directory containing the raw UN World Population Prospects data.
#'   \item \strong{DATA_PROCESSED}: Path to the directory where processed demographic data should be saved.
#' }
#'
#' @return The function does not return a value. It processes the UN demographic data and saves the aggregated results as a CSV file in the specified directory.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Loads raw demographic data from a CSV file.
#'   \item Filters the data for the specified years and African countries.
#'   \item Calculates the population, birth rate, and death rate per day for each country.
#'   \item Saves the processed data as a CSV file in the processed data directory.
#' }
#' The processed data file will be saved in the `PATHS$DATA_PROCESSED/demographics/` directory, with a filename that includes the year range of the data.
#'
#' @importFrom countrycode countrycode
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#'
#' @examples
#' \dontrun{
#' # Define paths for raw and processed data using get_paths()
#' PATHS <- get_paths()
#'
#' # Process the UN World Prospects data and save the results
#' process_demographics_data(PATHS)
#'}
#' @export

process_demographics_data <- function(PATHS) {

     # Load the necessary packages and data
     message("Loading raw UN World Prospects demographic data")
     d <- read.csv(file.path(PATHS$DATA_RAW, "demographics/WPP2024_Demographic_Indicators_Medium.csv"), stringsAsFactors = FALSE)

     # Filter data for the years 2000 to 2023
     years <- c(2000:2023)
     d <- d[d$Time %in% years,]

     # Filter data for African countries based on ISO3 codes from MOSAIC
     d <- d[d$ISO3_code %in% MOSAIC::iso_codes_africa,]

     # Create a data frame for the processed data
     out <- data.frame(
          country = NA,
          iso_code = d$ISO3_code,
          year = d$Time,
          population = d$TPopulation1Jan * 1000,  # Convert population to individuals
          births_per_day = (d$Births * 1000) / 365,  # Calculate births per day
          deaths_per_day = (d$Deaths * 1000) / 365  # Calculate deaths per day
     )

     # Calculate birth and death rates per day as a proportion of population
     out$birth_rate_per_day <- out$births_per_day / out$population
     out$death_rate_per_day <- out$deaths_per_day / out$population

     # Convert ISO3 country codes to country names
     out$country <- MOSAIC::convert_iso_to_country(out$iso_code)

     # Output the first few rows of the processed data for inspection
     head(out)

     # Define the output path for the processed data
     path <- file.path(PATHS$DATA_PROCESSED, glue("demographics/demographics_africa_{min(years)}_{max(years)}.csv"))

     # Create the output directory if it doesn't exist
     if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)

     # Save the processed data as a CSV file
     write.csv(out, file = path, row.names = FALSE)
     message(glue("Demographics data saved here: {path}"))
}
