#' Process UN World Prospects Data for African Countries
#'
#' This function processes demographic data from the UN World Population Prospects for African countries.
#' It loads available data, gets total population, birth rates, and death rates, and saves the processed data as a CSV file.
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
#'   \item Gets the population, birth rate, and death rate per day for each country.
#'   \item Saves the processed data as a CSV file in the processed data directory.
#' }
#' The processed data file will be saved in the `PATHS$DATA_PROCESSED/demographics/` directory, with a filename that includes the year range of the available data.
#'
#' Data Source: [United Nations World Population Prospects](https://population.un.org/wpp/)
#'
#' @importFrom countrycode countrycode
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv
#' @importFrom dplyr distinct
#'
#' @examples
#' \dontrun{
#' # Define paths for raw and processed data using get_paths()
#' PATHS <- get_paths()
#'
#' # Process the UN World Prospects data for all available years and save the results
#' process_demographics_data(PATHS)
#' }
#'
#' @export

process_UN_demographics_data <- function(PATHS) {

     message("Loading raw UN World Prospects demographic data...")

     path_pop <- file.path(PATHS$DATA_RAW, "demographics/UN_world_population_prospects_1967_2100_population_size.csv")
     path_birth <- file.path(PATHS$DATA_RAW, "demographics/UN_world_population_prospects_1967_2100_birth_rate.csv")
     path_death <- file.path(PATHS$DATA_RAW, "demographics/UN_world_population_prospects_1967_2100_death_rate.csv")

     # Check if files exist
     if (!file.exists(path_pop) || !file.exists(path_birth) || !file.exists(path_death)) {
          stop("Error: One or more required data files are missing.")
     }

     d_population <- read.csv(path_pop, stringsAsFactors = FALSE)
     d_birth_rate <- read.csv(path_birth, stringsAsFactors = FALSE)
     d_death_rate <- read.csv(path_death, stringsAsFactors = FALSE)

     # Process population data
     d_population <- data.frame(iso_code = d_population$Iso3,
                                year = d_population$Time,
                                total_population = d_population$Value)

     d_population <- dplyr::distinct(d_population)

     if (F) {
          plot(d_population$year, d_population$total_population, col = factor(d_population$iso_code), main = "Total Population Over Time", xlab = "Year", ylab = "Total Population")
     }

     # Process birth rate data
     d_birth_rate <- data.frame(iso_code = d_birth_rate$Iso3,
                                year = d_birth_rate$Time,
                                births_per_1000 = d_birth_rate$Value)

     d_birth_rate <- dplyr::distinct(d_birth_rate)

     if (F) {
          plot(d_birth_rate$year, d_birth_rate$births_per_1000, col = factor(d_birth_rate$iso_code), main = "Birth Rate Over Time", xlab = "Year", ylab = "Births per 1000")
     }

     # Process death rate data
     d_death_rate <- data.frame(iso_code = d_death_rate$Iso3,
                                year = d_death_rate$Time,
                                deaths_per_1000 = d_death_rate$Value)

     d_death_rate <- dplyr::distinct(d_death_rate)

     if (F) {
          plot(d_death_rate$year, d_death_rate$deaths_per_1000, col = factor(d_death_rate$iso_code), main = "Death Rate Over Time", xlab = "Year", ylab = "Deaths per 1000")
     }

     # Merge data frames
     message("Cleaning and merging into one data frame...")
     d <- merge(d_population, d_birth_rate, by = c("iso_code", "year"), all = TRUE)
     d <- merge(d, d_death_rate, by = c("iso_code", "year"), all = TRUE)

     # Determine the range of available years dynamically
     min_year <- min(d$year, na.rm=TRUE)
     max_year <- max(d$year, na.rm=TRUE)

     # Output the first few rows of the processed data for inspection
     head(d)

     # Define the output path for the processed data
     path <- file.path(PATHS$DATA_PROCESSED, glue("demographics/UN_world_population_prospects_{min_year}_{max_year}.csv"))

     # Create the output directory if it doesn't exist
     if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)

     # Save the processed data as a CSV file
     write.csv(d, file = path, row.names = FALSE)
     message("Done.")
     message(glue("Processed UN demographics data from {min_year} to {max_year} saved here: {path}"))

}
