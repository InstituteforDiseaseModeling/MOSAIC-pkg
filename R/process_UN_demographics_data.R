#' Process UN World Prospects Data for African Countries
#'
#' This function processes demographic data from the UN World Population Prospects for African countries.
#' It loads available data, gets total population, birth rates, and death rates, and saves the processed 
#' data as both annual and daily CSV files. The daily data is created through linear interpolation.
#'
#' @param PATHS A list containing paths where raw and processed data are stored.
#' PATHS is typically the output of the `get_paths()` function and should include the following components:
#' \itemize{
#'   \item \strong{DATA_RAW}: Path to the directory containing the raw UN World Population Prospects data.
#'   \item \strong{DATA_PROCESSED}: Path to the directory where processed demographic data should be saved.
#' }
#'
#' @return The function does not return a value. It processes the UN demographic data and saves both 
#' annual and daily interpolated results as CSV files in the specified directory.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Loads raw demographic data from CSV files.
#'   \item Merges population, birth rate, and death rate data.
#'   \item Saves the annual data as `UN_world_population_prospects_annual.csv`.
#'   \item Creates daily interpolated data using linear interpolation between annual values.
#'   \item Saves the daily data as `UN_world_population_prospects_daily.csv`.
#' }
#' The processed data files will be saved in the `PATHS$DATA_PROCESSED/demographics/` directory.
#'
#' Data Source: [United Nations World Population Prospects](https://population.un.org/wpp/)
#'
#' @importFrom countrycode countrycode
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv txtProgressBar setTxtProgressBar
#' @importFrom dplyr distinct
#' @importFrom stats approx
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

     # Create the output directory if it doesn't exist
     output_dir <- file.path(PATHS$DATA_PROCESSED, "demographics")
     if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

     # Save the ANNUAL data
     path_annual <- file.path(output_dir, "UN_world_population_prospects_annual.csv")
     write.csv(d, file = path_annual, row.names = FALSE)
     message(glue("Annual UN demographics data from {min_year} to {max_year} saved here: {path_annual}"))

     # Create DAILY interpolated data
     message("Creating daily interpolated demographic data...")
     
     # Create a daily time series for all years
     daily_dates <- seq(from = as.Date(paste0(min_year, "-01-01")), 
                       to = as.Date(paste0(max_year, "-12-31")), 
                       by = "day")
     
     # Initialize empty dataframe for daily data
     daily_data <- data.frame()
     
     # Process each country
     unique_iso <- unique(d$iso_code)
     
     # Progress bar
     pb <- txtProgressBar(min = 0, max = length(unique_iso), style = 3)
     
     for (i in seq_along(unique_iso)) {
          iso <- unique_iso[i]
          
          # Get country data
          country_data <- d[d$iso_code == iso, ]
          country_data <- country_data[order(country_data$year), ]
          
          # Create annual dates (July 1st of each year as midpoint)
          annual_dates <- as.Date(paste0(country_data$year, "-07-01"))
          
          # Create daily dataframe for this country
          country_daily <- data.frame(
               date = daily_dates,
               iso_code = iso
          )
          
          # Linear interpolation for each variable
          # Total population
          if (nrow(country_data) > 1) {
               country_daily$total_population <- approx(
                    x = annual_dates,
                    y = country_data$total_population,
                    xout = daily_dates,
                    method = "linear",
                    rule = 2  # Extrapolate using nearest value
               )$y
               
               # Births per 1000
               country_daily$births_per_1000 <- approx(
                    x = annual_dates,
                    y = country_data$births_per_1000,
                    xout = daily_dates,
                    method = "linear",
                    rule = 2
               )$y
               
               # Deaths per 1000
               country_daily$deaths_per_1000 <- approx(
                    x = annual_dates,
                    y = country_data$deaths_per_1000,
                    xout = daily_dates,
                    method = "linear",
                    rule = 2
               )$y
          } else {
               # If only one year of data, use constant values
               country_daily$total_population <- country_data$total_population[1]
               country_daily$births_per_1000 <- country_data$births_per_1000[1]
               country_daily$deaths_per_1000 <- country_data$deaths_per_1000[1]
          }
          
          # Add year column for compatibility
          country_daily$year <- as.numeric(format(country_daily$date, "%Y"))
          
          # Append to main dataframe
          daily_data <- rbind(daily_data, country_daily)
          
          # Update progress bar
          setTxtProgressBar(pb, i)
     }
     
     close(pb)
     
     # Sort by iso_code and date
     daily_data <- daily_data[order(daily_data$iso_code, daily_data$date), ]
     
     # Reorder columns
     daily_data <- daily_data[, c("iso_code", "date", "year", "total_population", 
                                   "births_per_1000", "deaths_per_1000")]
     
     # Save the DAILY data
     path_daily <- file.path(output_dir, "UN_world_population_prospects_daily.csv")
     write.csv(daily_data, file = path_daily, row.names = FALSE)
     message(glue("Daily interpolated UN demographics data saved here: {path_daily}"))
     
     # Also save with the legacy filename for backward compatibility
     path_legacy <- file.path(output_dir, glue("UN_world_population_prospects_{min_year}_{max_year}.csv"))
     write.csv(d, file = path_legacy, row.names = FALSE)
     message(glue("Legacy file also saved for compatibility: {path_legacy}"))

     message("Done.")

}
