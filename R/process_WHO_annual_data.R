#' Download, Process, and Save Historical Annual WHO Cholera Data for Africa
#'
#' This function compiles historical cholera data from the WHO for African countries (AFRO region) for the years 1949 to 2024. The data is processed and saved in the MOSAIC directory structure. It includes reported cholera cases, deaths, and case fatality ratios (CFR).
#'
#' @param PATHS A list containing the paths to raw and processed data directories. Typically generated from `get_paths()` and should include:
#' \itemize{
#'   \item \strong{DATA_RAW}: Path to the directory containing raw WHO cholera data.
#'   \item \strong{DATA_WHO_ANNUAL}: Path to the directory where processed cholera data will be saved.
#' }
#'
#' @return The function does not return a value but processes and saves the historical WHO cholera data into CSV files in the processed data directory.
#'
#' @details
#' This function compiles cholera data for African countries (AFRO region) by processing historical data from 1949-2024. The data sources are as follows:
#' \itemize{
#'   \item \strong{1949-2021}: Data were compiled from WHO annual reports by **Our World In Data**. See \url{https://ourworldindata.org}.
#'   \item \strong{2022}: Data were manually extracted from the WHO annual report.
#'   \item \strong{2023-2024}: Data are downloaded directly from the **WHO AWD GIS dashboard**.
#' }
#' Additionally, the function calculates 95% binomial confidence intervals for the CFR where applicable, and the data is saved as CSV files in the processed data directory.
#'
#' @importFrom utils read.csv write.csv download.file
#' @importFrom stats binom.test
#' @importFrom base tolower toupper paste
#'
#' @examples
#' \dontrun{
#' # Assuming PATHS is generated from get_paths()
#' PATHS <- get_paths()
#'
#' # Download and process WHO annual cholera data
#' process_WHO_annual_data(PATHS)
#' }
#'
#' @export


process_WHO_annual_data <- function(PATHS) {

     if (!dir.exists(PATHS$DATA_WHO_ANNUAL)) dir.create(PATHS$DATA_WHO_ANNUAL, recursive = TRUE)

     ################################################################################
     # Process global annual cholera data from 1949 to 2021 (precollated by OWiD)
     ################################################################################

     message("Processing WHO cholera data from 1949 to 2021...")

     # Load historical cholera cases and deaths
     cholera_cases <- utils::read.csv(file.path(PATHS$DATA_RAW, "WHO/annual/who_global_1949_2021/number-reported-cases-of-cholera.csv"), stringsAsFactors = FALSE)
     cholera_deaths <- utils::read.csv(file.path(PATHS$DATA_RAW, "WHO/annual/who_global_1949_2021/number-of-reported-cholera-deaths.csv"), stringsAsFactors = FALSE)

     # Filter for AFRO region data
     cholera_cases_afr <- cholera_cases[cholera_cases$Code %in% MOSAIC::iso_codes_who_afro, ]
     colnames(cholera_cases_afr)[colnames(cholera_cases_afr) == "Entity"] <- "country"
     colnames(cholera_cases_afr)[colnames(cholera_cases_afr) == "Code"] <- "iso_code"
     colnames(cholera_cases_afr)[colnames(cholera_cases_afr) == "Year"] <- "year"
     colnames(cholera_cases_afr)[colnames(cholera_cases_afr) == "Reported.cholera.cases"] <- "cases_total"
     cholera_cases_afr$region <- "AFRO"
     cholera_cases_afr$cases_imported <- NA
     cholera_cases_afr$cfr <- NA

     cholera_deaths_afr <- cholera_deaths[cholera_deaths$Code %in% MOSAIC::iso_codes_who_afro, ]
     colnames(cholera_deaths_afr)[colnames(cholera_deaths_afr) == "Entity"] <- "country"
     colnames(cholera_deaths_afr)[colnames(cholera_deaths_afr) == "Code"] <- "iso_code"
     colnames(cholera_deaths_afr)[colnames(cholera_deaths_afr) == "Year"] <- "year"
     colnames(cholera_deaths_afr)[colnames(cholera_deaths_afr) == "Reported.cholera.deaths"] <- "deaths_total"

     cholera_data_1949_2021 <- merge(cholera_cases_afr, cholera_deaths_afr[, c("country", "year", "iso_code", "deaths_total")], by = c("country", "iso_code", "year"), all.x = TRUE)
     cholera_data_1949_2021$cfr <- cholera_data_1949_2021$deaths_total / cholera_data_1949_2021$cases_total
     cholera_data_1949_2021$cfr[is.nan(cholera_data_1949_2021$cfr)] <- NA

     utils::write.csv(cholera_data_1949_2021, file = file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_1949_2021.csv"), row.names = FALSE)
     message("WHO cholera data (1949-2021) processed and saved to:", file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_1949_2021.csv"))

     ################################################################################
     # Process cholera data for 2022
     ################################################################################

     message("Processing WHO cholera data from 2022...")

     cholera_data_2022 <- data.frame(
          region = rep("AFRO", 17),
          country = c("Benin", "Burkina Faso", "Burundi", "Cameroon", "Democratic Republic of Congo", "Ethiopia", "Kenya", "Liberia", "Malawi", "Mozambique", "Nigeria", "Rwanda", "Somalia", "South Africa", "South Sudan", "Zambia", "Zimbabwe"),
          iso_code = c("BEN", "BFA", "BDI", "CMR", "COD", "ETH", "KEN", "LBR", "MWI", "MOZ", "NGA", "RWA", "SOM", "ZAF", "SSD", "ZMB", "ZWE"),
          cases_total = c(433, 4, 25, 14431, 18961, 846, 3525, 367, 17488, 4378, 23839, 24, 15653, 1, 424, 34, 4),
          cases_imported = c(0, 0, 0, 15, 0, 0, 0, 0, 186, NA, 0, 0, 0, 0, 0, 0, 1),
          deaths_total = c(2, 0, 0, 279, 298, 27, 64, 0, 576, 22, 597, 0, 88, 0, 1, 0, 1),
          cfr = c(0.5, 0.0, 0.0, 1.9, 1.6, 3.2, 1.8, 0.0, 3.3, 0.5, 2.5, 0.0, 0.6, 0.0, 0.2, 0.0, 25.0),
          year = 2022,
          stringsAsFactors = FALSE
     )
     cholera_data_2022$cfr <- cholera_data_2022$cfr / 100

     utils::write.csv(cholera_data_2022, file = file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_2022.csv"), row.names = FALSE)
     message("WHO cholera data (2022) processed and saved to:", file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_2022.csv"))



     ################################################################################
     # Process global annual cholera data from 2023 and 2024
     ################################################################################

     # Message indicating start of processing for 2023-2024 data
     message("Processing WHO cholera data for 2023 and 2024...")

     # Download the most up-to-date 2024 data from WHO
     url <- "https://who.maps.arcgis.com/sharing/rest/content/items/3aa7bfec5da047a7ba7d4f9bcebd0061/data"
     path <- file.path(PATHS$DATA_RAW, "WHO/annual/who_global_2023_2024/cholera_adm0_public_2024.csv")

     # Try downloading the 2024 data, if download fails, load the previously downloaded file
     tryCatch({
          message("Attempting to download the latest 2024 cholera data from WHO...")
          utils::download.file(url, path, mode = "wb")
          message("Successfully downloaded the 2024 cholera data.")
     }, error = function(e) {
          message("Download failed. Loading previously downloaded 2024 cholera data...")
     })

     # Load the 2023 and 2024 data
     cholera_data_2023 <- utils::read.csv(file.path(PATHS$DATA_RAW, "WHO/annual/who_global_2023_2024/cholera_adm0_public_2023.csv"), stringsAsFactors = FALSE)
     cholera_data_2024 <- utils::read.csv(file.path(PATHS$DATA_RAW, "WHO/annual/who_global_2023_2024/cholera_adm0_public_2024.csv"), stringsAsFactors = FALSE)

     # Add the year column to each dataset
     cholera_data_2023$year <- 2023
     cholera_data_2024$year <- 2024

     # Filter the datasets to include only AFRO region data
     cholera_data_2023 <- cholera_data_2023[cholera_data_2023$who_region == "AFRO",]
     cholera_data_2024 <- cholera_data_2024[cholera_data_2024$who_region == "African Region",]
     cholera_data_2024$who_region <- "AFRO"

     # Combine the 2023 and 2024 data
     cholera_data_2023_2024 <- rbind(cholera_data_2023, cholera_data_2024)

     # Rename columns to standardized names
     colnames(cholera_data_2023_2024)[colnames(cholera_data_2023_2024) == "case_total"] <- "cases_total"
     colnames(cholera_data_2023_2024)[colnames(cholera_data_2023_2024) == "death_total"] <- "deaths_total"
     colnames(cholera_data_2023_2024)[colnames(cholera_data_2023_2024) == "who_region"] <- "region"
     colnames(cholera_data_2023_2024)[colnames(cholera_data_2023_2024) == "iso_3_code"] <- "iso_code"
     colnames(cholera_data_2023_2024)[colnames(cholera_data_2023_2024) == "adm0_name"] <- "country"

     # Add cases_imported and CFR columns
     cholera_data_2023_2024$cases_imported <- NA
     cholera_data_2023_2024$cfr <- cholera_data_2023_2024$deaths_total / cholera_data_2023_2024$cases_total
     cholera_data_2023_2024$cfr[is.nan(cholera_data_2023_2024$cfr)] <- NA

     # Remove unwanted columns such as "first_epiwk" and "last_epiwk"
     cholera_data_2023_2024 <- cholera_data_2023_2024[, !(colnames(cholera_data_2023_2024) %in% c("first_epiwk", "last_epiwk"))]

     # Standardize country names in the 2023-2024 data to match the 2022 data
     country_names <- sort(unique(c(cholera_data_1949_2021$country, cholera_data_2022$country)))

     capitalize_words <- function(name) {
          s <- tolower(name)
          s <- strsplit(s, " ")[[1]]
          s <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
          return(s)
     }

     cholera_data_2023_2024$country <- sapply(cholera_data_2023_2024$country, capitalize_words)

     # Manually fix mismatches (e.g., "D.R. Congo" to "Democratic Republic of Congo")
     cholera_data_2023_2024$country[cholera_data_2023_2024$country == "Democratic Republic Of Congo"] <- "Democratic Republic of Congo"
     cholera_data_2023_2024$country[cholera_data_2023_2024$country == "Democratic Republic Of The Congo"] <- "Democratic Republic of Congo"
     cholera_data_2023_2024$country[cholera_data_2023_2024$country == "United Republic Of Tanzania"] <- "Tanzania"

     # Verify if all country names match
     mismatches <- cholera_data_2023_2024$country[!(cholera_data_2023_2024$country %in% country_names)]
     if (length(mismatches) == 0) {
          message("All country names match the expected names.")
     } else {
          message("Mismatched country names:")
          print(mismatches)
     }

     # Save the processed 2023-2024 data
     utils::write.csv(cholera_data_2023_2024, file = file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_2023_2024.csv"), row.names = FALSE)
     message("WHO cholera data (2023-2024) processed and saved to:", file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_2023_2024.csv"))



     ################################################################################
     # Combine data from 1949-2024 and calculate totals for AFRO region
     ################################################################################

     # Combine 1949-2024 data
     combined_cholera_data <- rbind(cholera_data_1949_2021, cholera_data_2022, cholera_data_2023_2024)
     combined_cholera_data$country[combined_cholera_data$country == "Democratic Republic of the Congo"] <- "Democratic Republic of Congo"

     # Calculate AFRO region totals for each year
     afro_totals <- aggregate(cbind(cases_total, deaths_total) ~ year, data = combined_cholera_data, sum, na.rm = TRUE)
     afro_totals$region <- "AFRO"
     afro_totals$country <- "AFRO Region"
     afro_totals$iso_code <- "AFRO"
     afro_totals$cases_imported <- NA
     afro_totals$cfr <- afro_totals$deaths_total / afro_totals$cases_total

     # Combine AFRO region totals with the combined data
     combined_cholera_data <- rbind(combined_cholera_data, afro_totals)

     # Calculate 95% binomial confidence intervals for CFR
     combined_cholera_data$cfr_lo <- combined_cholera_data$cfr_hi <- NA

     for (i in seq_len(nrow(combined_cholera_data))) {
          if (!is.na(combined_cholera_data$cases_total[i]) & !is.na(combined_cholera_data$deaths_total[i])) {
               if (combined_cholera_data$cases_total[i] > 0) {
                    conf_int <- stats::binom.test(x = combined_cholera_data$deaths_total[i], n = combined_cholera_data$cases_total[i])$conf.int
                    combined_cholera_data$cfr_lo[i] <- conf_int[1]
                    combined_cholera_data$cfr_hi[i] <- conf_int[2]
               }
          }
     }

     # Save the combined 1949-2024 data
     utils::write.csv(combined_cholera_data, file = file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_1949_2024.csv"), row.names = FALSE)
     message("WHO cholera data (1949-2024) combined and saved to:", file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual_1949_2024.csv"))

}






