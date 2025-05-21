#' Process UNICEF Child Malnutrition Data
#'
#' Reads the "Survey Estimates" sheet from the raw UNICEF malnutrition Excel file,
#' drops footnote columns, reshapes key indicators to long format, and writes a CSV.
#'
#' @param paths A named list of file paths with components:
#'   \itemize{
#'     \item raw$malnutrition_data: path to the raw Excel file
#'     \item processed$malnutrition_data: path to write the processed CSV
#'   }
#' @return Invisibly returns the processed data frame (long format).
#' @importFrom readxl read_excel
#' @importFrom utils write.csv
#' @export
#'

process_UNICEF_malnutrition_data <- function(PATHS) {

     # Read raw data from the specified sheet
     df_raw <- readxl::read_excel(file.path(PATHS$DATA_RAW, 'UNICEF', 'child_malnutrition', 'JME_Country_Estimates_May_2023.xlsx'), sheet = "Survey Estimates")

     # Drop any columns containing footnotes
     df <- df_raw[, !grepl("Footnote", names(df_raw))]

     # Define the indicators to reshape
     indicators <- c("Severe Wasting", "Wasting", "Overweight", "Stunting", "Underweight")
     long_list <- vector("list", length(indicators))

     # Loop over each indicator and reshape
     for (i in seq_along(indicators)) {
          ind <- indicators[i]
          temp <- df[, c("ISO code", "Country and areas", "Year*", ind)]
          names(temp)[names(temp) == "Year*"] <- "year"
          names(temp)[names(temp) == ind] <- "estimate"
          temp$indicator <- ind
          long_list[[i]] <- temp
     }

     # Combine into a single data frame
     malnutrition_long <- do.call(rbind, long_list)

     # Clean up column names
     names(malnutrition_long) <- c("iso_code", "country", "year", "estimate", "indicator")

     # Write to CSV
     out_path <- file.path(PATHS$DATA_PROCESSED, 'UNICEF', 'UNICEF_child_malnutrition_data.csv')
     message("Writing processed malnutrition data to CSV: ", out_path)
     utils::write.csv(malnutrition_long, file = out_path, row.names = FALSE)

}
