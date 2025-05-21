#' Process World Bank Population Density Data
#'
#' Reads the raw World Bank population density CSV (skipping the first 4 metadata rows), reshapes it to long format via a for loop,
#' writes the processed data as a CSV to disk, and returns it. Uses base R functions only.
#'
#' @param PATHS List. Project paths object with components `DATA_RAW` and `DATA_PROCESSED`.
#'   - Expects the raw file at:
#'     `file.path(PATHS$DATA_RAW, 'world_bank', 'API_EN.POP.DNST_DS2_en_csv_v2_85433.csv')`
#'   - Will write output to:
#'     `file.path(PATHS$DATA_PROCESSED, 'world_bank', 'pop_density_data_world_bank.csv')`
#' @return A data.frame with columns:
#'   \describe{
#'     \item{iso_code}{ISO country code (character)}
#'     \item{year}{Year (integer)}
#'     \item{pop_density}{Population density (numeric, people per sq. km)}
#'   }
#'   Invisibly returns the data.frame after writing the CSV.
#' @export
#'

process_WB_population_density_data <- function(PATHS) {

     # Read raw data
     raw <- utils::read.csv(
          file.path(PATHS$DATA_RAW, 'world_bank', 'population_density', 'API_EN.POP.DNST_DS2_en_csv_v2_85433.csv'),
          stringsAsFactors = FALSE, skip = 4
     )

     # Identify year columns (XYYYY)
     year_cols <- grep("^X[0-9]{4}$", names(raw), value = TRUE)

     # Prepare empty data.frame for results
     df_long <- data.frame(
          iso_code    = character(0),
          year        = integer(0),
          pop_density = numeric(0),
          stringsAsFactors = FALSE
     )

     # Loop over each country (row) and build long format
     for (i in seq_len(nrow(raw))) {

          iso  <- raw$Country.Code[i]
          vals <- as.numeric(raw[i, year_cols])
          yrs  <- as.integer(sub("^X", "", year_cols))

          df_i <- data.frame(
               iso_code    = rep(iso, length(yrs)),
               year        = yrs,
               pop_density = vals,
               stringsAsFactors = FALSE
          )

          df_long <- rbind(df_long, df_i)
     }

     # Write processed output as CSV
     out_path <- file.path(PATHS$DATA_PROCESSED, 'world_bank', 'world_bank_population_density_data.csv')
     message("Processed World Bank population density data saved here: ", out_path)
     utils::write.csv(df_long, file = out_path, row.names = FALSE)

}
