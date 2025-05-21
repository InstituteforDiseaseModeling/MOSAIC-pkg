#' Process World Bank GDP data
#'
#' Reads the raw World Bank GDP CSV, reshapes it to long format via a for loop,
#' writes the processed data as a CSV to disk, and saves it to processed data.
#'
#' @param PATHS List. Project paths object with components `DATA_RAW` and `DATA_PROCESSED`.
#'   - Expects the raw file at:
#'     `file.path(PATHS$DATA_RAW, 'world_bank', 'API_NY.GDP.MKTP.CD_DS2_en_csv_v2_132025.csv')`
#'   - Will write output to:
#'     `file.path(PATHS$DATA_PROCESSED, 'world_bank', 'GDP_data_world_bank.csv')`
#' @return A data.frame with columns:
#'   \describe{
#'     \item{iso_code}{ISO country code (character)}
#'     \item{year}{Year (integer)}
#'     \item{GDP}{Gross Domestic Product (numeric)}
#'   }
#'   Invisibly returns the data.frame after writing the CSV.
#' @export

process_WB_GDP_data <- function(PATHS) {

     # Read raw data
     raw <- utils::read.csv(file.path(PATHS$DATA_RAW, 'world_bank', 'GDP', 'API_NY.GDP.MKTP.CD_DS2_en_csv_v2_132025.csv'), stringsAsFactors = FALSE, skip = 4)

     # Identify year columns (XYYYY)
     year_cols <- grep("^X[0-9]{4}$", names(raw), value = TRUE)

     # Prepare empty data.frame for results
     df_long <- data.frame(
          iso_code = character(0),
          year     = integer(0),
          GDP      = numeric(0),
          stringsAsFactors = FALSE
     )

     # Loop over each country (row) and build long format
     for (i in seq_len(nrow(raw))) {

          iso  <- raw$Country.Code[i]
          # Extract GDP values and corresponding years
          vals <- as.numeric(raw[i, year_cols])
          yrs  <- as.integer(sub("^X", "", year_cols))

          # Build per-country data.frame
          df_i <- data.frame(
               iso_code = rep(iso, length(yrs)),
               year     = yrs,
               GDP      = vals,
               stringsAsFactors = FALSE
          )

          # Append to master
          df_long <- rbind(df_long, df_i)
     }

     # Write processed output as CSV
     out_path <- file.path(PATHS$DATA_PROCESSED, 'world_bank', 'world_bank_GDP_data.csv')
     message("Processed World Bank GDP data saved here: ", out_path)
     utils::write.csv(df_long, file = out_path, row.names = FALSE)

}
