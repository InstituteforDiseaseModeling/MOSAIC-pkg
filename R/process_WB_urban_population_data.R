#' Process World Bank Urban Population Proportion Data
#'
#' Reads the raw World Bank urban population proportion CSV, reshapes it to long format via a for loop,
#' writes the processed data as a CSV to disk, and saves it to processed data.
#'
#' @param PATHS List. Project paths object with components `DATA_RAW` and `DATA_PROCESSED`.
#'   - Reads the newest raw CSV for indicator `SP.URB.TOTL.IN.ZS` in
#'     `file.path(PATHS$DATA_RAW, 'world_bank', 'urban_population')` (see [download_WB_data()]).
#'   - Writes `file.path(PATHS$DATA_PROCESSED, 'world_bank', 'world_bank_urban_population_data.csv')`, creating the directory if needed.
#' @return A data.frame with columns:
#'   \describe{
#'     \item{iso_code}{ISO country code (character)}
#'     \item{year}{Year (integer)}
#'     \item{urban_pop_prop}{Urban population proportion (numeric, percent of total population)}
#'   }
#'   Invisibly returns the data.frame after writing the CSV.
#' @export
#'

process_WB_urban_population_data <- function(PATHS) {

     # Read raw data
     raw <- utils::read.csv(
          .wb_newest_raw(PATHS, "urban_population", "SP.URB.TOTL.IN.ZS"),
          stringsAsFactors = FALSE, skip = 4
     )

     # Identify year columns (XYYYY)
     year_cols <- grep("^X[0-9]{4}$", names(raw), value = TRUE)

     # Prepare empty data.frame for results
     df_long <- data.frame(
          iso_code       = character(0),
          year           = integer(0),
          urban_pop_prop = numeric(0),
          stringsAsFactors = FALSE
     )

     # Loop over each country (row) and build long format
     for (i in seq_len(nrow(raw))) {

          iso  <- raw$Country.Code[i]
          vals <- as.numeric(raw[i, year_cols])
          yrs  <- as.integer(sub("^X", "", year_cols))

          df_i <- data.frame(
               iso_code       = rep(iso, length(yrs)),
               year           = yrs,
               urban_pop_prop = vals,
               stringsAsFactors = FALSE
          )

          df_long <- rbind(df_long, df_i)
     }

     # Write processed output as CSV
     out_dir <- file.path(PATHS$DATA_PROCESSED, 'world_bank')
     if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
     out_path <- file.path(out_dir, 'world_bank_urban_population_data.csv')
     message("Processed World Bank urban population proportion data saved here: ", out_path)
     utils::write.csv(df_long, file = out_path, row.names = FALSE)
     invisible(df_long)
}
