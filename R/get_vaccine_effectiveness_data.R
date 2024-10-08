#' Get Vaccine Effectiveness Data
#'
#' This function saves a dataset containing vaccine effectiveness data extracted from multiple studies. The data is saved as a CSV file.
#'
#' @param PATHS A list containing the paths where the data will be saved. Typically generated by the `get_paths()` function and should include:
#' \itemize{
#'   \item \strong{DATA_VACCINE_EFFECTIVENESS}: Path to the directory where the vaccine effectiveness data will be saved.
#' }
#'
#' @export

get_vaccine_effectiveness_data <- function(PATHS) {

     if (!dir.exists(PATHS$DATA_VACCINE_EFFECTIVENESS)) dir.create(PATHS$DATA_VACCINE_EFFECTIVENESS, recursive = TRUE)

     # Data from Azman et al (2016), Qadri et al (2016, 2018), Malembaka et al (2024)
     df <- data.frame(
          day = c(60, mean(c(7, 180)), mean(c(7, 730)), mean(c(30*12, 30*17)), mean(c(30*24, 30*36))),
          effectiveness = c(0.873, 0.4, 0.39, 0.527, 0.447),
          effectiveness_hi = c(0.99, 0.6, 0.52, 0.674, 0.594),
          effectiveness_lo = c(0.702, 0.11, 0.23, 0.314, 0.248),
          day_min = c(NA, 7, 7, 30*12, 30*24),
          day_max = c(NA, 180, 730, 30*17, 30*36),
          source = c('Azman et al (2016)', 'Qadri et al (2016)', 'Qadri et al (2018)', 'Malembaka et al (2024)', 'Malembaka et al (2024)')
     )

     # Save the dataset as CSV
     path <- file.path(PATHS$DATA_VACCINE_EFFECTIVENESS, "vaccine_effectiveness_data.csv")
     write.csv(df, path, row.names = FALSE)
     message(paste("Vaccine effectiveness data saved to:", path))

     path <- file.path(PATHS$DOCS_TABLES, "vaccine_effectiveness_data.csv")
     write.csv(df, path, row.names = FALSE)
     message(paste("Vaccine effectiveness also data saved to:", path))
}
