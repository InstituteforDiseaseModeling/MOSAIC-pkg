#' Generate Directory Paths for the MOSAIC Project
#'
#' The `get_paths()` function generates a structured list of file paths for different data and document directories in the MOSAIC project, based on the provided root directory.
#'
#' @param root A string specifying the root directory of the MOSAIC project. All other paths will be generated relative to this root.
#'
#' @return A named list containing the following paths:
#' \item{ROOT}{The root directory provided by the user.}
#' \item{DATA_RAW}{Path to the raw data directory (under "MOSAIC-data/data/raw").}
#' \item{DATA_PROCESSED}{Path to the processed data directory (under "MOSAIC-data/data/processed").}
#' \item{MODEL_INPUT}{`NULL`, reserved for the path to the model input (if applicable).}
#' \item{MODEL_OUTPUT}{`NULL`, reserved for the path to the model output (if applicable).}
#' \item{DOCS_FIGURES}{Path to the figures directory (under "MOSAIC-docs/figures").}
#' \item{DOCS_TABLES}{Path to the tables directory (under "MOSAIC-docs/tables").}
#' \item{DOCS_PARAMS}{Path to the parameters directory (under "MOSAIC-docs/parameters").}
#'
#' @details
#' This function helps organize the directory structure for data and document storage in the MOSAIC project by generating paths for raw data, processed data, figures, tables, and parameters. The paths are returned as a named list and can be used to streamline the access to various project-related directories.
#'
#' @examples
#'\dontrun{
#' root_dir <- "/{full file path}/MOSAIC"
#' PATHS <- get_paths(root_dir)
#' print(PATHS$DATA_RAW)
#'}
#' @export

get_paths <- function(root=NULL) {

     if (is.null(root)) {

          if ('root_directory' %in% names(options())) {
               root <- getOption('root_directory')
          } else {
               stop("Cannot find root_directory")
          }

     }

     PATHS <- list()
     PATHS$ROOT <- root
     PATHS$DATA_RAW <- file.path(root, "MOSAIC-data/raw")
     PATHS$DATA_PROCESSED <- file.path(root, "MOSAIC-data/processed")
     PATHS$DATA_SHAPEFILES <- file.path(root, "MOSAIC-data/processed/shapefiles")
     PATHS$DATA_ELEVATION <- file.path(root, "MOSAIC-data/processed/elevation")
     PATHS$DATA_CLIMATE <- file.path(root, "MOSAIC-data/processed/climate")
     PATHS$DATA_ENSO <- file.path(root, "MOSAIC-data/processed/ENSO")
     PATHS$DATA_OAG <- file.path(root, "MOSAIC-data/processed/OAG")
     PATHS$DATA_WHO_ANNUAL <- file.path(root, "MOSAIC-data/processed/WHO/annual")
     PATHS$DATA_WHO_WEEKLY <- file.path(root, "MOSAIC-data/processed/WHO/weekly")
     PATHS$DATA_DEMOGRAPHICS <- file.path(root, "MOSAIC-data/processed/demographics")
     PATHS$DATA_WASH <- file.path(root, "MOSAIC-data/processed/WASH")
     PATHS$DATA_SYMPTOMATIC <- file.path(root, "MOSAIC-data/processed/symptomatic")
     PATHS$DATA_IMMUNITY <- file.path(root, "MOSAIC-data/processed/immunity")
     PATHS$DATA_SUSPECTED <- file.path(root, "MOSAIC-data/processed/suspected_cases")
     PATHS$DATA_VACCINE_EFFECTIVENESS <- file.path(root, "MOSAIC-data/processed/vaccine_effectiveness")
     PATHS$MODEL_INPUT <- file.path(root, "MOSAIC-pkg/model/input")
     PATHS$MODEL_OUTPUT <- file.path(root, "MOSAIC-pkg/model/output")
     PATHS$DOCS_FIGURES <- file.path(root, "MOSAIC-docs/figures")
     PATHS$DOCS_TABLES <- file.path(root, "MOSAIC-docs/tables")
     PATHS$DOCS_PARAMS <- file.path(root, "MOSAIC-docs/parameters")

     return(PATHS)

}
