#' Generate Directory Paths for the MOSAIC Project
#'
#' The `get_paths()` function generates a structured list of file paths for different data and document directories in the MOSAIC project, based on the provided root directory.
#'
#' @param root A string specifying the root directory of the MOSAIC project; if `NULL`, `getOption("root_directory")` (see \code{\link{set_root_directory}}).
#'
#' @return A named list of paths, all built with \code{file.path(root, ...)}:
#' \item{ROOT}{The root directory.}
#' \item{DATA_RAW, DATA_PROCESSED}{\code{MOSAIC-data/raw} and \code{MOSAIC-data/processed}.}
#' \item{DATA_SCRAPE_WHO_VACCINATION}{\code{MOSAIC-data/raw/WHO/vaccination}.}
#' \item{DATA_SCRAPE_WHO_WEEKLY, DATA_SCRAPE_GTFCC}{WHO AWD and GTFCC scrapes under
#'   \code{ees-cholera-mapping/data/cholera/}.}
#' \item{DATA_DEM, DATA_EMDAT_RAW, DATA_IDMC_RAW}{Raw inputs under \code{MOSAIC-data/raw/}
#'   (\code{DEM}, \code{EMDAT}, \code{IDMC}).}
#' \item{DATA_SHAPEFILES, DATA_SIMILARITY_MATRIX, DATA_ELEVATION, DATA_ENSO, DATA_OAG,
#'   DATA_UNICEF, DATA_WORLD_BANK, DATA_DEMOGRAPHICS, DATA_WASH, DATA_SYMPTOMATIC,
#'   DATA_IMMUNITY, DATA_SUSPECTED, DATA_VACCINE_EFFECTIVENESS}{Processed data under
#'   \code{MOSAIC-data/processed/} (\code{shapefiles}, \code{similarity_matrix},
#'   \code{elevation}, \code{enso}, \code{OAG}, \code{UNICEF}, \code{world_bank},
#'   \code{demographics}, \code{WASH}, \code{symptomatic}, \code{immunity},
#'   \code{suspected_cases}, \code{vaccine_effectiveness}).}
#' \item{DATA_CLIMATE, DATA_CLIMATE_DAILY}{\code{MOSAIC-data/processed/climate/weekly}
#'   and \code{.../climate/daily}.}
#' \item{DATA_WHO_ANNUAL, DATA_WHO_WEEKLY, DATA_WHO_DAILY}{\code{MOSAIC-data/processed/WHO/}
#'   \code{annual}, \code{weekly}, \code{daily}.}
#' \item{DATA_JHU_WEEKLY, DATA_JHU_DAILY}{\code{MOSAIC-data/processed/JHU/weekly} and \code{.../daily}.}
#' \item{DATA_SUPP_WEEKLY, DATA_SUPP_DAILY}{Both \code{MOSAIC-data/processed/SUPP/daily}:
#'   the weekly supplemental file \code{cholera_country_weekly_processed.csv} is
#'   written and read there.}
#' \item{DATA_CHOLERA_WEEKLY, DATA_CHOLERA_DAILY, DATA_AI_WEEKLY}{Combined surveillance under
#'   \code{MOSAIC-data/processed/cholera/} (\code{weekly}, \code{daily}, \code{ai/weekly}).}
#' \item{DATA_GTFCC_VACCINATION}{\code{MOSAIC-data/processed/GTFCC/vaccination}.}
#' \item{DATA_EMDAT, DATA_IDMC}{\code{MOSAIC-data/processed/EMDAT/weekly} and
#'   \code{.../IDMC/weekly}.}
#' \item{ENSO_DATA_REPO, OPEN_METEO_REPO, AI_CHOLERA_REPO}{Sibling repositories
#'   \code{enso-data}, \code{open-meteo-pipeline}, \code{ai-cholera-data-mining}.}
#' \item{MODEL_INPUT, MODEL_OUTPUT}{\code{MOSAIC-pkg/model/input} and \code{MOSAIC-pkg/model/output}.}
#' \item{DOCS_FIGURES, DOCS_TABLES, DOCS_PARAMS}{\code{MOSAIC-docs/figures}, \code{tables},
#'   \code{parameters}.}
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
     PATHS$DATA_SCRAPE_WHO_VACCINATION <- file.path(root, "MOSAIC-data/raw/WHO/vaccination")
     PATHS$DATA_SCRAPE_WHO_WEEKLY <- file.path(root, "ees-cholera-mapping/data/cholera/who/awd")
     PATHS$DATA_SCRAPE_GTFCC <- file.path(root, "ees-cholera-mapping/data/cholera/epicentre/gtfcc")
     PATHS$DATA_RAW <- file.path(root, "MOSAIC-data/raw")
     PATHS$DATA_PROCESSED <- file.path(root, "MOSAIC-data/processed")
     PATHS$DATA_SHAPEFILES <- file.path(root, "MOSAIC-data/processed/shapefiles")
     PATHS$DATA_SIMILARITY_MATRIX <- file.path(root, "MOSAIC-data/processed/similarity_matrix")
     PATHS$DATA_DEM <- file.path(root, "MOSAIC-data/raw/DEM")
     PATHS$DATA_ELEVATION <- file.path(root, "MOSAIC-data/processed/elevation")
     PATHS$DATA_CLIMATE <- file.path(root, "MOSAIC-data/processed/climate/weekly")
     PATHS$DATA_CLIMATE_DAILY <- file.path(root, "MOSAIC-data/processed/climate/daily")
     PATHS$DATA_ENSO <- file.path(root, "MOSAIC-data/processed/enso")
     PATHS$ENSO_DATA_REPO <- file.path(root, "enso-data")
     PATHS$OPEN_METEO_REPO <- file.path(root, "open-meteo-pipeline")
     PATHS$AI_CHOLERA_REPO <- file.path(root, "ai-cholera-data-mining")
     PATHS$DATA_OAG <- file.path(root, "MOSAIC-data/processed/OAG")
     PATHS$DATA_UNICEF <- file.path(root, "MOSAIC-data/processed/UNICEF")
     PATHS$DATA_WORLD_BANK <- file.path(root, "MOSAIC-data/processed/world_bank")
     PATHS$DATA_WHO_ANNUAL <- file.path(root, "MOSAIC-data/processed/WHO/annual")
     PATHS$DATA_WHO_WEEKLY <- file.path(root, "MOSAIC-data/processed/WHO/weekly")
     PATHS$DATA_WHO_DAILY <- file.path(root, "MOSAIC-data/processed/WHO/daily")
     PATHS$DATA_JHU_WEEKLY <- file.path(root, "MOSAIC-data/processed/JHU/weekly")
     PATHS$DATA_JHU_DAILY <- file.path(root, "MOSAIC-data/processed/JHU/daily")
     PATHS$DATA_SUPP_WEEKLY <- file.path(root, "MOSAIC-data/processed/SUPP/daily")
     PATHS$DATA_SUPP_DAILY <- file.path(root, "MOSAIC-data/processed/SUPP/daily")
     PATHS$DATA_CHOLERA_WEEKLY <- file.path(root, "MOSAIC-data/processed/cholera/weekly")
     PATHS$DATA_CHOLERA_DAILY <- file.path(root, "MOSAIC-data/processed/cholera/daily")
     PATHS$DATA_AI_WEEKLY <- file.path(root, "MOSAIC-data/processed/cholera/ai/weekly")
     PATHS$DATA_DEMOGRAPHICS <- file.path(root, "MOSAIC-data/processed/demographics")
     PATHS$DATA_WASH <- file.path(root, "MOSAIC-data/processed/WASH")
     PATHS$DATA_SYMPTOMATIC <- file.path(root, "MOSAIC-data/processed/symptomatic")
     PATHS$DATA_IMMUNITY <- file.path(root, "MOSAIC-data/processed/immunity")
     PATHS$DATA_SUSPECTED <- file.path(root, "MOSAIC-data/processed/suspected_cases")
     PATHS$DATA_VACCINE_EFFECTIVENESS <- file.path(root, "MOSAIC-data/processed/vaccine_effectiveness")
     PATHS$DATA_GTFCC_VACCINATION <- file.path(root, "MOSAIC-data/processed/GTFCC/vaccination")
     PATHS$DATA_EMDAT_RAW <- file.path(root, "MOSAIC-data/raw/EMDAT")
     PATHS$DATA_EMDAT <- file.path(root, "MOSAIC-data/processed/EMDAT/weekly")
     PATHS$DATA_IDMC_RAW <- file.path(root, "MOSAIC-data/raw/IDMC")
     PATHS$DATA_IDMC <- file.path(root, "MOSAIC-data/processed/IDMC/weekly")
     PATHS$MODEL_INPUT <- file.path(root, "MOSAIC-pkg/model/input")
     PATHS$MODEL_OUTPUT <- file.path(root, "MOSAIC-pkg/model/output")
     PATHS$DOCS_FIGURES <- file.path(root, "MOSAIC-docs/figures")
     PATHS$DOCS_TABLES <- file.path(root, "MOSAIC-docs/tables")
     PATHS$DOCS_PARAMS <- file.path(root, "MOSAIC-docs/parameters")

     return(PATHS)

}
