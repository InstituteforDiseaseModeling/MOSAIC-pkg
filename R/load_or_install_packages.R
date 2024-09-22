#' Load or Install Required Packages (with GitHub handling for mobility, propvacc, and MOSAIC)
#'
#' This function checks whether the specified packages are installed. If a package is not installed, it will be installed. Special handling is included for `mobility`, `propvacc`, and `MOSAIC`, which are installed from GitHub.
#'
#' @param packages A character vector of package names to load or install.
#'
#' @details
#' For most packages, this function installs them from CRAN if they are not already installed. Special cases are handled for the following packages:
#' \itemize{
#'   \item \strong{mobility}: Installed from GitHub at \url{https://github.com/COVID-19-Mobility-Data-Network/mobility}.
#'   \item \strong{propvacc}: Installed from GitHub at \url{https://github.com/gilesjohnr/propvacc}.
#'   \item \strong{MOSAIC}: Installed from GitHub at \url{https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg}.
#' }
#'
#' @examples
#' \dontrun{
#' load_or_install_packages(c("ggplot2", "mobility", "propvacc", "MOSAIC"))
#' }
#'
#' @importFrom remotes install_github
#' @export

load_or_install_packages <- function(packages) {

     installed_packages <- installed.packages()[, "Package"]

     for (pkg in packages) {

          if (pkg == "mobility" && !pkg %in% installed_packages) {

               message("Installing package 'mobility' from GitHub...")
               remotes::install_github("COVID-19-Mobility-Data-Network/mobility")
               library(mobility, character.only = TRUE)

          } else if (pkg == "propvacc" && !pkg %in% installed_packages) {

               message("Installing package 'propvacc' from GitHub...")
               remotes::install_github("gilesjohnr/propvacc")
               library(propvacc, character.only = TRUE)

          } else if (pkg == "MOSAIC" && !pkg %in% installed_packages) {

               message("Installing package 'MOSAIC' from GitHub...")
               remotes::install_github("InstituteforDiseaseModeling/MOSAIC-pkg")
               library(MOSAIC, character.only = TRUE)

          } else if (!pkg %in% installed_packages) {

               message(paste("Installing package:", pkg))
               install.packages(pkg)
               library(pkg, character.only = TRUE)

          } else {

               library(pkg, character.only = TRUE)

          }
     }

}
