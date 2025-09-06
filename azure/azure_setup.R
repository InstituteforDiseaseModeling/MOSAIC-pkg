install.packages('remotes')

if (!requireNamespace("MOSAIC", quietly = TRUE)) {
     message("Installing package 'MOSAIC' from GitHub...")
     if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
     remotes::install_github("InstituteforDiseaseModeling/MOSAIC-pkg")
}

library(MOSAIC)

packages <- c(
     "arrow",
     "cowplot",
     "dplyr",
     "elevatr",
     "glue",
     "ggplot2",
     "ggrepel",
     "ggraph",
     "grid",
     "gridExtra",
     "igraph",
     "keras3",
     "minpack.lm",
     "mobility",    # GitHub: https://github.com/COVID-19-Mobility-Data-Network/mobility
     "patchwork",
     "propvacc",    # GitHub: https://github.com/gilesjohnr/propvacc
     "raster",
     "rnaturalearth",
     "reshape2",
     "RColorBrewer",
     "shiny",
     "shinyWidgets",
     "tensorflow",
     "tidyr",
     "viridis"
)

MOSAIC::load_or_install_packages(packages)
