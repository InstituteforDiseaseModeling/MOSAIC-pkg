#' Plot MOSAIC Country Map
#'
#' This function creates a map of Africa highlighting countries included in the
#' MOSAIC framework. The map is saved as a PNG file in the directory specified by \code{PATHS$DOCS_FIGURES}.
#'
#' @param PATHS A named list containing file paths required for output. It must include:
#' \describe{
#'   \item{\strong{DOCS_FIGURES}}{Path to the directory where the map image will be saved.}
#' }
#'
#' @return Message
#'
#' @importFrom ggplot2 ggplot geom_sf scale_fill_manual theme_minimal element_blank element_text unit
#' @importFrom rnaturalearth ne_countries
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom grDevices png dev.off
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   PATHS <- list(DOCS_FIGURES = "path/to/figures")
#'   # Generate and save the MOSAIC country map
#'   plot_mosaic_country_map(PATHS)
#'
#' }
#'

plot_mosaic_country_map <- function(PATHS) {

     # Load African country shapefiles
     africa <- rnaturalearth::ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
     africa <- dplyr::mutate(africa, iso_a3 = toupper(iso_a3))

     # Create a factor variable indicating MOSAIC inclusion
     africa$mosaic_status <- ifelse(africa$iso_a3 %in% MOSAIC::iso_codes_mosaic, "Included", "Not Included")
     africa$mosaic_status <- factor(africa$mosaic_status, levels = c("Included", "Not Included"))

     pal <- c("Included" = "#0167af", "Not Included" = "#f7f7f7")

     p <-
          ggplot2::ggplot(data = africa) +
          ggplot2::geom_sf(fill = "white", color = "black") +
          ggplot2::geom_sf(aes(fill = mosaic_status), color = "black") +
          ggplot2::scale_fill_manual(
               values = c("Included" = "#0167af", "Not Included" = "#f7f7f7"),
               breaks = "Included",
               labels = "Sub-Saharan countries\nincluded in MOSAIC"
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(
               panel.grid.major = ggplot2::element_blank(),
               panel.grid.minor = ggplot2::element_blank(),
               axis.text = ggplot2::element_blank(),
               axis.title = ggplot2::element_blank(),
               legend.position = "right",
               legend.title = ggplot2::element_blank(),
               legend.text = ggplot2::element_text(size = 10.5),
               legend.key.height = ggplot2::unit(1.25, "cm"),
               legend.key.width = ggplot2::unit(0.75, "cm"),
               plot.margin = ggplot2::unit(c(0.25, 0.25, 0.25, 0.25), "inches")
          )

     output_file <- file.path(PATHS$DOCS_FIGURES, "mosaic_country_map.png")

     grDevices::png(filename = output_file, width = 2000, height = 1600, units = "px", res = 300)
     print(p)
     grDevices::dev.off()

     message(glue::glue("MOSAIC country map saved to: {output_file}"))

     return(p)
}
