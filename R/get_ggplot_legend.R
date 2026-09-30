#' Extract the Legend from a ggplot Object
#'
#' This function extracts the legend from a ggplot object and returns it as a grob (graphical object).
#' The extracted legend can then be used independently, for instance, to combine the legend with other plots.
#'
#' @param plot A ggplot object from which the legend should be extracted.
#'
#' @return A grob representing the legend of the ggplot object, or an empty
#'   \code{grid::nullGrob()} when the plot has no legend (e.g. a mapped layer with
#'   no rows, such as a single-location flux network), so the result can always
#'   be passed to \code{gridExtra::grid.arrange()} or \code{cowplot::plot_grid()}.
#'
#' @details This function converts a ggplot object into a grob using **`ggplotGrob()`** and
#' extracts the legend. ggplot2 >= 3.5 lays out one guide-box slot per position
#' (\code{"guide-box-right"}, \code{"guide-box-bottom"}, ...), leaving empty slots
#' as zero grobs; the first non-empty slot is returned, and the single
#' \code{"guide-box"} grob of older ggplot2 versions is matched too.
#'
#' @importFrom ggplot2 ggplotGrob
#' @importFrom grid grid.draw
#'
#' @examples
#' # Create a simple ggplot object
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, color = factor(gear))) +
#'      ggplot2::geom_point() +
#'      ggplot2::scale_color_discrete(name = "Gear")
#'
#' # Extract the legend from the plot
#' legend <- get_ggplot_legend(p)
#'
#' # Display the extracted legend
#' grid::grid.draw(legend)
#'
#' @export

get_ggplot_legend <- function(plot) {

     # Convert the ggplot object to a gtable object
     gtable <- ggplot2::ggplotGrob(plot)

     # Guide-box grobs: layout slots named "guide-box[-<position>]" (ggplot2
     # >= 3.5) or a grob named "guide-box" (older); empty slots are zeroGrobs.
     grob_names <- vapply(gtable$grobs, function(x) as.character(x$name %||% "")[1],
                          character(1))
     is_box <- grepl("^guide-box", gtable$layout$name) | grepl("^guide-box", grob_names)
     is_empty <- vapply(gtable$grobs, function(x) inherits(x, "zeroGrob"), logical(1))
     idx <- which(is_box & !is_empty)
     legend <- if (length(idx)) gtable$grobs[[idx[1]]] else grid::nullGrob()

     return(legend)
}
