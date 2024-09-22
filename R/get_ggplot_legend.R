#' Extract the Legend from a ggplot Object
#'
#' This function extracts the legend from a ggplot object and returns it as a grob (graphical object).
#' The extracted legend can then be used independently, for instance, to combine the legend with other plots.
#'
#' @param plot A ggplot object from which the legend should be extracted.
#'
#' @return A grob representing the legend of the ggplot object.
#'
#' @details This function converts a ggplot object into a grob using **`ggplotGrob()`** and
#' extracts the legend, which is stored in the grob under the name "guide-box". The function
#' can be used to separate the legend from the plot and combine it with other plots as needed.
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

     # Extract the legend, which is named "guide-box" in the gtable
     legend <- gtable$grobs[[which(sapply(gtable$grobs, function(x) x$name) == "guide-box")]]

     return(legend)
}
