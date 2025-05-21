#' Plot spatial importation hazard heatmap
#'
#' @description
#' Generates a heatmap of spatial importation hazards over time and across locations using ggplot2.
#'
#' @param H Numeric matrix of spatial importation hazards (output from \code{calc_spatial_hazard}), with rows as time steps and columns as locations. Row names should be dates in "YYYY-MM-DD" format for proper date parsing.
#'
#' @return Invisibly returns a ggplot object displaying the heatmap.
#'
#' @import ggplot2
#' @importFrom grid unit
#' @export
#'

plot_spatial_hazard <- function(H) {

     # Validate input
     if (!is.matrix(H)) stop("`H` must be a numeric matrix.")
     Tt <- nrow(H)
     J  <- ncol(H)

     # Attempt to parse rownames as Date
     rn <- rownames(H)
     if (!is.null(rn) && all(!is.na(as.Date(rn, format = "%Y-%m-%d")))) {
          time_dates <- as.Date(rn, format = "%Y-%m-%d")
          use_date <- TRUE
     } else {
          time_dates <- seq_len(Tt)
          use_date <- FALSE
     }

     # Location labels
     location_vec <- if (!is.null(colnames(H))) colnames(H) else seq_len(J)

     # Build long-format data frame
     df <- expand.grid(
          time_index = seq_len(Tt),
          location   = location_vec,
          stringsAsFactors = FALSE
     )
     df$hazard <- as.vector(H)
     df$time <- if (use_date) rep(time_dates, times = J) else df$time_index

     # Determine thinning for x-axis breaks (~10 labels)
     x_breaks <- if (use_date) {
          pretty(time_dates)
     } else {
          pretty(df$time_index)
     }

     # Plot heatmap
     p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = location, fill = hazard)) +
          ggplot2::geom_tile() +
          ggplot2::scale_fill_gradient(low = "white", high = "purple4") +
          ({
               if (use_date) {
                    ggplot2::scale_x_date(breaks = x_breaks,
                                          date_labels = "%Y-%m-%d",
                                          expand = c(0, 0))
               } else {
                    ggplot2::scale_x_continuous(breaks = x_breaks,
                                                expand = c(0, 0))
               }
          }) +
          ggplot2::scale_y_discrete(expand = c(0, 0)) +
          ggplot2::guides(fill = ggplot2::guide_colorbar(
               direction = "horizontal",
               barwidth = grid::unit(10, "cm"),
               barheight = grid::unit(0.3, "cm")
          )) +
          ggplot2::labs(x = if (use_date) "Date" else "Time index",
                        y = "Location",
                        fill = "Spatial Hazard") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
               panel.spacing       = ggplot2::unit(0, "null"),
               axis.text.x         = ggplot2::element_text(hjust = 1),
               axis.text.y         = ggplot2::element_text(size = ggplot2::rel(0.8)),
               axis.title.x        = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
               axis.title.y        = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
               legend.position     = "bottom",
               legend.key          = ggplot2::element_rect(color = "black", fill = NA),
               legend.key.width    = grid::unit(2, "cm"),
               legend.key.height   = grid::unit(0.4, "cm"),
               legend.background   = ggplot2::element_blank()
          )

     print(p)
     invisible(p)
}
