#' Plot spatial importation hazard
#'
#' @description
#' Generates a heatmap of spatial importation hazards over time and across locations using ggplot2.
#'
#' @param H Numeric matrix of spatial importation hazards (output from \code{calc_spatial_hazard}),
#'   with **rows** = locations and **columns** = time steps.  Column names should be dates
#'   in "YYYY-MM-DD" format for proper date parsing; row names will be used as location labels.
#'
#' @return Invisibly returns a ggplot object displaying the heatmap.
#'
#' @import ggplot2
#' @importFrom grid unit
#' @export
#'
#' Plot spatial importation hazard
#'
#' @description
#' Generates a heatmap of spatial importation hazards over time and across locations using ggplot2.
#'
#' @param H Numeric matrix of spatial importation hazards (output from \code{calc_spatial_hazard}),
#'   with **rows** = locations and **columns** = time steps.  Column names should be dates
#'   in "YYYY-MM-DD" format for proper date parsing; row names will be used as location labels.
#'
#' @return Invisibly returns a ggplot object displaying the heatmap.
#'
#' @import ggplot2
#' @importFrom grid unit
#' @export
plot_spatial_hazard <- function(H) {
     if (!is.matrix(H)) stop("`H` must be a numeric matrix.")

     # dimensions
     J  <- nrow(H)
     Tt <- ncol(H)

     # parse column names as dates?
     cn <- colnames(H)
     if (!is.null(cn) && all(!is.na(as.Date(cn, "%Y-%m-%d")))) {
          time_vals <- as.Date(cn, "%Y-%m-%d")
          use_date  <- TRUE
     } else {
          time_vals <- seq_len(Tt)
          use_date  <- FALSE
     }

     # get location labels and sort them alphanumerically
     rn <- rownames(H)
     loc_vals <- if (!is.null(rn)) rn else paste0("loc_", seq_len(J))
     loc_vals <- sort(loc_vals)

     # build long data.frame
     df <- data.frame(
          time     = rep(time_vals, each    = J),
          location = rep(loc_vals,  times   = Tt),
          hazard   = as.vector(H),
          stringsAsFactors = FALSE
     )
     # ensure factor levels follow the sorted order
     df$location <- factor(df$location, levels = loc_vals)

     # choose x-scale
     if (use_date) {
          x_scale <- scale_x_date(
               breaks      = pretty(time_vals),
               date_labels = "%Y-%m-%d",
               expand      = c(0, 0)
          )
          x_lab <- "Date"
     } else {
          x_scale <- scale_x_continuous(
               breaks = pretty(df$time),
               expand = c(0, 0)
          )
          x_lab <- "Time index"
     }

     p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = location, fill = hazard)) +
          ggplot2::geom_tile() +
          x_scale +
          ggplot2::scale_y_discrete(expand = c(0, 0)) +
          ggplot2::scale_fill_gradient(low = "white", high = "purple4") +
          ggplot2::guides(fill = ggplot2::guide_colorbar(
               direction  = "horizontal",
               barwidth   = unit(10, "cm"),
               barheight  = unit(0.3, "cm")
          )) +
          ggplot2::labs(
               x    = x_lab,
               y    = "Location",
               fill = "Spatial Hazard"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
               panel.spacing     = unit(0, "null"),
               axis.text.x       = ggplot2::element_text(hjust = 1, angle = if(use_date) 45 else 0),
               axis.text.y       = ggplot2::element_text(size = ggplot2::rel(0.8)),
               axis.title.x      = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
               axis.title.y      = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
               legend.position   = "bottom",
               legend.key        = ggplot2::element_rect(color = "black", fill = NA),
               legend.key.width  = unit(2, "cm"),
               legend.key.height = unit(0.4, "cm"),
               legend.background = ggplot2::element_blank()
          )

     print(p)
     invisible(p)
}
