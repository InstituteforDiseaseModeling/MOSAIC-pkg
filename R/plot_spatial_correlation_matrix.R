#' Heat-map of a spatial–correlation matrix (ggplot2)
#'
#' Draws a diverging-colour heat-map of the *L × L* correlation matrix produced
#' by `calc_spatial_correlation_matrix()`.
#'
#' @param C Numeric matrix of correlations (values in \[-1, 1\]).
#'
#' @return A **ggplot** object (rendered automatically in interactive sessions).
#' @export
plot_spatial_correlation_heatmap <- function(C) {

     if (!is.matrix(C))
          stop("`C` must be a numeric matrix.")

     # -------- tidy to long format ------------------------------------------
     loc_i <- rownames(C) %||% seq_len(nrow(C))
     loc_j <- colnames(C) %||% seq_len(ncol(C))

     df <- data.frame(
          loc_i = factor(rep(loc_i, times = ncol(C)),
                         levels = rev(loc_i)),  # flip for y-axis top-to-bottom
          loc_j = factor(rep(loc_j, each  = nrow(C)),
                         levels = loc_j),
          corr  = as.vector(C)
     )

     # -------- plot ----------------------------------------------------------
     ggplot2::ggplot(df, ggplot2::aes(x = loc_j, y = loc_i, fill = corr)) +
          ggplot2::geom_tile() +
          ggplot2::scale_fill_gradient2(low = "firebrick",
                                        mid = "white",
                                        high = "steelblue",
                                        limits = c(-1, 1),
                                        midpoint = 0,
                                        name = expression(C[ij])) +
          ggplot2::labs(title = "Spatial correlation matrix",
                        x = "Location j", y = "Location i") +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                         panel.grid  = ggplot2::element_blank())
}
