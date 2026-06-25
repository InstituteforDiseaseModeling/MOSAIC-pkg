#' Plot the model-implied diffusion connectivity matrix (\eqn{\pi_{ij}})
#'
#' Draws a heatmap of the normalized gravity connectivity matrix
#' \eqn{\pi_{ij}} (\code{\link{calc_diffusion_matrix_pi}}), the probability that
#' a traveler leaving origin \eqn{i} arrives at destination \eqn{j}. This is the
#' \eqn{\pi_{ij}} panel of the \code{"spatial"} figure group in
#' \code{\link{render_MOSAIC_figures}}.
#'
#' The aesthetic matches the published \eqn{\pi} panel of
#' \code{\link{plot_mobility}} exactly (\code{theme_bw()}, viridis "G" fill,
#' bottom colorbar). Axes are ordered south-to-north by latitude via a keyed
#' \code{factor(levels = )} over the named matrix (values follow their labels);
#' no positional sort of labels is performed.
#'
#' This is the model-derived sibling of the observed-OAG \eqn{\pi} panel in
#' \code{\link{plot_mobility}}; the two visualize different quantities (modeled
#' vs observed) and are intentionally separate.
#'
#' @param pi Numeric J x J connectivity matrix from
#'   \code{\link{calc_mobility_flux}} (element \code{$pi}), diagonal \code{NA}.
#' @param location_name Character vector of length J giving axis labels in the
#'   order of \code{pi}'s rows/columns. Defaults to \code{rownames(pi)}.
#' @param latitude Numeric vector of length J giving each location's latitude in
#'   the row order of \code{pi}; used to order the axes south-to-north (matching
#'   \code{\link{plot_mobility}}). When \code{NULL} (default) the axes are left
#'   in the order of \code{location_name}.
#'
#' @return Invisibly, the \pkg{ggplot2} object.
#'
#' @seealso \code{\link{calc_mobility_flux}}, \code{\link{plot_mobility}}.
#' @importFrom ggplot2 ggplot aes geom_tile xlab ylab theme_bw theme element_text margin guides guide_colorbar
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
plot_diffusion_pi <- function(pi, location_name = rownames(pi), latitude = NULL) {

  if (!is.matrix(pi) || !is.numeric(pi))
    stop("`pi` must be a numeric matrix.")
  J <- nrow(pi)
  if (is.null(location_name)) location_name <- paste0("loc_", seq_len(J))
  if (length(location_name) != J)
    stop("`location_name` length (", length(location_name),
         ") must match nrow(pi) (", J, ").")

  loc <- as.character(location_name)

  # Order south-to-north by latitude when supplied (mirrors plot_mobility.R:56).
  # Keyed factor over the NAMED matrix: values follow their labels (F3-safe).
  if (!is.null(latitude)) {
    if (length(latitude) != J)
      stop("`latitude` length must match nrow(pi).")
    iso3_ordered <- loc[order(as.numeric(latitude))]
  } else {
    iso3_ordered <- loc
  }

  dimnames(pi) <- list(loc, loc)

  p <- ggplot2::ggplot(
    data = reshape2::melt(pi, varnames = c("origin", "destination"),
                          value.name = "value")
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(
        x    = factor(.data$destination, levels = iso3_ordered),
        y    = factor(.data$origin, levels = iso3_ordered),
        fill = .data$value
      )
    ) +
    ggplot2::xlab('Destination') +
    ggplot2::ylab('Origin') +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(size = 10, angle = 90, vjust = 0.5),
      axis.text.y  = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 12, margin = ggplot2::margin(t = 15)),
      axis.title.y = ggplot2::element_text(size = 12, margin = ggplot2::margin(r = 15)),
      legend.position = 'bottom'
    ) +
    viridis::scale_fill_viridis(option = "G", direction = 1) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title = 'Probability of daily travel given departure from origin',
        title.position = 'top',
        label.theme = ggplot2::element_text(size = 9),
        barwidth = 20,
        barheight = 0.5,
        frame.colour = 'black',
        ticks = TRUE
      )
    )

  print(p)
  invisible(p)
}
