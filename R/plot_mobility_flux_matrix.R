#' Plot the model-implied daily mobility flux matrix (\eqn{M_{ij}})
#'
#' Heatmap of the model's implied daily flux
#' \eqn{M_{ij} = N_i \tau_i \pi_{ij}} (\code{\link{calc_mobility_flux}} element
#' \code{$flux}) — the expected number of travelers per day from origin \eqn{i}
#' to destination \eqn{j}. This is the modeled flight-matrix panel of the
#' \code{"spatial"} figure group in \code{\link{render_MOSAIC_figures}}.
#'
#' The aesthetic matches the observed flight-matrix (M) panel of
#' \code{\link{plot_mobility}} exactly (\code{theme_bw()}, black->skyblue
#' \code{log(value + 1)} fill with the \code{0/10/100/1,000/10,000} colorbar,
#' bottom legend). Only the data and colorbar title differ: this plots the
#' model's \emph{implied} flux from its calibrated parameters, so the title
#' reads "Model-implied mean daily trips". Axes are ordered south-to-north by
#' latitude via a keyed \code{factor(levels = )} over the named matrix. The
#' diagonal is \code{NA} (no self-travel) and is rendered as \code{grey90}.
#'
#' @param flux Numeric J x J flux matrix from \code{\link{calc_mobility_flux}}
#'   (element \code{$flux}), diagonal \code{NA}.
#' @param location_name Character vector of length J, config order. Defaults to
#'   \code{rownames(flux)}.
#' @param latitude Numeric vector of length J giving each location's latitude in
#'   the row order of \code{flux}; used to order the axes south-to-north
#'   (matching \code{\link{plot_mobility}}). When \code{NULL} (default) the axes
#'   are left in the order of \code{location_name}.
#'
#' @return Invisibly, the \pkg{ggplot2} object.
#'
#' @seealso \code{\link{calc_mobility_flux}}, \code{\link{plot_mobility}}.
#' @importFrom ggplot2 ggplot aes geom_tile xlab ylab theme_bw theme element_text margin scale_fill_gradient guide_colorbar
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
plot_mobility_flux_matrix <- function(flux, location_name = rownames(flux),
                                      latitude = NULL) {

  if (!is.matrix(flux) || !is.numeric(flux))
    stop("`flux` must be a numeric matrix.")
  J <- nrow(flux)
  if (is.null(location_name)) location_name <- paste0("loc_", seq_len(J))
  if (length(location_name) != J)
    stop("`location_name` length (", length(location_name),
         ") must match nrow(flux) (", J, ").")

  loc <- as.character(location_name)

  # Order south-to-north by latitude when supplied (mirrors plot_mobility.R:56).
  # Keyed factor over the NAMED matrix: values follow their labels (F3-safe).
  if (!is.null(latitude)) {
    if (length(latitude) != J)
      stop("`latitude` length must match nrow(flux).")
    iso3_ordered <- loc[order(as.numeric(latitude))]
  } else {
    iso3_ordered <- loc
  }

  dimnames(flux) <- list(loc, loc)

  # Adaptive colorbar breaks fit to this run's modeled flux range (the fixed
  # observed-OAG 0/10/.../10,000 ticks fall off a smaller modeled range).
  br <- .mosaic_flux_log_breaks(as.numeric(flux))

  p <- ggplot2::ggplot(
    data = reshape2::melt(flux, varnames = c("origin", "destination"),
                          value.name = "value")
  ) +
    ggplot2::geom_tile(
      ggplot2::aes(
        x    = factor(.data$destination, levels = iso3_ordered),
        y    = factor(.data$origin, levels = iso3_ordered),
        fill = log(.data$value + 1)
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
    ggplot2::scale_fill_gradient(
      low      = "black",
      high     = "skyblue",
      na.value = "grey90",
      breaks   = br$breaks,
      labels   = br$labels,
      guide    = ggplot2::guide_colorbar(
        title = 'Model-implied mean daily trips',
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
