#' Plot the model-implied mobility flux network
#'
#' Network map of the model's implied daily mobility flux
#' \eqn{M_{ij} = N_i \tau_i \pi_{ij}} (\code{\link{calc_mobility_flux}}): nodes
#' are placed at each location's centroid, and edges connect origin-destination
#' pairs colored and sized by flux. This is the network panel of the
#' \code{"spatial"} figure group in \code{\link{render_MOSAIC_figures}}.
#'
#' The aesthetic matches the mobility-network panel of
#' \code{\link{plot_mobility}} exactly (\code{theme_void()}, grey Africa
#' backdrop, viridis "B" edge color on \code{log(count + 1)} with the
#' \code{0/10/100/1,000/10,000} colorbar, white \code{shape = 21} nodes,
#' \code{ggrepel} ISO3 labels, and the extract-legend + \code{grid.arrange}
#' recombination). Only the data and colorbar title differ: this plots the
#' model's \emph{implied} flux (title "Model-implied mean daily trips") rather
#' than the \emph{observed} OAG flights, and shows \strong{all} edges above a
#' small threshold (\code{count > 1}) rather than a top-fraction subset.
#'
#' When a \code{basemap} (an \pkg{sf} polygon layer) is supplied it is drawn as
#' a backdrop in full, so neighboring countries appear as reference outlines,
#' and the view is then cropped (via \code{\link[ggplot2]{coord_sf}}) to the
#' bounding box of the network countries' polygons (those whose \code{iso3}
#' matches \code{location_name}), expanded by a small margin. The renderer
#' passes the packaged low-resolution Africa ADM0 layer
#' (\code{inst/extdata/africa_adm0_lowres.geojson}) — it never calls a
#' GeoBoundaries / \code{\link{get_country_shp}} API (P5). When \code{basemap}
#' is \code{NULL} (subnational / no ISO match) the view falls back to the node
#' bounding box.
#'
#' @param flux Numeric J x J flux matrix from \code{\link{calc_mobility_flux}}
#'   (element \code{$flux}), diagonal \code{NA}.
#' @param coords J x 2 numeric matrix of \code{longitude}, \code{latitude} in the
#'   same row order as \code{flux} (element \code{$coords} of
#'   \code{\link{calc_mobility_flux}}).
#' @param location_name Character vector of length J, config order. Defaults to
#'   \code{rownames(flux)}.
#' @param basemap Optional \pkg{sf} object drawn as a polygon backdrop. When
#'   \code{NULL} (default), the network is drawn without a continental backdrop.
#'
#' @return Invisibly, the combined legend+map grob
#'   (\code{gridExtra::grid.arrange} output), or the single \pkg{ggplot2} object
#'   when \pkg{gridExtra} is unavailable.
#'
#' @seealso \code{\link{calc_mobility_flux}}, \code{\link{plot_mobility}}.
#' @importFrom ggplot2 ggplot aes geom_sf geom_segment geom_point scale_color_viridis_c scale_size_continuous theme_void theme element_text guide_colorbar coord_sf
#' @import ggplot2
#' @export
plot_mobility_flux_network <- function(flux,
                                       coords,
                                       location_name = rownames(flux),
                                       basemap       = NULL) {

  if (!is.matrix(flux) || !is.numeric(flux))
    stop("`flux` must be a numeric matrix.")
  J <- nrow(flux)
  if (is.null(location_name)) location_name <- paste0("loc_", seq_len(J))
  if (!is.matrix(coords) || nrow(coords) != J || ncol(coords) < 2L)
    stop("`coords` must be a J x 2 matrix of longitude/latitude.")
  if (length(location_name) != J)
    stop("`location_name` length must match nrow(flux).")

  loc <- as.character(location_name)
  lon <- as.numeric(coords[, 1L])
  lat <- as.numeric(coords[, 2L])

  # Node table mirrors plot_mobility's `lonlat` (iso3, lon, lat).
  lonlat <- data.frame(iso3 = loc, lon = lon, lat = lat,
                       stringsAsFactors = FALSE)

  # Edge list from off-diagonal finite flux; keep all above a small threshold
  # (count > 1), drawn in ascending order so strong edges render on top.
  min_trips <- 1
  idx <- which(is.finite(flux) & flux > min_trips, arr.ind = TRUE)
  melt_M <- data.frame(
    origin          = loc[idx[, 1L]],
    destination     = loc[idx[, 2L]],
    count           = flux[idx],
    origin_lon      = lon[idx[, 1L]],
    origin_lat      = lat[idx[, 1L]],
    destination_lon = lon[idx[, 2L]],
    destination_lat = lat[idx[, 2L]],
    stringsAsFactors = FALSE
  )
  melt_M <- melt_M[order(melt_M$count), , drop = FALSE]

  # Adaptive colorbar breaks fit to this run's modeled flux range (shared with
  # plot_mobility_flux_matrix; replaces the fixed observed-OAG 0/.../10,000).
  br <- .mosaic_flux_log_breaks(melt_M$count)

  # Crop window. With a basemap, crop the view to the bounding box of the
  # network countries' polygons (iso3 %in% location_name), so neighbors fall in
  # as reference outlines but the view stays on the network region. Without a
  # basemap (subnational / no ISO match), crop to the node bounding box.
  margin_frac <- 0.05    # expand each span by 5%
  margin_min  <- 0.5     # ...but at least ~0.5 degrees
  crop_box <- NULL
  if (!is.null(basemap) && "iso3" %in% names(basemap)) {
    net_poly <- basemap[basemap$iso3 %in% loc, , drop = FALSE]
    if (nrow(net_poly) > 0L) {
      bb <- sf::st_bbox(net_poly)
      crop_box <- c(xmin = bb[["xmin"]], xmax = bb[["xmax"]],
                    ymin = bb[["ymin"]], ymax = bb[["ymax"]])
    }
  }
  if (is.null(crop_box)) {
    crop_box <- c(xmin = min(lon), xmax = max(lon),
                  ymin = min(lat), ymax = max(lat))
  }
  dx <- max((crop_box[["xmax"]] - crop_box[["xmin"]]) * margin_frac, margin_min)
  dy <- max((crop_box[["ymax"]] - crop_box[["ymin"]]) * margin_frac, margin_min)
  xlim_crop <- c(crop_box[["xmin"]] - dx, crop_box[["xmax"]] + dx)
  ylim_crop <- c(crop_box[["ymin"]] - dy, crop_box[["ymax"]] + dy)

  # Base map + colorbar pass (plot_mobility.R:143-176)
  p2 <- ggplot2::ggplot()
  if (!is.null(basemap)) {
    p2 <- p2 + ggplot2::geom_sf(data = basemap, fill = "#E0E0E0", color = "black")
  }
  p2 <- p2 +
    ggplot2::geom_segment(data = melt_M,
                          ggplot2::aes(x = .data$origin_lon,
                                       y = .data$origin_lat,
                                       xend = .data$destination_lon,
                                       yend = .data$destination_lat,
                                       color = log(.data$count + 1)),
                          linewidth = 0.8) +
    ggplot2::scale_color_viridis_c(
      option = "B",
      direction = -1,
      breaks = br$breaks,
      labels = br$labels,
      guide = ggplot2::guide_colorbar(
        title = "Model-implied mean daily trips",
        title.position = "top",
        barwidth = 20,
        barheight = 0.6,
        ticks = TRUE,
        frame.colour = "black",
        ticks.colour = "black"
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text  = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 12),
      legend.key.width = grid::unit(3, "cm")
    )

  # Extract the legend (MOSAIC helper, as in plot_mobility).
  legend <- MOSAIC::get_ggplot_legend(p2)

  # Second pass: linewidth-mapped segments + nodes + labels (plot_mobility.R:182-198)
  p2 <- p2 +
    ggplot2::geom_segment(data = melt_M,
                          ggplot2::aes(x = .data$origin_lon,
                                       y = .data$origin_lat,
                                       xend = .data$destination_lon,
                                       yend = .data$destination_lat,
                                       linewidth = log(.data$count + 1),
                                       color = log(.data$count + 1))) +
    ggplot2::geom_point(data = lonlat,
                        ggplot2::aes(x = .data$lon, y = .data$lat),
                        color = "black", fill = 'white',
                        shape = 21, size = 2.5) +
    ggrepel::geom_text_repel(data = lonlat,
                             ggplot2::aes(x = .data$lon, y = .data$lat,
                                          label = .data$iso3),
                             size = 3.25, max.overlaps = Inf,
                             bg.color = "white", bg.r = 0.15,
                             point.padding = 0.3, box.padding = 0.4,
                             min.segment.length = 0, segment.size = 0.2,
                             segment.color = "grey40") +
    ggplot2::scale_size_continuous(range = c(0.001, 2.5)) +
    ggplot2::theme(legend.position = 'none')

  # Crop the view AFTER the legend extraction and second pass so coord_sf
  # survives the grid.arrange legend recombination. With a basemap (an sf
  # layer in the plot) coord_sf governs the panel; without one it still clips
  # the segment/point layers to the node bbox window.
  p2 <- p2 + ggplot2::coord_sf(xlim = xlim_crop, ylim = ylim_crop,
                               expand = FALSE)

  if (requireNamespace("gridExtra", quietly = TRUE)) {
    combined_plot <- gridExtra::grid.arrange(
      grobs = list(p2, legend),
      ncol = 1,
      heights = c(0.9, 0.1)
    )
    return(invisible(combined_plot))
  }

  print(p2)
  invisible(p2)
}
