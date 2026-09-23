#' Publication figures for the fused overland OD connectivity model
#'
#' Four figures that together tell the fused-OD story: how the sources combine,
#' what that changes structurally, what it changes in amplitude, and what it
#' looks like geographically.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}.
#' @param suffix Which \code{\link{est_mobility}} run to read for the fused
#'   quantities. Default \code{"_fused_raked_tt"}.
#' @param out_dir Directory for the PNGs. Default \code{PATHS$DOCS_FIGURES}.
#' @param verbose Print each file written.
#'
#' @return Invisibly, a character vector of the files written.
#'
#' @section Figures:
#' \describe{
#'   \item{\code{fused_od_1_sources.png}}{Small multiples of the four
#'     row-normalised source matrices and the fused result, shared sequential
#'     scale. Shows what each source contributes.}
#'   \item{\code{fused_od_2_landneighbour_share.png}}{Per-country share of
#'     outflow reaching a land neighbour, air vs fused. The single scalar that
#'     captures what the method fixes.}
#'   \item{\code{fused_od_3_tau_prior.png}}{Daily departure probability, air fit
#'     vs overland prior with 95% intervals, log scale.}
#'   \item{\code{fused_od_4_corridors.png}}{Dominant destination per origin drawn
#'     on the map, air vs fused.}
#' }
#'
#' @section Design:
#' Colour follows the data's job: a single-hue sequential ramp for magnitude
#' (heatmaps), and two categorical hues for the air-vs-fused contrast. The
#' categorical pair was validated for colour-vision deficiency (worst-pair
#' protan dE 24.7, normal-vision dE 33.6, both well clear of the 8 / 15 floors),
#' and every two-series panel also carries position or direct labels so identity
#' never rests on colour alone. Countries are ordered by latitude in the
#' heatmaps, matching \code{\link{plot_mobility}}, so the diagonal band is
#' geographic adjacency.
#'
#' @importFrom utils read.csv
#' @export
plot_mobility_fused <- function(PATHS,
                                suffix  = "_fused_raked_tt",
                                out_dir = NULL,
                                verbose = TRUE) {

     if (is.null(out_dir)) out_dir <- PATHS$DOCS_FIGURES
     .mosaic_check_suffix(suffix, is_default = FALSE)
     # Route WRITES, not just reads. Previously these four were string
     # literals, so two runs with different `suffix` silently overwrote each
     # other's figures -- the same defect class as the est_mobility one.
     .fig <- function(f) .mosaic_suffix_path(file.path(out_dir, f), suffix)
     dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
     gg <- ggplot2::ggplot

     # ---- design tokens (validated reference palette) ----------------------
     SEQ      <- c("#cde2fb", "#9ec5f4", "#5598e7", "#2a78d6", "#256abf", "#184f95", "#0d366b")
     AIR      <- "#eb6834"   # categorical slot 2
     FUSED    <- "#2a78d6"   # categorical slot 1
     INK      <- "#0b0b0b"
     INK2     <- "#52514e"
     SURFACE  <- "#fcfcfb"
     GRID     <- "#e6e5e1"

     base_theme <- ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
               plot.background   = ggplot2::element_rect(fill = SURFACE, colour = NA),
               panel.background  = ggplot2::element_rect(fill = SURFACE, colour = NA),
               panel.grid.major  = ggplot2::element_line(colour = GRID, linewidth = 0.3),
               panel.grid.minor  = ggplot2::element_blank(),
               plot.title        = ggplot2::element_text(face = "bold", size = 13, colour = INK),
               plot.subtitle     = ggplot2::element_text(size = 10, colour = INK2,
                                                         margin = ggplot2::margin(b = 8)),
               plot.caption      = ggplot2::element_text(size = 8, colour = INK2, hjust = 0),
               axis.text         = ggplot2::element_text(colour = INK2),
               axis.title        = ggplot2::element_text(colour = INK2),
               legend.title       = ggplot2::element_text(colour = INK2, size = 9),
               legend.text        = ggplot2::element_text(colour = INK2, size = 9),
               strip.text        = ggplot2::element_text(face = "bold", colour = INK, size = 9,
                                                         lineheight = 1.25)
          )

     mob <- file.path(PATHS$DATA_PROCESSED, "mobility")
     rd  <- function(f) {
          m <- as.matrix(utils::read.csv(f, row.names = 1, check.names = FALSE))
          colnames(m) <- gsub("^X", "", colnames(m)); m
     }
     written <- character(0)

     # latitude ordering, matching plot_mobility()
     lonlat <- utils::read.csv(file.path(PATHS$MODEL_INPUT,
                                         paste0("mobility_lon_lat", suffix, ".csv")))
     iso_ord <- lonlat$iso3[order(lonlat$lat)]

     # ======================================================================
     # FIG 1 - the four sources and the fused result
     # ======================================================================
     srcs <- c(desa = "UN DESA migrant stock", abel_cohen = "Abel & Cohen flows",
               sci = "Meta social connectedness", contiguity = "Land contiguity")
     panels <- list()
     for (s in names(srcs)) {
          f <- file.path(mob, sprintf("M_structure_%s.csv", s))
          if (file.exists(f)) panels[[srcs[[s]]]] <- rd(f)
     }
     f_fused <- file.path(mob, "M_structure_fused.csv")
     if (file.exists(f_fused)) panels[["Fused (weighted mean)"]] <- rd(f_fused)

     long <- do.call(rbind, lapply(names(panels), function(nm) {
          m <- panels[[nm]]
          keep <- intersect(iso_ord, rownames(m))
          m <- m[keep, keep, drop = FALSE]
          data.frame(panel = nm,
                     origin = rep(rownames(m), times = ncol(m)),
                     dest   = rep(colnames(m), each = nrow(m)),
                     value  = as.vector(m), stringsAsFactors = FALSE)
     }))
     # Put the decisive number in the strip: how much of each source's mass
     # lands on a shared-border pair. That is the whole reason the sources are
     # blended, and it is invisible in 1,600 cells of heatmap.
     contig0 <- rd(file.path(mob, "M_structure_contiguity.csv")) > 0
     lab <- vapply(names(panels), function(nm) {
          m <- panels[[nm]]; k <- intersect(rownames(m), rownames(contig0))
          m <- m[k, k]; diag(m) <- 0
          sprintf("%s\n%d pairs  -  %.0f%% to land neighbours",
                  nm, sum(m > 0), 100 * sum(m * contig0[k, k]) / sum(m))
     }, "")
     long$panel <- unname(lab[match(long$panel, names(panels))])
     long$panel <- factor(long$panel, levels = unname(lab))
     long$origin <- factor(long$origin, levels = iso_ord)
     long$dest   <- factor(long$dest,   levels = iso_ord)
     long$value[long$value <= 0] <- NA        # true zeros recede to the surface

     p1 <- gg(long, ggplot2::aes(dest, origin, fill = value)) +
          ggplot2::geom_raster() +
          ggplot2::facet_wrap(~panel, nrow = 1) +
          # log10, not sqrt: row-normalising over 40 destinations puts the
          # typical cell near 0.025, so a linear or sqrt ramp collapses every
          # panel into the same pale wash and hides the structural difference
          # between the sparse sources and the dense SCI one.
          ggplot2::scale_fill_gradientn(
               colours = SEQ, na.value = SURFACE, trans = "log10",
               limits  = c(1e-3, 0.5), oob = scales::squish,
               breaks  = c(1e-3, 1e-2, 1e-1, 0.5),
               labels  = c("0.1%", "1%", "10%", ">=50%"),
               name    = "Share of origin's outflow") +
          ggplot2::coord_equal() +
          ggplot2::labs(
               title    = "Four bilateral sources fuse into one connectivity structure",
               subtitle = paste("Each panel is row-normalised: a row is one origin's distribution over destinations.",
                                "\nCountries ordered south to north, so the diagonal band is geographic adjacency."),
               x = "Destination", y = "Origin",
               caption = paste("Weights: UN DESA 0.30 + Abel-Cohen 0.20 (one migration block, the two are not independent)",
                               "+ Meta SCI 0.35 + contiguity 0.15.",
                               "\nColour is log-scaled and clipped at 0.1% and 50%; blank cells are true zeros.",
                               "Social connectedness is the long-range source - densest, and least",
                               "\nconcentrated on borders - so it supplies the links migration data misses.")) +
          base_theme +
          ggplot2::theme(axis.text = ggplot2::element_blank(),
                         panel.grid = ggplot2::element_blank(),
                         legend.position = "bottom",
                         legend.key.width = grid::unit(46, "pt"),
                         legend.key.height = grid::unit(8, "pt"))

     f1 <- .fig("fused_od_1_sources.png")
     ggplot2::ggsave(f1, p1, width = 15, height = 5.2, dpi = 300, bg = SURFACE)
     written <- c(written, f1)

     # ======================================================================
     # FIG 2 - land-neighbour share, air vs fused
     # ======================================================================
     contig <- rd(file.path(mob, "M_structure_contiguity.csv")) > 0
     pi_air <- rd(file.path(PATHS$MODEL_INPUT, "mobility_pi.csv"))
     pi_fus <- rd(file.path(PATHS$MODEL_INPUT, paste0("mobility_pi", suffix, ".csv")))
     iso <- Reduce(intersect, list(rownames(contig), rownames(pi_air), rownames(pi_fus)))

     share <- function(P) {
          P <- P[iso, iso]; C <- contig[iso, iso]
          diag(P) <- 0
          rowSums(P * C, na.rm = TRUE) / pmax(rowSums(P, na.rm = TRUE), 1e-12)
     }
     d2 <- data.frame(iso = iso, air = share(pi_air), fused = share(pi_fus),
                      stringsAsFactors = FALSE)
     d2 <- d2[order(d2$fused), ]
     d2$iso <- factor(d2$iso, levels = d2$iso)

     p2 <- gg(d2) +
          ggplot2::geom_segment(ggplot2::aes(y = iso, yend = iso, x = air, xend = fused),
                                colour = GRID, linewidth = 1.6, lineend = "round") +
          ggplot2::geom_point(ggplot2::aes(x = air,   y = iso, colour = "Air (OAG flights)"), size = 2.4) +
          ggplot2::geom_point(ggplot2::aes(x = fused, y = iso, colour = "Fused overland"),    size = 2.4) +
          ggplot2::scale_colour_manual(values = c("Air (OAG flights)" = AIR,
                                                  "Fused overland"    = FUSED), name = NULL) +
          ggplot2::scale_x_continuous(labels = function(x) paste0(round(x * 100), "%"),
                                      limits = c(0, 1)) +
          ggplot2::labs(
               title    = "Overland fusion moves connectivity onto land neighbours",
               subtitle = paste0("Share of each country's outbound connectivity that reaches a country it shares a land border with.",
                                 "\nMedian rises from ", round(100 * stats::median(d2$air)), "% (air) to ",
                                 round(100 * stats::median(d2$fused)), "% (fused)."),
               x = "Share of outflow to a land neighbour", y = NULL,
               caption = "Air routing concentrates on hub airports; overland routing follows borders.") +
          base_theme +
          ggplot2::theme(legend.position = "top",
                         panel.grid.major.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_text(size = 7))

     f2 <- .fig("fused_od_2_landneighbour_share.png")
     ggplot2::ggsave(f2, p2, width = 7.5, height = 9, dpi = 300, bg = SURFACE)
     written <- c(written, f2)

     # ======================================================================
     # FIG 3 - departure probability: air fit vs overland prior
     # ======================================================================
     # air comparator stays unsuffixed by design; the fitted series must be
     # the suffixed run, otherwise this figure plots the rake TARGET and is
     # structurally unable to reveal a target-vs-fit divergence.
     ta <- utils::read.csv(file.path(PATHS$MODEL_INPUT, "param_tau_departure.csv"))
     ta <- ta[ta$parameter_name == "mean", c("i", "parameter_value")]
     names(ta) <- c("iso", "air")
     tv <- utils::read.csv(file.path(PATHS$MODEL_INPUT, "param_tau_departure_overland.csv"))
     d3 <- merge(ta, tv[, c("iso_code", "tau_daily", "ci_lo", "ci_hi", "evidence")],
                 by.x = "iso", by.y = "iso_code")
     d3 <- d3[order(d3$tau_daily), ]
     d3$iso <- factor(d3$iso, levels = d3$iso)
     d3$evidence <- factor(ifelse(d3$evidence == "E3", "Country-specific evidence",
                                  "Regional default (wider prior)"),
                           levels = c("Country-specific evidence", "Regional default (wider prior)"))

     p3 <- gg(d3) +
          ggplot2::geom_linerange(ggplot2::aes(y = iso, xmin = ci_lo, xmax = ci_hi),
                                  colour = FUSED, alpha = 0.35, linewidth = 1.8) +
          ggplot2::geom_point(ggplot2::aes(x = air, y = iso, colour = "Air fit (current prior)"),
                              size = 2.2) +
          ggplot2::geom_point(ggplot2::aes(x = tau_daily, y = iso, colour = "Overland prior (mean, 95% CI)"),
                              size = 2.2) +
          ggplot2::facet_grid(evidence ~ ., scales = "free_y", space = "free_y", switch = "y") +
          ggplot2::scale_colour_manual(values = c("Air fit (current prior)" = AIR,
                                                  "Overland prior (mean, 95% CI)" = FUSED),
                                       name = NULL) +
          ggplot2::scale_x_log10() +
          ggplot2::labs(
               title    = "Overland departure rates sit about an order of magnitude above air",
               subtitle = paste0("Daily probability a person leaves their country. Median ",
                                 signif(stats::median(d3$air), 2), " (air) vs ",
                                 signif(stats::median(d3$tau_daily), 2),
                                 " (overland) - a ", round(stats::median(d3$tau_daily) / stats::median(d3$air)),
                                 "x lift.\nBars are the prior's 95% interval, deliberately wide: this is a calibration prior, not a measurement."),
               x = "Daily departure probability (log scale)", y = NULL,
               caption = paste0("Overland values from border-throughput and IOM DTM evidence (MOSAIC-OCV E3).\n",
                                "Countries without country-specific evidence take the regional median\n",
                                "with a deliberately wider interval.")) +
          base_theme +
          ggplot2::theme(legend.position = "top",
                         panel.grid.major.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_text(size = 7),
                         strip.placement = "outside",
                         strip.text.y.left = ggplot2::element_text(angle = 90, size = 8))

     f3 <- .fig("fused_od_3_tau_prior.png")
     ggplot2::ggsave(f3, p3, width = 8, height = 9.8, dpi = 300, bg = SURFACE)
     written <- c(written, f3)

     # ======================================================================
     # FIG 4 - dominant corridor per origin, air vs fused, on the map
     # ======================================================================
     afr <- tryCatch(sf::st_read(file.path(PATHS$DATA_SHAPEFILES, "AFRICA_ADM0.shp"), quiet = TRUE),
                     error = function(e) NULL)
     if (!is.null(afr)) {
          xy <- stats::setNames(as.data.frame(lonlat[, c("lon", "lat")]), c("lon", "lat"))
          rownames(xy) <- lonlat$iso3

          top1 <- function(P, lab) {
               P <- P[iso, iso]; diag(P) <- 0
               j <- apply(P, 1, function(r) colnames(P)[which.max(r)])
               data.frame(panel = lab, origin = iso, dest = unname(j),
                          x = xy[iso, "lon"], y = xy[iso, "lat"],
                          xend = xy[unname(j), "lon"], yend = xy[unname(j), "lat"],
                          w = apply(P, 1, max), stringsAsFactors = FALSE)
          }
          d4 <- rbind(top1(pi_air, "Air (OAG flights)"), top1(pi_fus, "Fused overland"))
          d4$panel <- factor(d4$panel, levels = c("Air (OAG flights)", "Fused overland"))

          p4 <- gg() +
               ggplot2::geom_sf(data = afr, fill = "#f3f2ef", colour = "#dedcd7", linewidth = 0.2) +
               ggplot2::geom_curve(data = d4,
                                   ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
                                                colour = panel, linewidth = w),
                                   curvature = 0.18, alpha = 0.75,
                                   arrow = grid::arrow(length = grid::unit(4, "pt"), type = "closed")) +
               ggplot2::geom_point(data = unique(d4[, c("x", "y")]),
                                   ggplot2::aes(x, y), size = 0.7, colour = INK2) +
               ggplot2::facet_wrap(~panel) +
               ggplot2::scale_colour_manual(values = c("Air (OAG flights)" = AIR,
                                                       "Fused overland" = FUSED), guide = "none") +
               ggplot2::scale_linewidth_continuous(range = c(0.3, 1.8), name = "Share of outflow") +
               ggplot2::coord_sf(xlim = range(xy$lon) + c(-3, 3),
                                 ylim = range(xy$lat) + c(-3, 3), expand = FALSE) +
               ggplot2::labs(
                    title    = "Where each country sends most of its travellers",
                    subtitle = "Single strongest destination per origin. Air picks distant hubs; overland picks the country next door.",
                    x = NULL, y = NULL,
                    caption = "Arrow width is the share of that origin's outflow captured by its top destination.") +
               base_theme +
               ggplot2::theme(axis.text = ggplot2::element_blank(),
                              panel.grid = ggplot2::element_blank(),
                              legend.position = "bottom")

          f4 <- .fig("fused_od_4_corridors.png")
          ggplot2::ggsave(f4, p4, width = 11, height = 6.4, dpi = 300, bg = SURFACE)
          written <- c(written, f4)
     }

     if (verbose) for (f in written) message("  wrote ", f)
     invisible(written)
}
