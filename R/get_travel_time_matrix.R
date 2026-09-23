#' Build an overland least-cost travel-time matrix between countries
#'
#' Downloads (and caches) the Malaria Atlas Project motorized friction surface
#' and computes least-cost accumulated travel time, in \strong{hours}, between
#' country centroids. This is the geometry layer for
#' \code{est_mobility(distance_metric = "travel_time")} -- an overland
#' effective distance that replaces great-circle kilometres.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}.
#' @param iso_codes ISO3 codes. Defaults to \code{MOSAIC::iso_codes_mosaic}.
#' @param aggregate_factor Integer cell aggregation applied to the ~1 km
#'   friction raster before building the transition graph. Default \code{6}
#'   (~5 km). See "Resolution" below -- this is the single knob trading
#'   accuracy against memory.
#' @param aggregate_fun How to combine friction within an aggregated block.
#'   \strong{\code{"min"} (default) is the correct choice} and \code{"mean"}
#'   is offered only for comparison. Roads are thin linear low-friction
#'   features; averaging a block smears them into the surrounding landscape and
#'   destroys the network. Measured on a West-African tile, \code{fact = 6}
#'   gives a median friction of 0.00120 min/m under \code{min} versus 0.00914
#'   under \code{mean} -- a ~7.6x difference that propagates straight into
#'   travel time. Using \code{mean} produced a 369 h median country-to-country
#'   time and NGA-NER at 181 h, against ~56 h from the E3 reference build.
#' @param dataset_id MAP raster id. Default
#'   \code{"Accessibility__202001_Global_Motorized_Friction_Surface"} (the 2019
#'   v5.1 surface, published 2020-01).
#' @param cache If \code{TRUE} (default), reuse a previously built matrix and
#'   the cached friction raster instead of refetching/recomputing.
#' @param verbose Print progress.
#'
#' @return Invisibly, a square numeric matrix of travel time in HOURS,
#'   dimnames = ISO3, diagonal 0. Also written to
#'   \code{processed/mobility/D_traveltime_hours.csv}.
#'
#' @section Method:
#' Mirrors the MOSAIC-OCV E3 Route-2b recipe: aggregate the friction surface,
#' \code{gdistance::transition(1/mean(x), directions = 8)} to get conductance,
#' \code{geoCorrection(type = "c")} to scale conductance by true inter-cell
#' distance in metres, then \code{costDistance} between centroids. Friction is
#' in \strong{minutes per metre}, so accumulated cost is in minutes; divided by
#' 60 for hours.
#'
#' @section Resolution (read before changing \code{aggregate_factor}):
#' E3 used \code{aggregate_factor = 3} (~2.5 km) but ran on \strong{10-country
#' regional} extents (~960x1360 cells). A continental Africa extent at the same
#' factor is ~2800x2920 = 8.2M cells, and \code{gdistance} builds a sparse
#' transition matrix with 8 neighbours per cell -- roughly 65M non-zeros, which
#' will exhaust memory on most hosts. The default \code{6} (~5 km, ~2.0M cells)
#' is the continental-scale compromise. Country-to-country distances here are
#' hundreds of km, so 5 km cells are adequate for national centroids; drop to 3
#' only for a regional subset.
#'
#' @section Deviation from E3:
#' E3 used \strong{population-weighted} centroids (WorldPop). This uses the
#' geometric centroids MOSAIC already computes via \code{\link{get_centroid}},
#' the same ones the great-circle path uses -- so air and travel-time distances
#' are strictly comparable. For large countries with off-centre populations
#' (e.g. COD, TCD) a population-weighted centroid would shift the origin point
#' meaningfully; that is a known, unimplemented refinement.
#'
#' @seealso \code{\link{est_mobility}}, \code{\link{process_mobility_od_data}}
#'
#' @importFrom utils write.csv read.csv
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' D_hours <- get_travel_time_matrix(PATHS)
#' est_mobility(PATHS, od_source = "fused", distance_metric = "travel_time")
#' }
get_travel_time_matrix <- function(PATHS,
                                   iso_codes        = NULL,
                                   aggregate_factor = 6L,
                                   aggregate_fun    = c("min", "mean"),
                                   dataset_id       = "Accessibility__202001_Global_Motorized_Friction_Surface",
                                   cache            = TRUE,
                                   verbose          = TRUE) {

     # Literal requireNamespace() calls, not a loop: R CMD check's static
     # scanner cannot see names supplied through a variable, which is why
     # these Suggests went undeclared and undetected. `raster` and `sf` are in
     # Imports, so they need no guard.
     if (!requireNamespace("malariaAtlas", quietly = TRUE)) {
          stop("Package 'malariaAtlas' is required to fetch the friction surface.\n",
               "  install.packages('malariaAtlas')", call. = FALSE)
     }
     if (!requireNamespace("gdistance", quietly = TRUE)) {
          stop("Package 'gdistance' is required for least-cost travel time.\n",
               "  install.packages('gdistance')", call. = FALSE)
     }
     aggregate_fun <- match.arg(aggregate_fun)
     if (is.null(iso_codes)) {
          iso_codes <- get("iso_codes_mosaic", envir = asNamespace("MOSAIC"))
     }
     iso_codes <- sort(unique(toupper(iso_codes)))

     out_dir <- file.path(PATHS$DATA_PROCESSED, "mobility")
     dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
     # Key the matrix on everything that changes it. Previously a single
     # unkeyed D_traveltime_hours.csv + setequal(rownames, iso) meant a call
     # with a different aggregate_fun returned the OTHER matrix, silently.
     f_out <- file.path(out_dir, sprintf("D_traveltime_hours_agg%d_%s.csv",
                                         as.integer(aggregate_factor), aggregate_fun))

     if (cache && file.exists(f_out)) {
          D <- as.matrix(utils::read.csv(f_out, row.names = 1, check.names = FALSE))
          if (setequal(rownames(D), iso_codes)) {
               if (verbose) message("Using cached travel-time matrix: ", basename(f_out))
               return(invisible(D[iso_codes, iso_codes, drop = FALSE]))
          }
          if (verbose) message("Cached matrix has different ISO coverage; rebuilding.")
     }

     # ---- centroids (same source as the great-circle path) -----------------
     shapefiles <- file.path(PATHS$DATA_SHAPEFILES, paste0(iso_codes, "_ADM0.shp"))
     shapefiles <- shapefiles[file.exists(shapefiles)]
     if (length(shapefiles) < 2L) {
          stop("Need >=2 country shapefiles in ", PATHS$DATA_SHAPEFILES, call. = FALSE)
     }
     cent <- do.call(rbind, lapply(shapefiles, MOSAIC::get_centroid))
     cent <- cent[cent$iso3 %in% iso_codes, , drop = FALSE]
     cent <- cent[order(cent$iso3), , drop = FALSE]
     if (verbose) message(glue::glue("Centroids for {nrow(cent)} countries"))

     # ---- friction surface --------------------------------------------------
     f_ras <- file.path(PATHS$DATA_RAW, "friction",
                        sprintf("%s_agg%d_%s.tif", gsub("[^A-Za-z0-9]", "_", dataset_id),
                                as.integer(aggregate_factor), aggregate_fun))
     dir.create(dirname(f_ras), recursive = TRUE, showWarnings = FALSE)

     if (cache && file.exists(f_ras)) {
          if (verbose) message("Using cached friction raster: ", basename(f_ras))
          fr <- raster::raster(f_ras)
     } else {
          # Pad the union of the country POLYGONS, not the centroid hull.
          # Centroid-padding clipped 8 of 40 countries, MRT by ~570 km and
          # ZAF by ~428 km, putting a hard raster wall inside their territory.
          pad <- 2
          bxs <- lapply(shapefiles, function(f)
               sf::st_bbox(suppressMessages(sf::st_read(f, quiet = TRUE))))
          bb <- c(xmin = min(vapply(bxs, `[[`, 0, "xmin")) - pad,
                  xmax = max(vapply(bxs, `[[`, 0, "xmax")) + pad,
                  ymin = min(vapply(bxs, `[[`, 0, "ymin")) - pad,
                  ymax = max(vapply(bxs, `[[`, 0, "ymax")) + pad)
          if (verbose) {
               message(glue::glue("Fetching MAP friction surface over bbox ",
                                  "[{round(bb['xmin'],1)}, {round(bb['ymin'],1)}] .. ",
                                  "[{round(bb['xmax'],1)}, {round(bb['ymax'],1)}] ..."))
          }
          shp <- sf::st_as_sfc(sf::st_bbox(bb, crs = 4326))
          r <- malariaAtlas::getRaster(dataset_id = dataset_id, shp = sf::as_Spatial(shp))
          fr <- raster::raster(r)
          if (aggregate_factor > 1L) {
               if (verbose) {
                    message(glue::glue("Aggregating x{aggregate_factor} (fun = {aggregate_fun}) ..."))
               }
               fr <- raster::aggregate(fr, fact = as.integer(aggregate_factor),
                                       fun = get(aggregate_fun), na.rm = TRUE)
          }
          raster::writeRaster(fr, f_ras, overwrite = TRUE)
     }
     if (verbose) {
          message(glue::glue("Friction grid: {nrow(fr)} x {ncol(fr)} cells ",
                             "(~{round(raster::res(fr)[1] * 111, 1)} km)"))
     }

     # ---- least-cost travel time -------------------------------------------
     if (verbose) message("Building transition graph (this is the slow step) ...")
     # friction is minutes per metre -> conductance is its reciprocal
     tr <- gdistance::transition(fr, function(x) 1 / mean(x), directions = 8)
     # scale conductance by true inter-cell distance in metres
     tr <- gdistance::geoCorrection(tr, type = "c")

     pts <- as.matrix(cent[, c("lon", "lat")])
     if (verbose) message("Computing pairwise least-cost distances ...")
     cost_min <- gdistance::costDistance(tr, pts)
     D <- as.matrix(cost_min) / 60           # minutes -> hours
     dimnames(D) <- list(cent$iso3, cent$iso3)
     diag(D) <- 0

     # Cap HERE, before the CSV is written, so every consumer sees the same
     # finite matrix. Capping in est_mobility() made the cap depend on which
     # ISO subset was requested, and left Inf in the cached file for anyone
     # else. All-non-finite would also have produced cap = -Inf.
     n_inf <- sum(!is.finite(D))
     if (n_inf) {
          fin <- D[is.finite(D) & D > 0]
          if (!length(fin)) {
               stop("Every country pair is unreachable -- the friction raster does not ",
                    "cover the centroids (check the bounding box).", call. = FALSE)
          }
          cap <- max(fin) * 1.5
          warning(n_inf, " unreachable pair(s) capped at ", round(cap, 1),
                  " h (1.5x the largest finite travel time).", call. = FALSE)
          D[!is.finite(D)] <- cap
     }
     if (verbose) {
          fin <- D[is.finite(D) & D > 0]
          message(glue::glue("Travel time: median {round(stats::median(fin),1)} h, ",
                             "range {round(min(fin),1)}-{round(max(fin),1)} h",
                             if (n_inf) glue::glue(", {n_inf} unreachable") else ""))
     }

     utils::write.csv(as.data.frame(D), f_out)
     if (verbose) message("  -> ", f_out)
     invisible(D)
}
