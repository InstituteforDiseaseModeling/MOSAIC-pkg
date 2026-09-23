#' Fuse four bilateral mobility sources into a connectivity structure
#'
#' Builds a row-normalised origin-destination \emph{structure} matrix by
#' ensemble-averaging four sources: UN DESA bilateral migrant stock, Abel &
#' Cohen bilateral flows, the Meta Social Connectedness Index, and land
#' contiguity. The result is a unit-free topology -- who connects to whom and
#' how strongly, relative to each origin's other destinations -- consumed by
#' \code{est_mobility(od_source = "fused")}.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}.
#' @param iso_codes ISO3 codes to include. Defaults to
#'   \code{MOSAIC::iso_codes_mosaic}.
#' @param weights Named numeric ensemble weights. Default
#'   \code{c(desa = 0.30, abel_cohen = 0.20, sci = 0.35, contiguity = 0.15)},
#'   the E3 design values. Rescaled to sum to 1 over the sources actually
#'   available.
#' @param snapshot_dir Directory of raw sources. \code{NULL} (default) selects
#'   the newest \code{snapshot_<date>/} written by
#'   \code{\link{download_mobility_od_sources}}.
#' @param flow_year_min Earliest Abel-Cohen \code{year0} period to include.
#'   Default 2015 (the 2015-2020 period used by E3).
#' @param sci_denormalise Multiply the Meta SCI matrix by destination
#'   population before row-normalising. \code{TRUE} by default and strongly
#'   recommended: \code{scaled_sci} divides destination size out by
#'   construction, so leaving it normalised contributes a destination-size
#'   exponent of ~0 to the gravity fit and -- at weight 0.35 -- drags the
#'   fused \code{omega} well below the value the volume sources agree on.
#' @param flow_estimator Which Abel & Cohen estimator column to use. Default
#'   \code{"da_pb_closed"}, the \strong{pseudo-Bayesian} estimate the authors
#'   recommend (\emph{Sci Data} 2022: it \dQuote{performs consistently better
#'   than the other estimation methods}). The E3 prototype used
#'   \code{"da_min_closed"}, a \emph{minimum} flow consistent with the stock
#'   change rather than a per-pair estimate; on a 10-country West-African set
#'   every origin was populated, but across the MOSAIC-40 it leaves
#'   \strong{AGO, GNQ and SOM with no intra-set mass at all} and reduces
#'   Namibia to a single 0.22-person cell, which row-normalisation then
#'   promotes to weight 1.0.
#' @param verbose Print per-source coverage.
#'
#' @return Invisibly, a list with \code{M} (the fused row-normalised matrix),
#'   \code{components} (the per-source row-normalised matrices), and
#'   \code{weights} (as actually applied). Writes
#'   \code{processed/mobility/M_structure_fused.csv} plus one CSV per
#'   component.
#'
#' @section Why these weights:
#' DESA (stock) and Abel-Cohen (flow) are \strong{not independent} -- the flow
#' estimates are derived from successive stock tables -- so they share a single
#' 0.5 migration block rather than getting 0.35 each. SCI carries 0.35 as a
#' destination-affinity kernel that is independent of the migration data.
#' Contiguity gets 0.15 as a weak structural prior that a shared land border
#' implies movement even where the other sources are sparse.
#'
#' @section Direction convention:
#' All matrices are \strong{origin (row) -> destination (column)} and each row
#' sums to 1. DESA's Table 1 is published destination-major (its column 1 is
#' the destination, column 6 the origin), so it is transposed on read; verified
#' against the known Burkina Faso -> Cote d'Ivoire corridor (1,820,882 persons
#' in the 2024 column). Getting this backwards silently transposes the entire
#' connectivity structure.
#'
#' @section What this is NOT:
#' A row-normalised structure carries \strong{no amplitude}. It says nothing
#' about how many people move, only where they go given that they move. DESA
#' is a decades-accumulated \emph{stock}; dividing it by population does not
#' yield a weekly departure rate (the E3 work measured a ~700x inflation from
#' exactly that mistake). Departure amplitude comes from
#' \code{fit_prob_travel()} on real flow data, never from this matrix.
#'
#' @seealso \code{\link{download_mobility_od_sources}}, \code{\link{est_mobility}}
#'
#' @importFrom utils read.csv write.csv
#' @importFrom glue glue
#' @export
process_mobility_od_data <- function(PATHS,
                                     iso_codes     = NULL,
                                     weights       = c(desa = 0.30, abel_cohen = 0.20,
                                                       sci = 0.35, contiguity = 0.15),
                                     snapshot_dir  = NULL,
                                     flow_year_min = 2015L,
                                     sci_denormalise = TRUE,
                                     flow_estimator = c("da_pb_closed", "da_min_closed",
                                                        "da_min_open", "da_pb_open"),
                                     verbose       = TRUE) {

     flow_estimator <- match.arg(flow_estimator)
     if (is.null(iso_codes)) iso_codes <- iso_codes_mosaic
     iso_codes <- sort(unique(toupper(iso_codes)))
     n <- length(iso_codes)

     if (is.null(snapshot_dir)) snapshot_dir <- .mobility_od_newest_snapshot(PATHS)
     if (is.na(snapshot_dir) || !dir.exists(snapshot_dir)) {
          stop("No mobility-OD snapshot found.\n",
               "  Fetch one with download_mobility_od_sources(PATHS).", call. = FALSE)
     }
     if (verbose) message("Fusing mobility OD sources from: ", basename(snapshot_dir))

     comp <- list()
     comp$desa       <- .od_desa(snapshot_dir, iso_codes, verbose)
     comp$abel_cohen <- .od_abel_cohen(snapshot_dir, iso_codes, flow_year_min, verbose,
                                       estimator = flow_estimator)
     comp$sci        <- .od_sci(snapshot_dir, iso_codes, verbose,
                                denormalise = sci_denormalise, PATHS = PATHS)
     comp$contiguity <- .od_contiguity(PATHS, iso_codes, verbose)

     have <- names(comp)[!vapply(comp, is.null, logical(1))]
     if (!length(have)) stop("No mobility OD source could be built.", call. = FALSE)
     if (length(have) < length(comp) && verbose) {
          message("  NOTE: missing source(s): ",
                  paste(setdiff(names(comp), have), collapse = ", "),
                  " -- weights rescaled over the rest.")
     }

     w <- weights[have]
     w <- w / sum(w)

     M <- matrix(0, n, n, dimnames = list(iso_codes, iso_codes))
     for (s in have) M <- M + w[[s]] * comp[[s]]
     M <- .od_row_normalise(M)

     out_dir <- file.path(PATHS$DATA_PROCESSED, "mobility")
     dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
     utils::write.csv(as.data.frame(M), file.path(out_dir, "M_structure_fused.csv"))
     for (s in have) {
          utils::write.csv(as.data.frame(comp[[s]]),
                           file.path(out_dir, sprintf("M_structure_%s.csv", s)))
     }

     if (verbose) {
          message(glue::glue("\nFused {length(have)} sources over {n} countries",
                             " (weights: ",
                             paste(sprintf("%s=%.2f", names(w), w), collapse = ", "), ")"))
          message("  -> ", file.path(out_dir, "M_structure_fused.csv"))
     }
     invisible(list(M = M, components = comp[have], weights = w))
}


#' Row-normalise, zero diagonal, safe on all-zero rows
#' @keywords internal
#' @noRd
.od_row_normalise <- function(M) {
     diag(M) <- 0
     rs <- rowSums(M, na.rm = TRUE)
     ok <- rs > 0
     M[ok, ] <- M[ok, , drop = FALSE] / rs[ok]
     M[!ok, ] <- 0
     M[is.na(M)] <- 0
     M
}

#' Empty ISO x ISO matrix
#' @keywords internal
#' @noRd
.od_empty <- function(iso) {
     matrix(0, length(iso), length(iso), dimnames = list(iso, iso))
}

#' UN DESA bilateral migrant stock -> row-normalised origin x destination
#' @keywords internal
#' @noRd
.od_desa <- function(snap, iso, verbose) {
     f <- list.files(snap, pattern = "undesa.*\\.xlsx$", full.names = TRUE)
     if (!length(f)) { if (verbose) message("  desa: absent"); return(NULL) }

     d <- suppressWarnings(readxl::read_excel(f[1], sheet = "Table 1", skip = 10))
     nm <- names(d)
     col_dest <- grep("^Location code of destination$", nm)
     col_orig <- grep("^Location code of origin$", nm)
     # readxl repairs the three duplicate year blocks (both sexes / male /
     # female) to "2024...15", "2024...23", "2024...31". The FIRST is the
     # both-sexes total -- column 15, matching the E3 provenance note.
     col_2024 <- grep("^2024", nm)[1]
     if (!length(col_dest) || !length(col_orig) || is.na(col_2024)) {
          warning("DESA Table 1 schema changed; skipping this source.", call. = FALSE)
          return(NULL)
     }

     m49  <- suppressWarnings(as.integer(unlist(d[[col_orig]])))
     m49d <- suppressWarnings(as.integer(unlist(d[[col_dest]])))
     val  <- suppressWarnings(as.numeric(unlist(d[[col_2024]])))

     o <- countrycode::countrycode(m49,  "un", "iso3c", warn = FALSE)
     e <- countrycode::countrycode(m49d, "un", "iso3c", warn = FALSE)

     keep <- !is.na(o) & !is.na(e) & o %in% iso & e %in% iso & o != e & !is.na(val) & val > 0
     M <- .od_empty(iso)
     if (any(keep)) {
          # rows are ORIGIN, columns DESTINATION
          M[cbind(match(o[keep], iso), match(e[keep], iso))] <- val[keep]
     }
     if (verbose) {
          message(glue::glue("  desa      : {sum(keep)} directed pairs, ",
                             "{format(round(sum(val[keep])/1e6, 2), nsmall = 2)}M persons"))
     }
     .od_row_normalise(M)
}

#' Abel & Cohen bilateral flows -> row-normalised origin x destination
#' @keywords internal
#' @noRd
.od_abel_cohen <- function(snap, iso, year_min, verbose,
                           estimator = "da_pb_closed") {
     f <- list.files(snap, pattern = "abel_cohen.*\\.csv$", full.names = TRUE)
     if (!length(f)) { if (verbose) message("  abel_cohen: absent"); return(NULL) }

     d <- utils::read.csv(f[1], stringsAsFactors = FALSE)
     need <- c("year0", "orig", "dest", estimator)
     if (length(setdiff(need, names(d)))) {
          warning("Abel-Cohen schema changed; skipping this source.", call. = FALSE)
          return(NULL)
     }
     # sexes are separate rows; sum them
     d <- d[d$year0 >= year_min & d$orig %in% iso & d$dest %in% iso & d$orig != d$dest, ,
            drop = FALSE]
     d$v <- suppressWarnings(as.numeric(d[[estimator]]))
     d <- d[!is.na(d$v) & d$v > 0, , drop = FALSE]

     M <- .od_empty(iso)
     if (nrow(d)) {
          agg <- tapply(d$v, list(d$orig, d$dest), sum, na.rm = TRUE)
          ri <- match(rownames(agg), iso); ci <- match(colnames(agg), iso)
          for (i in seq_along(ri)) for (j in seq_along(ci)) {
               v <- agg[i, j]
               if (!is.na(v)) M[ri[i], ci[j]] <- M[ri[i], ci[j]] + v
          }
     }
     if (verbose) {
          message(glue::glue("  abel_cohen: {sum(M > 0)} directed pairs, ",
                             "periods >= {year_min}"))
     }
     .od_row_normalise(M)
}

#' Meta SCI (country level) -> row-normalised origin x destination
#' @keywords internal
#' @noRd
.od_sci <- function(snap, iso, verbose, denormalise = TRUE, PATHS = NULL) {
     f <- list.files(snap, pattern = "sci.*\\.csv$", full.names = TRUE)
     if (!length(f)) { if (verbose) message("  sci: absent"); return(NULL) }

     # na.strings = "": Namibia's ISO2 code is the literal string "NA". With
     # read.csv's default na.strings = "NA" all 178 Namibian rows become
     # missing BEFORE countrycode() sees them -- countrycode("NA") correctly
     # returns "NAM", so this is a reader bug, not a mapping one. Symptom was
     # NAM as the only all-zero row AND column of the SCI component, which
     # then re-weighted NAM's fusion onto a 0.22-person Abel-Cohen residual
     # and produced a spurious 322 persons/day NAM->SEN corridor.
     d <- utils::read.csv(f[1], stringsAsFactors = FALSE, na.strings = "")
     need <- c("user_country", "friend_country", "scaled_sci")
     if (length(setdiff(need, names(d)))) {
          warning("Meta SCI schema changed; skipping this source.", call. = FALSE)
          return(NULL)
     }
     # SCI ships ISO2
     o <- countrycode::countrycode(d$user_country,   "iso2c", "iso3c", warn = FALSE)
     e <- countrycode::countrycode(d$friend_country, "iso2c", "iso3c", warn = FALSE)
     v <- suppressWarnings(as.numeric(d$scaled_sci))

     keep <- !is.na(o) & !is.na(e) & o %in% iso & e %in% iso & o != e & !is.na(v) & v > 0
     M <- .od_empty(iso)
     if (any(keep)) M[cbind(match(o[keep], iso), match(e[keep], iso))] <- v[keep]

     # scaled_sci = connections / (users_i * users_j): destination size is
     # divided out BY CONSTRUCTION, so a gravity fit on it returns a
     # destination-size exponent of ~0 (measured slope -0.13 vs +0.61/+0.64
     # for the volume sources). Multiplying by destination population restores
     # a volume-like quantity (measured slope +0.76). Origin size cancels in
     # the row normalisation, so only N_j is needed. Population is a proxy for
     # Meta's user base -- imperfect where penetration differs, but far closer
     # than treating a normalised index as a volume.
     if (isTRUE(denormalise) && !is.null(PATHS)) {
          Nj <- .od_destination_population(PATHS, iso)
          if (!is.null(Nj)) {
               M <- sweep(M, 2, Nj, "*")
               if (verbose) message("  sci       : de-normalised by destination population")
          } else if (verbose) {
               message("  sci       : NOT de-normalised (population unavailable) -- ",
                       "this source will contribute no destination-size signal")
          }
     }
     if (verbose) message(glue::glue("  sci       : {sum(keep)} directed pairs"))
     .od_row_normalise(M)
}

#' Land contiguity from ADM0 shapefiles -> row-normalised
#' @keywords internal
#' @noRd
.od_contiguity <- function(PATHS, iso, verbose) {
     dir_shp <- PATHS$DATA_SHAPEFILES
     if (is.null(dir_shp) || !dir.exists(dir_shp)) {
          if (verbose) message("  contiguity: DATA_SHAPEFILES not found")
          return(NULL)
     }
     files <- file.path(dir_shp, paste0(iso, "_ADM0.shp"))
     have  <- file.exists(files)
     if (sum(have) < 2L) {
          if (verbose) message("  contiguity: <2 country shapefiles present")
          return(NULL)
     }

     geoms <- lapply(files[have], function(f)
          suppressMessages(sf::st_geometry(sf::st_read(f, quiet = TRUE))))
     g <- do.call(c, geoms)
     present <- iso[have]

     M <- .od_empty(iso)
     ok <- tryCatch({
          # st_touches misses neighbours across small topology gaps between
          # separately-sourced country polygons; a geodesic distance threshold
          # is robust to them (the E3 build hit exactly this).
          dm <- sf::st_distance(g)
          dm <- matrix(as.numeric(dm), nrow = length(present))
          nb <- which(dm < 10000 & dm >= 0, arr.ind = TRUE)
          nb <- nb[nb[, 1] != nb[, 2], , drop = FALSE]
          if (nrow(nb)) {
               M[cbind(match(present[nb[, 1]], iso),
                       match(present[nb[, 2]], iso))] <- 1
          }
          TRUE
     }, error = function(e) FALSE)
     if (!ok) {
          if (verbose) message("  contiguity: distance computation failed")
          return(NULL)
     }
     if (verbose) {
          message(glue::glue("  contiguity: {sum(M > 0)} shared-border pairs ",
                             "({sum(have)}/{length(iso)} shapefiles)"))
     }
     .od_row_normalise(M)
}


#' Destination population vector for SCI de-normalisation
#'
#' @keywords internal
#' @noRd
.od_destination_population <- function(PATHS, iso) {
     f <- file.path(PATHS$DATA_DEMOGRAPHICS, "demographics_africa_2000_2023.csv")
     if (!file.exists(f)) return(NULL)
     d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
     if (is.null(d) || !all(c("iso_code", "year", "population") %in% names(d))) return(NULL)
     d <- d[d$year == 2017L & d$iso_code %in% iso, , drop = FALSE]
     N <- stats::setNames(as.numeric(d$population), d$iso_code)[iso]
     if (anyNA(N)) return(NULL)
     N
}
