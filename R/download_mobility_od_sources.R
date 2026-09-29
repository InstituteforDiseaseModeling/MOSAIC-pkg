#' Download the bilateral mobility sources used to build the fused OD structure
#'
#' Fetches the three external origin-destination sources that
#' \code{\link{process_mobility_od_data}} fuses into a connectivity structure
#' for \code{\link{est_mobility}}: UN DESA bilateral migrant stock, Abel &
#' Cohen bilateral migration flows, and the Meta (Facebook) Social
#' Connectedness Index. The fourth source, land contiguity, is derived locally
#' from the ADM0 shapefiles and needs no download.
#'
#' All three are open; none needs credentials.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}. Must include
#'   \code{DATA_RAW}.
#' @param sources Which to fetch. Default all three.
#' @param snapshot_date Date stamp for the archive subdirectory. Defaults to
#'   \code{Sys.Date()}.
#' @param overwrite If \code{FALSE} (default), an existing snapshot is kept.
#' @param verbose Print progress.
#'
#' @return Invisibly, a \code{data.frame}: \code{source}, \code{ok},
#'   \code{bytes}, \code{file}, \code{note}.
#'
#' @section Sources and provenance:
#' \describe{
#'   \item{\strong{UN DESA International Migrant Stock 2024}}{Bilateral migrant
#'     \emph{stock} (persons born in the origin residing in the destination),
#'     Table 1, 2024 both-sexes column. ~6 MB xlsx. The un.org host rejects a
#'     default \code{libcurl} agent with HTTP 403, so a browser User-Agent is
#'     sent. Licence: UN open data.}
#'   \item{\strong{Abel & Cohen 2022}}{Bilateral migration \emph{flow}
#'     estimates (\emph{Sci Data}), figshare article 12845711,
#'     \code{bilat_mig_sex.csv} (~23 MB), ISO3 already. The
#'     \code{da_min_closed} estimator over the latest period is used
#'     downstream. Licence: CC BY.}
#'   \item{\strong{Meta Social Connectedness Index}}{HDX
#'     \code{social-connectedness-index}, resource \strong{\code{country.csv}}
#'     (~0.6 MB), columns \code{user_country}/\code{friend_country} (ISO2) and
#'     \code{scaled_sci}. Licence: CC BY-NC (Meta Data for Good) -- see the
#'     note below.}
#' }
#'
#' \strong{Deviation from the E3 prototype.} The original MOSAIC-OCV E3 work
#' used the GADM-1 resource (273 MB) and summed sub-region pairs up to national
#' totals. \code{country.csv} is Meta's own national-level product: ~500x
#' smaller, no aggregation step, and a directly-measured national index rather
#' than a sum of relative sub-national indices. Because the two are not
#' guaranteed to be proportional, a fused structure built here is close to but
#' not byte-identical with the E3 artifacts.
#'
#' \strong{Licensing.} Meta SCI is \strong{CC BY-NC}. That is more restrictive
#' than the other MOSAIC inputs and than the CC BY / UN terms of DESA and
#' Abel-Cohen. Set \code{sources} to exclude \code{"sci"} (and reweight in
#' \code{\link{process_mobility_od_data}}) if a downstream use is commercial.
#'
#' @seealso \code{\link{process_mobility_od_data}}, \code{\link{est_mobility}}
#'
#' @importFrom utils download.file
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' download_mobility_od_sources(PATHS)
#' process_mobility_od_data(PATHS)
#' }
download_mobility_od_sources <- function(PATHS,
                                         sources       = c("desa", "abel_cohen", "sci"),
                                         snapshot_date = Sys.Date(),
                                         overwrite     = FALSE,
                                         verbose       = TRUE) {

     if (is.null(PATHS$DATA_RAW)) {
          stop("PATHS must include DATA_RAW (regenerate via get_paths()).", call. = FALSE)
     }
     sources <- match.arg(sources, several.ok = TRUE)

     snap <- file.path(PATHS$DATA_RAW, "mobility_od", paste0("snapshot_", format(snapshot_date)))
     dir.create(snap, recursive = TRUE, showWarnings = FALSE)

     spec <- list(
          desa = list(
               file = "undesa_ims_stock_2024_destination_origin.xlsx",
               url  = paste0("https://www.un.org/development/desa/pd/sites/",
                             "www.un.org.development.desa.pd/files/",
                             "undesa_pd_2024_ims_stock_by_sex_destination_and_origin.xlsx"),
               # un.org 403s a default libcurl agent
               ua   = TRUE, min_bytes = 1e6),
          abel_cohen = list(
               file = "abel_cohen_2022_bilat_mig_sex.csv",
               url  = "https://ndownloader.figshare.com/files/53235860",
               ua   = FALSE, min_bytes = 1e6),
          sci = list(
               file = "meta_sci_country.csv",
               url  = paste0("https://data.humdata.org/dataset/",
                             "e9988552-74e4-4ff4-943f-c782ac8bca87/resource/",
                             "652cf9c9-541f-47de-8d53-ff818062bd0c/download/country.csv"),
               ua   = TRUE, min_bytes = 1e5)
     )

     out <- lapply(sources, function(s) {
          sp   <- spec[[s]]
          dest <- file.path(snap, sp$file)
          blank <- data.frame(source = s, ok = FALSE, bytes = NA_real_,
                              file = dest, note = NA_character_,
                              stringsAsFactors = FALSE)

          if (file.exists(dest) && !overwrite) {
               if (verbose) message(glue::glue("  {s}: already present"))
               blank$ok <- TRUE; blank$bytes <- file.info(dest)$size
               blank$note <- "existing"
               return(blank)
          }

          if (verbose) message(glue::glue("  {s}: downloading ..."))
          res <- .mosaic_download(
               url       = sp$url,
               dest      = dest,
               min_bytes = sp$min_bytes,
               headers   = if (isTRUE(sp$ua)) c("User-Agent" = .mosaic_browser_ua()) else NULL,
               overwrite = overwrite,
               verbose   = verbose)
          if (!res$ok) {
               blank$note <- res$note
               if (verbose) message(glue::glue("     FAILED: {res$note}"))
               return(blank)
          }
          sz <- res$bytes
          if (verbose) message(glue::glue("     {round(sz/1e6, 1)} MB -> {basename(dest)}"))
          data.frame(source = s, ok = TRUE, bytes = sz, file = dest,
                     note = NA_character_, stringsAsFactors = FALSE)
     })

     out <- do.call(rbind, out)
     if (verbose) {
          message(glue::glue("\nmobility OD sources: {sum(out$ok)}/{nrow(out)} -> {snap}"))
          if (any(!out$ok)) message("  failed: ", paste(out$source[!out$ok], collapse = ", "))
     }
     invisible(out)
}


#' @keywords internal
#' @noRd
.mosaic_browser_ua <- function() {
     paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
            "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0 Safari/537.36")
}


#' Newest mobility-OD snapshot directory
#'
#' @keywords internal
#' @noRd
.mobility_od_newest_snapshot <- function(PATHS) {
     base <- file.path(PATHS$DATA_RAW, "mobility_od")
     if (!dir.exists(base)) return(NA_character_)
     snaps <- list.dirs(base, recursive = FALSE, full.names = TRUE)
     snaps <- snaps[grepl("^snapshot_\\d{4}-\\d{2}-\\d{2}$", basename(snaps))]
     snaps <- snaps[vapply(snaps, function(d) length(list.files(d)) > 0L, logical(1))]
     if (!length(snaps)) return(NA_character_)
     snaps[order(basename(snaps), decreasing = TRUE)][1L]
}
