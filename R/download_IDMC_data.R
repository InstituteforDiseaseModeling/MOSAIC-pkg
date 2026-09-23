#' Download IDMC Internal Displacement Updates (IDU) from the HDX mirrors
#'
#' Downloads the per-country IDMC \strong{Internal Displacement Updates} (IDU)
#' event CSVs published on the Humanitarian Data Exchange (HDX) and archives a
#' date-stamped snapshot into \code{PATHS$DATA_IDMC_RAW}, ready for
#' \code{\link{process_IDMC_data}}.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}. Must include
#'   \code{DATA_IDMC_RAW}.
#' @param iso_codes Character vector of ISO3 codes to fetch. Defaults to
#'   \code{MOSAIC::iso_codes_mosaic} (the MOSAIC-40).
#' @param snapshot_date Date stamp for the archive subdirectory. Defaults to
#'   \code{Sys.Date()}.
#' @param overwrite If \code{FALSE} (default) and today's snapshot directory
#'   already exists with files in it, the download is skipped and the existing
#'   snapshot is reported. Set \code{TRUE} to re-download.
#' @param verbose If \code{TRUE} (default), print a per-country progress line
#'   and a coverage summary.
#'
#' @return Invisibly, a data frame with one row per requested ISO code and
#'   columns \code{iso_code}, \code{ok}, \code{n_events}, \code{date_min},
#'   \code{date_max}, \code{file}, \code{note}.
#'
#' @details
#' \strong{Why HDX and not the \code{idmc} R package.} The CRAN \code{idmc}
#' package wraps the IDMC \code{external-api} endpoint, which is credential
#' gated: an unauthenticated request to
#' \code{helix-tools-api.idmcdb.org/external-api/idus/all/} returns
#' \code{HTTP 403 "Client is not registered."}, and \code{idmc_get_data()}
#' errors out unless an IDMC-issued URL is present in the \code{IDMC_API}
#' environment variable. The HDX per-country mirrors carry the same IDU event
#' records, are CC BY-IGO, need no credentials, and are refreshed daily. They
#' also omit \code{standard_popup_text}, which \code{idmc_get_data()}
#' unconditionally parses -- so HDX data cannot be routed through that package
#' anyway.
#'
#' \strong{Resource discovery.} Download URLs embed CKAN resource UUIDs that
#' change when IDMC re-publishes, so they are resolved at call time from the
#' HDX CKAN API (\code{package_show?id=<iso>-idmc-idu-events}) rather than
#' hardcoded.
#'
#' \strong{Snapshot archiving (important).} IDU is a \emph{rolling,
#' provisional} product: records age out and figures are revised. Snapshots are
#' therefore written to a dated subdirectory and never overwritten in place,
#' mirroring the EM-DAT and WHO-dashboard conventions elsewhere in MOSAIC.
#' \code{process_IDMC_data()} reads whichever directory it is pointed at, so
#' pass \code{source_dir} to reprocess a historical snapshot.
#'
#' \strong{Coverage.} As of 2026-09-17, 38 of the 40 MOSAIC countries have an
#' HDX IDU dataset; \strong{ERI and TGO have none}. Most series begin
#' 2025-01-01 (KEN is a notable exception, reaching back to 2011). Zero cells
#' before a country's first observed event mean "not in this extract", NOT "no
#' displacement occurred" -- see the coverage caveat in
#' \code{?process_IDMC_data} before using these as a covariate.
#'
#' @source IDMC Internal Displacement Updates via HDX,
#'   \url{https://data.humdata.org/dataset/}\code{<iso>-idmc-idu-events}.
#'   Licence: CC BY-IGO. Cite IDMC.
#'
#' @seealso \code{\link{process_IDMC_data}} to build the country-week panels,
#'   \code{\link{process_EMDAT_data}} for the sibling hazard panels.
#'
#' @importFrom utils read.csv
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' download_IDMC_data(PATHS)
#' process_IDMC_data(PATHS, source_dir = file.path(PATHS$DATA_IDMC_RAW,
#'                                                 paste0("hdx_", Sys.Date())))
#' }
download_IDMC_data <- function(PATHS,
                               iso_codes     = NULL,
                               snapshot_date = Sys.Date(),
                               overwrite     = FALSE,
                               verbose       = TRUE) {

     if (is.null(PATHS$DATA_IDMC_RAW)) {
          stop("PATHS must include DATA_IDMC_RAW (regenerate via get_paths()).",
               call. = FALSE)
     }
     if (is.null(iso_codes)) {
          iso_codes <- get("iso_codes_mosaic", envir = asNamespace("MOSAIC"))
     }
     iso_codes <- toupper(unique(iso_codes))

     snap_dir <- file.path(PATHS$DATA_IDMC_RAW, paste0("hdx_", format(snapshot_date)))
     if (dir.exists(snap_dir) && length(list.files(snap_dir, pattern = "\\.csv$")) && !overwrite) {
          if (verbose) {
               message(glue::glue(
                    "Snapshot already exists at {snap_dir} ",
                    "({length(list.files(snap_dir, pattern = '\\\\.csv$'))} files). ",
                    "Pass overwrite = TRUE to re-download."))
          }
          return(invisible(.idmc_snapshot_summary(snap_dir, iso_codes)))
     }
     dir.create(snap_dir, recursive = TRUE, showWarnings = FALSE)

     if (verbose) message(glue::glue("Downloading IDMC IDU events for {length(iso_codes)} countries -> {snap_dir}"))

     out <- lapply(iso_codes, function(iso) {

          blank <- data.frame(iso_code = iso, ok = FALSE, n_events = NA_integer_,
                              date_min = NA_character_, date_max = NA_character_,
                              file = NA_character_, note = NA_character_,
                              stringsAsFactors = FALSE)

          url <- tryCatch(.idmc_hdx_resource_url(iso), error = function(e) NULL)
          if (is.null(url)) {
               blank$note <- "no HDX dataset"
               if (verbose) message(glue::glue("  {iso}: no HDX IDU dataset"))
               return(blank)
          }

          dest <- file.path(snap_dir, sprintf("%s_idmc_idu_events.csv", tolower(iso)))
          dl <- .mosaic_download(
               url, dest, min_bytes = 1L, overwrite = TRUE, verbose = FALSE,
               # reject an HTML error page served with HTTP 200
               validate = function(f) {
                    h <- readLines(f, n = 1L, warn = FALSE)
                    length(h) > 0L && grepl("iso3", h, fixed = TRUE)
               })
          if (!dl$ok) {
               blank$note <- dl$note
               if (verbose) message(glue::glue("  {iso}: download FAILED ({dl$note})"))
               return(blank)
          }

          d <- tryCatch(utils::read.csv(dest, stringsAsFactors = FALSE),
                        error = function(e) NULL)
          if (is.null(d) || !nrow(d)) {
               blank$ok <- TRUE; blank$n_events <- 0L; blank$file <- dest
               blank$note <- "empty"
               if (verbose) message(glue::glue("  {iso}: 0 events"))
               return(blank)
          }

          dts <- suppressWarnings(as.Date(d$displacement_start_date))
          dts <- dts[!is.na(dts)]
          res <- data.frame(
               iso_code = iso, ok = TRUE, n_events = nrow(d),
               date_min = if (length(dts)) format(min(dts)) else NA_character_,
               date_max = if (length(dts)) format(max(dts)) else NA_character_,
               file = dest, note = NA_character_, stringsAsFactors = FALSE)
          if (verbose) {
               message(glue::glue("  {iso}: {nrow(d)} events  {res$date_min} -> {res$date_max}"))
          }
          res
     })

     out <- do.call(rbind, out)

     if (verbose) {
          got  <- sum(out$ok, na.rm = TRUE)
          miss <- out$iso_code[!out$ok]
          message(glue::glue(
               "\nDownloaded {got}/{nrow(out)} countries, ",
               "{sum(out$n_events, na.rm = TRUE)} events total -> {snap_dir}"))
          if (length(miss)) message("  no data: ", paste(miss, collapse = ", "))
          message("Next: process_IDMC_data(PATHS, source_dir = \"", snap_dir, "\")")
     }

     invisible(out)
}


#' Resolve the current HDX CSV download URL for one country's IDU dataset
#'
#' @keywords internal
#' @noRd
.idmc_hdx_resource_url <- function(iso) {
     api <- sprintf(
          "https://data.humdata.org/api/3/action/package_show?id=%s-idmc-idu-events",
          tolower(iso))
     txt <- suppressWarnings(tryCatch(
          paste(readLines(api, warn = FALSE), collapse = ""),
          error = function(e) NULL))
     if (is.null(txt)) stop("HDX lookup failed for ", iso, call. = FALSE)

     js <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
     if (!isTRUE(js$success)) stop("HDX returned success=false for ", iso, call. = FALSE)

     res <- js$result$resources
     if (is.null(res) || !nrow(res)) stop("no resources for ", iso, call. = FALSE)

     csv <- res[tolower(res$format) == "csv", , drop = FALSE]
     if (!nrow(csv)) stop("no CSV resource for ", iso, call. = FALSE)
     csv$url[1L]
}


#' Summarise an already-downloaded snapshot directory
#'
#' @keywords internal
#' @noRd
.idmc_snapshot_summary <- function(snap_dir, iso_codes) {
     do.call(rbind, lapply(iso_codes, function(iso) {
          f <- file.path(snap_dir, sprintf("%s_idmc_idu_events.csv", tolower(iso)))
          if (!file.exists(f)) {
               return(data.frame(iso_code = iso, ok = FALSE, n_events = NA_integer_,
                                 date_min = NA_character_, date_max = NA_character_,
                                 file = NA_character_, note = "absent from snapshot",
                                 stringsAsFactors = FALSE))
          }
          d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                        error = function(e) NULL)
          # ok must mean "readable IDU data", not "a file is present". A
          # 0-byte file or an HTML error page previously returned
          # ok = TRUE / n_events = 0, indistinguishable from a country that
          # genuinely reported no events.
          usable <- !is.null(d) && "iso3" %in% names(d)
          dts <- if (usable && nrow(d)) {
               x <- suppressWarnings(as.Date(d$displacement_start_date)); x[!is.na(x)]
          } else as.Date(character(0))
          data.frame(iso_code = iso, ok = usable,
                     n_events = if (!usable) NA_integer_ else nrow(d),
                     date_min = if (length(dts)) format(min(dts)) else NA_character_,
                     date_max = if (length(dts)) format(max(dts)) else NA_character_,
                     file = f, note = NA_character_, stringsAsFactors = FALSE)
     }))
}
