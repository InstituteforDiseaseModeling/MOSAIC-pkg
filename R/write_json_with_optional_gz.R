#' Write a list to a .json file and optionally produce a byte-equal .json.gz sidecar
#'
#' @description
#' Writes `data_list` to `file_path` using [write_list_to_json()] (plain
#' .json, no compression). If `gz_sidecar = TRUE`, additionally produces
#' `<file_path>.gz` by running [R.utils::gzip()] on the just-written file.
#'
#' Because the .gz is derived from the exact bytes on disk, it is
#' guaranteed to decompress to a byte-identical copy of the .json -- there
#' is no parallel `jsonlite::toJSON()` call and therefore no opportunity
#' for the two outputs to drift.
#'
#' @param data_list  Named list to serialise.
#' @param file_path  Output path. Must end in `.json` (the .gz sidecar
#'                   is derived from this path).
#' @param gz_sidecar Logical. If `TRUE`, also write `<file_path>.gz` as a
#'                   gzip of the .json. Default `FALSE`.
#'
#' @return The path to the .json file (invisibly).
#'
#' @details
#' Intended for the data-raw config builders that ship LASER configuration
#' artifacts under `inst/extdata/`. Production callers can flip the
#' sidecar on via the `MOSAIC_WRITE_GZ_SIDECARS` environment variable
#' (see [.mosaic_write_gz_sidecars()]) without editing source.
#'
#' @seealso [write_list_to_json()], [R.utils::gzip()],
#'   [.mosaic_write_gz_sidecars()]
#'
#' @export
write_json_with_optional_gz <- function(data_list,
                                        file_path,
                                        gz_sidecar = FALSE) {

     if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
          stop("You must provide a valid output file path.")
     }
     if (!grepl("\\.json$", file_path, ignore.case = TRUE)) {
          stop("file_path must end in .json; the .gz sidecar is derived from it.")
     }

     MOSAIC::write_list_to_json(data_list, file_path, compress = FALSE)

     if (isTRUE(gz_sidecar)) {
          gz_path <- paste0(file_path, ".gz")
          R.utils::gzip(filename  = file_path,
                        destname  = gz_path,
                        remove    = FALSE,
                        overwrite = TRUE)
          if (!file.exists(gz_path)) {
               stop("gzip sidecar was not created at: ",
                    normalizePath(gz_path, winslash = "/", mustWork = FALSE))
          }
          message("gzip sidecar written to: ",
                  normalizePath(gz_path, winslash = "/"))
     }

     invisible(file_path)
}


#' Should data-raw builders produce .json.gz sidecars?
#'
#' Reads the `MOSAIC_WRITE_GZ_SIDECARS` environment variable and returns
#' `TRUE` iff it is set to a value parseable as a true logical (e.g.
#' `"TRUE"`, `"true"`, `"1"`). Defaults to `FALSE` -- the sidecar is
#' opt-in. Used by the production-calibration config builders
#' (`data-raw/make_config_default.R`,
#' `data-raw/make_config_default_MOZ.R`).
#'
#' @return Length-1 logical.
#' @keywords internal
#' @noRd
.mosaic_write_gz_sidecars <- function() {
     val <- toupper(trimws(Sys.getenv("MOSAIC_WRITE_GZ_SIDECARS", unset = "FALSE")))
     val %in% c("TRUE", "T", "1", "YES", "Y", "ON")
}
