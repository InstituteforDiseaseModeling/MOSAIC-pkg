#' Read a JSON File into an R List
#'
#' @description
#' Reads a JSON file (optionally compressed with gzip) and converts it into a named R list.
#'
#' @param file_path A character string specifying the full file path to the input JSON file.
#'
#' @return A named R list containing the data from the JSON file.
#'
#' @details
#' The function first verifies that the specified file exists. If the file name ends with ".gz" (ignoring case),
#' it is assumed to be gzipped and is read using a gzfile connection. Otherwise, the file is read directly.
#' The JSON text is then parsed with \code{jsonlite::fromJSON()} into an R list.
#'
#' @examples
#' \dontrun{
#'   # Read from a plain JSON file.
#'   data_list <- read_json_to_list("output.json")
#'   print(data_list)
#'
#'   # Read from a gzipped JSON file.
#'   data_list_gz <- read_json_to_list("output.json.gz")
#'   print(data_list_gz)
#' }
#'
#' @import jsonlite
#' @export
#'

read_json_to_list <- function(file_path) {

     if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
          stop("You must provide a valid input file path.")
     }

     if (!file.exists(file_path)) stop("The file does not exist: ", file_path)

     # Hand the path (or connection) straight to fromJSON rather than
     # readLines() + paste(collapse = "\n"). `config_default.json` is 5.76 MB,
     # so the old route materialised a 5.76 MB intermediate string on top of
     # the line vector for no benefit: 0.167 s against 0.123 s, same result
     # (verified identical on the default config).
     if (grepl("\\.gz$", file_path, ignore.case = TRUE)) {
          con <- gzfile(file_path, "rt")
          on.exit(close(con), add = TRUE)
          return(jsonlite::fromJSON(con))
     }

     jsonlite::fromJSON(file_path)
}

# Session cache for configs read from disk, keyed on path + size + mtime.
#
# Why this exists: `run_simulation(config = "path.json")` is a supported input
# (see `?run_simulation`), and the parse is 0.167 s against a 0.94 s
# simulation -- an 18% tax on any loop that passes a path rather than a list.
# `run_fit_sandbox()` and the rolling-CV config reader have the same shape.
# Calibration is unaffected either way: its worker passes an in-memory list.
#
# Deliberately NOT applied to the exported `read_json_to_list()`, which stays a
# pure read -- callers of an exported reader should get the file as it is on
# disk now, and a cache there would be a silent semantic change.
.MOSAIC_JSON_CACHE <- new.env(parent = emptyenv())

# Invalidation is size + mtime (microsecond resolution), so writing a config
# and re-reading it in the same session re-parses, which is the behaviour any
# caller that round-trips a file through disk depends on.
.mosaic_json_stamp <- function(file_path) {
     i <- file.info(file_path)
     paste0(i$size[1], "@", format(i$mtime[1], "%Y-%m-%d %H:%M:%OS6"))
}

.mosaic_read_json_cached <- function(file_path, max_entries = 8L) {

     key   <- normalizePath(file_path, winslash = "/", mustWork = TRUE)
     stamp <- .mosaic_json_stamp(key)
     hit   <- .MOSAIC_JSON_CACHE[[key]]
     if (!is.null(hit) && identical(hit$stamp, stamp)) return(hit$value)

     value <- read_json_to_list(key)

     # A parsed 40-location config is ~3.4 MB, and workers are long-lived, so
     # the cache is bounded. Cleared wholesale rather than evicted one at a
     # time: the access pattern is a handful of configs per session, not a
     # working set worth an LRU.
     if (length(ls(.MOSAIC_JSON_CACHE, all.names = TRUE)) >= max_entries) {
          rm(list = ls(.MOSAIC_JSON_CACHE, all.names = TRUE),
             envir = .MOSAIC_JSON_CACHE)
     }
     assign(key, list(stamp = stamp, value = value), envir = .MOSAIC_JSON_CACHE)
     value
}
