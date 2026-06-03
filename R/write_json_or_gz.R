#' Write a list to .json, .json.gz, or both -- byte-equal by construction
#'
#' @description
#' Serialises `data_list` to JSON text once and writes that single
#' in-memory string to either the plain `.json` file, the gzipped
#' `.json.gz` file, or both -- as selected by the `write_json` and
#' `write_gz` arguments. Because both outputs are written from the same
#' byte string, the `.json.gz` always decompresses to a file byte-
#' identical to the `.json` (when both are produced); the pair cannot
#' drift.
#'
#' Replaces the older `write_json_with_optional_gz()` "sidecar" model.
#' Both flags are first-class peers: any combination except
#' `write_json = FALSE, write_gz = FALSE` is allowed.
#'
#' @param data_list  Named list to serialise.
#' @param file_path  Base output path. Must end in `.json`. The `.gz`
#'                   output (if `write_gz = TRUE`) is written to
#'                   `paste0(file_path, ".gz")`.
#' @param write_json Logical. If `TRUE` (default), write the plain
#'                   `.json` file.
#' @param write_gz   Logical. If `TRUE`, write the gzipped `.json.gz`
#'                   file. Default `FALSE`.
#'
#' @return The base `file_path` (invisibly).
#'
#' @details
#' Both files come from a single `jsonlite::toJSON()` call. There is no
#' parallel serialisation, no patch-and-rewrite loop, and no possibility
#' for the `.json` and `.json.gz` to diverge.
#'
#' At least one of `write_json` / `write_gz` must be `TRUE`; passing
#' both as `FALSE` errors immediately (the function would otherwise
#' produce nothing).
#'
#' @examples
#' \dontrun{
#'   # Plain JSON only (default)
#'   write_json_or_gz(my_config, "config.json")
#'
#'   # gzip only
#'   write_json_or_gz(my_config, "config.json", write_json = FALSE, write_gz = TRUE)
#'
#'   # Both, guaranteed byte-equal
#'   write_json_or_gz(my_config, "config.json", write_json = TRUE, write_gz = TRUE)
#' }
#'
#' @seealso [write_list_to_json()]
#'
#' @export
write_json_or_gz <- function(data_list,
                             file_path,
                             write_json = TRUE,
                             write_gz   = FALSE) {

     if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
          stop("You must provide a valid output file path.")
     }
     if (!grepl("\\.json$", file_path, ignore.case = TRUE)) {
          stop("file_path must end in .json")
     }
     if (!isTRUE(write_json) && !isTRUE(write_gz)) {
          stop("At least one of write_json or write_gz must be TRUE; ",
               "otherwise the function would produce no output.")
     }

     output_dir <- dirname(file_path)
     if (!dir.exists(output_dir)) {
          stop("The directory for the output file path does not exist: ", output_dir)
     }

     # Serialise once. Both downstream writers consume the same byte
     # string so the .json and .json.gz are byte-equal by construction.
     json_text <- jsonlite::toJSON(data_list,
                                   digits     = NA,
                                   pretty     = TRUE,
                                   auto_unbox = TRUE)

     if (isTRUE(write_json)) {
          if (file.exists(file_path) && !file.remove(file_path)) {
               stop("Unable to remove existing file at: ",
                    normalizePath(file_path, winslash = "/"))
          }
          writeLines(json_text, con = file_path)
          if (!file.exists(file_path)) {
               stop("JSON file was not successfully created at: ",
                    normalizePath(file_path, winslash = "/", mustWork = FALSE))
          }
          message("JSON file successfully written to: ",
                  normalizePath(file_path, winslash = "/"))
     }

     if (isTRUE(write_gz)) {
          gz_path <- paste0(file_path, ".gz")
          if (file.exists(gz_path) && !file.remove(gz_path)) {
               stop("Unable to remove existing file at: ",
                    normalizePath(gz_path, winslash = "/"))
          }
          con <- gzfile(gz_path, "wt")
          writeLines(json_text, con = con)
          close(con)
          if (!file.exists(gz_path)) {
               stop("Gzipped JSON file was not successfully created at: ",
                    normalizePath(gz_path, winslash = "/", mustWork = FALSE))
          }
          message("Gzipped JSON file successfully written to: ",
                  normalizePath(gz_path, winslash = "/"))
     }

     invisible(file_path)
}
