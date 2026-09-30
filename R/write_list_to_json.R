#' Write an R List to a JSON File
#'
#' @description
#' Takes a named R list and writes it to a JSON file.
#'
#' @param data_list A named list containing the data to write.
#' @param file_path A character string specifying the full file path for the output JSON file.
#' @param compress Logical. If TRUE, the JSON is written in gzipped format and the file extension is forced to .json.gz.
#'        Default is FALSE.
#'
#' @return This function does not return a value. It prints a message indicating that the file was successfully written.
#'
#' @details
#' The function converts the R list to JSON text using \code{jsonlite::toJSON()} (with pretty printing enabled) and then writes it out either to a
#' plain text file or to a gzipped file if \code{compress = TRUE}. The gzipped file is created using a connection
#' opened with \code{gzfile()}. Numbers are written with 17 significant digits, enough to round-trip every double
#' exactly, so a config read back from the file reproduces a simulation bit for bit. The text is written to a
#' temporary file in the target directory and then renamed over \code{file_path}, so an interrupted write never
#' leaves a truncated file behind.
#'
#' @examples
#' \dontrun{
#'   sample_data <- list(
#'     group1 = list(
#'       value1 = rnorm(100),
#'       value2 = runif(100)
#'     ),
#'     group2 = list(
#'       message = "Hello, MOSAIC!",
#'       timestamp = Sys.time()
#'     )
#'   )
#'
#'   # Write to a plain JSON file.
#'   output_file <- "output.json"
#'   write_list_to_json(data_list = sample_data, file_path = output_file, compress = FALSE)
#'
#'   # Write to a gzipped JSON file.
#'   output_file_gz <- "output.json"  # Note: the function will change this to output.json.gz
#'   write_list_to_json(data_list = sample_data, file_path = output_file_gz, compress = TRUE)
#'
#'   # Load the plain JSON file to inspect its contents.
#'   json_data <- jsonlite::read_json(output_file)
#'   print(json_data)
#' }
#'
#' @import jsonlite
#' @export
#'

write_list_to_json <- function(data_list, file_path, compress = FALSE) {

     if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
          stop("You must provide a valid output file path.")
     }

     # When compression is enabled, ensure file_path ends with .json.gz.
     if (compress) {
          if (!grepl("\\.json\\.gz$", file_path, ignore.case = TRUE)) {

               file_path <- sub("\\.json$", "", file_path, ignore.case = TRUE)
               file_path <- paste0(file_path, ".json.gz")
          }
     }

     # Check that the directory exists.
     output_dir <- dirname(file_path)
     if (!dir.exists(output_dir)) stop("The directory for the output file path does not exist: ", output_dir)

     # 17 significant digits round-trip every double exactly (15, jsonlite's
     # digits = NA, does not); same constant as the run_MOSAIC() writers.
     json_text <- jsonlite::toJSON(data_list,
                                   digits = .MOSAIC_JSON_DIGITS,
                                   pretty = TRUE,
                                   auto_unbox = TRUE)

     # Atomic write: temp file in the same directory, then rename into place.
     tmp_path <- tempfile(pattern = ".tmp_write_list_to_json_", tmpdir = output_dir,
                          fileext = if (compress) ".json.gz" else ".json")
     on.exit(if (file.exists(tmp_path)) unlink(tmp_path), add = TRUE)

     if (compress) {
          con <- gzfile(tmp_path, "wt")
          writeLines(json_text, con = con)
          close(con)
     } else {
          writeLines(json_text, con = tmp_path)
     }

     if (!file.rename(tmp_path, file_path)) {
          stop("Unable to move the written JSON into place at: ",
               normalizePath(file_path, winslash = "/", mustWork = FALSE))
     }

     if (!file.exists(file_path)) stop("JSON file was not successfully created at: ", normalizePath(file_path, winslash = "/"))
     message("JSON file successfully written to: ", normalizePath(file_path, winslash = "/"))

}
