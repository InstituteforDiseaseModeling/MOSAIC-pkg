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

     # Read file as gzipped if the extension indicates gzip.
     if (grepl("\\.gz$", file_path, ignore.case = TRUE)) {

          con <- gzfile(file_path, "rt")
          json_text <- readLines(con)
          close(con)

     } else {
          json_text <- readLines(file_path)
     }

     # Combine the lines into a single JSON string.
     json_string <- paste(json_text, collapse = "\n")

     # Convert the JSON string into an R list.
     data_list <- jsonlite::fromJSON(json_string)

     return(data_list)
}
