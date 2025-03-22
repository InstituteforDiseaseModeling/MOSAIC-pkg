#' Read a Python-Compatible Object File into an R List
#'
#' @description
#' Reads a Python-compatible `.obj` or `.obj.gz` file and converts it back into an R list.
#'
#' @param file_path A character string specifying the full file path of the `.obj` or `.obj.gz` file to load.
#'
#' @return A named R list containing the data from the `.obj` file.
#'
#' @details
#' This function uses \code{sciris.sc.load()} from Python to read the `.obj` file, which is then converted back into
#' an R list using \code{reticulate::py_to_r()}. If the file is compressed (`.obj.gz`), it is automatically decompressed.
#' The function assumes that the virtual Python environment has already been activated via `.onAttach()`.
#'
#' @examples
#' \dontrun{
#'   # Load an uncompressed .obj file
#'   params <- read_obj_to_list("config.obj")
#'
#'   # Load a compressed .obj.gz file
#'   params_gz <- read_obj_to_list("config.obj.gz")
#'
#'   # Inspect contents
#'   str(params)
#' }
#'
#' @import reticulate
#' @export
#'

read_obj_to_list <- function(file_path) {

     # Validate file_path
     if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
          stop("You must provide a valid file path.")
     }

     # Ensure file has a valid .obj or .obj.gz extension
     if (!grepl("\\.obj(\\.gz)?$", file_path, ignore.case = TRUE)) {
          stop("File must have a .obj or .obj.gz extension.")
     }

     # Check if the file exists
     if (!file.exists(file_path)) {
          stop("File not found: ", normalizePath(file_path, winslash = "/"))
     }

     # Import sciris and check if it's available
     sc <- tryCatch(
          reticulate::import("sciris"),
          error = function(e) stop("Failed to import 'sciris' in Python environment: ", e$message)
     )

     # Load the object file
     py_data <- tryCatch(
          sc$load(file_path),
          error = function(e) stop("Failed to load Python-compatible object file: ", e$message)
     )

     # Convert Python dictionary back to R list
     r_data <- tryCatch(
          reticulate::py_to_r(py_data),
          error = function(e) stop("Failed to convert Python object to R list: ", e$message)
     )

     message("Python-compatible object successfully loaded from: ", normalizePath(file_path, winslash = "/"))
     return(r_data)
}
