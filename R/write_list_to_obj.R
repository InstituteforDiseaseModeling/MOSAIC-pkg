#' Write an R List to a Python-Compatible Object
#'
#' @description
#' Takes a named R list and writes it to a Python-compatible `.obj` or `.obj.gz` file, preserving dictionary-like structure.
#'
#' @param data_list A named list containing the data to write.
#' @param file_path A character string specifying the full file path for the output `.obj` or `.obj.gz` file.
#' @param compress Logical. If TRUE, the file is saved as a gzip-compressed `.obj.gz` file. Default is FALSE.
#'
#' @return This function does not return a value. It prints a message indicating that the file was successfully written.
#'
#' @details
#' This function converts the R list to a Python-compatible dictionary using \code{reticulate::r_to_py()} and writes it to a `.obj`
#' or `.obj.gz` file using \code{sciris.sc.save()}. Since the virtual Python environment is already activated via `.onAttach()`,
#' the function assumes that the correct environment is in use.
#'
#' The function leverages `sciris.sc.save()` for file storage. Full documentation for `sciris.sc.save()` can be found here:
#' [Sciris save() documentation](https://docs.sciris.org/en/latest/api/_autosummary/sciris.sc_fileio.save.html)
#'
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
#'   # Write to an uncompressed Python-compatible object file.
#'   output_file <- "output.obj"
#'   write_list_to_obj(data_list = sample_data, file_path = output_file, compress = FALSE)
#'
#'   # Write to a compressed Python-compatible object file.
#'   output_file_gz <- "output.obj.gz"
#'   write_list_to_obj(data_list = sample_data, file_path = output_file_gz, compress = TRUE)
#'
#'   # Load in Python:
#'   # import sciris as sc
#'   # params = sc.load('output.obj.gz')
#'   # print(params)
#' }
#'
#' @import reticulate
#' @export
#'

write_list_to_obj <- function(data_list, file_path, compress = FALSE) {

     # Validate file_path
     if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
          stop("You must provide a valid output file path.")
     }

     # Ensure file_path has the correct extension
     if (compress) {
          if (!grepl("\\.obj\\.gz$", file_path, ignore.case = TRUE)) {
               file_path <- sub("\\.obj$", "", file_path, ignore.case = TRUE)
               file_path <- paste0(file_path, ".obj.gz")
          }
     } else {
          if (!grepl("\\.obj$", file_path, ignore.case = TRUE)) {
               stop("Output file must have a .obj or .obj.gz extension.")
          }
     }

     # Check that the directory exists
     output_dir <- dirname(file_path)
     if (!dir.exists(output_dir)) {
          stop("The directory for the output file path does not exist: ", output_dir)
     }

     # Remove any existing file
     if (file.exists(file_path)) {
          if (!file.remove(file_path)) {
               stop("Unable to remove existing file at: ", normalizePath(file_path, winslash = "/"))
          }
     }

     define_precision <- function(x, digits) {
          if (is.numeric(x)) {
               return(signif(x, digits = digits))  # or use round(x, digits) for decimal precision
          } else if (is.list(x)) {
               return(lapply(x, define_precision, digits = digits))
          } else {
               return(x)
          }
     }

     # Optionally reduce precision of numeric data
     data_list <- define_precision(data_list, digits = 4)

     # Convert R list to Python dictionary
     py_data <- tryCatch(
          reticulate::r_to_py(data_list),
          error = function(e) stop("Failed to convert R list to Python dictionary: ", e$message)
     )

     # Import sciris and check if it's available
     sc <- tryCatch(
          reticulate::import("sciris"),
          error = function(e) stop("Failed to import 'sciris' in Python environment: ", e$message)
     )

     # Write the object file with different settings based on compression
     tryCatch({
          if (compress) {
               sc$save(
                    filename = file_path,
                    obj = py_data,
                    folder = NULL,
                    method = "pickle",
                    compression = "gzip",
                    compresslevel = as.integer(9),
                    verbose = 0,
                    sanitizepath = TRUE,
                    die = FALSE,
                    allow_empty = FALSE
               )
          } else {
               sc$save(
                    filename = file_path,
                    obj = py_data,
                    folder = NULL,
                    method = "pickle",
                    compression = "none",
                    compresslevel = as.integer(5),
                    verbose = 0,
                    sanitizepath = TRUE,
                    die = FALSE,
                    allow_empty = FALSE
               )
          }
     }, error = function(e) {
          stop("Failed to write Python-compatible object file: ", e$message)
     })

     # Confirm successful file creation
     if (!file.exists(file_path)) {
          stop("Object file was not successfully created at: ", normalizePath(file_path, winslash = "/"))
     }

     message("Python-compatible object successfully written to: ", normalizePath(file_path, winslash = "/"))
}
