#' Write an R List to a YAML File
#'
#' @description
#' Takes a named R list and writes it to a YAML file.
#'
#' @param data_list A named list containing the data to write.
#' @param file_path A character string specifying the full file path for the output YAML file.
#' @param compress Logical. If TRUE, the YAML is written in gzipped format and the file extension is forced to .yaml.gz.
#'        Default is FALSE.
#'
#' @return This function does not return a value. It prints a message indicating that the file was successfully written.
#'
#' @details
#' The function converts the R list to YAML text using \code{yaml::as.yaml()} and then writes it out either to a
#' plain text file or to a gzipped file if \code{compress = TRUE}. The gzipped file is created using a connection
#' opened with \code{gzfile()}.
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
#'   # Write to a plain YAML file.
#'   output_file <- "output.yaml"
#'   write_list_to_yaml(data_list = sample_data, file_path = output_file, compress = FALSE)
#'
#'   # Write to a gzipped YAML file.
#'   output_file_gz <- "output.yaml"  # Note: the function will change this to output.yaml.gz
#'   write_list_to_yaml(data_list = sample_data, file_path = output_file_gz, compress = TRUE)
#'
#'   # Load the plain YAML file to inspect its contents.
#'   yaml_data <- yaml::read_yaml(output_file)
#'   print(yaml_data)
#' }
#'
#' @import yaml
#' @export
#'

write_list_to_yaml <- function(data_list, file_path, compress = FALSE) {

     if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
          stop("You must provide a valid output file path.")
     }

     # When compression is enabled, ensure file_path ends with .yaml.gz.
     if (compress) {

          if (!grepl("\\.yaml\\.gz$", file_path, ignore.case = TRUE)) {

               file_path <- sub("\\.yaml$", "", file_path, ignore.case = TRUE)
               file_path <- paste0(file_path, ".yaml.gz")

          }
     }

     # Check that the directory exists.
     output_dir <- dirname(file_path)
     if (!dir.exists(output_dir)) stop("The directory for the output file path does not exist: ", output_dir)

     # Remove any existing file at the specified file path.
     if (file.exists(file_path)) {

          if (!file.remove(file_path)) stop("Unable to remove existing file at: ", normalizePath(file_path, winslash = "/"))

     }

     yaml_text <- yaml::as.yaml(data_list, precision = 4)

     if (compress) {

          con <- gzfile(file_path, "wt")
          writeLines(yaml_text, con = con)
          close(con)

     } else {

          writeLines(yaml_text, con = file_path)

     }

     if (!file.exists(file_path)) stop("YAML file was not successfully created at: ", normalizePath(file_path, winslash = "/"))
     message("YAML file successfully written to: ", normalizePath(file_path, winslash = "/"))
}
