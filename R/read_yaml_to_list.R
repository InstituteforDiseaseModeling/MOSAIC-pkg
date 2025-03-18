#' Read a YAML File into an R List
#'
#' @description
#' Reads a YAML file and converts its contents into a named R list.
#'
#' @param file_path A character string specifying the full file path for the input YAML file.
#'
#' @return A named R list containing the data read from the YAML file.
#'
#' @details
#' This function reads a YAML file from disk. If the file is gzipped (i.e., its name ends with .gz),
#' the file is decompressed on the fly using a connection opened with \code{gzfile()}.
#' The YAML content is then parsed using \code{yaml::read_yaml()} or \code{yaml::yaml.load()}.
#'
#' @examples
#' \dontrun{
#'   # Read from a plain YAML file.
#'   input_file <- "output.yaml"
#'   data_list <- read_yaml_to_list(file_path = input_file)
#'   print(data_list)
#'
#'   # Read from a gzipped YAML file.
#'   input_file_gz <- "output.yaml.gz"
#'   data_list_gz <- read_yaml_to_list(file_path = input_file_gz)
#'   print(data_list_gz)
#' }
#'
#' @import yaml
#' @export
#'

read_yaml_to_list <- function(file_path) {

     if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
          stop("You must provide a valid input file path.")
     }

     if (!file.exists(file_path)) stop("The file does not exist: ", file_path)

     # Check if the file is gzipped based on its extension.
     if (grepl("\\.gz$", file_path, ignore.case = TRUE)) {

          con <- gzfile(file_path, "rt")
          yaml_text <- paste(readLines(con), collapse = "\n")
          close(con)
          data_list <- yaml::yaml.load(yaml_text)

     } else {

          data_list <- yaml::read_yaml(file_path)

     }

     return(data_list)
}
