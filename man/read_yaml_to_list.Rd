% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_yaml_to_list.R
\name{read_yaml_to_list}
\alias{read_yaml_to_list}
\title{Read a YAML File into an R List}
\usage{
read_yaml_to_list(file_path)
}
\arguments{
\item{file_path}{A character string specifying the full file path for the input YAML file.}
}
\value{
A named R list containing the data read from the YAML file.
}
\description{
Reads a YAML file and converts its contents into a named R list.
}
\details{
This function reads a YAML file from disk. If the file is gzipped (i.e., its name ends with .gz),
the file is decompressed on the fly using a connection opened with \code{gzfile()}.
The YAML content is then parsed using \code{yaml::read_yaml()} or \code{yaml::yaml.load()}.
}
\examples{
\dontrun{
  # Read from a plain YAML file.
  input_file <- "output.yaml"
  data_list <- read_yaml_to_list(file_path = input_file)
  print(data_list)

  # Read from a gzipped YAML file.
  input_file_gz <- "output.yaml.gz"
  data_list_gz <- read_yaml_to_list(file_path = input_file_gz)
  print(data_list_gz)
}

}
