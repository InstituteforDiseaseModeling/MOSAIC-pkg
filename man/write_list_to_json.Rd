% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_list_to_json.R
\name{write_list_to_json}
\alias{write_list_to_json}
\title{Write an R List to a JSON File}
\usage{
write_list_to_json(data_list, file_path, compress = FALSE)
}
\arguments{
\item{data_list}{A named list containing the data to write.}

\item{file_path}{A character string specifying the full file path for the output JSON file.}

\item{compress}{Logical. If TRUE, the JSON is written in gzipped format and the file extension is forced to .json.gz.
Default is FALSE.}
}
\value{
This function does not return a value. It prints a message indicating that the file was successfully written.
}
\description{
Takes a named R list and writes it to a JSON file.
}
\details{
The function converts the R list to JSON text using \code{jsonlite::toJSON()} (with pretty printing enabled) and then writes it out either to a
plain text file or to a gzipped file if \code{compress = TRUE}. The gzipped file is created using a connection
opened with \code{gzfile()}.
}
\examples{
\dontrun{
  sample_data <- list(
    group1 = list(
      value1 = rnorm(100),
      value2 = runif(100)
    ),
    group2 = list(
      message = "Hello, MOSAIC!",
      timestamp = Sys.time()
    )
  )

  # Write to a plain JSON file.
  output_file <- "output.json"
  write_list_to_json(data_list = sample_data, file_path = output_file, compress = FALSE)

  # Write to a gzipped JSON file.
  output_file_gz <- "output.json"  # Note: the function will change this to output.json.gz
  write_list_to_json(data_list = sample_data, file_path = output_file_gz, compress = TRUE)

  # Load the plain JSON file to inspect its contents.
  json_data <- jsonlite::read_json(output_file)
  print(json_data)
}

}
