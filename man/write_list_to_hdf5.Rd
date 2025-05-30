% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_list_to_hdf5.R
\name{write_list_to_hdf5}
\alias{write_list_to_hdf5}
\title{Write an R list to an HDF5 file}
\usage{
write_list_to_hdf5(
  data_list,
  file_path,
  compress_chunks = FALSE,
  compress_file = FALSE
)
}
\arguments{
\item{data_list}{A named list containing the data to write.}

\item{file_path}{A character string specifying the full file path, including file extension (.h5 or .h5.gz).}

\item{compress_chunks}{Logical. If TRUE, applies internal dataset compression (level 9).}

\item{compress_file}{Logical. If TRUE, creates a gzip-compressed copy (.gz) without removing the original file.}
}
\value{
No value returned. Prints message upon successful writing.
}
\description{
Writes an R list to an HDF5 file (.h5 or .h5.gz) format.
}
