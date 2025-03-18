#' Read an HDF5 File into an R List
#'
#' @description
#' Reads an HDF5 file (supports .h5 or .h5.gz formats) into a named R list.
#'
#' @param file_path Path to the .h5 or .h5.gz file.
#'
#' @return A named R list representing the hierarchical structure of the HDF5 file.
#'
#' @importFrom hdf5r H5File
#' @importFrom R.utils gunzip
#' @export
#'

read_hdf5_to_list <- function(file_path) {

     if (!file.exists(file_path)) stop("The file does not exist: ", file_path)

     is_gz <- grepl("\\.gz$", file_path)

     if (is_gz) {
          temp_file <- tempfile(fileext = ".h5")
          R.utils::gunzip(filename = file_path, destname = temp_file, overwrite = TRUE, remove = FALSE)
          h5_file <- hdf5r::H5File$new(temp_file, mode = "r")
     } else {
          h5_file <- hdf5r::H5File$new(file_path, mode = "r")
     }

     read_group <- function(h5_obj) {
          result <- list()
          members <- h5_obj$ls()

          for (i in seq_len(nrow(members))) {
               member_name <- members$name[i]
               member <- h5_obj[[member_name]]

               if (inherits(member, "H5Group")) {
                    result[[member_name]] <- read_group(member)
               } else {
                    # Use the read method with drop = TRUE to avoid dimensionality mismatch issues.
                    result[[member_name]] <- member$read(drop = TRUE)
               }
               member$close()
          }
          return(result)
     }

     out_list <- read_group(h5_file)
     h5_file$close_all()

     if (is_gz && file.exists(temp_file)) file.remove(temp_file)

     return(out_list)
}
