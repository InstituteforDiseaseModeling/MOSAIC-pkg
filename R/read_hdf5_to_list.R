#' Read an HDF5 File into an R List
#'
#' @description
#' \code{read_hdf5_to_list} opens an HDF5 file and recursively parses its contents into a named R list.
#'
#' @param file_path A character string specifying the full file path to the HDF5 file.
#'
#' @return A named R list representing the hierarchical structure of the HDF5 file.
#'
#' @examples
#' \dontrun{
#'   # Assuming you have written an HDF5 file using write_list_to_hdf5:
#'   output_file <- "output.h5"
#'   # Read the HDF5 file back into an R list.
#'   data_list <- read_hdf5_to_list(file_path = output_file)
#'   print(data_list)
#' }
#' @importFrom hdf5r H5File
#' @export
#'

read_hdf5_to_list <- function(file_path) {

     if (!file.exists(file_path)) stop("The file does not exist: ", file_path)

     # Open the HDF5 file for reading.
     h5_file <- hdf5r::H5File$new(file_path, mode = "r")

     # Internal recursive function to read groups and datasets.
     read_group <- function(h5_obj) {

          result <- list()
          members <- h5_obj$ls()

          for (i in seq_len(nrow(members))) {

               member_name <- members$name[i]
               member <- h5_obj[[member_name]]

               if (inherits(member, "H5Group")) {

                    result[[member_name]] <- read_group(member)

               } else {

                    # Read the dataset in full.
                    result[[member_name]] <- member[]

               }

               member$close()  # Close the object after reading.
          }

          return(result)
     }

     # Read the file's root group.
     out_list <- read_group(h5_file)
     h5_file$close_all()
     return(out_list)

}
