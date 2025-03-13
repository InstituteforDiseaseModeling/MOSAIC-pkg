#' Write an R List to an HDF5 File with Optional Compression
#'
#' @description
#' \code{write_list_to_hdf5} takes a named R list and writes it recursively to an HDF5 file. The hierarchical
#' structure of the list is preserved by creating groups for nested lists and writing atomic values, vectors, or
#' matrices directly as datasets. If \code{compress} is TRUE, every dataset is written using deflate compression
#' at level 9.
#'
#' @param data_list A named list containing the data to write.
#' @param file_path A character string specifying the full file path for the output HDF5 file.
#' @param compress Logical. If TRUE, HDF5 file datasets are created with compression level 9. Default is FALSE.
#'
#' @return This function does not return a value. It prints a message indicating that the file was successfully written.
#'
#' @details
#' The function checks that the output directory exists and removes any existing file at \code{file_path}. It opens
#' an HDF5 file for writing, then uses an internal recursive function to traverse the R list. For each atomic value
#' (or matrix), if \code{compress} is TRUE, the dataset is created with level 9 compression (using a default chunk size);
#' otherwise it is written without compression.
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
#'   # Write the list to a specified HDF5 file with compression enabled.
#'   output_file <- "output.h5"
#'   write_list_to_hdf5(data_list = sample_data, file_path = output_file, compress = TRUE)
#'
#'   # Load the file to inspect its contents.
#'   h5_file <- hdf5r::H5File$new(output_file, mode = "r")
#'   print(h5_file$ls())
#'   print(h5_file[["group1"]][["value1"]][,])
#'   h5_file$close_all()
#' }
#'
#' @importFrom hdf5r H5File
#' @export
write_list_to_hdf5 <- function(data_list, file_path, compress = FALSE) {

     if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
          stop("You must provide a valid output file path.")
     }

     # Check that the directory exists.
     output_dir <- dirname(file_path)
     if (!dir.exists(output_dir)) {
          stop("The directory for the output file path does not exist: ", output_dir)
     }

     # If a file already exists at this location, remove it.
     if (file.exists(file_path)) {
          if (!file.remove(file_path)) {
               stop("Unable to remove existing file at: ", normalizePath(file_path, winslash = "/"))
          }
     }

     # Open an HDF5 file for writing.
     h5_file <- hdf5r::H5File$new(file_path, mode = "w")

     # Internal recursive function to write the list to the HDF5 object.
     write_recursive <- function(h5_obj, lst) {

          for (item_name in names(lst)) {

               item_value <- lst[[item_name]]

               if (is.list(item_value)) {

                    # Create a subgroup for nested lists.
                    subgroup <- h5_obj$create_group(item_name)
                    write_recursive(h5_obj = subgroup, lst = item_value)
                    subgroup$close()

               } else {

                    if (compress) {

                         # Create the dataset with compression level 9.
                         # Note: Order of named arguments is re-arranged to avoid ambiguity.
                         h5_obj$create_dataset(
                              name = item_name,
                              robj = item_value,
                              chunk_dims = 'auto',
                              gzip_level = 9
                         )

                    } else {

                         # Write atomic or matrix objects without compression.
                         h5_obj[[item_name]] <- item_value

                    }
               }
          }
     }

     # Write the provided list to the HDF5 file.
     write_recursive(h5_obj = h5_file, lst = data_list)

     h5_file$close_all()

     if (!file.exists(file_path)) {
          stop("HDF5 file was not successfully created at: ", normalizePath(file_path, winslash = "/"))
     }

     message("HDF5 file successfully written to: ", normalizePath(file_path, winslash = "/"))
}
