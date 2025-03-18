#' Write an R list to an HDF5 file
#'
#' Writes an R list to an HDF5 file (.h5 or .h5.gz) format.
#'
#' @param data_list A named list containing the data to write.
#' @param file_path A character string specifying the full file path, including file extension (.h5 or .h5.gz).
#' @param compress_chunks Logical. If TRUE, applies internal dataset compression (level 9).
#' @param compress_file Logical. If TRUE, creates a gzip-compressed copy (.gz) without removing the original file.
#'
#' @return No value returned. Prints message upon successful writing.
#'
#' @importFrom hdf5r H5File
#' @importFrom R.utils gzip
#' @export
#'

write_list_to_hdf5 <- function(data_list,
                               file_path,
                               compress_chunks = FALSE,
                               compress_file = FALSE) {

     if (missing(file_path)) stop("You must provide a valid output file path.")

     output_dir <- dirname(file_path)
     if (!dir.exists(output_dir)) stop("The directory for the output file path does not exist: ", output_dir)

     is_gz <- grepl("\\.gz$", file_path)
     base_file_ext <- tools::file_ext(gsub("\\.gz$", "", file_path))

     if (base_file_ext != "h5") stop("Unsupported file extension: ", base_file_ext)

     if (!compress_file && file.exists(file_path)) file.remove(file_path)

     temp_file <- tempfile(fileext = paste0(".", base_file_ext))
     h5_file <- hdf5r::H5File$new(temp_file, mode = "w")

     write_recursive <- function(h5_obj, lst) {

          for (item_name in names(lst)) {

               item_value <- lst[[item_name]]

               if (is.list(item_value)) {

                    subgroup <- h5_obj$create_group(item_name)
                    write_recursive(h5_obj = subgroup, lst = item_value)
                    subgroup$close()

               } else {

                    if (compress_chunks) {

                         h5_obj$create_dataset(name = item_name,
                                               robj = item_value,
                                               chunk_dims = "auto",
                                               gzip_level = 9)

                    } else {

                         h5_obj[[item_name]] <- item_value

                    }
               }
          }
     }

     write_recursive(h5_obj = h5_file, lst = data_list)
     h5_file$close_all()

     final_path <- if (compress_file || is_gz) {

          if (is_gz) file_path else paste0(file_path, ".gz")

     } else {

          file_path

     }

     if (compress_file || is_gz) {

          R.utils::gzip(temp_file, destname = final_path, overwrite = TRUE, remove = TRUE)

     } else {

          file.copy(temp_file, final_path, overwrite = TRUE)
          file.remove(temp_file)

     }

     if (!file.exists(final_path)) stop("File was not successfully created at: ", final_path)
     message("File successfully written to: ", normalizePath(final_path, winslash = "/"))

}
