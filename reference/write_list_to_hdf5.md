# Write an R list to an HDF5 file

Writes an R list to an HDF5 file (.h5 or .h5.gz) format.

## Usage

``` r
write_list_to_hdf5(
  data_list,
  file_path,
  compress_chunks = FALSE,
  compress_file = FALSE
)
```

## Arguments

- data_list:

  A named list containing the data to write.

- file_path:

  A character string specifying the full file path, including file
  extension (.h5 or .h5.gz).

- compress_chunks:

  Logical. If TRUE, applies internal dataset compression (level 9).

- compress_file:

  Logical. If TRUE, creates a gzip-compressed copy (.gz) without
  removing the original file.

## Value

No value returned. Prints message upon successful writing.
