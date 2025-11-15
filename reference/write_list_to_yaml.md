# Write an R List to a YAML File

Takes a named R list and writes it to a YAML file.

## Usage

``` r
write_list_to_yaml(data_list, file_path, compress = FALSE)
```

## Arguments

- data_list:

  A named list containing the data to write.

- file_path:

  A character string specifying the full file path for the output YAML
  file.

- compress:

  Logical. If TRUE, the YAML is written in gzipped format and the file
  extension is forced to .yaml.gz. Default is FALSE.

## Value

This function does not return a value. It prints a message indicating
that the file was successfully written.

## Details

The function converts the R list to YAML text using
[`yaml::as.yaml()`](https://rdrr.io/pkg/yaml/man/as.yaml.html) and then
writes it out either to a plain text file or to a gzipped file if
`compress = TRUE`. The gzipped file is created using a connection opened
with [`gzfile()`](https://rdrr.io/r/base/connections.html).

## Examples

``` r
if (FALSE) { # \dontrun{
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

  # Write to a plain YAML file.
  output_file <- "output.yaml"
  write_list_to_yaml(data_list = sample_data, file_path = output_file, compress = FALSE)

  # Write to a gzipped YAML file.
  output_file_gz <- "output.yaml"  # Note: the function will change this to output.yaml.gz
  write_list_to_yaml(data_list = sample_data, file_path = output_file_gz, compress = TRUE)

  # Load the plain YAML file to inspect its contents.
  yaml_data <- yaml::read_yaml(output_file)
  print(yaml_data)
} # }
```
