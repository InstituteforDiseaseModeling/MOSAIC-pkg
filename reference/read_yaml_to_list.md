# Read a YAML File into an R List

Reads a YAML file and converts its contents into a named R list.

## Usage

``` r
read_yaml_to_list(file_path)
```

## Arguments

- file_path:

  A character string specifying the full file path for the input YAML
  file.

## Value

A named R list containing the data read from the YAML file.

## Details

This function reads a YAML file from disk. If the file is gzipped (i.e.,
its name ends with .gz), the file is decompressed on the fly using a
connection opened with
[`gzfile()`](https://rdrr.io/r/base/connections.html). The YAML content
is then parsed using
[`yaml::read_yaml()`](https://rdrr.io/pkg/yaml/man/read_yaml.html) or
[`yaml::yaml.load()`](https://rdrr.io/pkg/yaml/man/yaml.load.html).

## Examples

``` r
if (FALSE) { # \dontrun{
  # Read from a plain YAML file.
  input_file <- "output.yaml"
  data_list <- read_yaml_to_list(file_path = input_file)
  print(data_list)

  # Read from a gzipped YAML file.
  input_file_gz <- "output.yaml.gz"
  data_list_gz <- read_yaml_to_list(file_path = input_file_gz)
  print(data_list_gz)
} # }
```
