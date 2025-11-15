# Generate Directory Paths for the MOSAIC Project

The `get_paths()` function generates a structured list of file paths for
different data and document directories in the MOSAIC project, based on
the provided root directory.

## Usage

``` r
get_paths(root = NULL)
```

## Arguments

- root:

  A string specifying the root directory of the MOSAIC project. All
  other paths will be generated relative to this root.

## Value

A named list containing the following paths:

- ROOT:

  The root directory provided by the user.

- DATA_RAW:

  Path to the raw data directory (under "MOSAIC-data/data/raw").

- DATA_PROCESSED:

  Path to the processed data directory (under
  "MOSAIC-data/data/processed").

- MODEL_INPUT:

  `NULL`, reserved for the path to the model input (if applicable).

- MODEL_OUTPUT:

  `NULL`, reserved for the path to the model output (if applicable).

- DOCS_FIGURES:

  Path to the figures directory (under "MOSAIC-docs/figures").

- DOCS_TABLES:

  Path to the tables directory (under "MOSAIC-docs/tables").

- DOCS_PARAMS:

  Path to the parameters directory (under "MOSAIC-docs/parameters").

## Details

This function helps organize the directory structure for data and
document storage in the MOSAIC project by generating paths for raw data,
processed data, figures, tables, and parameters. The paths are returned
as a named list and can be used to streamline the access to various
project-related directories.

## Examples

``` r
if (FALSE) { # \dontrun{
root_dir <- "/{full file path}/MOSAIC"
PATHS <- get_paths(root_dir)
print(PATHS$DATA_RAW)
} # }
```
