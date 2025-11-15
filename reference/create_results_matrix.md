# Create Results Matrix Using Schema

Creates a pre-allocated results matrix with proper column structure
based on a calibration results schema.

## Usage

``` r
create_results_matrix(schema, n_rows = 1)
```

## Arguments

- schema:

  A calibration_results_schema object from
  [`create_results_schema()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/create_results_schema.md)

- n_rows:

  Number of rows to pre-allocate

## Value

A numeric matrix with proper column names and dimensions

## Examples

``` r
if (FALSE) { # \dontrun{
schema <- create_results_schema(config_base)
results_matrix <- create_results_matrix(schema, n_rows = 1000)
} # }
```
