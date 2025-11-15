# Get Column Indices from Schema

Helper function to get column indices for specific data types from a
schema. This eliminates manual column counting and hard-coded indices.

## Usage

``` r
get_col_indices(schema, type = c("metadata", "params", "likelihood"))
```

## Arguments

- schema:

  A calibration_results_schema object

- type:

  Type of columns to retrieve: "metadata", "params", or "likelihood"

## Value

Integer vector of column indices

## Examples

``` r
if (FALSE) { # \dontrun{
schema <- create_results_schema(config_base)
param_indices <- get_col_indices(schema, "params")
likelihood_idx <- get_col_indices(schema, "likelihood")
} # }
```
