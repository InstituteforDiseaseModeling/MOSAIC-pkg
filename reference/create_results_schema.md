# Create Results Schema for Calibration Workflows

Creates a standardized schema for managing results matrices in
calibration workflows. This eliminates common bugs related to column
misalignment, manual index management, and inconsistent parameter
extraction.

## Usage

``` r
create_results_schema(config_base, include_metadata = TRUE)
```

## Arguments

- config_base:

  A base configuration object used to determine parameter structure

- include_metadata:

  Logical. Whether to include simulation metadata columns (sim, iter,
  seed)

## Value

A calibration_results_schema object containing:

- `metadata_cols`: Names of metadata columns

- `param_cols`: Names of parameter columns

- `all_cols`: All column names in order

- `n_metadata`: Number of metadata columns

- `n_params`: Number of parameter columns

- `n_total`: Total number of columns

- `param_template`: Template parameter vector for validation

## Details

The schema provides a single source of truth for results matrix
structure, eliminating manual column counting and index management. It
ensures consistent parameter extraction and provides validation for
parameter insertion.

Metadata columns are always placed first, followed by parameter columns.
The 'seed' parameter is automatically excluded from parameter columns if
present in the configuration to avoid duplication with metadata.

## See also

[`create_results_matrix`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/create_results_matrix.md),
[`insert_parameters`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/insert_parameters.md)

## Examples

``` r
if (FALSE) { # \dontrun{
config_base <- get_location_config("ETH")
schema <- create_results_schema(config_base)
print(schema$all_cols)  # See all column names
} # }
```
