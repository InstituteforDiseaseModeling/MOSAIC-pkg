# Insert Parameters into Results Row

Safely inserts parameter values into a results row using schema
validation. This ensures parameter structure consistency and catches
mismatches early.

## Usage

``` r
insert_parameters(result_row, schema, params_config)
```

## Arguments

- result_row:

  Numeric vector representing a single results row

- schema:

  A calibration_results_schema object

- params_config:

  Configuration object containing parameters to extract

## Value

Updated result_row with parameters inserted

## Examples

``` r
if (FALSE) { # \dontrun{
schema <- create_results_schema(config_base)
result_row <- rep(NA_real_, schema$n_total)
params <- sample_parameters(PATHS, config_base, seed = 123)
result_row <- insert_parameters(result_row, schema, params)
} # }
```
