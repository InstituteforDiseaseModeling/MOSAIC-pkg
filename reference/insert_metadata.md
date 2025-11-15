# Insert Metadata into Results Row

Safely inserts metadata values into a results row using schema
structure.

## Usage

``` r
insert_metadata(
  result_row,
  schema,
  sim_id = NA,
  iter = NA,
  seed = NA,
  likelihood = NA
)
```

## Arguments

- result_row:

  Numeric vector representing a single results row

- schema:

  A calibration_results_schema object

- sim_id:

  Simulation ID (optional)

- iter:

  Iteration number (optional)

- seed:

  Random seed (optional)

- likelihood:

  Log-likelihood value (optional)

## Value

Updated result_row with metadata inserted

## Examples

``` r
if (FALSE) { # \dontrun{
schema <- create_results_schema(config_base)
result_row <- rep(NA_real_, schema$n_total)
result_row <- insert_metadata(result_row, schema, sim_id = 1, iter = 1, seed = 123)
} # }
```
