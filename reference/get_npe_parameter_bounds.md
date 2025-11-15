# Get NPE Parameter Bounds

Extracts mathematically correct bounds for NPE training from prior
distributions, applying universal constraints based on parameter type.
This ensures NPE training respects theoretical parameter constraints
rather than using empirical data ranges.

## Usage

``` r
get_npe_parameter_bounds(
  param_names,
  priors_file = NULL,
  safety_buffer = 1e-10,
  verbose = FALSE
)
```

## Arguments

- param_names:

  Character vector of parameter names

- priors_file:

  Path to priors.json file (optional)

- safety_buffer:

  Numeric buffer for numerical stability (default: 1e-10)

- verbose:

  Logical whether to print diagnostics

## Value

Data frame with columns: parameter, min, max
