# Create Default LASER Configuration

Generates a default configuration for running LASER model simulations.
Automatically defines default values of all model parameters and initial
conditions for the cholera metapopulation transmission model. The
function uses
[`make_LASER_config`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)
to check that all parameter values meet model specifications.

## Usage

``` r
get_default_LASER_config(PATHS)
```

## Arguments

- PATHS:

  A named list of file path components required for accessing input
  data. Expected list elements include:

  MODEL_INPUT

  :   Character string. Path to directory containing model input CSV
      files.

  DATA_WHO_DAILY

  :   Character string. Path to directory containing processed WHO
      cholera daily CSV data.

## Value

A list object containing all necessary parameters and initial conditions
for LASER model simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- list(
  MODEL_INPUT = "path/to/model_input",
  DATA_WHO_DAILY = "path/to/daily_data"
)

default_config <- get_default_config(PATHS)
} # }
```
