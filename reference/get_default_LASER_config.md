# Create Default LASER Configuration

Generates a default configuration for running LASER model simulations.
Automatically defines default values of all model parameters and initial
conditions for the cholera metapopulation transmission model. The
function uses
[`make_LASER_config`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_LASER_config.md)
to check that all parameter values meet model specifications.

## Usage

``` r
get_default_LASER_config(PATHS = NULL)
```

## Arguments

- PATHS:

  Optional. Retained for backwards compatibility; no longer used. The
  default config is now located via
  [`system.file()`](https://rdrr.io/r/base/system.file.html) so the
  function works from any installed MOSAIC package.

## Value

A list object containing all necessary parameters and initial conditions
for LASER model simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
default_config <- get_default_LASER_config()
} # }
```
