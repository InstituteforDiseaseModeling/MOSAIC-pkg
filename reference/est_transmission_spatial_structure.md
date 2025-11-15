# Estimate spatial transmission structure using INLA

Prepare input data and fit a negative-binomial spatial random-field
model (SPDE + IID site effect) to obtain location-specific
multiplicative transmission modifiers.

## Usage

``` r
est_transmission_spatial_structure(PATHS, config)
```

## Arguments

- PATHS:

  A named list with at minimum:

  DATA_WORLD_BANK, DATA_ELEVATION, DATA_CLIMATE

  :   Character paths to covariate files.

  MODEL_INPUT

  :   Character path to the directory where the parameter CSV will be
      written.

- config:

  A named list with elements…

## Value

A list with components

- relative_multiplier:

  Numeric vector \\\exp(u_j)\\.

- model:

  Fitted `INLA` object.

- inputs:

  Internal objects (`mesh`, `data_list`, …).

- data:

  Merged site-level data frame used in the fit.

## Details

The response is total reported incidence aggregated over the analysis
window. A log-offset of \\\log(N_j \times T)\\ is applied, where \\N_j\\
is site-level population and \\T\\ the number of observation days. The
latent field is a Matérn SPDE (Lindgren et al., 2011). A CSV of the
estimated multipliers is written to
`file.path(PATHS$MODEL_INPUT, "param_beta_relative_multiplier.csv")`.

## References

Lindgren, F., Rue, H. & Lindström, J. (2011). …

## Examples

``` r
if (FALSE) { # \dontrun{
  paths  <- list(MODEL_INPUT = tempdir(), DATA_WORLD_BANK = "...", ...)
  config <- MOSAIC::config_default[1:3]   # first three sites
  res <- est_transmission_spatial_structure(paths, config)
  head(res$relative_multiplier)
} # }
```
