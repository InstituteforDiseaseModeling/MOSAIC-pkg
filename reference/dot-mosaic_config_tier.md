# Surveillance trust tiers carried by a config

`config$reported_tier` is an integer matrix aligned with
`reported_cases` (1 observed, 2 reconstructed, 3 imputed; `NA` where the
week has no observation), built by `make_config_default.R` from the
surveillance `disaggregation_method`. The confidence weights cannot
stand in for it: observed AI weeks and documented zeros carry 0.8-0.95
while spread WHO reports carry 0.5-0.9.

## Usage

``` r
.mosaic_config_tier(config)
```

## Arguments

- config:

  A config list.

## Value

The tier matrix, or `NULL` when the config has none.
