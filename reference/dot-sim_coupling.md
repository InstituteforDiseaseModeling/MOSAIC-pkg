# Pearson coupling matrix between per-patch prevalence series

Pearson coupling matrix between per-patch prevalence series

## Usage

``` r
.sim_coupling(y)
```

## Arguments

- y:

  `[nobs, npatches]` matrix of prevalence fractions.

## Value

An `[npatches, npatches]` correlation matrix; rows and columns of
constant-prevalence patches are `NaN`.
