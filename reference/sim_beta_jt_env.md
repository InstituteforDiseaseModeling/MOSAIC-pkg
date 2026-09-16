# Environmental transmission rate from suitability

`beta_j0_env * (1 + (psi - psi_bar) / psi_bar)`, where `psi_bar` is each
patch's mean suitability *over time*. The normalisation makes the matrix
a relative-suitability modulation of a per-patch baseline rather than an
absolute rate, so `beta_j0_env` stays interpretable and calibratable.

## Usage

``` r
sim_beta_jt_env(par)
```

## Arguments

- par:

  Parameters from
  [`sim_params()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_params.md).

## Value

A `[nticks, npatches]` matrix.
