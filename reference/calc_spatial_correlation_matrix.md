# Build the full spatial–correlation matrix across all locations

Given LASER simulation outputs, this function constructs the \\J \times
J\\ matrix \\\mathbf C = \[\mathcal{C}\_{ij}\]\\ of Pearson
spatial–correlation coefficients (Keeling & Rohani 2002) for every pair
of locations.

## Usage

``` r
calc_spatial_correlation_matrix(I_sym, I_asym, N)
```

## Arguments

- I_sym:

  Numeric matrix (J × T). Symptomatic infections.

- I_asym:

  Numeric matrix (J × T). Asymptomatic infections. Must have the same
  dimensions as `I_sym`.

- N:

  Population sizes. Accepts

  - a single scalar (same for every j & t),

  - a length-J vector (different per location, constant in time), or

  - a full J × T matrix (time‑varying populations).

## Value

A J × J symmetric numeric matrix whose \\(i,j)\\ entry is
\\\mathcal{C}\_{ij}\\; diagonal elements are set to 1. Pairs that cannot
be computed (all `NA`, zero variance, etc.) are `NA_real_`.

## Details

All three input objects must be arranged as (locations J × time T) (rows
are the J locations; columns are the T time steps).

Total infections at time t in location j are taken as \\I\_{jt} =
I^{(\mathrm{sym})}\_{jt} + I^{(\mathrm{asym})}\_{jt}\\; these are
converted to prevalence by dividing by the matching population size
\\N\_{jt}\\. Pair-wise correlations are then computed with
[`calc_spatial_correlation()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_spatial_correlation.md).

## See also

[`calc_spatial_correlation`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_spatial_correlation.md)
