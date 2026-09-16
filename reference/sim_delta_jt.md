# Environmental decay rate from suitability

`1 / (fast + pbeta(psi, a, b) * (slow - fast))`. The beta CDF maps
suitability in \[0, 1\] to \[0, 1\], potentially non-linearly, and that
factor interpolates the survival time between `decay_days_short` and
`decay_days_long`. So decay is FAST where suitability is low (`psi = 0`
gives `1 / fast`) and SLOW where it is high (`psi = 1` gives
`1 / slow`).

## Usage

``` r
sim_delta_jt(par)
```

## Arguments

- par:

  Parameters from
  [`sim_params()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_params.md).

## Value

A `[nticks, npatches]` matrix of per-day decay rates.

## Details

This is the engine's only scipy call (`scipy.stats.beta.cdf`); base R's
[`pbeta()`](https://rdrr.io/r/stats/Beta.html) is a drop-in.
