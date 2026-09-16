# Draw binomial counts

Draw binomial counts

## Usage

``` r
.sim_binom(ctl, site, n, p)
```

## Arguments

- ctl:

  Draw controller from
  [`sim_draws()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_draws.md).

- site:

  Draw-site label; must be one of `.SIM_DRAW_SITES`.

- n:

  Integer vector of trial counts, length `npatches`.

- p:

  Numeric vector (or scalar) of success probabilities.

## Value

Integer vector of length `npatches`.
