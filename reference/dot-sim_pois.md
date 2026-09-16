# Draw Poisson counts

Draw Poisson counts

## Usage

``` r
.sim_pois(ctl, site, lambda, npatches = length(lambda))
```

## Arguments

- ctl:

  Draw controller from
  [`sim_draws()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_draws.md).

- site:

  Draw-site label; must be one of `.SIM_DRAW_SITES`.

- lambda:

  Numeric vector (or scalar) of rates.

- npatches:

  Patch count, needed when `lambda` is scalar.

## Value

Integer vector of length `npatches`.
