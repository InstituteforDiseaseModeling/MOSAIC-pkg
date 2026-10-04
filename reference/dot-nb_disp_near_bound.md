# Whether a dispersion estimate is within its 95% interval of the lower bound

`TRUE` where the log-scale 95% interval of `k`,
`k * exp(-1.96 * se / k)`, reaches the lower bound of 0.1: such a fit
cannot be told apart from one clamped at the bound (see
[`est_nb_dispersion`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md)).

## Usage

``` r
.nb_disp_near_bound(k, se)
```

## Arguments

- k:

  Numeric vector of dispersion estimates.

- se:

  Numeric vector of their standard errors.

## Value

Logical vector; `FALSE` where `k` or `se` is not finite.
