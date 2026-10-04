# Whether a dispersion fit is censored at the lower bound

A fit clamped at the lower bound (status `clamped_lower_bound`) or
within its 95% interval of it (`near_lower_bound`) is censored, not a
measurement: it takes the panel trend when one is supplied, and is left
out of fitting a trend.

## Usage

``` r
.nb_disp_censored(status)
```

## Arguments

- status:

  Character vector of fit statuses.

## Value

Logical vector.
