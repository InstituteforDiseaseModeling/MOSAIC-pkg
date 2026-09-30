# Detect whether a daily series is a downscaled weekly series

A weekly total divided by 7 and rounded leaves every Monday-Sunday block
with at most two distinct values, and those two adjacent integers. All
40 MOSAIC surveillance locations match this signature in 100% of
complete blocks.

## Usage

``` r
.nb_disp_cadence(y, dates)
```

## Arguments

- y:

  Numeric vector of observations on a daily grid.

- dates:

  Date vector the same length as `y`.

## Value

A list with `share` (proportion of complete weekly blocks matching the
signature, `NA_real_` when too few blocks exist to judge) and `offset`
(the detected block boundary, 0-6 days from Monday).
