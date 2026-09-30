# Aggregate a daily observation row to complete Monday-Sunday weeks

Aggregate a daily observation row to complete Monday-Sunday weeks

## Usage

``` r
.nb_disp_weekly(y, dates, w = NULL, offset = 0L)
```

## Arguments

- y:

  Numeric vector of daily observations (may contain `NA`).

- dates:

  Date vector the same length as `y`.

- w:

  Optional per-observation confidence weights the same length as `y`.
  Verified constant within every reporting week, so the weekly weight is
  the within-week mean.

- offset:

  Integer 0-6 block-boundary offset, from `.nb_disp_cadence`.

## Value

A data.frame with `week`, `y` (weekly total) and `w` (weekly weight), or
`NULL` when no complete week exists.
