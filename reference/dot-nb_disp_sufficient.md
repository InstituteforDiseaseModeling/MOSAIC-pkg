# Whether weekly totals carry enough evidence to estimate a dispersion

Whether weekly totals carry enough evidence to estimate a dispersion

## Usage

``` r
.nb_disp_sufficient(y, w = NULL)
```

## Arguments

- y:

  Numeric weekly totals.

- w:

  Optional weekly weights; weeks with a non-positive or missing weight
  do not count.

## Value

`TRUE` when the weeks meet the minimum number of weeks, total count and
number of non-zero weeks.
