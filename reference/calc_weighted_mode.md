# Weighted mode estimation using kernel density

Estimates the mode of a weighted distribution using kernel density
estimation.

## Usage

``` r
calc_weighted_mode(x, w)
```

## Arguments

- x:

  Numeric vector of values

- w:

  Numeric vector of weights (same length as x)

## Value

Estimated mode (scalar)

## Details

Uses kernel density estimation with weights to find the mode (peak
density). Falls back to weighted median if density estimation fails.

## Examples

``` r
x <- c(1, 2, 2, 3, 3, 3, 4, 4, 5)
w <- rep(1, 9)
calc_weighted_mode(x, w)
#> [1] 3.008918
```
