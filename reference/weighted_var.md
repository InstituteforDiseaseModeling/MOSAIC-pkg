# Weighted Statistical Functions for Posterior Analysis

Helper functions for calculating weighted statistics used in posterior
parameter distribution analysis. Weighted variance calculation

## Usage

``` r
weighted_var(x, w)
```

## Arguments

- x:

  Numeric vector of values

- w:

  Numeric vector of weights (same length as x)

## Value

Weighted variance (scalar)

## Details

Calculates the bias-corrected weighted variance for a vector of values.

Uses the bias correction: Var = sum(w \* (x - mu)^2) / (sum(w) -
sum(w^2)/sum(w)) where mu is the weighted mean. Returns 0 for single
values or invalid denominators.

## Examples

``` r
x <- c(1, 2, 3, 4, 5)
w <- c(0.1, 0.2, 0.4, 0.2, 0.1)
weighted_var(x, w)
#> [1] 1.621622
```
