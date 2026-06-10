# Weighted quantiles

Calculates weighted quantiles for a vector of values using linear
interpolation.

## Usage

``` r
weighted_quantiles(x, w, probs)
```

## Arguments

- x:

  Numeric vector of values

- w:

  Numeric vector of weights (same length as x)

- probs:

  Numeric vector of quantile probabilities (between 0 and 1)

## Value

Vector of weighted quantiles

## Details

Drops non-finite values and non-positive weights, sorts the survivors by
value, then delegates to
[`weighted_quantiles_presorted`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/weighted_quantiles_presorted.md)
for the cumulative-weight interpolation. (Splitting out the sorted core
lets hot callers sort once and reuse the order; the public behaviour is
unchanged.)

## Examples

``` r
x <- c(1, 2, 3, 4, 5)
w <- c(0.1, 0.2, 0.4, 0.2, 0.1)
weighted_quantiles(x, w, c(0.25, 0.5, 0.75))
#> [1] 1.75 2.50 3.25
```
