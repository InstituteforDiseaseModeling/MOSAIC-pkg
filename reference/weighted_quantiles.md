# Weighted quantiles

Calculates weighted quantiles for a vector of values using linear
interpolation between midpoint plotting positions.

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
for the interpolation. (Splitting out the sorted core lets hot callers
sort once and reuse the order.)

Each observation is placed at the midpoint of its own weight block,
\\(\sum\_{k \le i} w_k - w_i/2) / \sum_k w_k\\, and quantiles are
linearly interpolated between those positions. For equal weights the
positions are \\(i - 0.5)/n\\, so the unweighted case reduces to the
standard Hazen (type-5) quantile. Before v0.71.1 the upper edge
\\\sum\_{k \le i} w_k\\ was used instead, which biased every quantile
downward in proportion to how concentrated the weights were; see NEWS
for the size of the effect on
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md).

## Examples

``` r
x <- c(1, 2, 3, 4, 5)
w <- c(0.1, 0.2, 0.4, 0.2, 0.1)
weighted_quantiles(x, w, c(0.25, 0.5, 0.75))
#> [1] 2.166667 3.000000 3.833333
```
