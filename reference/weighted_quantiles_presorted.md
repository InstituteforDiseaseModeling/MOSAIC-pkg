# Weighted quantiles from already-sorted, pre-filtered inputs

Internal core of
[`weighted_quantiles`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/weighted_quantiles.md):
the cumulative-weight interpolation, assuming inputs are already
filtered (finite values, positive weights) and sorted ascending by value
with weights aligned. Exposed so hot callers that sort once and reuse
the order across many subsets (e.g.
[`optimize_ensemble_subset`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/optimize_ensemble_subset.md))
can skip the per-call [`order()`](https://rdrr.io/r/base/order.html).

## Usage

``` r
weighted_quantiles_presorted(x_sorted, w_sorted, probs)
```

## Arguments

- x_sorted:

  Numeric vector of values, sorted ascending, all finite.

- w_sorted:

  Numeric vector of weights aligned with `x_sorted`, all finite and
  positive.

- probs:

  Numeric vector of quantile probabilities (between 0 and 1).

## Value

Vector of weighted quantiles, one per element of `probs`.

## See also

[`weighted_quantiles`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/weighted_quantiles.md)
