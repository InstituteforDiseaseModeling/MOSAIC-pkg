# Calculate Prior and Posterior Quantiles with KL Divergence

Calculates quantiles for both prior (all simulations) and posterior
(weighted best subset) distributions, with KL divergence between them
using KDE.

## Usage

``` r
calc_model_posterior_quantiles(
  results,
  probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
  output_dir = "./results",
  verbose = TRUE
)
```

## Arguments

- results:

  Data frame with calibration results containing parameter columns and
  index columns (is_finite, is_retained, is_best_subset)

- probs:

  Numeric vector of quantile probabilities (default: c(0.025, 0.25, 0.5,
  0.75, 0.975))

- output_dir:

  Directory path to save results (default: "./results")

- verbose:

  Logical; print progress messages (default: TRUE)

## Value

Data frame with both prior and posterior quantiles plus KL divergence

## Details

The function:

1.  Calculates unweighted quantiles from ALL finite simulations (prior)

2.  Calculates weighted quantiles from best subset (posterior)

3.  Computes KL divergence using KDE on empirical distributions

4.  Returns both prior and posterior rows with a 'type' identifier

5.  KL divergence appears only in posterior rows

## See also

[`weighted_quantiles`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/weighted_quantiles.md),
[`calc_model_ess`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md)
