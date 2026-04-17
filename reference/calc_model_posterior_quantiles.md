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
  priors = NULL,
  subset_col = "is_best_subset",
  weight_col = "weight_best",
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

- priors:

  Optional priors list (as returned by `get_location_priors`). When
  provided, prior distribution families are read from this object
  instead of the static `estimated_parameters` lookup. This ensures
  posterior fitting uses the actual prior family, which is critical for
  staged estimation with country-specific priors. When NULL (default),
  falls back to the static lookup for backward compatibility.

- subset_col:

  Character name of the boolean subset-membership column to use when
  computing posterior quantiles. Defaults to `"is_best_subset"`
  (tier-selected subset). Pass `"is_best_subset_opt"` to drive the
  posterior from the optimizer-refined subset.

- weight_col:

  Character name of the per-row weight column paired with `subset_col`.
  Defaults to `"weight_best"`.

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
