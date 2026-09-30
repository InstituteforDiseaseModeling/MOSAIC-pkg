# Calculate Parameter-Specific ESS

Computes the effective sample size (ESS) for individual parameters using
one of two methods: KDE-based marginal posterior estimation (default,
`marginal_method = "kde"`, also the
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
control default `ESS_marginal_method`) or binned marginal ESS.

## Usage

``` r
calc_model_ess_parameter(
  results,
  param_names,
  likelihood_col = "likelihood",
  n_bins = 100,
  n_grid = 100,
  method = c("kish", "perplexity"),
  marginal_method = c("kde", "binned"),
  verbose = FALSE
)
```

## Arguments

- results:

  Data frame containing simulation results

- param_names:

  Character vector of parameter names to analyze (required)

- likelihood_col:

  Character name of the column containing log-likelihood values
  (default: "likelihood")

- n_bins:

  Integer number of bins for the binned method, or NULL for adaptive
  sqrt(n) scaling. Fixed bin count (e.g. 100) removes sample-size
  dependence from ESS estimates. Default: 100. Only used by "binned"
  method.

- n_grid:

  Integer number of grid points for KDE evaluation (default: 100, used
  only by "kde" method)

- method:

  Character string specifying ESS formula: "kish" or "perplexity"

- marginal_method:

  Character string specifying how marginal weights are constructed:
  "kde" (default, KDE-based marginal posterior estimation) or "binned"
  (more conservative, directly sensitive to importance weight
  distribution — recommended for final production runs).

- verbose:

  Logical whether to print progress messages (default: FALSE)

## Value

Data frame with columns:

- parameter: Parameter name

- type: Parameter type ('global' or 'location')

- iso_code: ISO code for location parameters (NA for global)

- ess_marginal: Marginal ESS for this parameter

## Details

Both methods start from the raw importance weights \\w_i \propto
\exp(\ell_i - \max \ell)\\. The `"kde"` method estimates each
parameter's weighted marginal density on a grid over the range of its
draws (bandwidth from Silverman's rule on the unweighted draws), divides
it by a uniform density on that range, applies the configured ESS
formula to the normalised grid ratios and rescales from grid points to
draws (`n / n_grid`). The `"binned"` method applies the ESS formula to
the weight totals of `n_bins` equal-width bins and rescales by
`n / n_occupied`.

**Limitation.** Neither estimate is bounded by the ESS of the importance
weights themselves. Because the KDE bandwidth does not shrink with the
weights and both results are rescaled by the number of draws, a weight
vector that has collapsed onto a single draw (exact IS ESS = 1) still
yields a marginal ESS that grows with `n` (for `"kde"` roughly `n/5`
under a uniform prior and `n/25` under a lognormal one; for `"binned"`
about `n` over the number of occupied bins), and for a non-uniform prior
the uniform reference mixes the prior's shape into the result. Read
these values alongside the exact importance-sampling diagnostics of
[`calc_is_diagnostics`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_is_diagnostics.md),
which do detect the collapse.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with simulation results
ess_results <- calc_model_ess_parameter(
  results = simulation_results,
  param_names = c("tau_i", "beta_j0_tot", "gamma_2")
)

# With custom likelihood column name
ess_results <- calc_model_ess_parameter(
  results = simulation_results,
  param_names = c("tau_i", "beta_j0_tot", "gamma_2"),
  likelihood_col = "log_lik"
)

} # }
```
