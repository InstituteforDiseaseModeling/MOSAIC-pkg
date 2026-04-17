# Plot Detailed Prior to Posterior Distributions

Creates detailed multi-panel visualizations showing the progression from
prior to posterior distributions through retained and best subsets, with
ESS and KL metrics.

## Usage

``` r
plot_model_posteriors_detail(
  quantiles_file,
  results_file,
  priors_file,
  posteriors_file = NULL,
  diagnostics_file = NULL,
  output_dir = "./results/plots",
  subset_col = "is_best_subset",
  weight_col = "weight_best",
  verbose = TRUE
)
```

## Arguments

- quantiles_file:

  Path to the posterior_quantiles.csv file from
  calc_model_posterior_quantiles

- results_file:

  Path to samples.parquet file containing all simulation results

- priors_file:

  Path to priors.json file

- posteriors_file:

  Path to posteriors.json file (optional, for theoretical distributions)

- diagnostics_file:

  Path to convergence_diagnostics.json file (optional, for ESS values)

- output_dir:

  Directory to save plots (default: "./results/plots")

- subset_col:

  Character name of the boolean subset-membership column used to select
  the "best subset" panel of the detailed posterior plot. Defaults to
  `"is_best_subset"`. Pass `"is_best_subset_opt"` to plot the
  optimizer-refined subset.

- weight_col:

  Character name of the per-row weight column paired with `subset_col`.
  Defaults to `"weight_best"`.

- verbose:

  Logical; print progress messages (default: TRUE)

## Value

List of plot filenames created (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
plot_model_posteriors(
  quantiles_file = "./results/posterior_quantiles.csv",
  results_file = "./results/samples.parquet",
  priors_file = "./priors.json",
  posteriors_file = "./results/posteriors.json",
  output_dir = "./results/plots"
)
} # }
```
