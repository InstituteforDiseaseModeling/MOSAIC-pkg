# Plot Posterior Parameter Correlation Heatmap

Generates a heatmap of pairwise Spearman correlations between parameters
in the posterior (best subset), revealing trade-offs and redundancies.
Parameters are clustered hierarchically and only correlations exceeding
a threshold are labeled.

## Usage

``` r
plot_model_parameter_correlation(
  results_file,
  priors_file = NULL,
  output_dir = ".",
  cor_threshold = 0.3,
  max_params = 25,
  verbose = TRUE
)
```

## Arguments

- results_file:

  Path to samples.parquet file containing calibration results

- priors_file:

  Path to priors.json (used for parameter descriptions)

- output_dir:

  Directory to write the output figure

- cor_threshold:

  Minimum absolute correlation to display as text label. Default 0.3.

- max_params:

  Maximum number of parameters to display. The top `max_params`
  most-correlated parameters are selected. Default 25.

- verbose:

  Print progress messages

## Value

Invisible NULL. Writes a PNG file to `output_dir`.
