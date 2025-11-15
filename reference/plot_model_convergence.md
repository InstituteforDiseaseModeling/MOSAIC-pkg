# Plot Model Convergence Diagnostics from Files

Creates a comprehensive diagnostic plot by reading convergence results
from files written by
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md).
Visualizes log-likelihood rankings with the best model highlighted and
displays convergence metrics.

## Usage

``` r
plot_model_convergence(results_dir, plots_dir = NULL, verbose = TRUE)
```

## Arguments

- results_dir:

  Character string specifying the directory containing
  convergence_results.parquet and convergence_diagnostics.json files.

- plots_dir:

  Character string specifying the directory where the plot should be
  saved. If NULL, defaults to ../plots/ relative to results_dir.

- verbose:

  Logical indicating whether to print messages. Default is TRUE.

## Value

Invisibly returns the ggplot object.

## Details

This function creates a publication-quality diagnostic plot showing:

- Ranked log-likelihood curve with the best model highlighted

- Convergence diagnostics displayed in the plot subtitle/caption

- Clear indication of model ensemble agreement metrics

The function reads from two files:

- `convergence_results.parquet` — Per-simulation data

- `convergence_diagnostics.json` — Aggregate metrics and descriptions

## Important Note

This visualizes **model-weight agreement** diagnostics, not MCMC
convergence. The "convergence" here refers to agreement across parameter
draws, not chain convergence. For MCMC diagnostics, use trace plots,
R-hat, etc.

## See also

[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md)

Other calibration-metrics:
[`calc_convergence_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_convergence_diagnostics.md),
[`calc_model_agreement_index()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_agreement_index.md),
[`calc_model_aic_delta()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_aic_delta.md),
[`calc_model_akaike_weights()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_akaike_weights.md),
[`calc_model_convergence()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_convergence.md),
[`calc_model_cvw()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_cvw.md),
[`calc_model_ess()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ess.md),
[`calc_model_max_weight()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_max_weight.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# First calculate convergence (writes files)
calc_model_convergence(
  PATHS = get_paths(),
  results = my_results,
  output_dir = "path/to/results"
)

# Then create plot from files
plot <- plot_model_convergence(
  results_dir = "path/to/results",
  plots_dir = "path/to/plots"
)
} # }
```
