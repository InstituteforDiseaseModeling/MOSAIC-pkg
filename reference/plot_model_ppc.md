# Plot Posterior Predictive Checks from Ensemble Predictions

Creates professional posterior predictive check (PPC) plots to assess
model fit using CSV files from stochastic/ensemble predictions.
Generates diagnostic plots across six pages: density overlays, credible
interval coverage, calibration scatter plots, Q-Q plots, residuals vs
observed, and temporal residual patterns.

## Usage

``` r
plot_model_ppc(
  predictions_dir = NULL,
  predictions_files = NULL,
  locations = NULL,
  model = NULL,
  output_dir,
  verbose = TRUE
)
```

## Arguments

- predictions_dir:

  Character string specifying directory containing prediction CSV files.
  Function will auto-discover predictions_ensemble\_*.csv or
  predictions_stochastic\_*.csv files.

- predictions_files:

  Character vector of specific CSV file paths to use. Overrides
  predictions_dir if provided.

- locations:

  Character vector of specific locations to plot. NULL (default) uses
  all locations.

- model:

  Legacy: A laser-cholera Model object (for backward compatibility). Not
  recommended - use CSV-based inputs instead.

- output_dir:

  Directory where PPC plots will be saved. Creates "ppc" subdirectory.

- verbose:

  Logical indicating whether to print progress messages (default: TRUE)

## Value

NULL (invisibly). Creates PDF files as side effects.

## Details

Creates a 6-page multi-page PDF per output file:

1.  Density overlays of observed vs predicted median distributions

2.  Credible interval coverage analysis (50\\

3.  Observed vs predicted calibration scatter plots

4.  Quantile-quantile plots

5.  Residuals vs observed

6.  Temporal residual patterns

Output files:

- ppc.pdf - Aggregate diagnostics (all locations combined)

- ppc_ISO.pdf - Per-location diagnostics (e.g., ppc_ETH.pdf,
  ppc_KEN.pdf)

## Examples

``` r
if (FALSE) { # \dontrun{
# From predictions directory (auto-discovers CSV files)
plot_model_ppc(
  predictions_dir = "output/3_results/figures/predictions",
  output_dir = "output/3_results/figures/predictions"
)

# Specific locations only
plot_model_ppc(
  predictions_dir = "output/plots/predictions",
  output_dir = "output/plots",
  locations = c("ETH", "KEN")
)
} # }
```
