# Plot Posterior Predictive Checks for LASER Model

Creates professional posterior predictive check (PPC) plots to assess
model fit. Generates three separate PDF files with density overlays, Q-Q
plots, and residual analysis.

## Usage

``` r
plot_model_ppc(model, output_dir, verbose = TRUE)
```

## Arguments

- model:

  A laser-cholera Model object containing results and parameters

- output_dir:

  Directory where PPC plots will be saved

- verbose:

  Logical indicating whether to print progress messages (default: TRUE)

## Value

NULL (invisibly). Creates PDF files as side effects.

## Details

This function creates three professional-quality PPC plots:

- ppc_density.pdf - Density overlays and observed vs predicted scatter
  plots

- ppc_qqplots.pdf - Q-Q plots comparing observed and predicted
  distributions

- ppc_residuals.pdf - Residual analysis including temporal patterns
