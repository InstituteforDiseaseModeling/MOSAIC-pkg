# Plot Predicted Suitability and Reported Cholera Cases for a Specific Country

This function plots the reported cholera cases and predicted
environmental suitability for a specific country over time, with shaded
regions indicating cholera outbreaks. Requires that est_suitability()
has been run first to generate prediction files.

## Usage

``` r
plot_suitability_and_cases(PATHS, plot_iso_code)
```

## Arguments

- PATHS:

  A list of paths to the directories where data is stored and plots will
  be saved. Must include MODEL_INPUT (for reading prediction files) and
  DOCS_FIGURES (for saving plots).

- plot_iso_code:

  The ISO code of the country to plot (e.g., "AGO", "CMR").

## Value

A combined plot showing both cholera case bars and smoothed suitability
predictions for the specified country.

## Details

The function reads pred_psi_suitability_day.csv created by
est_suitability(), which contains: date, cases, pred, pred_smooth, and
iso_code columns.
