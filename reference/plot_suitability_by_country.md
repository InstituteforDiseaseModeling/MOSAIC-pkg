# Plot Predicted Suitability for All Countries

This function generates a facet plot of predicted environmental
suitability for all countries over time. Requires that est_suitability()
has been run first to generate the necessary prediction files.

## Usage

``` r
plot_suitability_by_country(PATHS)
```

## Arguments

- PATHS:

  A list of paths to the directories where data is stored and plots will
  be saved. Must include MODEL_INPUT (for reading prediction files) and
  DOCS_FIGURES (for saving plots).

## Value

A facet plot of predicted environmental suitability for all countries,
filtered to years \>= 2021. The plot includes suitability predictions
with separate colors for historical (black) and future (orange)
predictions.

## Details

The function reads pred_psi_suitability_week.csv created by
est_suitability(), which contains the columns: iso_code, year, week,
date, cases, cases_binary, and pred.
