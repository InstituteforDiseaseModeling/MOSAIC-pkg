# Estimate Case Fatality Rate using Hierarchical GAM with Time Series Splines

Fits a hierarchical generalized additive model (GAM) to estimate case
fatality rates (CFR) across African countries and time periods. The
model uses smooth splines for temporal trends with country-specific
random effects, handling missingness by borrowing strength across
countries and years. Results are saved to MODEL_INPUT directory for use
in MOSAIC simulations.

## Usage

``` r
est_CFR_hierarchical(
  PATHS,
  min_cases = 20,
  k_year = 12,
  include_country_trends = TRUE,
  population_weighted = FALSE,
  save_diagnostics = TRUE,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  List containing paths to data directories, typically from get_paths()

- min_cases:

  Integer, minimum number of cases required to include observation
  (default 20)

- k_year:

  Integer, number of basis functions for year spline (default 12)

- include_country_trends:

  Logical, whether to include country-specific temporal trends (default
  TRUE)

- population_weighted:

  Logical, whether to weight observations by population size from UN
  data (default FALSE)

- save_diagnostics:

  Logical, whether to save diagnostic plots (default TRUE)

- verbose:

  Logical, whether to print progress messages (default TRUE)

## Value

List containing:

- model: Fitted GAM model object

- predictions: Data frame with CFR estimates for all countries and years

- country_effects: Estimated country-specific deviations

- temporal_trend: Population-average temporal trend

- validation: Cross-validation results for recent years

- summary: Summary statistics and model fit metrics

## Details

The function fits a hierarchical GAM model with the following structure:

- Global temporal trend using thin-plate splines

- Country-specific random intercepts

- Optional country-specific smooth deviations from global trend

- Binomial likelihood with logit link for proper handling of rates

- Predictions extended to current year based on Sys.Date()

Model outputs include:

- Point estimates and 95% confidence intervals for all country-years

- Country-specific random effects quantifying systematic differences

- Smooth temporal trends showing CFR evolution over time

- Model diagnostics and validation metrics

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard usage
PATHS <- get_paths()
cfr_model <- est_CFR_hierarchical(PATHS)

# Custom settings for smoother trends
cfr_model <- est_CFR_hierarchical(
  PATHS,
  min_cases = 50,
  k_year = 8,
  include_country_trends = FALSE
)
} # }
```
