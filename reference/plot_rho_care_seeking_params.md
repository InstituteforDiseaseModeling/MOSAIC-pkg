# Plot Rho (Care-Seeking Rate) Prior Estimation

Generates a two-panel figure summarising the empirical data and fitted
Beta prior for the rho parameter (care-seeking rate).

## Usage

``` r
plot_rho_care_seeking_params(PATHS)
```

## Arguments

- PATHS:

  A list of paths from
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).
  Must include:

  - **MODEL_INPUT**: directory containing `param_rho_care_seeking.csv`.

  - **DOCS_FIGURES**: directory where the PNG will be saved.

## Details

Panel A — dot-range plot of all per-stratum GEMS estimates and the Wiens
et al. 2025 meta-analytic severe-diarrhea estimate, with points sized by
relative inverse-variance weight.

Panel B — fitted Beta density curve with the 5th, 50th, and 95th
percentiles annotated.
