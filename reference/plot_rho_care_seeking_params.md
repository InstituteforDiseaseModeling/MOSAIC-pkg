# Plot Rho (Care-Seeking Rate) Prior Estimation

Generates a two-panel figure summarising the empirical anchors (Wiens et
al. 2025 general diarrhea + severe-diarrhea/cholera strata) and the
fitted Beta prior for the rho parameter (care-seeking rate).

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

Panel A - dot-range plot of the two Wiens 2025 pooled estimates with
their 95% CIs. Random-effects pooling combines them as described in
[`get_rho_care_seeking_params`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_rho_care_seeking_params.md).

Panel B - fitted Beta density curve with the 2.5th, 50th, and 97.5th
percentiles annotated.
