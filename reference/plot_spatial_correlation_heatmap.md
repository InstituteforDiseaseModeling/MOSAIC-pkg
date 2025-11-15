# Heat-map of a spatial–correlation matrix (ggplot2)

Draws a diverging-colour heat-map of the *L × L* correlation matrix
produced by
[`calc_spatial_correlation_matrix()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_spatial_correlation_matrix.md).

## Usage

``` r
plot_spatial_correlation_heatmap(C)
```

## Arguments

- C:

  Numeric matrix of correlations (values in \[-1, 1\]).

## Value

A **ggplot** object (rendered automatically in interactive sessions).
