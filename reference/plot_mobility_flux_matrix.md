# Plot the model-implied daily mobility flux matrix (\\M\_{ij}\\)

Heatmap of the model's implied daily flux \\M\_{ij} = N_i \tau_i
\pi\_{ij}\\
([`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md)
element `$flux`) — the expected number of travelers per day from origin
\\i\\ to destination \\j\\. This is the modeled flight-matrix panel of
the `"spatial"` figure group in
[`render_MOSAIC_figures`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/render_MOSAIC_figures.md).

## Usage

``` r
plot_mobility_flux_matrix(
  flux,
  location_name = rownames(flux),
  latitude = NULL
)
```

## Arguments

- flux:

  Numeric J x J flux matrix from
  [`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md)
  (element `$flux`), diagonal `NA`.

- location_name:

  Character vector of length J, config order. Defaults to
  `rownames(flux)`.

- latitude:

  Numeric vector of length J giving each location's latitude in the row
  order of `flux`; used to order the axes south-to-north (matching
  [`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md)).
  When `NULL` (default) the axes are left in the order of
  `location_name`.

## Value

Invisibly, the ggplot2 object.

## Details

The aesthetic matches the observed flight-matrix (M) panel of
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md)
exactly (`theme_bw()`, black-\>skyblue `log(value + 1)` fill with the
`0/10/100/1,000/10,000` colorbar, bottom legend). Only the data and
colorbar title differ: this plots the model's *implied* flux from its
calibrated parameters, so the title reads "Model-implied mean daily
trips". Axes are ordered south-to-north by latitude via a keyed
`factor(levels = )` over the named matrix. The diagonal is `NA` (no
self-travel) and is rendered as `grey90`.

## See also

[`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md),
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md).
