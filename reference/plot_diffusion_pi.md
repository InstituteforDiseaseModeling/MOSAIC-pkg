# Plot the model-implied diffusion connectivity matrix (\\\pi\_{ij}\\)

Draws a heatmap of the normalized gravity connectivity matrix
\\\pi\_{ij}\\
([`calc_diffusion_matrix_pi`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_diffusion_matrix_pi.md)),
the probability that a traveler leaving origin \\i\\ arrives at
destination \\j\\. This is the \\\pi\_{ij}\\ panel of the `"spatial"`
figure group in
[`render_MOSAIC_figures`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/render_MOSAIC_figures.md).

## Usage

``` r
plot_diffusion_pi(pi, location_name = rownames(pi), latitude = NULL)
```

## Arguments

- pi:

  Numeric J x J connectivity matrix from
  [`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md)
  (element `$pi`), diagonal `NA`.

- location_name:

  Character vector of length J giving axis labels in the order of `pi`'s
  rows/columns. Defaults to `rownames(pi)`.

- latitude:

  Numeric vector of length J giving each location's latitude in the row
  order of `pi`; used to order the axes south-to-north (matching
  [`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md)).
  When `NULL` (default) the axes are left in the order of
  `location_name`.

## Value

Invisibly, the ggplot2 object.

## Details

The aesthetic matches the published \\\pi\\ panel of
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md)
exactly (`theme_bw()`, viridis "G" fill, bottom colorbar). Axes are
ordered south-to-north by latitude via a keyed `factor(levels = )` over
the named matrix (values follow their labels); no positional sort of
labels is performed.

This is the model-derived sibling of the observed-OAG \\\pi\\ panel in
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md);
the two visualize different quantities (modeled vs observed) and are
intentionally separate.

## See also

[`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md),
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md).
