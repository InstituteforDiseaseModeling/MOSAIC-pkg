# Plot the model-implied mobility flux network

Network map of the model's implied daily mobility flux \\M\_{ij} = N_i
\tau_i \pi\_{ij}\\
([`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md)):
nodes are placed at each location's centroid, and edges connect
origin-destination pairs colored and sized by flux. This is the network
panel of the `"spatial"` figure group in
[`render_MOSAIC_figures`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/render_MOSAIC_figures.md).

## Usage

``` r
plot_mobility_flux_network(
  flux,
  coords,
  location_name = rownames(flux),
  basemap = NULL
)
```

## Arguments

- flux:

  Numeric J x J flux matrix from
  [`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md)
  (element `$flux`), diagonal `NA`.

- coords:

  J x 2 numeric matrix of `longitude`, `latitude` in the same row order
  as `flux` (element `$coords` of
  [`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md)).

- location_name:

  Character vector of length J, config order. Defaults to
  `rownames(flux)`.

- basemap:

  Optional sf object drawn as a polygon backdrop. When `NULL` (default),
  the network is drawn without a continental backdrop.

## Value

Invisibly, the combined legend+map grob
([`gridExtra::grid.arrange`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)
output), or the single ggplot2 object when gridExtra is unavailable.

## Details

The aesthetic matches the mobility-network panel of
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md)
exactly (`theme_void()`, grey Africa backdrop, viridis "B" edge color on
`log(count + 1)` with the `0/10/100/1,000/10,000` colorbar, white
`shape = 21` nodes, `ggrepel` ISO3 labels, and the extract-legend +
`grid.arrange` recombination). Only the data and colorbar title differ:
this plots the model's *implied* flux (title "Model-implied mean daily
trips") rather than the *observed* OAG flights, and shows **all** edges
above a small threshold (`count > 1`) rather than a top-fraction subset.

When a `basemap` (an sf polygon layer) is supplied it is drawn as a
backdrop in full, so neighboring countries appear as reference outlines,
and the view is then cropped (via
[`coord_sf`](https://ggplot2.tidyverse.org/reference/ggsf.html)) to the
bounding box of the network countries' polygons (those whose `iso3`
matches `location_name`), expanded by a small margin. The renderer
passes the packaged low-resolution Africa ADM0 layer
(`inst/extdata/africa_adm0_lowres.geojson`) — it never calls a
GeoBoundaries /
[`get_country_shp`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_country_shp.md)
API (P5). When `basemap` is `NULL` (subnational / no ISO match) the view
falls back to the node bounding box.

## See also

[`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md),
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md).
