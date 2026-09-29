# Build an overland least-cost travel-time matrix between countries

Downloads (and caches) the Malaria Atlas Project motorized friction
surface and computes least-cost accumulated travel time, in **hours**,
between country centroids. This is the geometry layer for
`est_mobility(distance_metric = "travel_time")` – an overland effective
distance that replaces great-circle kilometres.

## Usage

``` r
get_travel_time_matrix(
  PATHS,
  iso_codes = NULL,
  aggregate_factor = 6L,
  aggregate_fun = c("min", "mean"),
  dataset_id = "Accessibility__202001_Global_Motorized_Friction_Surface",
  cache = TRUE,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- iso_codes:

  ISO3 codes. Defaults to
  [`MOSAIC::iso_codes_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/iso_codes_mosaic.md).

- aggregate_factor:

  Integer cell aggregation applied to the ~1 km friction raster before
  building the transition graph. Default `6` (~5 km). See "Resolution"
  below – this is the single knob trading accuracy against memory.

- aggregate_fun:

  How to combine friction within an aggregated block. **`"min"`
  (default) is the correct choice** and `"mean"` is offered only for
  comparison. Roads are thin linear low-friction features; averaging a
  block smears them into the surrounding landscape and destroys the
  network. Measured on a West-African tile, `fact = 6` gives a median
  friction of 0.00120 min/m under `min` versus 0.00914 under `mean` – a
  ~7.6x difference that propagates straight into travel time. Using
  `mean` produced a 369 h median country-to-country time and NGA-NER at
  181 h, against ~56 h from the E3 reference build.

- dataset_id:

  MAP raster id. Default
  `"Accessibility__202001_Global_Motorized_Friction_Surface"` (the 2019
  v5.1 surface, published 2020-01).

- cache:

  If `TRUE` (default), reuse a previously built matrix and the cached
  friction raster instead of refetching/recomputing.

- verbose:

  Print progress.

## Value

Invisibly, a square numeric matrix of travel time in HOURS, dimnames =
ISO3, diagonal 0. Also written to
`processed/mobility/D_traveltime_hours.csv`.

## Method

Mirrors the MOSAIC-OCV E3 Route-2b recipe: aggregate the friction
surface, `gdistance::transition(1/mean(x), directions = 8)` to get
conductance, `geoCorrection(type = "c")` to scale conductance by true
inter-cell distance in metres, then `costDistance` between centroids.
Friction is in **minutes per metre**, so accumulated cost is in minutes;
divided by 60 for hours.

## Resolution (read before changing `aggregate_factor`)

E3 used `aggregate_factor = 3` (~2.5 km) but ran on **10-country
regional** extents (~960x1360 cells). A continental Africa extent at the
same factor is ~2800x2920 = 8.2M cells, and `gdistance` builds a sparse
transition matrix with 8 neighbours per cell – roughly 65M non-zeros,
which will exhaust memory on most hosts. The default `6` (~5 km, ~2.0M
cells) is the continental-scale compromise. Country-to-country distances
here are hundreds of km, so 5 km cells are adequate for national
centroids; drop to 3 only for a regional subset.

## Deviation from E3

E3 used **population-weighted** centroids (WorldPop). This uses the
geometric centroids MOSAIC already computes via
[`get_centroid`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_centroid.md),
the same ones the great-circle path uses – so air and travel-time
distances are strictly comparable. For large countries with off-centre
populations (e.g. COD, TCD) a population-weighted centroid would shift
the origin point meaningfully; that is a known, unimplemented
refinement.

## See also

[`est_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md),
[`process_mobility_od_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_mobility_od_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
D_hours <- get_travel_time_matrix(PATHS)
est_mobility(PATHS, od_source = "fused", distance_metric = "travel_time")
} # }
```
