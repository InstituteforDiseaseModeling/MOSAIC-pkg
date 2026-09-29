# Publication figures for the fused overland OD connectivity model

Four figures that together tell the fused-OD story: how the sources
combine, what that changes structurally, what it changes in amplitude,
and what it looks like geographically.

## Usage

``` r
plot_mobility_fused(
  PATHS,
  suffix = "_fused_raked_tt",
  out_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- suffix:

  Which
  [`est_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md)
  run to read for the fused quantities. Default `"_fused_raked_tt"`.

- out_dir:

  Directory for the PNGs. Default `PATHS$DOCS_FIGURES`.

- verbose:

  Print each file written.

## Value

Invisibly, a character vector of the files written.

## Figures

- `fused_od_1_sources.png`:

  Small multiples of the four row-normalised source matrices and the
  fused result, shared sequential scale. Shows what each source
  contributes.

- `fused_od_2_landneighbour_share.png`:

  Per-country share of outflow reaching a land neighbour, air vs fused.
  The single scalar that captures what the method fixes.

- `fused_od_3_tau_prior.png`:

  Daily departure probability, air fit vs overland prior with 95%
  intervals, log scale.

- `fused_od_4_corridors.png`:

  Dominant destination per origin drawn on the map, air vs fused.

## Design

Colour follows the data's job: a single-hue sequential ramp for
magnitude (heatmaps), and two categorical hues for the air-vs-fused
contrast. The categorical pair was validated for colour-vision
deficiency (worst-pair protan dE 24.7, normal-vision dE 33.6, both well
clear of the 8 / 15 floors), and every two-series panel also carries
position or direct labels so identity never rests on colour alone.
Countries are ordered by latitude in the heatmaps, matching
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md),
so the diagonal band is geographic adjacency.
