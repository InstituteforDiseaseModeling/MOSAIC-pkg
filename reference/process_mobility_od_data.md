# Fuse four bilateral mobility sources into a connectivity structure

Builds a row-normalised origin-destination *structure* matrix by
ensemble-averaging four sources: UN DESA bilateral migrant stock, Abel &
Cohen bilateral flows, the Meta Social Connectedness Index, and land
contiguity. The result is a unit-free topology – who connects to whom
and how strongly, relative to each origin's other destinations –
consumed by `est_mobility(od_source = "fused")`.

## Usage

``` r
process_mobility_od_data(
  PATHS,
  iso_codes = NULL,
  weights = c(desa = 0.3, abel_cohen = 0.2, sci = 0.35, contiguity = 0.15),
  snapshot_dir = NULL,
  flow_year_min = 2015L,
  sci_denormalise = TRUE,
  flow_estimator = c("da_pb_closed", "da_min_closed", "da_min_open", "da_pb_open"),
  verbose = TRUE
)
```

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- iso_codes:

  ISO3 codes to include. Defaults to
  [`MOSAIC::iso_codes_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/iso_codes_mosaic.md).

- weights:

  Named numeric ensemble weights. Default
  `c(desa = 0.30, abel_cohen = 0.20, sci = 0.35, contiguity = 0.15)`,
  the E3 design values. Rescaled to sum to 1 over the sources actually
  available.

- snapshot_dir:

  Directory of raw sources. `NULL` (default) selects the newest
  `snapshot_<date>/` written by
  [`download_mobility_od_sources`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/download_mobility_od_sources.md).

- flow_year_min:

  Earliest Abel-Cohen `year0` period to include. Default 2015 (the
  2015-2020 period used by E3).

- sci_denormalise:

  Multiply the Meta SCI matrix by destination population before
  row-normalising. `TRUE` by default and strongly recommended:
  `scaled_sci` divides destination size out by construction, so leaving
  it normalised contributes a destination-size exponent of ~0 to the
  gravity fit and – at weight 0.35 – drags the fused `omega` well below
  the value the volume sources agree on.

- flow_estimator:

  Which Abel & Cohen estimator column to use. Default `"da_pb_closed"`,
  the **pseudo-Bayesian** estimate the authors recommend (*Sci Data*
  2022: it “performs consistently better than the other estimation
  methods”). The E3 prototype used `"da_min_closed"`, a *minimum* flow
  consistent with the stock change rather than a per-pair estimate; on a
  10-country West-African set every origin was populated, but across the
  MOSAIC-40 it leaves **AGO, GNQ and SOM with no intra-set mass at all**
  and reduces Namibia to a single 0.22-person cell, which
  row-normalisation then promotes to weight 1.0.

- verbose:

  Print per-source coverage.

## Value

Invisibly, a list with `M` (the fused row-normalised matrix),
`components` (the per-source row-normalised matrices), and `weights` (as
actually applied). Writes `processed/mobility/M_structure_fused.csv`
plus one CSV per component.

## Why these weights

DESA (stock) and Abel-Cohen (flow) are **not independent** – the flow
estimates are derived from successive stock tables – so they share a
single 0.5 migration block rather than getting 0.35 each. SCI carries
0.35 as a destination-affinity kernel that is independent of the
migration data. Contiguity gets 0.15 as a weak structural prior that a
shared land border implies movement even where the other sources are
sparse.

## Direction convention

All matrices are **origin (row) -\> destination (column)** and each row
sums to 1. DESA's Table 1 is published destination-major (its column 1
is the destination, column 6 the origin), so it is transposed on read;
verified against the known Burkina Faso -\> Cote d'Ivoire corridor
(1,820,882 persons in the 2024 column). Getting this backwards silently
transposes the entire connectivity structure.

## What this is NOT

A row-normalised structure carries **no amplitude**. It says nothing
about how many people move, only where they go given that they move.
DESA is a decades-accumulated *stock*; dividing it by population does
not yield a weekly departure rate (the E3 work measured a ~700x
inflation from exactly that mistake). Departure amplitude comes from
`fit_prob_travel()` on real flow data, never from this matrix.

## See also

[`download_mobility_od_sources`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/download_mobility_od_sources.md),
[`est_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md)
