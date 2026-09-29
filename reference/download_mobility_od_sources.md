# Download the bilateral mobility sources used to build the fused OD structure

Fetches the three external origin-destination sources that
[`process_mobility_od_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_mobility_od_data.md)
fuses into a connectivity structure for
[`est_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md):
UN DESA bilateral migrant stock, Abel & Cohen bilateral migration flows,
and the Meta (Facebook) Social Connectedness Index. The fourth source,
land contiguity, is derived locally from the ADM0 shapefiles and needs
no download.

## Usage

``` r
download_mobility_od_sources(
  PATHS,
  sources = c("desa", "abel_cohen", "sci"),
  snapshot_date = Sys.Date(),
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).
  Must include `DATA_RAW`.

- sources:

  Which to fetch. Default all three.

- snapshot_date:

  Date stamp for the archive subdirectory. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- overwrite:

  If `FALSE` (default), an existing snapshot is kept.

- verbose:

  Print progress.

## Value

Invisibly, a `data.frame`: `source`, `ok`, `bytes`, `file`, `note`.

## Details

All three are open; none needs credentials.

## Sources and provenance

- **UN DESA International Migrant Stock 2024**:

  Bilateral migrant *stock* (persons born in the origin residing in the
  destination), Table 1, 2024 both-sexes column. ~6 MB xlsx. The un.org
  host rejects a default `libcurl` agent with HTTP 403, so a browser
  User-Agent is sent. Licence: UN open data.

- **Abel & Cohen 2022**:

  Bilateral migration *flow* estimates (*Sci Data*), figshare article
  12845711, `bilat_mig_sex.csv` (~23 MB), ISO3 already. The
  `da_min_closed` estimator over the latest period is used downstream.
  Licence: CC BY.

- **Meta Social Connectedness Index**:

  HDX `social-connectedness-index`, resource **`country.csv`** (~0.6
  MB), columns `user_country`/`friend_country` (ISO2) and `scaled_sci`.
  Licence: CC BY-NC (Meta Data for Good) – see the note below.

**Deviation from the E3 prototype.** The original MOSAIC-OCV E3 work
used the GADM-1 resource (273 MB) and summed sub-region pairs up to
national totals. `country.csv` is Meta's own national-level product:
~500x smaller, no aggregation step, and a directly-measured national
index rather than a sum of relative sub-national indices. Because the
two are not guaranteed to be proportional, a fused structure built here
is close to but not byte-identical with the E3 artifacts.

**Licensing.** Meta SCI is **CC BY-NC**. That is more restrictive than
the other MOSAIC inputs and than the CC BY / UN terms of DESA and
Abel-Cohen. Set `sources` to exclude `"sci"` (and reweight in
[`process_mobility_od_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_mobility_od_data.md))
if a downstream use is commercial.

## See also

[`process_mobility_od_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_mobility_od_data.md),
[`est_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md)

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
download_mobility_od_sources(PATHS)
process_mobility_od_data(PATHS)
} # }
```
