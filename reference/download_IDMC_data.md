# Download IDMC Internal Displacement Updates (IDU) from the HDX mirrors

Downloads the per-country IDMC **Internal Displacement Updates** (IDU)
event CSVs published on the Humanitarian Data Exchange (HDX) and
archives a date-stamped snapshot into `PATHS$DATA_IDMC_RAW`, ready for
[`process_IDMC_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_IDMC_data.md).

## Usage

``` r
download_IDMC_data(
  PATHS,
  iso_codes = NULL,
  snapshot_date = Sys.Date(),
  overwrite = FALSE,
  verbose = TRUE
)
```

## Source

IDMC Internal Displacement Updates via HDX,
<https://data.humdata.org/dataset/>`<iso>-idmc-idu-events`. Licence: CC
BY-IGO. Cite IDMC.

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).
  Must include `DATA_IDMC_RAW`.

- iso_codes:

  Character vector of ISO3 codes to fetch. Defaults to
  [`MOSAIC::iso_codes_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/iso_codes_mosaic.md)
  (the MOSAIC-40).

- snapshot_date:

  Date stamp for the archive subdirectory. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- overwrite:

  If `FALSE` (default) and today's snapshot directory already exists
  with files in it, the download is skipped and the existing snapshot is
  reported. Set `TRUE` to re-download.

- verbose:

  If `TRUE` (default), print a per-country progress line and a coverage
  summary.

## Value

Invisibly, a data frame with one row per requested ISO code and columns
`iso_code`, `ok`, `n_events`, `date_min`, `date_max`, `file`, `note`.

## Details

**Why HDX and not the `idmc` R package.** The CRAN `idmc` package wraps
the IDMC `external-api` endpoint, which is credential gated: an
unauthenticated request to
`helix-tools-api.idmcdb.org/external-api/idus/all/` returns
`HTTP 403 "Client is not registered."`, and `idmc_get_data()` errors out
unless an IDMC-issued URL is present in the `IDMC_API` environment
variable. The HDX per-country mirrors carry the same IDU event records,
are CC BY-IGO, need no credentials, and are refreshed daily. They also
omit `standard_popup_text`, which `idmc_get_data()` unconditionally
parses – so HDX data cannot be routed through that package anyway.

**Resource discovery.** Download URLs embed CKAN resource UUIDs that
change when IDMC re-publishes, so they are resolved at call time from
the HDX CKAN API (`package_show?id=<iso>-idmc-idu-events`) rather than
hardcoded.

**Snapshot archiving (important).** IDU is a *rolling, provisional*
product: records age out and figures are revised. Snapshots are
therefore written to a dated subdirectory and never overwritten in
place, mirroring the EM-DAT and WHO-dashboard conventions elsewhere in
MOSAIC.
[`process_IDMC_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_IDMC_data.md)
reads whichever directory it is pointed at, so pass `source_dir` to
reprocess a historical snapshot.

**Coverage.** As of 2026-09-17, 38 of the 40 MOSAIC countries have an
HDX IDU dataset; **ERI and TGO have none**. Most series begin 2025-01-01
(KEN is a notable exception, reaching back to 2011). Zero cells before a
country's first observed event mean "not in this extract", NOT "no
displacement occurred" – see the coverage caveat in
[`?process_IDMC_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_IDMC_data.md)
before using these as a covariate.

## See also

[`process_IDMC_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_IDMC_data.md)
to build the country-week panels,
[`process_EMDAT_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_EMDAT_data.md)
for the sibling hazard panels.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
download_IDMC_data(PATHS)
process_IDMC_data(PATHS, source_dir = file.path(PATHS$DATA_IDMC_RAW,
                                                paste0("hdx_", Sys.Date())))
} # }
```
