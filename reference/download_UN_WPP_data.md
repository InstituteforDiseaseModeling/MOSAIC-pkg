# Download UN World Population Prospects demographic indicators

Fetches the UN WPP "Demographic Indicators (Medium variant)" bulk CSV –
open, no credentials – and writes the three series MOSAIC consumes
(total population, crude birth rate, crude death rate) into
`MOSAIC-data/raw/demographics/` in the column layout
[`process_UN_demographics_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_UN_demographics_data.md)
reads.

## Usage

``` r
download_UN_WPP_data(
  PATHS,
  iso_codes = NULL,
  year_start = 1967L,
  revision = 2024L,
  url = NULL,
  snapshot_date = Sys.Date(),
  overwrite = FALSE,
  verbose = TRUE
)
```

## Source

UN DESA Population Division, World Population Prospects,
<https://population.un.org/wpp/>. Licence CC BY 3.0 IGO.

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).
  Must include `DATA_RAW`.

- iso_codes:

  ISO3 codes to keep. `NULL` (the default) resolves to
  [`MOSAIC::iso_codes_africa`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/iso_codes_africa.md)
  – **54** countries, all 40 MOSAIC countries included. Note the
  hand-downloaded exports carry **58** ISO codes, so this default
  silently drops MYT, REU, SHN and ESH (all are present in the WPP
  source; this is a scope choice, not a source limit). Pass
  `character(0)` for every country – see the scope warning under Output
  layout before doing so.

- year_start:

  Earliest year to keep. Default `1967`, matching the existing exports.

- revision:

  WPP revision year. Default `2024`. Bumping this is the single change
  needed when the UN publishes a new revision.

- url:

  Full override for the source URL. Built from `revision` when `NULL`.

- snapshot_date:

  Date stamp for the output filenames. Defaults to today.

- overwrite:

  If `FALSE` (default), skip when today's files exist.

- verbose:

  Print progress.

## Value

Invisibly, a `data.frame`: `measure`, `ok`, `n_rows`, `n_countries`,
`year_min`, `year_max`, `file`.

## Units (read before changing this)

WPP publishes `TPopulation1July` in **thousands**; the Data-Portal
exports previously kept in `raw/demographics/` are in **persons**. This
function multiplies population by 1000 so both vintages agree. Verified
against the existing export (MOZ 2020: bulk `30783.688` thousand -\>
`30783688` persons, matching the Data-Portal file exactly). `CBR` and
`CDR` are per-1,000 in both and are passed through unchanged (MOZ 2020:
38.736 and 8.007 in both). Getting this wrong is a silent 1000x error in
every population-scaled quantity in the model.

`Variant == "Medium"` is selected, matching the `"Median"`-labelled rows
in the Data-Portal exports (same series, different label).

## Output layout

Files are written as
`UN_world_population_prospects_<measure>_wpp<revision>_<date>.csv` with
columns `Iso3`, `Time`, `Value`, `IndicatorName`, `Variant`.
[`process_UN_demographics_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_UN_demographics_data.md)
selects the newest file per measure, so pre-existing hand-downloaded
exports remain valid and are simply outranked. Nothing is deleted.

## See also

[`process_UN_demographics_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_UN_demographics_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
download_UN_WPP_data(PATHS)
process_UN_demographics_data(PATHS)
} # }
```
