# Download World Bank indicators from the Indicators API

Fetches each indicator from the open World Bank Indicators API (v2, no
credentials) and writes it into `MOSAIC-data/raw/world_bank/<subdir>/`
in the **same wide "bulk CSV" layout the web portal produces** – four
metadata lines, then
`Country Name, Country Code, Indicator Name, Indicator Code` followed by
one column per year. The `process_WB_*_data()` functions therefore read
an API pull and a hand-downloaded portal export identically.

## Usage

``` r
download_WB_data(
  PATHS,
  indicators = MOSAIC_WB_INDICATORS,
  snapshot_date = Sys.Date(),
  overwrite = FALSE,
  per_page = 20000L,
  verbose = TRUE
)
```

## Source

World Bank Indicators API,
<https://datahelpdesk.worldbank.org/knowledgebase/articles/889392>.
Licence CC BY 4.0.

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).
  Must include `DATA_RAW`.

- indicators:

  Named character vector mapping World Bank indicator codes to the
  `raw/world_bank/` subdirectory each belongs in. Defaults to the four
  MOSAIC consumes; see
  [`MOSAIC_WB_INDICATORS`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/MOSAIC_WB_INDICATORS.md).

- snapshot_date:

  Date stamp for the output filenames. Defaults to today.

- overwrite:

  If `FALSE` (default), an indicator whose file for `snapshot_date`
  already exists is skipped; `TRUE` replaces that dated file (use only
  to repair a bad snapshot).

- per_page:

  API page size (default 20000; the API caps near 32767).

- verbose:

  Print progress and a summary.

## Value

Invisibly, a `data.frame` with one row per indicator: `indicator`,
`subdir`, `ok`, `n_countries`, `year_min`, `year_max`, `file`, `note`.

## Details

**No credentials.** <https://api.worldbank.org/v2/> is open. This
retires four of the six manual sources reported by
[`check_mosaic_manual_inputs`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_mosaic_manual_inputs.md).

**Newest-wins, not overwrite.** Files are stamped
`API_<INDICATOR>_DS2_en_csv_v2_api_<date>.csv` and the
`process_WB_*_data()` functions select the newest match for their
indicator. Existing hand-downloaded portal exports are left in place and
still readable – an API pull simply outranks them by date. Nothing is
deleted. Each file is written atomically (temporary file, then rename)
and logged as one row in `raw/world_bank/PROVENANCE.md`.

**All countries are fetched**, not just the MOSAIC-40: the processors do
their own ISO filtering, and keeping the full panel means the raw file
stays reusable. Records the API returns with a blank `countryiso3code` –
the income-group aggregates (*High income*, *Low income*, *Lower/Upper
middle income*, *Not classified*) – are dropped, since they are not
countries and nothing downstream uses them.

**Switching to the API is NOT a no-op.** The World Bank revises history,
so a live pull differs from an older portal vintage. Measured 2026-09-17
against the 2025-04-15 GDP export: all 40 MOSAIC countries present in
both, 2025 gained as a new year, and of 2,331 shared MOSAIC-40
country-years **83.7% were identical** while 194 (8.3%) differed by
\>1%. Those differences are genuine national-accounts rebasing, not a
conversion error: 19 of 40 countries have NO year differing by more than
1% (3 are identical to the byte), and the rest differ in contiguous year
blocks with smooth country-specific ratios (MLI 1980-2023 ratio
1.13-1.54; AGO 2002-2023 ratio 1.12-1.25). Expect GDP-derived quantities
to move for ~21 countries on first use.

## See also

[`process_WB_GDP_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_WB_GDP_data.md),
[`process_WB_poverty_ratio_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_WB_poverty_ratio_data.md),
[`process_WB_population_density_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_WB_population_density_data.md),
[`process_WB_urban_population_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_WB_urban_population_data.md)

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
download_WB_data(PATHS)
process_WB_GDP_data(PATHS)
} # }
```
