# Process AI-mined Weekly Cholera Surveillance Data into the MOSAIC Per-Source Schema

Reads the per-country `cholera_weekly_<ISO>.csv` files produced by the
`ai-cholera-data-mining` repository, applies the "keep-and-weight"
ingestion filter, reformats the rows to match the WHO/JHU per-source
schema used by
[`process_cholera_surveillance_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_cholera_surveillance_data.md),
and carries the per-observation `confidence_weight` and
`disaggregation_method` through as two extra columns. The combined
output is written as a single processed CSV that the surveillance
combiner reads as a fourth source.

## Usage

``` r
process_AI_cholera_data(PATHS)
```

## Arguments

- PATHS:

  A list of file paths (typically from
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)).
  Must include:

  - **AI_CHOLERA_REPO**: Path to the `ai-cholera-data-mining` repo (its
    `data/<ISO>/cholera_weekly_<ISO>.csv` files are read).

  - **DATA_AI_WEEKLY**: Output directory for the combined processed CSV.

## Value

Invisibly returns the combined processed data frame. Side effect: writes
`DATA_AI_WEEKLY/cholera_country_weekly_processed.csv` with columns
`iso_code, country, year, week, cases, deaths, date_start, date_stop, month, confidence_weight, disaggregation_method`.

## Details

**Keep-and-weight filter.** The AI repo tags each row by
`disaggregation_method`. We keep:

- `"observed"` — direct weekly observations extracted from reports.

- `"fourier_*"` — historical annual/quarterly totals disaggregated to
  weekly via a seasonal template. These carry a lower
  `confidence_weight` (typically ~0.5) so downstream consumers can
  down-weight them rather than treat them as direct observations.

- `"documented_zero"` — confirmed-absence weeks. They flow through the
  merge and only fill genuine gaps because the surveillance combiner's
  priority de-duplication keeps higher-priority sources where they
  overlap.

We drop `"assumed_zero"` (default-zero assumptions with no evidentiary
basis) and any other / missing method tag.

The `source` column in the raw AI file tracks the agent's *collection
method*, not data origin, so it is intentionally not carried through;
[`process_cholera_surveillance_data()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_cholera_surveillance_data.md)
labels every row from this file as source `"AI"` for priority purposes.

CRLF line endings in the raw files leave a trailing carriage return on
the last column (`disaggregation_method`); it is stripped on read.

## See also

[`process_cholera_surveillance_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_cholera_surveillance_data.md)
