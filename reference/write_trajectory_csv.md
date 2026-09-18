# Write ensemble trajectory channels to per-location CSV

Exports the ensemble-median daily trajectory of every captured channel
to a plain-text CSV, one file per location, as `trajectories_<LOC>.csv`.

## Usage

``` r
write_trajectory_csv(
  trajectories,
  dir_out,
  channels = NULL,
  digits = 6L,
  verbose = TRUE
)
```

## Arguments

- trajectories:

  A `mosaic_trajectories` object, or a path to a
  `trajectories_ensemble.rds` file.

- dir_out:

  Directory to write into; created if absent.

- channels:

  Optional character vector selecting channels. Default `NULL` writes
  every channel present.

- digits:

  Significant figures to round values to. Default 6.

- verbose:

  Logical; print one message per file written.

## Value

Invisibly, the character vector of written file paths.

## Why this exists

[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
reduces the per-member channel arrays into a `mosaic_trajectories`
object and persists it to `2_calibration/trajectories_ensemble.rds`.
That file is an R binary, so the MOSAIC-results promoter classifies it
as heavy and records it `store:"pending"` — declared in the manifest but
never committed. The consequence is that only `reported_cases` and
`reported_deaths` (via the prediction CSVs) reach the archive, while
`incidence`, `new_symptomatic`, `disease_deaths`, the compartments and
the derived channels do not, and are unreadable outside R even when
present.

A text CSV is copied into git by the existing promoter with no schema or
promoter change.

## Format and size

WIDE, not long: one row per `(location, date)` with one column per
channel. A long table repeats `location` and `date` once per channel,
which for 24 channels measured 2.72 MB against 582 KB wide on a national
model (4.7x), and 410 KB against 202 KB after gzip.

Values are rounded to `digits` significant figures. At the default 6
this measured 582 KB per national model (202 KB packed) against 970 KB
(373 KB packed) at full precision — model output does not carry 15
significant figures of information.

The per-member `$lines` component is deliberately NOT exported: at 6
significant figures it measured 62.7 MB raw / 7.8 MB packed for a single
national model, which belongs in blob storage rather than git.

## What is lost

The summary carries the weighted **median** only, so these are central
trajectories with no credible intervals. Intervals for these channels
require the per-member data. `reported_cases` and `reported_deaths` keep
their intervals in the prediction CSVs.

## Examples

``` r
if (FALSE) { # \dontrun{
# From a run directory, or to backfill an already-promoted model:
write_trajectory_csv(
  file.path(dir_output, "2_calibration", "trajectories_ensemble.rds"),
  file.path(dir_output, "3_results", "predictions")
)
} # }
```
