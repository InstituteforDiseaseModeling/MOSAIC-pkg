# Write ensemble trajectory channels to per-location CSV

Exports the ensemble central daily trajectory of every captured channel
(the artifact's `$summary[[channel]]$median` field, which is not always
a median; see "What is lost") to a plain-text CSV, one file per
location, as `trajectories_<LOC>.csv`.

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

## Deaths and the population balance

When the run integrated the reported CFR out (MOSAIC \>= v0.96.0),
`reported_deaths` and `disease_deaths` are the members' deaths redrawn
from the calibrated CFR. The population compartments and `N` carry the
engine's own fatal draws at the prior `mu_jt` (the fatal share `p_fatal`
of symptomatic onsets, a few percent, never enters `Isym`), so `N`
balances against those, not against the redrawn `disease_deaths`.
`disease_deaths` are true deaths, i.e. reported deaths / `rho_deaths`,
and rest on the pinned `rho_deaths`.

## What is lost

The summary carries one central line per channel and no credible
intervals. The central line is not the same statistic for every column:

- `reported_cases` and `reported_deaths` follow the run's per-channel
  `central_method` (default weighted **median** for cases, weighted
  **mean** for deaths) over the engine-level member trajectories,
  matching `predicted_central` in the prediction CSVs; `disease_deaths`
  follows the deaths `central_method`.

- `mass_balance` is a ratio of the compartments' weighted means, `CFR` a
  ratio of 28-day rolling sums of the weighted-mean reported deaths and
  cases, and `epidemic_frac` the weighted mean of the reconstructed
  epidemic flag.

- `I_total` is the sum of the `Isym` and `Iasym` medians.

- Every other channel is the weighted **median**.

Intervals for these channels require the per-member data.
`reported_cases` and `reported_deaths` keep their intervals in the
prediction CSVs.

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
