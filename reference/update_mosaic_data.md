# Refresh every MOSAIC data input that can be refreshed automatically

Single entry point for the data-preparation pipeline: syncs the external
scraper repos, re-runs every `download_*` / `process_*` / `est_*` step
in dependency order, and returns a per-step status table. Replaces
hand-running `model/LAUNCH.R`.

## Usage

``` r
update_mosaic_data(
  root = NULL,
  steps = NULL,
  skip = NULL,
  refresh_repos = TRUE,
  date_stop = Sys.Date() + 540,
  dry_run = FALSE,
  stop_on_error = FALSE,
  verbose = TRUE
)
```

## Arguments

- root:

  Path to the MOSAIC parent directory (the one holding `MOSAIC-pkg`,
  `MOSAIC-data`, and the scraper repos as siblings). Defaults to
  `get_paths()$ROOT` if a root has already been set.

- steps:

  Character vector of step ids or group ids (`"1A"`, `"2"`,
  `"process_OAG_data"`) to run. `NULL` (default) runs everything
  eligible. See
  [`list_mosaic_data_steps`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/list_mosaic_data_steps.md).

- skip:

  Character vector of step or group ids to exclude.

- refresh_repos:

  If `TRUE` (default), `git pull --ff-only` the five scraper repos first
  via
  [`refresh_data_repos`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/refresh_data_repos.md).

- date_stop:

  Upper date bound passed to
  [`est_vaccination_rate()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_vaccination_rate.md).
  Defaults to **today plus 540 days**, NOT today.

  This must cover the psi forecast horizon, because
  `data-raw/make_config_default.R` derives the config's `date_stop` from
  the per-country minimum of the psi prediction dates and then requires
  every time-varying matrix to span exactly that window. A
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html) default silently
  produced a vaccination matrix 139 days shorter than the psi horizon,
  and the config build failed validation with “nu_1_jt must be a matrix
  with ... columns equal to the daily sequence from date_start to
  date_stop”. Over-covering is harmless (rates are zero beyond the
  data); under-covering is fatal and the error names the wrong culprit.

- dry_run:

  If `TRUE`, print the preflight report and the execution plan, then
  stop without running anything or touching any file.

- stop_on_error:

  If `TRUE`, abort at the first failure instead of continuing. `FALSE`
  by default.

- verbose:

  Print progress and the closing summary.

## Value

Invisibly, a `data.frame` with one row per step: `step`, `group`,
`status`, `seconds`, `message`. `status` is one of `"ok"`, `"failed"`,
`"blocked"` (a dependency failed), `"not_run"` (aborted before this step
under `stop_on_error`) or `"pending"` (returned by `dry_run`, where
nothing executes). Attributes `"manual_inputs"` and `"repo_sync"` carry
the preflight frame and any sync warning.

## Details

Unlike `LAUNCH.R`, a failing step does **not** abort the run: it is
recorded, its dependents are marked `blocked`, and the remaining
independent steps still execute. One run therefore surfaces every
problem at once instead of one per invocation.

## Scope — data building, not model fitting

This function builds **data**. It does not fit models. Group 4A
(`compile_suitability_data`) assembles the LSTM training panel from its
13 upstream producers — climate, ENSO, demographics, surveillance,
mobility, epidemic peaks, EM-DAT, the four World Bank indicators, WASH
and elevation — and is in the default plan, so the suitability *data*
stays in step with its inputs on an ordinary run.

Fitting the suitability model is a separate concern and is
**deliberately not reachable from here**. Call
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
directly, or use the calibration workflow: it needs the TensorFlow/keras
Python environment, budgets ~6 GB per seed worker, and runs for hours,
so it belongs on its own schedule with its own failure handling.
`est_suitability` was a registry step (group 4B) up to v0.91.13 and was
removed in v0.91.14.

## Manual inputs

Some sources have no automated route and must be refreshed by hand.
Every run begins with a preflight that checks each one and prints its
age and refresh instructions; `dry_run = TRUE` prints the preflight
alone. See
[`check_mosaic_manual_inputs`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_mosaic_manual_inputs.md)
for the manifest. Nothing here blocks the run – a stale manual input
degrades the relevant outputs, it does not stop the pipeline.

## What this does NOT do

- **Package data objects.** Rebuilding `priors_default` /
  `config_default` requires `devtools::install(".")` *between*
  `data-raw/make_priors_default.R` and `data-raw/make_config_default.R`,
  so it cannot run in one session. The summary flags when a rebuild
  looks warranted.

- **Plots.** Visualisation is not a data step; use the `plot_*`
  functions directly.

- **Calibration.** See
  [`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).

## See also

[`list_mosaic_data_steps`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/list_mosaic_data_steps.md),
[`check_mosaic_manual_inputs`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/check_mosaic_manual_inputs.md),
[`refresh_data_repos`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/refresh_data_repos.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# What would run, and what needs manual attention?
update_mosaic_data("~/MOSAIC", dry_run = TRUE)

# Full automated refresh (no suitability)
res <- update_mosaic_data("~/MOSAIC")
subset(res, status != "ok")

# Resume after fixing a failure
update_mosaic_data("~/MOSAIC", steps = c("3A", "3D", "3E"), refresh_repos = FALSE)
} # }
```
