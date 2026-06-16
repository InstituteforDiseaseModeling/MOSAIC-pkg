# Deterministic Fit-Diagnostic Sandbox: One LASER Run with Parameter Overrides

Runs a **single deterministic** LASER simulation from a calibration
config (typically a run's medoid config) with optional point-value
parameter overrides, then scores the result against the observed series
carried in the config. This is the experiment unit behind the active
fit-diagnostic workflow (the `diagnose-fit` skill): ~1-2 seconds per
run, no calibration machinery, so a modeller (or the
`mosaic-calibration-doctor` agent) can test hypotheses about which
parameters drive a fit deficiency before committing to an expensive
recalibration.

It is country-agnostic — nothing is hard-coded to a specific location.
The observed data, dates, and locations are read from the supplied
config.

## Usage

``` r
run_fit_sandbox(
  config,
  params = list(),
  seed = 42L,
  locations = NULL,
  full_metrics = TRUE,
  outdir = NULL,
  run_label = "fit_sandbox",
  quiet = TRUE,
  .laser_runner = run_LASER
)
```

## Arguments

- config:

  A config as a named list, or a path to a config JSON (e.g.
  `.../2_calibration/best_model/config_medoid.json`).

- params:

  Named list of point-value parameter overrides applied to the config
  before the run (unknown names are skipped with a warning). Default
  [`list()`](https://rdrr.io/r/base/list.html).

- seed:

  Integer RNG seed for the LASER run. Default `42L`.

- locations:

  Integer indices of location rows to aggregate. Default `NULL` (all
  locations).

- full_metrics:

  Logical; if `TRUE` (default) compute the full
  [`calc_fit_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_fit_diagnostics.md)
  bias/shape/variance scorecard, else only top-line R\\^2\\/bias/CFR.

- outdir:

  Optional directory; if supplied, writes `predictions_ensemble.csv` and
  `metrics.json` under `outdir/<run_label>/`. Default `NULL` (return
  only).

- run_label:

  Character label for the run (used for the output subdirectory and
  recorded in metrics). Default `"fit_sandbox"`.

- quiet:

  Logical passed to
  [`run_LASER()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md).
  Default `TRUE`.

- .laser_runner:

  Function used to run the model; defaults to
  [`run_LASER()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md).
  Exposed as a seam for testing with a stubbed engine.

## Value

A named list with `predictions` (long data.frame in the standard
ensemble format), `metrics` (top-line metrics plus, when
`full_metrics=TRUE`, `fit_diagnostics` and a merged `scorecard`),
`params_applied` (data.frame of old/new values), and `run_label`.

## Details

Generalises the project-local `sensitivity_sandbox.R` pattern into the
package. Predicted and observed series are aggregated (summed) across
the selected `locations` to a single series before scoring, matching the
country-level diagnostic use case; pass a single index in `locations`
for a per-patch view. Full metrics are delegated to
[`calc_fit_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_fit_diagnostics.md).

## See also

[`calc_fit_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_fit_diagnostics.md),
[`run_LASER()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md)
