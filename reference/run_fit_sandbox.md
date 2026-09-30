# Deterministic Fit-Diagnostic Sandbox: One Simulation with Parameter Overrides

Runs a **single deterministic** simulation from a calibration config
(typically a run's medoid config) with optional point-value parameter
overrides, then scores the result against the observed series carried in
the config. This is the experiment unit behind the active fit-diagnostic
workflow (the `diagnose-fit` skill): ~1-2 seconds per run, no
calibration machinery, so a modeller (or the `mosaic-calibration-doctor`
agent) can test hypotheses about which parameters drive a fit deficiency
before committing to an expensive recalibration.

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
  .sim_runner = run_simulation
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

  Integer RNG seed for the simulation run. Default `42L`.

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
  [`run_simulation()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_simulation.md).
  Default `TRUE`.

- .sim_runner:

  Function used to run the model; defaults to
  [`run_simulation()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_simulation.md).
  Exposed as a seam for testing with a stubbed engine.

## Value

A named list with `predictions` (long data.frame in the standard
ensemble format, plus `n_locations_observed`), `metrics` (top-line
metrics, including the 1-based `score_idx_cases`/`score_idx_deaths`
scored-window starts, plus, when `full_metrics=TRUE`, `fit_diagnostics`
and a merged `scorecard`), `params_applied` (data.frame of old/new
values), and `run_label`.

## Details

Generalises the project-local `sensitivity_sandbox.R` pattern into the
package. Predicted and observed series are aggregated (summed) across
the selected `locations` to a single series before scoring, matching the
country-level diagnostic use case; pass a single index in `locations`
for a per-patch view. Full metrics are delegated to
[`calc_fit_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_fit_diagnostics.md).

Scoring is paired and windowed the way
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
scores a fit. For each day, the scored predicted total sums only the
location-days that carry an observation, so a location with no
surveillance contributes to neither side (a day with no observation at
any selected location is `NA`, never 0). The leading unscored steps –
the default likelihood scored window
([`mosaic_control_defaults()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md)
`likelihood`: `burn_in_days`, 30 days) and, for cases, the two-step
initial-condition warm-up – are dropped from both series before any
metric is computed. A run calibrated with a non-default
`burn_in_days`/`score_start_cases`/`deaths_score_start` is still scored
here on the default window. The returned `predictions` use the same
pairing but are not windowed: on a day where at least one selected
location is observed, `observed` and `predicted_*` are both summed over
exactly those observed locations (`n_locations_observed` records how
many), so the two columns are always comparable; on a day with no
observation at any selected location, `observed` is `NA` and
`predicted_*` is the full aggregate over all selected locations. For a
single location this is simply its own observed and predicted series.

On a config that predates the v0.96.0 mortality model (it carries any of
`mu_j_baseline`, `mu_j_epidemic_factor`, `CFR_target`, `mu_j`), the
engine uses `CFR_target` as a constant reported CFR and ignores `mu_jt`,
so on such a config a `CFR_target` override is applied and a `mu_jt`
override is skipped with a warning.

## See also

[`calc_fit_diagnostics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_fit_diagnostics.md),
[`run_simulation()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_simulation.md)
