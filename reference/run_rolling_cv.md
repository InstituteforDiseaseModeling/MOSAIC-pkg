# Rolling-Window Forecast Validation for the MOSAIC Transmission Model

Runs an expanding-window (fixed-anchor) rolling-origin backtest of the
MOSAIC transmission model. For each cutoff date `T` it (1) trains the
environmental-suitability (psi) LSTM on data up to `T`, (2) injects that
psi into the config, (3) calibrates the transmission model on observed
cases up to `T`, and (4) projects forward over the out-of-sample (OOS)
window.

## Usage

``` r
run_rolling_cv(
  PATHS,
  iso,
  n_cutoffs = 12L,
  latest_cutoff = NULL,
  step_months = 1L,
  horizons_months = c(1, 3, 5),
  embargo_weeks = 1L,
  base_config = MOSAIC::config_default,
  priors = MOSAIC::priors_default,
  control = NULL,
  optimize_subset = TRUE,
  models = c("ensemble", "ensemble_opt", "best", "medioid"),
  n_reps_best_medioid = 50L,
  est_suitability_spec = list(),
  dask_spec = NULL,
  dir_output,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  Path list from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- iso:

  Character; one ISO3 code or a vector (coupled metapopulation).

- n_cutoffs:

  Integer; number of monthly cutoffs (default 12).

- latest_cutoff:

  Date/character or NULL; most-recent cutoff. If NULL, computed as
  `(last scorable observed date) - embargo - max(horizons)`.

- step_months:

  Integer; months between cutoffs (default 1).

- horizons_months:

  Numeric vector of forecast horizons in months (default `c(1,3,5)`);
  used to set the projection length and to label OOS points. The largest
  horizon bounds the latest cutoff.

- embargo_weeks:

  Integer; gap between IS stop (T) and OOS start (default 1).

- base_config:

  MOSAIC config (default
  [`MOSAIC::config_default`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_default.md));
  its window defines the anchor and projection span.

- priors:

  Priors list (default
  [`MOSAIC::priors_default`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/priors_default.md)).

- control:

  `run_MOSAIC` control list, or NULL for an experiment-grade cheap
  default (fixed `n_simulations`, plots off). See
  [`mosaic_control_defaults`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_control_defaults.md).

- optimize_subset:

  Logical (default `TRUE`); enable `run_MOSAIC`'s post-ensemble
  best-subset optimizer (`control$predictions$optimize_subset`). When
  `TRUE` the harness sets this on the resolved control for every cutoff,
  so the ensemble is re-scored against the training-window observed
  series and the posterior is driven by the optimizer-selected subset.
  Set `FALSE` to use the raw candidate ensemble.

- models:

  Character vector of model types to score and carry in
  `predictions.parquet` (default all four:
  `c("ensemble","ensemble_opt","best","medioid")`). `"ensemble"`
  (posterior-weighted candidate) is always included. `"ensemble_opt"` is
  the optimizer-selected subset (only emitted when
  `optimize_subset = TRUE` and `ensemble_optimized.rds` exists).
  `"best"` and `"medioid"` are re-simulated from their saved configs
  (see `n_reps_best_medioid`). Each model appears as a value of the
  `model` column.

- n_reps_best_medioid:

  Integer (default 50); number of stochastic LASER reruns used to build
  the predictive median + intervals for the `best` and `medioid`
  configs. These reruns execute locally in the calling R process (not on
  Dask), so cost scales with this value times the number of cutoffs and
  locations.

- est_suitability_spec:

  Named list of *modeling* arguments passed through to
  [`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
  (e.g. `architecture`, `feature_set`, `response_var`, `bias_correct`,
  and the lstm_v2 `arch_control` list). Date arguments are ignored
  (harness-owned). Deprecated v0.33 keys (`n_splits`,
  `exclude_covariates`) are accepted but ignored with a per-cutoff
  deprecation message — prefer `arch_control` for lstm_v2 knobs.

- dask_spec:

  Optional Dask/Coiled spec passed to `run_MOSAIC`.

- dir_output:

  Directory for the experiment artifact (created if needed).

- verbose:

  Logical (default TRUE).

## Value

Invisibly, the manifest list. Side effects: writes `manifest.json`,
`predictions.parquet`, and `runs/` under `dir_output`.

## Details

This function is a **fit-and-forecast engine only**: it produces
calibrations, projections, and one organized predictions artifact. It
does **not** compute evaluation metrics, baselines, or skill scores —
those are done post-hoc by reading `predictions.parquet`.

**Window.** The in-sample (IS) start is fixed at `config$date_start`
(the anchor); the simulation runs over the full `config` window
(`config$date_start` .. `config$date_stop`). For each cutoff `T` the
calibration likelihood only scores weeks \\\le T\\ (observations after
`T` are masked to `NA`); the post-`T` portion of the simulation is the
forecast. A `embargo_weeks` gap separates the IS stop (`T`) from the OOS
start (`T + embargo`); the embargo week is neither trained nor labeled
OOS.

**Leakage discipline.** psi is re-fit per cutoff with
`fit_date_stop = T`; the harness *owns* the leakage-critical date
arguments to
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
and overrides any date keys passed via `est_suitability_spec` (with a
warning). `est_suitability_spec` therefore controls only modeling
choices (target, features, architecture), not the cutoff window.

**Coupled metapopulation.** `iso` may be a single country or a vector; a
vector runs as the coupled metapopulation (one calibration per cutoff
covering all listed locations). Thus there is one `run_MOSAIC`
calibration per cutoff (not per country).

**Outputs.** Under `dir_output`: `manifest.json` (settings + per-run
index with status), `predictions.parquet` (the compiled long table), and
`runs/cutoff_<T>/` (the native `run_MOSAIC` directory for each cutoff).
`predictions.parquet` is a derived view — it can be rebuilt from the run
directories with
[`compile_rolling_cv_predictions`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_rolling_cv_predictions.md).

The predictions table has one row per (cutoff x location x date x
metric) with columns:
`run_id, iso_code, anchor_date, cutoff_date, date, metric, segment`
(IS/embargo/OOS),
`weeks_ahead, horizon_bucket, observed, observed_source, pred_median`,
and CI columns (`pi*_lo`/`pi*_hi`). `observed` is the held-out
(unmasked) trusted surveillance value, so OOS rows carry the real target
for post-hoc scoring.

## See also

[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md),
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md),
[`compile_rolling_cv_predictions`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_rolling_cv_predictions.md)
