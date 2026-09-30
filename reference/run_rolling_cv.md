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
  models = c("ensemble", "ensemble_opt", "medoid"),
  n_reps_best_medoid = 50L,
  central_method = "mean",
  est_suitability_spec = list(),
  psi_cache = NULL,
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
  The harness forces `paths$clean_output = TRUE`, so each cutoff's
  `runs/cutoff_<T>/` directory is wiped before its calibration and
  simulation shards left by an earlier interrupted attempt cannot enter
  the new posterior.

- optimize_subset:

  Logical (default `TRUE`); enable `run_MOSAIC`'s post-ensemble
  best-subset optimizer (`control$predictions$optimize_subset`). When
  `TRUE` the harness sets this on the resolved control for every cutoff,
  so the ensemble is re-scored against the training-window observed
  series and the posterior is driven by the optimizer-selected subset.
  Set `FALSE` to use the raw candidate ensemble.

- models:

  Character vector of model types to score and carry in
  `predictions.parquet` (default
  `c("ensemble","ensemble_opt","medoid")`). `"ensemble"`
  (posterior-weighted candidate) is always included. `"ensemble_opt"` is
  the optimizer-selected subset, emitted only for cutoffs where the
  optimizer actually ran and selected a subset (`subset_opt.rds`
  present, or a finite `n_ensemble_params_tier` in `summary.json`); with
  `optimize_subset = FALSE` it is dropped from `models` up front, and a
  cutoff whose optimizer selected nothing is skipped with a warning
  rather than duplicating the candidate ensemble, which
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  saves as a fallback `ensemble_optimized.rds`. `"medoid"` is
  re-simulated from its saved config (see `n_reps_best_medoid`).
  `"best"` is accepted for back-compat but is no longer produced by
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  (no `config_best.json`); it is skipped with a warning unless an older
  run dir still carries that file. Each model appears as a value of the
  `model` column.

- n_reps_best_medoid:

  Integer (default 50); number of stochastic reruns used to build the
  predictive median + intervals for the `best` and `medoid` configs.
  These reruns execute locally in the calling R process, so cost scales
  with this value times the number of cutoffs and locations.

- central_method:

  Ensemble central tendency used for the compiled predictions and the
  in-sample calibration metrics/medoid: `"mean"` (default; the expected
  count, which never collapses to zero on sparse deaths) or `"median"`
  (the typical trajectory; the default from v0.46.1 to v0.97.x). Scalar
  or per-channel `c(cases=, deaths=)`. The predictions table carries
  `pred_central` (this choice) plus `pred_mean`/`pred_median` for
  cross-walk; WIS/coverage remain quantile-based and are unaffected.

- est_suitability_spec:

  Named list of *modeling* arguments passed through to
  [`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
  (e.g. `architecture`, `feature_set`, `response_var`, `bias_correct`,
  and the lstm_v2 `arch_control` list). Date arguments are ignored
  (harness-owned). Deprecated v0.33 keys (`n_splits`,
  `exclude_covariates`) are accepted but ignored with a per-cutoff
  deprecation message – prefer `arch_control` for lstm_v2 knobs. When
  `psi_cache` is supplied this spec is used *only* to recompute the
  cache spec-hash for validation; the per-cutoff
  [`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
  fit is skipped entirely.

- psi_cache:

  NULL (default) or a directory produced by
  [`prefit_rolling_cv_psi`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/prefit_rolling_cv_psi.md).
  When NULL the per-cutoff psi is re-fit in-place (original behavior).
  When set, the per-cutoff
  [`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
  call is skipped and the frozen `psi_<T>.csv` is loaded from this cache
  directory instead. The run **hard-errors** if a requested cutoff is
  absent from the cache manifest, if the run's `est_suitability_spec`
  hash does not match the manifest `spec_hash` recorded for that cutoff,
  or if the cache's prediction window does not cover `base_config`'s
  start through the cutoff's last OOS date. When `psi_cache` is NULL the
  per-cutoff psi is fitted into a scratch `MODEL_INPUT` (the canonical
  `pred_psi_suitability_day.csv` is never overwritten) and kept as
  `runs/psi_cutoff_<T>.csv`.

- dir_output:

  Directory for the experiment artifact (created if needed).

- verbose:

  Logical (default TRUE).

## Value

Invisibly, the manifest list. Side effects: writes `manifest.json`,
`predictions.parquet`, and `runs/` under `dir_output`. `manifest.json`
is rewritten atomically after every cutoff (`status = "running"`, then
`"complete"`), so the cutoffs finished before an interruption can still
be compiled with
[`compile_rolling_cv_predictions`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_rolling_cv_predictions.md).

## Details

This function is a **fit-and-forecast engine only**: it produces
calibrations, projections, and one organized predictions artifact. It
does **not** compute evaluation metrics, baselines, or skill scores –
those are done post-hoc by reading `predictions.parquet`.

**Window.** The in-sample (IS) start is fixed at `config$date_start`
(the anchor); the simulation runs over the full `config` window
(`config$date_start` .. `config$date_stop`). For each cutoff `T` the
calibration likelihood only scores weeks \\\le T\\ (observations after
`T` are masked to `NA`); the post-`T` portion of the simulation is the
forecast. A `embargo_weeks` gap separates the IS stop (`T`) from the OOS
start: dates in `(T, T + embargo]` are labeled `"embargo"` (neither
trained nor scored) and the first OOS date is `T + embargo + 1`.
`weeks_ahead` counts whole weeks from that first OOS date (week 1 = its
first seven days).

**Leakage discipline.** What is rebuilt as of each cutoff:

- psi is re-fit per cutoff with `fit_date_stop = T`; the harness *owns*
  the leakage-critical date arguments to
  [`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
  and overrides any date keys passed via `est_suitability_spec` (with a
  warning), so the spec controls only modeling choices (target,
  features, architecture).

- The reported CFR `mu_jt` and its prior are rebuilt from a WHO-annual
  GAM fitted only to years up to `year(T) - 1`, carried flat past that
  year's 1 July. The annual totals used are the final revised ones, not
  the vintage that had been published at `T`.

- Observed cases and deaths after `T` are masked, and epidemic peaks
  whose 14-day peak-shape scoring window reaches past `T` are dropped
  from the cutoff config (an empty set is kept as a 0-row table so the
  likelihood never falls back to the full
  [`epidemic_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/epidemic_peaks.md)
  dataset). The remaining peaks were still *detected* on the full
  record.

What is **not** as of the cutoff, so the hindcast is only approximately
leak-free:

- psi fitted in place (`psi_cache = NULL`), or from a cache built with
  any feature set other than `"v7.4"`, is trained on the canonical
  suitability panel. Its per-country target anchors and its
  flood-probability GAM were fitted on the whole panel, including rows
  after `T`. The run warns when this is the case; build the cache with
  [`prefit_rolling_cv_psi`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/prefit_rolling_cv_psi.md)`(est_suitability_spec = list(feature_set = "v7.4", ...))`
  for per-cutoff leak-free panels.

- Every prior other than `mu_jt` comes from `priors` unchanged.
  `priors_default` carries centres derived from surveillance through its
  build date (e.g. the per-country `beta_j0_tot` centres, recentred on
  posterior medians of fits to the full record); supply as-of priors for
  a strictly leak-free hindcast.

**Coupled metapopulation.** `iso` may be a single country or a vector; a
vector runs as the coupled metapopulation (one calibration per cutoff
covering all listed locations). Thus there is one `run_MOSAIC`
calibration per cutoff (not per country).

**Outputs.** Under `dir_output`: `manifest.json` (settings + per-run
index with status), `predictions.parquet` (the compiled long table), and
`runs/cutoff_<T>/` (the native `run_MOSAIC` directory for each cutoff).
`predictions.parquet` is a derived view – it can be rebuilt from the run
directories with
[`compile_rolling_cv_predictions`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_rolling_cv_predictions.md).

The predictions table has one row per (cutoff x location x date x
metric) with columns:
`run_id, iso_code, anchor_date, cutoff_date, date, metric, segment`
(IS/embargo/OOS),
`weeks_ahead, horizon_bucket, observed, observed_source, pred_central`
(the scored series), `pred_mean, pred_median, central_method`, and CI
columns (`pi*_lo`/`pi*_hi`). `observed` is the held-out (unmasked)
trusted surveillance value, so OOS rows carry the real target for
post-hoc scoring.

## See also

[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md),
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md),
[`compile_rolling_cv_predictions`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_rolling_cv_predictions.md)
