# Post-hoc evaluation of a rolling-window forecast-validation artifact

Reads a
[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
predictions artifact and computes out-of-sample forecast skill: per-cell
metrics (R2 correlation & SSE, bias ratio, 50/95\\ horizons, plus skill
(MAE ratio) versus seasonal-climatology and persistence baselines,
aggregated across cutoffs and countries with bootstrap CIs.

## Usage

``` r
evaluate_rolling_cv(
  predictions,
  horizons_months = c(1, 3, 5),
  baselines = c("seasonal", "persistence"),
  metrics = c("cases", "deaths"),
  trusted_only = TRUE,
  n_boot = 1000L,
  seed = 1L
)
```

## Arguments

- predictions:

  A `run_rolling_cv` output directory, a path to a
  `predictions.parquet`, or the predictions data frame itself.

- horizons_months:

  Cumulative OOS horizons in months (default `c(1,3,5)`).

- baselines:

  Baselines for skill scores (default `c("seasonal","persistence")`).

- metrics:

  Metrics to evaluate (default `c("cases","deaths")`).

- trusted_only:

  Drop rows with `observed_source == "AI"` (default TRUE).

- n_boot:

  Bootstrap resamples over cells for the skill CI (default 1000).

- seed:

  RNG seed for the bootstrap (default 1).

  When the predictions carry a `model` column (`ensemble`,
  `ensemble_opt`, `best`, `medoid`), every metric is computed and
  reported **per model**; absent that column all rows are treated as a
  single `"ensemble"` model (back-compatible).

## Value

A list with:

- cells:

  Per (cutoff x model x iso x metric x window) metrics + per-baseline
  skill.

- summary:

  Aggregated across cells per (model x metric x window): cell count,
  mean/median of each metric, and mean baseline skill with bootstrap CI.

## Details

This is the post-hoc companion to
[`run_rolling_cv()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
— the harness emits predictions only; all judgement (metrics, baselines,
skill) lives here.

Horizons are **cumulative** (\\\le h\\ months): the \\\le h\\ window is
every OOS point within `h` months of the forecast origin, computed from
`date` and `cutoff_date` (not the disjoint `horizon_bucket` label), so
the \\\le\\max-horizon window equals the full scored OOS set. An `IS`
window (training fit, \\\le\\ cutoff) is always reported.

Scoring uses **trusted observed only**: rows whose `observed_source` is
missing or not `"AI"` (when `trusted_only = TRUE`).

Baselines, fit per (cutoff, country, metric) on the IS observed history:
`"persistence"` (trailing 4-point mean carried forward) and `"seasonal"`
(week-of-year mean of IS observations). Skill is the MAE ratio \\1 -
MAE\_{model}/MAE\_{baseline}\\ (positive = model beats baseline).

## See also

[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md),
[`calc_model_R2`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_R2.md),
[`calc_bias_ratio`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_bias_ratio.md)
