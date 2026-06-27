# Post-hoc evaluation of a rolling-window forecast-validation artifact

Reads a
[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
predictions artifact and computes out-of-sample forecast skill: per-cell
metrics (R2 correlation & SSE, bias ratio, 50/95\\ horizons, plus
MAE-skill and WIS-skill versus seasonal-climatology and persistence
baselines, aggregated across cutoffs and countries with bootstrap CIs
(suppressed at small cell counts).

## Usage

``` r
evaluate_rolling_cv(
  predictions,
  horizons_months = c(1, 3, 5),
  baselines = c("seasonal", "persistence", "persistence_last"),
  metrics = c("cases", "deaths"),
  trusted_only = TRUE,
  embargo_weeks = 0,
  ess_min = 0,
  min_cells_ci = 5L,
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

  Baselines for skill scores (default
  `c("seasonal","persistence","persistence_last")`).

- metrics:

  Metrics to evaluate (default `c("cases","deaths")`).

- trusted_only:

  Drop rows with `observed_source == "AI"` (default TRUE).

- embargo_weeks:

  Channel-specific OOS embargo, in weeks, after the cutoff before
  scoring begins. A scalar applies to all metrics; a named vector (e.g.
  `c(cases = 4, deaths = 6)`) applies per metric. Default `0`.

- ess_min:

  Minimum importance-weight ESS for a cell to enter the summary
  aggregation. Default `0` (gate off). See Details.

- min_cells_ci:

  Minimum number of finite cells in a (model, metric, window) group
  required to emit a bootstrap CI; below this the CI is suppressed (lo =
  hi = NA) and `ci_suppressed = TRUE`. Default `5`.

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
  mae_skill and wis_skill, the cell `ess` and `ess_ok` flag.

- summary:

  Aggregated across `ess_ok` cells per (model x metric x window):
  `n_cells_used` / `n_cells_total`, mean/median of each metric, and mean
  baseline skills with bootstrap CI (suppressed when fewer than
  `min_cells_ci` finite cells), plus `ess_gated` and `ci_suppressed`
  flags.

## Details

This is the post-hoc companion to
[`run_rolling_cv()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
— the harness emits predictions only; all judgement (metrics, baselines,
skill) lives here.

Horizons are **cumulative** (\\\le h\\ months): the \\\le h\\ window is
every OOS point within `h` months of the per-metric OOS-scoring origin,
computed from `date` and `cutoff_date` (not the disjoint
`horizon_bucket` label), so the \\\le\\max-horizon window equals the
full scored OOS set. An `IS` window (training fit, \\\le\\ cutoff) is
always reported.

Scoring uses **trusted observed only**: rows whose `observed_source` is
missing or not `"AI"` (when `trusted_only = TRUE`).

**Channel-specific embargo.** `embargo_weeks` may be a scalar (all
metrics) or a named vector such as `c(cases = 4, deaths = 6)`. For each
metric the OOS scoring start is re-derived as
`cutoff_date + embargo_weeks[metric] * 7`; only OOS rows at or after
that per-metric boundary are scored, independent of the single `segment`
label baked into predictions. Default (`0`) reproduces the prior
behavior of scoring the entire `segment == "OOS"` block.

**Baselines**, fit per (cutoff, country, metric) on the IS observed
history. `"seasonal"` (week-of-year climatology) is the **primary**
baseline and **requires \\\ge 2\\ years of in-sample history**; with
less, the seasonal baseline (and its skill) is `NA` (no grand-mean
fallback). `"persistence"` (trailing 4-point mean carried forward) and
`"persistence_last"` (the single last observed value) are secondary
nulls. Each baseline is also given 50/95\\ quantiles of its in-sample
residuals, so a genuine **WIS-skill** exists: \\1 -
WIS\_{model}/WIS\_{baseline}\\. `mae_skill` is the analogous MAE ratio.
Positive = model beats the baseline; `NA` when the baseline (or its
interval) cannot be formed.

**Weight-ESS gate.** If the predictions carry an `ess` column (the
cutoff calibration's importance-weight ESS, constant per cutoff), each
cell is flagged `ess_ok = (!is.na(ess) & ess >= ess_min)`. Cells failing
the gate remain in `$cells` (flagged) but are **excluded** from the
`$summary` aggregation. With `ess_min = 0` (default) the gate is off (NA
ess still flags `ess_ok = FALSE`, but the gate threshold 0 admits any
finite ess). When the `ess` column is absent, `ess_ok = TRUE` for all
cells (back-compat) and `$summary$ess_gated = FALSE`.

## See also

[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md),
[`calc_model_R2`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_R2.md),
[`calc_bias_ratio`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_bias_ratio.md)
