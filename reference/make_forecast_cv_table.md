# Publication table for rolling-origin forecast cross-validation

Turns the scored cells from
[`evaluate_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md)
into the publication table companion to
[`plot_forecast_cv_grid`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_forecast_cv_grid.md):
a per country \\\times\\ cutoff \\\times\\ horizon detail table plus
per-country and pooled summary rows at the primary horizon.

## Usage

``` r
make_forecast_cv_table(
  cells,
  model = "ensemble_opt",
  metrics = c("cases", "deaths"),
  horizons_months = c(1, 2, 3),
  primary_horizon = 3,
  exploratory_isos = "NGA",
  near_cast_metrics = "deaths",
  train_start = NULL,
  dir_output = NULL,
  file_prefix = "forecast_cv_table"
)
```

## Arguments

- cells:

  Scored cells: `evaluate_rolling_cv(...)$cells`, or a path to a
  `scores_cells.parquet`/`.csv`. Must carry
  `iso_code, cutoff_date, metric, model, window` and skill/bias columns.

- model:

  Model to tabulate (default `"ensemble_opt"`).

- metrics:

  Metrics to include (default `c("cases","deaths")`).

- horizons_months:

  Horizons to include as detail rows (default `c(1,2,3)`); matched
  against `window == "OOS<=Nmo"`.

- primary_horizon:

  Horizon used for the summary rows (default 3); must be one of
  `horizons_months`.

- exploratory_isos:

  Isos flagged exploratory + dropped from the pooled summary (default
  `"NGA"`).

- near_cast_metrics:

  Metrics flagged conditional near-casts (default `"deaths"`).

- train_start:

  Anchor for the reported training-window length. NULL (default) uses
  each cell's `anchor_date` (the run's config start, carried by
  [`evaluate_rolling_cv()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md)
  cells), else `MOSAIC::config_default$date_start`.

- dir_output:

  If non-NULL, write `<file_prefix>_detail` and `_summary` as parquet
  (if arrow) else CSV.

- file_prefix:

  Output stem (default `"forecast_cv_table"`).

## Value

A list with `$detail` (per country x cutoff x horizon x metric) and
`$summary` (per country + pooled, at `primary_horizon`).

## Details

The headline skill is WIS-skill vs the seasonal-climatology baseline
(\\1 - WIS\_{model}/WIS\_{clim}\\); bias-ratio is a secondary
cumulative-bias check. NGA is flagged exploratory (surveillance
data-quality confound) and excluded from the pooled summary; deaths are
flagged conditional near-casts (the scoring embargo is shorter than the
infection-to-reported-death dwell).

Summary rows aggregate only cells that pass the weight-ESS gate
(`ess_ok`, when the column is present), matching
`evaluate_rolling_cv()$summary` and
[`plot_forecast_cv_skill`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_forecast_cv_skill.md);
`n_gated` counts the origins left out.

## See also

[`plot_forecast_cv_grid`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_forecast_cv_grid.md),
[`evaluate_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md)
