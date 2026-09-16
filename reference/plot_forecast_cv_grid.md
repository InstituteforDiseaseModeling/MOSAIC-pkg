# Publication timeseries grid for rolling-origin forecast cross-validation

Renders a country (column) \\\times\\ cutoff (row) grid of
observed-vs-model timeseries for one metric, the intuitive companion to
the scalar skill table
([`make_forecast_cv_table`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_forecast_cv_table.md)).
One figure per metric (cases in MOSAIC blue, deaths in MOSAIC red).

## Usage

``` r
plot_forecast_cv_grid(
  predictions,
  metric = c("cases", "deaths"),
  model = "ensemble_opt",
  isos = NULL,
  x_range = NULL,
  forecast_display_months = 9,
  scored_horizon_months = 3,
  ci = c("pi95", "pi50"),
  colors = NULL,
  dir_output = NULL,
  file_prefix = "forecast_cv_grid",
  width_per_country = 4.2,
  height_per_cutoff = 1.55,
  save_pdf = TRUE,
  save_png = FALSE,
  verbose = TRUE
)
```

## Arguments

- predictions:

  Predictions to plot: a data.frame, a path to a
  `predictions*.parquet`/`.csv`, or a directory holding per-cell
  `*/cutoff_*/predictions.parquet`. Must carry
  `iso_code, cutoff_date, date, metric, segment, observed, pred_median, model`
  and the CI columns for `ci` (e.g. `pi95_lo`/`pi95_hi`).

- metric:

  Single metric to render: `"cases"` or `"deaths"`.

- model:

  Model series to plot (default `"ensemble_opt"`, the pre-registered
  headline).

- isos:

  Country column order (character); default sorted unique isos.

- x_range:

  Optional `c(min,max)` Date for the shared x-axis; default spans
  `min(cutoff) - 6 months` to the earlier of
  `max(cutoff) + forecast_display_months` and the last observed date.

- forecast_display_months:

  Months of forecast shown past each cutoff (default 9). Beyond this,
  model line and OOS points are dropped.

- scored_horizon_months:

  Formally-scored horizon marker (default 3).

- ci:

  CI band to shade: `"pi95"` (default) or `"pi50"`.

- colors:

  Optional `c(line=, ci=)` hex overrides; default derives from
  [`mosaic_colors`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_colors.md)`(metric)` +
  a lightened CI variant.

- dir_output:

  If non-NULL, save the figure there.

- file_prefix:

  Output file stem (default `"forecast_cv_grid"`); the metric and model
  are appended.

- width_per_country, height_per_cutoff:

  Panel sizing (inches) for the saved canvas (default 4.2 and 1.55).

- save_pdf, save_png:

  Save a (cairo) PDF and/or PNG (default PDF only).

- verbose:

  Emit progress messages (default TRUE).

## Value

(Invisibly) the assembled `patchwork` object.

## Details

Encodings (locked for the OCV-4 experiment):

- Observed points by `segment`: filled circle = in-sample (training,
  \\\le\\ cutoff), `x` = embargo-gap weeks, open circle = out-of-sample
  (validation, shown up to `forecast_display_months` past the cutoff).

- Model `pred_median`: solid line + full-opacity CI ribbon for dates
  \\\le\\ cutoff; solid line + lighter CI ribbon after the cutoff
  (distinguished by the dashed cutoff rule) (clipped to
  `forecast_display_months`).

- Dashed vertical line at the cutoff; a faint dotted line at
  `cutoff + scored_horizon_months` marks the formally-scored boundary
  (the displayed window is longer than the scored window on purpose).

Per-country fixed y-axis (free across countries) is achieved by
assembling one
[`patchwork`](https://patchwork.data-imaginist.com/reference/patchwork-package.html)
column per country (vanilla `facet_grid` cannot free y by column). The
canvas is sized *width* \\\propto\\ number of countries and *height*
\\\propto\\ number of cutoffs, so adding countries widens the figure
rather than squishing panels.

## See also

[`make_forecast_cv_table`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_forecast_cv_table.md),
[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md),
[`evaluate_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md)
