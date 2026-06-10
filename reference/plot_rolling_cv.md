# Presentation-quality plot of a rolling-origin forecast-validation artifact

Renders the out-of-sample forecast from a
[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
predictions artifact for one metric, one panel per cutoff (forecast
origin), restricted to the assessed horizon (default \\\le 5\\ months).
The two posterior ensembles (`ensemble`, `ensemble_opt`) are drawn as
"hero" series with 50/95\\ configurations (`best`, `medioid`) are drawn
as thin reference lines. Held-out observations are overlaid as points
and the forecast origin is marked with a labelled dashed rule. Each
panel is annotated with the cumulative \\\le\\max-horizon skill (R\\^2\\
correlation + WIS) of the ensembles, computed via
[`evaluate_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md).

## Usage

``` r
plot_rolling_cv(
  predictions,
  metric = c("cases", "deaths"),
  horizon_max_months = 5,
  context_months = 2,
  models_ribbon = c("ensemble", "ensemble_opt"),
  models_line = c("best", "medioid"),
  annotate = TRUE,
  eval = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  dir_output = NULL,
  file_prefix = "rolling_cv_forecast",
  width = 11,
  height_panel = 3.1,
  dpi = 300,
  base_size = 14,
  save_pdf = TRUE,
  verbose = TRUE
)
```

## Arguments

- predictions:

  A `run_rolling_cv` output directory, a path to a
  `predictions.parquet`, or the predictions data frame itself.

- metric:

  Metric to plot: `"cases"` (default) or `"deaths"`.

- horizon_max_months:

  Max assessed horizon in months; right edge of each panel relative to
  its cutoff (default 5).

- context_months:

  Months of in-sample context shown before the cutoff (left edge of each
  panel, default 2).

- models_ribbon:

  Models drawn with 50/95\\ (default `c("ensemble", "ensemble_opt")`).

- models_line:

  Models drawn as thin reference median lines (default
  `c("best", "medioid")`).

- annotate:

  Annotate each panel with ensemble \\\le\\max-horizon skill
  (R\\^2\\\_corr + WIS). Default `TRUE`.

- eval:

  Optional precomputed
  [`evaluate_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md)
  result (its `$cells`) to source annotations from; recomputed if
  `NULL`.

- title, subtitle, caption:

  Optional plot labels; sensible defaults are derived from the artifact
  when `NULL`.

- dir_output:

  Directory to write figures to. If `NULL` (default) nothing is written
  and the ggplot objects are returned for further use.

- file_prefix:

  Filename stem for written figures (default `"rolling_cv_forecast"`).

- width:

  Figure width in inches (default 11).

- height_panel:

  Height in inches allotted per cutoff panel (default 3.1).

- dpi:

  Raster resolution for the PNG (default 300).

- base_size:

  Base font size for
  [`theme_mosaic()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/theme_mosaic.md)
  (default 14).

- save_pdf:

  Also write a vector PDF alongside each PNG. Default `TRUE`.

- verbose:

  Print progress messages. Default `TRUE`.

## Value

Invisibly, a list with:

- overview:

  A faceted ggplot (one panel per cutoff).

- per_cutoff:

  Named list of single-cutoff ggplots.

- data:

  The truncated long data frame that was plotted.

- skill:

  The \\\le\\max-horizon skill cells used for annotation.

- files:

  Character vector of any files written.

## Details

**Horizon truncation.** The compiled artifact stores the full OOS path
to `window_stop` regardless of cutoff, so an early cutoff appears to
forecast years ahead. This function clips each panel to
`[cutoff - context_months, cutoff + horizon_max_months]` so the plotted
window equals the scored window. Scoring in
[`evaluate_rolling_cv()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md)
is already restricted to cumulative \\\le h\\-month windows, so this is
a display change only.

**Styling.** Uses
[`theme_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/theme_mosaic.md)
and the MOSAIC semantic palette. Output is sized for 16:9 slides and
written as both PNG (raster, `dpi`) and PDF (vector) when `dir_output`
is supplied.

## See also

[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md),
[`evaluate_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md),
[`theme_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/theme_mosaic.md),
[`mosaic_colors`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/mosaic_colors.md)
