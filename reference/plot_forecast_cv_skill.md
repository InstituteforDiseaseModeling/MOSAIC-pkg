# Headline forecast-CV plot: per-country out-of-sample metric across origins

Draws the headline view of a
[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
/ forecast-CV experiment that
[`plot_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_rolling_cv.md)
does not: a per-country out-of-sample summary at a single primary
horizon, **one point per forecast origin** (cutoff), faceted by metric.
With only a few origins per country there is no meaningful interval, so
it shows the **raw per-origin values** plus the per-country median
(diamond) and a tally annotation – never a confidence interval –
matching the small-\\n\\ discipline of
[`evaluate_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md).

## Usage

``` r
plot_forecast_cv_skill(
  x,
  value = c("R2_corr", "bias_ratio", "R2_sse", "wis_skill", "mae_skill"),
  horizon_months = 6,
  baseline = "seasonal",
  model = "ensemble",
  metrics = c("cases", "deaths"),
  show_gated = TRUE,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  dir_output = NULL,
  file_prefix = "forecast_cv",
  width = 9,
  height = NULL,
  dpi = 300,
  base_size = 14,
  verbose = TRUE
)
```

## Arguments

- x:

  A forecast-CV output directory (with `scores_cells.parquet`), a path
  to that parquet/csv, or the `$cells` data frame from
  [`evaluate_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md).

- value:

  What to plot (see Details). Default `"R2_corr"`.

- horizon_months:

  Primary OOS horizon; mapped to the `OOS<={h}mo` window (default 6).

- baseline:

  Baseline for the skill values (default `"seasonal"`); ignored for
  R2/bias.

- model:

  Model type to plot (default `"ensemble"`).

- metrics:

  Channels to facet (default `c("cases","deaths")`).

- show_gated:

  Logical; draw ESS-gated-out origins as hollow points instead of
  dropping them (default TRUE). Gated origins are excluded from the
  median + tally regardless.

- title, subtitle, caption:

  Plot labels (sensible defaults from `value`).

- dir_output:

  Directory to write the figure (PNG + PDF); NULL returns the ggplot
  only.

- file_prefix:

  Filename stem (default `"forecast_cv"`).

- width, height, dpi, base_size:

  Figure geometry.

- verbose:

  Logical (default TRUE).

## Value

Invisibly, a list with `plot` (ggplot), `data` (plotted long table),
`summary` (per unit x metric: median, n origins, tally, n_gated), and
`files` (written paths).

## Details

`value` selects what to plot:

- `"R2_corr"` / `"R2_sse"` – out-of-sample R^2 (shape vs scale-aware);
  tally = median.

- `"bias_ratio"` – mean(pred)/mean(obs); reference line at 1 (perfect);
  tally = origins within \[0.5, 2\].

- `"wis_skill"` / `"mae_skill"` – skill vs `baseline`
  (`1 - score_model/score_baseline`); reference line at 0; tally =
  origins with skill \> 0 (i.e. beating the baseline).

All are **conditional/hindcast** values when the experiment used
realized covariates (the forecast-CV default) – label them so.

## See also

[`plot_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_rolling_cv.md),
[`evaluate_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/evaluate_rolling_cv.md),
[`run_rolling_cv`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
