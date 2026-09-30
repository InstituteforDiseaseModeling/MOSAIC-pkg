# Plot the route-decomposed effective reproductive number over time

Renders the per-location R_eff(t) produced by
[`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md)
or
[`add_reproductive_numbers`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/add_reproductive_numbers.md).
When the input carries the route components (`estimand` `"R_hum"` and
`"R_env"`) and `routes = TRUE`, the environmental reproductive number is
drawn as a filled area from zero and the human-to-human contribution is
stacked on top of it, so the upper edge is the total \\R\_{eff} =
R\_{env} + R\_{hum}\\ (purple line) and the height of the orange band is
how much human transmission adds. A dashed reference line marks
\\R\_{eff} = 1\\. Without route rows (older artifacts) the total alone
is drawn.

## Usage

``` r
plot_Reff(
  reff,
  show_iqr = FALSE,
  smooth_days = 14L,
  title = NULL,
  ncol = NULL,
  base_size = 12,
  routes = TRUE
)
```

## Arguments

- reff:

  A `reproductive_numbers` data.frame with columns `location`, `date`,
  `central`, optionally `estimand` and the quantile columns. Leading
  rows with a non-finite total are dropped per location.

- show_iqr:

  Logical. Also draw the inner 50% (`q25`-`q75`) total-R band. Default
  `FALSE`.

- smooth_days:

  Integer. Centered rolling-mean window (days) for the displayed series;
  `1` plots the raw daily values. Default `14`.

- title:

  Character or `NULL` for the default title.

- ncol:

  Integer facet columns for multi-location input (`NULL`:
  `min(3, n_locations)`).

- base_size:

  Numeric base font size for
  [`theme_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/theme_mosaic.md).

- routes:

  Logical. Stack `R_env` and `R_hum` under the total when present.
  Default `TRUE`.

## Value

A `ggplot` object (not printed or saved).

## Details

**Headline series.** On the re-simulation path `central` is the MEDOID
trajectory's R_t (a coherent member, preserving peak timing and height);
on the direct path it is the renewal on weighted-median incidence. The
daily series is noisy, so each component is shown as a centered
`smooth_days` rolling mean, taken over the days on which the total is
defined (a silent route counts as 0 there, as in
[`calc_Reff()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md))
so the smoothed stack still sums to the smoothed total, with the raw
daily total as a faint background line.

**Faint band.** When populated, the `q2.5`-`q97.5` total-R band is the
per-calendar-date range across members. Member peaks are
phase-misaligned, so it does not show the epidemic's peak R_t; the
per-member peak statistic (attr `peak_Rt`) is annotated instead.

## See also

[`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md),
[`add_reproductive_numbers`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/add_reproductive_numbers.md).

## Examples

``` r
if (FALSE) { # \dontrun{
tr  <- readRDS("2_calibration/trajectories_ensemble.rds")
# The medoid config carries the calibrated kernel and decay rates; the base
# 1_inputs/config.json holds prior centres (see add_reproductive_numbers()).
cfg <- jsonlite::fromJSON("2_calibration/best_model/config_medoid.json")
print(plot_Reff(calc_Reff(tr, cfg)))
} # }
```
