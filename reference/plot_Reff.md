# Plot Cori effective reproductive number (R_eff) over time

Renders the per-location, time-varying Cori (2013) instantaneous
**infection** effective reproductive number \\R\_{jt}\\ produced by
[`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md).
The headline line is the **medoid trajectory** R_t (`central`) drawn as
a bold purple line over date, with a horizontal reference line at
\\R\_{\mathrm{eff}} = 1\\. Because the medoid is a single coherent
member trajectory it preserves the timing and height of the epidemic's
R_t peak (typically 2-3.3), unlike a per-calendar-day cross-member
median which flattens phase-misaligned peaks toward 1.

## Usage

``` r
plot_Reff(reff, show_iqr = FALSE, title = NULL, ncol = NULL, base_size = 12)
```

## Arguments

- reff:

  A `reproductive_numbers` `data.frame` from
  [`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md)
  with columns `location`, `date`, `t`, `estimand`, `central` (the
  medoid trajectory R_t), and the per-calendar-date quantile columns
  (`q2.5`, `q25`, `q50`, `q75`, `q97.5`). The `peak_Rt`,
  `central_definition`, `band_definition` and `ci_source` attributes
  (when present) drive the annotation and caption. Leading warm-up rows
  with a non-finite `central` are dropped per location so no gap
  artifact is plotted.

- show_iqr:

  Logical. Draw the inner 50\\ addition to the faint 95\\ medoid line
  and the faint 95\\ back-compatibility.

- title:

  Character or `NULL`. Plot title. `NULL` (default) uses
  `"Effective reproductive number"` for multi-location input and
  `"Effective reproductive number: <LOC>"` for a single location.

- ncol:

  Integer. Number of facet columns for multi-location input. `NULL`
  (default) uses `min(3, n_locations)`.

- base_size:

  Numeric. Base font size passed to
  [`theme_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/theme_mosaic.md).
  Default `12`.

## Value

A `ggplot` object (not printed or saved). The driver
[`add_reproductive_numbers`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/add_reproductive_numbers.md)
is responsible for saving it.

## Details

**Faint posterior band.** The 95\\ the per-calendar-date posterior range
*across members* and is rendered faintly. The caption makes explicit
that this band does *not* represent the epidemic's peak R_t (member
peaks are phase-misaligned in calendar time); that explosivity statistic
is shown by the medoid line and the per-member peak R_t annotation. The
inner 50\\ `show_iqr = TRUE` and those columns are populated.

**Per-member peak R_t annotation.** When the input carries an
`attr(reff, "peak_Rt")` data.frame (per-location posterior-weighted
`q2.5`/`q50`/`q97.5` of each member's time-max R_t), the plot annotates
it: for a single location in the subtitle, for multi-location input as a
per-facet in-panel label. Older artifacts that lack the attribute are
handled gracefully (the annotation is simply omitted).

**Graceful CI handling.** If the quantile columns are absent or all `NA`
(e.g. an older artifact with strided trajectory lines, attr
`ci_source = "unavailable_strided_lines"`), the band is omitted (medoid
line only) and the caption notes the missing CI. The function never
errors when the band or peak annotation is missing.

## References

Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new framework and
software to estimate time-varying reproduction numbers during epidemics.
American Journal of Epidemiology 178(9):1505-1512.

## See also

[`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md)
to compute the series;
[`add_reproductive_numbers`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/add_reproductive_numbers.md)
to apply both to an output directory.

## Examples

``` r
if (FALSE) { # \dontrun{
tr  <- readRDS("2_calibration/trajectories_ensemble.rds")
cfg <- jsonlite::fromJSON("1_inputs/config.json")
reff <- calc_Reff(tr, cfg)
p <- plot_Reff(reff)
print(p)
} # }
```
