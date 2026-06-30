# Plot Cori effective reproductive number (R_eff) over time

Renders the per-location, time-varying Cori (2013) instantaneous
**infection** effective reproductive number \\R\_{jt}\\ produced by
[`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md).
Draws the posterior median series (`q50` when the re-simulated CI is
present, else the `central` medoid series) as a purple line over date
with a horizontal reference line at \\R\_{\mathrm{eff}} = 1\\; when the
posterior credible-interval columns are populated it overlays the 95\\
`q25`-`q75` band when `show_iqr = TRUE`). Multi-location input is
faceted by location; a single (national) location is rendered as one
panel titled with the location.

## Usage

``` r
plot_Reff(reff, show_iqr = FALSE, title = NULL, ncol = NULL, base_size = 12)
```

## Arguments

- reff:

  A `reproductive_numbers` `data.frame` from
  [`calc_Reff`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md)
  with columns `location`, `date`, `t`, `estimand`, `central`, and the
  quantile columns (`q2.5`, `q25`, `q50`, `q75`, `q97.5`). The
  `ci_source` attribute (if present) is used for the caption. Leading
  warm-up rows with a non-finite `central` are dropped per location so
  no gap artifact is plotted.

- show_iqr:

  Logical. Draw the inner 50\\ addition to the 95\\ median line and the
  95\\ back-compatibility; set `TRUE` to re-enable the inner band when
  the `q25`/`q75` columns are populated.

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

**Graceful CI handling.** On production-default trajectory artifacts the
per-member `lines` are time-strided, so
[`calc_Reff()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_Reff.md)
cannot compute a daily renewal CI and returns the `q*` columns as
all-`NA` with `attr(reff, "ci_source") = "unavailable_strided_lines"`.
This function detects an all-`NA` CI per ribbon level and simply omits
that ribbon (drawing the central line only), adding a caption noting the
CI is unavailable for the artifact. It never errors when the CI is
missing.

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
