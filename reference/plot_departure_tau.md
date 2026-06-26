# Plot the model-implied departure probability (\\\tau_i\\) and daily travelers

Two-panel figure for the `"spatial"` group of
[`render_MOSAIC_figures`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/render_MOSAIC_figures.md):
(left, A) a forest plot of the daily departure probability \\\tau_i\\
per location, with optional 95\\ intervals; (right, B) the implied total
daily travelers leaving each origin, \\N_i \tau_i\\.

## Usage

``` r
plot_departure_tau(tau, N, location_name = names(tau), ci = NULL)
```

## Arguments

- tau:

  Numeric vector of daily departure probabilities (length J), in config
  order. From
  [`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md)
  element `$tau`.

- N:

  Numeric vector of origin populations (length J), config order.

- location_name:

  Character vector of length J giving the labels, config order. Defaults
  to `names(tau)`.

- ci:

  Optional data frame of credible intervals with columns `location`
  (matching `location_name`), `lower`, and `upper` (on the \\\tau\\
  scale). When `NULL` (default), no interval bars are drawn.

## Value

Invisibly, the combined cowplot object (or the single forest ggplot2
object if cowplot is unavailable).

## Details

The aesthetic matches the published \\\tau_j\\ panel of
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md)
exactly (`theme_minimal()`, red dashed mean line, black `shape = 21`
points, `dodgerblue` bars on a `scale_x_sqrt` axis,
[`cowplot::plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)
with A/B labels). Both panels are ordered by \\\tau_i\\ via
`reorder(iso3, mean)` — a keyed reorder over the data frame, not a
positional sort (F3-safe).

\\\tau_i\\ is a **daily** probability (the engine consumes it per-day in
the spatial-hazard step), so \\N_i \tau_i\\ is the expected number of
travelers *per day*. CI bars are drawn only when `ci` is supplied (from
the optional `mobility_tau_ci.csv` run artifact); otherwise the left
panel shows point estimates only — matching
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md)
minus the error bars.

## See also

[`calc_mobility_flux`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_mobility_flux.md),
[`plot_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_mobility.md).
