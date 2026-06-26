# Plot comprehensive model trajectories for one location

Renders the multi-panel "Model trajectories" figure for a single
location from a persisted `mosaic_trajectories` artifact (written by
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
when `control$predictions$capture_trajectories = TRUE`). Each panel
shows the comprehensive internal-state channels over time — surveillance
fit (reported cases/deaths), burden, incidence drivers, force of
infection, the SVEIR + W compartments, and sanity checks (mass balance,
population, epidemic fraction) — with a uniform-thinned set of
**actual** posterior member trajectories (the spaghetti, conveying
spread) and the weighted **median** overlaid in bold. Observed
surveillance points are overlaid only on the
`reported_cases`/`reported_deaths` panels.

## Usage

``` r
plot_model_trajectories(
  trajectories,
  location,
  output_dir,
  prefix = "trajectories",
  verbose = TRUE
)
```

## Arguments

- trajectories:

  A `mosaic_trajectories` object (from
  `readRDS("2_calibration/trajectories_ensemble.rds")`).

- location:

  Character. ISO/location code to plot; must appear in
  `trajectories$location_names`.

- output_dir:

  Directory to write the figures into (created if needed).

- prefix:

  File prefix. Default `"trajectories"` — outputs
  `<prefix>_<location>.pdf` and `<prefix>_<location>_p1.png`.

- verbose:

  Logical. Print progress. Default `TRUE`.

## Value

Invisibly, a named list of the file paths written.

## Details

**Pure read-render:** this function never imports LASER, never
re-simulates, and never re-weights — it consumes only the compact
artifact (per-channel weighted median + thinned actual lines, both
already reduced over the best subset). It is the trajectory analogue of
[`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md).

**Central line:** for `reported_cases` / `reported_deaths` the bold line
is the ensemble *central* series following
`control$predictions$central_method` (weighted median or weighted mean,
per channel) and is **bit-identical** to the cases/deaths *prediction*
plots – it is reduced from the same captured draws over the same final
displayed member set and weights (no re-simulation). All other channels
(compartments, FOI, incidence, derived) have no prediction-plot
counterpart and use the conventional weighted *median*.

**Weighting note:** the central series and lines are reduced over the
*final displayed* member set and weights: the *candidate* best subset
(`is_best_subset` / `weight_best`) when subset optimization is off, and
the *optimized* subset (`is_best_subset_opt` / `weight_best_opt`) when
`control$predictions$optimize_subset = TRUE`. Channels are captured at
sim time (stream-to-disk) and reduced over the optimized members with NO
re-simulation. The CFR(t) panel carries dashed endemic/epidemic regime
reference lines when `trajectories$cfr_refs` is present.

## See also

[`render_MOSAIC_figures`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/render_MOSAIC_figures.md),
[`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md).
