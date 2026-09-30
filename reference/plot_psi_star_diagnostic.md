# Plot Raw vs Calibrated Environmental Suitability (psi vs psi\*)

The psi_star parameters recalibrate the LSTM suitability signal on the
logit scale before it enters the transmission model. Without this plot
there is no routine way to see how much the calibration suppresses or
reshapes the LSTM output. The subtitle reports the mean suppression
percentage, making it immediately clear whether psi is acting as a
meaningful seasonal driver or being effectively disabled.

## Usage

``` r
plot_psi_star_diagnostic(dirs, PATHS = NULL, location_names, verbose = TRUE)
```

## Arguments

- dirs:

  Named list of output directory paths as returned by the internal
  `.mosaic_ensure_dir_tree()` helper inside
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).
  Required entries: `dirs$res_fig_diag` (output), `dirs$inputs` (for
  `config.json`), `dirs$res_posterior` (for `parameter_estimates.csv`).

- PATHS:

  Optional named list of project paths as returned by
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md);
  used only as a fallback to read
  `PATHS$MODEL_INPUT/pred_psi_suitability_day.csv` when the run's
  `config.json` carries no usable `psi_jt`. Default `NULL`.

- location_names:

  Character vector of ISO3 location codes (e.g. `"MOZ"`). One plot is
  generated per location.

- verbose:

  Logical; if `TRUE` (default) emits progress messages.

## Value

Invisibly returns a named list of `ggplot` objects, one per location.
Saves PNG files to `dirs$res_fig_diag` with filenames
`psi_raw_vs_psi_star_{j}.png`.

## Details

Creates a time-series plot comparing the raw LSTM-predicted
environmental suitability psi with the calibrated psi\* after applying
the posterior psi_star parameters (`psi_star_a`, `psi_star_b`,
`psi_star_z`, `psi_star_k`) via
[`calc_psi_star`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_psi_star.md).

The raw series is the `psi_jt` row the run actually calibrated on, read
from `1_inputs/config.json` over its daily `date_start`..`date_stop`
grid, so the figure is a pure read of the run directory and renders
post-hoc on any machine. Only when `config.json` has no `psi_jt` of
matching length does it fall back to the `psi` column of
`PATHS$MODEL_INPUT/pred_psi_suitability_day.csv`.

The plot is skipped gracefully (with a message) for any location where:

- the psi_star posterior parameters are absent from
  `parameter_estimates.csv` (e.g. parameters were frozen or not
  sampled),

- neither `psi_jt` nor the fallback CSV is available, or

- the suitability data contains no rows for the location or calibration
  window.

## See also

[`calc_psi_star`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_psi_star.md)
for the transformation applied.
