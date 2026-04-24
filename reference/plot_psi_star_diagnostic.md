# Plot Raw vs Calibrated Environmental Suitability (ψ vs ψ\*)

The psi_star parameters recalibrate the LSTM suitability signal on the
logit scale before it enters the transmission model. Without this plot
there is no routine way to see how much the calibration suppresses or
reshapes the LSTM output. The subtitle reports the mean suppression
percentage, making it immediately clear whether ψ is acting as a
meaningful seasonal driver or being effectively disabled.

## Usage

``` r
plot_psi_star_diagnostic(dirs, PATHS, location_names, verbose = TRUE)
```

## Arguments

- dirs:

  Named list of output directory paths as returned by the internal
  `.mosaic_ensure_dir_tree()` helper inside
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md).
  Required entries: `dirs$res_fig_diag` (output), `dirs$inputs` (for
  `config.json`), `dirs$res_post` (for `parameter_estimates.csv`).

- PATHS:

  Named list of project paths as returned by
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).
  Used to locate `pred_psi_suitability_day.csv` at `PATHS$MODEL_INPUT`.

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
environmental suitability ψ with the calibrated ψ\* after applying the
posterior psi_star parameters (`psi_star_a`, `psi_star_b`, `psi_star_z`,
`psi_star_k`) via
[`calc_psi_star`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_psi_star.md).

The plot is skipped gracefully (with a warning) for any location where:

- the psi_star posterior parameters are absent from
  `parameter_estimates.csv` (e.g. parameters were frozen or not
  sampled),

- `pred_psi_suitability_day.csv` cannot be found at `PATHS$MODEL_INPUT`,
  or

- the suitability data contains no rows for the location or calibration
  window.

## See also

[`calc_psi_star`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_psi_star.md)
for the transformation applied.
