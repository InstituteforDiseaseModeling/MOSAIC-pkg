# Render all MOSAIC figures from a finished run directory

Reconstructs every
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
pipeline figure **from the data artifacts on disk** in a finished output
directory, writing them into `3_results/figures/**`. This is the
visualization layer of the modeling/visualization split:
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
writes a complete, self-describing run directory (all numeric artifacts,
ensemble `.rds` objects, diagnostic CSVs) independent of plotting, and
this function turns that directory into figures. It can therefore be run
post-hoc, on a different machine, or repeatedly without re-running
calibration.

## Usage

``` r
render_MOSAIC_figures(dir_output, which = NULL, plots = TRUE, verbose = TRUE)
```

## Arguments

- dir_output:

  Character. Path to a finished
  [`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
  output directory (the one containing `1_inputs/`, `2_calibration/`,
  `3_results/`).

- which:

  Character vector selecting figure groups to render, or `NULL`
  (default) for all. Valid groups: `"convergence"`, `"posterior"`,
  `"predictions"`, `"ppc"`, `"sensitivity"`, `"psi_star"`, `"spatial"`,
  `"trajectories"`.

- plots:

  Logical. Master switch. When `FALSE` the function returns immediately
  without rendering (mirrors `control$paths$plots`). Default `TRUE`.

- verbose:

  Logical. Print progress messages. Default `TRUE`.

## Value

Invisibly, a named logical vector indicating which figure groups were
attempted (`TRUE`) vs skipped (`FALSE`).

## Pure read-render (no re-simulation)

This function **never** calls
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md),
[`run_LASER()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_LASER.md),
or
[`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md).
Ensemble plots are reconstructed from the persisted `.rds` objects
(`2_calibration/ensemble_optimized.rds` or `ensemble_candidate.rds`, and
`medoid_ensemble.rds`). A missing, corrupt, or schema-incompatible
artifact causes the affected figure to be **warned-and-skipped**, never
rebuilt — rebuilding would trigger local simulation on the client
(`calc_model_ensemble(precomputed_results = NULL)` falls back to
PSOCK/sequential), which this function deliberately avoids. Every figure
is wrapped in `tryCatch` so one failure never aborts the rest.

## See also

[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
(writes the run directory),
[`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md),
[`plot_model_ppc`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ppc.md).
