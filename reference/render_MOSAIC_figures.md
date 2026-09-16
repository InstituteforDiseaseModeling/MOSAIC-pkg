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
render_MOSAIC_figures(
  dir_output,
  which = NULL,
  plots = TRUE,
  verbose = TRUE,
  cl = NULL,
  n_cores = 1L
)
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

- cl:

  Optional PSOCK cluster (from
  [`make_mosaic_cluster`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/make_mosaic_cluster.md))
  used to render the three per-location figure families in parallel: the
  prior/posterior distributions, the per-category posterior detail
  pages, and the trajectory pages. A cluster passed here is **borrowed,
  never stopped** – the caller owns its lifecycle. One figure per worker
  process, each opening and closing its own graphics device, so no
  device is ever shared.

- n_cores:

  Integer. When `cl` is `NULL` and this is greater than 1, render builds
  its own PSOCK cluster of this size and stops it before returning.
  Capped at `.MOSAIC_DETAIL_MAX_WORKERS` for the two memory-heavy
  families regardless. Default `1L` (serial, unchanged).

## Value

Invisibly, a named logical vector indicating which figure groups were
attempted (`TRUE`) vs skipped (`FALSE`).

## Pure read-render (no re-simulation)

This function **never** calls
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md),
[`run_simulation()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_simulation.md),
or
[`sample_parameters()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md).
Ensemble plots are reconstructed from the persisted `.rds` objects
(`2_calibration/ensemble_optimized.rds` or `ensemble_candidate.rds`, and
`medoid_ensemble.rds`). A missing, corrupt, or schema-incompatible
artifact causes the affected figure to be **warned-and-skipped**, never
rebuilt — rebuilding would trigger local simulation on the client
([`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
always simulates, via PSOCK or sequentially), which this function
deliberately avoids. Every figure is wrapped in `tryCatch` so one
failure never aborts the rest.

## Parallel rendering

At 40 locations this stage is the largest single-threaded block in a
production run: measured at 35.9 min of a 260-min 100,000-simulation
run, of which
[`plot_model_posteriors_detail()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_posteriors_detail.md)
alone was 20.0 min for 286 PDFs. The work is embarrassingly parallel –
every page is an independent `ggsave` to its own filename – so passing
`cl` or `n_cores` divides it across workers.
[`run_MOSAIC()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
passes `n_cores`: by the time figures render, the calibration cluster
has already been stopped (it goes at `R/run_MOSAIC.R`, right after the
calibration loop) and
[`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)'s
own cluster has come and gone too, so there is nothing left to borrow
and nothing to contend with for R's 128-connection ceiling.

Per-location failures are isolated: a worker that errors on one location
produces a warning on the master and the remaining locations still
render, matching the serial path's per-figure `tryCatch`.

## See also

[`run_MOSAIC`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_MOSAIC.md)
(writes the run directory),
[`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md),
[`plot_model_ppc`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ppc.md).
