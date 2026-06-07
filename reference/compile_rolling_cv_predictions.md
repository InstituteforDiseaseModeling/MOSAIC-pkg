# Rebuild the rolling-CV predictions table from run directories

Regenerates `predictions.parquet` from the per-cutoff `run_MOSAIC`
directories under a
[`run_rolling_cv()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
artifact (e.g. after adding a cutoff or to add quantile columns),
without recalibrating.

## Usage

``` r
compile_rolling_cv_predictions(
  dir_output,
  base_config = MOSAIC::config_default,
  models = NULL,
  n_reps_best_medioid = NULL,
  write = TRUE
)
```

## Arguments

- dir_output:

  A
  [`run_rolling_cv()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/run_rolling_cv.md)
  output directory (must contain `manifest.json` and `runs/`).

- base_config:

  Config used to recover the held-out (unmasked) observed series
  (default
  [`MOSAIC::config_default`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/config_default.md));
  must match the run config.

- write:

  Logical; write `predictions.parquet` (default TRUE).

## Value

The compiled long predictions data frame (invisibly if written).
