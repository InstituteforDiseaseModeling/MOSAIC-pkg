# Assemble the model convergence-status table

Reads the calibration convergence diagnostics JSON from a results
directory and assembles the metric/value/target/status table that
[`plot_model_convergence_status`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_convergence_status.md)
renders. The derived `convergence_status.csv` (distinct from the raw
`convergence_results.parquet`) is written here so it is produced
independently of plotting, and the plot consumes the returned table.

## Usage

``` r
calc_model_convergence_status(results_dir, output_dir = NULL, verbose = TRUE)
```

## Arguments

- results_dir:

  Path to the results directory containing the convergence diagnostics
  JSON (and, optionally, `parameter_ess.csv`).

- output_dir:

  Optional directory to write `convergence_status.csv`. When `NULL`
  (default) no file is written.

- verbose:

  Logical; print progress messages.

## Value

A list with: `metrics_data` (data.frame with columns `Metric`,
`Description`, `Target`, `Value`, `Status`), `metric_expressions` (list
of plotmath expressions, aligned row-for-row with `metrics_data`),
`diagnostics` (the parsed JSON), `param_ess_data` (data.frame or
`NULL`), `n_params`, `n_pass`, and `target_ess_param`. Returns `NULL` if
no diagnostics file is found or no metrics could be assembled.

## Details

Auto-detects the diagnostics file in this order:
`convergence_diagnostics.json` (likelihood-based) then
`convergence_diagnostics_loss.json` (loss-based).
