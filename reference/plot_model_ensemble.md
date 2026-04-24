# Plot Ensemble Predictions from a mosaic_ensemble Object

Renders time-series plots from a `mosaic_ensemble` object produced by
[`calc_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md).
Shows the weighted median prediction line with confidence interval
ribbons and observed data points. Optionally saves per-location
prediction CSVs for downstream use.

## Usage

``` r
plot_model_ensemble(
  ensemble,
  output_dir,
  data_dir = NULL,
  file_prefix = "ensemble",
  title_label = "Posterior Ensemble",
  save_predictions = FALSE,
  verbose = TRUE
)
```

## Arguments

- ensemble:

  A `mosaic_ensemble` object returned by
  [`calc_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md).

- output_dir:

  Character. Directory where plots are saved (and CSVs, when `data_dir`
  is not provided). Created if it does not exist.

- data_dir:

  Character. Directory where per-location prediction CSVs are written
  when `save_predictions = TRUE`. Defaults to `output_dir` for backwards
  compatibility. Pass a separate path (e.g. `3_results/predictions`) to
  keep CSVs out of the figures tree. Created if it does not exist.

- file_prefix:

  Character. Prefix used in output filenames:
  `predictions_<prefix>_<LOC>.pdf/csv` for per-location outputs and
  `predictions_<prefix>_cases_all.pdf` / `_deaths_all.pdf` for
  multi-location overview plots. Default `"ensemble"`.

- title_label:

  Character. Leading label used in plot titles
  (`"<title_label>: <LOC>"`). Default `"Posterior Ensemble"`.

- save_predictions:

  Logical. Save per-location prediction CSVs. Default `FALSE`.

- verbose:

  Logical. Print progress messages. Default `TRUE`.

## Value

Invisibly returns a list with:

- individual:

  Named list of ggplot objects, one per location.

- cases_faceted:

  Faceted cases plot (multi-location only).

- deaths_faceted:

  Faceted deaths plot (multi-location only).

- simulation_stats:

  Simulation metadata from the ensemble object.

## See also

[`calc_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md)
to compute the ensemble.
