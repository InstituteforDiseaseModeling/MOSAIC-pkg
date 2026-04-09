# Plot Stochastic Ensemble Predictions

Renders timeseries plots from a `mosaic_ensemble` object produced by
[`calc_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md).
Optionally saves per-location prediction CSVs for downstream use.

Accepting a pre-computed ensemble object means simulations are never run
twice when both metrics and plots are needed.

## Usage

``` r
plot_model_ensemble(
  ensemble,
  output_dir,
  save_predictions = FALSE,
  verbose = TRUE
)
```

## Arguments

- ensemble:

  A `mosaic_ensemble` object returned by
  [`calc_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md).

- output_dir:

  Character. Directory where plots and CSVs are saved. Created if it
  does not exist.

- save_predictions:

  Logical. Save per-location prediction CSVs
  (`predictions_stochastic_<location>.csv`). Default `FALSE`.

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
