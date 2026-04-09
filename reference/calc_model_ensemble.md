# Run Stochastic Ensemble Simulations for Best Model

Runs `n_simulations` stochastic LASER simulations of a single model
configuration (typically `config_best`) with different random seeds and
aggregates the results into mean predictions and uncertainty envelopes.

Separating computation from plotting allows ensemble results to be used
for R², bias ratios, windowed fit metrics, and summary statistics
without necessarily generating plots, and avoids running simulations
twice when both metrics and plots are needed.

## Usage

``` r
calc_model_ensemble(
  config,
  n_simulations = 100L,
  envelope_quantiles = c(0.025, 0.975),
  parallel = FALSE,
  n_cores = NULL,
  root_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- config:

  Named list. Model configuration as returned by
  [`sample_parameters`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sample_parameters.md)
  (typically `config_best`).

- n_simulations:

  Integer. Number of stochastic LASER simulations to run. Default
  `100L`.

- envelope_quantiles:

  Numeric vector of length 2. Lower and upper quantiles for uncertainty
  envelopes. Stored in return object for use by
  [`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md).
  Default `c(0.025, 0.975)`.

- parallel:

  Logical. Use R parallel cluster for simulations. Default `FALSE`.

- n_cores:

  Integer or `NULL`. Number of cores when `parallel = TRUE`. `NULL` uses
  `detectCores() - 1`.

- root_dir:

  Character. MOSAIC root directory. Required when `parallel = TRUE` so
  workers can call
  [`set_root_directory()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/set_root_directory.md).

- verbose:

  Logical. Print progress messages. Default `TRUE`.

## Value

A named list (S3 class `"mosaic_ensemble"`) containing:

- cases_stats:

  List with `mean`, `median`, `lower`, `upper` matrices (n_locations ×
  n_time_points).

- deaths_stats:

  Same structure as `cases_stats` for deaths.

- cases_array:

  3-D array (n_locations × n_time_points × n_successful) of raw
  per-simulation case counts.

- deaths_array:

  Same for deaths.

- obs_cases:

  Observed cases matrix from `config$reported_cases`.

- obs_deaths:

  Observed deaths matrix from `config$reported_deaths`.

- n_simulations:

  Requested number of simulations.

- n_successful:

  Number that completed without error.

- seeds:

  Integer vector of seeds used.

- location_names:

  Character vector of location names.

- n_locations:

  Number of locations.

- n_time_points:

  Number of time steps.

- envelope_quantiles:

  The quantile pair passed in (for plotting).

- date_start:

  Character. Simulation start date from config.

- date_stop:

  Character. Simulation end date from config.

## See also

[`plot_model_ensemble`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/plot_model_ensemble.md)
to render plots from this object.
