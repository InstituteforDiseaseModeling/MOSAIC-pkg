# Plot Stochastic Model Fit: Timeseries with Uncertainty Envelopes

Runs multiple stochastic simulations of a model configuration and
creates timeseries plots showing mean predictions with confidence
envelopes.

## Usage

``` r
plot_model_fit_stochastic(
  config,
  n_simulations = 100,
  output_dir,
  envelope_quantiles = c(0.1, 0.9),
  save_predictions = FALSE,
  parallel = FALSE,
  n_cores = NULL,
  root_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- config:

  A configuration list/object as produced by sample_parameters(). Must
  contain all necessary parameters for running laser_cholera model
  including:

  - reported_cases - observed cases data

  - reported_deaths - observed deaths data

  - location_name - location identifiers

  - date_start and date_stop - time range for the plot

- n_simulations:

  Integer specifying number of stochastic simulations to run. Default is
  100.

- output_dir:

  Character string specifying the directory where plots should be saved.
  Directory will be created if it doesn't exist.

- envelope_quantiles:

  Numeric vector of length 2 specifying the quantiles for the confidence
  envelope. Default is c(0.1, 0.9) for 80% CI.

- save_predictions:

  Logical indicating whether to save prediction data to CSV files.
  Default is FALSE. If TRUE, saves predictions_stochastic_location.csv
  for each location.

- parallel:

  Logical indicating whether to use parallel computation. Default is
  FALSE. When TRUE, simulations are run in parallel across multiple
  cores.

- n_cores:

  Integer specifying number of cores to use for parallel computation.
  Default is NULL (uses detectCores() - 1). Only used when parallel =
  TRUE.

- root_dir:

  Character string specifying the root directory for MOSAIC project.
  Required when parallel = TRUE. Workers need this to initialize PATHS
  correctly.

- verbose:

  Logical indicating whether to print progress messages. Default is
  TRUE.

## Value

Invisibly returns a list containing:

- `individual`: Named list of plots for each location

- `cases_faceted`: Faceted plot of cases by location (if n_locations \>
  1)

- `deaths_faceted`: Faceted plot of deaths by location (if n_locations
  \> 1)

- `simulation_stats`: Statistics from the simulations including
  successful run count and aggregated predictions

## Details

This function performs multiple stochastic runs of the laser-cholera
model with identical configuration but different random seeds. It then
aggregates the results to show:

- Mean predictions across all simulations

- Confidence envelopes based on specified quantiles

- Observed data for comparison

- Summary statistics (total counts, correlations)

Unlike plot_model_fit(), this function does not calculate or display
likelihood values, focusing instead on prediction uncertainty through
stochastic simulation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Sample parameters for best model
best_config <- sample_parameters(PATHS, priors = priors,
                                 config = base_config, seed = 123)

# Create stochastic plots with 100 simulations
plots <- plot_model_fit_stochastic(
    config = best_config,
    n_simulations = 100,
    output_dir = "output/plots",
    envelope_quantiles = c(0.1, 0.9),
    save_predictions = TRUE  # Save predictions to CSV
)
# Saves: predictions_stochastic_ETH.csv (or other location names)

# Access individual plots
plots$individual[["ETH"]]  # Individual location plot for Ethiopia
} # }
```
