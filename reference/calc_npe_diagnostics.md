# Calculate NPE Model Diagnostics with Simulation-Based Calibration (SBC)

Performs comprehensive SBC diagnostics for NPE models trained with
train_npe(). Tests calibration by sampling from priors, generating
synthetic data, running NPE inference, and checking coverage/uniformity
of posterior distributions.

## Usage

``` r
calc_npe_diagnostics(
  n_sbc_sims,
  model,
  npe_model_dir = NULL,
  PATHS,
  priors,
  config_base,
  param_names = NULL,
  n_npe_samples = 100,
  probs = c(0.025, 0.25, 0.75, 0.975),
  seed_offset = 1000,
  output_dir = NULL,
  verbose = TRUE,
  parallel = FALSE,
  n_cores = NULL
)
```

## Arguments

- n_sbc_sims:

  Integer number of SBC test simulations to run

- model:

  NPE model object from train_npe() (required)

- npe_model_dir:

  Character path to NPE model directory (optional, extracted from model)

- PATHS:

  List containing project paths from get_paths()

- priors:

  Prior distributions object from get_location_priors()

- config_base:

  Base configuration from get_location_config()

- param_names:

  Character vector of parameter names (optional, extracted from model)

- n_npe_samples:

  Number of NPE posterior samples per test (default 100)

- probs:

  Numeric vector of quantiles for coverage (default c(0.025, 0.25, 0.75,
  0.975))

- seed_offset:

  Integer offset for random seeds (default 1000)

- output_dir:

  Character path to save diagnostic files (required for plotting)

- verbose:

  Logical whether to print progress messages (default TRUE)

- parallel:

  Logical whether to run simulations in parallel (default FALSE)

- n_cores:

  Number of cores for parallel processing (default NULL = auto)

## Value

List with SBC diagnostic results compatible with test_17 workflow:

- coverage:

  Matrix of coverage indicators (n_sbc_sims x n_params)

- sbc_ranks:

  Matrix of SBC ranks (n_sbc_sims x n_params)

- summary:

  Data frame with per-parameter summary statistics

- param_names:

  Character vector of parameter names

- diagnostics:

  List with overall diagnostic metrics for retraining decisions

## Details

This function is specifically designed for the test_17 calibration
workflow. It uses estimate_npe_posterior() for inference and produces
output files compatible with plot_npe_convergence_status().
