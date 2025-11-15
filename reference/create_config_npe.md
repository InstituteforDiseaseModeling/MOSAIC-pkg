# Create NPE Configuration

Creates a configuration file using posterior median values from NPE.
Automatically handles location-specific parameters by stripping location
suffixes (e.g., "tau_i_ETH" -\> "tau_i") when matching to config fields.
Supports both single-location and multi-location configs.

## Usage

``` r
create_config_npe(
  posterior_result,
  config_base,
  output_file = NULL,
  use_median = TRUE,
  verbose = TRUE,
  debug = FALSE
)
```

## Arguments

- posterior_result:

  NPE posterior result object containing:

  - samples - Matrix of posterior samples (with location suffixes)

  - quantiles - Optional data frame with q0.5 column for medians

  - n_samples - Number of posterior samples

- config_base:

  Base configuration to update. Location-specific parameters should be
  named without location suffixes (e.g., "tau_i" not "tau_i_ETH")

- output_file:

  Path to save config JSON (NULL = don't save)

- use_median:

  Use median (TRUE) or mean (FALSE) for point estimates

- verbose:

  Print progress messages including number of parameters updated

## Value

Updated configuration list with npe_metadata including:

- n_parameters_updated - Number of parameters successfully updated

- n_parameters_skipped - Number of parameters not found in config

## Details

For single-location configs, location-specific parameters are stored as
scalars. For multi-location configs, they are stored as vectors ordered
by location_name.
