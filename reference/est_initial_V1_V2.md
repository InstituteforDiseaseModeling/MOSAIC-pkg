# Estimate Initial V1 and V2 Compartments for All Locations

Estimates initial V1 (one-dose) and V2 (two-dose) vaccination
compartment values for all locations specified in a config, with
uncertainty quantification through Monte Carlo sampling from prior
distributions. Returns results formatted as a priors-like object with
Beta distribution parameters for V1/N and V2/N proportions.

## Usage

``` r
est_initial_V1_V2(
  PATHS,
  priors,
  config,
  n_samples = 1000,
  t0 = NULL,
  verbose = TRUE,
  parallel = FALSE,
  variance_inflation = 1
)
```

## Arguments

- PATHS:

  List object from get_paths() containing data directory paths

- priors:

  Prior distributions object (e.g., from priors_default or custom)

- config:

  Configuration object containing location names (location_name or
  location_codes) and parameters

- n_samples:

  Integer, number of Monte Carlo samples for uncertainty (default 1000)

- t0:

  Date object for initial condition estimation (default from
  config\$date_start or config\$t0)

- verbose:

  Logical, whether to print progress messages (default TRUE)

- parallel:

  Logical, whether to use parallel processing (default FALSE). When
  TRUE, automatically detects cores and handles platform differences.

- variance_inflation:

  Numeric factor to inflate variance of fitted Beta distributions
  (default 1 = no inflation). Values \> 1 increase uncertainty while
  preserving the mean. For example, 2 doubles the variance.

## Value

A list with priors_default-compatible structure containing:

- metadata:

  Information about the estimation run including summary statistics

- parameters_location:

  Parameter-first hierarchy matching priors_default:

  - prop_V1_initial: Beta distribution parameters for V1/N by location

  - prop_V2_initial: Beta distribution parameters for V2/N by location

  Each parameter contains \$parameters\$location\$ISO_CODE with shape1,
  shape2, and metadata

## Details

The function performs the following steps for each location:

1.  Loads vaccination and population data

2.  Runs Monte Carlo simulation sampling from priors for omega and phi

3.  Calculates V1 and V2 for each sample

4.  Fits Beta distributions to V1/N and V2/N proportions

5.  Compiles results in standardized format

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard usage
PATHS <- get_paths()
initial_conditions <- est_initial_V1_V2(
  PATHS = PATHS,
  priors = priors_default,
  config = config_example,
  n_samples = 1000
)

# Access results for a location (priors_default-compatible structure)
eth_v1_params <- initial_conditions$parameters_location$prop_V1_initial$parameters$location$ETH
eth_v1_shape1 <- eth_v1_params$shape1
eth_v1_mean <- eth_v1_params$metadata$mean
} # }
```
