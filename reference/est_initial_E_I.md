# Estimate Initial E and I Compartments from Surveillance Data

This function estimates the initial number of individuals in the Exposed
(E) and Infected (I) compartments at model start time using recent
surveillance data through a Monte Carlo simulation approach.

## Usage

``` r
est_initial_E_I(
  PATHS,
  priors,
  config,
  n_samples = 1000,
  t0 = NULL,
  lookback_days = 21,
  verbose = TRUE,
  parallel = FALSE,
  variance_inflation = 2
)
```

## Arguments

- PATHS:

  List of paths from
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- priors:

  Prior distributions for parameters (e.g., `priors_default`).

- config:

  Configuration object with location codes and `date_start`. Must
  include `config$location_name` and `config$date_start`.

- n_samples:

  Number of Monte Carlo samples (default 1000).

- t0:

  Target date for estimation (default from `config$date_start`).

- lookback_days:

  Days of surveillance data to use (default 21).

- verbose:

  Print progress messages (default TRUE).

- parallel:

  Enable parallel processing for Monte Carlo sampling when
  `n_samples >= 100` (default FALSE). Uses
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)
  with all available cores. Note: Not supported on Windows.

- variance_inflation:

  Factor to create variance in Beta distributions (default 2). Sets
  ci_lower = mean_val / variance_inflation and ci_upper = mean_val \*
  variance_inflation. Values \> 1 create wider distributions around the
  sample mean. Should be \> 1.1 for meaningful variance.

## Value

A list with two main components:

- metadata:

  List containing estimation details: description, version, date, t0,
  lookback_days, n_samples, and method.

- parameters_location:

  List with `prop_E_initial` and `prop_I_initial`, each containing:

  - parameter_name: Parameter identifier

  - distribution: `"beta"`

  - parameters\$location: Named list by ISO code with `shape1` and
    `shape2`

## Details

The method back-calculates true infections from reported cases using the
surveillance cascade, accounts for reporting delays, and estimates E/I
compartments based on epidemiological progression rates. Includes
comprehensive parameter validation, numerical stability protections, and
optional parallel processing.

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS  <- get_paths()
priors <- priors_default
config <- config_default
results <- est_initial_E_I(
  PATHS, priors, config,
  n_samples = 1000,
  variance_inflation = 2           # Factor for expanding CI bounds around sample mean
)
} # }
```
