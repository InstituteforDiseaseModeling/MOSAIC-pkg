# Estimate Initial R Compartment from Historical Cholera Surveillance

Estimates the initial proportion of the population in the Recovered (R)
compartment based on historical cholera surveillance data. The R
compartment represents individuals with natural immunity from previous
cholera infection, subject to waning immunity.

## Usage

``` r
est_initial_R(
  PATHS,
  priors,
  config,
  n_samples = 1000,
  t0 = NULL,
  disaggregate = TRUE,
  verbose = TRUE,
  parallel = FALSE,
  variance_inflation = 1
)
```

## Arguments

- PATHS:

  List containing paths to data directories

- priors:

  List containing prior distributions for model parameters

- config:

  List containing model configuration including location codes

- n_samples:

  Integer, number of Monte Carlo samples for uncertainty quantification
  (default 1000)

- t0:

  Date object, target date for estimation (default NULL uses current
  date)

- disaggregate:

  Logical, whether to use Fourier disaggregation (TRUE) or mid-year
  point estimate (FALSE)

- verbose:

  Logical, whether to print progress messages (default TRUE)

- parallel:

  Logical, whether to use parallel processing for locations when
  length(location_codes) \>= 8 (default FALSE). Uses
  parallel::mclapply() with all available cores. Note: Not supported on
  Windows.

- variance_inflation:

  Numeric factor to inflate variance of fitted Beta distributions
  (default 1 = no inflation). Values \> 1 increase uncertainty while
  preserving the mean. For example, 2 doubles the variance.

## Value

List with structure matching priors_default for prop_R_initial
parameters

## Examples

``` r
if (FALSE) { # \dontrun{
PATHS <- get_paths()
priors <- priors_default
config <- config_default
initial_R <- est_initial_R(PATHS, priors, config, n_samples = 1000,
                           t0 = as.Date("2024-01-01"), disaggregate = TRUE)
} # }
```
