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

  Multiplicative CI factor for the Beta refit (default 2): the Beta
  keeps the Monte Carlo mean and its spread is fit to the target 95% CI
  mean / VI to mean \* VI. A scalar or a named per-ISO vector. Should be
  \> 1.1 for meaningful variance.

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

The method back-calculates symptom onsets from reported cases through
the engine's reporting chain and maps them to E/I stocks at t0 (see
[`est_initial_E_I_location`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_initial_E_I_location.md)).
Each Monte Carlo draw samples `sigma`, `iota`, `gamma_1`, `gamma_2`,
`rho`, `chi_endemic` and `delta_reporting_cases` from
`priors$parameters_global` (a missing prior is replaced by a fixed value
with a warning); the parallel and sequential branches run the same draw
function. Draws with E or I = 0 are kept in the mean. Locations with no
usable surveillance in the window (no rows, or every case count NA) get
the near-zero Beta(0.01, 99999.99) template, the same prior as a window
that reports zero cases throughout: absent surveillance is not evidence
of active infection at t0, so it never seeds more E/I than confirmed
zeros. Locations with surveillance but too few usable draws, or an
estimation error, get the fallback Beta priors (Beta(1, 9999) for E,
Beta(0.5, 9999.5) for I).

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
