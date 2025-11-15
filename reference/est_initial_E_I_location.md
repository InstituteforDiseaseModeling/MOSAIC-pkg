# Estimate E and I Compartments for a Single Location

This function performs the actual E/I estimation for a single location
using surveillance data and epidemiological parameters. It
back-calculates true infections from reported cases using the
surveillance cascade, accounts for infection-to-report delays, and
estimates current E and I compartments based on disease progression.

## Usage

``` r
est_initial_E_I_location(
  cases,
  dates,
  population,
  t0,
  lookback_days = 60,
  sigma,
  rho,
  chi,
  tau_r,
  iota,
  gamma_1,
  gamma_2,
  verbose = FALSE
)
```

## Arguments

- cases:

  Vector of daily suspected cholera cases (must be same length as dates)

- dates:

  Vector of dates corresponding to cases (Date class)

- population:

  Total population of the location (must be positive)

- t0:

  Target date for estimation (Date class)

- lookback_days:

  Days of data to use (default 60, must be positive)

- sigma:

  Symptomatic proportion (must be in (0,1\])

- rho:

  Reporting rate - proportion of symptomatic cases reported (must be in
  (0,1\])

- chi:

  Diagnostic positivity - proportion of suspected cases that are true
  cholera (must be in (0,1\])

- tau_r:

  Reporting delay in days from symptom onset to report (must be
  non-negative)

- iota:

  Incubation rate (1/incubation period, must be positive)

- gamma_1:

  Symptomatic recovery rate (must be positive)

- gamma_2:

  Asymptomatic recovery rate (must be positive)

- verbose:

  Print detailed progress (default FALSE)

## Value

A list with two components:

- E:

  Number of individuals in Exposed compartment (non-negative numeric)

- I:

  Number of individuals in Infected compartment (non-negative numeric)

Returns E=0, I=0 if no cases in lookback window. Includes numerical
stability protections and parameter validation. Warns if E or I exceed
2% of population.

## Details

The function includes comprehensive parameter validation, numerical
stability protections for exponential calculations, and detailed
progress reporting when verbose=TRUE. Uses exact infectiousness kernels
for the I compartment estimation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with synthetic data
cases <- rpois(60, lambda = 5)
dates <- seq(as.Date("2023-01-01"), by = "day", length.out = 60)
result <- est_initial_E_I_location(
  cases = cases,
  dates = dates,
  population = 1000000,
  t0 = as.Date("2023-03-01"),
  sigma = 0.125,
  rho = 0.1,
  chi = 0.5,
  tau_r = 4,
  iota = 0.714,
  gamma_1 = 0.2,
  gamma_2 = 0.67
)
} # }
```
