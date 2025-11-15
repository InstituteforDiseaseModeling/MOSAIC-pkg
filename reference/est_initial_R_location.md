# Estimate Initial R Compartment for Single Location

Estimates the recovered population at time t0 for a single location
based on historical infection data, accounting for waning immunity.

## Usage

``` r
est_initial_R_location(
  cases,
  dates,
  population,
  t0,
  epsilon,
  sigma = 0.01,
  rho = 0.1,
  chi = 0.5,
  iota,
  gamma_1,
  gamma_2
)
```

## Arguments

- cases:

  Vector of reported suspected cholera cases

- dates:

  Vector of dates corresponding to cases

- population:

  Population size at t0

- t0:

  Target date for estimation

- epsilon:

  Waning immunity rate (per day)

- sigma:

  Proportion of infections that are symptomatic

- rho:

  Reporting rate of symptomatic cases

- chi:

  Diagnostic positivity rate

- iota:

  Incubation rate (per day)

- gamma_1:

  Symptomatic recovery rate (per day)

- gamma_2:

  Asymptomatic recovery rate (per day)

## Value

Numeric value of R compartment size at t0
