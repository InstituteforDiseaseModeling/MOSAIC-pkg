# Calculate deaths from symptomatic infections with threshold-dependent IFR

Maps symptomatic infections to deaths using an infection fatality ratio
(IFR) that increases during epidemic periods when incidence exceeds a
threshold. The IFR can also include a temporal trend component. Deaths
can be delayed relative to infections to account for the time between
symptom onset and death.

## Usage

``` r
calc_deaths_from_infections(
  infections,
  N,
  mu_baseline,
  mu_slope = 0,
  mu_epidemic_factor,
  epidemic_threshold,
  delta_t = 0
)
```

## Arguments

- infections:

  Numeric vector of new symptomatic infections over time for one
  location

- N:

  Scalar or vector of population size (can be time-varying)

- mu_baseline:

  Scalar baseline IFR for this location (between 0 and 1)

- mu_slope:

  Scalar temporal slope factor (default 0 for no trend)

- mu_epidemic_factor:

  Scalar proportional increase during epidemic (e.g., 0.5 = 50%
  increase)

- epidemic_threshold:

  Scalar location-specific threshold (infections/N) for epidemic
  definition

- delta_t:

  Non-negative integer number of days for the infection-to-death delay
  (default 0)

## Value

Numeric vector of deaths at each time step

## Details

The time-varying IFR is calculated as: \$\$\mu_t = \mu\_{baseline}
\times (1 + \mu\_{slope} \times t) \times (1 + \mu\_{epidemic} \times
\mathbb{1}\_{epidemic})\$\$

where \\\mathbb{1}\_{epidemic}\\ is 1 when incidence rate exceeds the
threshold, 0 otherwise.

When `delta_t > 0`, deaths at time `t` result from infections at time
`t - delta_t`. The first `delta_t` elements of the output will be `NA`
since no infection data is available before the start of the time
series.

## Examples

``` r
# Simulate an outbreak
infections <- c(10, 20, 50, 100, 80, 40, 20, 10)
deaths <- calc_deaths_from_infections(
  infections = infections,
  N = 100000,
  mu_baseline = 0.01,
  mu_slope = 0,
  mu_epidemic_factor = 0.5,
  epidemic_threshold = 30/100000,
  delta_t = 0
)

# With temporal trend
deaths_trend <- calc_deaths_from_infections(
  infections = infections,
  N = 100000,
  mu_baseline = 0.01,
  mu_slope = 0.1,  # 10% increase over time period
  mu_epidemic_factor = 0.5,
  epidemic_threshold = 30/100000,
  delta_t = 0
)

# With 3-day infection-to-death delay
deaths_delayed <- calc_deaths_from_infections(
  infections = infections,
  N = 100000,
  mu_baseline = 0.01,
  mu_slope = 0,
  mu_epidemic_factor = 0.5,
  epidemic_threshold = 30/100000,
  delta_t = 3
)
```
