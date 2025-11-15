# Estimate Initial V1 and V2 Compartments for a Location

Estimates the number of individuals in the V1 (one-dose) and V2
(two-dose) vaccination compartments at a given time point (t0) for a
single location. The function tracks vaccination cohorts through time
using a FIFO (First-In-First-Out) queue system, accounts for waning
immunity via exponential decay, incorporates vaccine effectiveness, and
respects operational constraints including vaccination coverage ceilings
and minimum inter-dose intervals.

## Usage

``` r
est_initial_V1_V2_location(
  dates,
  doses,
  t0,
  N = NA,
  vacc_ceiling_frac = 0.6,
  omega1 = 0.003,
  omega2 = 0.003,
  t_lag = 14L,
  min_interdose_days = 40L,
  phi1 = 1,
  phi2 = 1,
  allocation_logic = "FIFO"
)
```

## Arguments

- dates:

  A vector of dates when doses were administered (Date or character that
  can be coerced to Date). Must be the same length as doses.

- doses:

  A numeric vector of doses administered on each date. Must be positive
  values. Must be the same length as dates.

- t0:

  The target date for which to estimate V1 and V2 compartments (Date
  object). Only vaccination events on or before this date are
  considered.

- N:

  Population size(s) for the location. Can be:

  - A single numeric value for constant population

  - A numeric vector (same length as dates) for time-varying population

  - NA if ceiling constraint is not needed

- vacc_ceiling_frac:

  Maximum fraction of population that can receive first doses (numeric
  between 0 and 1). Default is 0.6 (60% coverage ceiling).

- omega1:

  Waning rate for one-dose protection (numeric, positive). Units are
  1/days. Default corresponds to approximately 231-day half-life
  (0.003).

- omega2:

  Waning rate for two-dose protection (numeric, positive). Units are
  1/days. Default equals omega1 but typically should be lower (slower
  waning).

- t_lag:

  Lag time in days for vaccine protection to develop after
  administration (integer, non-negative). Default is 14 days.

- min_interdose_days:

  Minimum number of days required between first and second doses
  (integer, non-negative). Default is 40 days following WHO OCV
  guidelines.

- phi1:

  Vaccine effectiveness for one-dose regimen (proportion developing
  protection). Numeric between 0 and 1. Default is 1.0 (100%
  effectiveness).

- phi2:

  Vaccine effectiveness for two-dose regimen (proportion developing
  protection). Numeric between 0 and 1. Default is 1.0 (100%
  effectiveness).

- allocation_logic:

  Character string specifying second dose allocation method when ceiling
  is reached. Either "FIFO" (First-In-First-Out, oldest cohorts first)
  or "LIFO" (Last-In-First-Out, newest eligible cohorts first). Default
  is "FIFO" for backward compatibility. LIFO is more realistic for
  actual campaigns.

## Value

A named numeric vector with two elements:

- V1:

  Estimated number of effectively protected individuals with one-dose at
  time t0

- V2:

  Estimated number of effectively protected individuals with two-dose at
  time t0

## Details

The function implements a sophisticated vaccine allocation algorithm
that:

**Cohort Tracking:**

- Maintains separate cohorts for first and second doses with timestamps

- Uses FIFO logic to allocate second doses from oldest eligible
  first-dose cohorts

- Second doses decrement V1 and increment V2 (individuals move between
  compartments)

**Constraint Enforcement:**

- Vaccination ceiling: First doses cannot exceed
  `vacc_ceiling_frac * N[i]` at each time point

- Inter-dose interval: Second doses only from cohorts at least
  `min_interdose_days` old

- No same-day second doses (enforced by inter-dose interval)

- Time-varying population: When N is a vector, ceiling adapts to
  population at each vaccination date

**Allocation Priority:**

1.  If V1 at ceiling, prioritize second doses from eligible cohorts

2.  Allocate first doses up to remaining ceiling capacity

3.  Use any remaining doses for additional second doses if possible

4.  Excess doses beyond all constraints are effectively lost

**Second Dose Allocation Logic (when ceiling reached):**

- FIFO: Allocates to oldest eligible cohorts first (traditional
  approach)

- LIFO: Allocates to newest eligible cohorts first (more realistic)

LIFO is recommended as it better reflects real campaign behavior where
recent vaccinees are more accessible for follow-up doses.

**Waning Immunity:** Final V1 and V2 estimates account for exponential
decay of protection: \$\$V_i(t_0) = \sum\_{cohorts} size_j \times
exp(-\omega_i \times (t_0 - date_j - t\_{lag}))\$\$

**Vaccine Effectiveness:** The output represents effectively protected
individuals, not just vaccinated: \$\$V1\_{effective} = V1\_{vaccinated}
\times \phi_1\$\$ \$\$V2\_{effective} = V2\_{vaccinated} \times
\phi_2\$\$ Note: Ceiling constraints operate on administrative coverage
(doses given), not effective coverage.

## References

WHO (2017). Cholera vaccines: WHO position paper. Weekly Epidemiological
Record.

## See also

[`est_vaccination_rate`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_vaccination_rate.md)
for processing vaccination campaign data
[`est_vaccine_effectiveness`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_vaccine_effectiveness.md)
for vaccine efficacy parameters
[`est_immune_decay_vaccine`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_immune_decay_vaccine.md)
for waning rate estimation

## Examples

``` r
if (FALSE) { # \dontrun{
# Create sample vaccination data
dates <- seq.Date(from = as.Date("2023-01-01"),
                  to = as.Date("2023-12-31"),
                  by = "month")
doses <- c(50000, 45000, 40000, 35000, 30000, 25000,
           20000, 15000, 10000, 5000, 3000, 2000)

# Example 1: Constant population
v_const <- est_initial_V1_V2_location(
  dates = dates,
  doses = doses,
  t0 = as.Date("2024-01-01"),
  N = 1000000,
  vacc_ceiling_frac = 0.6,
  omega1 = 0.003,
  omega2 = 0.0015,
  t_lag = 14,
  min_interdose_days = 40,
  phi1 = 0.65,  # 65% one-dose effectiveness
  phi2 = 0.85,  # 85% two-dose effectiveness
  allocation_logic = "FIFO"  # Default
)

# Example 2: Time-varying population (e.g., 1% annual growth)
N_vec <- 1000000 * (1.01 ^ (0:11/12))  # Monthly growth
v_varying <- est_initial_V1_V2_location(
  dates = dates,
  doses = doses,
  t0 = as.Date("2024-01-01"),
  N = N_vec,
  allocation_logic = "LIFO"  # Recent cohorts get second doses first
)

print(v_const)
# V1      V2
# 80246   67074  

print(v_varying) 
# V1      V2
# 65432   67074  # May differ due to population changes
} # }
```
