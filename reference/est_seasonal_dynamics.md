# Estimate Seasonal Dynamics for Cholera and Precipitation Using Daily Fourier Series

This function retrieves historical precipitation data, processes cholera
case data, and fits seasonal dynamics models using a double Fourier
series at the daily scale (p = 365). Weekly data is expanded by
assigning each weekly value to every day in that week.

## Usage

``` r
est_seasonal_dynamics(
  PATHS,
  date_start,
  date_stop,
  min_obs = 30,
  clustering_method,
  k,
  exclude_iso_codes = NULL,
  data_sources = c("WHO", "JHU", "SUPP"),
  envelope_floor = 0.1
)
```

## Arguments

- PATHS:

  A list containing paths where raw and processed data are stored. See
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- date_start:

  A date in YYYY-MM-DD format indicating the start date of the
  precipitation data.

- date_stop:

  A date in YYYY-MM-DD format indicating the stop date of the
  precipitation data.

- min_obs:

  The minimum number of observations required to fit the Fourier series
  to cholera case data.

- clustering_method:

  The clustering method to use (e.g., "kmeans", "ward.D2", etc.).

- k:

  Number of clusters for grouping countries by seasonality.

- exclude_iso_codes:

  Optional character vector of ISO codes to exclude from clustering and
  neighbor matching.

- data_sources:

  Character vector of data sources to include. Default is c('WHO',
  'JHU', 'SUPP').

- envelope_floor:

  Minimum allowed value of the human-transmission envelope 1 + f(t) over
  the year (default 0.1); case-fit coefficients whose envelope dips
  lower are shrunk towards zero.

## Value

Saves daily fitted values and parameter estimates to CSV.

## Details

**Phase convention.** The coefficients are fit against calendar
day-of-year
([`lubridate::yday()`](https://lubridate.tidyverse.org/reference/day.html),
t = 1 is 1 January) with `p = 365`. The engine
([`sim_beta_jt_human()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_beta_jt_human.md))
evaluates the envelope at the day-of-year of each simulated day, so the
coefficients keep this meaning whatever the simulation `date_start`.

**Weekly alignment.** Precipitation is summed over the same ISO weeks
(Monday to Sunday) as the weekly surveillance file and merged on the
week's start date; weeks with fewer than 7 days of precipitation at the
edges of the window are dropped.

**Positivity of the envelope.** The engine uses the case-fit
coefficients as a multiplicative modulation of the mean human
transmission rate, `beta_j0_hum * (1 + f(t))`. A Fourier fit to
affine-normalised weekly cases has no such constraint and can dip below
-1, which the engine clamps to zero transmission for weeks at a time.
Multiplicative seasonal forcing requires the envelope to stay positive
(amplitude below 1 in `beta(t) = beta0 (1 + beta1 cos(wt))`; Keeling &
Rohani 2008, *Modeling Infectious Diseases in Humans and Animals*,
section 5.2; King et al. 2008, *Nature* 454:877, fit cholera seasonality
on the log scale for the same reason). When
`min_t(1 + f(t)) < envelope_floor` the four case coefficients (and their
SE and CI) are multiplied by the single factor
`(1 - envelope_floor) / -min_t f(t)`, which keeps the fitted phase and
the relative shape of the season and lowers only its amplitude. The
factor applied is recorded in the `envelope_scale` column (1 =
unchanged). `envelope_floor` is a numerical positivity margin, not an
estimated trough: it leaves room for prior draws around the mean
(make_priors_default widens the SE) without crossing zero.
