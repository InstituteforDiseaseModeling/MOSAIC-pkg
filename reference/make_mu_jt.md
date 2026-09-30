# Build the daily reported-CFR matrix mu_jt from annual estimates

Expands per-location, per-year reported case fatality ratios (from
[`est_CFR_hierarchical`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_CFR_hierarchical.md))
into the \[location x day\] matrix the engine reads as `config$mu_jt`.
Values are interpolated linearly on the logit scale between mid-year
points, and held flat before the first and after the last estimated
year.

## Usage

``` r
make_mu_jt(
  cfr_estimates,
  location_name,
  date_start,
  date_stop,
  interpolation = c("linear_logit", "step")
)
```

## Arguments

- cfr_estimates:

  Data frame with columns `iso_code`, `year` and either `logit_mean` or
  `cfr_estimate` (for example `est_CFR_hierarchical()$predictions`, or
  the `cfr_hierarchical_estimates.csv` it writes).

- location_name:

  Character vector of ISO codes, in config row order.

- date_start, date_stop:

  First and last simulation dates (Date or character).

- interpolation:

  `"linear_logit"` (default) or `"step"` (the calendar-year value
  applies to every day of that year).

## Value

Numeric matrix with `length(location_name)` rows and one column per day
from `date_start` to `date_stop`; every value in (0, 1).

## Details

Days past the last estimated year carry that year's value forward, so a
simulation window that runs beyond the WHO annual record is filled with
the most recent estimate rather than an extrapolated trend.

The values are the GAM's logit-scale centres, i.e. the median of each
year's reported-CFR distribution, not its mean. That is the exact prior
centre for the integrated deaths likelihood (whose year deviations have
mean zero on the logit scale). A simulation that draws deaths directly
at `mu_jt` – a scenario run or a prior-predictive check – runs 5-10%
below WHO-annual deaths (2018-25); the logit-normal mean would
over-predict, because annual CFR is lower in high-case years.

## See also

[`est_CFR_hierarchical`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_CFR_hierarchical.md)

## Examples

``` r
est <- data.frame(iso_code = rep(c("AAA", "BBB"), each = 3),
                  year = rep(2023:2025, 2),
                  cfr_estimate = c(0.02, 0.025, 0.03, 0.01, 0.01, 0.012))
mu <- make_mu_jt(est, c("AAA", "BBB"), "2023-01-01", "2025-12-31")
dim(mu)
#> [1]    2 1096
```
