# Check for overdispersion in a count time series

This function checks for overdispersion in a univariate count time
series using: (1) a Poisson GLM with intercept only, and (2) an optional
Poisson GLM with 2-harmonic seasonal Fourier terms based on day-of-year.

## Usage

``` r
check_overdispersion(
  y,
  date = NULL,
  seasonality = FALSE,
  period = 365,
  print = TRUE
)
```

## Arguments

- y:

  A numeric vector of non-negative integer counts (e.g., cases or
  deaths).

- date:

  A vector of class `Date` corresponding to `y`. Required if
  `seasonality = TRUE`.

- seasonality:

  Logical; if `TRUE`, also fits a GLM with 2-harmonic seasonal terms.
  Default = `FALSE`.

- period:

  Integer; seasonal period (default = 365 for daily data).

- print:

  Logical; if `TRUE`, prints a summary of results. Default = `TRUE`.

## Value

A list with dispersion metrics:

- dispersion_ratio:

  Variance-to-mean ratio

- pearson_dispersion_intercept:

  Pearson statistic for intercept-only model

- pearson_dispersion_seasonal:

  Pearson statistic for seasonal model (if applicable)

- overdispersed_intercept:

  Logical; TRUE if intercept model dispersion \> 1.5

- overdispersed_seasonal:

  Logical; TRUE if seasonal model dispersion \> 1.5 (if applicable)

- n_obs:

  Number of non-missing observations

## Details

Overdispersion is assessed using the Pearson dispersion statistic and
the variance-to-mean ratio. If the Pearson statistic exceeds 1.5, the
series is flagged as overdispersed.

## Examples

``` r
if (FALSE) { # \dontrun{
  check_overdispersion(y = c(0, 2, 1, 0, 1, 3, 2, 0))
  dates <- as.Date("2023-03-01") + 0:13
  cases <- c(0, 1, 2, 0, 2, 4, 3, 1, 0, 5, 2, 3, 1, 0)
  check_overdispersion(y = cases, date = dates, seasonality = TRUE)
} # }
```
