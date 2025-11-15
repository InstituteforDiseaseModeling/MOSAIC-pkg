# Temporal Disaggregation of Annual Cases to Daily Resolution

Disaggregates annual cholera case counts to daily resolution using
Fourier series seasonal patterns. Ensures exact preservation of annual
totals.

## Usage

``` r
disagg_annual_cases_to_daily(annual_cases, years, a1, b1, a2, b2)
```

## Arguments

- annual_cases:

  Vector of annual case counts

- years:

  Vector of years corresponding to cases

- a1:

  First harmonic cosine coefficient

- b1:

  First harmonic sine coefficient

- a2:

  Second harmonic cosine coefficient

- b2:

  Second harmonic sine coefficient

## Value

Data frame with columns: date, year, day_of_year, cases, annual_total
