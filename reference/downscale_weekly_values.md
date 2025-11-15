# Downscale weekly values to a daily time series

This function takes a weekly time series (start dates and values) and
returns a complete daily time series with values distributed across the
7 days of each week. If `integer = TRUE`, remainders are centered in the
week. Missing weeks or days are filled with NA to ensure completeness.

## Usage

``` r
downscale_weekly_values(date_start, value, integer = TRUE)
```

## Arguments

- date_start:

  A vector of Dates (class "Date") representing the start of each week.
  Must fall on Mondays.

- value:

  A numeric vector of weekly totals (e.g., cases or rainfall).

- integer:

  Logical; if TRUE, treat values as counts and distribute remainder
  symmetrically.

## Value

A data frame with columns: `date`, `value`, one row per day from min to
max date.

## Examples

``` r
downscale_weekly_values(as.Date(c("2023-01-02", "2023-01-16")), c(10, 3), integer = TRUE)
#>          date value
#> 1  2023-01-02     1
#> 2  2023-01-03     1
#> 3  2023-01-04     2
#> 4  2023-01-05     2
#> 5  2023-01-06     2
#> 6  2023-01-07     1
#> 7  2023-01-08     1
#> 8  2023-01-09    NA
#> 9  2023-01-10    NA
#> 10 2023-01-11    NA
#> 11 2023-01-12    NA
#> 12 2023-01-13    NA
#> 13 2023-01-14    NA
#> 14 2023-01-15    NA
#> 15 2023-01-16     0
#> 16 2023-01-17     0
#> 17 2023-01-18     1
#> 18 2023-01-19     1
#> 19 2023-01-20     1
#> 20 2023-01-21     0
#> 21 2023-01-22     0
```
