# Plot recovery durations and rates for symptomatic and asymptomatic infections

This function visualizes the assumed shedding durations and
corresponding recovery rates (1/duration) for symptomatic and
asymptomatic cholera infections using transparent bars and solid mean
lines. Recovery duration is shown on the x-axis.

## Usage

``` r
plot_recovery_duration(
  PATHS = NULL,
  symp_range = c(3, 7),
  asymp_range = c(7, 14)
)
```

## Arguments

- PATHS:

  Optional list containing output paths. If provided, must include
  `DOCS_FIGURES` where plot will be saved.

- symp_range:

  A numeric vector of length 2 indicating the minimum and maximum
  shedding duration (in days) for symptomatic infections. Default is
  c(3, 7).

- asymp_range:

  A numeric vector of length 2 indicating the minimum and maximum
  shedding duration (in days) for asymptomatic infections. Default is
  c(7, 14).

## Value

Invisibly returns the ggplot object. If `PATHS` is supplied, also saves
a PNG.

## Details

If a list `PATHS` is provided with a valid `DOCS_FIGURES` element, the
plot will also be saved as a PNG in that directory as
"recovery_rates.png".

## Examples

``` r
plot_recovery_duration()
#> Error in geom_rect(aes(xmin = min_days, xmax = max_days, ymin = ymin,     ymax = ymax, fill = type), alpha = 0.4, color = NA): could not find function "geom_rect"
plot_recovery_duration(symp_range = c(4, 6), asymp_range = c(8, 12))
#> Error in geom_rect(aes(xmin = min_days, xmax = max_days, ymin = ymin,     ymax = ymax, fill = type), alpha = 0.4, color = NA): could not find function "geom_rect"
```
