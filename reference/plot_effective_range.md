# Plot Effective Range Function

Visualizes the relationship between top percentile and effective AIC
range

## Usage

``` r
plot_effective_range(percentiles = seq(0.5, 30, by = 0.5))
```

## Arguments

- percentiles:

  Numeric vector of percentiles to evaluate (default: 0.5 to 30)

## Value

A plot showing the effective AIC range as a function of percentile

## Examples

``` r
if (FALSE) { # \dontrun{
plot_effective_range()
plot_effective_range(percentiles = seq(1, 20, by = 1))
} # }
```
