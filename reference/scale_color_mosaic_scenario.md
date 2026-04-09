# MOSAIC Scenario Color/Fill Scales for ggplot2

Apply the dedicated MOSAIC scenario palette to discrete aesthetics.
Baseline is always MOSAIC blue (position 1).

## Usage

``` r
scale_color_mosaic_scenario(...)

scale_colour_mosaic_scenario(...)

scale_fill_mosaic_scenario(...)
```

## Arguments

- ...:

  Passed to
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

## Value

A ggplot2 scale object.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(x, y, color = scenario)) +
  geom_line() + scale_color_mosaic_scenario()
} # }
```
