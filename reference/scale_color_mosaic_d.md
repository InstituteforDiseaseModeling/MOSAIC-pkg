# MOSAIC Discrete Color/Fill Scales for ggplot2

Apply the MOSAIC qualitative palette to discrete aesthetics.

## Usage

``` r
scale_color_mosaic_d(palette = "default", direction = 1, alpha = 1, ...)

scale_colour_mosaic_d(palette = "default", direction = 1, alpha = 1, ...)

scale_fill_mosaic_d(palette = "default", direction = 1, alpha = 1, ...)
```

## Arguments

- palette:

  Palette name: `"default"` or `"extended"`.

- direction:

  `1` or `-1`.

- alpha:

  Transparency (0-1).

- ...:

  Passed to
  [`discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

## Value

A ggplot2 scale object.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(x, y, color = group)) + geom_point() + scale_color_mosaic_d()
} # }
```
