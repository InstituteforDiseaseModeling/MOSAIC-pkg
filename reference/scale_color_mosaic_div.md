# MOSAIC Diverging Color/Fill Scales for ggplot2

Apply a MOSAIC diverging palette to continuous aesthetics.

## Usage

``` r
scale_color_mosaic_div(palette = "blue_red", direction = 1, ...)

scale_colour_mosaic_div(palette = "blue_red", direction = 1, ...)

scale_fill_mosaic_div(palette = "blue_red", direction = 1, ...)
```

## Arguments

- palette:

  Palette name: `"blue_red"`, `"blue_orange"`, `"green_purple"`.

- direction:

  `1` or `-1`.

- ...:

  Passed to
  [`scale_colour_gradientn`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Value

A ggplot2 scale object.
