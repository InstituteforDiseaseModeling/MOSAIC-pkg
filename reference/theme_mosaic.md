# MOSAIC ggplot2 Theme

A clean, minimal theme designed for publication-quality scientific
figures. White background, light gridlines, appropriate text sizing.

## Usage

``` r
theme_mosaic(base_size = 11, base_family = "", grid = "major")
```

## Arguments

- base_size:

  Base font size (default 11).

- base_family:

  Base font family (default `""`).

- grid:

  Grid lines to show: `"major"` (default), `"none"`, or `"both"`.

## Value

A ggplot2 theme object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
ggplot(data, aes(x, y)) + geom_point() + theme_mosaic()
} # }
```
