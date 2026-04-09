# MOSAIC Sequential Color/Fill Scales for ggplot2

Apply a MOSAIC sequential palette to continuous aesthetics.

## Usage

``` r
scale_color_mosaic_c(palette = "blues", direction = 1, begin = 0, end = 1, ...)

scale_colour_mosaic_c(
  palette = "blues",
  direction = 1,
  begin = 0,
  end = 1,
  ...
)

scale_fill_mosaic_c(palette = "blues", direction = 1, begin = 0, end = 1, ...)
```

## Arguments

- palette:

  Palette name: `"blues"`, `"reds"`, `"greens"`, `"teals"`, `"heat"`,
  `"grays"`.

- direction:

  `1` (light to dark) or `-1`.

- begin:

  Start position in \[0, 1\].

- end:

  End position in \[0, 1\].

- ...:

  Passed to
  [`scale_colour_gradientn`](https://ggplot2.tidyverse.org/reference/scale_gradient.html).

## Value

A ggplot2 scale object.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(df, aes(x, y, fill = value)) + geom_tile() + scale_fill_mosaic_c("heat")
} # }
```
