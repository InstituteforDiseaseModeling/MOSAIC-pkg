# Display a Color Palette as Swatches

Renders colored rectangles with hex codes for visual inspection and QA.
Works with any character vector of hex colors.

## Usage

``` r
plot_mosaic_palette(colors, labels = TRUE, main = NULL)
```

## Arguments

- colors:

  Character vector of hex colors (named or unnamed).

- labels:

  If `TRUE` (default), show hex codes and names below each swatch.

- main:

  Optional plot title.

## Value

Invisible `NULL`. Called for its side effect (plot).

## Examples

``` r
plot_mosaic_palette(mosaic_colors())

plot_mosaic_palette(mosaic_pal_sequential(9, "blues"), main = "Blues")

plot_mosaic_palette(mosaic_colors("compartments"), main = "SEIR")

```
