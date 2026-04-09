# Create Lighter, Darker, or Muted Variants of a Color

Manipulates colors in RGB space. Useful for creating secondary line
colors, muted backgrounds, or custom uncertainty shades.

## Usage

``` r
mosaic_color_variant(col, mode = c("lighten", "darken", "mute"), amount = 0.3)
```

## Arguments

- col:

  One or more hex colors.

- mode:

  Adjustment mode: `"lighten"`, `"darken"`, or `"mute"`.

- amount:

  Adjustment amount (0-1, default 0.3).

## Value

Character vector of adjusted hex colors.

## Examples

``` r
mosaic_color_variant("#0167AF", "lighten", 0.4)
#> [1] "#67A4CF"
mosaic_color_variant("#B5123B", "darken", 0.3)
#> [1] "#7F0D29"
mosaic_color_variant(mosaic_colors("cases", "deaths"), "mute", 0.5)
#> [1] "#2C5F83" "#762539"
```
