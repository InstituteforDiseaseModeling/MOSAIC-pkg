# Generate a Qualitative MOSAIC Palette

Returns a set of perceptually distinct, CVD-safe colors for categorical
data. For n \<= 7, uses the curated default palette. For n \<= 12, uses
the extended palette. For n \> 12, generates colors in HCL space.

## Usage

``` r
mosaic_pal_discrete(n, palette = "default", direction = 1, alpha = 1)
```

## Arguments

- n:

  Number of colors.

- palette:

  Palette name: `"default"` (7 colors) or `"extended"` (12 colors).

- direction:

  `1` (default order) or `-1` (reversed).

- alpha:

  Transparency (0-1, default 1).

## Value

Character vector of hex color codes.
