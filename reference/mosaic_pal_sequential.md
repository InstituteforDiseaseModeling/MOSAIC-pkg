# Generate a Sequential MOSAIC Palette

Interpolates from a stored ramp with monotone luminance for grayscale
safety.

## Usage

``` r
mosaic_pal_sequential(n, palette = "blues", direction = 1, begin = 0, end = 1)
```

## Arguments

- n:

  Number of colors.

- palette:

  Palette name: `"blues"` (default), `"reds"`, `"greens"`, `"teals"`,
  `"heat"`, `"grays"`.

- direction:

  `1` (light to dark) or `-1` (dark to light).

- begin:

  Start position in \[0, 1\] (default 0).

- end:

  End position in \[0, 1\] (default 1).

## Value

Character vector of hex color codes.
