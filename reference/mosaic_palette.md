# Generate a MOSAIC Color Palette

Dispatches to the appropriate palette generator based on type.

## Usage

``` r
mosaic_palette(
  n,
  type = c("discrete", "sequential", "diverging"),
  name = NULL,
  direction = 1,
  alpha = 1
)
```

## Arguments

- n:

  Number of colors needed.

- type:

  Palette type: `"discrete"`, `"sequential"`, or `"diverging"`.

- name:

  Palette name within type. For sequential: `"blues"`, `"reds"`,
  `"greens"`, `"teals"`, `"heat"`, `"grays"`. For diverging:
  `"blue_red"`, `"blue_orange"`, `"green_purple"`.

- direction:

  `1` (default) or `-1` to reverse.

- alpha:

  Transparency (0-1, default 1).

## Value

Character vector of hex color codes.
