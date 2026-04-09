# Get MOSAIC Semantic Colors

Returns named hex color codes for recurring MOSAIC quantities. Use these
for consistent coloring across all package visualizations.

## Usage

``` r
mosaic_colors(...)
```

## Arguments

- ...:

  Color names to retrieve. If empty, returns all core semantic colors.
  Special set names: `"core"`, `"compartments"`, `"calibration"`,
  `"status"`, `"param_groups"`, `"all"`.

## Value

Named character vector of hex color codes.

## Details

The core semantic colors are:

- `mosaic_blue` (`#0167AF`): Primary identity color, model estimates

- `data` (`#2D2D2D`): Observed data (near-black, always background
  layer)

- `forecast` (`#4A90C9`): Forecast extensions (lighter blue)

- `cases` (`#0167AF`): Cholera cases (same as mosaic_blue)

- `deaths` (`#B5123B`): Deaths (burgundy-crimson)

- `environmental` (`#009988`): Environmental reservoir, WASH (teal)

- `vaccination` (`#228833`): Vaccination/OCV (green)

- `reference` (`#888888`): Reference lines, baselines, priors (gray)

- `highlight` (`#EE6677`): Emphasis, alerts (coral-red)

- `mobility` (`#CCBB44`): Human mobility (gold)

Compartment colors (S/E/I/R/V1/V2/W) are accessed via
`mosaic_colors("compartments")` or by name (e.g., `mosaic_colors("S")`).

## Examples

``` r
mosaic_colors()                            # All core colors
#>   mosaic_blue          data      forecast         cases        deaths 
#>     "#0167AF"     "#2D2D2D"     "#4A90C9"     "#0167AF"     "#B5123B" 
#> environmental   vaccination     reference     highlight      mobility 
#>     "#009988"     "#228833"     "#888888"     "#EE6677"     "#CCBB44" 
mosaic_colors("cases", "deaths")           # Specific colors
#>     cases    deaths 
#> "#0167AF" "#B5123B" 
mosaic_colors("compartments")              # SEIR compartment colors
#>         S         E         I         R        V1        V2         W 
#> "#4477AA" "#EE7733" "#CC3311" "#CCBB44" "#66AA44" "#228833" "#009988" 
mosaic_colors("S", "E", "I", "R")          # Subset of compartments
#>         S         E         I         R 
#> "#4477AA" "#EE7733" "#CC3311" "#CCBB44" 
mosaic_colors("calibration")               # Prior (gray) + Posterior (blue)
#>     Prior Posterior 
#> "#888888" "#0167AF" 
mosaic_colors("status")                    # pass/warn/fail/info
#>      pass      warn      fail      info 
#> "#228833" "#CCBB44" "#CC3311" "#0167AF" 
mosaic_colors("param_groups")              # Parameter group colors
#>        Transmission Disease Progression         Vaccination       Environmental 
#>           "#0167AF"           "#EE7733"           "#228833"           "#009988" 
#>         Observation            Mobility         Seasonality         Suitability 
#>           "#888888"           "#CCBB44"           "#66CCEE"           "#AA3377" 
```
