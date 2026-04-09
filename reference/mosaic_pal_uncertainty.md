# Generate Graduated Uncertainty Colors from a Base Color

Produces progressively lighter/desaturated variants of the base color
for layered credible interval ribbons. The standard levels are 50\\

## Usage

``` r
mosaic_pal_uncertainty(base_color = "#0167AF", n_levels = 3, labels = NULL)
```

## Arguments

- base_color:

  Hex color for the point estimate (default: MOSAIC blue).

- n_levels:

  Number of uncertainty levels (default 3).

- labels:

  Character vector of level names. Default: `c("50%", "75%", "95%")`.

## Value

Named character vector of hex color codes for ribbon fills.

## Details

Uncertainty is never a standalone color. It is always derived from the
point estimate color.

## Examples

``` r
# Cases (blue)
mosaic_pal_uncertainty()
#>       50%       75%       95% 
#> "#3B86BC" "#73A6CB" "#A9C7DC" 

# Deaths (burgundy)
mosaic_pal_uncertainty("#B5123B")
#>       50%       75%       95% 
#> "#BF4664" "#CB798E" "#DCACB8" 

# Environmental (teal)
mosaic_pal_uncertainty("#009988")
#>       50%       75%       95% 
#> "#3CADA0" "#75C2B9" "#ABD8D3" 

# Custom levels
mosaic_pal_uncertainty(n_levels = 5,
  labels = c("10%", "25%", "50%", "75%", "95%"))
#>       10%       25%       50%       75%       95% 
#> "#3B86BC" "#5796C3" "#73A6CB" "#8EB7D3" "#A9C7DC" 
```
