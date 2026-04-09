# Generate Colors for MOSAIC SSA Countries

Assigns colors to the 40 MOSAIC countries grouped by WHO sub-region.
Within each region, hues are evenly spaced at constant chroma and
luminance. This ensures geographically close countries have similar but
distinguishable colors.

## Usage

``` r
mosaic_pal_countries(iso_codes = NULL, group_by_region = TRUE)
```

## Arguments

- iso_codes:

  Character vector of ISO3 codes to include. Default: all 40 MOSAIC
  countries.

- group_by_region:

  If `TRUE` (default), assign hue ranges by WHO sub-region. If `FALSE`,
  use simple HCL rotation for all countries.

## Value

Named character vector keyed by ISO3 code.

## Examples

``` r
mosaic_pal_countries()
#>       BEN       BFA       CIV       GHA       GIN       GMB       GNB       LBR 
#> "#BA6D7D" "#B96F77" "#B87070" "#B6726A" "#B47363" "#B1755C" "#AE7756" "#AB794F" 
#>       MLI       MRT       NER       NGA       SEN       SLE       TGO       BDI 
#> "#A77B49" "#A37D43" "#9F7F3E" "#9A813A" "#958337" "#8F8535" "#898734" "#788C3A" 
#>       ERI       ETH       KEN       MWI       RWA       SOM       SSD       TZA 
#> "#6E8E3F" "#648F46" "#58914E" "#4B9357" "#3C945F" "#299568" "#099671" "#009679" 
#>       UGA       AGO       CAF       CMR       COD       COG       GAB       GNQ 
#> "#009681" "#009593" "#00949A" "#0093A0" "#0091A6" "#1B8FAC" "#358DB0" "#498AB4" 
#>       MOZ       TCD       BWA       NAM       SWZ       ZAF       ZMB       ZWE 
#> "#5987B7" "#6884B9" "#847CBA" "#9577B7" "#A371B2" "#AE6EAB" "#B56BA1" "#B96B95" 
mosaic_pal_countries(c("ETH", "KEN", "TZA"))
#>       ETH       KEN       TZA 
#> "#648F46" "#58914E" "#009679" 
```
