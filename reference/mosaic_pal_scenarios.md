# Generate a Scenario Comparison Palette

Returns a dedicated palette for comparing intervention scenarios. The
baseline scenario always receives MOSAIC blue (position 1). Scales from
2 to 16 curated colors, then generates in HCL for larger n.

## Usage

``` r
mosaic_pal_scenarios(n, labels = NULL)
```

## Arguments

- n:

  Number of scenarios (including baseline).

- labels:

  Optional character vector of scenario names.

## Value

Named character vector of hex color codes.

## Examples

``` r
mosaic_pal_scenarios(4)
#>   Baseline Scenario_2 Scenario_3 Scenario_4 
#>  "#0167AF"  "#EE6677"  "#228833"  "#CCBB44" 
mosaic_pal_scenarios(4, labels = c("Baseline", "Vaccination", "WASH", "Combined"))
#>    Baseline Vaccination        WASH    Combined 
#>   "#0167AF"   "#EE6677"   "#228833"   "#CCBB44" 
mosaic_pal_scenarios(12)
#>    Baseline  Scenario_2  Scenario_3  Scenario_4  Scenario_5  Scenario_6 
#>   "#0167AF"   "#EE6677"   "#228833"   "#CCBB44"   "#66CCEE"   "#AA3377" 
#>  Scenario_7  Scenario_8  Scenario_9 Scenario_10 Scenario_11 Scenario_12 
#>   "#EE7733"   "#009988"   "#332288"   "#CC79A7"   "#DDCC77"   "#88CCEE" 
```
