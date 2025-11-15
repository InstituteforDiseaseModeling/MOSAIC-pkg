# Plot spatial importation hazard

Generates a heatmap of spatial importation hazards over time and across
locations using ggplot2.

Generates a heatmap of spatial importation hazards over time and across
locations using ggplot2.

## Usage

``` r
plot_spatial_hazard(H)
```

## Arguments

- H:

  Numeric matrix of spatial importation hazards (output from
  `calc_spatial_hazard`), with **rows** = locations and **columns** =
  time steps. Column names should be dates in "YYYY-MM-DD" format for
  proper date parsing; row names will be used as location labels.

## Value

Invisibly returns a ggplot object displaying the heatmap.

Invisibly returns a ggplot object displaying the heatmap.
