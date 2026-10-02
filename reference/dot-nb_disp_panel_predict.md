# Dispersion predicted by a panel mean-dispersion trend

Dispersion predicted by a panel mean-dispersion trend

## Usage

``` r
.nb_disp_panel_predict(trend, mean_weekly)
```

## Arguments

- trend:

  List with `intercept` and `slope`.

- mean_weekly:

  Numeric vector of positive weekly means.

## Value

`exp(intercept + slope * log(mean_weekly))`, held inside the numerical
bounds.
