# Fit the panel mean-dispersion trend from a configuration

Estimates each location's cases dispersion on the scored window (from
day `burn_in_days + 1`, observed weeks only when the config carries
`reported_tier`), then regresses log k on log mean weekly cases over the
locations with an estimate of their own: finite, not at a bound, not
collapsed. Used to derive `.NB_DISP_PANEL_TREND`.

## Usage

``` r
.nb_disp_panel_trend_fit(config, burn_in_days = 45L)
```

## Arguments

- config:

  A config with `reported_cases`, `date_start`, `location_name` and
  optionally `reported_cases_weight` and `reported_tier`.

- burn_in_days:

  Integer number of leading days left unscored.

## Value

List with `intercept`, `slope`, `sigma` (residual SD of log k), `n` and
the per-location `table`.
