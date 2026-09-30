# Two-harmonic seasonal human-transmission envelope

`beta_j0_hum * (1 + a1 cos(2 pi t/p) + b1 sin(2 pi t/p) + a2 cos(4 pi t/p) + b2 sin(4 pi t/p))`.

## Usage

``` r
sim_beta_jt_human(par)
```

## Arguments

- par:

  Parameters from
  [`sim_params()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_params.md).

## Value

A `[nticks, npatches]` matrix.

## Details

**`t` is 1-indexed**, not 0-indexed — `t = 1:nticks`. The Python
implementation writes `np.arange(0, nticks) + 1` with the comment "R is
1-indexed, so we start at 1", i.e. it already matches an R convention.
Using `0:(nticks-1)` here would phase-shift the whole seasonal envelope
by one day.

**Calendar phase.** The coefficients are estimated on calendar
day-of-year
([`est_seasonal_dynamics()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_seasonal_dynamics.md),
t = 1 is 1 January), so tick `i` is evaluated at
`t = par$season_t0 + i`, where `season_t0` is the day-of-year of
`date_start` minus one (set by
[`sim_params()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/sim_params.md);
0 for a 1 January start, and taken as 0 when absent).
