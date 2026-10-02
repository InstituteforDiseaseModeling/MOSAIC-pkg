# Reporting weeks of a daily grid

The one definition of the reporting weeks of a daily grid, used by the
weekly cases likelihood
(`calc_model_likelihood(cases_scoring = "weekly")`) and the
observation-level posterior predictive
([`calc_model_ensemble()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/calc_model_ensemble.md),
under either cases scoring rule), on the block formula
[`.nb_disp_block()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/dot-nb_disp_block.md)
that the dispersion estimate
([`est_nb_dispersion()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md))
and the integrated deaths likelihood aggregate with, so all of them sum
the same days. MOSAIC surveillance weeks run Monday to Sunday: processed
weekly rows are dated by their Monday,
[`downscale_weekly_values()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/downscale_weekly_values.md)
spreads each total over that Monday to Sunday, so the daily totals of
every complete week sum to the processed weekly total the config was
built from: exactly where that total is a whole count, and within half a
case where it is a fractional imputed total (79 weeks in config_default
v6.1). Blocks are counted from a fixed Monday (1970-01-05), never from
the first day of the grid: config_default starts on a Sunday, whose
block is a one-day partial week.

## Usage

``` r
.mosaic_week_blocks(dates, offset = 0L, partial = c("keep", "drop"))
```

## Arguments

- dates:

  Vector of consecutive daily `Date`s.

- offset:

  Integer 0-6, the day after Monday on which the reporting week starts:
  `est_nb_dispersion()$week_offset` (0, Monday, for every current MOSAIC
  location).

- partial:

  `"keep"` (default) or `"drop"`: whether a week cut by the start or end
  of `dates` is a block of the days it has, or no block at all.

## Value

A list with `index` (integer, the block of each day, numbered from 1 at
the first block; `NA` for the days of a dropped partial week), and, one
entry per block, `week_start` (`Date`, the first day of the block's
week), `complete` (logical: all seven of its days lie in `dates`;
`FALSE` only for a kept partial week), `start` and `end` (positions in
`dates` of the block's first and last day).

## Details

A week cut by the start or end of the grid is a partial week, and the
two consumers treat it differently, so each states its choice through
`partial`. The weekly cases likelihood scores weekly totals, which a
partial week is not, so it drops them (`"drop"`: their days belong to no
block). The observation-level predictive draws noise for every day,
including days that are never scored, so it keeps them (`"keep"`: a
partial week is a block of the days it has).
