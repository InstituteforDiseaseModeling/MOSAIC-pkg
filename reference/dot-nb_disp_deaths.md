# Deaths dispersion, falling back to every week when observed weeks are too few

As
[`est_nb_dispersion`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md)
(no panel trend), except that a location whose observed weeks alone
cannot carry an estimate (status `no_estimate_observed_insufficient`
under `obs_tier`) takes its estimate from every scored week instead. Too
few observed weeks is not evidence of Poisson scatter, and deaths have
no panel trend to borrow, so without this a single-location run would
score such a location at the Poisson limit. The integrated deaths
likelihood applies the same rule to its dispersion (`.d7_setup`).

## Usage

``` r
.nb_disp_deaths(
  obs,
  weights_obs = NULL,
  date_start,
  location_name = NULL,
  shrink = TRUE,
  obs_tier = NULL
)
```

## Arguments

- obs, weights_obs, date_start, location_name, shrink, obs_tier:

  As for
  [`est_nb_dispersion`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md).

## Value

The
[`est_nb_dispersion()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md)
table.

## Details

Only those locations are re-estimated, on their own and without
shrinkage, so their value is the same at every scale, and the other
locations' rows – including their shrinkage, whose trend is fitted on
observed-weeks estimates only – are exactly those of
[`est_nb_dispersion()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_nb_dispersion.md).
A location whose every-week fit gives no estimate either keeps the first
table's row.
