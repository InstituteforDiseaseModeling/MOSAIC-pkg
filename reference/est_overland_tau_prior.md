# Build a per-country overland departure-rate prior for MOSAIC calibration

Turns the E3 evidence band into a per-country Beta prior on the DAILY
departure probability `tau_i`, suitable for use as a calibration prior
in place of the air-derived one.

## Usage

``` r
est_overland_tau_prior(
  PATHS,
  iso_codes = NULL,
  write = TRUE,
  in_set_adjust = TRUE,
  tau_weekly = MOSAIC_E3_TAU_WEEKLY,
  tau_weekly_default = NULL,
  span = 10,
  span_default = 30,
  family = c("lognormal", "beta"),
  verbose = TRUE
)
```

## Arguments

- PATHS:

  List of paths from
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md).

- iso_codes:

  ISO3 codes. Defaults to
  [`MOSAIC::iso_codes_mosaic`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/iso_codes_mosaic.md).

- write:

  Write `param_tau_departure_overland.csv`? `TRUE` by default. Internal
  callers passing a narrowed `iso_codes` must pass `FALSE`, or they
  truncate the model-input file to that subset.

- in_set_adjust:

  Scale each country's tau by the share of its outbound migrant stock
  whose destination is inside the patch set. `TRUE` by default. Without
  it a country whose largest corridors leave the patch set (Ethiopia -\>
  Djibouti/Sudan, South Sudan -\> Sudan, South Africa -\> Lesotho) has
  that flow silently re-routed onto its in-set neighbours by the row
  normalisation of `pi_ij`.

- tau_weekly:

  Named weekly departure probabilities. Defaults to
  [`MOSAIC_E3_TAU_WEEKLY`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/MOSAIC_E3_TAU_WEEKLY.md).

- tau_weekly_default:

  Weekly value for countries the E3 work never covered. Default `NULL` =
  the median of `tau_weekly`.

- span:

  Target 95% interval width, as a MULTIPLICATIVE factor, for countries
  with direct E3 evidence. Default `10` – one order of magnitude, which
  is what the underlying border-throughput evidence actually supports
  (every count is a floor).

- span_default:

  Target 95% span for countries with no country-specific evidence.
  Default `30`.

- family:

  Prior family. `"lognormal"` (default) or `"beta"`. Lognormal is
  strongly preferred: a Beta wide enough to express an
  order-of-magnitude floor is J-shaped, putting the prior mode at zero
  departure rate.

- verbose:

  Print a summary.

## Value

Invisibly, a `data.frame`: `iso_code`, `tau_weekly`, `tau_daily`, `sd`,
`shape1`, `shape2`, `ci_lo`, `ci_hi`, `evidence` (`"E3"` or
`"default"`). Written to `model/input/param_tau_departure_overland.csv`.

## Choosing the width

The E3 band spans roughly 1e-3 to 5e-3 per week, a factor of 5. Treating
that as a 95% interval on a log scale gives
`sdlog = log(5) / (2 * 1.96) = 0.41`, hence `CV ~ 0.43`, rounded to
**0.45**; unevidenced countries get **0.75**.

**Realised intervals, measured, not derived:** CV 0.45 gives a 95% span
of **6.4x** and CV 0.75 gives **29.3x** – not the "factor of 10" an
earlier version of this note claimed. The lognormal-to-Beta transfer
moves the *moment*, not the *interval*, and the two diverge above CV ~
0.6. A true factor-10 interval needs CV = 0.55.

**This prior is TIGHTER than the one it replaces.** The production air
prior in `priors_default` is not the raw fit in
`param_tau_departure.csv`: `data-raw/make_priors_default.R` applies
`tau_uncertainty_factor = 0.001`, giving a median CV of **1.39** with 31
of 40 countries having `shape1 < 1` (mode at zero). So adopting this
prior is a ~12x re-centring combined with a ~3x tightening, on a
quantity whose sources are explicitly floors. Since `tau_i` is weakly
identified and MOSAIC uses the prior as its proposal, the centre largely
determines the posterior – widening to CV \>= 1 would require switching
to a lognormal, because a Beta with CV \>= 1 is J-shaped (mode at zero).

## Relationship to the air prior

Air-derived `tau_i` has median ~3e-5/day; these overland values are
~4e-5 to 7e-4/day, median 3.6e-4 – a **~12x lift**.

The supporting anchor is StatsSA Tourism 2024 (Report 03-51-02):
**68-91% of South Africa's ~8.9M annual cross-border arrivals are
overland**, so an air-only fit captures ~9-32% of flow, i.e. road:air
**~2x to ~10x** for a formal, highly air-connected economy. Informal
crossings, which StatsSA cannot see, push this higher for porous or
landlocked countries. (Earlier notes in this repo cite "~1:90 air:road"
and a "~100x undercount": those restate the 91% overland SHARE as a
ratio and are not supported by the source. A related 7x inflation
appears where E3 compared a weekly overland rate against MOSAIC's daily
air rate.)

These REPLACE the air prior for an overland-aware calibration. Note the
conceptually cleaner object is additive, `tau_air + tau_overland`, since
the air residual carries the long-range, non-adjacent links that are the
only route by which a distant country can be seeded.

## See also

[`rake_mobility_od_to_tau`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/rake_mobility_od_to_tau.md),
[`est_mobility`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_mobility.md)
