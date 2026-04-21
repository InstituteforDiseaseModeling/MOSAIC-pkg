# Estimate V1/V2 Initial-Condition Beta Priors from OCV Campaign History

Builds country-specific Beta priors for `prop_V1_initial` and
`prop_V2_initial` by reading the raw GTFCC OCV request log, classifying
doses by regimen (single-dose Euvichol-S vs. two-dose
Shanchol/Euvichol/Euvichol+), pairing rounds within each campaign,
applying `omega_1` / `omega_2` waning from administration date to
simulation start, and moment-matching the resulting country-level
proportions to Beta distributions.

## Usage

``` r
est_initial_V1_V2(
  PATHS,
  config,
  date_start = NULL,
  cv = 0.4,
  omega_1 = NULL,
  omega_2 = NULL,
  t_lag = 14,
  vacc_ceiling_frac = 0.7,
  fallback_shape1_V1 = 0.5,
  fallback_shape2_V1 = 49.5,
  fallback_shape1_V2 = 0.5,
  fallback_shape2_V2 = 99.5,
  verbose = TRUE
)
```

## Arguments

- PATHS:

  A list of paths (as returned by
  [`get_paths`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)).
  Used to locate
  `ees-cholera-mapping/data/cholera/epicentre/gtfcc/cholera_vacc_requests.csv`
  via `PATHS$ROOT`.

- config:

  A MOSAIC config list (e.g. `config_default`). Used for
  `location_name`, `N_j_initial`, and `date_start`.

- date_start:

  Simulation start date (Date or character `"YYYY-MM-DD"`). Defaults to
  `config$date_start`. Only campaigns delivered strictly before this
  date contribute to V1/V2 initial conditions.

- cv:

  Coefficient of variation for the Beta prior (natural scale). Default
  `0.40`. Tighter values produce narrower priors; looser values permit
  more data-driven posterior movement.

- omega_1:

  One-dose waning rate (per day). Defaults to the natural-scale mean of
  `priors_default$parameters_global$omega_1`.

- omega_2:

  Two-dose waning rate (per day). Defaults to the natural-scale mean of
  `priors_default$parameters_global$omega_2`.

- t_lag:

  Protection onset lag (days). Waning starts at `event_date + t_lag`.
  Default 14.

- vacc_ceiling_frac:

  Hard ceiling on combined V1+V2 coverage (fraction of population).
  Default 0.70 (v0.28.7; was 0.60 in v0.28.6). If the waned dose sum
  exceeds this, V1 and V2 are scaled proportionally. Rationale for 0.70:
  published OCV campaigns routinely reach 65-80% coverage (Abubakar et
  al. 2018 DRC 65%; Qadri et al. 2015 BGD 75%; Luquero et al. 2014 Haiti
  80%+). A lower ceiling under-captures real emergency responses.

- fallback_shape1_V1, fallback_shape2_V1, fallback_shape1_V2,
  fallback_shape2_V2:

  Beta shape parameters used as a fallback when a country has no OCV
  history in the CSV. Defaults match the pre-v0.22.11 uninformative
  priors (`Beta(0.5, 49.5)` for V1, `Beta(0.5, 99.5)` for V2).

- verbose:

  Logical; emit per-country diagnostics. Default TRUE.

## Value

A list with the same nested structure as
`priors_default$parameters_location`: `$prop_V1_initial$location[[iso]]`
and `$prop_V2_initial$location[[iso]]` each carry
`list(distribution = "beta", parameters = list(shape1, shape2))`.

## Details

Biological notes:

- V1/V2 in the MOSAIC/LASER model are *administrative* compartments
  (dose received). LASER's `vaccinated.py` independently splits the
  initial counts into immune (V*k*imm) and susceptible (V*k*sus)
  substates via `phi_1`/`phi_2`. This function therefore does NOT
  multiply by `phi_1`/`phi_2` — doing so would double-count
  effectiveness, a bug present in the pre-v0.22.11 implementation.

- For two-dose regimens, the R01 attendees who return for R02 transition
  V1\\\to\\V2 at the R02 date. Non-returners (R01_doses \\-\\ R02_doses,
  when positive) remain in V1 with `omega_1` waning from R01 onward.

- Single-dose Euvichol-S campaigns contribute only to V1.

- Blank/unknown vaccine products are treated conservatively as two-dose
  (most pre-2022 campaigns were two-dose Shanchol).

- **Waning model:** single exponential. Real OCV waning is biphasic
  (fast decay in months 0-12, slower thereafter; see Xu et al. 2024, Bi
  et al. 2017). Single exponential biases V1/V2 counts upward by roughly
  10-20% for campaigns 2-3 years pre-`date_start` relative to a biphasic
  model. This is considered acceptable for initial-condition purposes;
  implement biphasic separately if needed.

- **Coverage ceiling:** `vacc_ceiling_frac = 0.70` reflects observed
  peak OCV coverage in published campaigns: DRC Katanga 65% (Abubakar et
  al. 2018), Bangladesh 75% (Qadri et al. 2015), Haiti 80%+ (Luquero et
  al. 2014). A lower ceiling under-captures real high-coverage emergency
  responses.

## See also

[`process_GTFCC_vaccination_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_GTFCC_vaccination_data.md),
[`est_vaccination_rate`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_vaccination_rate.md),
[`convert_country_to_iso`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/convert_country_to_iso.md).
