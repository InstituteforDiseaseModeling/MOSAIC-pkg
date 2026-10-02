# Compile Environmental Suitability Data for Cholera Transmission

Compiles multiple processed data sources (climate, ENSO, cholera
surveillance, demographics, WASH) into a single feature-engineered
training dataset for environmental suitability modeling. Computes
derived covariates including rolling windows, anomalies, and climate /
WASH interaction terms.

## Usage

``` r
compile_suitability_data(
  PATHS,
  cutoff,
  use_epidemic_peaks = FALSE,
  date_start = "2000-01-01",
  date_stop = NULL,
  forecast_mode = TRUE,
  forecast_horizon = 3,
  include_lags = FALSE,
  include_flood_prob = TRUE,
  gam_train_stop = NULL,
  target_anchor_stop = NULL,
  backfill_case_gaps = FALSE,
  backfill_max_weeks = 2L,
  backfill_method = "linear"
)
```

## Arguments

- PATHS:

  A list containing paths where the data is stored. Typically generated
  by the
  [`get_paths()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_paths.md)
  function and should include:

  - **DATA_CLIMATE**: Path to the directory where processed climate data
    is stored.

  - **DATA_ENSO**: Path to the directory where ENSO data is stored.

  - **DATA_CHOLERA_WEEKLY**: Path to the directory containing processed
    combined weekly cholera cases data (WHO+JHU+SUPP sources).

- cutoff:

  Numeric threshold for case counts. Used when use_epidemic_peaks =
  FALSE. Weeks with cases \>= cutoff are considered outbreak periods for
  the environmental suitability calculation. Default behavior uses this
  value to create the binary indicator.

- use_epidemic_peaks:

  Logical. If TRUE, uses epidemic peak periods from est_epidemic_peaks()
  to define environmental suitability using exact peak_start to
  peak_stop dates. Provides deterministic outbreak detection without any
  modifications or lead-up periods. Default FALSE for backwards
  compatibility.

- date_start:

  Start of the data window. Accepts a year (e.g. `2000`) or a date
  string (e.g. `"2000-01-01"`); only the year is used for filtering.
  Default `"2000-01-01"`. This caps how far back the panel extends
  (important when the AI source supplies pre-2000 reconstructions). Pass
  `NULL` to auto-detect the start from case data (one year prior to the
  earliest case observation).

- date_stop:

  End of the data window (year or date string). If NULL (default),
  auto-detected from the latest raw ENSO data availability.

- forecast_mode:

  Logical. If TRUE, only includes variables that can support forecasting
  at the specified horizon. (As of v0.30.26 the case-dependent epidemic
  memory and spatial-import blocks are removed entirely, so this flag
  now affects only the final summary-message text and the
  climate-anomaly leading-edge behavior.) Default TRUE.

- forecast_horizon:

  Numeric. Forecasting horizon in months. Variables dependent on
  surveillance data beyond this horizon are excluded in forecast mode.
  Default 3 months.

- include_lags:

  Logical. If TRUE, includes time-lagged climate variables with
  epidemiologically-informed lag periods specific to each variable.
  Default FALSE.

- include_flood_prob:

  Logical. If TRUE (default), imputes a continuous `emdat_flood_prob`
  column for every (iso_code, year, week) row via a binomial GAM trained
  on observed `emdat_flood_active` events (see
  [`impute_flood_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_flood_probability.md)),
  and adds four rolling-window aggregates (`emdat_flood_prob_4w_max`,
  `emdat_flood_prob_12w_max`, `emdat_flood_prob_12w_sum`,
  `emdat_flood_prob_anom`). The probability covariates are populated in
  both the historical training and the forecast windows, so the
  downstream LSTM consumes the same feature definition in both regimes.

  As of the v7.4 hazard redesign this flag is the master switch for ALL
  THREE hazard-imputation GAMs: it additionally triggers
  [`impute_cyclone_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_cyclone_probability.md)
  (adds `emdat_cyclone_prob` and `emdat_cyclone_prob_12w_max`) and
  [`impute_drought_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_drought_probability.md)
  (adds `drought_prob` and the slow integrator `drought_prob_26w_mean`).
  Each GAM is gated on its own required-column contract, so a missing
  input skips only that hazard.

- gam_train_stop:

  Date or character (`"YYYY-MM-DD"`) or `NULL`. Passed through to all
  three hazard imputers
  ([`impute_flood_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_flood_probability.md),
  [`impute_cyclone_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_cyclone_probability.md),
  [`impute_drought_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_drought_probability.md)).
  When non-`NULL`, each hazard GAM is FIT only on rows with
  `date <= gam_train_stop` and then predicts every row – the
  leakage-hygiene hook for building a leak-free per-cutoff panel in
  rolling-origin forecast CV. Default `NULL` = full-data fit
  (back-compatible).

- target_anchor_stop:

  Date or character (`"YYYY-MM-DD"`) or `NULL`. Upper bound on the rows
  used to compute the normalising ANCHORS for the response variables –
  `ti_p99` (`transmission_intensity`), the global count/rate p99s
  (targets A, C) and the per-country `cp99c`/`cp99r` and median
  population (targets B, D). When non-`NULL`, the p99 anchors use only
  trusted rows (not AI, not imputed) with `date <= target_anchor_stop`,
  and the median population behind the 5-cases/week floor uses the
  non-AI rows with `date <= target_anchor_stop`, while every row in the
  panel still RECEIVES a target scored on those anchors.

  This is the target-side sibling of `gam_train_stop` and closes the
  remaining leak in a per-cutoff panel: because such a panel keeps
  `date_stop = NULL` (it must span rows past the cutoff so the model can
  predict the forecast window), a full-window anchor makes the target at
  time *t* depend on data after *t*. It also removes retroactive
  re-baselining – with a fixed anchor window a data refresh can no
  longer shift the entire historical target series.

  Default `NULL` = full-window anchors (back-compatible). Note
  `target_F_rank_per_country` is a rank over the country's whole
  observed series and is full-window BY CONSTRUCTION; this argument does
  not and cannot make it leak-free.

  The panel records the effective anchor window end (the last trusted,
  observed row used for the anchors) in a constant `target_anchor_stop`
  column.
  [`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
  warns when a `target_*` response is fit with a cutoff earlier than
  this date.

  **Leakage scope.** `gam_train_stop` and `target_anchor_stop` bound the
  hazard GAMs and the target anchors only. Covariate standardisations
  are still computed over the whole panel window, including rows after
  either bound: the week-of-year climatology anomalies (`precip_anom`,
  `temp_anom`, `soil_moisture_anom`, ...), the per-country `spei_approx`
  scaling, the precipitation p90 / temperature p95 extreme thresholds,
  and the `emdat_flood_prob_anom` baseline. No post-cutoff case data
  enters these, but in a per-cutoff panel the pre-cutoff values of those
  covariates depend weakly on the post-cutoff climate distribution, so
  such a panel is leak-free on the target and hazard side only.

- backfill_case_gaps:

  Logical (default `FALSE`). When `TRUE`, linearly interpolate short
  interior gaps in the weekly case series (e.g. holiday non-reporting)
  before the suitability target is built, via
  [`backfill_weekly_case_gaps`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/backfill_weekly_case_gaps.md).
  Off by default since v0.101.0: the lstm_v2 suitability model already
  leaves unobserved (NA-case) weeks out of its training targets, so a
  filled week only adds an interpolated target; and the fill cannot tell
  a week no source reported from one the surveillance reconciliation
  deliberately emptied (in the v0.101.0 panel, 15 of the 26 filled weeks
  were imputed weeks that the WHO annual account or the half-case
  residue rule had removed). The frozen legacy suitability path still
  turns unobserved weeks into zeros
  ([`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
  with the v0.33 architecture), the false zeros this fill was written to
  prevent; set `TRUE` to rebuild a panel for it. A filled week is
  imputed, not observed: it is flagged in `cases_interpolated` (all
  `FALSE` when the fill is off), labelled
  `disaggregation_method = "backfill_interpolated"` with `source` and
  `deaths` left missing, carries a `confidence_weight` of at most 0.5
  (never above the weaker of the two reported weeks it is interpolated
  from), and never defines a target anchor. A week some source reported,
  a deaths-only week, is not a gap and is not filled.

- backfill_max_weeks:

  Integer (default `2L`). Maximum interior gap length (in weeks) to
  interpolate; longer gaps are left for the zero-fill.

- backfill_method:

  Interpolation method passed to
  [`backfill_weekly_case_gaps`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/backfill_weekly_case_gaps.md)
  (default `"linear"`).

## Value

This function processes the data and merges the climate, ENSO, and
cholera cases data into a single dataset. It creates a `cases_binary`
column indicating environmental suitability based on case patterns using
sophisticated temporal logic. The processed dataset is saved as a CSV
file.

## Details

As of v0.30.26 the vaccination, epidemic-memory (cases-derived), and
spatial-import (mobility-network) blocks are no longer computed. They
were already excluded from
[`est_suitability()`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)'s
LSTM covariate list – the vaccination signal is a downstream
intervention (deployed in response to cholera, not in anticipation of
environmental suitability), and the case-derived epidemic-memory
features are target leakage for a model whose response is itself a
case-derived binary.

The function performs the following key steps:

- Loads and merges climate data (temperature, precipitation, etc.) from
  multiple countries

- Loads ENSO and IOD climate index data

- Loads combined weekly cholera case surveillance data from WHO, JHU,
  and supplemental sources

- Merges all datasets by country, year, and week

- Creates environmental suitability indicator (`cases_binary`) using
  either epidemic peak-based or threshold-based methods

- Saves the complete merged dataset for use in environmental suitability
  modeling

**Environmental Suitability Methods:**

- **Epidemic Peak-based** (use_epidemic_peaks = TRUE): Uses
  deterministic outbreak detection from est_epidemic_peaks() using exact
  peak_start to peak_stop dates without any modifications

- **Threshold-based** (use_epidemic_peaks = FALSE): Uses simple case
  count cutoffs with temporal lead-up logic

The epidemic peak-based method provides deterministic, epidemiologically
meaningful outbreak periods and is recommended for consistent
environmental suitability modeling.

## See also

[`get_cases_binary`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_cases_binary.md)
for the threshold-based environmental suitability method
[`get_cases_binary_from_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_cases_binary_from_peaks.md)
for the epidemic peak-based method
[`est_epidemic_peaks`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_epidemic_peaks.md)
for epidemic peak detection
