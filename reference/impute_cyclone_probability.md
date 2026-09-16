# Impute Country-Week Tropical-Cyclone Probability from EM-DAT and Climate

Fits a binomial GAM on observed EM-DAT tropical-cyclone / storm-surge
events (`emdat_cyclone_active`, 0/1) as a function of a **wind/coastal**
mechanistic predictor set that is deliberately distinct from the
precipitation-driven flood GAM. The key proxy is the weekly maximum 10m
wind speed (`wind_speed_10m_max`); precipitation channels capture the
co-arriving surge/rain, and ENSO/IOD teleconnection lags capture the
seasonal steering of cyclone tracks into the Southwest Indian Ocean /
Mozambique Channel. The fitted model predicts a continuous cyclone
probability for every (iso_code, year, week) row – historical AND
forecast – so the downstream psi LSTM consumes the same feature
definition in training and inference.

## Usage

``` r
impute_cyclone_probability(
  d,
  output_col = "emdat_cyclone_prob",
  gam_train_stop = NULL,
  diagnostics = TRUE,
  diag_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- d:

  A data.frame with one row per (iso_code, year, week). Must contain the
  columns returned by `.impute_cyclone_probability_required()`:
  `iso_code`, `year`, `week`, `date`; the binary target
  `emdat_cyclone_active` (0/1; NA in forecast rows is fine); the WHO
  subregion `region`; the storm proxy `wind_speed_10m_max`;
  precipitation channels `precipitation_sum`, `precip_sum_2w`,
  `precip_sum_4w`; and the teleconnection indices `ENSO34`, `IOD` (lags
  computed inline by the function).

- output_col:

  Character. Name of the new probability column. Default
  `"emdat_cyclone_prob"`.

- gam_train_stop:

  Date or character (`"YYYY-MM-DD"`) or `NULL`. When non-`NULL`, the
  binomial GAM is FIT only on rows with `date <= gam_train_stop`; the
  fitted model then PREDICTS every row (rows `<= gam_train_stop`
  in-sample, rows `> gam_train_stop` leak-free extrapolation). The
  leakage-hygiene hook for rolling-origin forecast CV. Default `NULL` =
  current full-data fit (back-compatible). Only the fit-row subset
  changes; `select=TRUE`/fREML/the formula are identical.

- diagnostics:

  Logical. If `TRUE`, fits a rolling-year cross-validation and writes
  diagnostic artefacts to `diag_dir`.

- diag_dir:

  Character. Directory for diagnostic outputs (created if missing).
  Ignored when `diagnostics = FALSE`.

- verbose:

  Logical. Echo progress messages.

## Value

The input data.frame with one added column named `output_col` carrying a
non-NA numeric in \[0, 1\] for every row.

## Details

**Why a separate GAM (not folded into the flood label).** Tropical
cyclones carry the opposite climate signature from ordinary
precipitation floods: a sharp wind spike with a short, crisply-dated
footprint. The flood-only EM-DAT filter historically dropped all 49
tropical-cyclone / surge events, which neutered the flood GAM's wind
smooth. Modeling cyclones with their own wind-led GAM restores that
signal as a clean input channel; the psi LSTM recombines the flood and
cyclone probabilities downstream (the shared cholera-contamination
mechanism lives in the LSTM, not in the label).

The GAM is fit with
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
`family = stats::binomial(link = "logit")` (predictions naturally in
\[0, 1\]), `method = "fREML"`, `select = TRUE` (smoothness null-space
shrinkage – drives uninformative smooths toward zero, the same
principled mechanism used by
[`impute_flood_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_flood_probability.md)).
Predictors computed inline (the caller need not supply them):

- ENSO34 and IOD at lags 8, 16, 24 weeks (seasonal cyclone-season
  steering; SWIO cyclone activity co-varies with ENSO/IOD phase).

- Nothing else – the wind and short-window precip channels are the acute
  proxy; the country random effect absorbs baseline coastal-exposure
  differences.

The active fraction is very low (~74 weeks out of ~35k country-weeks,
~0.2\\ concentrates predicted probability on the historically
cyclone-exposed coastal countries (MOZ, MWI, ZWE, MDG-adjacent belt) and
keeps landlocked / non-cyclone countries near zero. A region-conditional
wind smooth (`s(wind_speed_10m_max, by = region_f)`) lets Southern
Africa (the cyclone belt) map wind to risk more steeply than the other
subregions.

Sentinel handling mirrors the flood imputer: any NA prediction (lag
warm-up edge, residual missing predictor) is filled with that country's
mean predicted probability; global mean is the last resort. The function
asserts the final column is numeric, in \[0, 1\], and NA-free.

If `diagnostics = TRUE`, a rolling-year CV (3 most-recent fully-observed
years) writes per-fold AUC / Brier / log-loss to
`cyclone_gam_cv_metrics.csv`. A mean CV AUC below 0.65 emits a warning
but the column is still returned.

## See also

[`impute_flood_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_flood_probability.md),
[`compile_suitability_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_suitability_data.md),
[`process_EMDAT_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_EMDAT_data.md)
