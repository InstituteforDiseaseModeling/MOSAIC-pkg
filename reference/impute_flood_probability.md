# Impute Country-Week Flood Probability from EM-DAT and Climate Covariates

Fits an enriched binomial GAM on observed EM-DAT flood events
(`emdat_flood_active`, 0/1) as a function of climate anomalies,
multi-month antecedent precipitation, region-conditional precipitation,
joint precip/soil-moisture interactions, ENSO/IOD long lags, and country
random effects. The fitted model is then used to predict a continuous
flood probability for every (iso_code, year, week) row – historical AND
forecast. The imputed column lets the downstream LSTM consume the same
feature definition in training and inference, eliminating the
distribution shift the raw 0/1 indicator would otherwise introduce.

## Usage

``` r
impute_flood_probability(
  d,
  output_col = "emdat_flood_prob",
  diagnostics = TRUE,
  diag_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- d:

  A data.frame with one row per (iso_code, year, week). Must contain the
  columns returned by `.impute_flood_probability_required()`:
  `iso_code`, `year`, `week`, `date`; the binary target
  `emdat_flood_active` (0/1; NA in forecast rows is fine); the WHO
  subregion `region`; the precipitation channels `precipitation_sum`,
  `precip_anom`, `precip_sum_2w/4w/8w/12w`, `precip_extreme_p90_count`;
  soil moisture `soil_moisture_0_to_10cm_mean`, `soil_moisture_anom`;
  `spei_approx`; atmospheric humidity `relative_humidity_2m_mean`,
  `rh_mean_12w`; storm proxy `wind_speed_10m_max`; and the
  teleconnection indices `ENSO3`, `ENSO34`, `ENSO4`, `IOD` (lags and
  long-window precip sums computed inline by the function).

- output_col:

  Character. Name of the new probability column. Default
  `"emdat_flood_prob"`.

- diagnostics:

  Logical. If `TRUE`, fits a rolling-year cross-validation and writes
  four artefacts to `diag_dir`: `flood_gam_smooths.png`,
  `flood_gam_calibration.png`, `flood_gam_cv_metrics.csv`,
  `flood_gam_summary.txt`.

- diag_dir:

  Character. Directory for diagnostic outputs (created if missing).
  Ignored when `diagnostics = FALSE`.

- verbose:

  Logical. Echo progress messages.

## Value

The input data.frame with one added column named `output_col` carrying a
non-NA numeric in \[0, 1\] for every row.

## Details

The four "enriched" predictors (added in v0.30.23 over the prior
v0.30.20-21 baseline) – long-memory antecedent precipitation at 24-week
and 52-week horizons, a region-conditional medium-window precip smooth,
and a precip-anomaly x soil-moisture-anomaly joint interaction – improve
detection of major cyclone-driven flood events (Idai 2019, Kenneth 2019,
Eloise 2021, Ana 2022, Gombe 2022, Freddy 2023). On a 10-cyclone
benchmark this formulation lands every event in the top 21\\
distribution (median 93rd percentile), the most reliable cyclone
detector across the rebuild strategies tested.

The GAM is fit with
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html) (mgcv's big-data
variant – uses block updates, discrete covariate binning, and
parallel-friendly linear algebra; on ~30k rows it returns in ~30-40
sec). `family = stats::binomial(link = "logit")` produces predictions
naturally in \[0, 1\] without rescaling. `method = "fREML"`,
`select = TRUE` (smoothness null-space shrinkage, the principled mgcv
mechanism for driving uninformative smooths toward zero – replaces the
iterative in-sample-p-value pruning loop that v0.30.24 used). Predictors
computed inline (so the caller does not need to provide them):

- ENSO34 and IOD at lags 8, 16, 24 weeks (current value is the raw
  column).

- Long-memory antecedent precipitation: `precip_sum_24w` (~6-month basin
  saturation memory) and `precip_sum_52w` (annual).

- Joint climate index: `precip_anom * soil_moisture_anom` (very-wet AND
  already-saturated regime).

Region-conditional precip-sum-4w smooth requires a `region` column on
the input dataframe (4-region WHO classification: Central / East /
Southern / West Africa).

Sentinel handling: any row whose prediction is NA (e.g. a lag-startup
edge before week 20 of the earliest year, or a row with a missing
predictor that survived upstream imputation) is filled with that
country's mean predicted probability. Rows where even the country mean
is NA fall back to the global mean. The function asserts the final
column is in \[0, 1\] and contains no NAs.

If `diagnostics = TRUE`, the function additionally runs a rolling-year
cross-validation: for each of the 3 most recent fully-observed years Y,
the GAM is refit on rows with `year < Y` and used to predict rows with
`year == Y`. Per-fold AUC, Brier score, and log-loss are written to
`flood_gam_cv_metrics.csv`. If the mean CV AUC is below 0.65, a warning
is emitted but the function still returns the column.

## See also

[`compile_suitability_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_suitability_data.md),
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md),
[`process_EMDAT_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/process_EMDAT_data.md)
