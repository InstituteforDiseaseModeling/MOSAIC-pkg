# Impute Country-Week Drought Probability from a SPEI-Deficit Label

Fits a binomial GAM that maps ENSO/IOD teleconnections and antecedent
temperature / precipitation-deficit onto a **sustained-SPEI-deficit**
drought label, then predicts a continuous drought probability for every
(iso_code, year, week) row – historical AND forecast. The value of the
GAM is the *teleconnection-to-drought lead-time mapping*: it lets the
downstream psi LSTM see drought risk building from the slow ocean-state
drivers, months before the local moisture deficit fully develops.

## Usage

``` r
impute_drought_probability(
  d,
  spei_threshold = -0.8,
  sustain_weeks = 12L,
  output_col = "drought_prob",
  integrator_col = "drought_prob_26w_mean",
  integrator_weeks = 26L,
  gam_train_stop = NULL,
  diagnostics = TRUE,
  diag_dir = NULL,
  verbose = TRUE
)
```

## Arguments

- d:

  A data.frame with one row per (iso_code, year, week). Must contain the
  columns returned by `.impute_drought_probability_required()`:
  `iso_code`, `year`, `week`, `date`; `spei_approx` (used ONLY to build
  the label, never as a predictor); antecedent `temp_anom`,
  `precip_anom`, `precip_sum_12w`, `precipitation_sum`; the WHO
  subregion `region`; and the teleconnection indices `ENSO34`, `IOD`
  (lags computed inline).

- spei_threshold:

  Numeric. The 12-week rolling-mean `spei_approx` at or below which a
  country-week is labeled drought-active. Default `-0.8` (justified
  above; see `sustain_weeks`).

- sustain_weeks:

  Integer. Rolling-mean window (weeks) enforcing the "sustained"
  requirement. Default `12L`.

- output_col:

  Character. Name of the new probability column. Default
  `"drought_prob"`.

- integrator_col:

  Character. Name of the slow long-memory integrator column. Default
  `"drought_prob_26w_mean"`.

- integrator_weeks:

  Integer. Trailing window (weeks) for the slow integrator. Default
  `26L` (~half a year; see Details).

- gam_train_stop:

  Date or character (`"YYYY-MM-DD"`) or `NULL`. When non-`NULL`, the
  binomial GAM is FIT only on rows with `date <= gam_train_stop`; the
  fitted model then PREDICTS every row (rows `<= gam_train_stop`
  in-sample, rows `> gam_train_stop` leak-free extrapolation). The
  leakage-hygiene hook for rolling-origin forecast CV. The SPEI-deficit
  LABEL's rolling mean (an input transform) is unaffected – only the GAM
  fit-row subset is capped. Default `NULL` = current full-data fit
  (back-compatible). Only the fit-row subset changes;
  `select=TRUE`/fREML/the formula are identical.

- diagnostics:

  Logical. If `TRUE`, fits a rolling-year cross-validation and writes
  diagnostic artefacts to `diag_dir`.

- diag_dir:

  Character. Directory for diagnostic outputs (created if missing).
  Ignored when `diagnostics = FALSE`.

- verbose:

  Logical. Echo progress messages.

## Value

The input data.frame with TWO added columns: `output_col` (per-week
probability, non-NA numeric in \[0, 1\]) and `integrator_col`
(trailing-window mean of the probability, the slow long-memory
drought-state channel, also non-NA in \[0, 1\]).

## Details

**Label (derived here, NOT from EM-DAT events).** EM-DAT drought records
are unusable as a weekly label (median ~344-day duration, dozens of
multi-year events, ~18/40 countries "active" in a single week – a
constant background wash). Instead the label is built from the panel's
own `spei_approx` (per-country standardized precipitation minus
evapotranspiration): a country-week is `drought_active = 1` when the
**12-week rolling mean of `spei_approx` is at or below
`spei_threshold`** (default -0.8). The 12-week window enforces the
"sustained" requirement (a single dry week does not qualify) and the
rolling mean is the standard meteorological way to distinguish a drought
*spell* from short-term dryness. On the production panel this yields
~15\\ meteorological droughts (Southern Africa 2015-16 El Nino &
2018-19, Horn of Africa 2016-17, the 2023-24 El Nino) – NOT the smeared
multi-year mess the EM-DAT drought type produces.

**CRITICAL leakage control.** The concurrent `spei_approx` is the label
source, so it (and its rolling mean) is **excluded** from the predictor
set – predicting the label from its own generator would be trivially
circular and would defeat the entire purpose (the lead-time mapping).
The predictors are exogenous ocean-state teleconnections and antecedent
temperature / precipitation-deficit only.

The GAM is fit with
[`mgcv::bam()`](https://rdrr.io/pkg/mgcv/man/bam.html),
`family = stats::binomial(link = "logit")`, `method = "fREML"`,
`select = TRUE` (smoothness null-space shrinkage). Predictors:

- ENSO34 and IOD at lags 8, 16, 24 weeks (the slow ocean-state drivers
  that LEAD drought by a season or two – this lead time is the product
  the GAM sells to the LSTM).

- Antecedent `temp_anom` (heat amplifies evaporative demand),
  `precip_anom`, and long-window antecedent precipitation
  `precip_sum_12w` / `precip_sum_24w` (accumulated rainfall deficit; 24w
  computed inline).

- `s(iso_code_f, bs = "re")` country random effect (baseline aridity /
  drought propensity).

The concurrent `spei_approx` and its rolling mean are NOT predictors
(label leakage; see the leakage-control note above).

**Slow integrator.** Drought is a persistent state whose cholera
relevance accumulates (WASH strain, water-source concentration,
migration). A single per-week probability under-represents that memory,
so a trailing `integrator_weeks`-week mean of the probability is emitted
alongside. 26 weeks (~half a year) is chosen because it spans a full
dry-season build without bleeding a prior year's drought into the next –
long enough to be a genuine slow channel, short enough to remain
seasonally resolved. It is a backward-looking rolling mean (no future
leakage) and warm-up NAs are filled with the country mean.

Sentinel handling mirrors the flood/cyclone imputers: NA predictions are
filled with the country mean, then the global mean. Both output columns
are asserted numeric, in \[0, 1\], NA-free.

## See also

[`impute_flood_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_flood_probability.md),
[`impute_cyclone_probability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/impute_cyclone_probability.md),
[`compile_suitability_data`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/compile_suitability_data.md)
