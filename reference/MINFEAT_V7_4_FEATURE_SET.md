# v7.4 LSTM covariate feature set (v7.3 + lean cyclone/drought hazard channels)

The full v7.3 38-feature vector PLUS four new hazard channels from the
v7.4 hazard-covariate redesign: two cyclone channels
(`emdat_cyclone_prob`, `emdat_cyclone_prob_12w_max`) and two drought
channels (`drought_prob`, `drought_prob_26w_mean`). These are INPUT
feature-channels to the psi LSTM (which emits the single psi the SEIR
engine consumes) – they never touch the engine directly. Total: 42
features. Provenance:
`claude/plan_forecast_cv/PLAN_HAZARD_COVARIATES_V7_4.md`.

## Usage

``` r
MINFEAT_V7_4_FEATURE_SET
```

## Format

Character vector of covariate column names.

## Details

The new features are subject to the same OOS psi-CV screening gate that
produced v7.3 (cross-country Pearson + WIS-skill, per-fold hazard-GAM
leakage control); features that do not improve OOS psi should be dropped
before v7.4 is promoted to default.

## See also

[`get_feature_set`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/get_feature_set.md),
[`est_suitability`](https://institutefordiseasemodeling.github.io/MOSAIC-pkg/reference/est_suitability.md)
