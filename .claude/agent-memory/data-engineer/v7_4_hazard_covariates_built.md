---
name: v7-4-hazard-covariates-built
description: v7.4 hazard redesign IMPLEMENTED (not committed) — separate cyclone GAM + SPEI-label drought GAM as psi LSTM input channels; the [[emdat-flood-only-filter-drops-cyclones]] fix shipped as separate series, not by widening the flood filter
metadata:
  type: project
---

The [[emdat_flood_only_filter_drops_cyclones]] cyclone-blindness was fixed via the
**v7.4 hazard-covariate redesign** (plan:
`claude/plan_forecast_cv/PLAN_HAZARD_COVARIATES_V7_4.md`). Built at MOSAIC v0.62.0
(NOT committed — ml-scientist owns the psi rebuild before commit).

**Architecture contract:** SEIR engine takes ONE covariate (psi). The 3 hazard-GAM
probabilities (flood/cyclone/drought) are INPUT feature-channels to the psi LSTM,
which emits the single psi. Hazards never touch the engine directly.

**Resolution chosen: SEPARATE series, NOT filter-widening.** The flood label
(`Disaster Type=="Flood"`) is UNCHANGED. `process_EMDAT_data.R` now emits a
parallel `cyclones_country_weekly.csv` from `Disaster Type=="Storm"` AND
`Disaster Subtype %in% c("Tropical cyclone","Storm surge")` = 49 events / **74
active cyclone-weeks** over 9 ISOs (MOZ 27, ZWE 18, MWI 10, SOM 9, ZAF 5, TZA 2,
COD/GMB/SWZ 1). Storm-General/Severe/Lightning/Tornado/Hail EXCLUDED. Returns
`c(flood=..., cyclone=...)` now (was a single path — updated all callers/tests).
Shared internal helpers `.emdat_event_dates()` + `.emdat_build_hazard_panel(prefix)`.

**Two new imputers (sibling to impute_flood_probability, same mgcv::bam +
binomial/logit + fREML + select=TRUE + country-mean sentinel):**
- `impute_cyclone_probability()` → `emdat_cyclone_prob` + `emdat_cyclone_prob_12w_max`.
  Wind-led: `s(wind_speed_10m_max)` + `s(wind_speed_10m_max, by=region_f)` + short
  precip + ENSO34/IOD lags + iso RE. Dev.expl 53% on production panel. **Validated:
  every MOZ landfall in 83rd–99.6th country percentile** (Freddy 99.6, Jude 99.5,
  Eloise 98.9, Gombe 98.3, Idai 94.1, Dikeledi 96.3; Chido lowest at 83rd — far-north
  landfall diluted by national mean, a SUBNATIONAL limit not a bug). Coastal
  concentration correct: MOZ>ZWE>MWI>SOM>ZAF, ~2e-5 for landlocked C/W Africa.
- `impute_drought_probability()` → `drought_prob` + `drought_prob_26w_mean`
  (26w=slow integrator). **LABEL is SPEI-derived, NOT EMDAT** (EMDAT drought is a
  smeared multi-year mess — median 344d, 18-country concurrency). Label =
  `12w-rolling-mean(spei_approx) <= -0.8` → **5633 pos / 36779 (15.3%), 38 ISOs**;
  recovers HoA 2016-17, SthAfrica 2015-16/2018-19, 2023-24. 15% base rate is fine
  (drought is a persistent STATE). **CRITICAL leakage control: concurrent spei_approx
  is EXCLUDED from predictors** (it generates the label); predictors are ENSO/IOD
  lags + antecedent temp_anom/precip_anom/precip_sum_12w/24w + iso RE. The value is
  the teleconnection→drought LEAD-TIME map. Dev.expl 68%.

**DECIDED (flag for ml-scientist/user review):** SPEI threshold **-0.8**, sustain
window **12w**, drought integrator **26w**. -1.0/8w (an earlier try) UNDER-captured
known droughts because spei_approx is a per-country full-series z-score of
(precip−ET0), not deseasonalized — the 12w-rollmean ≤ -0.8 form fixed that.

**Wiring:** `compile_suitability_data.R` merges cyclone panel (step 3c, NA-aware in
exclude_cols like floods) + calls both imputers after the flood block, gated on
`include_flood_prob` (now the master switch for ALL 3 hazard GAMs, each with its own
required-col contract via `.impute_*_probability_required()`). New cols flow through
as remaining_cols in final ordering. `feature_sets.R`: **v7.4 = v7.3 (38) + 4** =
`emdat_cyclone_prob`, `emdat_cyclone_prob_12w_max`, `drought_prob`,
`drought_prob_26w_mean` = 42. v7.3 unchanged + still default.

**Panel written to v7.4-tagged path** `cholera_country_weekly_suitability_data_v7.4.csv`
(297 cols) — canonical panel PRESERVED. floods_country_weekly.csv was regenerated but
is content-identical (flood logic unchanged); cyclones_country_weekly.csv is new.

**Gotcha found + fixed:** drought integrator scatter-back must carry a stable
`.orig_row` index through the dplyr group/arrange — the naive `order(iso,date)`
re-map only works if input `d` is already sorted (production panel happened to be;
shuffled-input regression proved the index fix, max diff 0).

**Left for ml-scientist:** LSTM ingestion verification of the 4 channels, OOS-psi CV
screening (cross-country Pearson + WIS-skill, per-fold hazard-GAM leakage gate — keep
only features that improve OOS psi; "ship zero" is legitimate), psi rebuild on dugong
(local PSOCK, parallel_seeds=1). Prior-impact re-check post-rebuild = disease-modeler.
