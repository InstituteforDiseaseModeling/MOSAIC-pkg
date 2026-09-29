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

---

## Post-hoc audit (2026-09-16, MOSAIC v0.85.0) — five defects found in the shipped build

Full write-up + repro scripts: `claude/review_v084/findings/DATA.md`,
`claude/review_v084/scratch/DATA/`. All measured on the real
`cholera_country_weekly_suitability_data_v7.4_emdat2026-07-09.csv` (local laptop).

1. **The `.orig_row` "gotcha fixed" above was fixed one level too late.** `preds` itself
   is computed on the re-sorted `d_aug` and written back with `d[[output_col]] <- preds`
   — a positional assignment into `d`'s ORIGINAL order — in **all three** imputers. The
   integrator then pairs `.dp = preds` (d_aug order) with `d$iso_code`/`d$date` (d order),
   so the defended path is fed the undefended vector. Shuffled input:
   `cor(correct, shuffled) = -0.03` for `emdat_flood_prob` and `drought_prob`. Latent only
   because `compile_suitability_data()` happens to leave `d` sorted, and because **every
   test fixture calls `d[order(iso_code, date), ]` first**. This is CLAUDE.md lesson #18's
   shape: re-apply a mechanism's test to the structure that replaces it.
2. **The drought label's generator IS in the predictor set.** `precip_sum_12w` is the 12w
   rolling SUM of precipitation over exactly the label's 12w window, and
   `spei_approx = scale(precip - et0)`. Measured: `cor(12w-rollmean(spei), precip_sum_12w)`
   mean **0.879**, max **0.999**; single-predictor AUC `-precip_sum_12w` = **0.783** vs
   0.827 for the excluded `spei_approx` itself, while **`ENSO34` alone = 0.507 and `IOD`
   = 0.484 (chance)**. So the "CRITICAL leakage control" note is cosmetic and the
   advertised teleconnection LEAD-TIME product is not what the column delivers — the
   0.95-0.98 CV AUC is the circular term + the country RE. The only gate
   (`mean CV AUC < 0.65` -> warning) therefore can never fire.
3. **The drought GAM does not converge on production data** ("algorithm did not converge"
   + "fitted probabilities numerically 0 or 1"), silently — there is no
   `gam_model$converged` check. Result is near-binary: 62.0% of cells < 0.01, 3.3% > 0.99,
   min = `.Machine$double.eps`, max = exactly 1. **SOM and SWZ are pinned at ~1e-4 for all
   1,414 weeks** — a flat-zero channel for the country whose 2016-17 HoA drought the
   docstring claims the label recovers. Also reproduces on the 624-row synthetic fixture.
4. **The drought GAM trains on forecast-window rows; flood/cyclone do not.** Flood/cyclone
   labels are NA past the EM-DAT panel end by design; `drought_active` is derived inside
   the imputer from `spei_approx`, which is populated to the panel end — so **1,560 rows
   (147 positives)** past the observed horizon (2026-04-30 -> 2027-02-04) enter the fit as
   if they were observation, and they are CMIP6 projections
   (see [[open-meteo-climate-horizon-provenance]]). Knock-on: `max(years)` differs, so the
   drought CV folds are 2024/2025/2026 vs flood/cyclone 2023/2024/2025, and the final
   drought fold validates on a mostly-projected year while the comment says "fully-observed".
5. **Pooled CV AUC is the wrong metric for cyclone.** 74 active weeks in 9 of 40 ISOs +
   `s(iso_code_f, bs="re")` -> AUC 0.99/0.91/0.99 is earned by ranking exposed countries
   above the 31 that never have an event, not by timing skill. Flood (positives in all 40)
   gives a credible 0.87/0.82/0.82. Report a **within-country stratified** AUC.

Two more, smaller: flood's `*_4w_max`/`*_12w_max`/`*_12w_sum` keep their warm-up NAs
(120/440/440, and `emdat_flood_prob_12w_max` is a v7.3 feature) while the cyclone sibling's
`12w_max` is explicitly NA-filled; and `diagnostics = TRUE` is hardcoded at all three
compile call sites writing to `PATHS$DOCS_FIGURES`, which
`.rcv_build_leakfree_panel_v74()` does NOT redirect — so every CV cutoff overwrites the
canonical `MOSAIC-docs/figures/*_imputation/` artefacts and costs a **measured 76-101 s**
per compile (flood alone 25 s -> 83 s, 3.3x).

**Canonical panel still lacks all 4 v7.4 columns** (289 cols vs 297/298 in the tagged
panels), and `test-feature_sets.R`'s schema-drift guard exists for v7.3 only — so
`feature_set = "v7.4"` cannot run against the default artefact, and
`?est_suitability`'s `@param feature_set` still documents only v7.3/default even though
`.rcv_psi_v74_request()` keys the whole leak-free-panel mode on that string.

**Left for ml-scientist:** LSTM ingestion verification of the 4 channels, OOS-psi CV
screening (cross-country Pearson + WIS-skill, per-fold hazard-GAM leakage gate — keep
only features that improve OOS psi; "ship zero" is legitimate), psi rebuild on dugong
(local PSOCK, parallel_seeds=1). Prior-impact re-check post-rebuild = disease-modeler.
