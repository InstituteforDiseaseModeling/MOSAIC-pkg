---
name: v74-hazard-ingestion-and-screening
description: v7.4 hazard channels (cyclone/drought) INGEST cleanly into lstm_v2 (38->42, verified); but source_csv is hardcoded + hazard GAMs have no per-fold cutoff = leak; screening spec + leak-free gate written under claude/psi_v74_screening/
metadata:
  type: project
---

v7.4 hazard-covariate build (data-engineer, v0.62.0, local uncommitted): panel
`MOSAIC-data/processed/cholera/weekly/cholera_country_weekly_suitability_data_v7.4.csv`
(297 cols vs 289), feature_sets.R `v7.4` = v7.3(38) + 4 hazard channels = 42
(emdat_cyclone_prob, emdat_cyclone_prob_12w_max, drought_prob, drought_prob_26w_mean).

**INGESTION VERIFIED (yes).** feature-name-driven build is fully dynamic:
`.psi_build_data` does `feats<-intersect(features,cols)`, scaler on all feats,
tensor `ncol(X)`; FiLM trunk `n_features<-dim(X_train)[3]`. Dry build (MOZ/MWI/ETH):
X_train/X_pred grew [.,13,38]->[.,13,42], all finite, 4 hazard feats in used set.
1-epoch RW-CV fit (4 ISO, 16 folds) trained + predicted finite, no shape break.
No wiring change needed for the tensor to widen.

**WIRING GAP 1 (must fix before v7.4 CV/prod runs on the tagged panel):**
`.est_suitability_lstm_v2` (R/run_rolling_cv_suitability.R:201-202) HARDCODES
source_csv to the canonical `cholera_country_weekly_suitability_data.csv` — NO
arch_control$source_csv hook. To run on the v7.4 panel you must either write the
v7.4 panel under the canonical filename in a per-run DATA_CHOLERA_WEEKLY dir
(what the screening driver does) or add a source_csv override. Durable fix =
arch_control$source_csv.

**WIRING GAP 2 = LEAKAGE (cardinal).** `impute_cyclone_probability` /
`impute_drought_probability` (R/) fit their binomial GAMs on ALL rows — NO
cutoff/fit_date_stop arg. The baked hazard prob columns in the panel saw the
whole time series, so feeding them raw into rolling-CV leaks future outcomes.
Demonstrated: leak-free (train<=cutoff) vs baked columns differ in the OOS window
by mean|.| ~0.05-0.07. Durable fix = add gam_train_stop to both imputers +
per-fold recompute. Screening stand-in: `claude/psi_v74_screening/build_leakfree_hazard_panels.R`
refits each GAM on date<=cutoff (formulae cut verbatim: cyclone uses
s(iso_code_f,re)+s(wind)+s(wind,by=region_f)+precip/ENSO/IOD, fREML+select;
drought excludes spei_approx, iso RE + ENSO/IOD + temp/precip anoms, label =
12w-rollmean(spei)<=-0.8; integrator .complete=TRUE + country-mean fill; cyclone
12w_max = slide max .before=11 .complete=FALSE, verified 0-diff vs panel). ~4s/8-ISO
so full-40 ~15-20s/cutoff, negligible vs the LSTM.

**SCREENING SPEC (ready, NOT run — launches on dugong after baseline frees cores):**
`claude/psi_v74_screening/run_psi_v74_screening.R` + `score_*.R`.
- 4 ablation arms via arch_control$exclude_covariates on feature_set="v7.4":
  arm_base(=v7.3, verified identical), arm_cyclone(+2), arm_drought(+2), arm_v74(42).
- 3 cutoffs 2023-06-01/2024-01-01/2024-08-01; OOS = forward block cutoff..+5mo
  scored vs observed intensity (internal RW grid only selects epochs).
- Score: pooled cross-country Pearson (per-iso then median across iso+cutoff) +
  WIS-skill vs climatology; MAE reference-only.
- 5-seed logit ensemble (est_suitability does the logit pooling), parallel_seeds=1
  (SERIAL — TF oversubscription), CORES=detectCores()-30 via env, per-seed TF
  intraop capped.
- ACCEPT RULE: keep a channel-pair only if pooled OOS Pearson AND/OR WIS-skill
  improves vs base without degrading either, across >=3 cutoffs + 5 seeds, with
  bootstrap-CI lower bound of the delta >0. SHIP-ZERO (keep v7.3) legitimate.
  If exactly one pair clears, promote a 40-feat intermediate not full v7.4.

**SPEI-label flag (c):** data-engineer chose threshold -0.8 / 12w sustain / 26w
integrator. Defensible (moderate persistent deficit, half-year cholera-relevance
memory) but NOT OOS-validated. Screening sweeps them ONLY if the drought pair
clears the default-label bar (SPEI_GRID: default/harder -1.0/shorter 8w/slower
39w; set PSI_V74_SWEEP_SPEI=1 to force). No point tuning a label whose channel
earns nothing.
