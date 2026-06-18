---
name: best-subset-preset-redteam
description: Statistical red-team (R1+R2) of mosaic_best_subset_presets() draft — R1 found gate-constant + budget-exponent defects; R2 verified the rewrite fixed budget for T<=725 but left reachability overstated, era-level drift, and an engine batch-cap ceiling
metadata:
  type: project
---

Red-team of `MOSAIC-pkg/claude/draft_mosaic_best_subset_presets.R` against
`MOSAIC-Mozambique/output/analysis/best_subset_metaparams_dataset.csv` (443 runs,
350 fit-eligible; use col `n_simulations_total`). Draft NOT merged (lives in claude/).
Scratch: `claude/preset_validation/redteam_stat/`.

**ROUND 1 (v1 draft) — found 3 defects (all real):**
1. **Percentile-basis confusion.** `ess_min` = min over ~38 marginal-ESS ≈ 2.5th pctile,
   already ≈ the prop=0.975 gate, NOT the 5th-pctile (prop=0.95) gate. The "1.34× ess_min
   → 5th pctile" step cancels against switching K 0.17→0.23; shipped K=0.23 just = raw
   ess_min curve. Lesson: when a gate references quantile q, anchor on q — `min` over k
   draws ≠ 5th pctile unless k≈20.
2. **Budget exponent ≠ engine exponent.** v1 budget = power-law (T/0.23)^(1/0.72); engine
   fits ESS~**sqrt(n)** (`calc_bookend_batch_size.R:41-96`, `run_MOSAIC_helpers.R:1545,1562`).
3. **Omitted n_params_sampled** (−0.029/param on log ess_min, p≈0).

**ROUND 2 (v2 rewrite, K=0.18, headroom=2.0) — verdict CONCERNS, conditional ship:**
- **Budget (R1#2) FIXED for the common range.** v2 budget covers the engine sqrt-need 2.0–4.3×
  at every T in 100..1000; all 90 converged runs had v2_budget(target) ≥ N_used (100%).
- **NEW Area-2 ceiling (v2 fixed wrong knob at high T):** v2 raises only `max_simulations_total`
  but leaves batch caps default → engine TRUE max = 8*500 + 10*10000 = **104k sims**. Engine can
  only converge to **ESS_param ≈ 750**; at T=1000 it needs ~185k, hits the 104k batch-cap
  UNCONVERGED regardless of v2's 318k budget. v2 accepts ess_param up to 99999 with no warning.
  Durable: `max_simulations_total` is NOT the binding cap above T≈750 — the batch-count product is.
- **Reachability (fixed-mode ESS_param=0.18*n^0.72) OVERSTATED.** "0.8× achievable so prop=0.95
  comfortably cleared" is a fitted-curve-intercept claim (0.18/0.23), not per-run. ess_min scatters
  ±81% run-to-run (sd log resid 0.595). Lognormal-CDF reconstruction (validated r=0.976 vs recorded
  ess_pct): gate clears at v2 reach for only ~**86%** of configs (model-free lower bound 71%), not
  ~95%. BLAST RADIUS SMALL: the law computes ESS_param only in FIXED mode where it's a documented
  diagnostic (doesn't drive convergence); adaptive mode honors user T verbatim.
- **NEW era-level drift (fragility):** achievable ess_min dropped ~36% across eras at fixed N+nparm
  (era0.4 coef −0.44 p=0.02; era0.3 −0.38 p<1e-4). Corpus is 248/350 from higher-ESS eras 0.1/0.2x,
  so pooled K=0.18 is optimistic for current-era runs (era0.4 n=9, directional only).
- **n_params (R1#3) still document-only.** Quantified: 43-param achieves 13% less ess_min than 38,
  31% less than 30 → v2 reach too high by ~15% at 43 params. Should enter the law, not just docs.
- **subset_size→ESS_best=round(ssz/1.13) SOUND.** On clean rows (ESS_best≥80, n_best<0.5*pool)
  median n_best/ESS_best = 1.133, no-intercept lm 1.18; robust across ETH/KEN/MOZ/NGA/SOM
  (country medians 1.15–1.19) and shape-on configs. ESS_best is the within-subset ESS target
  (`get_default_subset_tiers`→`grid_search_best_subset(target_ESS=ESS_B)`); n_best emerges. Edge:
  floor ESS_best≥30 breaks round-trip for subset_size<34 (+13% err); ssz>1130 silently caps at ~1130.
- **Foundation `ess_min∝n^0.72` shape SOUND.** β 0.70–0.78 all specs; sqrt(0.5) z=11.4 & linear(1.0)
  z=−9.0 both excluded. (Exponent drifts by era, interaction p<0.001, but the power-law shape holds.)
- **Mechanical hygiene CLEAN.** Exclusivity, floors (ess_param=10→2500), overflow guard, headroom≥1,
  finite/length checks all correct. No commission errors in code.

**Geometry confirmed inert** (doctor, on real MOZ draws): A_best/CVw_best 0.95/0.7 select identical
subset as spec 0.70/1.0. v2 correctly DROPS geometry → auto-resolves the spec divergence. Sound call.
