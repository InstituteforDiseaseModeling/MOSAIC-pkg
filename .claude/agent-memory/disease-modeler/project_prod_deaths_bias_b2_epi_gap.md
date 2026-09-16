---
name: project-prod-deaths-bias-b2-epi-gap
description: 2026-07-01 promoted-production (v2026-07-01.01, priors 15.18) deaths-bias diagnosis — B2 CFR_target derivation CONFIRMED live/correct, CFR_target prior CORRECTLY centered on observed WHO-GAM CFR; residual ~2x deaths bias is the mu_j_epidemic_factor (1+epi) runtime escalation NOT folded into the B2 derivation, plus ensemble up-weighting of high-death draws
metadata:
  type: project
---

# Production deaths-bias @ v2026-07-01.01 (priors 15.18 / config 4.7): B2 works, the epidemic-factor gap is the culprit

**Data source.** 27 promoted national medoid configs at
`/Users/johngiles/MOSAIC/MOSAIC-results/models/national/<ISO>/v2026-07-01.01/2_calibration/best_model/config_medoid.json`
+ per-model `manifest.json` metrics (local laptop). 12-country sample: ETH COD NGA SSD TCD ZWE MOZ MWI COG BFA LBR UGA.

**FINDING 1 — B2 is LIVE and EXACT.** For every medoid, the B2 identity
`mu_j_baseline = CFR_target*(1-exp(-gamma_1))*rho/(rho_deaths*chi_epidemic)` reproduces
`CFR_target` to 3 decimals when inverted. The chain-factor drift that drove the
B1/ETH-x0.40 stop-gap era is STRUCTURALLY CURED — that old diagnosis
([[project_eth_deaths_cfr_dwell_mismatch]]) is now OBSOLETE for these runs. Do NOT
reintroduce chain-factor re-anchors.

**FINDING 2 — CFR_target prior is CORRECTLY centered, NOT miscentered ~2x.** The
CFR_target lognormal median == the observed WHO-GAM reported CFR (2021-25 mean) by
construction, verified against `model/input/param_mu_disease_mortality.csv`. Medoid
CFR_target vs prior median is MIXED (NGA 2.28x, ETH 1.62x UP; SSD 0.82, ZWE 0.55, MOZ
0.77, UGA 0.27 DOWN) — mean ~1.0. So CFR_target posterior drift is NOT the systematic
driver. (The earlier [[project_deaths_bias_cfr_target_drift]] "CFR_target drifts 2-3.5x"
was a pre-B2 observation; under B2 CFR_target is the *anchor*, and it is not systematically high.)

**FINDING 3 — THE CULPRIT: the mu_j_epidemic_factor (1+epi) runtime escalation is NOT
in the B2 derivation.** Engine (laser-cholera infectious.py:81):
`mu_jt = mu_j_baseline*(1+mu_j_slope*t)*(1+mu_j_epidemic_factor*epidemic_flag)`; deaths
drawn from mu_jt (the config's stored `mu_jt` MATRIX is vestigial — engine recomputes it;
the 3-30x-inflated stored mu_jt in medoids is a red herring for deaths). B2 derives
mu_j_baseline so the *baseline* implied CFR == CFR_target, but reported cases are an Isym
read dominated by epidemic-flagged ticks, so the REALIZED reported CFR ≈
CFR_target*(1+mu_j_epidemic_factor). Since epidemic_flag is ~1 for essentially all ticks
in these fits, the deaths bias tracks (1+epi): ZWE bias2.40/(1+1.198)=1.09, MWI
1.40/1.58=0.89, COD 2.13/1.75=1.22, COG 3.76/2.40=1.56 — largely explained. B2 cancels
chi_epidemic but forgot the (1+epi) term that multiplies mu on the SAME epidemic ticks.

**FINDING 4 — residual ~1.5x floor even where epi≈0.** SSD epi=0.06 but deaths bias 2.19;
ETH residual 1.69, NGA 1.62, MOZ 1.63 after dividing out (1+epi). manifest
cfr_implied_predicted (ENSEMBLE) is ~2-2.9x cfr_implied_observed AND higher than the
medoid CFR_target (ETH ensemble 3.8% vs medoid CFR_target 1.955% vs observed 1.3%). The
extra gap = the ensemble UP-WEIGHTING high-death parameter sets (higher CFR_target and/or
higher epi draws survive best-subset weighting because the deaths channel is
under-identified vs N). This is a scoring/weighting problem, not biology — hand to
statistician / run_MOSAIC best-subset owner.

**PRIORITIZED FIX (biology lane):**
1. **B2.2 — fold the epidemic factor into the B2 derivation** (dominant, structural).
   Derive `mu_j_baseline = CFR_target*(1-exp(-gamma_1))*rho / (rho_deaths*chi_epidemic*(1+mu_j_epidemic_factor))`
   so realized epidemic-tick reported CFR == CFR_target. Same architecture as B2, one more
   sampled term in the denominator (sample_parameters.R B2.1 block). RECALIBRATION-GATED.
   Requires swe for the sample-time plumbing; DM owns the identity. ~1.5x reduction, all countries.
2. **Lower the mu_j_epidemic_factor center** is the WRONG fix — the +50% surge is literature-
   anchored (v15.18 note) and biologically real; the bug is that it's double-applied, not too big.
   Do NOT shrink it to chase deaths bias.
3. **BFA/LBR CFR clamp** — observed CFR ~0.03%/0% clamped up to the 0.2% floor
   (make_priors_default.R L1804 `max(...,0.002)`); minor, low-signal, leave unless BFA/LBR
   deaths matter.
4. **Ensemble up-weighting of high-death draws** (Finding 4 residual) — NOT a prior problem;
   statistician / best-subset weighting.

**How to apply.** B2.2 is the durable next step, gated on a fresh calibration confirming the
predicted ~1.5x deaths reduction with cases untouched (mu never enters the case channel).
Validate on ETH/SSD/COG (high bias, spread of epi). Bump priors_default version + rebuild.
