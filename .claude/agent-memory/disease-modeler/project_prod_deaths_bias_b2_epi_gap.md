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

**FINDING 2 — PARTIALLY OVERTURNED 2026-09-23, see [[cfr-submodel-simplification-audit]].**
Correct vs *WHO annual* (geomean 1.045) but the comparator was wrong: against the data the
likelihood actually scores (`config_default$reported_cases/deaths`, multi-source weekly,
2023-2027) the centre is **1.119x high, sd(log) 0.261** over the 17 countries with >=50
observed deaths — NGA 1.42x, SOM 2.31x, CIV 1.48x, GHA 1.46x, COG 1.41x. Original text follows.

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

---

## UPDATE 2026-09-19 — B2.2 gap QUANTIFIED at production scale and measured by direct arm

Full evidence: `MOSAIC-pkg/claude/inference_lab/reports/CFR-MATH.md` (CFR-MATH-01).

**Per-country decomposition, `dugong:~/prod100k_v087` (40 locs, n=100,000):**
`predicted_CFR / observed_CFR  =  (prior median CFR_target / observed CFR)  x  chain residual`

- **chain residual, 19 countries with >=50 observed deaths: median 1.444, IQR [1.297, 1.659],
  geomean 1.420, sd(log) 0.246.**
- **`Gamma(3,6)` median of `(1+eps)` = 1.446.** Exact match. FINDING 3 is confirmed, quantitatively,
  in 19 independent countries.
- **The prior CENTRE is right**: median `prior CFR_target / observed CFR` = **1.027**,
  IQR [0.955, 1.220]. FINDING 2 confirmed.
- FINDING 4's "residual ~1.5x floor where epi~0" is SUPERSEDED: the large pred/obs outliers are
  PRIOR-CENTRING errors, not a separate floor — CMR 2.81 = prior 2.59 x chain 1.09; BFA 0.05 =
  0.05 x 1.05; LBR 4.37 = 3.45 x 1.27; NAM 3.13 = 2.80 x 1.12. Only SOM and KEN have both terms
  elevated (and their observed CFRs, 0.43%/0.68%, are plausibly under-ascertained).

**The identity is EXACT, verified at machine precision.** Over 25,000 ETH draws
`cfr_epidemic_<iso>` (from `.mosaic_add_implied_cfr_columns()`) equals
`CFR_target * (1 + mu_j_epidemic_factor)` to **max abs err 1.11e-16**, and
`cfr_baseline_<iso> = CFR_target * chi_endemic/chi_epidemic` to 5.55e-17. The shipped diagnostic
columns already REPORT the gap.

**Direct paired arm (CRN, 115 ETH posterior members):** applying
`mu_0 = CFR_target*chain/(1+eps)` gives **aggregate deaths x0.696** (analytic
`1/E_deaths-wtd[1+eps]` = 0.663), **cases invariant to <0.6%**, median member deaths bias
1.323 -> 0.886. Paired **dR2_deaths = -0.0001 +/- 0.012** => **B2.2 buys BIAS, not R2.**

**NEW, important:** once B2.2 is in, the deaths LEVEL becomes **invariant to the eps prior**
(B2.2 + Gamma(3,6) = x0.6959; B2.2 + Gamma(1,8) = x0.6973). So B2.2 and an eps re-shape are
SUBSTITUTES for the bias, not complements — do not count both. Ship B2.2 on its own merits.

**REVISED PRIORITIZED FIX #2.** The old note said "lowering the mu_j_epidemic_factor center is the
WRONG fix — the +50% surge is literature-anchored". **That premise is now falsified** — see
[[mu-j-epidemic-factor-prior]]: the +50% is UNCITED, and in the periods the ENGINE flags the
observed CFR is 0.41-0.54x the endemic CFR. Re-shape it to `Gamma(1,8)` for correctness, but
expect no additional bias gain on top of B2.2.

**HARD ORDERING CONSTRAINT (new).** B2.2 must ship AFTER the A1b likelihood fix. Under the
baseline `-y*log(1e6)` zero rule a x0.70 on mu costs ~525 nats and the sampler undoes it by
re-selecting high-`CFR_target` draws; under A1b it gains ~145 nats. See
[[deaths-channel-limits]].
