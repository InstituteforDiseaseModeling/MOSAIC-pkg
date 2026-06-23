---
name: project-eth-deaths-cfr-dwell-mismatch
description: ETH deaths over-prediction = systematic gamma_1/chi chain-factor drift; the v15.10 ETH x0.40 stop-gap was REMOVED and replaced by the B1 global re-anchor (priors_default v15.14 / config_default v4.4, recalibration-gated)
metadata:
  type: project
---

# ETH deaths over-prediction: gamma_1/chi chain-factor drift -> B1 global re-anchor

**The mechanism.** The realized reported CFR is
`reported_CFR = mu_j_baseline * chi * rho_deaths / (gamma_1 * rho)` (see
[[reference_cfr_mu_j0_identity]]; the deaths likelihood identifies the PRODUCT
mu_j*rho_deaths, not the factors). The CFR->mu derivation in make_priors_default.R
balances the chain factor (chi/gamma_1) at the PRIOR centers. But calibration pulls
gamma_1 DOWN and chi UP, so the realized chain factor drops below the prior chain
factor and implied CFR inflates. Cases are unaffected (mu_j and rho_deaths never
enter the case channel). ETH was where it bit worst (chainF 0.68 => 1.47x inflation;
deaths over-predicted ~3.3x, realized CFR ~3.7% vs observed ~1.2%).

**CONFIRMED SYSTEMATIC, not ETH-specific (Stage-1 review, 2026-06-22).** 19
single-location fits (`output/full_metapop/stage1_individual/`): gamma_1 pulled DOWN
in 16/19 (posterior median ~0.092 vs the derivation's frozen lognormal-MEAN 0.1133),
chi pulled UP (posterior-implied avg ~0.70 vs prior-mean avg 0.639). Median chainF
~0.80 => median ~1.25x implied-CFR inflation purely from chain-factor drift. Implied
CFR over-predicts observed by ~2.19x on the MEDIAN ensemble (central_method-
independent); ~1.25x of that is the chain-factor drift, the residual ~1.8x is the
deaths-vs-N structural under-identification + central_method unmasking
([[project_central_method_v038]]) and is NOT chased.

**RESOLUTION — B1 static re-anchor (priors_default v15.14 / config_default v4.4,
2026-06-22, recalibration-gated, NOT yet committed).** Per statistician spec
`claude/prior_fix_spec/SPEC_prior_fixes.md`:
- Re-anchored the CFR->mu derivation inputs from the prior centers to
  posterior-consistent static anchors: gamma_1 0.1133 -> **0.10** (the lognormal
  MEDIAN exp(meanlog), = the already-shipped config scalar), chi 0.639 -> **0.70**.
- `cfr_to_mu_adjustment` 0.183 -> 0.147 => every per-country mu_j_baseline Gamma
  center scales **x0.805** (~1.25x deaths/implied-CFR reduction; cases untouched).
  The exact ratio tracks the shipped rho/rho_deaths means (they cancel in new/old).
- **The v15.10 ETH-only x0.40 stop-gap is REMOVED** (subsumed; keeping both would
  double-count). ETH's derived mu goes 0.00088 (stop-gapped) -> 0.00177 (the
  unstopgapped baseline 0.0022 x 0.805). ETH still carries the most extreme chainF,
  so recalibration should re-check whether a SMALL residual ETH factor (~0.85, NOT
  0.40) is needed — flagged, NOT pre-applied.
- Biological sign-off: gamma_1=0.10 => 10d dwell, the shipped recovery-rate prior
  median, in the cholera infectious-period literature (~7-14d). chi=0.70 is an
  interior cohort average between endemic (0.52) and epidemic (0.76) prior means.
- gamma_1 prior WIDTH NOT narrowed (deferred recovery-rate biology check).

**B1 vs B2.** B1 is the static re-anchor (one constant, make_priors_default.R only).
B2 (the durable end-state) would couple mu_j_baseline to the SAMPLED gamma_1/chi at
sample time so implied CFR is drift-invariant — a sample_parameters.R refactor (swe),
Phase 2, gated on Stage-C recalibration confirming B1 delivers the predicted CFR
reduction. B1 first because it removes the dominant center bias cheaply and lets us
validate the chain-factor hypothesis before paying for B2.

**Build/sync discipline (Lesson #12).** The two objects are built by independent
scripts: ALWAYS rebuild priors first, `devtools::install`, then rebuild config
(config sources mu_j_baseline + beta_j0_tot from the installed priors). The env-
desync fail-loud assert in make_config_default.R guards the window match; the
test-cfr-pipeline-consistency.R drift guard asserts config$mu_j_baseline == priors
Gamma mean per country. Both pass at v15.14/v4.4.

**How to apply:** if you change ETH's (or any country's) mu_j_baseline again, do it
in priors_default first, then rebuild/sync config, then confirm
test-cfr-pipeline-consistency.R passes. The B1 re-anchor is now the baseline — do
NOT re-introduce the x0.40 ETH stop-gap. When B2 lands, the chain factor enters the
identity at its realized value and even the B1 static anchor becomes unnecessary.
