---
name: project-eth-deaths-cfr-dwell-mismatch
description: ETH-only mu_j_baseline x0.40 stop-gap (priors_default v15.10 / config_default v3.8) for the gamma_1 dwell mismatch that over-predicts ETH reported deaths ~3.3x
metadata:
  type: project
---

# ETH deaths over-prediction: gamma_1 dwell mismatch + x0.40 mu_j_baseline stop-gap

**The fact.** Ethiopia's deterministic-default reported deaths over-predict ~3.3x
(realized reported CFR ~3.7% vs observed ~1.2%) while cases stay near-unbiased.
The v0.14.0 CFR->mu identity is `mu_j_baseline = CFR * gamma_1 * rho / (rho_deaths * chi)`
and plugs gamma_1 in at its PRIOR MEAN (~0.114, 8.8d infectious dwell). But ETH
calibration drifts gamma_1 to the long-dwell tail (~0.076, ~14d). Per-case CFR
scales as `mu_j / gamma_1`, so a lower realized gamma_1 inflates the realized
reported CFR. cases are unaffected because mu_j (and rho_deaths) never enter the
case channel. See [[reference_cfr_mu_j0_identity]] for the identity and the fact
that the deaths likelihood only identifies the PRODUCT mu_j*rho_deaths.

**The stop-gap.** Scale ETH's derived mu_j_baseline mean by 0.40 (CV unchanged):
- priors_default v15.10 (1093e700): Gamma rate 1816.28 -> 4540.71, shape stays 4,
  so Gamma mean 0.00220230 -> 0.00088092. Applied surgically in BOTH
  data/priors_default.rda and inst/extdata/priors_default.json, and as an
  `eth_dwell_stopgap <- 0.40` branch in data-raw/make_priors_default.R.
- config_default v3.8 (this fix): config sources mu_j_baseline directly from the
  priors_default Gamma means (make_config_default.R ~L199, `shape/rate`), so the
  x0.40 flows through by construction. The shipped data/config_default.rda was
  patched SURGICALLY (only ETH's entry + metadata version) to mirror the surgical
  priors patch and avoid input drift on the other 39 countries; a full regen would
  reproduce the same ETH value. No ETH special-casing is needed in
  make_config_default.R itself (just a provenance comment).

The x0.40 returns deterministic ETH reported CFR to ~1.5% (deaths bias ~1.3x).
GTFCC treated-CFR target is <1%.

**Why a stop-gap, not a cure.** The dwell mismatch is structural and affects ALL
countries, not just ETH — ETH is just where it currently bites worst. The durable
fixes are (a) a dwell-adjusted CFR->mu derivation (don't freeze gamma_1 at its
prior mean), and/or (b) the run_MOSAIC best-subset weighting fix — the dAIC-4
truncation currently discards the deaths signal so the ensemble reports near the
prior center. Do NOT chase the exact x0.31 point estimate from the broken
weighting; 0.40 is a deliberate round re-center, not an optimized fit.

**The drift bug this memory was created alongside.** v15.10 patched priors
(.rda + .json) but NOT config_default.rda, desyncing the two objects and failing
the Lesson-#12 drift guard test-cfr-pipeline-consistency.R
("config_default$mu_j_baseline matches priors_default Gamma mean per country").
The MOSAIC v0.44.12 fix syncs config_default to the prior mean. Both data objects
now agree per country (tol 1e-6). The two objects are built by independent scripts,
so any future mu_j_baseline edit MUST touch priors first, then rebuild/sync config.

**Why:** ETH deterministic deaths were visibly over-predicting in best-subset
smoke runs; a fast re-center was needed while the structural dwell fix is scoped.
**How to apply:** if you change ETH's mu_j_baseline again, do it in priors_default
first (the x0.40 currently lives in the Gamma rate), then re-sync config_default
and confirm test-cfr-pipeline-consistency.R passes. When the dwell-adjusted
derivation lands, REMOVE this x0.40 stop-gap (both the prior rate scaling and this
note) — it is double-counting once gamma_1 enters the identity at its realized value.
