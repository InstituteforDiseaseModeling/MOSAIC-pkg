---
name: v0102-psi-collapse-review
description: v0.102.0 red-team (2026-10-03) of the calibrate_psi_predictions floor->identity rule + config v6.2 psi re-correction; how to verify a post-hoc psi re-correction without the fit's diagnostics, what the rule really tests, and which test mutants survive
metadata:
  type: project
---

Reviewed fix/v0102-sparse (d50d46f59; config v6.2 rda 2fa0f56b..., json b9d7feca...). Verdict APPROVE-WITH-NITS:
data and config verified clean, design caveats handed to ml-scientist.

**What the rule really is.** `sd(a_c*x + b_c) < amp_range[1]*sd(x)` reduces to `a_c < amp_range[1]` (the
clamped slope). It is a slope threshold, not an identification test. The docs and the warning say
"the outbreak weeks do not identify that slope", but a slope of 0.40 at t = 146 is also "collapsed".
It is also discontinuous: slope 0.49 vs 0.51 gave mean psi 0.174 vs 0.497. TGO's C3 floor map
(A = 0.501) flips its level 3.8x against the 6 other 2026 refits (fit at 0.546-0.772, t ~0.8). UGA is
window-sensitive: its 2018+ outbreak slope is 0.81 ("fit"), but the production 2015+ fit is clamped.

**Verifying a psi re-correction when the run saved no per-country diagnostics** (attrs are lost at
`el[, cols]` in run_rolling_cv_suitability, so the manifest never has them):
- Recover each country's logit-affine map from the day file: `lm(logit(psi) ~ logit(pred_smooth))`,
  max residual ~1e-11.
- Classify each map: floor-blend has A near 0.5; ceiling-blend has A near 2; clamp-only has B = +/-4
  exactly (only offset-only clamping can avoid a blend).
- Reconcile against the run log's "N identity / N guarded" warning counts. C3: 9 identity, plus
  4 floor + 5 ceiling = 9 guarded, so TGO is settled.
- Decompose maps on the 0.02 grid: CIV/GMB/UGA have A = 0.505 exactly, so w = 0.66 and a_c = 0.25.
  TGO is NOT slope-clamped (it would need w = 0.6652). "Guard-constant map" claims therefore need a
  per-country check.
- Field-level diff of the CSVs with `paste -d'|' old new | awk`: only psi/pred_bias_corrected changed,
  in exactly 4 x 3406 day rows.

**Test gaps (mutation, 2026-10-03).**
- Killed: revert to v0.101.0, and identity output via a `plogis(xall)` round trip.
- Survived: floor threshold 0.30, 0.65, or hard-coded 0.5 instead of `amp_range[1]` (every collapse
  fixture sits at a_c = 0.25, and the only fit fixture is at 0.7). Also survived: a collapsed country
  that also fires the "guarded" warning (testthat 3e lets extra warnings bubble up).

**Ceiling asymmetry (open).** 4 of the 5 ceiling maps are guard constants too, e.g. BFA B = 2.24 = 0.56 x 4
and SWZ = 0.34 x (4, 4). Left unchanged by design.

**Why:** the next psi refit will re-roll TGO/UGA across the threshold. The rule's docs over-claim.
**How to apply:** on any psi refit or rule change, re-run the map-recovery + log-count reconciliation.
Ask for an identification-based trigger and for calibration_diagnostics persisted in the manifest.
Related: [[config-default-psi-provenance]], [[suitability-anchor-floor-traps]], [[reviewer-checklist]].
