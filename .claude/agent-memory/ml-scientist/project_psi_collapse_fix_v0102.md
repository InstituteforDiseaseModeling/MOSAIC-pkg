---
name: psi-collapse-fix-v0102
description: v0.102.0 psi bias-correction rule - a fit below the 0.5x amplitude floor falls back to identity; why the old floor map was guard constants, the evidence, and the OOS finding that the per-country correction does not beat identity even where well identified
metadata:
  type: project
---

**Fact.** From v0.102.0, `calibrate_psi_predictions()` does not apply a fit whose slope would push psi's logit amplitude below `amp_range[1]` (0.5). The country gets identity instead, with status "collapsed".
- The rule was re-applied to C3 without retraining. CIV, GMB, TGO and UGA change; the other 36 countries are byte-identical.
- Staged at `claude/v0102_psi/` on the laptop. The evidence harness is in `work/` there.

**Why the old floor map was not an estimate.**
- The slope was clamped to 0.25 and the OLS intercept kept (not re-fitted), then clamped. The 0.02-grid blend then landed at w = 0.66, giving A = 0.505 and B = 0.66 × b_c.
- CIV got (0.505, −2.64) in all 7 of the 2026 refits.
- `check_psi_amplitude` cannot flag floor maps: 0.505 is above its 0.5 trigger, while 0.498 would be flagged, so flagging is a coin flip.
- The floor binds exactly when the outbreak-week slope is unidentified. On C3's 2018+ window, t is −2.1..1.6 for the floor countries and ≥ 3.4 for every fitted country.
- CIV's LSTM output sits at the 0.01 ensemble clamp for all of 2018-2023. The floor map's only effect was to squash the 2024-26 excursions.

**Evidence that held up.**
- **In-sample:** the native LSTM amplitude is about calibrated against the target. The all-weeks calibration slope has median 1.15, and 26/29 countries are above 0.75.
- **OOS, the floor countries:** forward-chaining over the full-manifest fold predictions shows identity and the floor map as a statistical tie. Slope-1 + offset refits are worse and eps-unstable.
- **OOS, well-identified countries (big finding):** where the correction is a genuine fit, identity beats it on Brier and BCE in both forward-chaining variants, with country-clustered bootstrap CIs excluding 0.
- **OOS calibration slope median 0.34:** the LSTM is about 3x over-dispersed out of sample.
- **Implication:** the per-country correction's OOS value is unproven. Post-1.0 candidates are to drop it, or to refit it on held-out fold predictions.
- **The logit-OLS level is eps-sensitive** through capped targets: logit(1 − 1e−6) = +13.8. CIV's slope-1 offset is 3.77 at eps 1e−6 and 2.68 at 1e−2.

**Re-application traps.**
- The stored day file starts 2018-01-01, but the fit uses outbreak weeks from about 2015-03, so any REFIT rule cannot be reproduced from stored files (CIV has 38 pre-2018 outbreak weeks, UGA 96, TGO 6). Identity can be.
- Maps are recoverable exactly: they are logit-affine with R² = 1. Floor status may need the log's guarded count; TGO's A = 0.501 was resolved because 4 floor + 5 ceiling candidates = 9 guarded.
- **TGO is threshold-borderline.** It hit the floor only in C3; 6 of 7 refits fitted it at 0.55-0.77. Under the new rule its level swings 4x across replicates. An identifiability (t-stat) trigger is the follow-up.

**Why:** these facts stopped a guard artifact being mistaken for a calibrated psi, and they reframe the bias correction as unvalidated OOS.
**How to apply:**
- Treat a floor or collapse status as "correction not identified".
- Never present the per-country correction as OOS-validated.
- Score post-hoc psi maps with fold-prediction forward-chaining and a country-clustered bootstrap, reporting in-sample and OOS separately.
- See also [[psi-G-redteam-v4]] (where the B1 guards came from) and [[psi-refit-v0101-c3]].
