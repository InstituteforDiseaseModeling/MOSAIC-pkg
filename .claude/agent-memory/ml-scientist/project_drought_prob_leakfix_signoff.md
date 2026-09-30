---
name: drought-prob-leakfix-signoff
description: 2026-09-30 sign-off of the lagged/observed-climate drought_prob GAM. Old one was a circular spei nowcast with no lead skill; new one has real 12w lead skill. Affects v7.4 only, not the v7.3 production refit.
metadata:
  type: project
---
I signed off the drought_prob fix on 2026-09-30 (data-pipeline-07: local climate lagged by sustain_weeks, fit capped at climate_obs_stop). It is correct and leak-free.

**Why the old one was bad:** the old GAM used concurrent precip/temp, which are the ingredients of the SPEI label. It scored AUC 0.97 on the concurrent label, both in-sample and at an OOS cutoff of 2023-01-01. On the label 12 weeks ahead it scored only **0.54**: it was a nowcast of spei_approx, which is already an LSTM feature, and not a lead-time signal. The new GAM scores 0.83 on the concurrent label and **0.66 at 12 weeks ahead** (OOS). Deviance explained drops from 69.2% to 28.5% and r(old,new) is 0.60. This is expected and fine.

**Association with psi:** the median |Spearman| against target_D is similar or slightly higher for the new version (point 0.06-0.14 vs 0.10-0.14; the 26w integrator 0.17-0.20 vs 0.11). Signs are mixed across countries, as expected for a weak signal. The integrator gain may be an autocorrelation artefact, so do not oversell it.

**How to apply:**
- Only feature_set "v7.4" contains drought_prob/drought_prob_26w_mean. The v7.3 production refit is unaffected.
- Any v7.4-vs-v7.3 screening done on an old panel is stale for the drought channels. Re-screen it on a recompiled panel.
- The horizon is the earlier of the ERA5 end (2026-09-09) and the observed ENSO/IOD end (2026-08-30). It is binding on the teleconnection side.
- Residuals that are not leaks: spei_approx standardisation is a full-window climatology, and the week that straddles the ERA5 end mixes up to 3 days of projection.
- Screen harness: MOSAIC-pkg/claude/h3_drought_screen/screen.R. Related: [[project-v74-hazard-ingestion-and-screening]].
