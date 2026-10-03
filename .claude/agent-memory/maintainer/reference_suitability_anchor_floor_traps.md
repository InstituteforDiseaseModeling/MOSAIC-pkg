---
name: suitability-anchor-floor-traps
description: compile_suitability_data target_D anchor traps - the 5-case floor's median population is taken over non-AI rows, so floor-bound anchors move when the AI row set changes; quote leak ratios on the log1p scale; how to isolate a target change's effect on the psi bias correction
metadata:
  type: reference
---

Learned verifying the v0.101 trust commits (fix/v0101-trust, 2026-10-01).

- **The floor is not AI-invariant.** `.csd_response_targets()` sets
  cp99r = max(p99 of trusted-row rates, 5 / median(pop) * 1e5). The median population is
  taken over `pop_rows = !is_ai` (before dc1048068 this was the old `trusted_mask`, the
  same set), and empty grid cells count too. An AI-free surveillance input, or an AI cap,
  turns AI weeks into NA-source grid rows, so the median population changes. That moves
  the anchor of every floor-bound country (11 in v0.101: BFA, BWA, ERI, GAB, GMB, GNB,
  GNQ, MLI, MRT, SEN, SWZ), by up to x1.17. Real case: GNB moved x1.056 between v0.100.1
  and v0.101 purely from AI rows 638 -> 407. The staged psi README still said "the
  imputed cap cannot move the anchors". Any comment or claim that anchors are
  "INVARIANT to include_ai" is false for floor-bound countries.
- **Leak ratios go on the log1p scale.** The target is log1p(rate)/log1p(cp99r). A raw
  cp99r ratio overstates the effect on targets once rates are near 1 per 100k. For ZAF at
  a 2022-12-29 cutoff the raw ratio is 43x but the target-scale ratio is 36x.
- **Isolating a target change's effect on the bias correction.** Hold a staged run's
  `pred_smooth` fixed. Re-run `calibrate_psi_predictions(..., obs_col = "intensity")`
  with the old and then the new panel target_D. The staged weekly file starts at
  `pred_date_start`, so this reproduces exactly (to 2e-15) only for countries with no
  observations between 2015 and that start date. Recover a run's effective map by
  regressing logit(psi) on logit(pred_smooth); R^2 = 1.
- **Amplitude flags at 2.0 are a coin flip.** BFA, CAF, LBR, SWZ and ZAF sit at amp_ratio
  of about 2.00 across replicates. Do not credit or blame a change for a flag flip unless
  disjoint-seed replicates agree.

Related: [[config-default-psi-provenance]], [[reviewer-checklist]].
