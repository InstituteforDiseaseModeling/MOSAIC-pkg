---
name: weighted-kde-bandwidth-trap
description: Kish-n_eff weighted nrd0 bandwidth (.bw_nrd0_weighted, v0.99.11 h5-stats) blows up as n_eff->1 via n_eff/(n_eff-1) SD correction; KL underestimates exactly in the tempered ESS~1 regime
metadata:
  type: project
---
`.bw_nrd0_weighted()` (R/calc_kl_divergence.R) bias-corrects the weighted SD by n_eff/(n_eff-1). One-hot weights return NA, but near-one-hot (w = .999/.001, n_eff 1.002) inflates SD ~22x, so the KDE is wide and KL comes out ~1.4 for a near point mass (the true value is large). Tempered best-subset weighting really does reach ESS_B ~1, so this regime shows up in production.

**Why:** found in the red-team of fix/handoff-h5-stats (2026-09-30). Suggested fix: return NA when n_eff < 2, and drop the correction or cap it.

**How to apply:** when reviewing any weighted KDE or bandwidth code, probe n_eff in {1, 1.01, 4, 100}. Check that the results are continuous in that range and stay NA or large near a point mass. See [[test-suite-isolation-traps]].
