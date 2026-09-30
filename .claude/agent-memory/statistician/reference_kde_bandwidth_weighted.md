---
name: kde-bandwidth-weighted
description: Any KDE of an importance-weighted sample must take its bandwidth from the WEIGHTED spread and Kish n_eff, not bw.nrd0(draws) — unweighted bw smooths a concentrated posterior back to the prior (KL 3.19 -> ~1.3)
metadata:
  type: reference
---

Invariant: for a weighted sample, bandwidth = 0.9 * min(sd_w, IQR_w/1.34) * n_eff^(-1/5), with
n_eff = 1/sum(w^2) (normalised) and sd_w bias-corrected by n_eff/(n_eff-1). Equal weights must
reduce EXACTLY to stats::bw.nrd0 (tested). n_eff < 2 -> bandwidth undefined -> return NA,
never a finite KL. Guarding only exact one-hot is NOT enough: the n_eff/(n_eff-1) correction
diverges as n_eff -> 1, so weights (0.999, 0.001) inflated sd ~22x and gave posterior KL ~2 for a
near point mass (red-team catch; tempered best-subset weights really reach ESS ~1).

**Why:** stats::density(weights=) ignores the weights when choosing bw. IS-weighted U(0,1) draws
targeting N(0.5,0.01): analytic KL 3.19; unweighted bw (~0.066) gave ~1.3; weighted bw gives 3.08
(the gap is the KDE's own bw^2 inflation). Reported as KL ~0.85 for point-mass weights on U(0,1).

**Where:** shared core `.kl_divergence_kde()` + `.bw_nrd0_weighted()` in R/calc_kl_divergence.R,
used by both `calc_kl_divergence()` and `.mosaic_posterior_kl()` (fix/handoff-h5-stats,
2026-09-30). Second trap in the same estimator: a single grid over the pooled range plateaus near
log(n_grid) once P is narrower than a cell — integrate on P's own support. Check any other
weighted-KDE site (calc_weighted_mode, ess_marginal "kde") for the same unweighted-bw defect.
Related: [[weighting-scheme-invariants]].
