---
name: coarsened-posterior-v130
description: MOSAIC 1.3.0 (branch feat/v1.2.0-optimize-subset-default, 2026-10-13) made the coarsened posterior (alpha=10) the default + SIR ensemble + new auto gate; traps found while implementing
metadata:
  type: project
---

1.3.0 default `best_subset_weighting = "coarsened"`: w ∝ exp(eta(ll-max)), eta = alpha/n_obs
(alpha 10, n_obs = scored location-days). Quantiles/posteriors.json from weight_all over
is_finite; ensemble members = systematic resample K = clamp(round(Kish), min/max_best_subset),
count/K weights in is_best_subset/weight_best. "saturated" kept, bit-identical to v1.2.1 (stub golden
fixture tests/testthat/fixtures/stub_posterior_golden_v121.rds). Auto gate = function-specific ESS
(Owen, Kish-bounded) on gate weights + seed-batch ratio < batch_ratio_max (0.2); linear-vs-sqrt
better-R2 growth model.

**Why:** saturated subset was n=108 everywhere by arithmetic; KDE marginal ESS passed a point mass.

**How to apply / traps found (all CONFIRMED):**
- cumsum(w/sum(w)) can exceed 1 a few ulp BEFORE its last element -> findInterval() errors.
  Cap with pmin(...,1). Hit on real national draws; [[systematic-resample-cumsum]].
- Interpolated (type-7) quantile threshold disagrees with the integer convergence count: with
  39 params and prop 40/41 it sits between min and 2nd-min, closing the predictive phase without
  convergence (4/28 replays). Use order statistic k = n - ceil(prop*n) + 1, and compare counts
  with ceiling(prop*n - 1e-9) (prop from JSON = 40/41 + 1e-16).
- Replay (28 national, prod targets ESS_param 1000): 11 converge (median 27.8k draws), 15 hit 50k,
  GHA/UGA stop at 33-36k when 10 predictive fallback batches of 1000 run out.
- Ensemble cost: unique members 97-1500 (median 990) at max_best_subset 1500 vs ~108 before.
- load_all master + PSOCK workers = workers run the INSTALLED package; every sim fails silently
  (0/500 successful). Real runs need R CMD INSTALL into a lib + R_LIBS for workers.
- n_obs counts location-days even under cases_scoring="weekly" (flatter posterior there) — open.
