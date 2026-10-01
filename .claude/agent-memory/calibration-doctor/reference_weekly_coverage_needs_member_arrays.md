---
name: reference-weekly-coverage-needs-member-arrays
description: Weekly PI coverage must come from ensemble member arrays; summing daily CSV quantiles overstates coverage at small counts, for deaths and for pooled series. Also safe thresholds for posterior-collapse and medoid-coherence checks
metadata:
  type: reference
---

**Weekly intervals.** predictions_ensemble_*.csv carry DAILY quantiles only (ci_1 = 2.5/97.5%,
ci_2 = 25/75%; envelope c(0.025,0.25,0.75,0.975)). Summing daily bounds to weeks (comonotone
approximation) is fine for big counts but wrong elsewhere. Measured on the v2026-06-25 continental
run (40 loc, arrays on disk), exact vs approx weekly 95% coverage: COD/SSD/ETH within 0.03;
ZAF 0.53 vs 0.96, LBR 0.58 vs 1.00, UGA 0.18 vs 0.58, MWI 0.40 vs 0.60; deaths approx ~1.00
almost everywhere; pooled continental cases 0.80 vs 0.99, deaths 0.28 vs 1.00. Approx/exact WIS
is 0.93-1.01 for tier-A cases (median 0.99) but 0.56-1.38 for sparse deaths.
Exact path: 2_calibration/ensemble_candidate.rds with cases_array/deaths_array (needs
control$io$persist_ensemble_arrays = TRUE); flatten [param x stoch] param-fastest, weights
rep(w, times = n_stoch)/n_stoch, midpoint weighted quantiles (MOSAIC::weighted_quantiles >= v0.71.1).
Validated: recomputed daily quantiles reproduce the CSV (means exact; pre-v0.71.1 runs used
upper-edge positions, which reproduce exactly).

**Posterior collapse.** Use posterior/prior IQR ratio, not SD ratio: SD ratio is dominated by the
heavy tails of lognormal priors (zeta_ratio SD ratio 0.013 with no collapse). Min IQR ratio over
3,639 parameter rows of 32 production runs = 0.073, so < 0.02 is a safe collapse flag. BFRS from
prior draws almost never moves a posterior median outside the prior 95% (0 of 3,639).

**Medoid coherence.** National medoid/ensemble case totals 0.78-1.12; in multi-location models
per-location ratios legitimately span 0.26-14 (one member cannot match every location), so check
the pooled ratio there.

Related: [[project-v1-acceptance-rubric]].
