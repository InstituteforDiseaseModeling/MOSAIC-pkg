---
name: forecast-cv-horizon-scoring
description: forecast-CV re-scoring gotchas — cumulative-window duplication at long horizons, baseline reconstructable from predictions parquet, hindcast-not-forecast framing
metadata:
  type: project
---

Re-scoring the rolling-origin forecast-CV artifact (`output/validation/forecast_cv/**/predictions_all.parquet`) at horizons beyond the shipped 1/2/3-mo.

**Baseline is reconstructable from the parquet alone.** `evaluate_rolling_cv()` rebuilds the seasonal (week-of-year climatology, requires >=2yr IS history) and persistence baselines internally from the `segment=="IS"` block of `predictions_all`. So ANY horizon can be re-scored post-hoc from the parquet — no need to re-run sims. Internal helpers `MOSAIC:::.rcv_window_metrics` (returns n,R2_corr,R2_sse,bias_ratio,cov50,cov95,wis,mae,rmse) and `.rcv_skill` are directly callable.

**Cumulative-window duplication trap.** OOS<=h windows are cumulative and CAP at available data. A cutoff whose scoreable horizon is only ~5mo (later origins) still produces a scored cell for OOS<=6mo, <=7mo, ... <=29mo — all IDENTICAL (the full ~5mo slice), mislabeled as long-horizon forecasts. Pooling these inflates n and biases long-h medians toward short forecasts. **Fix:** only keep a (cutoff,h) cell if that cutoff's max scoreable dd actually reaches ~h months.
- **Why:** scoreable horizon = (common data end ~2026-06-07) − cutoff, so it shrinks monotonically across quarterly origins (2024-01 ≈29mo scoreable down to 2026-01 ≈5mo). Predicted horizon is longer (13–37mo).

**Cumulative vs non-cumulative tell different coverage stories.** Cumulative cov95 for cases looks stable ~0.5 because early well-covered months dominate the average; the NON-cumulative per-month view reveals cases 95% PI coverage collapses 0.64(m1)→~0.5(m2-8)→<0.35(m11+)→<0.2(m24+). Always compute non-cumulative bins for honest decay.

**Framing:** spec.json labels this experiment "HINDCAST / conditional model skill, realized covariates -- NOT forecast skill." Do not present as forecast skill without that caveat.
