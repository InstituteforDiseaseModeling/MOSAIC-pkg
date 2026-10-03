---
name: v0100-rehearsal-cases-bias
description: v0.100.1 national rehearsal (2026-10-01) cases-level NO-GO is genuine trough over-prediction plus engine-only intervals, NOT mean-vs-median skew; a cases median line gives 1.217 (MAJOR) but the share criteria stay BLOCK on coverage; sum-of-daily-medians collapses sparse series
metadata:
  type: project
---

**Fact (frozen-rubric test-mode scoring of 28/28 national runs).** Everything passes except cases:
- N-BIAS-CASES 1.416 (BLOCK);
- N-SHARE-CASES-A 2/15 (BLOCK);
- N-SHARE-CASES-B 1/10 (BLOCK).

Recomputed exactly with the evaluator's own helpers: `MOSAIC-pkg/claude/stat_v0100/task3_central.R`.

- **The cases median line is not a fix.** Sum of daily weighted medians gives N-BIAS 1.217 (MAJOR), but
  shares stay 2/15 and 2/10 (BLOCK) and N-R2-VS-BASELINE goes to -0.0065 (MAJOR).
  - About a quarter (22-27%) of its bias "gain" is aggregation (corrected from "about half": 1.217 vs
    1.26-1.27 for the median of totals, against the mean's 1.416): the daily median of bursty or sparse series collapses
    (BFA 2.45 -> 0.505, CAF 0.99 -> 0.578; the deaths median-collapse problem again).
  - The coherent median of the predictive window TOTAL still gives 1.27.
- **Skew is small; the bias is genuine.**
  - The predictive-total mean/median ratio has median 1.03.
  - The observed totals sit at a median PIT of 0.27 in the member-total distribution (tier A 0.38, tier B
    0.135; p ~ 0.02 under calibration).
  - The excess is in TROUGHS. Low-tercile pred/obs is 1.25-276x and the high tercile is 0.72-0.98 in 10/15
    tier A: dynamic-range compression.
  - 9 of the 13 tier-A share failures are cov95, almost all low-side misses (KEN: 72% of weeks below the
    5% PIT, 5% above the 95%).
- **The engine-only intervals omit the likelihood's NB noise.** With NB(k_run) added:
  - tier-A coverage passes go 4/15 -> 15/15;
  - N-COV50 goes 0.365 -> 0.708;
  - WIS improves by 10.5% (a proper score, so this is not tuning). The exception is clamped-k CMR.

**Projection.** Re-selecting 13 of the 25 tier A/B countries under weekly scoring, with the rest frozen:
- N-BIAS-CASES 1.416 -> 1.09 (PASS);
- N-SHARE-A 3/15 under weekly scoring alone, 10/15 (MAJOR) with observation-predictive intervals added;
- N-SHARE-B stays ~1/10 (BLOCK). Its failures are sparse and quiet-start bias plus no-skill fits
  (BEN, UGA, ZAF), i.e. structural to closed national models.

**Why it matters:** the decision levers for v0.100.2 are the likelihood (weekly scoring; censored-k
handling) and coherent observation-predictive intervals, not the central line. See
[[daily-vs-weekly-cases-scoring]] and [[nb-dispersion-estimator-traps]].

**Outcome (v0.101.0, released 2026-10-02; there was no v0.100.2):** observation-level intervals shipped;
the weekly rule was built but BLOCKED at the likelihood gate, so daily stays the default; clamped-at-0.1 k
fits take the panel trend; and the user set the cases central line to the engine-level weighted MEDIAN
(deaths mean).

**How to apply:** do not recommend `central_method` changes to pass a bias-of-sums criterion. The mean
is the right point forecast for totals. Check per-tercile ratios before calling a level bias "skew".
