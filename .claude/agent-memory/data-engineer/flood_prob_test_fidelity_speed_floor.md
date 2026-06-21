---
name: flood-prob-test-fidelity-speed-floor
description: impute_flood_probability test speed/fidelity floor — synthetic dims, AUC margins, mgcv cold-start dominates wall time
metadata:
  type: project
---

`tests/testthat/test-impute_flood_probability.R` synthetic-panel sizing and the speed/fidelity tradeoff.

**Decision (2026-06-19):** `.mk_synth_suitability` defaults shrunk from `n_iso=5L` to `n_iso=2L` (kept `n_years=4L`). 2 ISO x 4 yr x 52 wk = 416 rows -> 210 training rows after the 52-week lag/window warm-up.

**Why:** test was ~26% of the suite. The row-shrink cuts each `mgcv::bam()` fit ~4.2s->2.0s (5 plain-fit tests + 1 diagnostics fit).

**Guards that set the floor (do not go below):**
- `nrow(train) >= 100` (R/impute_flood_probability.R ~line 243). At 2x4 train=210; at 2x3 train~106 (no margin).
- `precip_sum_52w` needs 52-wk warm-up per ISO -> first 51 rows/ISO dropped. So n_years >= 3 effectively forced (forecast year hidden).
- Recovery test asserts AUC > 0.75. At 2x3 AUC~0.76 (NO margin, fragile across seeds). At 2x4 AUC=0.82-0.88 across seeds 7/11/13/17 -> kept always-on, NOT gated. Do not reduce iso or years without re-checking AUC across all four seeds.

**How to apply:** if a future formula change drops synthetic AUC, raise n_iso/n_years rather than lowering the 0.75 threshold; the threshold is the scientific signal-recovery check.

**Non-obvious timing fact:** the test's COLD first run is ~48-50s regardless of data size — that's one-time mgcv/JIT compilation, NOT the fits. WARM steady-state (mgcv already loaded by earlier tests in a full-suite run) is what the row-shrink moves: ~28s (5-iso) -> ~19s (2-iso). Always compare warm-to-warm; a single cold `test_file` run is misleading.

**Diagnostics test (the only `diagnostics=TRUE` call):** at 2-iso the per-fold CV guards (>=100 train, >=20 val, both classes) are all unmet -> CV folds all skip -> exactly ONE GAM fit, no cv_metrics CSV (test only checks the CSV `if (file.exists)`). The remaining diagnostics cost (~5s over a plain fit) is the 14x18in ggplot `flood_gam_predictions_timeseries.png` render, which the test does NOT assert on; it's production behavior in `.write_flood_gam_diagnostics()` and was left untouched (test-only change preferred).
