---
name: psi-refit-v0101
description: v0.101.0 psi refit ROUND 1 on dugong (2026-10-01) - A/B/C 10-seed runs concurrently in 33 min; the Nino4 fix (C) shifts forecast-window psi systematically (34/40, ~2x noise) but the pre-cutoff C>A shift is A-side noise; recommended C at the time, superseded by C3 (shipped); CIV/SWZ/TGO level shifts come from bias-correction refits
metadata:
  type: project
---

The first round of the v0.101.0 psi refit ran on dugong on 2026-10-01; the shipped psi is the later C3 ([[psi-refit-v0101-c3]]). It is staged at `MOSAIC-pkg/claude/v0101_rebuild/psi/{A,B,C}` on the laptop, with a README. Panel md5 5bd9956a; cutoff 2026-09-17 (auto); horizon 2027-04-29.

- **Throughput.** Three concurrent runs, each with parallel_seeds=10 at `MOSAIC_PSI_CORE_BUDGET=56` (5 TF threads per worker), took 33 min each. Load average was about 300 on 176 cores, but the CPU was 31% idle and there were about 2.6M context switches/s. Load average overstates saturation for TF; check vmstat idle before relaunching.
- **Replicate floor.** A vs B (disjoint seeds), 2023+: median r 0.967, mean|diff| 0.028. In the forecast window: 0.941 and 0.035.
- **Same-seed noise.** Same seeds in a new process (A vs C on identical inputs, control windows) gives about 0.8x the disjoint-seed noise. Same seeds buy only about 20% noise reduction, so they are not bitwise pairing.
- **Control trap.** A sign test that looks systematic needs a control window and a replicate comparison. In 2026-08-01..09-17, C>A held in 38/40 countries (p=1e-9), but B>A also held in 34/40. A was the low outlier at the end of training, which is the noisiest window (|A-B| 0.072 vs 0.019 in 2023-24). Only the forecast window showed a real Nino4 effect: C>A 34/40 with B>A 25/40 (not significant), median |A-C| 0.065 vs 0.035, and Oct-Nov pooled +0.07.
- **Level shifts relative to v0.100.1 are reproducible across A/B/C** and are set by the per-country logit-affine bias correction.
  - CIV went from slope 1.52 / offset +3.62 to the amplitude-floor map 0.51 / -2.64, so psi fell x0.04. Outbreak weeks dropped from 108 to 72 once AI rows were removed.
  - SWZ hit the +4 offset clamp with only 13 outbreak weeks (psi x8.4).
  - TGO x2.7.
  - The effective map can be recovered exactly from the outputs: logit(psi) = A*logit(pred_smooth) + B with R^2 = 1. An offline re-fit cannot be done from the day CSV, because the in-run fit also uses 2015-2017 rows.
- **Forecast-window psi moved far from v0.100.1** (r 0.59). The cause is covariate refreshes (flood GAM via IOD lags and 52-week precipitation sums, plus the ERA5 swap), not the Nino4 artifact. C vs v0.100.1 has r 0.21.

**Why:** these are the decision numbers behind the v6.1 psi choice, and the controls are what kept the pre-cutoff signal from being misread.
**How to apply:** reuse `scripts/validate_psi.R` and the control-window sign test for any psi A/B. The ENSO input issue is in [[enso-bom-relative-gapfill]] and the anchor traps in [[panel-anchor-traps-v0101]]. This follows [[psi-refit-v0100]] and is followed by [[psi-refit-v0101-c3]].
