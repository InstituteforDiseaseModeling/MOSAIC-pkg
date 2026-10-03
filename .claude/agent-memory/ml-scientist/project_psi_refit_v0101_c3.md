---
name: psi-refit-v0101-c3
description: Final v0.101.0 psi = C3 (trust round-2 panel 2c575f8c, seeds 11-110; supersedes C2 0e7ec3f0) on dugong 2026-10-01. Lessons - anchor moves need nonzero fit-window targets; single-run shifts mislead, use pair means; seed set 11-110 runs low after Jun 2026 on 3 panels; SWZ level is a clamp coin flip; ZAF Jan-Mar 2027 spike in every run
metadata:
  type: project
---

The final v0.101.0 psi candidate is **C3**: panel C-trust round 2, md5 2c575f8c, from fix/v0101-trust b65cee162 and MOSAIC-data 04a6d0f. C2 (round-1 panel 0e7ec3f0) is superseded. Both are staged at `MOSAIC-pkg/claude/v0101_rebuild/psi/{C2,C2B,C3,C3B}` on the laptop; README sections 8-9 have the details. Recommendation: bake C3 (baked in 8ad91c7a4; shipped in v0.101.0).

**Lessons that held up:**
- **An anchor only rescales NONZERO targets.** BWA's cp99r moved x6.5 and then back by x0.154, yet its psi never moved: its 52 fit-window rows are AI zero-case weeks.
  - Count a country's target>0 rows in the 2015+ fit window before predicting psi impact.
  - This corrects the implication in [[panel-anchor-traps-v0101]]. Both traps there are now fixed: dc1048068, and round 2, which turned backfill off by default (802fcb062; the C3 panel was compiled at b65cee162).
- **Single-run comparisons mislead; use pair means.** Compare (run+rep)/2 differences against replicate noise / sqrt(2).
  - C2 looked 0.030 below C across the forecast window, but C2B showed only 0.007.
  - The pair means zeroed the pre-cutoff Nino4 "effect" (A-side noise).
  - The C2-pair "April rebound" turned out to be single-run highs (C2B) plus SWZ. In the C3 pair it vanished; the Oct-Nov up / Jan-Feb down signature stays.
- **Persistent seed-set level effect.** On 3 different panels, seeds 121-220 sit above seeds 11-110 from June 2026 on.
  - Pre-cutoff: +0.062 / +0.026 / +0.020. Forecast: +0.010 / +0.022 / +0.030. 2023..2026-05: about 0.
  - The country pattern is not stable (r 0.1-0.4).
  - The production seed set is systematically the low member in the end-of-training and extrapolation windows. A 20-seed pool would halve the gap. This partly explains why same-seed noise is about 0.9x disjoint.
- **SWZ's psi level is a bias-correction coin flip.** With 13 outbreak weeks and identical data, its offset sat at the +4 clamp in A, C, C2 and C2B (psi* 0.11-0.29), but was +1.36 / +2.32 in C3 / C3B (psi* 0.05-0.07; v0.100.1 0.033).
  - Treat any country with fewer than about 20 outbreak weeks near a guard as level-unidentified.
  - The other countries that flip at the 2.0 / 0.5 thresholds (BFA, CAF, LBR, ZAF; TGO, GMB) flip back between replicates.
- **Targeted target changes stay local in the calibration window but move the FiLM zone in the forecast window.**
  - Round-1 ZAF reshaping lowered snf_1 forecast psi (NAM, ZMB, SWZ, MWI).
  - Round 2 (ZAF report-dating, NAM 2025-W9 row) partly reversed it (ZMB Nov-Dec 2026 +0.15).
  - The calibration window stayed within noise both times.
- **Turning backfill off was psi-neutral**, as predicted: all 10 affected countries were within noise (same-seed 0.5-1.4x).
- **ZAF psi is 0.3-0.99 in Jan-Mar 2027 in EVERY run (v0.100.1 included).** The forecast ENSO is above the training range (ENSO4 2.17 vs a 2015-2026 max of 2.01), so this is extrapolation. ZAF's bias correction stays on the 2.0 amplitude ceiling as a coin flip.
- **The panel compile is bitwise reproducible** from the MOSAIC commit (load_all), the MOSAIC-data commit, enso_C and the peaks md5. The only input git does not pin is the gitignored `UN_world_population_prospects_daily.csv` (md5 011bc072…).
  - The bundles are at `C2/provenance/` and `C3/provenance/`.
  - When the coordinator replaces a panel in place, archive the old one and repoint the old validation script (done for C2: `panel_C_r1_0e7ec3f0.csv.gz`).
- **Throughput:** 2 concurrent runs at MOSAIC_PSI_CORE_BUDGET=85 (8 TF threads per worker) take about 25.5 min. 3 runs at 56 took 33 min.

**Why:** these are the evidence behind the v6.1 psi bake, and three traps that would have produced wrong conclusions: single-run attribution, anchor-impact assumptions, and the seed-set offset.
**How to apply:** for any psi A/B, run disjoint-seed replicates on BOTH arms and test pair means. Check fit-window target>0 counts before blaming anchors. Treat the level of SWZ-like low-outbreak countries and forecast-window psi as weakly identified. Follows [[psi-refit-v0101]] and [[psi-refit-v0100]].
