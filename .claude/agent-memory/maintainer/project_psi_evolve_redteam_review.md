---
name: psi-evolve-redteam-review
description: Red-team review of claude/psi_evolve/ (2026-09-21) — the three-estimator MAE divergence reproduced exactly, the seasonal-baseline index misalignment, and the block-10 EVAL_GRID cascade
metadata:
  type: project
---

Red-team review of `MOSAIC-pkg/claude/psi_evolve/` at commit `a0d9e1879` (branch
`feature/psi-12wk-evolve`). Facts worth keeping because they are expensive to re-derive.

**Why:** the programme runs 3-4 parallel MAE scorers over the same cells, and two
numbers on record are wrong in ways that static reading missed.

**How to apply:** on any future psi_evolve / forecast-CV scoring review, re-run these
reproductions before trusting a table.

### The "different cell filters" story was wrong — it is the ESTIMATOR
`FINAL_REPORT.md` explains persistence MAE 0.1348 vs 0.1490 as "the two scorers use
different cell filters". The cell sets are IDENTICAL (90 country-blocks, both gated on
>=4 window cells and >=8 pre-cutoff obs). The whole gap is the aggregation unit.
Reproduced to 4 dp from the frozen panel (`..._frozen_2026-09-17.csv`) + observations
alone — no psi cache needed, because persistence is a function of obs:
- **A** weighted.mean over country-BLOCKS (`accuracy_table.R`) = **0.1348**
- **B** per-country mean over blocks, then burden-weight (`arm_C_transforms.R` baseline
  rows) = **0.1490**
- **C** pool all blocks' cells per country, then burden-weight (`arm_C_transforms.R`
  `acc()`, i.e. the ARM rows) = **0.1494**
Driver: AGO appears in 2 of 6 selection blocks and LBR in 5 (all others 6), so A gives
them 2/5 units of weight and B/C give them 1. On the rebuilt panel the same three
estimators give 0.1298 / 0.1450 / (n/a). **arm_C's table compares arms (C) against
baselines (B)** — only 0.3% apart, but it contradicts its own "one table, one cell set".

### `psi_<cutoff>.csv` is DAILY; `obs` is WEEKLY (Thursdays)
`prefit_rolling_cv_psi()` copies `pred_psi_suitability_day.csv` verbatim. Every scorer
`merge()`s down to ~13 weekly cells per country-block — except `arm_C_transforms.R:612`,
which indexes the DAILY baseline vector positionally: `b$point[seq_len(nrow(o))]`. Benign
for `persistence` (constant within a block), **wrong for `seasonal`**: it pairs 13 weekly
observations with the climatology of the first ~13 CALENDAR DAYS. Reproduced exactly:
as-coded **0.1975** (the number in REGISTRY row 112), correctly aligned **0.1935**. So
"TWO ARMS NOW BEAT THE CLIMATOLOGY BASELINE" is really one (C7c_C9d 0.1921, +0.7%);
C7b_C9d 0.1935 ties.

### The scored window is 13.14 weeks, not 13
`test_end - test_start` = 91 days inclusive. Blocks 5/8/9/10 therefore contain a **week
14** and block 9 has no week 1. Headline MAE tables filter by DATE (include wk 14);
per-horizon tables filter `wk %in% 1:13` (drop it). "Overall MAE" and the sum of the
three horizon bands are on different cell sets.

### Fold counts: 6 of 8 geometry presets verified, 2 wrong
Re-derived via `.psi_load_arch_control()` + `.psi_make_rw_cv_steps()` over the 9 cutoffs:
p000 89, p001 255, F4 229, F1 677, F2 1351, F3 912 — all match the comments and
PROTOCOL §6 exactly. **F5 = 306** (comment says "~142 -> ~230") and **F6 = 333**
(comment says "~213"). Also `F5` is a NAME COLLISION: PROTOCOL §6 registers F5 as a
7-day stride / 2,697 folds / "out of budget at any seed count", while `run_arm.R` and
REGISTRY row 147 define it as 84-day stride / mty 2.
F6's `min_train_years = 270/365.25` is SAFE: `int_fields` in `.psi_load_arch_control()`
does not contain `min_train_years`, so it survives as 0.7392 and
`fit_date_start + round(365.25 * 0.7392)` = exactly +270 days. Latent trap: adding it to
`int_fields` would silently make it 1 year.

### Block 10 (`split="confirmation2"`) broke two hard gates
Commit `a0d9e1879` claims it "audited every reader" by grepping `split ==` filters. It
missed the readers that count rows or ENUMERATE the legal split values:
- `run_arm.R:52` `if (length(cutoffs) != 9L) stop(...)` — 10 prod rows now, so **no arm
  can be fitted at all**.
- `score_psi_arm.R:105` `if (!all(g$split %in% c("selection","confirmation"))) stop(...)`
  — **every** `score_psi_arm()` call errors; verified by running
  `claude/psi_evolve/test_score_psi_arm.R`, which dies on line 14.
- `launch_queue.sh:31` treats 9 of 10 cutoffs as "already complete".

See [[reference-keras3-r-api-traps]] for the TFT/DLinear code findings.
