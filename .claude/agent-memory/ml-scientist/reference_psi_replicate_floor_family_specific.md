---
name: psi-replicate-floor-is-family-specific
description: The psi_evolve "0.1% MAE replicate floor" is an LSTM-family measurement; the DLinear family's own replicate pair spreads 1.59% — never reuse one family's noise floor to certify another's effect
metadata:
  type: reference
---

Measured replicate pairs (identical config, disjoint seed block, n_seeds = 10, 6 selection
blocks, burden-weighted raw-psi MAE over the 13-week test window):

| pair | MAEs | spread |
|---|---|---|
| P000 / P000R (LSTM) | 0.1996 / 0.1998 | **0.10%** |
| NDe / NDeR (DLinear) | 0.1748 / 0.1776 | **1.59%** — 16x larger |

The 0.10% figure is the one the registry uses to certify claims like "D9b is 39x the noise"
(REGISTRY.tsv:130). It does **not** transfer: on the DLinear family the whole refinement ladder
(ND 0.1772, NDe 0.1748, NDr 0.1810, NDk9 0.1804, NDi 0.1776, NDeR 0.1776 — 3.49% span) sits
inside roughly two NDe/NDeR spreads, so no DLinear refinement is resolvable at 10 seeds.

Same lesson at the gate level: `promote_gate.R` verdicts for one configuration family land at
ND 4/6, NDe 3/6, NDeR 2/6 blocks — the pass/fail depends on the seed block alone. The timing gate
(`diag_blend_needs_psi.R:166`) already had its threshold raised from 0.5% to a measured 2.57pp
replicate floor for the same reason.

**Rule:** measure the replicate floor *within the arm family you are testing* before quoting a
multiple-of-noise. A floor inherited from a different architecture is not a floor.

Related: [[nd-dlinear-evidence-audit]]
