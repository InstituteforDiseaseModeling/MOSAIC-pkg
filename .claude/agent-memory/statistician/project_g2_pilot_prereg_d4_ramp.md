---
name: g2-pilot-prereg-d4-ramp
description: G2 window-pilot pre-registration (D1 gate, fingerprint a322e9e4, 2026-10-03 13:58), the D4 continental-ramp decision (KEEP), and the 1.0.3 quiet_start trap
metadata:
  type: project
---

**G2 pre-registration** is at `claude/plan_2018_start/g2_prereg/` (laptop): G2_PREREGISTRATION.md, g2_run.sh
and g2_decide.R. Fingerprint `a322e9e4…`, registered 2026-10-03 13:58 PDT, before any v2026-10.03 or 2018
output.
- **Design.** P = 8 national runs at a 2018 start, 0.103.0, 30k x 5 (GHA TCD BFA NAM SSD TGO + MOZ ETH).
  R = the same countries from v2026-10.03.
- **Machinery.**
  - The frozen 1.0.2 evaluator in test mode, with R mounted as P's national baseline via a MOSAIC-results-layout
    symlink view. Its OVL machinery then gives the paired rWIS on common weeks.
  - A second test-mode run gives R's location passes.
  - extinction_monitor.R runs on both arms.
- **GO iff all of:**
  - no new BLOCK failure (excluding M-PROVENANCE and M-BUDGET);
  - N-RWIS-CASES <= 1.10;
  - N-MATERIAL-REGRESSIONS <= 2;
  - no country with extinct_wt(P) >= 0.5 where R < 0.5 (the cases line is the weighted median, so 0.5 means
    the line itself is extinct);
  - N-RWIS-DEATHS <= 1.25;
  - cases passes P >= R - 1.
- It judges the composite (window + nu split + IC seeding + data rebuild) and makes no attribution.

**D4 (continental time ramp): KEEP**, `export CONT_TIME_RAMP=1`.
- The continental baseline v2026-06-25.01 was itself a 2018-start model with the identical 0.667→1.333 ramp over
  its whole window.
- Re-anchoring at 2023 keys the fit to the scorecard window and buys nothing: the pre-2023 share of weighted LL
  is 41.2% (keep) vs 40.9% (re-anchor) vs 49.5% (flat) for cases; for deaths 32.2% / 31.7% / 39.9%.

**Why:** the user accepted the re-ignition risk with G2 as the gate, and overrode deaths_score_start (deaths are
scored over the full window).

**How to apply:**
- Never re-run or re-tune G2 after seeing results.
- The pilot must run the exact suite build. Its runner needs the 1.0.2 rubric copy plus EXPECT_* overrides,
  because 1.0.3's 60k minimum would fail its budget preflight.
- For 1.0.3: the evaluator uses union(plan$quiet_start, the run's quiet_start_seeded). Replacing the plan list
  with the v18.0 list therefore only REMOVES the R² downgrade for COG and RWA in the 2018 suite. Disclose this,
  and have the user confirm.
- See [[rubric-amendment-1-0-2]].
