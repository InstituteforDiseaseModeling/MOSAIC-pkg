---
name: deploy-chain-review-checklist
description: Recurring traps when reviewing claude/deploy_v0103 (dugong production chain): exit-0-on-failure steps, live-vs-pinned script execution, env knob leaks, stale hardcoded suite tags
metadata:
  type: project
---

Traps found reviewing the deploy chain v1.5 change (2026-10-21, pin 73b46815):

- `run_job.R` steps 2 (R_eff) and 3 (figures) catch errors and still exit 0; only the postflight
  (step 4) sets the exit code. Any wrapper that treats rc 0 as "outputs exist" (post_pass.sh,
  STAGE2_STATUS) is wrong unless it re-checks `reproductive_numbers.csv` + `.render_complete`.
- launch.sh copies run_job/lane/stage2_watch/post_pass into `_control/` (refuse-on-diff), but
  `warmstart/stage2_handoff.sh` and the assemblers run LIVE from ~/deploy_v0103 (assemblers are
  hash-checked by the handoff; the handoff itself is not). An unattended trigger 30 h later runs
  whatever is in the deploy dir then.
- launch.sh scrubs run_job knobs from the operator shell but not the handoff's knobs
  (WS_RULE, V3, STEP, SMOKE_SUITE, EXPECT_ASSEMBLER_SHA256, RULE_MD, AMENDMENT_DOC); anything
  spawned by launch.sh inherits them.
- Package 1.4.x `.mosaic_run_status()`: `completed_unconverged` is emitted whether or not
  outputs_ok, so it does not imply a populated ensemble (no `_partial` variant for it).
- RUNBOOK has hardcoded `v2026-10.04*` tags in watchdog commands (sections 1, 3, 6); the watchdog
  exits 0 immediately on an old suite whose job is done -> silent loss of protection.
- Dry runs: fake Rscript (`#!/bin/sh exit 0`) in claude/redteam/deploy/ exercises lanes/post pass.

**Why:** these are the failure classes where a green status hides a missing artifact or an
unreviewed script. **How to apply:** check each on any future deploy_v0103 diff.
