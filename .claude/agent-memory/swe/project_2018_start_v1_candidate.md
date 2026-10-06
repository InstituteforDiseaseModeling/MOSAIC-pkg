---
name: 2018-start-v1-candidate
description: 2026-10-03 user instruction to move config_default to a 2018-01-01 start, rerun all 33 models with larger budgets, and make that suite the v1.0 candidate; scoping plan location and the non-obvious blockers it found
metadata:
  type: project
---

**User instruction (2026-10-03, via coordinator):** "change the default config in the pkg to start at
2018 and rerun all models". The 2018-start suite is **the v1.0 candidate**: frozen rubric,
plan-only amendment before results, v1.0.0 only on GO / GO-WITH-CAVEATS. Budgets go up. Sequenced
AFTER v0.102.0 (psi fix + warm-start rule, config v6.2) and the v2026-10.03 suite. Target 0.103.0,
priors v18.0, config v7.0.

**Scoping plan:** `MOSAIC-pkg/claude/plan_2018_start/PLAN.md` (laptop), with all scripts and CSVs
alongside it. It recommends 2x budgets (60k x 5 / 200k x 3 / 500k x 5), about 58 h on dugong; the
window alone at the current budget is about 32 h.

**Why:** the user rejected 2018 on 2026-10-01 (it confounded v0.55 -> v0.100), then chose it again
for v1.0.

**How to apply:**
- **Keep the rubric's `windows.eval_start` at 2023-01-01.** It is a frozen performance block (check_amendment PERF_KEYS) and matches the baselines' 2023+ scoring; the warm-start assembler dies if it moves.
- **Amend only plan keys:** expect_date_start, versions, n_sims_plan_min, quiet_start. M-BUDGET is planned >= min for simulations but EXACT for national/regional n_iterations.
- **The main scientific risk is re-ignition.** 22 of 28 national models need re-ignition after 52+ silent weeks, with gaps up to 8 years; the engine has no importation term.
- **Gates before the suite:** an 8-country window pilot at the current budget, and a continental ensemble memory/time smoke (0.87-1.44 TB projected at 168 workers).

**Phase 1.5 done (2026-10-03):** branch `feat/v0103-2018-start` (worktree `.claude/worktrees/v0103`,
base = main b3a5d5a98's tree) has the nu split merged, D8 applied, the 0.102.0 red-team fixes, and the
FINAL objects: priors v18.0 (rda 174987ce...), config v7.0 (rda 16727817...), built in three passes
with passes 2 and 3 byte-identical ([[data-object-rebuild-traps]]). 18 quiet starts (D8 acceptance
set). Not pushed; the coordinator red-teams, then a PR.

**Phase 2 bundle:** `claude/deploy_v0103/`. It has BUDGET_SCALE (2 = suite, 1 = G2 pilot in *-pilot
suites), G3 (*-g3), the 1.0.3 rubric without an invented hash, `monitors/mem_watchdog.sh` and
RUNBOOK.md ([[suite-lane-kill-and-check-traps]]). D4 = keep the ramp (env.sh template
CONT_TIME_RAMP=1, CONT_WEIGHT_DEATHS=1). Its `warmstart/` is deploy_v0100's validated rule 1.2
with only the 2018 defaults changed: the assembler sha256 moved 83d343bd... -> 2a09c9d1..., and
stage2_handoff.sh pins the new hash. run_job.R refuses a version-stripped pooled warm start.

Related: [[window-and-ensemble-cost-scaling]], [[production-suite-protocol]].
