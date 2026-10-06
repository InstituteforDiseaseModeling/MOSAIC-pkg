---
name: rubric-amendment-1-1-0
description: Rubric 1.1.0 REGISTERED 2026-10-04 10:36:21 PDT, before any v2026-10.04 run completed. Full-window EVAL, tiers 1-2 graded (tier 3 reported), 2023+ breakdown, M-DEGENERATE on the sampling scale (log for lognormal and gamma). Evaluator 1.1.0. Thresholds 850b2258, fingerprint aabb9dd6.
metadata:
  type: project
---

**Registered 2026-10-04 10:36:21 PDT.** The suite launched at 09:21:46 under 1.0.4; no run had completed and no output had been read.
- Bundle: `claude/deploy_v0103/acceptance_v1.1.0/`; tooling in `_amend110_scratch/`.
- Thresholds `850b2258…`, fingerprint `aabb9dd6…`.
- Results: check 0 failures; 53/53 mutants rejected; tests 59/59; INSTALL OK.

**The six key-driven changes.** Each comes from a threshold key, so the evaluator falls back to the 1.0.x behaviour when the key is absent.
- `windows.eval_start` 2018-01-01. The unchanged 7-day finite rule drops the blanked burn-in, so each run starts at its first complete scored week (2018-02-19 under config 7.0). The definition is generic in effect.
- `windows.graded_tiers` [1, 2]. A week is graded only if all 7 days are tier 1 or 2; weeks with a tier-3 day go to EVAL_IMPUTED and are reported only. A day with no tier counts as graded; a config without `reported_tier` means every week is graded. 35.9% of 2018-22 case cells are imputed. BFA's 481 cases in 2025 are tier 3, so BFA is graded on 7 cases.
- `windows.breakdown_start` 2023-01-01 gives report-only EVAL_FROM_2023-01-01 rows, per location and pooled.
- `model.posterior_collapse_scale` = sampling with `log_scale_families` [lognormal, gamma]. Log scale applies only when all four quartiles are positive; every other family (beta, normal, truncnorm, uniform, unrecorded) stays natural. The effect can go either way. On v2026-10.02, CIV goes FAIL to PASS.
- Tiers, shares and the overlap use graded weeks, and the overlap now reaches back to 2018.

**Checker technique worth reusing:**
- **E1 compatibility:** the new evaluator run with the OLD thresholds must reproduce the old outputs exactly (it did on 5 suites). That proves every behaviour change comes from the new keys.
- **C structural check:** each LCS hunk of the evaluator diff must fall inside the named implementing functions (or a main-block line that reads a new key). It caught an edit inside `.tier`.

**Operational trap:** the suite froze 1.0.4. The 1.1.0 evaluator refuses that frozen copy by hash, so evaluate with the 1.1.0 bundle's own thresholds.

See [[rubric-amendment-1-0-4]] and [[rubric-amendment-1-0-3]].
