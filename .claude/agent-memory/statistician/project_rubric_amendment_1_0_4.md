---
name: rubric-amendment-1-0-4
description: Rubric 1.0.4 REGISTERED 2026-10-04 09:17:47 PDT (plan-only; 50k national / 250k continental, regional runs removed). Thresholds 643243b7, fingerprint 1376de74. The frozen evaluator skips a scale with no planned runs (criteria absent, not N/A). 1.1.0 (full-window EVAL + M-DEGENERATE sampling scale) is due before the first national run completes.
metadata:
  type: project
---

**Registered 2026-10-04 09:17:47 PDT.** Bundle `claude/deploy_v0103/acceptance_v1.0.4/`; tooling in `_amend104_scratch/`.
- Thresholds `643243b7…`, fingerprint `1376de74…`.
- Results: check 0 failures before and after registration; 44/44 mutants rejected; tests 48/48; INSTALL OK on e6beba91; prior bundles byte-identical.

**Why:** the user's decisions of 2026-10-04 08:55.
- The G2 pilot was killed at ~10% of wave 1 with no job done; it is archived on dugong. G3 is skipped.
- The suite runs at national 50k x 5 and continental 250k x 5, with no regional runs.
- 1.0.3's 60k minimum would make the preflight refuse the national runs. Its 4 planned regional runs would fail M-COMPLETE and R-COMPLETE, giving NO-GO (INCOMPLETE SUITE).

**Key finding.** With `"regional": {}`, the frozen evaluator in official mode evaluates no regional run, and `.scale_block()` returns early for that scale.
- No R-* row is emitted at all: the criteria are absent, not N/A.
- The report skips the section, and the verdict counts only emitted rows. So no evaluator change was needed.
- Tested on a no-regional view of v2026-10.02. 1.0.3 on the same view flags the 4 missing runs.

**Checker techniques:**
- B2 rebuilds the new text from the old by replacing the changed keys' line blocks, using a bracket-depth block finder. This handles the line-count change.
- E requires 1.0.4 output = 1.0.3 output minus its regional-scale rows, differing only in M-BUDGET.

**Disclosed:**
- The v1.0 claim now covers national and continental only; the user's original criterion named individual-country, regional and all-country fits.
- Seen before amending: the v2026-10.03 official NO-GO (CIV M-DEGENERATE; west R-POOLED-RWIS 1.252), G1 (plumbing, 300 sims) and the killed pilot (none of its outputs evaluated).

**Next (due before the first v2026-10.04 national run completes, ~3 h after launch): rubric 1.1.0.** User decisions of ~09:05:
- the evaluation window becomes the full fitted window;
- M-DEGENERATE moves to each parameter's sampling scale: log for lognormal, also checking gamma and other positive-skew families; natural scale otherwise.

1.1.0 also needs:
- a rule for imputed weeks (tier 3);
- tiers and shares recomputed on the new window;
- the overlap window extended to 2018;
- a report-only 2023+ breakdown;
- EVALUATOR_VERSION bumped;
- tests, checker, mutants and an E-check.

See [[rubric-amendment-1-0-3]] and [[warmstart-2018-suite-rule]].
