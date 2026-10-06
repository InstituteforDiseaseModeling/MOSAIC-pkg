---
name: rubric-amendment-1-0-2
description: v1.0 acceptance rubric amendment 1.0.2 (registered 2026-10-03 13:34 PDT) for MOSAIC 0.102.0 / suite v2026-10.03; plan-only; how amendments are made and checked; the traps hit
metadata:
  type: project
---

Rubric 1.0.2 is registered at `claude/deploy_v0100/acceptance_v1.0.2/` (laptop; claude/ is gitignored). Thresholds
sha256 01f53689…; bundle fingerprint sha256(PREREGISTRATION.sha256) 3a24eafe…. It changes only plan
expect_mosaic_version 0.102.0 and expect_config_version 6.2, plus metadata. EVALUATOR_VERSION stays 1.0.1 because
the evaluator logic is byte-identical.

**Why:** v2026-10.02 (0.101.0) was NO-GO under 1.0.1. It had 2 BLOCK failures (M-DEGENERATE CIV zeta_1 IQR ratio
0.015; regional/west R-POOLED-RWIS 1.305) and 4 MAJOR. The user chose a targeted fix (psi collapse fallback ->
config 6.2; warm-start rule 1.1) and the SAME thresholds. He explicitly rejected post-results scorecard changes,
including the M-DEGENERATE natural-vs-log IQR scale and the R-POOLED-RWIS pooled-vs-additive WIS.

**How to apply:**
- Any further release (0.102.1, or the v0103_* rlibs seen on the laptop) needs a new amendment. M-PROVENANCE
  pins the version exactly.
- The amendment recipe:
  - copy the bundle (not out/, test_views/ or .claude/);
  - edit the plan and metadata as text, so the diff stays at the changed lines;
  - adapt check_amendment.R: A = the chain of published hashes; B = leaf-by-leaf under both parses, plus a
    text-level line check; C = the evaluator differs only in the hash line and header comments, and every
    RUBRIC.md change sits in a [x.y.z]-tagged passage (LCS hunks);
  - mutation-test the checker (23 mutants);
  - E: compare the new evaluator with the frozen previous one on the latest real suite;
  - run verify_install.R on the built release to check the plan values;
  - write AMENDMENT.md last, then the manifest;
  - publish the manifest in the release PR body.
- Traps:
  - A unified diff of RUBRIC.md inside a ```diff fence: its own " ```" context lines close the fence. Use a
    4-backtick fence.
  - The deploy defaults (assemble_warmstart EXPECT_CONFIG=6.2) mean the rule-1.1 negative tests
    (claude/v0102_warmstart_rule/tests) need EXPECT_CONFIG set if they are re-run against a 0.101.0 library.
    Their RLIB claude/v0101_rebuild/rlib now holds 0.102.0.
- Related: [[warmstart-rule-1-1-pooled-excluded]].
