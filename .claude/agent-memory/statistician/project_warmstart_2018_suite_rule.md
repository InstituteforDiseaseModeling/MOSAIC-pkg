---
name: warmstart-2018-suite-rule
description: Warm-start rule for the 2018 suite v2026-10.04 (ACCEPTED 2026-10-03). Rule 1.2 unchanged; stage-2 V1/V3/V6, with V6 in A1's v2026-10.02 context and only the P priors swapped; fallback frozen 1.0. Generalised V6 harness 22f367bf reproduces A1 exactly; arm A comes from the merge commit's rda, not the JSON.
metadata:
  type: project
---

**Accepted (2026-10-03): rule `national-exclusion-1.2` unchanged for v2026-10.04.** This is option (a), registered as section A2 of rubric 1.0.3.
- Assembler 83d343bd, run with EXPECT_CONFIG 7.0, EXPECT_PRIORS 18.0 and RUBRIC_JSON set to the suite's 1.0.3 copy.
- Fallback: the frozen 1.0 script, 27a06556.
- A2 lives as a section of 1.0.3's AMENDMENT.md, not as a separate file, for two reasons:
  - the assembler hard-codes A1's hash;
  - `run_job.R` checks only the RULE.md and A1 hashes, so no code would read an A2 file.
- The text is section A2 of `claude/deploy_v0103/_amend103_scratch/AMENDMENT.template.md`. The plan_2018_start draft is superseded.

**Why V6 must be re-run.**
- V6 is data-dependent: the 2018 exclusion set, the donors' 2018 posteriors and the v18.0 base all change.
- The guards (no wider than base, median inside base's 95%) admit a pooled entry centred above base's median.
- A 2018-native V6 is impossible before stage 2, because its regional background is stage 2's output.
- G2 cannot serve: national only, 8 countries, and it is itself 2018 output.

**V6 design.**
- **Background:** A1's exact context: the v2026-10.02 regional runs, the 0.102.0 install, config 6.2 psi and `psi_star` from v17.1.
- **Arms:** A = the v18.0 base entries for P; B = the 2018 build.
- **Harness changes needed:** a loop over keys with pooled entries (A1's harness asserts west-only), and arm A read from the merged priors_default.json.
- **Acceptance check:** given A1's inputs, the harness must reproduce A1's table exactly.
- **Rejected:** a v2026-10.03 background. It adds a completion dependency and changes the context, so the test would no longer isolate the priors.

**Harness, built and validated 2026-10-03:**
- File: `claude/v0102_warmstart_rule/validation_2018/V6_prior_predictive_general.R`, sha256 22f367bf, chmod 444.
- Env: WS_DIR, ARM_A_FILE, ARM_A_LABEL, BG_ROOT, OUT, NC.
- **Must run on the 0.102.0 rlib** (`claude/v0101_rebuild/rlib`). It dies on any other version, so do not let anyone reinstall that rlib before stage 2.
- Regression: on A1's inputs it gives summary, bootstrap and all 2,592 draws identical to A1.
- Functional test: a synthetic eastern_eth UGA pooled entry, with arm A from the v17.1 rda; west is still exact.
- Negative control: that UGA entry shifted up fails, exit 1.
- **Arm A must be `data/priors_default.rda` at the merge commit, not the inst/extdata JSON.** The JSON twin rounds to 16 significant digits (relative error about 1e-15). That breaks byte-exact reproduction, and the rda is the object the assembler's install loads.

**How to apply:** at 1.0.3 registration, fold in:
- the A2 section;
- the §2.1 "model under test" line: rule 1.2, not the stale "1.1" in the Task-2 plan;
- one sentence in the thresholds `amendment` text.

See [[warmstart-rule-1-2]] and [[rubric-amendment-1-0-2]].
