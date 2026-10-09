---
name: rubric-amendment-1-1-2
description: Rubric 1.1.2 REGISTERED 2026-10-08 21:36:42 PDT for continental-only suite v2026-10.08-continental (MOSAIC 1.1.1); thresholds 5867d424, fingerprint 088c3036; plus a registered warm-start cross-check file (assembler vs 1.1.x eval_start trap); stage 2 fell back to rule 1.0 (V6 ZAF, again)
metadata:
  type: project
---

**Registered 2026-10-08 21:36:42 PDT**, before any v2026-10.08-continental output.
- Bundle `claude/deploy_v0103/acceptance_v1.1.2/`; tooling in `_amend112_scratch/`.
- Thresholds `5867d424…`, fingerprint `088c3036…`.
- Results: check 0 failures; 76/76 mutants; tests 63/63; E1 and E2 pass; INSTALL OK on build 18d5244ab.

**Plan changes from 1.1.1:**
- `expect_mosaic_version` 1.1.1;
- `national` becomes `[]`; the evaluator skips the scale, as it did for `regional: {}`;
- `continental` is 1.1.0's ssa block, line for line.

**Trap: the assembler 83d343bd cannot run against any 1.1.x rubric.**
- It hard-codes E3/E4 `eval_start` 2023-01-01 and cross-checks it against `rb$windows$eval_start`. Rubric 1.1.0
  moved that to 2018, so the cross-check dies.
- v2026-10.04 escaped only because its frozen rubric was 1.0.4.
- Fix used: register `warmstart_crosscheck_thresholds.json` in the bundle. It is the thresholds file with exactly
  `rubric_version` and `eval_start` changed, and check W verifies it.
- Never edit the assembler; see [[warmstart-rule-1-2]].

**Stage 2, built on the laptop:**
- 11 of 28 runs excluded: BEN, BFA (E1 NA r2 and E2 zeta), CIV, GHA, KEN, NER, TCD, TGO, TZA (E4), UGA, ZAF.
- Rule 1.2 passed V1 and V3, then failed V6 on ZAF in southern_moz (8.07 → 16.98), the same as v2026-10.04.
- So the frozen rule 1.0 was released, at `claude/deploy_v111/warmstart_staging/`.
- The file records the laptop `national_dir`. The env.sh template sets `EXPECT_WS_NATIONAL_DIR` to that path.
  Laptop and dugong inputs were checked md5-identical, 336/336.

**Why:** the pooled ZAF `beta_j0_tot` (southern donors) is reliably more explosive than base. Expect rule 1.0 at
every stage 2 until the rule changes.

**Provenance gap found in 1.1.1:** 1.1.1 cites build a66a19d2, but v2026-10.07 ran merge ca7eca12. Its config 7.1
differs (psi D was rebuilt under the same label). 1.1.2 checks data identity against ca7eca12.

See [[rubric-amendment-1-1-0]] and [[warmstart-2018-suite-rule]].
