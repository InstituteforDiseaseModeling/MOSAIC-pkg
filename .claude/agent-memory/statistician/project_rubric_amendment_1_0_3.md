---
name: rubric-amendment-1-0-3
description: Rubric 1.0.3 (2018-start v1.0 candidate) REGISTERED 2026-10-03 18:40:58 PDT on merge e6beba91 (PR #135). Thresholds df63c969, fingerprint 67d62fad. Six plan keys; plan bound to the merge commit (B3); E allows 3 difference classes; 39/39 mutants; A2's V3 uses the hand-off's V3 mode.
metadata:
  type: project
---

**REGISTERED 2026-10-03 18:40:58 PDT** with `finalize_1.0.3.sh` on merge e6beba91d (PR #135; same tree as 840272a1c).
- Thresholds `df63c969…`, fingerprint `67d62fad…`.
- Results: check 0 failures before and after registration; tests 47/47; 39/39 mutants rejected; INSTALL OK on `git archive e6beba91`; prior bundles byte-identical. The log is `_amend103_scratch/finalize_registration.log`.
- **Pre-registration fix to A2:** the swe found my literal V3 command ran the assembler in an unscrubbed shell, and its `ls` loop would count `validation/` as a differing file. Either would cause a false V3 fail and force the rule-1.0 fallback.
  - A2 now cites the hand-off's V3 mode, `V3=1 SUITE=$SUITE bash ~/deploy_v0103/warmstart/stage2_handoff.sh` (hand-off a4a366fc at registration).
  - The V6 and fallback text was aligned with RUNBOOK §4 (9b02d16d): rsync to and from the laptop, md5 of the rda (174987ce), the harness sha256, and keeping `warmstart_staging_rule1.2/`.
- **Lesson:** never pre-register a hand-typed operational command when a deploy script will own that step. Cite the script's mode and the criterion instead.

**Location.** The bundle is `claude/deploy_v0103/acceptance_v1.0.3/` (laptop). It still holds three placeholders in RUBRIC.md: `@@MERGE_SHA@@`, `@@AMENDED_ON@@` and `@@SEEN_V2026_10_03@@`.
- `PREREGISTRATION.sha256` now exists; `launch.sh` treats that as "registered". Never edit the bundle again; changes need a 1.0.4.
- Tooling is in `claude/deploy_v0103/_amend103_scratch/`: `finalize_1.0.3.sh`, `fill_amendment.py`, `set_lock.py`, `mutants.R`, `run_evals.sh`, `AMENDMENT.template.md`, and the snapshot of the prior bundles.

**To register:**
```
MERGE_SHA=<40hex> SEEN_FILE=<sentence> bash finalize_1.0.3.sh
```
- The SEEN sentence follows "Suite v2026-10.03 (MOSAIC 0.102.0, config 6.2, rule 1.2 warm starts):".
- The script verifies the build, fills the placeholders, re-locks, then runs the tests, the check, the mutants, verify_install on `git archive`, AMENDMENT.md, the manifest and the post-registration check.
- If the date is no longer 2026-10-03, the thresholds hash moves and the E evaluations re-run (~5 min). If v2026-10.03 has been pulled, both evaluators also run on it.
- The provisional thresholds hash with amended_on 2026-10-03 is df63c969.

**Plan changes:**
- 0.103.0 / 7.0 / 18.0 / 2018-01-01;
- minimums 60k / 200k / 500k;
- quiet_start = priors 18.0's 18-location list, in build order: BDI BEN BFA CAF CIV CMR GHA GIN NAM NER RWA SSD SWZ TCD TGO UGA ZAF ZWE.
- Unchanged: date_stop, eval_start 2023-01-01, iterations 5/3/null, success fraction, central line.
- **RWA stays Q** (D8 seeded it). Only COG leaves; the coordinator's "COG/RWA" wording came from the stale 17-list.

**Checker additions:**
- **B3** binds the plan to the merge commit: DESCRIPTION, config rda version and window, priors JSON version and quiet list (in order), and the likelihood tag `R/v0.103.0+near_bound_k_trend`.
- **E** allows exactly three difference classes: M-PROVENANCE; M-BUDGET; and the quiet-start consequences. Those are the L-CASES-R2 rows of moved locations, their quiet_start flag, and the cases share rows of their scale, with the same denominators.
- **F** checks for placeholders only outside fenced blocks, because the verbatim mutant output quotes `@@VERSION@@`.

**Disclosed effects:**
- v2026-10.02 under 1.0.3: BDI's L-CASES-R2 goes FAIL to PASS; BEN goes FAIL to N/A; N-SHARE-CASES-A 0.600 to 0.667.
- On the baseline view, COG goes PASS to FAIL at all three scales.

**Why:** the user rejected post-results rubric changes. The bundle must cite the exact merged versions and the post-F4 code, so registration waits for the merge SHA.

**How to apply:** do not edit the bundle after registration. If the merged build differs from the plan (quiet list, date_stop or tag), finalize stops at step 1; amend the plan by hand before registering.

See [[rubric-amendment-1-0-2]], [[warmstart-2018-suite-rule]] and [[nb-dispersion-estimator-traps]].
