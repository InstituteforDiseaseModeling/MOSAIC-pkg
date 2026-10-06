---
name: warmstart-rule-1-2
description: Warm-start rule national-exclusion-1.2 (Amendment A1, P = beta_j0_tot, p_beta) validated V1-V6 on the production install 2026-10-03 and chosen for v2026-10.03 stage 2; assembler 83d343bd; plus the validation-harness traps that bit
metadata:
  type: project
---

**Decision.** Rule 1.2 is chosen for v2026-10.03 stage 2.
- Rule 1.1 failed its own V6 on config 6.2. UGA's psi guard flips there, and pooled psi_star_a made UGA in
  central_cod more explosive.
- Amendment A1 (sha a3d55186…) drops psi_star_* from P.
- On MOSAIC 0.102.0 / config 6.2 with the v2026-10.02 nationals, 1.2 passed V1–V6:
  - V6: beta-only pools for BEN BFA CIV TGO are less explosive than base, with every CI below 0;
  - RWA and UGA get no pooled entry (pools wider than base);
  - V7 (consequence only): west R-POOLED-RWIS 1.305 → 1.165.

**The validated build.**
- Assembler `assemble_warmstart.R` sha `83d343bd…`; the fallback is the frozen `assemble_warmstart_rule1.0.R`
  (sha `27a06556…`).
- `stage2_handoff.sh` (WS_RULE=1.2 or 1.0) checks the assembler hash before running, and checks the
  provenance of every staged file afterwards.
- Validation artifacts are in `claude/v0102_warmstart_rule/validation_1.2/`.

**Why:** the user's binding rule is "1.2 if V1–V6 pass, else frozen 1.0, no further amendment".

**How to apply:**
- Never edit the assembler after recording `EXPECT_ASSEMBLER_SHA256`; a re-validation would be needed.
- The 1.0/1.1 priors objects must stay byte-identical. So new provenance goes into a 1.2-only metadata field
  (`assembler_sha256`) and into the manifest, which is not byte-compared.
- Traps that produced false results while validating:
  1. `system2("env", …)` subprocesses inherit the harness's `Sys.setenv()` values (ALLOW_CONFIG_MISMATCH,
     ASSEMBLER_FILE). Two negative tests passed vacuously until the variables were unset.
  2. A fake `$HOME` moves R's user library, so MOSAIC's dependencies disappear. Put the real user library in
     R_LIBS.
  3. run_job.R treats a file without `rule_version` as rule 1.0 even when it carries a pooling record; the
     handoff's provenance check covers this.
- Related: [[warmstart-rule-1-1-pooled-excluded]], [[g2-pilot-prereg-d4-ramp]].
