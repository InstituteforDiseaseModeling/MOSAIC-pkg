---
name: warmstart-rule11-redteam
description: Red-team of warm-start rule national-exclusion-1.1 (2026-10-03) - pre-registered V6 safety gate FAILS for UGA psi_star_a under config 6.2 (validation was run on 6.1); dugong carried the rule-1.0 assembler; run_job.R never checks the rule version
metadata:
  type: project
---

Red-team of `national-exclusion-1.1` (RULE.md sha 28480519..., assembler claude/deploy_v0100/warmstart/assemble_warmstart.R sha 7092f322...), 2026-10-03, for suite v2026-10.03 on MOSAIC 0.102.0 / config 6.2.

- Spec<->code faithful (donors, zeta screen, psi guard, mixture, floor, conflict, order, no extra tempering); V2/V3/V4 reproduce under 0.102.0.
- **All statistician validation (V1-V6) ran on MOSAIC 0.101.0 / config 6.1**; the rlib became 0.102.0 at 12:57. Config 6.2 changes psi for CIV/GMB/TGO/UGA only, and the psi guard reads the loaded config's psi_jt. On the same v2026-10.02 national runs, UGA's guard flips to pass (logit-psi SD 0.56 -> 1.11), giving pooled psi_star_a/b/k in eastern/central/continental.
- Re-running pre-declared V6 on 6.2: **central_cod UGA FAILS** (dmean +4.11 [1.81, 6.50], dP(>10) +0.080 [0.025, 0.145]); eastern passes narrowly. Decomposition: psi_star_a alone drives it (mean 26.3 -> 39.1); psi_star_b alone helps; k neutral. BEN psi-only pooling is neutral, so P_psi pooling has no demonstrated benefit. CIV/TGO beta-only pooling still passes on 6.2 psi.
- Integration: dugong `~/deploy_v0100/warmstart/assemble_warmstart.R` hashed to the FROZEN 1.0 script (27a06556...) at ~13:45. run_job.R preflight accepts any "17.1+warmstart" file with a `warmstart` metadata slot (a stale v2026-10.02 1.0 file passes). The assembler compares only the national config window, not the config version. stage2_handoff.sh defaults to v2026-10.02, and dugong's v2026-10.02-wssmoke already has done/ markers, so a re-run smoke is vacuous.
- Scripts are under git-ignored claude/, so there's no version history: hash-pin them.

**Why:** RULE.md section 7 says that after a V6 failure, 1.1 is not used without a documented amendment. The minimal amendment I recommended is P = {beta_j0_tot, p_beta}, i.e. drop P_psi.
**How to apply:** if asked about rule 1.1 or 1.2, first check whether an amendment landed. Always re-run validation on the exact production install/config, and treat the psi guard as config-dependent. Scratch and evidence: claude/v0102_warmstart_rule/redteam/ (V6_uga_cfg62_*, V6_decompose_*). See [[reference-beta-psi-shape-transfer]].
