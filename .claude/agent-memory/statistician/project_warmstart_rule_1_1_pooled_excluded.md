---
name: warmstart-rule-1-1-pooled-excluded
description: national-exclusion-1.1 (pre-registered 2026-10-03, sha256 28480519...cd3e4c) gives warm-start-EXCLUDED countries a linear-opinion-pool prior from zeta-compatible folded donors of their home region; on v2026-10.02 only west BEN/BFA/CIV/TGO change; key traps: exclusion is MNAR, A6 substitution != pooled prior, empty named list
metadata:
  type: project
---
**What exists.** The rule is in `claude/v0102_warmstart_rule/RULE.md` on the laptop. It is read-only, sha256 `28480519ab02c3a54876df313656079d1175d9d5fe80e90bb0a09ad9d6cd3e4c`, registered 2026-10-03 12:12:58 PDT. It is implemented in `claude/deploy_v0100/warmstart/assemble_warmstart.R`; `RULE_VERSION` defaults to 1.1, and 1.0 reproduces the frozen `assemble_warmstart_rule1.0.R` byte for byte. It targets MOSAIC 0.102.0 / suite v2026-10.03. It extends [[warmstart-v0100-design]].

**The rule.** It applies to an excluded country *e* (E1–E4 failed) and the pooled set P = `beta_j0_tot`, `p_beta`, `psi_star_a/b/z/k`:
- **Donors:** the folded runs in the union of the run_job.R regions containing *e*. Each donor's national zeta_1 95% interval must contain the consensus, the median of the folded national log-medians. With fewer than 3 donors, use all zeta-compatible folded runs; with still fewer, base.
- **Pool:** an equal-weight linear opinion pool of the donors' *final warm entries*, which are already tempered x2, using exact mixture moments. Never a product pool; see [[posterior-pooling-counts-prior-J-times]].
- **Guards:**
  - `psi_star_*` is pooled only if *e*'s logit-psi mean and SD are inside the donors' ranges (the psi_star map acts on the country's own psi);
  - no psi guard on beta, because the engine normalises beta_env by psi_bar, making it level-invariant;
  - a pool equal to base, wider than base, or with its median outside base's 95% keeps base.
- The entry is the same in every key.

**Measured on v2026-10.02.**
- The eastern, central and southern pools are as wide as or wider than base: the between-donor SD of log beta is 1.53 / 1.08 / 0.85 against base 1.175. So RWA and UGA keep base.
- West is homogeneous (SD 0.51). BEN, BFA, CIV and TGO get beta LN(-11.41, 0.958) instead of LN(-10.82, 1.175). Only BEN passes the psi guard; CIV and TGO fail on SD, BFA on mean.
- V6 prior-predictive check: 1.1 is less explosive for all four countries (CIV mean ratio 43 -> 20).
- V7 importance-sampling consequence check: R-POOLED-RWIS 1.305 -> 1.179, at ESS 81 -> 38.

**Why it matters / traps.**
- **Exclusion is MNAR.** Excluded countries are selected for over-prediction, so a pool of folded countries can sit high for them. RWA's base median is 1.2e-6, while the central pool median is 2.8e-5, 23x higher. The floor and conflict guards are what stop this.
- **A6 is not a pooled prior.** A6 substituted a neighbour's *posterior value*, bypassing the likelihood, and made CIV worse (5.0 -> 7.9). The pooled *prior* improved CIV under IS (5.03 -> 3.87): CIV's joint posterior was already below the neighbours'.
- **Code gotcha.** `list()[character(0)]` has NULL names, so V0 failed on a suite with no excluded countries. It surfaced only through the legacy E5 end-to-end test; fixed with `setNames()`.

**How to apply.**
- SUPERSEDED (2026-10-03): rule 1.1 failed its own V6 on config 6.2. UGA's psi guard flips there, and pooled
  psi_star_a makes UGA in central_cod more explosive. Amendment A1 (rule 1.2: P = beta_j0_tot, p_beta) is the
  production rule, and the assembler defaults to RULE_VERSION=1.2 with EXPECT_CONFIG 6.2. See
  [[warmstart-rule-1-2]]. The measurements above were made on config 6.1.
- Copying RULE.md to `~/v0102_warmstart_rule/` on dugong enables the hash check.
- Under 1.1 the legacy harness fails 2 assertions that expect 1.0's `base:national_excluded` label on P rows. The new harness is `claude/v0102_warmstart_rule/tests/negative_tests_rule1.1.R`, 33/33.
- Open question: should locations without a national run (12 in continental) pool too?
