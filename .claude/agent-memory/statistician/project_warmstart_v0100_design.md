---
name: warmstart-v0100-design
description: Stage-2 warm-start assembler for the v0.100.x suite (priors v17.0) - uniform x2 tempering plus a never-wider-than-base floor, globals and mu_jt at base, tau_i/alpha_1/quiet-start E-I excluded; built and tested 2026-09-30; base = the installed priors_default (v17.1 for the 0.101.0 suite that now decides v1.0)
metadata:
  type: project
---
**Status 2026-10-02:** the v0.100.1 suite (v2026-10.01) ran as a rehearsal and scored NO-GO; the v1.0
decision rests on the MOSAIC 0.101.0 suite (v2026-10.02, rubric 1.0.1, priors v17.1). The assembler checks
national runs against the loaded priors_default, so "v17.0" below means the run's base priors, and
"v0.100.2" in the exclusion rule refers to that next suite.

**What exists (2026-09-30).** The assembler is claude/deploy_v0100/warmstart/assemble_warmstart.R on the laptop, with a README.md beside it. It reads `NMME_DIR=<SUITE>/national` and writes `<SUITE>/warmstart/warmstart_priors_{eastern_eth,southern_moz,west_nga,central_cod,continental_ssa}.rds`, which is what run_job.R reads. Its validation covers structure, provenance re-derived from posteriors.json, `get_location_priors`, 100 `sample_parameters` draws against a base reference, and a JSON round trip. It was tested on 8 SMOKE national runs from the rebuild snapshot 2020b14e5. All 5 keys pass, outputs are byte-deterministic, an eastern_eth regional SMOKE run_MOSAIC accepts the warm start, and the negative tests pass 18/18.

**Design decisions.**
- **Every folded entry is inflated x2** with inflate_priors, keeping the mean. This is a power prior with a0 = 1/2, because the joint fit re-scores the national data; beta_j0_tot also absorbed imported infection.
- **Floor:** an entry wider than its base prior reverts to base, so an uninformative national fit leaves v17.0 unchanged.
  - In the 2026-06 design, the noise-only entries that happened to come out narrower than the prior (about half of those with width ratio 0.9-1.04) were folded at full strength.
  - Under tempering plus the floor, mostly beta_j0_tot, psi_star_a and psi_star_k survive.
  - `INFLATE_PARAMS=beta_j0_tot` restores the old rule.
- **Globals stay at base, not pooled.** See [[posterior-pooling-counts-prior-J-times]].
- **mu_jt stays at base;** cfr_posterior is not folded. See [[mu-jt-centre-from-config-widths-from-priors]].
- **Always kept at base:**
  - tau_i;
  - alpha_1 (pinned, D1);
  - prop_E/I_initial of the countries in `metadata$quiet_start_seeded`, read at run time. Rebuild commit 1e114b315 adds COG and other under-one-infection windows.

**Why.** The national posteriors are mostly the prior plus subset noise, and the joint fit re-uses the same data. See [[prior-object-validation-traps]] for the validation lessons.

**National exclusion rule (added 2026-10-01, pre-registered before any v0.100.2 output).** Identifier
`national-exclusion-1.0`. A run failing any of these is not folded, and every location entry of that
country stays at base:
- E1 integrity;
- E2 degenerate (rubric M-DEGENERATE: >= 30 sets, IQR ratio >= 0.02, max weight <= 0.5);
- E3 level (mean-line weekly cases pred/obs outside [0.5, 2] AND more than 50 cases off; the rubric
  tier-C rule at every tier);
- E4 shape (tier A/B, non-quiet-start, >= 26 weeks, R2 < 0.40; the rubric tier-B bar).

E5 then sends an entry back to base when its posterior median is outside the base prior's 95%. E5 must
run AFTER tempering and the floor. Run first, it mislabelled TZA `prop_S_initial`: that posterior is 7.35x
WIDER than its Beta(23855, 245) prior, because S is the normalised remainder.
- On the v0.100.1 rehearsal it excludes 7/28: UGA, ZAF, BEN, TGO, BFA, NAM, CIV.
- Thresholds are cross-checked against the hash-locked `rubric_thresholds.json`.
- Negative tests pass 38/38; outputs are byte-deterministic.

Asymmetric-loss rationale: the warm prior IS the joint prior in BFRS, but the joint fit re-scores the
national data.

**How to apply.**
- Run it on dugong after the national suite finishes, with the same installed MOSAIC. It refuses national runs whose 1_inputs/priors.json differs from the loaded priors_default.
- Open for the user: whether to accept uniform tempering over the 2026-06 beta-only rule.
- Open for the disease-modeler: whether the quiet-start seeding floor belongs in coupled fits at all, since they model importation.
