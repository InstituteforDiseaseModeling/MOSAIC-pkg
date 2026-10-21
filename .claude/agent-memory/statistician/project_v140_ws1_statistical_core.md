---
name: v140-ws1-statistical-core
description: MOSAIC 1.4.0 WS1 (2026-10-20) convergence status core - what was built, SPEC section 6 deviations (CIV k-hat), PIT-KL ceiling/NA rule, JSON round-trip traps, interim posthoc breakage until WS2
metadata:
  type: project
---

WS1 of the 1.4.0 convergence pipeline (branch feat/v1.4.0-convergence, uncommitted at hand-off 2026-10-20):
`.mosaic_bestfit_select/.mosaic_member_mce/.mosaic_khat_tempered/.mosaic_ess_growth_slope/.mosaic_projected_draws`
(R/calc_coarsened_posterior.R), exported `calc_convergence_status()` + `.mosaic_convergence_defaults()` +
`.mosaic_glossary()` (R/calc_convergence_status.R), calc_weight_health refactor, PIT KL replacing KDE.

**Validation (claude/plan_convergence/ws1_validate.R):** ensemble-ESS row reproduces SPEC section 6 exactly
(16/5/6, BFA flat). But SPEC's "other three rows green" is FALSE: CIV tempered k-hat 0.61 (yellow, ESS_all 1287),
CAF 0.93 / NER 1.01 red, ZAF/UGA yellow; MCE yellow/red tracks ESS. Overall 15 green / 6 yellow / 6 red.
**Why:** k-hat is not a function of ESS_all; a heavy tempered tail can coexist with a big ESS.
**How to apply:** CIV may never stop GREEN in adaptive mode unless its k-hat falls with draws; check in WS6.3.

**PIT KL:** at most log(B)=3.0 nats (one bin); NA (+warning) when Kish ESS < B (the (B-1)/(2 ESS) correction
would zero a point mass). Bins straddling the posterior's edges lower the value (decay fixture log4 -> 1.23).
Any test asserting KL > 3 or "uncapped" is obsolete.

**JSON traps:** jsonlite drops names of atomic vectors (flag ids are a character vector + parallel flag_text (SCHEMA_v2 ids budget_cap_reached, ess_growth_stalled, ensemble_size_bound, fixed_budget); per-parameter MCE needs
`.mosaic_weight_health_for_json()`); `.mosaic_write_summary_json` re-reads convergence_diagnostics.json written
WITHOUT na="null", so NA comes back as the string "NA" - field extractors must suppressWarnings(as.numeric(unlist())).

**Interim (until WS2):** calc_weight_health()$status is now green/yellow/red, so run_MOSAIC's
`identical(weight_health$status, "ok")` posthoc verdict is FALSE for coarsened runs, and run_MOSAIC's
weights/ess_target/batch_max/ess_method args trigger a deprecation warning. A transitional `weights_used` block
(members) MUST stay until WS2: run_MOSAIC sets `$weights_used_column`, so a missing `$weights_used` partial-matches
it and `wu$ess_kish` crashes the run. All-draws batch ratio dropped; the 1.4.0 core is ~0.17 s at 150k x 300. Related: [[project_coarsened_posterior_v130]], [[reference_kde_bandwidth_weighted]].
