---
name: cfr-subsystem-traps
description: Four verified traps in the CFR/mortality subsystem (dead config$mu_jt shipped to every worker at 7x the right value; a stale sibling implied-CFR calculator in run_fit_sandbox; a round-trip fixture that collapses chi_endemic==chi_epidemic so it cannot detect the mismatch; plot_CFR_by_country reading a hardcoded stale filename)
metadata:
  type: reference
---

Audited at v0.91.14 (`c657ee4ab`). Full map: `MOSAIC-pkg/claude/cfr_review/01_code_inventory.md`.
Verify before acting — these are code claims, not permanent truths.

**1. `config$mu_jt` was dead payload AND wrong by ~7x. FIXED in the R6 wave (branch
`feature/cfr-restructure`, config_default v4.8) — verify it landed before relying on this.**
`R/sim_params.R` never read it; the engine rebuilds `mu_jt` locally from
`mu_j_baseline/slope/epidemic_factor` (`R/sim_components.R:190-192`). `make_simulation_config()`
generated + validated a `[40 x 3322]` matrix (1.0 MB of a 10.1 MB config, ~20% of the JSON)
filled from *raw reported CFR*, while `mu_j_baseline` is `CFR_target x chain`; measured
`rowMeans(mu_jt)/mu_j_baseline` = 2.5-7.6. The whole config is `clusterExport`ed to every PSOCK
worker. Anyone who "helpfully" wires `config$mu_jt` into the engine gets a 7x hazard silently.

Two reusable lessons from removing it:
- **The legacy-tolerance mechanism when a config field dies.** Every saved run config on disk is
  replayed with `do.call(make_simulation_config, config)`, and that function has **no `...`** —
  dropping a formal turns every old config into an `unused argument` error. Adding `...` is the
  wrong fix: "rejects unknown args" is a *deliberate* safety property that three data-raw builders
  rely on (they inject `zeta_ratio`/`CFR_target`/weight matrices *after* validation precisely
  because of it). Correct fix = keep the formal, document it deprecated-and-ignored, never read it,
  never return it. Silent, targeted, keeps unknown-arg rejection intact.
- **Deleting a generation block can delete unrelated validation.** The `mu_jt` block was guarded
  `if (is.null(mu_jt) && !is.null(mu_j_baseline))` and *inside* it sat the only range/length checks
  for `mu_j_baseline`, `mu_j_slope`, `mu_j_epidemic_factor` — which are real engine params. Because
  `config_default` supplied `mu_jt`, that guard was false in production, so those three params were
  **never validated on the shipped path**. Re-home such checks under their own `!is.null()` guards;
  do not let them die with the block. (Note `.sim_patch_vector` broadcasts scalars, so the
  length-nL check here is stricter than the engine — fine for the 40/3/1-location configs in tree.)

Falsifiable gate used: `run_simulation()` on the epidemic fixture with and without `mu_jt` returns
`identical()` results. Use that shape of test for any "this field is dead" claim.

**How to test a data-object schema change without rebuilding the `.rda`.** You cannot
`assign()` over `MOSAIC::config_default` — the namespace is locked — so in-process patching is not
an option, and `.rda` rebuilds are often gated on another agent/integration step. Working recipe:
copy `DESCRIPTION NAMESPACE R/ data/ tests/` into a scratch dir, mutate the `.rda` there
(`load` -> edit -> `save`), `pkgload::load_all()` that copy, run the suite. Three traps, all hit:
(1) **symlink `inst/ data-raw/ model/` too** — a minimal copy silently converts tests into skips
(`test-ic_select_epoch` reads `test_path()/../../data-raw/`, `test-lasik_calculations` needs
`model/`), and a skip reads as a pass-count drop you will misattribute to your change;
(2) **`TESTTHAT_PARALLEL=false`** — `Config/testthat/parallel: true` + `load_all` on a non-installed
dir crashes the callr workers with "attempt to use zero-length variable name";
(3) **the snapshot ages** — concurrent agents edit the live tree, so diff `R/` and `tests/` between
snapshot and worktree before believing any delta. Every one of the four per-file deltas in this
paired run was a snapshot/copy artifact, zero were the schema change.

**2. A CFR fix landed in one of two siblings.** v0.88.0 added the `/(1 - exp(-gamma_1))`
incidence-dwell divisor to `R/calc_implied_cfr.R` and documented the 10.5x error it fixed —
`.fit_cfr_implied()` in `R/run_fit_sandbox.R:195-208` still has the pre-v0.88 form *and* the
`chi_blend` that B2.1 retired. Lesson #11 again. When touching any implied-CFR algebra,
`grep -rn "chi_endemic" R/` and check all four calculators
(`calc_implied_cfr`, `calc_cfr_period_implied`, `calc_model_ensemble` CFR channel,
`run_fit_sandbox`).

**3. The round-trip fixture is blind by construction.** Every case in
`tests/testthat/test-implied-cfr-roundtrip.R` sets `chi_endemic = chi_epidemic = chi`. The
invariant it claims to pin ("invert mu, recover CFR_target") only holds under that collapse; in
production (0.50 vs 0.75) neither emitted column equals `CFR_target`. **If a fixture assigns the
same value to two parameters whose difference is the thing under test, it tests nothing.**
Generalise: check a fixture's parameter *distinctness*, not just its coverage.

**4. Producer writes a year-flexible filename, consumer hardcodes the year.**
`process_CFR_data()` writes `case_fatality_ratio_2014_<max_year>.csv`;
`plot_CFR_by_country.R:26` reads `..._2014_2024.csv`. Both files exist on disk (2024 from May,
2026 from Sep, differing by up to 4.4 CFR points), so the figure renders stale and silently —
no error until someone deletes the old file. Same failure class as
[[run-id-suffix-and-unkeyed-cache]]: a read path and a write path that agree only by accident.

**Useful engine fact while here:** realized reported CFR = `CFR_target x (chi_eff/chi_epidemic)
x (1 + mu_j_slope*t_factor) x (1 + mu_j_epidemic_factor*flag)`. Verified empirically on MOZ
(1.502 measured vs 1.499 predicted at eps=0.5; 0.681 vs 0.667 in pure-endemic mode). The
"irreducible 1.3-1.5x residual, no closed form can absorb it" claim repeated across
`sample_parameters.R`, `make_config_default.R` and the `config_default` metadata string is the
omitted `(1 + eps)` factor and IS absorbable.
