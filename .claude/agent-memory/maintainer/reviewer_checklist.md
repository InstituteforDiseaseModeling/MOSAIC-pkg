---
name: reviewer-checklist
description: Institutional review checklist for MOSAIC data-object/config bumps and engine-semantic changes
metadata:
  type: feedback
---

Durable review checks for MOSAIC, built from caught regressions.

**Data-object bumps (config_default / priors_default):**
- Rebuild the changed array from its committed source and assert bit-exact match
  to the shipped .rda/.json (e.g. psi_jt from pred_psi_suitability_day.csv). See
  [[config-default-psi-provenance]].
- Structural-diff the .json (python json walk) to confirm ONLY the intended keys
  changed -- catches accidental window/prior drift.
- Verify version string bumped in BOTH the data-raw maker AND the metadata$version
  inside the object, and that the drift-guard test (test-cfr-pipeline-consistency.R,
  the Lesson-#12 guard) passes.
- Confirm the regeneration recipe is reproducible from the committed package +
  canonical pipeline (model/LAUNCH.R). If produced by an ad-hoc/compute-VM run with
  overrides, that is a provenance finding -- the artifact is hand-curated, document it.

**Always check for the compute-env parity trap:**
- `git log origin/main..HEAD` -- unpushed config/data commits mean hedgehog/Coiled
  (which install from remote) silently run STALE defaults on large calibrations.
  This is a SHOULD-FIX every time a default data object changes.

**Engine-semantic / field changes:** grep ALL siblings (grep -l old R/), incl.
temporarily-unused fns (Lesson #11). Use reported_deaths/reported_cases not
disease_* (Lesson #12).

**New fns / fixes ship WITHOUT a test = the #1 recurring smell** (Lessons #1/#6).
On any bug-fix commit, grep tests/testthat for the fixed function name; if zero
references, flag missing regression test (e.g. 979f066e CV fix had none).

**Build/runtime separation:** model/ is .Rbuildignore'd -> large CSVs there are
repo-bloat, not build breakage. Don't conflate.

**Removing a parameter from the model** (learned on CFR v2.1, v0.96): the sibling
set is larger than the config/sampler/convert lists. Also check
`data-raw/make_estimated_parameters_inventory.R` + `data/estimated_parameters.rda`
+ `R/estimated_parameters-data.R` row counts + `R/priors_default.R` section counts +
`.claude/skills` lever advice + the MOSAIC-docs spec + country-repo config builders.
Consumers of estimated_parameters all intersect with sample columns, so stale rows
fail SILENTLY (empty plot panels), never loudly.

**Mutation-test the wiring, not just the helpers.** Green unit tests on a helper say
nothing about whether run_MOSAIC calls it: on v0.96.1, replacing the worker's
integrated-deaths call with `if (FALSE)` left the WHOLE suite green (incl. the opt-in
integration test). Harness recipe: `git archive` HEAD to /tmp, apply one fixed-string
mutation per copy, `load_all` + `test_file`/`test_dir` with `TESTTHAT_PARALLEL=false`.
Opt-in tests (`MOSAIC_RUN_INTEGRATION=1`) never run in CI -- a fix guarded only by
one is unguarded.

**Fixture-loosening recurs:** identity tests that set `chi_endemic == chi_epidemic`
collapse the one axis the chi choice acts on (seen in v0.93 roundtrip tests AND v0.96
onset tests). Demand a chi-asymmetric arm (force epidemic PPV with
`epidemic_threshold = 0`, endemic with `= 1`).

**Two cheap check traps:** roxygen markdown turns `[a x b]` into `\link{a x b}` (Rd
cross-ref WARNING) -- escape as `\[a x b\]`; a NEWS.md `##` heading containing
" v2.1" makes R's news parser treat level-2 headings as versions (NOTE, and
`news()` reports the package as version 2.1).
