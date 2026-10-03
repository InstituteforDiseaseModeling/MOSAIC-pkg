---
name: test-suite-audit-traps
description: MOSAIC testthat suite traps found in the v0.99.9 audit - CI skip gaps, root_directory leak, stale installed build in PSOCK workers, mirror tests
metadata:
  type: project
---

Audit of tests/testthat at v0.99.9 (2026-09-29). Serial suite takes about 275 s, about 10.4k expectations, 0 failures. Traps worth re-checking each review:

- **CI has no ~/MOSAIC.** Run `HOME=<empty dir> TESTTHAT_PARALLEL=FALSE Rscript -e 'testthat::test_local(...)'` to get the skip list CI actually sees. samples_parquet_schema always skips there (skip_if_no_data needs ~/MOSAIC). sample_parameters_* only run because test-convert_config_to_dataframe.R / test-get_location_config.R leak `set_root_directory(system.file(...))` and never restore it, so the result depends on file order.
- **Env-gated tests that never run in CI:** MOSAIC_RUN_INTEGRATION (the run_MOSAIC end-to-end test, about 2 min, passes locally) and MOSAIC_RUN_KERAS_TESTS are set in no workflow step, not even the nightly one.
- **Stale installed MOSAIC.** The laptop had 0.91.14 installed while HEAD was 0.99.9. PSOCK workers `library(MOSAIC)` the installed build, so under devtools::test() any test without the version guard (only sim_rng_contract and optimize_ensemble_subset have it) runs stale code in its workers.
- **Mirror tests.** Several tests re-implement a production formula inline and never call package code. Detect them with a parse-based scan that flags test_that blocks referencing no namespace symbol. Confirm with a mutation: when the per-capita dose was reverted to raw W, only test-two-route-balance.R:49 caught it.
- **Likelihood tests run at the Poisson fallback.** calc_model_likelihood tests called without nb_k or config$date_start score at k=Inf and emit about 140 unasserted warnings. Even WITH config$date_start, a fixture shorter than 20 weeks (.NB_DISP_MIN_WEEKS) is `poisson_insufficient_data` (k=Inf), so the dispersion is never estimated: test-calc_model_likelihood.R #15 ("config$date_start drives per-location NB dispersion estimation", 84 days) estimates no k at all. Check the fixture length before trusting any dispersion test.

**Why:** these gaps let a regression stay green, the same way the lessons 13/18(v) failures did.
**How to apply:** for any review that touches tests, run both the normal and the HOME-less serial suite, and use a mutation to prove that a claimed guard actually fires.
