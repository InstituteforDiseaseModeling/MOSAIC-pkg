---
name: optin-tests-hide-stale-expectations
description: Opt-in tests (MOSAIC_RUN_INTEGRATION, skip_if_slow) never run in the default suite, so a default change leaves them asserting the old value; the v0.101.0 central-default flip did exactly this. Run them with NOT_CRAN=true when a default or a run_MOSAIC artifact changes
metadata:
  type: reference
---

**What broke (v0.101.0 integration, 2026-10-01).** The observation-level stream changed the
ensemble central default from `c(cases="mean", deaths="mean")` to `c(cases="median",
deaths="mean")` at every lockstep site, and its full suite was green. But
`tests/testthat/test-run_MOSAIC_integration.R` is gated by `MOSAIC_RUN_INTEGRATION=1`
and still asserted `summary.json central_method_cases == "mean"`. The stale expectation
surfaced only when the opt-in test was run during integration. Same shape as CLAUDE.md
lesson #11: a check that is temporarily unreached keeps its stale contract.

**How to run the gated tests.** Use
`NOT_CRAN=true MOSAIC_RUN_INTEGRATION=1 TESTTHAT_PARALLEL=FALSE Rscript -e 'devtools::load_all(); testthat::test_file(...)'`.
Without `NOT_CRAN=true`, a direct `test_file()` call hits `skip_on_cran()` and reports a
skip, which reads like success. The stubbed-engine run takes about 1.5 min at 40 locations.
`skip_if_slow()` needs `MOSAIC_RUN_SLOW_TESTS=1`; in Oct 2026 it gated only est_* tests
(CFR GAM, zeta priors, flood imputation).

**How to apply:**
- After changing a control default, an artifact field, or a summary.json key, grep
  `tests/` for the old value and include the gated files.
- Run `test-run_MOSAIC_integration.R` once, because it is the only end-to-end run_MOSAIC
  check.
- A copy of that test with extra fixture fields (e.g. a synthetic `reported_tier`) in
  `claude/` is a cheap way to drive a config-dependent code path through run_MOSAIC
  without committing a new slow test.

Related: [[testthat-runtime-lane]], [[r-filesystem-and-regex-traps]] (item 9: the
`ifelse(matrix, vector)` fixture trap, hit while building that synthetic tier).
