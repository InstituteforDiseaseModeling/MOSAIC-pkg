---
name: test-suite-isolation-traps
description: MOSAIC testthat traps - leaked root_directory, stale installed build in PSOCK workers, vacuous guarded asserts, Poisson-fallback noise; and the helpers that fix them
metadata:
  type: project
---

Recurring test-suite smells found in the 2026-09-29 deep review (pkgdocs group), with the fixes now in `tests/testthat/helper-skips.R`:

- **Leaked `options(root_directory)`**: a bare `set_root_directory()` in a test leaks into every later file (CI runs serial/alphabetical). Tests that only read packaged data silently depended on the leak. Use `local_test_root()` (scoped via withr; falls back to the package dir when ~/MOSAIC is absent). Top-level `withr::local_*` in a test file IS torn down at file end under testthat 3e (verified).
- **PSOCK workers run the INSTALLED MOSAIC** (`make_mosaic_cluster()` does `library(MOSAIC)`), not the load_all build. Locally the install was 0.91.14 vs tree 0.99.x. Guard any test that dispatches package code to workers with `skip_if_installed_build_stale()`.
- **Vacuous guards**: `if (!is.null(x$parameters$location))` / `x$distribution` style guards around the only assertions never fire (priors store `$location` directly). Grep for `if (` wrapping `expect_` when reviewing.
- **calc_model_likelihood without nb_k_* and without config$date_start** takes the Poisson-limit fallback with a warning; many likelihood test files still do this (extreme, obs_weights, weights_location_derivation - likelihood group).
- **Mirror tests** that transcribe a production expression and test the copy: replace with a call to the real helper on a tiny fixture, then mutation-test (edit the R line, confirm FAIL, restore).

**Why:** each of these let a regression pass green. **How to apply:** check new tests for these patterns in review; CI-like run = `HOME=/tmp/emptyhome R_LIBS=<libs> NOT_CRAN=true`.
