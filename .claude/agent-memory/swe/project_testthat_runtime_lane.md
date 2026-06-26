---
name: testthat-runtime-lane
description: testthat suite runtime profile + the 3 big fixed/sim costs (TF probe, lasik full sim, impute GAM fits) and where they hide
metadata:
  type: project
---

testthat runtime-reduction pass (RUNTIME lane; maintainer owns correctness lane in a parallel worktree).

**Profile method gotcha:** profiling each test file in a FRESH Rscript MASSIVELY over-counts — every fresh process pays ~22s startup (library(MOSAIC) 3.3s + reticulate py_available 2.5s + module probes 16s). A trivial pure-R file looked like 18-152s. Use a SINGLE-process harness instead: `testthat::test_dir(..., reporter = ListReporter$new())` then aggregate `real` per file. Script at `claude/full_suite_timed.R` (worktree).

**The dominant fixed cost is `setup-python.R`'s module probes**, paid ONCE serially / ONCE PER PSOCK WORKER under Config/testthat/parallel:
- `laser.cholera.calc_model_likelihood` 2.4s (2 tests read it)
- `laser.cholera.metapop.model` 3.6s (1 test reads it)
- `tensorflow` **10.4s — read by ZERO fast-tier tests** (`skip_without_tensorflow()` had no callers; the only TF test gates on keras3+MOSAIC_RUN_KERAS_TESTS). FIX: dropped the eager TF probe; made it lazy/cached inside `skip_without_tensorflow()`.

**Biggest hidden real-sim bloat: `test-lasik_calculations.R`** runs a FULL 40-location LASER `run_model` at file top level (~16s) to check beta_jt/R0/seasonality vs analytic formulas. It was only skipped BY ACCIDENT under `test_dir` (its `inst/extdata/config_default.json` path is cwd-relative and didn't resolve), but DID run under `devtools::test()`/`R CMD check`. Gated behind `skip_if_slow()`. The cwd-fragile path itself is the MAINTAINER's lane (env-dependent file access) — flagged, not fixed.

**`test-impute_flood_probability.R`** fits real `mgcv::bam()` GAMs ~7x (one per `impute_flood_probability()` call). Fixture already minimized w/ documented AUC margin — DO NOT shrink (breaks recovery AUC = correctness). Gated the 2 most-redundant blocks (the double-fit "is deterministic" + the diagnostics-artefacts CV block) behind `skip_if_slow()`.

**Slow-tier gate infra already existed:** `skip_if_slow()` in `helper-skips.R` keys off `MOSAIC_RUN_SLOW_TESTS`. Other gates: `MOSAIC_RUN_INTEGRATION` (run_MOSAIC_integration), `MOSAIC_RUN_KERAS_TESTS` (lstm e2e). Helper fns ARE callable at test-file top level (helpers sourced before files), so `skip_if_slow()` works at file scope.

**Result:** test_dir full suite 106.4s -> 80.4s (-24%); real `devtools::test()` saves additionally the ~16s lasik sim. 0 new failures. (Pre-existing 2 failures in `test-est_initial_E_I.R` parallel block @ line ~355 are env/correctness = maintainer's lane.)

Legitimately-heavy-but-kept (real logic, not sim bloat): ensemble_cluster_robust (PSOCK worker-death dispatcher, bounded timeouts), trajectories (mocked precomputed_results, tiny dims), render_spatial_group (real ggplot PNG render), dask_worker_schema_parity (R-side parquet serialization).
