---
name: testthat-parallel-nesting-landmine
description: Enabling Config/testthat/parallel crashes the suite because ~5 tests fork (mclapply) or spawn PSOCK clusters INSIDE a testthat worker; gate them with skip_if_testthat_parallel()
metadata:
  type: project
---

Enabling `Config/testthat/parallel: true` in DESCRIPTION makes each test FILE run in
a callr worker subprocess. Tests that spawn their OWN inner parallelism inside that
worker (parallel-over-parallel) corrupt testthat's result IPC and crash with
`unserialize ... ReadItem: unknown type N` or `no restore method available`. It is
NOT a worker-count problem (reproduces at 2 workers) and NOT a deadlock — it is the
nesting itself.

**Why:** the CLAUDE.md BLAS/Numba landmine generalized to testthat: an inner
mclapply fork / PSOCK `makeCluster` inside a callr child collides with the parent's
serialization stream.

**The inner-parallel tests (must self-skip in parallel mode):**
- `test-est_initial_E_I.R` "Parallel processing works" (est_initial_E_I(parallel=TRUE) → mclapply)
- `test-optimize_ensemble_subset.R` "parallel (cl, stride=1)..." (PSOCK makeCluster)
- `test-dask-psock-orchestrator.R` "B2: PSOCK plumbing..." (PSOCK makeCluster)
- `test-dask-local-separation.R` "B1: ...forks inherit it" (mclapply)
- `test-dask_worker_schema_parity.R` 3 tests at the mclapply lines (~372/424/486)
NOTE: `test-est_initial_R_parallel.R` and `test-calc_model_ensemble.R` look like they
spawn clusters but do NOT (R_parallel only checks docs/detectCores; ensemble uses mocks).

**How to apply:** gate each with `skip_if_testthat_parallel()` (helper-skips.R). The
reliable "am I a parallel worker?" signal is `nzchar(Sys.getenv("CALLR_IS_RUNNING"))`
— it is EMPTY in serial `devtools::test()` and in a plain `R CMD check` test process,
and SET in every callr child. `testthat::is_parallel()` is unreliable inside the worker
(returned FALSE in probes). So: local `devtools::test()` runs parallel (~68s) and the
inner-parallel tests self-skip; CI runs SERIAL (TESTTHAT_PARALLEL=FALSE in
R-CMD-check.yaml) so those tests actually execute and get covered. Do not flip CI to
parallel without re-solving the coverage gap.

**Tiering:** `skip_if_slow()` gates on `MOSAIC_RUN_SLOW_TESTS` (mirrors
MOSAIC_RUN_INTEGRATION/MOSAIC_RUN_KERAS_TESTS); applied to the est_zeta_{1,2,ratio}
prior-derivation tests (PNG-rendering, off hot path). Nightly schedule job sets it.

See also [[project-testthat-runtime-profile]].
