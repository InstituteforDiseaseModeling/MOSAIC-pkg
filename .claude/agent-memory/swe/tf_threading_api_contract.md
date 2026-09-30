---
name: tf-threading-api-contract
description: tf.config.threading set_intra/inter_op timing + convert=FALSE integer marshalling, empirically verified on TF 2.20.0 (the v0.55.9 parallel-seed psi path)
metadata:
  type: project
---

The v0.55.9 parallel-seed psi path caps each TF worker's Eigen intra-op pool via
`tf$config$threading$set_intra_op_parallelism_threads()`. Verified empirically
against the installed env (TF 2.20.0, reticulate, `convert = FALSE` handle):

- A `convert = FALSE` tf handle marshals an R integer correctly:
  `set_intra_op_parallelism_threads(3L)` → `get_..._threads()` reads back `3`.
  No need to pre-convert with `reticulate::r_to_py()`.
- MUST be called after `import("tensorflow")` but BEFORE any TF op. After the
  first op the runtime is initialized and it raises
  `RuntimeError: Intra op parallelism cannot be modified after initialization`.
- Therefore `try(..., silent = TRUE)` around the setter is CORRECT, not a mask:
  it no-ops the expected RuntimeError on the 2nd seed fit in the same process
  (serial path) and on any session where TF was already touched. It is only
  hiding a real error in the pathological case where it errors on the *first*
  call — acceptable trade-off.
- The BLAS/OMP pin (`.mosaic_set_blas_threads(1L)` + the 6 thread env vars) does
  NOT govern TF's intra-op Eigen pool — that is why the dedicated tf.config cap
  is needed on many-core hosts (dugong) where TF auto-sizes the pool to core
  count and `parallel_seeds>1` oversubscribes. See
  [[project_est_suitability_tf_thread_oversubscription]].

Wiring: `R/lstm_film_suitability.R:.psi_fit_predict_lstm` reads
`MOSAIC_PSI_TF_INTRAOP`/`MOSAIC_PSI_TF_INTEROP` (process-wide, so the SERIAL fit
caps too). `R/ensemble_suitability.R:.psi_fit_seeds_parallel` computes
`tf_intra = MOSAIC_PSI_CORE_BUDGET %/% n_workers` and sets the env per PSOCK
worker before `library(keras3)`. Degenerate case: budget %/% n_workers == 1 ⇒ 1
thread/worker (parallel seeds bought RAM pressure, no speed) — not warned today.
