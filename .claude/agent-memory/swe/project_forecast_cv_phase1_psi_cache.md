---
name: forecast-cv-phase1-psi-cache
description: Phase-1 forecast-CV psi freeze/cache substrate shipped (prefit_rolling_cv_psi + run_rolling_cv psi_cache + ess column); hashing/digest gotchas
metadata:
  type: project
---

Phase-1 of the forecast-CV plan (claude/plan_forecast_cv/PLAN.md) shipped into the
psi freeze/cache substrate.

**Why:** rolling-CV refit psi per cutoff every rerun; psi LSTM recurrent_dropout is
non-deterministic, so freeze-once-per-cutoff + reuse is required for reproducible backtests.

**How to apply (the moving parts):**
- `prefit_rolling_cv_psi(PATHS, cutoffs, est_suitability_spec, pred_date_start,
  pred_date_stop, dir_cache, verbose)` (NEW exported, R/prefit_rolling_cv_psi.R):
  fits psi per cutoff SERIALLY, atomically freezes MODEL_INPUT/pred_psi_suitability_day.csv
  -> dir_cache/psi_<T>.csv (tempfile in dir_cache + file.rename), writes psi_manifest.json,
  resume-skips on (psi_<T>.csv exists + matching spec_hash).
- `run_rolling_cv(..., psi_cache=NULL)`: when set, SKIPS est_suitability and loads frozen
  psi_<T>.csv via the existing .rolling_cv_psi_matrix(). Validates the WHOLE schedule
  UP FRONT (before the per-cutoff tryCatch loop) so a missing cutoff or spec_hash mismatch
  ABORTS the run — NOT swallowed as a per-cutoff "failed" record. Default NULL = bit-identical
  old behavior.
- `ess` column: .rcv_compile_all_models() (the SINGLE shared compile path used by BOTH the
  live loop AND compile_rolling_cv_predictions) attaches metrics$ess_best$value from
  runs/cutoff_<T>/2_calibration/diagnostics/convergence_diagnostics.json on every row;
  NA_real_ when file/key absent. Verified key: d$metrics$ess_best$value (scalar numeric).

**Hashing gotchas (load-bearing):**
- `digest` and `openssl` are NOT in DESCRIPTION. Code uses `requireNamespace("digest")`
  (installed at runtime here) for real sha256, else a deterministic serialize()/FNV byte-fold
  fallback (.rcv_bytes_hash). The user does NOT want digest added to DESCRIPTION (handle check
  centrally). If a future change needs declared sha256, ASK first.
- spec_hash keys on list(fit_date_stop=T, est_suitability_spec) AFTER stripping harness-owned
  date keys (.rcv_strip_date_keys). arch_control$n_seeds MUST be in the spec so it enters the
  hash — changing pooled-seed count invalidates the cache (by design).
- .rcv_bytes_hash FNV stays in double precision mod 2^32 (words exceed R integer range);
  format as two 16-bit halves (%04x%04x) — a single %08x on a >2^31 value overflows to NA.
  seq.int(lane, n, by=8) errors when lane>n: guard with `if (lane <= n)`.
- **spec_hash is a `serialize()` of the R object, so it is BYTE-identity, not semantic
  identity** (verified 2026-09-28 by re-implementing the helper and hashing variants):
  swapping top-level key order, swapping `arch_control` key order, or writing `n_seeds = 10`
  instead of `10L` ALL produce different hashes and a hard error at
  `.rcv_psi_cache_lookup()` (R/run_rolling_cv.R:598-605). Only `arch_control$parallel_seeds`
  is exempt (nulled at R/prefit_rolling_cv_psi.R:448). Practical rule: define `es_spec` ONCE
  in the experiment script and pass the SAME binding to `prefit_rolling_cv_psi()` and
  `run_rolling_cv()` — never retype it (this is what inst/examples/forecast_cv_experiment.R:201
  already does).
- **Cache filenames are keyed on the cutoff ALONE** (`psi_<T>.csv`,
  R/prefit_rolling_cv_psi.R:145), not on the spec_hash. Two psi ARMS (e.g. LSTM vs DLinear
  trunk) therefore COLLIDE: pointing arm B's prefit at arm A's `dir_cache` sees a spec_hash
  mismatch, refits, and `file.rename()` OVERWRITES arm A's frozen psi (:202) with no warning.
  Multi-arm experiments MUST use one `dir_cache` per arm.

**ML comment fixes (R/ensemble_suitability.R):** the determinism comments now say POOLED psi is
statistically equivalent across exec modes but PER-SEED draws are NOT bitwise-identical (thread
count -> float reduction order -> compounds recurrent_dropout). Added: TF-cap no-op-after-first-op
guard comment, and a warning() when parallel_seeds>1 + MOSAIC_PSI_CORE_BUDGET unset + detectCores>32
(cross-process oversubscription). See [[est_suitability-tf-thread-oversubscription]].

Tests: tests/testthat/test-prefit_rolling_cv_psi.R (cache hit/miss/mismatch, ess present+NA,
full run_rolling_cv up-front abort). All pass. Did NOT bump DESCRIPTION / run document() (user does
those centrally). inst/examples/forecast_cv_experiment.R intentionally untouched.
