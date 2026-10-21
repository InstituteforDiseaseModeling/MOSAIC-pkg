---
name: v140-ws2-pipeline-traps
description: MOSAIC 1.4.0 WS2 (2026-10-20) pipeline-integration traps - jsonlite digits=I(17) writes NA/Inf as strings, legacy-control test fixtures, scheme-dependent ESS_best, gate check cost under k-hat fallback
metadata:
  type: reference
---

Traps found wiring the 1.4.0 convergence status into run_MOSAIC (branch feat/v1.4.0-convergence):

- **jsonlite `digits = I(17)` (.MOSAIC_JSON_DIGITS) writes NA_real_ as the string "NA" and Inf as "Inf"**
  (default `na` becomes "string" with AsIs digits). Any schema JSON promising "number or null" must pass
  `na = "null"` (summary.json and calibration_diagnostics.json do; inputs JSON deliberately unchanged).
  `null = "null"` is needed too or a NULL element becomes `{}`.
- **Tests can no longer build a "1.3.0 control" from mosaic_control_defaults()**: it now carries
  posterior_weighting/status_thresholds, so calc_run_weight_health treats it as 1.4.0. Strip those keys and
  add best_subset_weighting (see `.legacy_control()` in test-calc_weight_health.R).
- **ESS_best is scheme-dependent**: under saturated/tempered the validator resets it to the legacy tier target
  100 unless the user changed it from the 1.4.0 default 200 (so the 1.2.1 golden reproduces). The status of a
  legacy run uses that value as its ensemble target.
- **.mosaic_tier_best_subset read control$targets$best_subset_weighting** -> after the rename it silently
  fell back to "saturated" for "tempered". A renamed key needs every reader grepped, including `%||%` fallbacks.
- **Gate cost (fixed in the WS2 follow-up)**: a check used to reload every shard and heavy-tail runs
  fell back to fixed 500-draw batches (290 checks to 150k). Now an in-memory pool
  (`.mosaic_gate_pool_update()`, new shards only; seeded once on resume) and geometric fallback growth of
  0.25 x N (`.mosaic_projected_batch_size()`): ~1.5 s per check at 150k x 46 params, 21 checks after the
  minimum run. `calc_bookend_batch_size()` kept its old signature (export change needs user approval) and is
  deprecated, never called by run_MOSAIC.
- Gate replay harness: claude/plan_convergence/ws2_replay.R drives the real `.mosaic_next_batch_size()` +
  `.mosaic_convergence_check_update_state(results = prefix, log = FALSE)` offline. Related: [[v140-ws1-statistical-core]].
