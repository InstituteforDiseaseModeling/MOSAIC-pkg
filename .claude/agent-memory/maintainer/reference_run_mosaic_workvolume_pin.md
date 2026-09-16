---
name: run-mosaic-workvolume-pin
description: The ONLY way to pin run_MOSAIC() to an exact simulation count is control$calibration$n_simulations (FIXED mode) — min/max_batches_adaptive and target_r2_adaptive do NOT bound total sims
metadata:
  type: reference
---

Verified on MOSAIC v0.69.1. Recurring mistake: people try to pin calibration work volume with the
adaptive knobs. That does not work.

**The real knob:** `control$calibration$n_simulations = <positive integer>` -> FIXED mode.
- `R/run_MOSAIC.R:3122` default is `NULL` with the literal comment "NULL = auto mode, integer = fixed mode".
- `.mosaic_normalize_n_sims()` (`R/run_MOSAIC_helpers.R:861-883`) maps NULL/"auto"/"algo" -> auto, a
  positive numeric -> `list(mode="fixed", fixed_target=<int>)`.
- `R/run_MOSAIC.R:1182-1187` logs "[FIXED MODE] Running exactly %d simulations" and runs `seq_len(target)`.
- Caveat (from [[coiled-longrun-psock]] territory): FIXED mode loses a whole un-gathered batch if the
  process dies; AUTO checkpoints per batch.

**Why the adaptive knobs don't pin:**
- The loop break is `if (state$converged || state$total_sims_run >= max_simulations_total) break`
  (`R/run_MOSAIC.R:1286`).
- `state$converged <- TRUE` comes solely from `prop_converged >= control$targets$ESS_param_prop`,
  gated only by `state$batch_number >= min_batches_adaptive`
  (`R/run_MOSAIC_helpers.R:1804-1810`). `target_r2_adaptive` NEVER enters it.
- `target_r2_adaptive` only feeds the Phase-1 ESS~sqrt(n) lm R^2 check (`helpers:1649-1665`) and
  `calc_bookend_batch_size(target_r_squared=)` (`helpers:1489`) — i.e. the phase transition and the
  predictive batch size, not the stop condition.
- `min_batches_adaptive == max_batches_adaptive` bounds Phase 1 only; Phase 2 predictive batches
  still run with floor `batch_size_adaptive` (`helpers:1506, 1520-1523`).
- Net: those knobs give a one-sided CAP (via `max_simulations_total`), never a floor.

**Validator floors that constrain small pinned runs** (`R/run_MOSAIC_helpers.R:9-13, 535-554`):
`.MOSAIC_MIN_BATCH_SIZE=10L`, `.MOSAIC_MIN_SIMULATIONS=100L`, `.MOSAIC_MAX_ITERATIONS=1000L`;
`max_simulations_total >= 100` AND `batch_size_adaptive < max_simulations_total`.

**Priors are never checked against config:** `.mosaic_validate_priors(priors, config)`
(`R/run_MOSAIC_helpers.R:655-676`) accepts `config` and never reads it. A 40-country
`priors_default` with a 1-location config runs fine and writes the full 40-country `priors.json`
to `1_inputs/`. `sample_parameters()` keys per-location priors by iso so the sampled config stays
J=1 — but the documented path is `get_location_priors()` (`R/run_MOSAIC.R:492, 585`). Two operators
using different priors objects produce measurably different runs under the same label.
