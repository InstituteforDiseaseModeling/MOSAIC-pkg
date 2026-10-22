---
name: v150-postcal-review
description: MOSAIC 1.5.0 (PR #142) post-cal resume / capture_reff / worker config rebuild review - real SIGKILL harness recipe, RNGkind worker-rebuild trap, reff_capture 3x RDS growth, HSIC behind plots
metadata:
  type: project
---

Review of 1.5.0 (718095fa0, 2026-10-21). Real-engine SIGKILL resume at CFR pilot, candidate
ensemble, medoid, pre-marker and in the R_eff post pass all reproduced the uninterrupted run
byte for byte (except provenance and paths). Branch R_eff equals 1.4.2's re-simulated R_eff
except for 3 attributes. Clean R CMD check gave 1 NOTE on both branch and main, including the
PDF manual. The PR's "2 warnings" claim was wrong.

Traps found:
- **RNGkind.** sample_parameters() uses set.seed(seed) under the caller's RNGkind, but PSOCK
  workers start with the default kind. Rebuilding configs on the workers therefore breaks
  parallel==sequential when the master's RNGkind is not the default (proven with
  L'Ecuyer-CMRG). A pre-existing sibling: calibration workers vs a master-side rebuild.
- **RDS size.** reff_capture makes ensemble_candidate.rds about 3x larger, and the
  ensemble_optimized.rds fallback copy duplicates it. Production continental was 358 MB.
- **HSIC.** parameter_sensitivity.csv moved behind plots / render. The dashboard reads it
  (MOSAIC-dashboard/build/registry_inputs.js:252). This is the case CLAUDE.md's
  "gated behind plots=TRUE" rule covers.

Harness: claude/redteam/v150/{real_run.R,post_pass.R,compare_runs.R}. It kills with
trace(fn, where=asNamespace("MOSAIC")) and then either tools::pskill(SIGKILL) or a delayed
`system("(sleep N; kill -9 PID)", wait=FALSE)`. Do NOT append `&`, because wait=FALSE already
adds one. Concurrent runs need a distinct R_PARALLEL_PORT, or serverSocket collides.

**Why:** these are the reusable verification recipes and the traps that self-tests missed.
**How to apply:** re-run the harness on any change to resume or post-cal. Check RNGkind
whenever work moves between master and workers.

**Round 2 (c5978b0ee, 2026-10-21): all fixed and APPROVED.**
- HSIC is back in the run and written atomically.
- `capture_reff` moved to the internal `.mosaic_calc_model_ensemble()`, so the exported
  function's object is identical to 1.4.2's.
- Workers inherit `RNGkind()`.
- The candidate RDS is 2.3x 1.4.2's, and `ensemble_optimized.rds` is unchanged.

Kill traces must now target `.mosaic_calc_model_ensemble`: call 1 is the pilot (through the
wrapper), 2 the candidate, 3 the medoid. The in-run HSIC subsample (global RNG) only happens
when the posterior ESS is >= 100. Use the stub with 1200 sims
(claude/redteam/v150/stub_hsic_resume.R) to exercise it.
