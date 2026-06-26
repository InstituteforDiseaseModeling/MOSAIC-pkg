---
name: project-trajectory-streamtodisk-review
description: Stream-to-disk trajectory capture review (v0.55.2) — orphaned-scratch-on-interrupt gap, on.exit pattern, version/NEWS tangle with concurrent ENSO work
metadata:
  type: project
---

Review of the trajectory stream-to-disk capture rework (uncommitted on branch viz-separation-render-figures, ~2026-06-25).

**Scratch-cleanup gap (the one real finding):** the candidate ensemble is run with `reduce_trajectories=FALSE`, which DELIBERATELY suppresses `calc_model_ensemble`'s internal `on.exit(unlink(scratch))` so the CALLER (`run_MOSAIC`) owns the lifecycle. `run_MOSAIC` only `unlink()`s the scratch dir inside the "UNIFIED TRAJECTORY REDUCE + PERSIST" block (~R/run_MOSAIC.R:2745). The window between candidate capture (~:2516) and that unlink covers all of `optimize_ensemble_subset()` + parquet writes + JSON patching. The RUN_FATAL `on.exit` at run_MOSAIC.R:1012 ONLY logs — it does NOT unlink. So an uncaught error or user interrupt in that window orphans `2_calibration/state/traj_scratch` (tens of thousands of per-sim .rds at 40-loc × 100 param × stoch). Scratch lives INSIDE dir_output (not /tmp — good, no collision; clean_output=TRUE rerun wipes it).
**Why:** stream-to-disk intentionally moved cleanup ownership to the caller; the caller's safety-net on.exit was never extended to cover the scratch.
**How to apply:** the fix is to register `on.exit(unlink(traj_scratch_dir, recursive=TRUE, force=TRUE), add=TRUE)` in run_MOSAIC right after `traj_scratch_dir` is defined (~:2464), guarded so it's a no-op if already cleaned. Any future caller-owned-scratch pattern needs the same belt-and-suspenders.

**Minor:** `.mosaic_build_trajectories` opens N per-channel `file()` write connections (~calc_model_ensemble.R:439) and only `close()`s them at :487; an error in between leaks open connections (R's 128-conn limit) for the session. `_bychan` subdir only unlink'd on success (:525) but it's nested in the caller-unlinked scratch so not a separate dir leak. Both callers tryCatch the reducer.

**No new dep:** serialize/unserialize/file/gzfile are base R; withr already in Imports. Confirmed.

**Orphan/replacement (Lessons #2/#9):** clean. Exactly two calc_model_ensemble calls in run_MOSAIC — candidate (capture ON, reduce deferred) + medoid (capture explicitly FALSE). The prior inline "re-sim the optimized subset" block was fully replaced by the deferred scratch reduce; no parallel re-sim path remains. All 4 new internal helpers wired (.mosaic_build_trajectories, .mosaic_persist_trajectory_artifact, .mosaic_compute_cfr_refs, plot_model_trajectories). pkgdown matches("^plot_") covers the new export.

**Tests:** 46 PASS via devtools::load_all (FAIL only against the stale installed pkg — always load_all for unreleased source). Real assertions incl. bit-identity weighted-median over subset (tol 1e-8), reported_deaths≠disease_deaths semantics, epidemic_frac lag direction, auto-scratch cleanup (after==before), R↔Python channel-list lockstep (#12 guard, resolves not-skips under load_all). LASER-free/CI-safe via precomputed_results.

**Version/NEWS tangle (release-hygiene call):** working tree intermixes the trajectory feature with SEPARATE concurrent ENSO multi-source work (R/process_ENSO_data.R + R/compile_suitability_data.R + man/process_ENSO_data.Rd + test-process_enso_data_source.R) — these must be committed separately and did NOT need touching for trajectories. DESCRIPTION=0.55.2, but NEWS top header is malformed: `# MOSAIC 0.54.0` carrying `## ...(v0.55.2)` + `## Red-team fixes (v0.55.1)` as subsections, and 0.55.0 (ENSO) has NO NEWS section.
**Recommendation given:** trajectory commit = single `# MOSAIC 0.55.2` header folding in the 0.54.0/0.55.1/0.55.2 trajectory content (it never shipped as separate releases); ENSO commit takes its own `# MOSAIC 0.55.0` section + bumps to 0.55.x. Sequence so versions are monotonic across the two commits.
