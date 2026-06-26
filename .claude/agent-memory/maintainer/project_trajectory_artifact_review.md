---
name: trajectory-artifact-review
description: Review facts for the trajectories figure group (v0.54.0) — capture triad, R<->Py channel-list lockstep, candidate-not-optimized persistence, set.seed side-effect
metadata:
  type: project
---

Trajectory capture/plot feature reviewed v0.54.0 (branch viz-separation-render-figures), APPROVE-WITH-NITS.

**Capture triad (must change in lockstep — Lesson #12 class):**
- local PSOCK worker: `R/calc_model_ensemble.R` `run_param_stoch_simulation` (`result$traj` + `result$traj_epi`)
- Dask Python worker: `inst/python/mosaic_dask_worker.py` `_TRAJ_CHANNELS` tuple + `run_laser_postca` `result["traj"]/["traj_epi"]`
- Dask R harvest: `R/run_MOSAIC_helpers.R` `.relist` block (relists `traj`/`traj_epi`)

**R<->Python channel-list duplication landmine:** `.MOSAIC_TRAJECTORY_CHANNELS_DEFAULT` (R, calc_model_ensemble.R top) and `_TRAJ_CHANNELS` (Python, mosaic_dask_worker.py) are two hand-maintained copies of the SAME 19-channel list. No automated parity test. Adding/removing a channel requires editing BOTH or the Dask backend silently drops it. Capability check (`.mosaic_build_trajectories` warns+NULL if no worker returned traj) catches a wholesale miss, NOT a single-channel divergence. **Future: a test that asserts the two lists match would close this.**

**Engine fact:** all 19 default channels ARE live in laser-cholera RInterface (`model.py:49-119` hasattr loops set S/E/Isym/Iasym/R/V1/V2 + disease_deaths/new_symptomatic/expected_cases/incidence/incidence_env/incidence_human/Lambda/N/Psi/reported_cases/reported_deaths/W + beta_jt_env/beta_jt_human). `incidence` is a real engine field (not derived). The commented `# self.X =` lines above each loop are misleading — the loops are authoritative.

**calc_model_ensemble call sites = 2 (not the PLAN's "4th site" wording):** posterior `run_MOSAIC.R:2448` (capture ON via control) + medoid `:2904` (capture hard-FALSE). Both correct. `plot_model_ensemble.R` mentions are just stop() strings.

**Persistence = candidate ensemble** (`run_MOSAIC.R:2493`, `.mosaic_persist_trajectory_artifact`), same member set as spatial artifacts. This DEVIATES from PLAN 14.B "reduce over optimized subset when available" — trajectory medians reflect candidate `weight_best`, while prediction plots reflect optimized `weight_best_opt`. Documented deviation, consistent with spatial precedent, slight inconsistency vs prediction plots. Not a regression.

**Reducer reproduces canonical median exactly:** `.mosaic_build_trajectories` uses identical `sim_weights <- rep(parameter_weights, times=n_stoch)/n_stoch` + `weighted_quantiles(as.vector(arr[i,j,,]),.,0.5)` flatten (param-fastest) as the main aggregation at `calc_model_ensemble.R:886-896`. The field-semantics test asserts `reported_deaths median == ens$deaths_median` exactly — genuine guard, not a stub.

**NIT (open): `set.seed(20260625L)` in `.mosaic_build_trajectories`** (uniform line-thinning) clobbers global `.Random.seed` with no save/restore/on.exit. Post-calibration master only, low risk, but should use a local RNG or restore. Hand to swe if touched again.

**NIT: roxygen em-dashes** added to calc_model_ensemble.R widen the pre-existing non-ASCII WARNING (baseline already flags this file for `█` + em-dashes). Not new, but new roxygen lines could use `--`.

R CMD check (`_R_CHECK_FORCE_SUGGESTS_=false`): 1E/2W/3N, ALL baseline (examples need root, non-ASCII, vignette-builder, :::, .run_sim_worker). NO new finding. plot_model_trajectories has NO @examples so adds zero example surface. All 8 trajectory tests pass LASER-free (22 assertions) via precomputed_results path.
