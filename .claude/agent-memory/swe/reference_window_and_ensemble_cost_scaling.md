---
name: window-and-ensemble-cost-scaling
description: Measured dugong suite cost model (per-run costs by J, per-tick cost flat so runtime is linear in window length T), the continental post-calibration anatomy with its UNEXPLAINED ~6 min/wave, and the T- and |B|-scaling of the ensemble config broadcast memory
metadata:
  type: reference
---

Measured 2026-10-03 from the pulled v2026-10.02 suite (MOSAIC 0.101.0, T = 1,580, dugong) plus laptop
paired tests. Scripts: `claude/plan_2018_start/07..10_*.R`; `10_budget_cost_model.R` reproduces the
suite's measured 15.7 h exactly, so it is the base for any window or budget estimate.

**Per engine run on a fully loaded dugong** (168 workers busy): national J=1 0.89 s, regional J=5-10
1.03 s, continental J=40 1.84 s. **Per-tick cost is flat**: 0.563 ms/tick at T=1580 vs 0.533 at T=3322
(cfr_test arm D, a 2018 config with CFR v2.1), so per-run cost is linear in T. Laptop paired engine at
J=40, T=1580 vs 3406: 2.12-2.47x. ENG-A-01's O(nticks^2) site is fixed (`sim_derived.R` hoisted row).

**Continental post-calibration (v2026-10.02, 91 min):** combine 3, forecast-year CFR pilot (108 x 1)
18.5, ensemble (108 x 10) 51.9, trajectories 6.5, medoid 100 reruns 6. A wave fit (pilot = F + 1
wave, ensemble = F + 6.43 waves) gives F ~ 12 min, consistent with the master serialising 1.1 GB
of param_configs to each of 168 workers (184 GB), plus **~6 min per wave that is UNEXPLAINED**. On the
laptop a real continental task costs 1.0-1.6 s (engine 0.5-0.85, CFR redraw 0.1-0.2, trajectory
saveRDS 0.35-0.54), and the master's serial steps (observation draws, summaries) take ~1-2 min per
1,080 members. Rejected hypothesis: holding 614 MB of distinct live configs does not slow the engine
(paired 0.89-1.07). Profile on dugong before trusting any continental post-cal estimate.

**EXPLAINED 2026-10-07 (the F term + the 2018 continental OOM):** `calc_model_ensemble.R:1020`
`clusterCall(cl, function(rd) {...}, root_dir)` closure carries the whole calc_model_ensemble frame
(4 preallocated dense NA arrays + param_configs + config/priors) = **5,742 MB per worker** at
40 loc x 3,406 d x 108 x 10, serialised serially by the master (~0.45 GB/s = the 30 GB/min ramp).
Workers hold it until their first task's gc, so peak = n_workers x (5.7 + export) GB before ANY task
runs (empty traj_scratch). Harness `~/ens_mem_diag/` on dugong. Same trap as [[psock-export-and-dead-guard-traps]] #1.

**Broadcast memory** = |B| x sampled-config size x ensemble workers, all on the host (local PSOCK). A
sampled 40-loc config is 10.15 MB at T=1580 and ~21 MB at T=3406, so a 2018 window doubles it (180 ->
373 GB at |B|=108, 168 workers). It scales with |B|, so tying ESS_best to N is infeasible for
continental without an ensemble-only worker cap (control has none: `.mosaic_ensemble_parallel_plan`
reuses `parallel$n_cores`). The coordinator measured a 673 GB box-wide peak and 22 GB master RSS at 2023.

**N-independent:** |B| = 108 at every budget (ESS_best 100), so ensemble RDS and memory depend on T and
|B| only; `samples.parquet` (continental 2.0 GB at 250k) scales with N.

Related: [[rengine-cost-model]], [[2015-window-runtime-blast]], [[production-suite-protocol]].
