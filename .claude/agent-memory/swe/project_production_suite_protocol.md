---
name: production-suite-protocol
description: How the 2026-09 national/regional and 2026-06 continental production runs were made, what v0.100 breaks in them, measured dugong cost model (per-run, J-ratios, wave quantization), and the claude/deploy_v0100 queue runner
metadata:
  type: project
---

MOSAIC-results production runs came from dugong scripts, NOT claude/full_metapop/01_individual_calibration.R:
- national v2026-09-18.01: dugong `~/run_country.R` (cfg2018_nmme v4.7/v15.18, FIXED 30k x 5, weights 1/1, burn-in 45,
  ESS 1000 @ 1-1/41, best 30..1500, n_iter_ensemble 5 / n_iter_best 10, alpha_1 sampled, Coiled, MOSAIC 0.55.12).
- regional v2026-09-18.01: `~/regional_nmme/run_region.R`, FOUR regions (eastern/southern/west/central), 100k x 3, net freed,
  ESS prop 0.85, priors = stage-2 warm start from the national posteriors.
- continental ssa v2026-06-25.01: `claude/full_metapop/03_full_metapop_fit_system.R continental full local` (MOSAIC 0.51.0):
  FIXED 250k x 5, net freed, weight_deaths 0.5, burn-in 42, weights_time ramp 2/3->4/3 over the scored window,
  ESS prop 0.85, best 30..1000, n_iter 10/100, 120 PSOCK cores; 16.9 h = 12 h sims + 3.4 h combine (1-file-per-sim, now fixed
  by 100-sim shards) + 1.5 h post-cal. Published run had per-location alpha_1 SAMPLED; it sat on its prior -> pinned since.
- Country rule was de facto "clear gap in positive case-weeks" (2018 window: all >= 19; 2023 window: >= 13 -> 27 + CAF).

**Why:** the v0.100 suite plan (2026-09-30) had to re-derive all of this; Coiled is gone, cfg2018 objects predate CFR v2.1.

**How to apply / cost model (dugong = 2x44-core Xeon 8473C @2.1 GHz, SMT2 -> 176 threads, ~3x slower per thread than the laptop):**
- per engine run at J=1, T=1580, n_iter 5, box fully loaded ~0.9 s. Derived from cfr_test arm D: 65 min for 30k x 5 at 85
  workers, but 30000/88-sim shards = 341 tasks on 85 workers = 5 waves (not 4.01) -> 1.77 s/run at T=3322.
- J-ratio (laptop, interleaved, n_iter 2): J10/J1 1.16-1.30, J40/J1 1.84-2.21; engine itself flat in J; sampled params 39/213/783.
- FIXED batch makespan = ceil(tasks/workers) task-lengths; national 30k at shard 100 on 42 workers wastes 11% -> runner sets
  io$shard_batch_size 25 (output byte-identical, verified).
- A FIXED run that dies AFTER consolidating shards cannot be resumed (run_MOSAIC refuses a smaller pool) -> requeue fresh.
- Runner: `claude/deploy_v0100/` (run_job.R 3 scales, lane.sh core-budget + warm-start-gated scheduler, launch/status/pack/pull).
  `setsid` and `pgrep -c` do not exist on macOS; `ls glob | wc` under pipefail kills a `set -e` script when nothing matches.
See [[reference-dugong-vm]], [[rengine-cost-model]].
