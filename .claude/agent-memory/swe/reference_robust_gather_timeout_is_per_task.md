---
name: robust-gather-timeout-is-per-task
description: .mosaic_cluster_lapply_robust's idle timeout (1800 s) is judged per TASK; shard batching (v0.87.0, 100 sims/task) made calibration tasks ~100x longer without rescaling it -> long chunks abort the run as a fake "worker crash". Also two PSOCK sites bypass the robust gather.
metadata:
  type: reference
---

`socketSelect(timeout = idle_timeout_sec)` in `.mosaic_cluster_lapply_robust()` stops the run if
no busy worker returns a result in that window. All workers start a chunk together, so the first
result arrives after one whole chunk. Measured v0.93.0 laptop: 40-loc worker 4.03 s/sim at
n_iterations=3 -> 403 s per 100-sim chunk; n_iterations=10 + PSOCK contention crosses 1800 s.
Repro: `claude/review_v093/swe/timeout_mechanism.R` (0.4 s sims, 2 s timeout: per-sim OK, chunk of 10 stalls).

**How to apply:** any change to the unit of work handed to the robust gather (chunking, more
iterations per task) must rescale `idle_timeout_sec`. Also: `render_MOSAIC_figures` (parLapplyLB,
runs inside run_MOSAIC before summary.json) and `.mosaic_reff_resim_ci` (parLapplyLB) use the
blocking gather that hangs forever on Linux worker death — see [[psock-blocking-gather-worker-death-deadlock]].
Found in the v0.93.0 review (claude/review_v093/swe_report.md, H1/M3).
