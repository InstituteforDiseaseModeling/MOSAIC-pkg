---
name: dugong-psock-ldpreload-gap
description: On dugong, PSOCK workers don't inherit the r-mosaic-Rscript LD_PRELOAD; for per-model batch fan-out use independent nohup-per-model via the wrapper, not a PSOCK cluster
metadata:
  type: project
---

For dugong batch fan-out over many model dirs (e.g. recomputing R_eff per model),
prefer ONE independent `nohup ~/bin/r-mosaic-Rscript ~/<worker>.R "$dir"` process
per model over a single `parallel::makePSOCKcluster` master.

**Why:** the prior R_eff batch (`claude/reff_batch_dugong.R`, a PSOCK cluster)
spawned workers via the *system* Rscript, and the wrapper's libexpat/libcrypto/
libssl `LD_PRELOAD` did NOT reliably cover those worker processes. The master
then died on result-gather even though the compute had succeeded — a false-alarm
crash, not a real failure. (See also [[psock_blocking_gather_worker_death_deadlock]]:
a worker process dying mid-`unserialize()` hangs/kills the master on Linux.)

**How to apply:** when fanning out per-model work on dugong, launch each model as
its own `nohup <wrapper> <worker.R> <dir> > log 2>&1 &` and `wait` on the PIDs in
a bash launcher (no master process gathering serialized results). Each process is
started DIRECTLY through `~/bin/r-mosaic-Rscript`, so it inherits the wrapper's
preload natively — no worker-env gap, nothing to crash on gather. Pattern shipped
as `claude/reff_batch_dugong_v2.R` (bash) + `claude/reff_one.R` (single-dir
worker running `add_reproductive_numbers(dir, recompute_ci=TRUE, plots=TRUE)`).
Note dugong stores these models FLAT: `~/MOSAIC/output/full_metapop_nmme/<ISO>`
(NOT `national/<ISO>`). See [[reference_dugong_vm]] for the wrapper/preload setup.
