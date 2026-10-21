---
name: remote-process-mgmt-traps
description: Self-inflicted dugong process-management traps from WS6 (2026-10-20) - pkill -f / pgrep pattern matches your own ssh shell, pgrep -af field 0 is the PID, core-accounting schedulers overshoot if the count misses not-yet-exec'd R
metadata:
  type: reference
---

Hit three times in one WS6 session on dugong (the scheduler + kill commands were mine; no foreign job was touched, but it was close):

- **`ssh dugong 'pkill -f "bf_sched.py tasks.txt"; ...'` kills the ssh's own `bash -c`**, because the
  pattern text is in that shell's argv. Same for `ps | awk '/[p]ython3 bf_sched.py/'` when the
  command line also contains the literal later on. Exit 255 = you killed yourself. Fix: write a pid
  file at launch and `kill $(cat pidfile)`, or match on `/proc/<pid>/comm`.
- **`pgrep -af` prints `PID cmdline`**, so `line.split()[0]` is the pid, not the binary. A scheduler
  that tested `a[0].endswith("/R")` counted zero busy cores and launched 14 x 16-core ensembles at
  once (360 procs on 176 cores). Use `a[1]`.
- Even with correct counting, a launch loop that re-counts 3 s after Popen misses R processes still
  inside the r-mosaic-Rscript wrapper; space launches >= 20 s or count the bash wrappers too.

Oversubscription silently inflates every wall-time you are measuring on the box; log it in the report.
Related: [[reference_dugong_vm]], [[suite-lane-kill-and-check-traps]].
