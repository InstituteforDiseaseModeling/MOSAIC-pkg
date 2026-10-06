---
name: suite-lane-kill-and-check-traps
description: How to kill one dugong suite job safely (setsid lane = process group holding the R master and PSOCK workers; snapshot-then-SIGKILL), the backgrounded-shell-function $! trap that made a test kill itself, and the pre-existing PDF-manual ERROR that --no-manual release checks hide
metadata:
  type: reference
---

Learned building `claude/deploy_v0103/monitors/mem_watchdog.sh` and its test (2026-10-03).

- **A lane's process group IS the job.** `launch.sh` starts each lane with `setsid`, so the lane
  leads its own group, and the job's R master and its PSOCK workers stay in it even after the
  workers are re-parented to init (they are started via `system()`; PPID trees do NOT find them).
  Kill one job = signal every member of the lane's group except the lane: the lane then records
  `failed/<JOB>` (rc 143) and other lanes are untouched. Refuse when `pgid != lane pid` (a lane
  started without setsid shares the operator's group).
- **Snapshot the targets at the trigger.** Re-listing the group for the SIGKILL sweep killed a
  process the lane started AFTER the job died; in production that could be the next job the lane
  just claimed. SIGKILL only survivors of the SIGTERM snapshot.
- **`$!` of a backgrounded shell FUNCTION is the forking subshell**, which stays in the caller's
  process group. `leader() { perl -e 'setpgrp...' "$@"; }; leader cmd &` gave a "lane" pid in the
  test's own group, and the cleanup `kill -KILL $(pgrep -g $PG)` killed the test itself (exit 137,
  no output). Use `( exec perl -e 'setpgrp(0,0); exec @ARGV' cmd ) &` and guard against your own pgid.
- **`wait_for 10 test "$(f)" -ge 2` evaluates `$(f)` once** at the call. Pass the condition as a
  string and `eval` it each iteration.
- **The PDF manual has failed R CMD check for a while.** LaTeX errors on Greek letters and
  maths symbols (beta, psi, sigma, U+2248, U+2265, U+2260) in roxygen-generated Rd. Identical at
  0.102.0 (`R CMD Rd2pdf` on a d50d46f59 export). The release checks of v0.101/v0.103 used
  `--no-manual`, which hides it. A full check otherwise passes (tests, vignettes, the known `:::` NOTE).

Related: [[psock-blocking-gather-worker-death-deadlock]], [[production-suite-protocol]],
[[data-object-rebuild-traps]].
