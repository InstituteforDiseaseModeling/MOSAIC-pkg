# Teardown for PSOCK clusters in tests that deliberately wedge or crash a worker.
#
# Why this exists: `stopCluster()` asks each worker to shut down by writing to
# its socket. A worker that is not READING that socket never gets the message.
# `test-ensemble_cluster_robust.R` has a task that calls `Sys.sleep(600)` on
# purpose -- that is the point of the test, which asserts the gather stops
# rather than hanging -- so `stopCluster()` returns cleanly while the worker
# sleeps on for ten more minutes.
#
# A leaked worker inherited the test process's stdout, so the pipe never
# reaches EOF and a test suite that has ALREADY FINISHED looks like it is
# hanging: `devtools::test() | tail` produces nothing and the R master is gone
# from the process table. It also holds ~1 GB and one of R's 128 connection
# slots, which is the likely cause of unrelated cluster tests failing later in
# the same run.
#
# The implementation lives in the package (R/cluster_teardown.R), because
# production has the same failure mode: an interrupted run_MOSAIC() leaks its
# workers the same way. These are thin wrappers so the tests exercise the
# production code rather than a parallel copy of it.
#
# A SIGKILLed worker does NOT need this -- `stopCluster()` handles a dead node
# fine (verified) -- but the teardown is applied uniformly in the two robustness
# files rather than only where it is strictly required, because "which of these
# clusters can wedge" is exactly the thing a future edit gets wrong.

# Worker PIDs, collected at cluster creation. Must be called BEFORE dispatching
# the task that wedges a worker, or the call blocks behind it.
cluster_worker_pids <- function(cl) MOSAIC:::.mosaic_cluster_worker_pids(cl)

# Stop the cluster, then kill any worker that outlived the request.
stop_cluster_hard <- function(cl, pids = integer(0)) {
  MOSAIC:::.mosaic_stop_cluster(cl, pids)
}

# How many of THESE pids are still live PSOCK workers. Counted per-pid rather
# than by scanning /proc for all of them, so a concurrent cluster elsewhere on
# the machine cannot make the teardown's own test flaky.
psock_alive <- function(pids) MOSAIC:::.mosaic_psock_alive(pids)
