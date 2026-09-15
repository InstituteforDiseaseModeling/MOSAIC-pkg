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
# A SIGKILLed worker does NOT need this -- `stopCluster()` handles a dead node
# fine (verified) -- but the teardown is applied uniformly in the two robustness
# files rather than only where it is strictly required, because "which of these
# clusters can wedge" is exactly the thing a future edit gets wrong.

# Worker PIDs, collected at cluster creation. Must be called BEFORE dispatching
# the task that wedges a worker, or the call blocks behind it.
cluster_worker_pids <- function(cl) {
  tryCatch(unlist(parallel::clusterEvalQ(cl, Sys.getpid())),
           error = function(e) integer(0))
}

# Stop the cluster, then kill any worker that outlived the request.
#
# The PID is re-checked against /proc before signalling: these run late in a
# teardown, and killing a recycled PID belonging to something else would be a
# far worse bug than the leak. Linux-only by design -- on any other platform
# this degrades to plain stopCluster(), which is what the tests did before.
stop_cluster_hard <- function(cl, pids = integer(0)) {
  try(parallel::stopCluster(cl), silent = TRUE)
  if (!length(pids) || !identical(.Platform$OS.type, "unix")) return(invisible(NULL))
  for (p in pids) {
    # readBin, NOT readLines: /proc/<pid>/cmdline is NUL-SEPARATED, and
    # readLines() truncates at the first NUL -- it returns
    # "/usr/lib/R/bin/exec/R" with none of the arguments, so a grepl("RSOCK", .)
    # guard over it can never match and silently disables the kill. That is
    # exactly how the first version of this helper failed while its tests still
    # passed (lesson 13: a guard over the wrong value is dead on arrival).
    cmd <- tryCatch({
      r <- readBin(sprintf("/proc/%d/cmdline", p), "raw", n = 16384L)
      rawToChar(r[r != as.raw(0)])
    }, error = function(e) "")
    # Still alive AND still a PSOCK worker: the shutdown did not land.
    if (grepl("RSOCK", cmd, fixed = TRUE)) {
      try(tools::pskill(p, tools::SIGKILL), silent = TRUE)
    }
  }
  invisible(NULL)
}

# How many of THESE pids are still live PSOCK workers. Counted per-pid rather
# than by scanning /proc for all of them, so a concurrent cluster elsewhere on
# the machine cannot make the teardown's own test flaky. Used by
# test-cluster_teardown.R, which is the only thing that can catch this helper
# silently becoming a no-op.
psock_alive <- function(pids) {
  if (!length(pids)) return(0L)
  sum(vapply(pids, function(p) {
    cmd <- tryCatch({
      r <- readBin(sprintf("/proc/%d/cmdline", p), "raw", n = 16384L)
      rawToChar(r[r != as.raw(0)])
    }, error = function(e) "")
    grepl("RSOCK", cmd, fixed = TRUE)
  }, logical(1)))
}
