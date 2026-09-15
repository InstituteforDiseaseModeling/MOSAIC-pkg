# =============================================================================
# test-cluster_teardown.R
#
# Tests the TEST HELPER in helper-cluster.R, which exists because
# `stopCluster()` cannot stop a worker that is not reading its socket. The
# robustness suites deliberately create exactly that situation (a task calling
# `Sys.sleep()` far past the gather's idle timeout), and the resulting orphan
# inherits the test process's stdout -- so a suite that has already FINISHED
# looks like it is hanging, because the pipe never reaches EOF.
#
# This file exists because the first version of that helper was a no-op and
# every test still passed. It guarded the kill with
# `grepl("RSOCK", readLines("/proc/<pid>/cmdline"))`, and /proc cmdline is
# NUL-separated, so readLines() truncates at the first NUL and returns
# "/usr/lib/R/bin/exec/R" with no arguments -- the guard could never match.
# That is CLAUDE.md lesson 13's shape: a guard over the wrong value is dead on
# arrival, and invisible unless something asserts the effect actually happens.
#
# So the first test below asserts the LEAK (plain stopCluster leaves the worker
# running) and the second asserts the helper REAPS it. Without the negative
# case, a helper that killed nothing would pass the positive one.
# =============================================================================

.skip_unless_reapable <- function() {
  skip_if_testthat_parallel()           # nested PSOCK collides with testthat IPC
  skip_on_os("windows")                 # /proc and SIGKILL semantics
  if (!dir.exists("/proc")) skip("no /proc on this platform")
  ok <- tryCatch({
    cl <- parallel::makeCluster(1L, type = "PSOCK"); parallel::stopCluster(cl); TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) skip("PSOCK cluster unavailable in this environment")
}

# A worker wedged well past the gather's idle timeout, exactly as
# test-ensemble_cluster_robust.R's unresponsive-worker test creates one. The
# sleep is bounded rather than the 600s the real test uses, so a failure here
# cannot leave a ten-minute orphan behind.
wedge_one_worker <- function(cl) {
  g <- function(i) { if (i == 1L) Sys.sleep(45); i }
  invisible(tryCatch(
    MOSAIC:::.mosaic_cluster_lapply_robust(cl = cl, X = as.list(1:2), fun = g,
                                           idle_timeout_sec = 3, progress = FALSE),
    error = function(e) e))
}

test_that("plain stopCluster() does NOT reap a wedged worker (the leak this guards)", {
  .skip_unless_reapable()
  cl <- parallel::makeCluster(2L, type = "PSOCK")
  pids <- cluster_worker_pids(cl)
  expect_equal(psock_alive(pids), 2L)

  wedge_one_worker(cl)
  try(parallel::stopCluster(cl), silent = TRUE)
  Sys.sleep(1)

  # The sleeping worker survives its own cluster being stopped. If this ever
  # starts passing with 0, `parallel` has changed and the helper can go.
  n_left <- psock_alive(pids)
  expect_gt(n_left, 0L)

  # Do not leave it behind for the rest of the suite.
  stop_cluster_hard(cl, pids)
  Sys.sleep(1)
  expect_equal(psock_alive(pids), 0L)
})

test_that("stop_cluster_hard() reaps a wedged worker", {
  .skip_unless_reapable()
  cl <- parallel::makeCluster(2L, type = "PSOCK")
  pids <- cluster_worker_pids(cl)
  expect_equal(psock_alive(pids), 2L)

  wedge_one_worker(cl)
  stop_cluster_hard(cl, pids)
  Sys.sleep(1)
  expect_equal(psock_alive(pids), 0L)
})

test_that("cluster_worker_pids() returns one real pid per worker", {
  .skip_unless_reapable()
  cl <- parallel::makeCluster(2L, type = "PSOCK")
  pids <- cluster_worker_pids(cl)
  on.exit(stop_cluster_hard(cl, pids), add = TRUE)

  expect_length(pids, 2L)
  expect_false(anyDuplicated(pids) > 0L)
  expect_false(Sys.getpid() %in% pids)
  expect_equal(psock_alive(pids), 2L)
})

test_that("stop_cluster_hard() is a no-op on pids that are not PSOCK workers", {
  # The PID-reuse guard. Passed this process's own pid, the helper must not
  # signal it -- if the /proc check ever breaks OPEN (matching everything)
  # rather than closed, the teardown would start killing the test runner.
  .skip_unless_reapable()
  cl <- parallel::makeCluster(1L, type = "PSOCK")
  pids <- cluster_worker_pids(cl)
  stop_cluster_hard(cl, c(pids, Sys.getpid()))
  expect_true(TRUE)   # reaching here at all is the assertion
  expect_equal(psock_alive(pids), 0L)
})
