# Regression tests for .mosaic_cluster_lapply_robust(), the worker-death-robust
# PSOCK gather behind calc_model_ensemble()'s parallel branch.
#
# Background (the bug these guard against): the previous gather used a BLOCKING
# unserialize() (parLapply / pblapply(cl=)). When a PSOCK worker PROCESS crashes
# mid-task (a fatal C-level abort in the embedded Python/numba engine, or an OOM
# kill -- NOT an R-level error, which the worker's tryCatch already converts to a
# success=FALSE record), Linux can block the master forever on the dead peer's
# socket. That is the observed "0 CPU on master AND all workers, frozen" ensemble
# deadlock on the production VM. The robust dispatcher waits on the worker sockets
# with socketSelect(timeout=) and surfaces the failure instead of hanging.
#
# These tests are PSOCK-only; skip where a PSOCK cluster cannot be created.

.skip_if_no_psock <- function() {
  # Spawning a PSOCK cluster (and SIGKILLing its workers) inside a testthat
  # parallel worker collides with testthat's own result IPC and crashes the
  # worker. Run serial-only (CLAUDE.md parallel-over-parallel nesting landmine);
  # the serial devtools::test()/CI run still exercises these.
  skip_if_testthat_parallel()
  ok <- tryCatch({
    cl <- parallel::makeCluster(1L, type = "PSOCK")
    parallel::stopCluster(cl)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) testthat::skip("PSOCK cluster unavailable in this environment")
}

test_that("robust gather returns correct results for more tasks than workers", {
  .skip_if_no_psock()
  cl <- parallel::makeCluster(4L, type = "PSOCK")
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  out <- MOSAIC:::.mosaic_cluster_lapply_robust(
    cl = cl, X = as.list(1:23),
    fun = function(i) i * 10L,
    idle_timeout_sec = 60, progress = FALSE)

  expect_length(out, 23L)
  # Order must be preserved (results indexed by task tag, not arrival order).
  expect_identical(vapply(out, identity, integer(1)), (1:23) * 10L)
})

test_that("a crashed worker PROCESS does not hang the master and degrades gracefully", {
  .skip_if_no_psock()
  skip_on_os("windows")  # tools::pskill(SIGKILL) semantics differ on Windows
  cl <- parallel::makeCluster(4L, type = "PSOCK")
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  # Task 7 hard-kills its own worker process (a process crash, not an R error).
  f <- function(i) {
    if (i == 7L) tools::pskill(Sys.getpid(), tools::SIGKILL)
    Sys.sleep(0.02)
    i
  }

  t0 <- Sys.time()
  out <- MOSAIC:::.mosaic_cluster_lapply_robust(
    cl = cl, X = as.list(1:20), fun = f,
    idle_timeout_sec = 30, progress = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  # MUST NOT hang: completes far inside the idle timeout.
  expect_lt(elapsed, 20)
  expect_length(out, 20L)

  is_dead <- vapply(out, function(r) is.list(r) && isTRUE(r$.mosaic_worker_died),
                    logical(1))
  # Exactly the killed task is recorded as a worker death; the rest succeed.
  expect_equal(sum(is_dead), 1L)
  expect_true(is_dead[7])
  ok_vals <- vapply(out[!is_dead], function(r) is.numeric(r) && length(r) == 1L,
                    logical(1))
  expect_true(all(ok_vals))
})

test_that("an unresponsive worker surfaces a diagnostic stop() within the idle timeout", {
  .skip_if_no_psock()
  cl <- parallel::makeCluster(2L, type = "PSOCK")
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  # Task 1 sleeps far longer than the idle timeout: the gather must STOP, not hang.
  g <- function(i) { if (i == 1L) Sys.sleep(600); i }

  t0 <- Sys.time()
  err <- tryCatch(
    MOSAIC:::.mosaic_cluster_lapply_robust(
      cl = cl, X = as.list(1:2), fun = g,
      idle_timeout_sec = 3, progress = FALSE),
    error = function(e) e)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "stalled|crashed|unresponsive")
  # Stops at (not long past) the timeout; never blocks indefinitely.
  expect_lt(elapsed, 30)
})
