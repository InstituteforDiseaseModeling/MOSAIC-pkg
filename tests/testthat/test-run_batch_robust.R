# Regression tests for .mosaic_run_batch()'s parallel branch -- the calibration
# (run_MOSAIC) parallel-LASER gather. This is the highest-exposure parallel gather
# in the package (10,000s of sims/country), and it shared the SAME blocking-
# unserialize() worker-death hang that froze the ensemble on the production VM.
#
# .mosaic_run_batch() now routes its parallel branch through the worker-death-robust
# .mosaic_cluster_lapply_robust() gather. These tests guard the calibration-specific
# wiring on top of that helper:
#   1. the dispatched closure resolves .run_sim_worker on the worker's .GlobalEnv
#      after the env is reparented to globalenv() (so per-task dispatch is light);
#   2. a crashed worker is coerced to a SCALAR FALSE (not the list marker), so the
#      calibration success tally `sum(unlist(success_indicators))` stays valid and
#      the batch degrades gracefully instead of hanging.
#
# PSOCK-only; skipped where a PSOCK cluster cannot be created.

.skip_if_no_psock_rb <- function() {
  ok <- tryCatch({
    cl <- parallel::makeCluster(1L, type = "PSOCK")
    parallel::stopCluster(cl)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) testthat::skip("PSOCK cluster unavailable in this environment")
}

test_that(".mosaic_run_batch parallel branch returns scalar logicals via worker globalenv", {
  .skip_if_no_psock_rb()
  cl <- parallel::makeCluster(3L, type = "PSOCK")
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  # Mimic run_MOSAIC: install .run_sim_worker on each worker's .GlobalEnv. The
  # worker_func passed to .mosaic_run_batch references it by name only; its env is
  # reparented to globalenv() inside .mosaic_run_batch so the closure ships light.
  parallel::clusterCall(cl, function() {
    assign(".run_sim_worker", function(sim_id) sim_id %% 2L == 0L, envir = .GlobalEnv)
    NULL
  })

  out <- MOSAIC:::.mosaic_run_batch(
    sim_ids = 1:10,
    worker_func = function(sim_id) .run_sim_worker(sim_id),
    cl = cl, show_progress = FALSE)

  expect_length(out, 10L)
  # Each result is a scalar logical (the calibration worker's contract).
  expect_true(all(vapply(out, function(r) is.logical(r) && length(r) == 1L, logical(1))))
  expect_identical(unlist(out), (1:10) %% 2L == 0L)
  # The exact downstream consumption pattern must work.
  expect_equal(sum(unlist(out)), 5L)
})

test_that(".mosaic_run_batch coerces a crashed worker to scalar FALSE (graceful, no hang)", {
  .skip_if_no_psock_rb()
  skip_on_os("windows")  # tools::pskill(SIGKILL) semantics differ on Windows
  cl <- parallel::makeCluster(4L, type = "PSOCK")
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  # Worker returns TRUE normally; sim_id 6 hard-kills its process (a crash, not an
  # R error -- the case that previously hung the master on Linux).
  parallel::clusterCall(cl, function() {
    assign(".run_sim_worker", function(sim_id) {
      if (sim_id == 6L) tools::pskill(Sys.getpid(), tools::SIGKILL)
      Sys.sleep(0.02)
      TRUE
    }, envir = .GlobalEnv)
    NULL
  })

  t0 <- Sys.time()
  out <- MOSAIC:::.mosaic_run_batch(
    sim_ids = 1:16,
    worker_func = function(sim_id) .run_sim_worker(sim_id),
    cl = cl, show_progress = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_lt(elapsed, 25)                 # MUST NOT hang
  expect_length(out, 16L)
  # The crashed sim is a SCALAR FALSE (NOT the list marker) so unlist()/sum() work.
  expect_true(all(vapply(out, function(r) is.logical(r) && length(r) == 1L, logical(1))))
  expect_false(out[[6]])
  expect_equal(sum(unlist(out)), 15L)    # 15 successes, the crashed one dropped
})
