# Regression for .mosaic_count_dask_workers(): the heartbeat worker count must
# track the LIVE worker count, not the stale client-side scheduler_info()
# 'workers' snapshot. The old implementation read
# len(scheduler_info()['workers']), which lags as workers join over time (the
# Coiled spin-up case): scale a cluster from 1 to N and that snapshot reports a
# too-low number (verified: stays 5 while 6 workers are live). The fix reads a
# fresh source (client.nthreads()). This test asserts the helper equals the
# scaled count, and that it does NOT equal the stale snapshot when they diverge.

test_that(".mosaic_count_dask_workers tracks the live count after scale-up", {
  skip_on_cran()
  dd <- tryCatch(reticulate::import("dask.distributed"),
                 error = function(e) skip("dask not available"))

  # 6 (not 4) is deliberate: the stale snapshot reliably lags at 5 here while
  # the fresh source reports 6 — at 4 the snapshot often catches up within the
  # sleep window and would not discriminate the bug (verified locally).
  n_target <- 6L
  cl <- dd$LocalCluster(n_workers = 1L, threads_per_worker = 1L,
                        processes = TRUE, dashboard_address = NULL)
  client <- dd$Client(cl)
  on.exit({
    try(client$close(), silent = TRUE)
    try(cl$close(), silent = TRUE)
  }, add = TRUE)

  # Let the single starting worker register.
  Sys.sleep(4)
  expect_equal(MOSAIC:::.mosaic_count_dask_workers(client), 1L)

  # Scale up and wait for the new workers to join the scheduler.
  cl$scale(n_target)
  Sys.sleep(3)
  tryCatch(client$wait_for_workers(n_target, timeout = 40),
           error = function(e) NULL)
  Sys.sleep(2)

  helper <- MOSAIC:::.mosaic_count_dask_workers(client)

  # The fresh count must equal the scaled target.
  expect_equal(helper, n_target)

  # Guard the actual bug: when the stale snapshot diverges from the live count,
  # the helper must follow the live count (n_target), not the stale value.
  raw <- tryCatch({
    py <- reticulate::py_run_string(
      "def _w(c):\n    return len(c.scheduler_info().get('workers', {}))\n",
      convert = TRUE)
    as.integer(py$"_w"(client))
  }, error = function(e) NA_integer_)
  if (!is.na(raw) && raw != n_target) {
    expect_false(helper == raw)
  }
})
