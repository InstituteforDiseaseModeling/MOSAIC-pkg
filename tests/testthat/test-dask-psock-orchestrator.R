# B2: the Dask orchestrator's sample+serialize and parquet-write loops run on a
# PSOCK cluster (fresh R processes) instead of mclapply/fork, so a worker can
# never inherit the parent's reticulate interpreter or live Dask sockets and
# deadlock/corrupt the client. These tests validate the plumbing + the invariant.

test_that("B2: PSOCK plumbing is serial-identical, order-preserving, on reticulate-free workers", {
  cl <- parallel::makeCluster(2L, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, suppressMessages(library(MOSAIC)))

  # Mirror .mosaic_run_batch_dask: export a (namespace) MOSAIC fn, drive it with a
  # globalenv-bound closure via parLapply, and require identical, ordered output.
  parallel::clusterExport(cl, "calc_log_mean_exp", envir = asNamespace("MOSAIC"))
  wf <- function(x) calc_log_mean_exp(x)
  environment(wf) <- globalenv()

  set.seed(3)
  inp    <- lapply(1:12, function(i) sort(rnorm(5, mean = i, sd = 1)))
  psock  <- parallel::parLapply(cl, inp, wf)
  serial <- lapply(inp, calc_log_mean_exp)
  expect_equal(psock, serial, tolerance = 0)      # bit-identical
  expect_equal(length(psock), length(inp))         # order/length preserved

  # Fork-safety property: PSOCK workers must NOT have reticulate loaded, so they
  # cannot touch the parent's Dask client.
  retic <- parallel::clusterEvalQ(cl, "reticulate" %in% loadedNamespaces())
  expect_false(any(unlist(retic)))

  # clusterExport-completeness for NON-exported internals. Production exports the
  # dot-prefixed worker fns (.mosaic_sample_and_serialize / _write_one_shard_dask)
  # via clusterExport — library(MOSAIC) alone does NOT put dot-prefixed internals
  # on a worker's search path, so the calc_log_mean_exp check above (an exported
  # fn) can't prove the export path works. Mirror it with a real non-exported fn:
  # export it from the namespace env and call it bare in a globalenv closure; if
  # the export were incomplete the parLapply would die with "object not found".
  parallel::clusterExport(cl, ".mosaic_set_all_thread_env",
                          envir = asNamespace("MOSAIC"))
  pin <- function(i) { .mosaic_set_all_thread_env(1L); Sys.getenv("ARROW_NUM_THREADS") }
  environment(pin) <- globalenv()
  pinned <- parallel::parLapply(cl, 1:3, pin)
  expect_true(all(vapply(pinned, identical, logical(1), "1")))
})

test_that("B2: orchestrator worker fns are Python/client-free (safe on any worker)", {
  # The submission + write worker bodies must never import reticulate or touch
  # the Dask client — that invariant is what makes running them off the parent
  # process safe. Grep their source as a regression guard.
  for (fn in c(".mosaic_sample_and_serialize", ".mosaic_write_one_shard_dask")) {
    body_txt <- paste(deparse(body(getFromNamespace(fn, "MOSAIC"))), collapse = "\n")
    expect_false(
      grepl("reticulate|client\\$|mosaic_worker|py_run|reticulate::import", body_txt),
      info = fn
    )
  }
})
