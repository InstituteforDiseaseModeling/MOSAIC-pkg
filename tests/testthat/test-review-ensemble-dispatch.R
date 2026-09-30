# Regression tests from the production-readiness review (group "ensemble"):
# the PSOCK dispatcher's handling of a task function that throws
# (ensemble-results-09), the removed per-simulation gc() (ensemble-results-08),
# and the dependency-free local-seed helper that replaced withr::with_seed
# (packaging-env-13).

test_that("a task that throws becomes a failed record, not a 'snow-try-error' string", {
  skip_if_testthat_parallel()
  cl <- tryCatch(parallel::makeCluster(2L, type = "PSOCK"), error = function(e) NULL)
  if (is.null(cl)) skip("PSOCK cluster unavailable in this environment")
  .wpids <- cluster_worker_pids(cl)
  on.exit(stop_cluster_hard(cl, .wpids), add = TRUE)

  X <- lapply(1:4, function(i) data.frame(param_idx = i, stoch_idx = 1L))
  f <- function(row) {
    if (row$param_idx == 3L) stop("boom")
    list(param_idx = row$param_idx, stoch_idx = row$stoch_idx, success = TRUE)
  }
  out <- MOSAIC:::.mosaic_cluster_lapply_robust(cl, X, f, idle_timeout_sec = 60,
                                                progress = FALSE)
  expect_true(all(vapply(out, is.list, logical(1))))
  # The consumer pattern in calc_model_ensemble() must not error.
  expect_identical(sum(vapply(out, function(r) isTRUE(r$.mosaic_worker_died), logical(1))), 0L)
  expect_identical(vapply(out, function(r) isTRUE(r$success), logical(1)),
                   c(TRUE, TRUE, FALSE, TRUE))
  expect_identical(out[[3]]$param_idx, 3L)
  expect_identical(out[[3]]$stoch_idx, 1L)
  expect_true(isTRUE(out[[3]]$.mosaic_task_error))
  expect_match(out[[3]]$error, "boom")

  # Atomic task results (the calibration batch returns a logical) pass through.
  out2 <- MOSAIC:::.mosaic_cluster_lapply_robust(cl, as.list(1:3), function(i) i > 1L,
                                                 idle_timeout_sec = 60, progress = FALSE)
  expect_identical(unlist(out2), c(FALSE, TRUE, TRUE))
})

test_that("the ensemble worker no longer forces a per-simulation gc()", {
  src <- paste(deparse(MOSAIC:::.mosaic_ensemble_sim_task), collapse = " ")
  expect_false(grepl("gc(", src, fixed = TRUE))
})

test_that("trajectory thinning does not call withr (a Suggests-only package)", {
  src <- paste(deparse(MOSAIC:::.mosaic_build_trajectories), collapse = " ")
  expect_false(grepl("withr::", src, fixed = TRUE))
})

test_that(".mosaic_with_local_seed is reproducible and restores the caller's RNG state", {
  set.seed(1)
  before <- .Random.seed
  a <- MOSAIC:::.mosaic_with_local_seed(20260625L, sample(100, 5))
  expect_identical(.Random.seed, before)
  b <- MOSAIC:::.mosaic_with_local_seed(20260625L, sample(100, 5))
  expect_identical(a, b)
  set.seed(20260625L)
  expect_identical(a, sample(100, 5))

  # With no .Random.seed at all, none is left behind.
  had <- exists(".Random.seed", envir = globalenv())
  saved <- if (had) get(".Random.seed", envir = globalenv())
  rm(".Random.seed", envir = globalenv())
  on.exit(if (had) assign(".Random.seed", saved, envir = globalenv()), add = TRUE)
  MOSAIC:::.mosaic_with_local_seed(1L, stats::runif(1))
  expect_false(exists(".Random.seed", envir = globalenv()))
})

test_that("dispatch failures are surfaced with their count and first error text", {
  res <- list(
    list(param_idx = 1L, success = TRUE),
    list(param_idx = 2L, .mosaic_task_error = TRUE, success = FALSE,
         error = "Error in .run_sim_worker(sim_id) : could not find function"),
    list(param_idx = 3L, .mosaic_task_error = TRUE, success = FALSE, error = "second"),
    list(.mosaic_worker_died = TRUE, success = FALSE, error = "connection reset"),
    TRUE)
  w <- testthat::capture_warnings(
    n <- MOSAIC:::.mosaic_warn_dispatch_failures(res, "run_MOSAIC simulation batch"))
  expect_identical(n, c(worker_died = 1L, task_error = 2L))
  expect_length(w, 2L)
  expect_match(w[1], "1 task\\(s\\) lost to worker-process crashes.*First: connection reset")
  expect_match(w[2], "2 task\\(s\\) failed because the task function threw.*could not find function")
  expect_silent(MOSAIC:::.mosaic_warn_dispatch_failures(list(TRUE, FALSE, list(success = TRUE)), "x"))
})

test_that("the calibration batch warns about task errors instead of dropping them", {
  src <- paste(deparse(MOSAIC:::.mosaic_run_batch), collapse = " ")
  expect_true(grepl(".mosaic_warn_dispatch_failures", src, fixed = TRUE))
  src_e <- paste(deparse(calc_model_ensemble), collapse = " ")
  expect_true(grepl(".mosaic_warn_dispatch_failures", src_e, fixed = TRUE))
})
