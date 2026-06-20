# Tests for the local(n_cores) / remote(dask_spec) separation:
#  Part A — control$parallel$n_cores must NEVER size the remote Coiled cluster;
#           dask_spec$n_workers is the sole remote-count source (required).
#  B1     — the orchestrator thread pin caps Apache Arrow and is inherited by forks.

test_that("Part A: coiled dask_spec requires n_workers; n_cores never substitutes", {
  # config needs only location_name to pass the early guard (run_MOSAIC.R:630);
  # the dask_spec validation fires before any reticulate/cluster work.
  cfg <- list(location_name = "MOZ")
  pri <- list(dummy = 1)              # non-empty list is all the early stopifnot checks
  for (nc in c(1L, 8L, 64L)) {
    ctrl <- MOSAIC::mosaic_control_defaults()
    ctrl$parallel$enable  <- TRUE
    ctrl$parallel$n_cores <- nc       # whatever this is, it must NOT become the cluster size
    expect_error(
      run_MOSAIC(config = cfg, priors = pri, dir_output = tempfile(),
                 control = ctrl, dask_spec = list(type = "coiled")),
      "n_workers must be a positive integer",
      info = paste("n_cores =", nc)
    )
  }
})

test_that("B1: .mosaic_set_all_thread_env pins ARROW_NUM_THREADS and forks inherit it", {
  skip_if_testthat_parallel()  # inner mclapply fork is unsafe inside a testthat worker
  old <- Sys.getenv("ARROW_NUM_THREADS", unset = NA_character_)
  on.exit(if (is.na(old)) Sys.unsetenv("ARROW_NUM_THREADS") else
            Sys.setenv(ARROW_NUM_THREADS = old), add = TRUE)

  MOSAIC:::.mosaic_set_all_thread_env(1L)
  expect_identical(Sys.getenv("ARROW_NUM_THREADS"), "1")
  # also confirm the canonical set is still pinned
  expect_identical(Sys.getenv("OPENBLAS_NUM_THREADS"), "1")

  # Forked children inherit the capped env (the orchestrator mclapply path).
  if (.Platform$OS.type == "unix") {
    kids <- parallel::mclapply(1:3, function(i) Sys.getenv("ARROW_NUM_THREADS"),
                               mc.cores = 2)
    expect_true(all(vapply(kids, identical, logical(1), "1")))
  }
})
