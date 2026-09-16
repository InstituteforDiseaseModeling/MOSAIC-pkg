# Regression tests for the PSOCK connection-budget clamp.
#
# Context: R allocates a fixed connection table at startup (128 slots by
# default, 3 taken by stdin/stdout/stderr) and every PSOCK worker holds one
# slot. An UNCLAMPED parallel::makeCluster() therefore throws "all 128
# connections are in use" on a host with more cores than that -- and when the
# throw lands inside a tryCatch (calc_model_ensemble(), called from
# run_MOSAIC()) the whole stage is silently skipped instead of running
# narrower. These tests pin the clamp and pin the requirement that EVERY PSOCK
# site in the package goes through it.

test_that(".mosaic_clamp_psock_workers leaves a request under budget untouched", {
  expect_identical(
    MOSAIC:::.mosaic_clamp_psock_workers(2L, reserve = 2L, verbose = FALSE),
    2L
  )
})

test_that(".mosaic_clamp_psock_workers clamps to free connections minus reserve", {
  free <- as.integer(parallelly::freeConnections())
  expect_identical(
    MOSAIC:::.mosaic_clamp_psock_workers(free + 500L, reserve = 2L, verbose = FALSE),
    max(1L, free - 2L)
  )
  # A larger reserve must give back proportionally fewer workers.
  expect_identical(
    MOSAIC:::.mosaic_clamp_psock_workers(free + 500L, reserve = 10L, verbose = FALSE),
    max(1L, free - 10L)
  )
})

test_that(".mosaic_clamp_psock_workers never returns a non-positive worker count", {
  # Reserve larger than the whole budget must still leave a runnable cluster.
  expect_gte(
    MOSAIC:::.mosaic_clamp_psock_workers(50L, reserve = 100000L, verbose = FALSE),
    1L
  )
})

test_that(".mosaic_clamp_psock_workers explains the remedy when it clamps", {
  free <- as.integer(parallelly::freeConnections())
  expect_message(
    MOSAIC:::.mosaic_clamp_psock_workers(free + 500L, what = "test workers"),
    "--max-connections"
  )
  expect_message(
    MOSAIC:::.mosaic_clamp_psock_workers(free + 500L, what = "test workers"),
    "test workers"
  )
  # Silent when it does not clamp.
  expect_silent(MOSAIC:::.mosaic_clamp_psock_workers(1L))
})

test_that(".mosaic_clamp_psock_workers rejects a malformed request", {
  expect_error(MOSAIC:::.mosaic_clamp_psock_workers(NA_integer_), "single non-NA integer")
  expect_error(MOSAIC:::.mosaic_clamp_psock_workers(c(1L, 2L)), "single non-NA integer")
})

test_that("the clamp is actually binding: a request above budget yields a usable cluster", {
  skip_on_cran()
  free <- as.integer(parallelly::freeConnections())
  n <- MOSAIC:::.mosaic_clamp_psock_workers(free + 500L, reserve = 2L, verbose = FALSE)
  # The point of the clamp is that makeCluster() SUCCEEDS at the clamped size.
  cl <- parallel::makeCluster(min(n, 3L), type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  expect_length(cl, min(n, 3L))
})

test_that("every PSOCK cluster site in R/ routes through the connection clamp", {
  # This is the asymmetry guard. calc_model_ensemble() sat unclamped for
  # months behind make_mosaic_cluster()'s clamp, so a 176-core host lost the
  # ensemble stage entirely. Any NEW parallel::makeCluster() site must either
  # clamp or explain itself here.
  r_dir <- normalizePath(file.path(testthat::test_path(), "..", "..", "R"),
                         mustWork = FALSE)
  skip_if_not(dir.exists(r_dir), "package source R/ not available (installed check)")

  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  creates <- character(0)
  for (f in r_files) {
    txt <- readLines(f, warn = FALSE)
    # Ignore roxygen/comment lines -- only real calls count.
    code <- txt[!grepl("^\\s*#", txt)]
    if (any(grepl("parallel::makeCluster\\(|parallel::makePSOCKcluster\\(", code))) {
      creates <- c(creates, f)
    }
  }
  expect_true(length(creates) > 0L,
              info = "sanity: expected at least one makeCluster site")

  unclamped <- Filter(function(f) {
    code <- readLines(f, warn = FALSE)
    !any(grepl("\\.mosaic_clamp_psock_workers", code))
  }, creates)

  expect_identical(
    basename(unclamped), character(0),
    info = paste0("PSOCK cluster created without the connection clamp in: ",
                  paste(basename(unclamped), collapse = ", "))
  )
})
