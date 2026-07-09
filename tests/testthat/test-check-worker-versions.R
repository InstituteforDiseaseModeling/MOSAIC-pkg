test_that("all workers matching orchestrator -> ok, no mismatches", {
  wv <- list(
    "tcp://10.0.0.1:1" = list(laser_cholera = "0.16.1"),
    "tcp://10.0.0.2:1" = list(laser_cholera = "0.16.1")
  )
  res <- MOSAIC:::.mosaic_check_worker_versions("0.16.1", wv)
  expect_true(res$ok)
  expect_equal(res$n_workers, 2L)
  expect_length(res$mismatches, 0L)
  expect_null(res$note)
})

test_that("a differing worker is detected and named with both versions", {
  wv <- list(
    "tcp://10.0.0.1:1" = list(laser_cholera = "0.16.1"),
    "tcp://10.0.0.2:1" = list(laser_cholera = "0.16.0")
  )
  res <- MOSAIC:::.mosaic_check_worker_versions("0.16.1", wv)
  expect_false(res$ok)
  expect_length(res$mismatches, 1L)
  expect_match(res$mismatches, "tcp://10.0.0.2:1", fixed = TRUE)
  expect_match(res$mismatches, "0.16.0", fixed = TRUE)
  expect_match(res$mismatches, "0.16.1", fixed = TRUE)
})

test_that("a worker with NULL / missing laser_cholera counts as mismatch (unknown)", {
  wv <- list(
    "tcp://10.0.0.1:1" = list(laser_cholera = NULL),
    "tcp://10.0.0.2:1" = list()   # missing entirely
  )
  res <- MOSAIC:::.mosaic_check_worker_versions("0.16.1", wv)
  expect_false(res$ok)
  expect_length(res$mismatches, 2L)
  expect_true(all(grepl("unknown", res$mismatches)))
})

test_that("NULL local version -> ok with an explanatory note (no abort)", {
  wv <- list("tcp://10.0.0.1:1" = list(laser_cholera = "0.16.1"))
  res <- MOSAIC:::.mosaic_check_worker_versions(NULL, wv)
  expect_true(res$ok)
  expect_length(res$mismatches, 0L)
  expect_false(is.null(res$note))
})

test_that("zero worker responses -> ok with a note (cannot verify)", {
  res <- MOSAIC:::.mosaic_check_worker_versions("0.16.1", list())
  expect_true(res$ok)
  expect_equal(res$n_workers, 0L)
  expect_false(is.null(res$note))
})

test_that("whitespace-padded versions compare equal", {
  wv <- list("tcp://10.0.0.1:1" = list(laser_cholera = " 0.16.1 "))
  res <- MOSAIC:::.mosaic_check_worker_versions("0.16.1", wv)
  expect_true(res$ok)
  expect_length(res$mismatches, 0L)
})
