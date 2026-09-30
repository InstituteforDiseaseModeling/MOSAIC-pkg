# =============================================================================
# test-read_json_to_list_cache.R
#
# The exported reader must stay PURE (no caching) and the internal cached
# reader must be correct about invalidation. Getting the second one wrong is a
# stale-config bug that shows up as a simulation silently running the previous
# config -- which no parity test would catch, because both runs would be
# self-consistent.
# =============================================================================

cfg_path <- function() system.file("extdata", "config_default.json", package = "MOSAIC")

test_that("the path route parses identically to the old readLines+paste route", {
  f <- cfg_path(); skip_if(f == "")
  old_route <- jsonlite::fromJSON(paste(readLines(f), collapse = "\n"))
  expect_identical(read_json_to_list(f), old_route)
})

test_that("the cached reader returns the same value as the pure reader", {
  f <- cfg_path(); skip_if(f == "")
  expect_identical(MOSAIC:::.mosaic_read_json_cached(f), read_json_to_list(f))
})

test_that("the cached reader is equivalent to the fromJSON options it replaced", {
  # sim_params() used simplifyVector = TRUE; run_fit_sandbox() and the
  # rolling-CV reader also passed simplifyMatrix = TRUE. All three are the
  # fromJSON defaults, which is what makes one shared reader safe.
  f <- cfg_path(); skip_if(f == "")
  expect_identical(MOSAIC:::.mosaic_read_json_cached(f),
                   jsonlite::fromJSON(f, simplifyVector = TRUE))
  expect_identical(MOSAIC:::.mosaic_read_json_cached(f),
                   jsonlite::fromJSON(f, simplifyVector = TRUE, simplifyMatrix = TRUE))
})

test_that("a cache hit is served without re-reading, and is not stale after a rewrite", {
  tmp <- file.path(tempdir(), "cachetest.json")
  jsonlite::write_json(list(a = 1L), tmp, auto_unbox = TRUE)
  first <- MOSAIC:::.mosaic_read_json_cached(tmp)
  expect_identical(first$a, 1L)

  # Same content, same size -- must still be a hit.
  expect_identical(MOSAIC:::.mosaic_read_json_cached(tmp), first)

  # Rewrite with DIFFERENT content. mtime moves, so the cache must invalidate.
  Sys.sleep(0.01)
  jsonlite::write_json(list(a = 2L), tmp, auto_unbox = TRUE)
  expect_identical(MOSAIC:::.mosaic_read_json_cached(tmp)$a, 2L)

  # Rewrite with different content of the SAME byte length -- size alone would
  # not catch this, which is why mtime is part of the key.
  Sys.sleep(0.01)
  jsonlite::write_json(list(a = 3L), tmp, auto_unbox = TRUE)
  expect_identical(MOSAIC:::.mosaic_read_json_cached(tmp)$a, 3L)
  unlink(tmp)
})

test_that("the cache is bounded", {
  d <- file.path(tempdir(), "cachebound"); dir.create(d, showWarnings = FALSE)
  paths <- file.path(d, sprintf("c%02d.json", 1:12))
  for (i in seq_along(paths)) jsonlite::write_json(list(i = i), paths[i], auto_unbox = TRUE)
  for (p in paths) invisible(MOSAIC:::.mosaic_read_json_cached(p, max_entries = 4L))
  expect_lte(length(ls(MOSAIC:::.MOSAIC_JSON_CACHE, all.names = TRUE)), 4L)
  # Correct values survive the bound.
  expect_identical(MOSAIC:::.mosaic_read_json_cached(paths[12], max_entries = 4L)$i, 12L)
  unlink(d, recursive = TRUE)
})

test_that("run_simulation() accepts a config path and the cache does not change results", {
  f <- cfg_path(); skip_if(f == "")
  skip_on_cran()
  cfg <- read_json_to_list(f)
  # Keep it cheap: two locations and 21 ticks. Both dimensions have to be cut
  # together -- shortening date_stop alone leaves the *_jt matrices at 1398
  # columns and sim_params() rejects the mismatch.
  n  <- length(cfg$location_name)
  nt <- as.integer(as.Date(cfg$date_stop) - as.Date(cfg$date_start)) + 1L
  keep_t <- 21L
  small <- cfg
  for (nm in names(cfg)) {
    v <- cfg[[nm]]
    if (is.matrix(v)) {
      if (nrow(v) == n && ncol(v) == n)       small[[nm]] <- v[1:2, 1:2, drop = FALSE]
      else if (nrow(v) == n && ncol(v) == nt) small[[nm]] <- v[1:2, 1:keep_t, drop = FALSE]
      else if (nrow(v) == n)                  small[[nm]] <- v[1:2, , drop = FALSE]
    } else if (is.atomic(v) && !is.null(v) && length(v) == n) small[[nm]] <- v[1:2]
  }
  small$location_name <- cfg$location_name[1:2]
  small$date_stop <- as.character(as.Date(small$date_start) + keep_t - 1L)

  tmp <- file.path(tempdir(), "small_cfg.json")
  MOSAIC::write_list_to_json(small, tmp)

  a <- run_simulation(config = tmp,   seed = 5L, quiet = TRUE)   # cache miss
  b <- run_simulation(config = tmp,   seed = 5L, quiet = TRUE)   # cache hit
  d <- run_simulation(config = small, seed = 5L, quiet = TRUE)   # in-memory
  expect_identical(a$results, b$results)
  expect_identical(a$results, d$results)
  unlink(tmp)
})
