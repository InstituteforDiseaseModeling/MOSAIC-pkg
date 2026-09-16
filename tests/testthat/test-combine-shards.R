# Regression tests for .mosaic_load_and_combine_results()'s chunked fast path.
#
# The >chunk_size branch reads each chunk with arrow::open_dataset() on the file
# vector instead of one read_parquet() per file. open_dataset() takes the FIRST
# file's schema, so shards that disagree on columns must fall back to the
# per-file rbindlist(fill = TRUE) path that tolerated them. These tests pin both
# halves: the fast path must agree with the per-file path exactly, and the
# fallback must still fill.

write_shards <- function(dir, dfs) {
     dir.create(dir, recursive = TRUE, showWarnings = FALSE)
     for (i in seq_along(dfs)) {
          arrow::write_parquet(dfs[[i]], file.path(dir, sprintf("sim_%07d.parquet", i)))
     }
     dir
}

# The per-file path this replaced, kept here as the oracle.
combine_per_file <- function(dir) {
     files <- list.files(dir, pattern = "^sim_.*\\.parquet$", full.names = TRUE)
     as.data.frame(data.table::rbindlist(lapply(files, arrow::read_parquet), fill = TRUE))
}

test_that("chunked fast path matches the per-file path when schemas agree", {
     skip_if_not_installed("arrow")

     set.seed(11)
     n <- 25L
     dfs <- lapply(seq_len(n), function(i) {
          data.frame(sim = i, likelihood = rnorm(1), a = runif(1), b = runif(1))
     })
     d <- write_shards(file.path(tempdir(), "combine_ok"), dfs)
     on.exit(unlink(d, recursive = TRUE), add = TRUE)

     # chunk_size < n forces the chunked branch (3 chunks of 10/10/5)
     got <- MOSAIC:::.mosaic_load_and_combine_results(d, chunk_size = 10L, verbose = FALSE)
     want <- combine_per_file(d)

     expect_identical(dim(got), dim(want))
     expect_identical(names(got), names(want))
     expect_equal(got, want)
     expect_identical(nrow(got), n)
})

test_that("mismatched shard schemas fall back and still fill", {
     skip_if_not_installed("arrow")

     set.seed(12)
     # Shards 1-10 carry column `a`; shards 11-20 carry `z` instead. Chunk 2 is
     # internally consistent, so the mismatch that matters is chunk-spanning as
     # well as within-chunk -- build one of each.
     dfs <- c(
          lapply(1:10,  function(i) data.frame(sim = i, likelihood = rnorm(1), a = runif(1))),
          lapply(11:15, function(i) data.frame(sim = i, likelihood = rnorm(1), z = runif(1))),
          lapply(16:20, function(i) data.frame(sim = i, likelihood = rnorm(1), a = runif(1)))
     )
     d <- write_shards(file.path(tempdir(), "combine_mixed"), dfs)
     on.exit(unlink(d, recursive = TRUE), add = TRUE)

     got  <- MOSAIC:::.mosaic_load_and_combine_results(d, chunk_size = 10L, verbose = FALSE)
     want <- combine_per_file(d)

     expect_equal(got, want)
     expect_identical(nrow(got), 20L)
     expect_true(all(c("a", "z") %in% names(got)))
     # The filled cells are the point: `z` is absent from shards 1-10.
     expect_true(anyNA(got$z))
     expect_true(anyNA(got$a))
})

test_that("the small-file branch is unaffected", {
     skip_if_not_installed("arrow")

     set.seed(13)
     dfs <- lapply(1:6, function(i) data.frame(sim = i, likelihood = rnorm(1), a = runif(1)))
     d <- write_shards(file.path(tempdir(), "combine_small"), dfs)
     on.exit(unlink(d, recursive = TRUE), add = TRUE)

     # chunk_size > n keeps the open_dataset(dir) branch
     got <- MOSAIC:::.mosaic_load_and_combine_results(d, chunk_size = 100L, verbose = FALSE)
     expect_identical(nrow(got), 6L)
     expect_setequal(got$sim, 1:6)
})
