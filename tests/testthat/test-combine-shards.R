# Regression tests for .mosaic_load_and_combine_results()'s chunked branch.
#
# v0.79.0 read each chunk with arrow::open_dataset(unify_schemas = TRUE);
# v0.86.0 reverted that after measuring it 2.1x SLOWER on dugong (100.48 vs
# 47.18 ms/shard at 1,355 columns) where a laptop had shown it 1.57x faster.
#
# These tests survive the revert unchanged because what they pin is the
# CONTRACT, not the implementation: whatever the branch does, it must agree with
# a plain per-file rbindlist(fill = TRUE) on row count, column set, row order
# and values -- including when shards disagree on columns. That contract is what
# makes the implementation safe to swap again.
#
# If anyone re-introduces open_dataset here, note that unify_schemas = TRUE is
# NOT optional: without it open_dataset adopts the FIRST file's schema and
# silently DROPS columns a later file adds. The mismatched-schema test below is
# what catches that, and it is also the reason the fast path was slow -- the
# schema scan is a second full traversal of every file.

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


# =============================================================================
# The SMALL-FILE branch (n_files <= chunk_size) must union schemas too.
#
# Found by the v0.84.0 red-team review (C1, four independent confirmations).
# It used a bare open_dataset(dir), which adopts the FIRST file's schema and
# silently drops any column a later file adds -- while the chunked branch
# tolerated the same case via rbindlist(fill = TRUE). Both return a data frame
# of the right row count, so the gap was invisible.
#
# v0.87.0 made this branch production's normal path: 100 simulations per shard
# turns a 100,000-simulation run into ~1,000 files, under chunk_size = 5000.
# run_MOSAIC.R then deletes the shards, so a dropped column is unrecoverable.
# =============================================================================

test_that("the small-file branch fills mismatched schemas instead of dropping", {
     skip_if_not_installed("arrow")

     set.seed(21)
     dfs <- c(
          lapply(1:4, function(i) data.frame(sim = i, likelihood = rnorm(1), a = runif(1))),
          lapply(5:8, function(i) data.frame(sim = i, likelihood = rnorm(1), z = runif(1)))
     )
     d <- write_shards(file.path(tempdir(), "combine_small_mixed"), dfs)
     on.exit(unlink(d, recursive = TRUE), add = TRUE)

     # chunk_size > n forces the small-file branch
     got  <- MOSAIC:::.mosaic_load_and_combine_results(d, chunk_size = 100L, verbose = FALSE)
     want <- combine_per_file(d)

     expect_identical(nrow(got), 8L)
     # The column a LATER file adds must survive. Without unify_schemas `z`
     # vanishes here and the test fails with 3 columns instead of 4.
     expect_true(all(c("a", "z") %in% names(got)))
     expect_setequal(names(got), names(want))
     expect_true(anyNA(got$a)); expect_true(anyNA(got$z))
     expect_setequal(got$sim, 1:8)
})

test_that("small-file and chunked branches agree on the same mismatched input", {
     skip_if_not_installed("arrow")

     set.seed(22)
     dfs <- c(
          lapply(1:6,  function(i) data.frame(sim = i, likelihood = rnorm(1), a = runif(1))),
          lapply(7:12, function(i) data.frame(sim = i, likelihood = rnorm(1), z = runif(1)))
     )
     d <- write_shards(file.path(tempdir(), "combine_branch_parity"), dfs)
     on.exit(unlink(d, recursive = TRUE), add = TRUE)

     small   <- MOSAIC:::.mosaic_load_and_combine_results(d, chunk_size = 100L, verbose = FALSE)
     chunked <- MOSAIC:::.mosaic_load_and_combine_results(d, chunk_size = 5L,   verbose = FALSE)

     ord <- function(x) { x <- x[order(x$sim), sort(names(x)), drop = FALSE]; rownames(x) <- NULL; x }
     expect_equal(ord(small), ord(chunked))
})
