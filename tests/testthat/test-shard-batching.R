# =============================================================================
# Shard batching (pipeline plan item 6b): control$io$shard_batch_size lets one
# parquet carry several simulations instead of one each.
#
# The invariant that matters is that NOTHING downstream can tell the difference
# except the file count: the resume watermark, the combined frame, and the
# success tally must all be identical to the one-file-per-simulation layout.
# =============================================================================

# The real io control: .mosaic_write_parquet() reads $compression and
# $compression_level, so a stub list would fail the write for the wrong reason.
test_io <- function() mosaic_control_defaults()$io

test_that("shard_batch_size resolves garbage to the safe default of 1", {
  f <- MOSAIC:::.mosaic_resolve_shard_batch
  expect_identical(f(NULL),          1L)
  expect_identical(f(NA),            1L)
  expect_identical(f(NA_integer_),   1L)
  expect_identical(f("nonsense"),    1L)
  expect_identical(f(0L),            1L)
  expect_identical(f(-5L),           1L)
  expect_identical(f(1L),            1L)
  expect_identical(f(100L),          100L)
  expect_identical(f(100.9),         100L)   # truncates, never rounds up
  expect_identical(f(c(50L, 99L)),   50L)    # first element wins
})

test_that("ids chunk contiguously and lose nothing", {
  f <- MOSAIC:::.mosaic_chunk_ids

  expect_identical(f(integer(0), 10L), list())

  # size 1 keeps the historical one-task-per-simulation shape
  expect_identical(f(1:3, 1L), list(1L, 2L, 3L))

  ch <- f(1:10, 4L)
  expect_length(ch, 3L)
  expect_identical(ch[[1]], 1:4)
  expect_identical(ch[[2]], 5:8)
  expect_identical(ch[[3]], 9:10)          # short final chunk
  expect_identical(unlist(ch), 1:10)       # nothing dropped or duplicated

  # chunks are contiguous, which is what makes the min-max filename meaningful
  for (c_i in f(101:250, 25L)) {
    expect_identical(c_i, seq(min(c_i), max(c_i)))
  }

  # a chunk larger than the batch returns one chunk
  expect_identical(f(1:5, 100L), list(1:5))
})

test_that("a single-id chunk keeps the historical filename", {
  # This is what makes shard_batch_size = 1 a true no-op: an existing output
  # directory must stay resumable and the file names must not move.
  d <- withr::local_tempdir()
  fake_row <- matrix(c(7, 1, 7, NA, -3.5), nrow = 1,
                     dimnames = list(NULL, c("sim", "iter", "seed_sim",
                                             "seed_iter", "likelihood")))
  local_mocked_bindings(
    .mosaic_run_simulation_worker = function(sim_id, ...) {
      m <- fake_row; m[1, "sim"] <- sim_id; m[1, "seed_sim"] <- sim_id; m
    },
    .package = "MOSAIC"
  )

  ok <- MOSAIC:::.mosaic_run_simulation_chunk(
    sim_ids = 7L, dir_cal_samples = d, io = test_io())

  expect_true(all(ok))
  expect_true(file.exists(file.path(d, "sim_0000007.parquet")))
  expect_false(file.exists(file.path(d, "sim_0000007-0000007.parquet")))
})

test_that("a multi-id chunk writes one shard named for its range", {
  d <- withr::local_tempdir()
  local_mocked_bindings(
    .mosaic_run_simulation_worker = function(sim_id, ...) {
      matrix(c(sim_id, 1, sim_id, NA, -sim_id), nrow = 1,
             dimnames = list(NULL, c("sim", "iter", "seed_sim",
                                     "seed_iter", "likelihood")))
    },
    .package = "MOSAIC"
  )

  ok <- MOSAIC:::.mosaic_run_simulation_chunk(
    sim_ids = 11:20, dir_cal_samples = d, io = test_io())

  expect_identical(ok, rep(TRUE, 10L))
  expect_identical(list.files(d), "sim_0000011-0000020.parquet")

  got <- as.data.frame(arrow::read_parquet(file.path(d, list.files(d))))
  expect_identical(nrow(got), 10L)
  expect_identical(as.integer(got$sim), 11:20)
  expect_identical(got$likelihood, as.numeric(-(11:20)))

  # the resume scan must see all ten, from the column not the name
  scan <- MOSAIC:::.mosaic_resume_scan(d)
  expect_identical(scan$n, 10L)
  expect_identical(scan$watermark, 20L)
  expect_identical(scan$ids, 11:20)
})

test_that("one failed simulation does not cost its chunk-mates", {
  d <- withr::local_tempdir()
  local_mocked_bindings(
    .mosaic_run_simulation_worker = function(sim_id, ...) {
      if (sim_id == 3L) return(NULL)               # draw rejected
      if (sim_id == 5L) stop("engine blew up")     # R-level error
      matrix(c(sim_id, 1, sim_id, NA, -1), nrow = 1,
             dimnames = list(NULL, c("sim", "iter", "seed_sim",
                                     "seed_iter", "likelihood")))
    },
    .package = "MOSAIC"
  )

  ok <- MOSAIC:::.mosaic_run_simulation_chunk(
    sim_ids = 1:6, dir_cal_samples = d, io = test_io())

  expect_identical(ok, c(TRUE, TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_identical(sum(ok), 4L)

  got <- as.data.frame(arrow::read_parquet(file.path(d, list.files(d))))
  expect_identical(as.integer(got$sim), c(1L, 2L, 4L, 6L))
})

test_that("a chunk whose simulations all fail writes nothing", {
  d <- withr::local_tempdir()
  local_mocked_bindings(
    .mosaic_run_simulation_worker = function(sim_id, ...) NULL,
    .package = "MOSAIC"
  )

  ok <- MOSAIC:::.mosaic_run_simulation_chunk(
    sim_ids = 1:4, dir_cal_samples = d, io = test_io())

  expect_identical(ok, rep(FALSE, 4L))
  expect_identical(list.files(d), character(0))
})

test_that("a chunk reports failure when the shard cannot be written", {
  # A row computed but not persisted is not a completed simulation; claiming it
  # would drift the batch tally from what resume can actually see on disk.
  d <- withr::local_tempdir()
  local_mocked_bindings(
    .mosaic_run_simulation_worker = function(sim_id, ...) {
      matrix(c(sim_id, 1, sim_id, NA, -1), nrow = 1,
             dimnames = list(NULL, c("sim", "iter", "seed_sim",
                                     "seed_iter", "likelihood")))
    },
    .mosaic_write_parquet = function(...) stop("disk full"),
    .package = "MOSAIC"
  )

  expect_warning(
    ok <- MOSAIC:::.mosaic_run_simulation_chunk(
      sim_ids = 1:3, dir_cal_samples = d, io = test_io()),
    "shard write failed for sims 1-3"
  )
  expect_identical(ok, rep(FALSE, 3L))
})

test_that("shard_batch_size is a documented io control defaulting to 100", {
  # Flipped from 1L in v0.87.0 after measuring, on dugong at production width:
  # 57x faster to combine, 174.2 -> 1.5 min resume scan at 100k, 34.6x smaller
  # on disk, and byte-identical samples.parquet.
  ctrl <- mosaic_control_defaults()
  expect_true("shard_batch_size" %in% names(ctrl$io))
  expect_identical(ctrl$io$shard_batch_size, 100L)
  expect_identical(mosaic_control_defaults(io = list(shard_batch_size = 250L))$io$shard_batch_size,
                   250L)
  expect_identical(mosaic_control_defaults(io = list(shard_batch_size = 1L))$io$shard_batch_size,
                   1L)
})

test_that("the batch is clamped so it cannot starve the cluster", {
  f <- MOSAIC:::.mosaic_resolve_shard_batch

  # Without sizing context the requested value stands (back-compatible).
  expect_identical(f(100L), 100L)

  # A fixed batch is a fixed number of TASKS. 500 simulations at 100 per shard
  # is 5 tasks, so on 24 workers only 5 would do anything. Clamp to keep at
  # least tasks_per_worker tasks each.
  expect_identical(f(100L, n_sims = 500L,   n_workers = 24L), 5L)
  expect_identical(f(100L, n_sims = 2000L,  n_workers = 24L), 20L)

  # At production scale the clamp never binds: 100k / (80 * 4) = 312 > 100.
  expect_identical(f(100L, n_sims = 100000L, n_workers = 80L), 100L)

  # Tiny budgets fall all the way back to one simulation per shard.
  expect_identical(f(100L, n_sims = 50L, n_workers = 24L), 1L)

  # Serial runs are one "worker", so the clamp is generous.
  expect_identical(f(100L, n_sims = 1000L, n_workers = 1L), 100L)

  # Garbage context is ignored rather than propagated.
  expect_identical(f(100L, n_sims = NA_integer_, n_workers = 24L), 100L)
  expect_identical(f(100L, n_sims = 500L, n_workers = NA_integer_), 100L)
  expect_identical(f(100L, n_sims = 0L, n_workers = 24L), 100L)

  # The clamp can never raise the requested value.
  expect_identical(f(1L, n_sims = 100000L, n_workers = 80L), 1L)
})
