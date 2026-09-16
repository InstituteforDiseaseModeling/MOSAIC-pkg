# Tests for run_MOSAIC() resume capability (resume = TRUE).
#
# These exercise the factored, engine-free internals so they run without the
# Python/simulation engine. The full end-to-end interrupt/resume reproducibility
# check lives in the smoke test (claude/smoke_test_resume.R) since it needs the
# simulation engine.

# ---- helpers ---------------------------------------------------------------

new_samples_dir <- function() {
  d <- file.path(tempfile("resume_"), "samples")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

make_shard <- function(dir, sim_id, rows = 1L) {
  df <- data.frame(
    sim       = rep(as.integer(sim_id), rows),
    iter      = 1L,
    seed_sim  = as.integer(sim_id),
    seed_iter = NA_real_,
    likelihood = -10
  )
  arrow::write_parquet(df, file.path(dir, sprintf("sim_%07d.parquet", sim_id)))
}

state_template <- function() {
  list(
    total_sims_run = 0L, total_sims_successful = 0L, batch_number = 0L,
    batch_success_rates = numeric(), batch_sizes_used = integer(),
    phase = "calibration", calib_batches = 0L, r2_ess = NA_real_,
    calibration_done = FALSE, ess_history = list(), ess_tracking = list(),
    param_names_est = c("a", "b"), converged = FALSE, predictive_done = FALSE,
    mode = "auto", fixed_target = NA_integer_, phase_batch_count = 0L,
    phase_last = NULL
  )
}

# ---- signature -------------------------------------------------------------

test_that("run_MOSAIC signature adds resume defaulting to FALSE", {
  fmls <- formals(run_MOSAIC)
  expect_true("resume" %in% names(fmls))
  expect_identical(eval(fmls$resume), FALSE)
  # Only addition; rest of the public signature is unchanged.
  expect_setequal(
    names(fmls),
    c("config", "priors", "dir_output", "control", "resume", "cluster", "...")
  )
})

# ---- .mosaic_resume_scan ---------------------------------------------------

test_that(".mosaic_resume_scan uses max id as watermark, not count", {
  d <- new_samples_dir()
  for (id in c(1L, 2L, 3L, 5L, 8L)) make_shard(d, id)
  scan <- MOSAIC:::.mosaic_resume_scan(d)
  expect_equal(scan$watermark, 8L)   # critical: max, never count (5)
  expect_equal(scan$n, 5L)
  expect_equal(scan$ids, c(1L, 2L, 3L, 5L, 8L))
})

test_that(".mosaic_resume_scan handles empty and missing directories", {
  d <- new_samples_dir()
  expect_equal(MOSAIC:::.mosaic_resume_scan(d)$n, 0L)
  expect_equal(MOSAIC:::.mosaic_resume_scan(d)$watermark, 0L)
  expect_equal(MOSAIC:::.mosaic_resume_scan(file.path(d, "does_not_exist"))$n, 0L)
})

test_that(".mosaic_resume_scan quarantines bad shards and sweeps temp files", {
  d <- new_samples_dir()
  make_shard(d, 1L); make_shard(d, 2L)
  writeLines("garbage", file.path(d, "sim_0000003.parquet"))  # corrupt
  file.create(file.path(d, "sim_0000004.parquet"))            # zero-byte
  file.create(file.path(d, ".mosaic_tmp_sim_0000009.parquet_abc"))  # orphaned temp

  scan <- MOSAIC:::.mosaic_resume_scan(d)

  expect_equal(scan$ids, c(1L, 2L))
  expect_equal(scan$watermark, 2L)
  expect_false(file.exists(file.path(d, ".mosaic_tmp_sim_0000009.parquet_abc")))
  expect_true(file.exists(file.path(d, ".quarantine", "sim_0000003.parquet")))
  expect_true(file.exists(file.path(d, ".quarantine", "sim_0000004.parquet")))
})

# ---- ids come from the `sim` column, not the filename ----------------------
#
# The scan reads each shard's `sim` column rather than parsing its name. That is
# what lets one shard carry many simulations (pipeline plan item 6b) without the
# resume watermark -- the guarantee that a resumed run never reuses a seed --
# depending on a filename convention.

# A shard holding several simulations, named for the range it covers.
make_batch_shard <- function(dir, sim_ids) {
  df <- data.frame(
    sim        = as.integer(sim_ids),
    iter       = 1L,
    seed_sim   = as.integer(sim_ids),
    seed_iter  = NA_real_,
    likelihood = -10
  )
  arrow::write_parquet(df, file.path(dir, sprintf("sim_%07d-%07d.parquet",
                                                  min(sim_ids), max(sim_ids))))
}

test_that(".mosaic_resume_scan counts every simulation in a multi-row shard", {
  d <- new_samples_dir()
  make_batch_shard(d, 1:100)
  make_batch_shard(d, 101:150)

  scan <- MOSAIC:::.mosaic_resume_scan(d)

  expect_equal(scan$n, 150L)            # simulations, not files
  expect_equal(scan$watermark, 150L)    # the seed frontier
  expect_equal(scan$ids, 1:150)
})

test_that("batched and one-per-file shards give an identical scan", {
  d1 <- new_samples_dir(); for (id in 1:20) make_shard(d1, id)
  d2 <- new_samples_dir(); make_batch_shard(d2, 1:20)

  a <- MOSAIC:::.mosaic_resume_scan(d1)
  b <- MOSAIC:::.mosaic_resume_scan(d2)

  expect_identical(a$ids, b$ids)
  expect_identical(a$n, b$n)
  expect_identical(a$watermark, b$watermark)
})

test_that("the sim column wins when it disagrees with the filename", {
  # A shard whose name says 9 but whose data says 42. The column is what the
  # combine and every downstream consumer use, so the scan must agree with them
  # -- otherwise the frontier could hand out an id that already exists.
  d <- new_samples_dir()
  arrow::write_parquet(
    data.frame(sim = 42L, iter = 1L, seed_sim = 42L, seed_iter = NA_real_,
               likelihood = -1),
    file.path(d, "sim_0000009.parquet"))

  scan <- MOSAIC:::.mosaic_resume_scan(d)

  expect_equal(scan$ids, 42L)
  expect_equal(scan$watermark, 42L)
})

test_that("a multi-row shard with an unreadable row set is quarantined whole", {
  d <- new_samples_dir()
  make_batch_shard(d, 1:10)
  # no `sim` column at all -> the whole shard is unusable
  arrow::write_parquet(
    data.frame(iter = 1L, likelihood = -3),
    file.path(d, "sim_0000011-0000020.parquet"))

  scan <- MOSAIC:::.mosaic_resume_scan(d)

  expect_equal(scan$ids, 1:10)
  expect_equal(scan$watermark, 10L)
  expect_true(file.exists(file.path(d, ".quarantine",
                                    "sim_0000011-0000020.parquet")))
})

# ---- shard validation (read data + schema/type, not just num_rows) ---------

test_that(".mosaic_resume_scan quarantines schema/type-invalid shards", {
  d <- new_samples_dir()
  make_shard(d, 1L); make_shard(d, 2L)
  # likelihood written as a string → must be rejected (would coerce the whole
  # combined column to character and silently zero the posterior)
  arrow::write_parquet(
    data.frame(sim = 3L, iter = 1L, seed_sim = 3L, seed_iter = NA_real_,
               likelihood = "not_a_number"),
    file.path(d, "sim_0000003.parquet"))
  # missing the likelihood column entirely → must be rejected
  arrow::write_parquet(
    data.frame(sim = 4L, iter = 1L, seed_sim = 4L, foo = 1.0),
    file.path(d, "sim_0000004.parquet"))

  scan <- MOSAIC:::.mosaic_resume_scan(d)
  expect_equal(scan$ids, c(1L, 2L))        # only the well-formed shards survive
  expect_equal(scan$watermark, 2L)
  expect_true(file.exists(file.path(d, ".quarantine", "sim_0000003.parquet")))
  expect_true(file.exists(file.path(d, ".quarantine", "sim_0000004.parquet")))
})

# ---- control$likelihood drift guard ----------------------------------------

test_that(".mosaic_resume_check_inputs guards control$likelihood drift", {
  base <- tempfile("ctl_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)
  priors <- list(a = 1); config <- list(location_name = "ETH")
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  wj(priors, file.path(inp, "priors.json"))
  wj(config, file.path(inp, "config.json"))

  lik <- list(weight_cases = 1, weight_deaths = 1, weight_wis = 0)
  control <- list(likelihood = lik)
  # control.json is written as control_record: list(control = control, ...)
  wj(list(control = control, timestamp = "t0"), file.path(inp, "control.json"))

  # identical likelihood → passes
  expect_true(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, control)))

  # changed weight → hard error
  control2 <- list(likelihood = list(weight_cases = 1, weight_deaths = 2, weight_wis = 0))
  expect_error(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, control2),
               "control\\$likelihood")

  # control omitted (back-compat) → likelihood check skipped, no error
  # suppressWarnings: no environment.json here, so the engine-version guard
  # correctly emits a "guard SKIPPED" warning that is orthogonal to this test.
  expect_true(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors)))
})

test_that(".mosaic_resume_check_inputs guards sampling, n_iterations, and mode drift", {
  base <- tempfile("ctl2_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)
  priors <- list(a = 1); config <- list(location_name = "ETH")
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  wj(priors, file.path(inp, "priors.json"))
  wj(config, file.path(inp, "config.json"))

  control <- list(
    likelihood  = list(weight_cases = 1),
    sampling    = list(sample_beta_j0_tot = TRUE, sample_tau_i = FALSE),
    calibration = list(n_iterations = 3L, n_simulations = NULL)  # auto mode
  )
  wj(list(control = control, timestamp = "t0"), file.path(inp, "control.json"))

  # identical → passes
  expect_true(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, control)))

  # sampling flag flipped → error
  c_samp <- control; c_samp$sampling$sample_tau_i <- TRUE
  expect_error(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, c_samp)),
               "control\\$sampling")

  # n_iterations changed → error
  c_iter <- control; c_iter$calibration$n_iterations <- 5L
  expect_error(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, c_iter)),
               "n_iterations")

  # mode switch auto → fixed → error
  c_mode <- control; c_mode$calibration$n_simulations <- 5000L
  expect_error(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors, c_mode)),
               "mode changed")
})

# ---- checkpoint round-trip -------------------------------------------------

test_that("resume checkpoint round-trips decision state", {
  d <- tempfile("ckpt_"); dir.create(d, recursive = TRUE)
  f <- file.path(d, "resume_checkpoint.rds")
  st <- state_template()
  st$total_sims_run <- 2000L; st$batch_number <- 4L; st$phase <- "predictive"
  st$phase_batch_count <- 2L; st$calibration_done <- TRUE; st$converged <- FALSE
  st$ess_tracking <- list(list(batch = 1L, total_sims = 500L, threshold_ess = 20,
                               min_ess = 10, median_ess = 30, max_ess = 50))
  st$batch_success_rates <- c(99, 100); st$batch_sizes_used <- c(500L, 500L)

  MOSAIC:::.mosaic_save_checkpoint(st, f)
  ck <- MOSAIC:::.mosaic_load_checkpoint(f)

  expect_equal(ck$batch_number, 4L)
  expect_equal(ck$phase, "predictive")
  expect_true(ck$calibration_done)
  expect_equal(ck$ess_tracking, st$ess_tracking)
  expect_equal(ck$batch_sizes_used, c(500L, 500L))
})

test_that(".mosaic_load_checkpoint returns NULL on missing/corrupt files", {
  expect_null(MOSAIC:::.mosaic_load_checkpoint(tempfile()))
  f <- tempfile(fileext = ".rds"); writeLines("not an rds file", f)
  expect_null(MOSAIC:::.mosaic_load_checkpoint(f))
})

# ---- .mosaic_reconstruct_state ---------------------------------------------

make_dirs <- function() {
  base <- tempfile("recon_")
  dirs <- list(
    cal_samples = file.path(base, "2_calibration/samples"),
    cal_state   = file.path(base, "2_calibration/state")
  )
  dir.create(dirs$cal_samples, recursive = TRUE)
  dir.create(dirs$cal_state, recursive = TRUE)
  dirs
}

test_that(".mosaic_reconstruct_state restores exactly from checkpoint", {
  dirs <- make_dirs()
  for (id in 1:5) make_shard(dirs$cal_samples, id)

  ck <- state_template()
  ck$batch_number <- 3L; ck$phase <- "predictive"; ck$phase_batch_count <- 1L
  ck$calib_batches <- 2L; ck$r2_ess <- 0.97; ck$calibration_done <- TRUE
  ck$converged <- TRUE   # already-done run → no ESS recompute needed
  ck$ess_tracking <- list(list(batch = 3L, total_sims = 5L, threshold_ess = 120,
                               min_ess = 110, median_ess = 130, max_ess = 150))
  MOSAIC:::.mosaic_save_checkpoint(ck, file.path(dirs$cal_state, "resume_checkpoint.rds"))

  control <- list(calibration = list(batch_size_adaptive = 2L))
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, c("a", "b"))

  expect_equal(st$total_sims_run, 5L)   # disk watermark, NOT checkpoint's total
  expect_equal(st$total_sims_successful, 5L)
  expect_equal(st$batch_number, 3L)
  expect_equal(st$phase, "predictive")
  expect_true(st$calibration_done)
  expect_true(st$converged)
})

test_that(".mosaic_reconstruct_state bootstraps from shards without a checkpoint", {
  dirs <- make_dirs()
  for (id in 1:10) make_shard(dirs$cal_samples, id)  # < 50 → ESS check short-circuits

  control <- list(calibration = list(batch_size_adaptive = 5L),
                  io = list(), targets = list())
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, c("a", "b"))

  expect_equal(st$total_sims_run, 10L)
  expect_equal(st$total_sims_successful, 10L)
  expect_equal(st$batch_number, 2L)     # ceiling(10 / 5)
  expect_false(st$converged)
})

test_that(".mosaic_reconstruct_state bootstrap batch_number uses shard COUNT, not watermark", {
  # Gapped shard set: ids {1,2,3,50} -> watermark 50 but only 4 shards. The
  # bootstrap batch estimate must use the count (ceiling(4/2)=2), not the
  # watermark (ceiling(50/2)=25 would falsely exceed max_batches_adaptive).
  dirs <- make_dirs()
  for (id in c(1L, 2L, 3L, 50L)) make_shard(dirs$cal_samples, id)
  control <- list(calibration = list(batch_size_adaptive = 2L, max_batches_adaptive = 4L),
                  io = list(), targets = list())
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, c("a", "b"))

  expect_equal(st$total_sims_run, 50L)     # frontier still uses the watermark
  expect_equal(st$total_sims_successful, 4L)
  expect_equal(st$batch_number, 2L)        # ceiling(count 4 / 2), NOT ceiling(50/2)
  expect_false(st$calibration_done)        # 2 < max_batches_adaptive(4)
})

test_that(".mosaic_reconstruct_state returns unchanged state when no shards (fresh)", {
  dirs <- make_dirs()
  control <- list(calibration = list(batch_size_adaptive = 5L))
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, c("a", "b"))
  expect_equal(st$total_sims_run, 0L)   # untouched → behaves as a fresh run
  expect_equal(st$batch_number, 0L)
})

test_that(".mosaic_reconstruct_state bootstrap marks calibration done when budget met", {
  dirs <- make_dirs()
  for (id in 1:10) make_shard(dirs$cal_samples, id)  # < 50 → ESS check short-circuits
  # batch_size 2 → ceiling(10/2) = 5 batches; max_batches_adaptive 4 → budget met.
  control <- list(calibration = list(batch_size_adaptive = 2L, max_batches_adaptive = 4L),
                  io = list(), targets = list())
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, c("a", "b"))
  expect_equal(st$batch_number, 5L)
  expect_true(st$calibration_done)   # avoids re-running the full adaptive allotment
})

test_that(".mosaic_reconstruct_state refreshes ESS over the full pool when shards exceed checkpoint", {
  # Auto-mode continuation: a partial batch wrote shards after the last
  # checkpoint, so reconstruct must re-run the ESS check over ALL shards and
  # append a fresh tracking point. Uses real (engine-free) ESS computation.
  skip_if_not_installed("arrow")
  dirs <- make_dirs()
  set.seed(42)
  # Use real estimated-parameter names so calc_model_ess_parameter accepts them.
  pnames <- head(MOSAIC::estimated_parameters$parameter_name, 2L)
  n <- 55L  # > 50 so .mosaic_ess_check_update_state does not short-circuit
  for (id in seq_len(n)) {
    df <- data.frame(sim = id, iter = 1L, seed_sim = id, seed_iter = NA_real_,
                     likelihood = -runif(1, 1, 100))
    df[[pnames[1]]] <- runif(1)
    df[[pnames[2]]] <- runif(1)
    arrow::write_parquet(df, file.path(dirs$cal_samples, sprintf("sim_%07d.parquet", id)))
  }

  # Checkpoint: not converged, last tracking point at total_sims = 30 (< 55 on disk).
  ck <- state_template()
  ck$batch_number <- 1L; ck$converged <- FALSE
  ck$ess_tracking <- list(list(batch = 1L, total_sims = 30L, threshold_ess = 10,
                               min_ess = 5, median_ess = 12, max_ess = 20))
  MOSAIC:::.mosaic_save_checkpoint(ck, file.path(dirs$cal_state, "resume_checkpoint.rds"))

  control <- mosaic_control_defaults()
  st <- MOSAIC:::.mosaic_reconstruct_state(state_template(), dirs, control, pnames)

  expect_equal(st$total_sims_run, n)                # disk watermark
  expect_gt(length(st$ess_tracking), 1L)            # refresh appended a point
  newest <- st$ess_tracking[[length(st$ess_tracking)]]
  expect_equal(newest$total_sims, n)                # computed over the FULL pool
})

# ---- input integrity check -------------------------------------------------

test_that(".mosaic_resume_check_inputs passes on match, errors on drift", {
  base <- tempfile("inputs_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)

  priors <- list(tau_i = list(shape = 2, rate = 1))
  config <- list(location_name = "ETH", value = 1)
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  wj(priors, file.path(inp, "priors.json"))
  wj(config, file.path(inp, "config.json"))

  # suppressWarnings: no environment.json here, so the engine-version guard
  # correctly emits a "guard SKIPPED" warning that is orthogonal to this test.
  expect_true(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors)))

  expect_error(
    MOSAIC:::.mosaic_resume_check_inputs(dirs, config, list(tau_i = list(shape = 20, rate = 1))),
    "priors"
  )
  expect_error(
    MOSAIC:::.mosaic_resume_check_inputs(dirs, list(location_name = "ETH", value = 2), priors),
    "config"
  )
})

test_that(".mosaic_resume_check_inputs is a no-op when inputs not yet persisted", {
  base <- tempfile("inputs_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)
  # No priors.json/config.json on disk (fresh resume) → must not error.
  expect_true(suppressWarnings(MOSAIC:::.mosaic_resume_check_inputs(dirs, list(x = 1), list(y = 2))))
})

# ---- transmission-engine guard ----------------------------------------------
#
# These replace the v0.12 -> v0.13 laser-cholera deaths-scale tests. The guard
# they covered was retired in v0.69.0 (its "current" operand was the installed
# laser-cholera wheel, which stopped describing what simulated anything at the
# v0.68.0 R cutover), but the two properties those tests actually asserted --
# tolerant version parsing, and a boundary that separates incompatible shards --
# are properties of the replacement, so they are re-homed here rather than lost.

test_that(".mosaic_run_engine classifies the v0.68.0 engine boundary", {
  f <- MOSAIC:::.mosaic_run_engine
  # before the cutover -> Python laser-cholera
  expect_equal(f("0.67.0"), "python")
  expect_equal(f("0.32.0"), "python")
  expect_equal(f("0.13"),   "python")
  expect_equal(f("0.0.9"),  "python")
  # at and after the cutover -> R
  expect_equal(f("0.68.0"), "R")
  expect_equal(f("0.69.3"), "R")
  expect_equal(f("1.0.0"),  "R")
  # suffixes reduce to the leading integer of each component
  expect_equal(f("0.67.0-dev"),  "python")
  expect_equal(f("0.68.0.9000"), "R")
  expect_equal(f("0.68rc1"),     "R")
  # unparseable / degenerate -> NA
  expect_true(is.na(f("garbage")))
  expect_true(is.na(f("")))
  expect_true(is.na(f(NA_character_)))
  expect_true(is.na(f("0")))   # major only, no minor
  expect_true(is.na(f(NULL)))
})

test_that(".mosaic_resume_check_inputs hard-errors on a pre-v0.68.0 run directory", {
  base <- tempfile("eng_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)
  priors <- list(a = 1); config <- list(location_name = "ETH")
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  wj(priors, file.path(inp, "priors.json"))
  wj(config, file.path(inp, "config.json"))

  # Shards from the Python engine -> refuse to pool them with R-engine draws.
  wj(list(R = list(MOSAIC = "0.67.0")), file.path(inp, "environment.json"))
  expect_error(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors),
               "simulated with the Python")

  # Same engine -> no error and, unlike the old guard, no warning either: the
  # discriminator is on disk, so there is nothing to be unable to determine.
  wj(list(R = list(MOSAIC = "0.68.0")), file.path(inp, "environment.json"))
  expect_silent(expect_true(
    MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors)))

  # Unparseable version -> guard cannot classify -> warn, do not block.
  wj(list(R = list(MOSAIC = "garbage")), file.path(inp, "environment.json"))
  expect_warning(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors),
                 "transmission-engine guard was SKIPPED")

  # No environment.json at all (pre-feature run) -> warn, do not block.
  unlink(file.path(inp, "environment.json"))
  expect_warning(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors),
                 "no 1_inputs/environment.json")
})

test_that("the resume guard needs no Python to classify a run directory", {
  # The point of keying on the MOSAIC version rather than a laser-cholera one:
  # the check must not depend on what is installed in the Python environment,
  # which after v0.69.0 no longer contains laser-cholera at all.
  #
  # removeSource() first: deparse() on a srcref-carrying closure reproduces the
  # comments, and the comments here legitimately mention laser-cholera to
  # explain what was removed. Stripping to code asserts the behaviour (no
  # Python call) instead of the prose.
  src <- paste(deparse(removeSource(MOSAIC:::.mosaic_resume_check_inputs)),
               collapse = "\n")
  expect_false(grepl("reticulate", src, fixed = TRUE))
  expect_false(grepl("importlib", src, fixed = TRUE))
  expect_false(grepl("pkg_laser_cholera", src, fixed = TRUE))
})

# ---- likelihood-value provenance guard (Phase 3 / PR #111) ----

test_that(".mosaic_likelihood_provenance always reports R-side scoring", {
  # Scoring used to happen on-worker in Python on the Dask backend, which is
  # why the descriptor carries an `engine` field at all. With that backend gone
  # the only producer is MOSAIC::calc_model_likelihood(), so `engine` is always
  # "R" -- but the field stays, because archived shards recorded
  # engine = "python" and resume must still be able to tell them apart.
  p <- MOSAIC:::.mosaic_likelihood_provenance()
  expect_equal(p$engine, "R")
  expect_equal(p$impl_version, MOSAIC:::.mosaic_likelihood_impl_version())

  # The descriptor takes no arguments. It carried an unused `lc_version` until
  # v0.69.0 -- C-1 had already reduced the body to a constant, leaving a
  # parameter every caller filled and nothing read. Asserted so it cannot creep
  # back as a silently-ignored knob (CLAUDE.md lesson #13).
  expect_length(formals(MOSAIC:::.mosaic_likelihood_provenance), 0L)
})

test_that(".mosaic_resume_check_inputs rejects a different likelihood provenance", {
  base <- tempfile("prov_"); inp <- file.path(base, "1_inputs")
  dir.create(inp, recursive = TRUE)
  dirs <- list(inputs = inp)
  config <- list(location_name = "ETH"); priors <- list(a = 1)
  wj <- function(x, f) jsonlite::write_json(x, f, pretty = TRUE, auto_unbox = TRUE, digits = NA)

  # Stamp the persisted MOSAIC version on the R side of the v0.68.0 engine
  # boundary so the transmission-engine guard passes and cannot mask the
  # likelihood-provenance behaviour under test here. This used to require
  # pinning a live laser-cholera version read out of the Python environment --
  # a docker image whose wheel lagged env.yml would trip the engine guard first
  # and never reach the provenance dict. The fixture is now a literal.
  r_engine_env <- list(R = list(MOSAIC = "0.68.0"))

  cur <- MOSAIC:::.mosaic_likelihood_provenance()

  # matching provenance -> passes
  wj(c(list(likelihood_provenance = cur), r_engine_env),
     file.path(inp, "environment.json"))
  expect_true(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors))

  # a shard scored by the removed Python worker path -> hard error
  wj(c(list(likelihood_provenance = list(engine = "python", impl_version = "0.14.0")),
       r_engine_env),
     file.path(inp, "environment.json"))
  expect_error(
    MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors),
    "likelihood provenance differs")

  # absent provenance (pre-feature run) -> skipped, no error
  wj(r_engine_env, file.path(inp, "environment.json"))
  expect_true(MOSAIC:::.mosaic_resume_check_inputs(dirs, config, priors))
})


